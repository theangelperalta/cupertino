(in-package :cupertino)

;;;; Parallel multi-cell front-end for xcodebuild.
;;;;
;;;; `run-xcodebuild/parallel' runs a scheme x destination matrix of xcodebuild
;;;; invocations ("cells") concurrently under a single supercons dashboard. Each
;;;; cell gets a compact stacked panel; cells can be driven by text parsing or,
;;;; when SWB is usable, by Swift Build protocol events (each with its own events
;;;; tempfile and proxy env, so concurrent interception never collides).

(defparameter *xcbuild-parallel-rows* 4
  "Max in-progress rows shown per cell panel in the combined dashboard.")

;;; One compact panel per matrix cell: bold label (scheme or `scheme @ dest`) +
;;; right-aligned counts, then up to *xcbuild-parallel-rows* in-progress rows.
(defclass xcbuild-cell-panel (sc:component)
  ((label :initarg :label :reader panel-label)
   (stats :initarg :stats :reader panel-stats)))

(defmethod sc:draw-unchecked ((c xcbuild-cell-panel) dimensions mode)
  (let* ((stats (panel-stats c))
         (width (max 20 (sc:dimensions-width dimensions))))
    (ecase mode
      (:normal
       (let* ((counts (xcbuild-counts-string stats))
              (name (xcbuild-truncate (panel-label c)
                                      (max 0 (- width (length counts) 1))))
              (jobs (xcbuild-stats-jobs stats)))
         (when (> (length jobs) *xcbuild-parallel-rows*)
           (setf jobs (last jobs *xcbuild-parallel-rows*)))
         (sc:make-lines
          (list* (xcbuild-row width (list (xcbuild-span name #'sc:bold))
                              (length name) counts)
                 (loop for job in jobs collect (xcbuild-job-line job width))))))
      (:final
       (let ((ok (eq (xcbuild-stats-result stats) :success)))
         (sc:make-lines
          (list (sc:make-line
                 (list (xcbuild-span (format nil "~A " (panel-label c)) #'sc:bold)
                       (xcbuild-span (if ok "✓ " "✗ ") (if ok #'sc:green #'sc:red))
                       (xcbuild-span (xcbuild-final-summary stats) #'sc:dim))))))))))

;;; Composite root: stack all cell panels vertically. The matrix-wide
;;; '** ACTION SUCCEEDED/FAILED **' banner is printed by run-xcodebuild after
;;; report-result-bundles so the verdict sits below the bundles section.
(defclass xcbuild-multi-progress (sc:component)
  ((panels :initarg :panels :reader multi-panels)))

(defmethod sc:draw-unchecked ((c xcbuild-multi-progress) dimensions mode)
  (let ((b (sc:make-draw-vertical dimensions)))
    (dolist (panel (multi-panels c))
      (sc:draw-vertical-draw b panel mode))
    (sc:draw-vertical-finish b)))

;;; ---------------------------------------------------------------------------
;;; Worker-pool runner over the scheme x destination matrix.
;;; ---------------------------------------------------------------------------

(defun make-xcbuild-cell-state (cell action)
  "Construct the per-cell mutable state plist for one matrix invocation.
:LABEL/:CMD come from CELL; :STATS is freshly allocated so cells don't share
counters; :PROC/:EXIT are populated as the cell runs."
  (list :label (getf cell :label) :cmd (getf cell :cmd)
        :stats (make-xcbuild-stats :action action)
        :proc nil :exit nil))

(defun make-xcbuild-cells-dashboard (states)
  "Build the composite dashboard root that stacks one panel per cell, in the
order STATES were created (== the order CELLS were supplied)."
  (let ((panels (mapcar (lambda (s) (make-instance 'xcbuild-cell-panel
                                                   :label (getf s :label)
                                                   :stats (getf s :stats)))
                        states)))
    (make-instance 'xcbuild-multi-progress :panels panels)))

(defun run-xcodebuild/parallel-raw-fallback (states)
  "Non-TTY fallback: run STATES sequentially with raw interactive I/O. SWB
needs the dashboard so this path implies the text pipeline. Returns the
worst exit code across all cells (0 only if every cell succeeded)."
  (reduce #'max (mapcar (lambda (s) (run-xcodebuild/raw (getf s :cmd))) states)
          :initial-value 0))

(defun xcbuild-parallel-render-loop (console root lock done?)
  "Render-thread body: tick the dashboard at *XCBUILD-RENDER-INTERVAL* until
DONE? (a thunk) returns true, taking LOCK around each render so per-cell
worker mutations don't tear the frame."
  (loop until (funcall done?)
        do (sleep *xcbuild-render-interval*)
           (unless (funcall done?)
             (bt:with-lock-held (lock)
               (sc:superconsole-render console root)))))

(defun xcbuild-swb-spawn-text-drain (out console stats root lock)
  "Spawn the thread that drains xcodebuild's stdout under SWB mode. The build
graph is event-driven, but the test-execution and SPM-resolution phases emit
no SWB events, so lines matching *XCBUILD-SWB-TEXT-TYPES* are fed through the
text classifier; everything else is discarded so the pipe never blocks."
  (bt:make-thread
   (lambda ()
     (loop for l = (read-line out nil :eof) until (eq l :eof)
           do (let ((ev (xcbuild-swb-text-event l)))
                (when ev
                  (bt:with-lock-held (lock)
                    (xcbuild-handle-line console stats ev nil)
                    (sc:superconsole-render console root))))))
   :name "xcbuild-drain"))

(defun xcbuild-swb-spawn-event-consumer (events-file done? console stats root lock)
  "Spawn the thread that tails EVENTS-FILE and applies each decoded SWB event
to STATS under LOCK, rendering after each one. Stops when DONE? returns true."
  (bt:make-thread
   (lambda ()
     (swb-consume-events
      events-file done?
      (lambda (event)
        (bt:with-lock-held (lock)
          (xcbuild-handle-event console stats event)
          (sc:superconsole-render console root)))))
   :name "xcbuild-events"))

(defun xcbuild-parallel-emit-trace-digests (console swb states)
  "End-of-run SWB trace digest. Emitting per-cell + combined here (after all
workers joined, before finalize) keeps the lines contiguous and clearly
post-build; emitting per-cell as cells finished interleaved the digest with
still-running cells' progress. Gated on the trace flag -- BUILD_CONSOLE_OUTPUT
counters accrue on every SWB build, so without this gate the digest fired
even when --swb-trace was absent."
  (when (and swb (swb-runner-config-trace swb))
    (dolist (st states)
      (let ((*xcbuild-emit-prefix* (getf st :label)))
        (xcbuild-emit-trace-digest console (getf st :stats)
                                   :trace-file (getf st :events-file))))
    (xcbuild-emit-trace-combined console
                                 (mapcar (lambda (s) (getf s :stats)) states))))

;;; Worker-pool runner over the scheme x destination matrix. CELLS is a list of
;;; plists (:label :cmd), each one xcodebuild invocation. When SWB is a non-NIL
;;; SWB-RUNNER-CONFIG, each cell runs its own SWB pipeline; otherwise cells
;;; parse text output. FAIL-FAST stops launching queued cells and terminates
;;; in-flight cells on the first failure.
(defun run-xcodebuild/parallel (cells action &key jobs fail-fast swb)
  "Run CELLS concurrently (<= JOBS at once) under one combined dashboard.
Return 0 if all cells succeed, else 1."
  (let* ((console (sc:make-superconsole))
         (states (mapcar (lambda (c) (make-xcbuild-cell-state c action)) cells))
         (root (make-xcbuild-cells-dashboard states))
         (lock (bt:make-lock "xcbuild-parallel"))   ; serializes stats + render + state
         (qlock (bt:make-lock "xcbuild-queue"))     ; guards queue + aborted
         (queue (copy-list states))
         (aborted nil)
         (done nil)
         (render-thread nil))
    (unless console
      (return-from run-xcodebuild/parallel
        (run-xcodebuild/parallel-raw-fallback states)))
    (labels ((next-state ()
               (bt:with-lock-held (qlock) (unless aborted (pop queue))))
             (note-failure ()
               (when fail-fast
                 (let (procs)
                   (bt:with-lock-held (qlock) (setf aborted t queue nil))
                   (bt:with-lock-held (lock)
                     (setf procs (remove nil (mapcar (lambda (s) (getf s :proc)) states))))
                   (dolist (p procs) (ignore-errors (uiop:terminate-process p))))))
             (record-proc (st proc)
               (bt:with-lock-held (lock) (setf (getf st :proc) proc)))
             (finish-state (st code)
               (bt:with-lock-held (lock)
                 (setf (getf st :exit) code)
                 (setf (xcbuild-stats-result (getf st :stats))
                       (if (zerop code) :success :failure)))
               (unless (zerop code) (note-failure)))
             (run-one/pretty (st)
               (let* ((proc (uiop:launch-program (getf st :cmd)
                                                 :output :stream :error-output :output))
                      (stream (uiop:process-info-output proc))
                      (stats (getf st :stats))
                      (in-diag nil))
                 (record-proc st proc)
                 (loop for line = (read-line stream nil :eof) until (eq line :eof)
                       do (bt:with-lock-held (lock)
                            (let ((*xcbuild-emit-prefix* (getf st :label)))
                              (setf in-diag
                                    (xcbuild-handle-line console stats
                                                         (classify-xcodebuild-line line)
                                                         in-diag)))
                            (sc:superconsole-render console root)))
                 (finish-state st (uiop:wait-process proc))))
             (run-one/swb (st)
               ;; Mirror run-xcodebuild/swb, but per cell, into the shared console.
               ;; SWB-RESOLVE-EVENTS-FILE gives each cell a tempfile by default,
               ;; or a per-cell-suffixed path under the user's --swb-trace-file
               ;; base; persistent paths are stashed on the state so the
               ;; post-run digest can reference them.
               (multiple-value-bind (events persistent-p)
                   (swb-resolve-events-file swb (getf st :label))
                 (setf (getf st :events-file) (when persistent-p events))
                 (let* ((env-cmd (pty-wrap-command
                                  (concatenate 'string
                                               (swb-env-prefix swb events)
                                               (getf st :cmd))))
                        (proc (uiop:launch-program env-cmd
                                                   :output :stream
                                                   :error-output :output))
                        (out (uiop:process-info-output proc))
                        (stats (getf st :stats))
                        (ev-done nil))
                   (record-proc st proc)
                   ;; Create the file up front so the reader can open it before
                   ;; the proxy does.
                   (ignore-errors (close (open events :direction :output
                                                      :if-does-not-exist :create
                                                      :if-exists :append)))
                   (let ((drain (xcbuild-swb-spawn-text-drain
                                 out console stats root lock))
                         (ev (xcbuild-swb-spawn-event-consumer
                              events (lambda () ev-done) console stats root lock))
                         (code (uiop:wait-process proc)))
                     (setf ev-done t)
                     (ignore-errors (bt:join-thread drain))
                     (ignore-errors (bt:join-thread ev))
                     (unless persistent-p (ignore-errors (delete-file events)))
                     (finish-state st code)))))
             (run-one (st) (if swb (run-one/swb st) (run-one/pretty st))))
      (setf render-thread
            (bt:make-thread
             (lambda () (xcbuild-parallel-render-loop
                         console root lock (lambda () done)))
             :name "xcbuild-parallel-render"))
      (unwind-protect
           (let ((workers
                   (loop repeat (max 1 (min (or jobs (length states)) (length states)))
                         collect (bt:make-thread
                                  (lambda () (loop for st = (next-state)
                                                   while st do (run-one st)))
                                  :name "xcbuild-worker"))))
             (dolist (w workers) (ignore-errors (bt:join-thread w)))
             (reduce (lambda (acc st)
                       (if (eq (xcbuild-stats-result (getf st :stats)) :success) acc 1))
                     states :initial-value 0))
        (setf done t)
        (when render-thread (ignore-errors (bt:join-thread render-thread)))
        (xcbuild-parallel-emit-trace-digests console swb states)
        ;; Combined matrix verdict is appended below the per-cell summary rows
        ;; in xcbuild-multi-progress's :final draw, so the banner is the last
        ;; line of the finalized canvas rather than scrollback above it.
        (when (sc:superconsole-output console)
          (sc:superconsole-finalize console root))))))
