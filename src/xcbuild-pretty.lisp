(in-package :cupertino)

;;;; supercons front-end for xcodebuild, styled after superconsole's buck2 UI.
;;;;
;;;; `run-xcodebuild/pretty' launches xcodebuild, reads its merged output line by
;;;; line, and classifies each line with `classify-xcodebuild-line'. Notable
;;;; events (warnings, errors, test results, phases) scroll as a styled log above
;;;; a live multi-line canvas that mimics buck2's "what's running now" dashboard:
;;;;
;;;;   Working on tasks for command: `build`.   Jobs: In progress: 8. Finished: …
;;;;   --------------------------------------------------------------------------
;;;;   AppDelegate.swift -- running action (compile)                       3.1s
;;;;   ViewController.swift -- running action (compile)                    1.4s
;;;;   …
;;;;
;;;; Build steps (compile/link/copy/sign) feed the live dashboard rather than the
;;;; scrollback, the way buck2 surfaces in-progress actions.

;;; ---------------------------------------------------------------------------
;;; Progress state
;;; ---------------------------------------------------------------------------

(defparameter *xcbuild-max-jobs* 8
  "How many in-progress actions to show in the live dashboard. xcodebuild gives
no explicit completion events, so we keep a sliding window of the most recent
actions and count older ones as finished, mirroring buck2's concurrent view.")

(defparameter *xcbuild-slow-threshold* 4.0
  "Seconds after which an in-progress action is highlighted red, like buck2.")

(defparameter *xcbuild-render-interval* 0.1
  "Seconds between background render-thread ticks. Each tick redraws the live
canvas so elapsed times keep advancing even when xcodebuild produces no output.")

(defstruct xcbuild-job
  "One in-progress action shown on the live dashboard."
  (id nil)                              ; SWB task id, for precise start/end matching
  (label "")                            ; left-hand text, e.g. "Foo.swift"
  (kind "")                             ; action verb, e.g. "compile"
  (test nil)                            ; T for a running test suite
  (passed 0)                            ; (test jobs) test cases passed so far
  (failed 0)                            ; (test jobs) test cases failed so far
  (start (get-internal-real-time)))

(defstruct xcbuild-stats
  (action "build")
  (compiled 0)
  (finished 0)                          ; actions that scrolled out of the window
  (skipped 0)                           ; up-to-date tasks (SWB mode)
  (progress nil)                        ; "completed / total" string (SWB mode)
  (warnings 0)
  (errors 0)
  (tests-passed 0)
  (tests-failed 0)
  (jobs '())                            ; oldest-first list of xcbuild-job
  (seen-diags (make-hash-table :test 'equal)) ; dedup (kind . message) (SWB mode)
  (start-time (get-internal-real-time))
  (result nil))                         ; :success / :failure, set on finalize

(defun xcbuild-action-verb (action)
  "Present-progressive verb for ACTION, e.g. \"build\" -> \"Building\"."
  (cond ((string= action "build") "Building")
        ((string= action "test") "Testing")
        ((string= action "clean") "Cleaning")
        ((string= action "archive") "Archiving")
        (t (string-capitalize action))))

(defun xcbuild-elapsed (start)
  "Elapsed wall-clock seconds since internal real-time START, as a float."
  (/ (float (- (get-internal-real-time) start))
     internal-time-units-per-second))

;;; ---------------------------------------------------------------------------
;;; In-progress action bookkeeping
;;; ---------------------------------------------------------------------------

(defun xcbuild-trim-jobs (stats)
  "Retire oldest (least-recently-active) jobs, counting them finished, until the
dashboard fits *xcbuild-max-jobs*."
  (loop while (> (length (xcbuild-stats-jobs stats)) *xcbuild-max-jobs*)
        do (pop (xcbuild-stats-jobs stats))
           (incf (xcbuild-stats-finished stats))))

(defun xcbuild-start-job (stats kind label)
  "Push a new in-progress build action onto STATS's dashboard."
  (setf (xcbuild-stats-jobs stats)
        (append (xcbuild-stats-jobs stats)
                (list (make-xcbuild-job :kind kind :label label))))
  (xcbuild-trim-jobs stats))

(defun xcbuild-bump-suite (stats suite passed-p)
  "Record a passed/failed test case for test SUITE on the live dashboard,
creating the suite's job on its first case and marking it most-recently-active so
the dashboard shows the suites currently producing results."
  (let ((job (find-if (lambda (j)
                        (and (xcbuild-job-test j)
                             (string= (xcbuild-job-label j) suite)))
                      (xcbuild-stats-jobs stats))))
    (unless job
      (setf job (make-xcbuild-job :label suite :test t)))
    (if passed-p (incf (xcbuild-job-passed job)) (incf (xcbuild-job-failed job)))
    ;; Move (or add) JOB to the end so the most recently active suite is freshest.
    (setf (xcbuild-stats-jobs stats)
          (append (remove job (xcbuild-stats-jobs stats)) (list job)))
    (xcbuild-trim-jobs stats)))

;;; ---------------------------------------------------------------------------
;;; Styled span/line helpers
;;; ---------------------------------------------------------------------------

(defun xcbuild-span (string &rest style-fns)
  "Make a supercons span for STRING with STYLE-FNS applied left to right
\(e.g. #'sc:green #'sc:bold). With no STYLE-FNS, an unstyled span."
  (if style-fns
      (sc:make-span-styled
       (reduce (lambda (acc fn) (funcall fn acc)) style-fns :initial-value string))
      (sc:make-span-unstyled string)))

(defun xcbuild-emit (console &rest spans)
  "Emit a single log line built from SPANS above CONSOLE's canvas."
  (sc:superconsole-emit console
                        (sc:make-lines (list (sc:make-line spans)))))

(defun xcbuild-truncate (string width)
  "Truncate STRING to at most WIDTH columns, ending in an ellipsis when cut.
Treats one char as one column, which is accurate for the ASCII paths we show."
  (if (<= (length string) width)
      string
      (if (>= width 1)
          (concatenate 'string (subseq string 0 (1- width)) "…")
          "")))

(defun xcbuild-row (width left-spans left-cols right-string &rest right-style-fns)
  "Build a LINE whose LEFT-SPANS (with combined column width LEFT-COLS) sit on the
left and RIGHT-STRING is flush-right at column WIDTH, styled by RIGHT-STYLE-FNS.
Assumes the caller already truncated LEFT-SPANS to fit."
  (let* ((right-cols (length right-string))
         (pad (max 1 (- width left-cols right-cols))))
    (sc:make-line
     (append left-spans
             (list (xcbuild-span (make-string pad :initial-element #\Space))
                   (apply #'xcbuild-span right-string right-style-fns))))))

;;; ---------------------------------------------------------------------------
;;; Live progress component
;;; ---------------------------------------------------------------------------

(defclass xcbuild-progress (sc:component)
  ((stats :initarg :stats :reader xcbuild-progress-stats)))

(defun xcbuild-counts-string (stats)
  "buck2-style job summary, e.g.
\"Jobs: In progress: 8. Finished: 412. Warnings: 1. Errors: 0. Time elapsed: 31.3s\"."
  (format nil "Jobs: In progress: ~D. Finished: ~D.~@[ Skipped: ~D.~]~@[ Warnings: ~D.~]~@[ Errors: ~D.~]~@[ Tests: ~A.~]~@[ Progress: ~A.~] Time elapsed: ~,1Fs"
          (length (xcbuild-stats-jobs stats))
          (xcbuild-stats-finished stats)
          (when (plusp (xcbuild-stats-skipped stats)) (xcbuild-stats-skipped stats))
          (when (plusp (xcbuild-stats-warnings stats)) (xcbuild-stats-warnings stats))
          (when (plusp (xcbuild-stats-errors stats)) (xcbuild-stats-errors stats))
          (when (or (plusp (xcbuild-stats-tests-passed stats))
                    (plusp (xcbuild-stats-tests-failed stats)))
            (format nil "~D✓/~D✗"
                    (xcbuild-stats-tests-passed stats)
                    (xcbuild-stats-tests-failed stats)))
          (xcbuild-stats-progress stats)
          (xcbuild-elapsed (xcbuild-stats-start-time stats))))

(defun xcbuild-final-summary (stats)
  "One-line completion summary for the final frame, e.g.
\"(417 compiled, 1 warning, 0 errors) in 31.3s\"."
  (format nil "(~D compiled~@[, ~D skipped~], ~D warning~:P, ~D error~:P~@[, tests ~A~]) in ~,1Fs"
          (xcbuild-stats-compiled stats)
          (when (plusp (xcbuild-stats-skipped stats)) (xcbuild-stats-skipped stats))
          (xcbuild-stats-warnings stats)
          (xcbuild-stats-errors stats)
          (when (or (plusp (xcbuild-stats-tests-passed stats))
                    (plusp (xcbuild-stats-tests-failed stats)))
            (format nil "~D✓/~D✗"
                    (xcbuild-stats-tests-passed stats)
                    (xcbuild-stats-tests-failed stats)))
          (xcbuild-elapsed (xcbuild-stats-start-time stats))))

(defun xcbuild-header-line (stats width)
  "The buck2 header: a left task label and right-aligned job counts."
  (let* ((left (format nil "Working on tasks for command: `~A`."
                       (xcbuild-stats-action stats)))
         (right (xcbuild-counts-string stats))
         (left (xcbuild-truncate left (max 0 (- width (length right) 1)))))
    (xcbuild-row width
                 (list (xcbuild-span left #'sc:bold))
                 (length left)
                 right)))

(defun xcbuild-job-line (job width)
  "A single in-progress row: description left, per-job elapsed right. Build actions
go red once past *xcbuild-slow-threshold*; test suites go red once any case fails."
  (let* ((elapsed (xcbuild-elapsed (xcbuild-job-start job)))
         (time (format nil "~,1Fs" elapsed))
         (test (xcbuild-job-test job))
         (desc (if test
                   (format nil "~A -- running (~D✓~@[ ~D✗~])"
                           (xcbuild-job-label job)
                           (xcbuild-job-passed job)
                           (when (plusp (xcbuild-job-failed job))
                             (xcbuild-job-failed job)))
                   (format nil "~A -- running action (~A)"
                           (xcbuild-job-label job) (xcbuild-job-kind job))))
         (desc (xcbuild-truncate desc (max 0 (- width (length time) 1))))
         (color (cond ((and test (plusp (xcbuild-job-failed job))) #'sc:red)
                      ((and (not test) (>= elapsed *xcbuild-slow-threshold*)) #'sc:red)
                      (t #'sc:cyan))))
    (xcbuild-row width (list (xcbuild-span desc color)) (length desc)
                 time #'sc:dim)))

(defmethod sc:draw-unchecked ((c xcbuild-progress) dimensions mode)
  (let* ((stats (xcbuild-progress-stats c))
         (width (max 20 (sc:dimensions-width dimensions))))
    (ecase mode
      (:normal
       (let ((jobs (xcbuild-stats-jobs stats)))
         ;; SWB mode can hold more live jobs than the window; show the newest.
         (when (> (length jobs) *xcbuild-max-jobs*)
           (setf jobs (last jobs *xcbuild-max-jobs*)))
         (sc:make-lines
          (list* (xcbuild-header-line stats width)
                 (sc:make-line
                  (list (xcbuild-span (make-string width :initial-element #\-) #'sc:dim)))
                 (loop for job in jobs
                       collect (xcbuild-job-line job width))))))
      (:final
       (let* ((noun (string-capitalize (xcbuild-stats-action stats)))
              (suffix (xcbuild-final-summary stats)))
         (sc:make-lines
          (list (if (eq (xcbuild-stats-result stats) :success)
                    (sc:make-line
                     (list (xcbuild-span (format nil "   ~A Succeeded " noun) #'sc:green #'sc:bold)
                           (xcbuild-span suffix #'sc:dim)))
                    (sc:make-line
                     (list (xcbuild-span (format nil "   ~A Failed " noun) #'sc:red #'sc:bold)
                           (xcbuild-span suffix #'sc:dim)))))))))))

;;; ---------------------------------------------------------------------------
;;; Driver
;;; ---------------------------------------------------------------------------

(defun xcbuild-handle-line (console stats event in-diagnostic)
  "Update STATS and, for notable events, emit a scrollback line for EVENT. Build
steps feed the live dashboard instead. Return the new IN-DIAGNOSTIC flag (true
while inside a multi-line compiler diagnostic, whose context lines are shown
verbatim)."
  (let* ((type (getf event :type))
         (text (getf event :text))
         (file (getf event :file)))
    (case type
      (:compile
       (incf (xcbuild-stats-compiled stats))
       (xcbuild-start-job stats "compile" (or file text)))
      (:link
       (xcbuild-start-job stats "link" (or (xcbuild-link-product text) "binary")))
      (:copy
       (xcbuild-start-job stats "copy" text))
      (:codesign
       (xcbuild-start-job stats "sign" text))
      (:phase
       (xcbuild-emit console (xcbuild-span text #'sc:bold)))
      (:warning
       (incf (xcbuild-stats-warnings stats))
       (xcbuild-emit console (xcbuild-span "    warning " #'sc:yellow #'sc:bold)
                     (xcbuild-span text)))
      (:error
       (incf (xcbuild-stats-errors stats))
       (xcbuild-emit console (xcbuild-span "      error " #'sc:red #'sc:bold)
                     (xcbuild-span text #'sc:red)))
      (:testing-start
       ;; The build phase is over; clear its jobs so the dashboard now tracks
       ;; running test suites.
       (setf (xcbuild-stats-jobs stats) nil)
       (xcbuild-emit console (xcbuild-span text #'sc:bold)))
      (:test-case-passed
       (incf (xcbuild-stats-tests-passed stats))
       ;; Passes feed the live suite dashboard rather than scrolling (avoids a
       ;; wall of ✓ lines); only failures are notable enough to scroll.
       (let ((suite (xcbuild-test-suite-name text)))
         (when suite (xcbuild-bump-suite stats suite t))))
      (:test-case-failed
       (incf (xcbuild-stats-tests-failed stats))
       (let ((suite (xcbuild-test-suite-name text)))
         (when suite (xcbuild-bump-suite stats suite nil)))
       (xcbuild-emit console (xcbuild-span "      ✗ " #'sc:red #'sc:bold)
                     (xcbuild-span (xcbuild-test-label text) #'sc:red)))
      ;; Suites appear on the dashboard lazily on their first case, so the
      ;; "started" lines (often all announced up front) need no handling.
      (:test-suite-start nil)
      (:test-suite-summary
       (xcbuild-emit console (xcbuild-span "    ") (xcbuild-span text #'sc:dim)))
      ((:result-success :result-failure)
       (xcbuild-emit console (xcbuild-span text #'sc:bold)))
      (:unknown
       ;; Context lines belonging to a diagnostic (source + caret): show verbatim.
       (when (and in-diagnostic (plusp (length (string-trim " " text))))
         (xcbuild-emit console (xcbuild-span "            ") (xcbuild-span text))))
      (:other nil))
    ;; Stay in diagnostic mode through the blank/context lines after a warn/error.
    (case type
      ((:error :warning) t)
      (:unknown in-diagnostic)
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Swift Build (SWBBuildService) structured events
;;;
;;; In --use-swb mode the dashboard is driven by decoded protocol events from
;;; the interception proxy (see src/swb/) instead of classified text. Events
;;; carry explicit task ids and per-task start/end, so jobs are tracked
;;; precisely rather than via a sliding window, and progress/diagnostics come
;;; straight from the build engine.
;;; ---------------------------------------------------------------------------

(defun swb-arg-json (arg)
  "Parse ARG, a frame argument carrying a JSON byte vector, into a hash-table.
Returns NIL on anything unparseable."
  (when (and (vectorp arg) (not (stringp arg)))
    (handler-case (yason:parse (map 'string #'code-char arg))
      (error () nil))))

(defun swb-first-string (&rest candidates)
  "First non-empty string among CANDIDATES, or \"task\"."
  (or (find-if (lambda (x) (and (stringp x) (plusp (length x)))) candidates)
      "task"))

(defun xcbuild-space-like-p (ch)
  "True for any whitespace or non-graphic character, including the Unicode
spaces (thin space, NBSP, ...) that Swift Build uses in its strings."
  (let ((c (char-code ch)))
    (or (char= ch #\Space)
        (member c '(9 10 11 12 13 133 160 5760 8232 8233 8239 8287 8203 12288))
        (<= 8192 c 8202)
        (not (graphic-char-p ch)))))

(defun xcbuild-clean (string)
  "Make STRING safe for a supercons span: collapse every run of whitespace
\(any kind, including Unicode spaces and embedded newlines) to one plain space
and trim. supercons rejects spans containing non-space whitespace."
  (if (stringp string)
      (string-trim '(#\Space)
                   (with-output-to-string (out)
                     (let ((prev nil))
                       (loop for ch across string
                             for sp = (xcbuild-space-like-p ch)
                             do (cond (sp (unless prev (write-char #\Space out))
                                          (setf prev t))
                                      (t (write-char ch out) (setf prev nil)))))))
      string))

(defun swb-task-kind (rule ruleid)
  "Map a SWB rule name to a short action verb for the dashboard."
  (let ((r (swb-first-string rule ruleid "")))
    (cond ((search "Compile" r) "compile")
          ((or (search "Ld" r) (search "Link" r)) "link")
          ((search "CodeSign" r) "sign")
          ((search "Copy" r) "copy")
          ((stringp rule) rule)
          (t "task"))))

(defun xcbuild-swb-task-started (stats outer)
  "Begin a job for a BUILD_TASK_STARTED payload
OUTER = #(task-id NIL NIL #(rule-id signature rule description ...))."
  (when (and (vectorp outer) (>= (length outer) 4))
    (let* ((id (aref outer 0))
           (inner (aref outer 3))
           (ruleid (when (and (vectorp inner) (>= (length inner) 1)) (aref inner 0)))
           (rule (when (and (vectorp inner) (>= (length inner) 3)) (aref inner 2)))
           (desc (when (and (vectorp inner) (>= (length inner) 4)) (aref inner 3)))
           (kind (swb-task-kind rule ruleid)))
      (when (string= kind "compile") (incf (xcbuild-stats-compiled stats)))
      (setf (xcbuild-stats-jobs stats)
            (append (xcbuild-stats-jobs stats)
                    (list (make-xcbuild-job
                           :id id :kind kind
                           :label (xcbuild-clean
                                   (swb-first-string desc ruleid rule)))))))))

(defun xcbuild-swb-task-ended (stats arg)
  "Retire the job matching a BUILD_TASK_ENDED payload (JSON {\"id\":N})."
  (let* ((h (swb-arg-json arg))
         (id (and h (gethash "id" h))))
    (when id
      (let ((job (find id (xcbuild-stats-jobs stats)
                       :key #'xcbuild-job-id :test #'eql)))
        (when job
          (setf (xcbuild-stats-jobs stats) (remove job (xcbuild-stats-jobs stats)))
          (incf (xcbuild-stats-finished stats)))))))

(defun xcbuild-swb-new-diag-p (stats kind message)
  "True the first time (KIND . MESSAGE) is seen this build. SWB emits the same
diagnostic at several scopes; dedup so counts match what a human expects."
  (let ((key (cons kind message))
        (table (xcbuild-stats-seen-diags stats)))
    (unless (gethash key table)
      (setf (gethash key table) t))))

(defun xcbuild-swb-diagnostic (console stats arg)
  "Handle a BUILD_DIAGNOSTIC_EMITTED payload. kind 1 = warning, 2 = error;
0 (note) is ignored to keep the scrollback quiet."
  (let ((h (swb-arg-json arg)))
    (when h
      (let ((kind (gethash "kind" h))
            (msg (xcbuild-clean (or (gethash "message" h) ""))))
        (cond
          ((and (eql kind 1) (xcbuild-swb-new-diag-p stats kind msg))
           (incf (xcbuild-stats-warnings stats))
           (when console
             (xcbuild-emit console (xcbuild-span "    warning " #'sc:yellow #'sc:bold)
                           (xcbuild-span msg))))
          ((and (eql kind 2) (xcbuild-swb-new-diag-p stats kind msg))
           (incf (xcbuild-stats-errors stats))
           (when console
             (xcbuild-emit console (xcbuild-span "      error " #'sc:red #'sc:bold)
                           (xcbuild-span msg #'sc:red)))))))))

(defun xcbuild-swb-progress (stats arg)
  "Update the overall progress string from a BUILD_PROGRESS_UPDATED payload
ARG = #(target \"completed / total\" percent ...)."
  (when (and (vectorp arg) (>= (length arg) 2) (stringp (aref arg 1)))
    (setf (xcbuild-stats-progress stats) (xcbuild-clean (aref arg 1)))))

(defun xcbuild-swb-build-ended (stats arg)
  "Record the build result from a BUILD_OPERATION_ENDED payload
\(JSON {\"status\":N}, 0 = success). The exit code remains authoritative."
  (let ((h (swb-arg-json arg)))
    (when h
      (let ((status (gethash "status" h)))
        (when (numberp status)
          (setf (xcbuild-stats-result stats)
                (if (zerop status) :success :failure)))))))

(defun xcbuild-handle-event (console stats event)
  "Apply one decoded SWB EVENT (a plist from swb:frame->event) to STATS,
emitting scrollback via CONSOLE for diagnostics."
  (let ((arg (first (getf event :args))))
    (case (getf event :type)
      (:task-started  (xcbuild-swb-task-started stats arg))
      (:task-ended    (xcbuild-swb-task-ended stats arg))
      (:diagnostic    (xcbuild-swb-diagnostic console stats arg))
      (:progress      (xcbuild-swb-progress stats arg))
      (:build-ended   (xcbuild-swb-build-ended stats arg))
      (t nil))))

;;; ---------------------------------------------------------------------------
;;; Driver
;;; ---------------------------------------------------------------------------

(defun run-xcodebuild/pretty (cmd-str action console)
  "Run CMD-STR for ACTION, rendering a buck2-style supercons UI on CONSOLE.
Return the xcodebuild exit code."
  (let* ((stats (make-xcbuild-stats :action action))
         (progress (make-instance 'xcbuild-progress :stats stats))
         (proc (uiop:launch-program cmd-str
                                    :output :stream
                                    :error-output :output
                                    :input :interactive))
         (stream (uiop:process-info-output proc))
         (in-diagnostic nil)
         ;; LOCK serializes the two threads: the reader (mutating stats and
         ;; queuing scrollback) and the render thread (drawing the canvas).
         (lock (bt:make-lock "xcbuild-render"))
         (done nil)
         (render-thread nil))
    (flet ((render ()
             (bt:with-lock-held (lock)
               (sc:superconsole-render console progress))))
      ;; A background thread re-draws the canvas on a timer so the elapsed times
      ;; (header total and per-action clocks) keep advancing while xcodebuild is
      ;; quiet, freeing the reader to simply block on READ-LINE.
      (setf render-thread
            (bt:make-thread
             (lambda ()
               (loop until done
                     do (sleep *xcbuild-render-interval*)
                        (unless done (render))))
             :name "xcbuild-render"))
      (unwind-protect
           (progn
             (loop for line = (read-line stream nil :eof)
                   until (eq line :eof)
                   do (bt:with-lock-held (lock)
                        (setf in-diagnostic
                              (xcbuild-handle-line console stats
                                                   (classify-xcodebuild-line line)
                                                   in-diagnostic))
                        ;; Draw immediately so scrollback (errors, warnings)
                        ;; appears the instant its line is read.
                        (sc:superconsole-render console progress)))
             (let ((exit-code (uiop:wait-process proc)))
               (setf (xcbuild-stats-result stats)
                     (if (zerop exit-code) :success :failure))
               exit-code))
        ;; Stop the render thread before drawing the final frame, so nothing
        ;; redraws over the finalized canvas.
        (setf done t)
        (when render-thread (ignore-errors (bt:join-thread render-thread)))
        ;; Always draw the final frame so the cursor/canvas is restored, even if
        ;; reading the stream errors out partway through.
        (when (sc:superconsole-output console)
          (sc:superconsole-finalize console progress))))))
