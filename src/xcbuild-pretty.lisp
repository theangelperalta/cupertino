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

(defparameter *xcbuild-show-cache-hits* nil
  "When true, the dashboard shows a cache-hit percentage: the fraction of the
build graph that was up to date and didn't need to run. Only meaningful in
--use-swb mode, where the task total and per-task starts are known, so it
defaults on whenever SWB interception is active (see RUN-XCODEBUILD).")

(defparameter *xcbuild-emit-prefix* nil
  "When set (a cell label), xcbuild-emit prefixes each scrollback line with it so
parallel runs' diagnostics stay attributable to their scheme/destination.")

(defstruct xcbuild-job
  "One in-progress action shown on the live dashboard."
  (id nil)                              ; SWB task id, for precise start/end matching
  (label "")                            ; left-hand text, e.g. "Foo.swift"
  (kind "")                             ; action verb, e.g. "compile"
  (test nil)                            ; T for a running test suite
  (resolve nil)                         ; T for an SPM package-resolution row
  (passed 0)                            ; (test jobs) test cases passed so far
  (failed 0)                            ; (test jobs) test cases failed so far
  (start (get-internal-real-time)))

(defstruct xcbuild-stats
  (action "build")
  (compiled 0)
  (finished 0)                          ; actions that scrolled out of the window
  (skipped 0)                           ; up-to-date tasks (SWB mode)
  (executed 0)                          ; tasks that actually ran (SWB task-started count)
  (total-tasks 0)                       ; build-graph size, max total seen in progress (SWB)
  ;; Captured at construction (main thread) so the render threads, which don't
  ;; inherit the dynamic binding, still see whether to show cache hits.
  (show-cache-hits *xcbuild-show-cache-hits*)
  (progress nil)                        ; "completed / total" string (SWB mode)
  (warnings 0)
  (errors 0)
  (tests-passed 0)
  (tests-failed 0)
  (jobs '())                            ; oldest-first list of xcbuild-job
  (seen-diags (make-hash-table :test 'equal)) ; dedup (kind . message) (SWB mode)
  (targets-started 0)                   ; BUILD_TARGET_STARTED count (SWB)
  (targets-ended 0)                     ; BUILD_TARGET_ENDED count (SWB)
  (console-outputs 0)                   ; BUILD_CONSOLE_OUTPUT_EMITTED count (SWB)
  ;; Lazily allocated -- only the --swb-trace path ever populates this, so the
  ;; common case avoids an unused hash-table per stats instance.
  (unknown-msgs nil)                    ; NIL or hash-table: name -> count
  (start-time (get-internal-real-time))
  (phase :build)                        ; :build until `Testing started', then :test
  (result nil))                         ; :success / :failure, set on finalize

(defun xcbuild-action-verb (action)
  "Present-progressive verb for ACTION, e.g. \"build\" -> \"Building\"."
  (cond ((string= action "build") "Building")
        ((string= action "test") "Testing")
        ((string= action "clean") "Cleaning")
        ((string= action "archive") "Archiving")
        (t (string-capitalize action))))

(defun xcbuild-phase-label (stats)
  "Per-cell phase indicator for a `test' action, NIL otherwise. A `test' run has
two phases: building the test bundles (\"Building\") then running them
(\"Testing\"); other actions have a single phase so need no indicator."
  (when (string= (xcbuild-stats-action stats) "test")
    (if (eq (xcbuild-stats-phase stats) :test) "Testing" "Building")))

(defun xcbuild-result-banner-text (stats text)
  "Relabel xcodebuild's `** TEST SUCCEEDED/FAILED **' banner when it is really the
build-for-testing result (action is `test' but the test phase has not started),
so it is not mistaken for the final test verdict. Other banners pass through."
  (if (and (string= (xcbuild-stats-action stats) "test")
           (eq (xcbuild-stats-phase stats) :build))
      (cond ((string= text "** TEST SUCCEEDED **") "** BUILD FOR TESTING SUCCEEDED **")
            ((string= text "** TEST FAILED **") "** BUILD FOR TESTING FAILED **")
            (t text))
      text))

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

(defun xcbuild-retire-suite (stats suite)
  "Remove any live-dashboard row whose test SUITE matches. No-op when SUITE
is NIL or no matching row exists. Called when xcodebuild announces a suite's
completion so finished suites don't linger until the sliding window evicts them."
  (when suite
    (setf (xcbuild-stats-jobs stats)
          (remove-if (lambda (j)
                       (and (xcbuild-job-test j)
                            (string= (xcbuild-job-label j) suite)))
                     (xcbuild-stats-jobs stats)))))

(defun xcbuild-bump-package (stats name kind)
  "Create or refresh the live SPM resolution row for package NAME with action
KIND, marking it most-recently-active so the dashboard shows the packages
currently resolving. NAME is NIL-safe (no-op)."
  (when name
    (let ((job (find-if (lambda (j)
                          (and (xcbuild-job-resolve j)
                               (string= (xcbuild-job-label j) name)))
                        (xcbuild-stats-jobs stats))))
      (unless job
        (setf job (make-xcbuild-job :label name :resolve t)))
      (setf (xcbuild-job-kind job) kind)
      (setf (xcbuild-stats-jobs stats)
            (append (remove job (xcbuild-stats-jobs stats)) (list job)))
      (xcbuild-trim-jobs stats))))

(defun xcbuild-retire-packages (stats)
  "Remove all live SPM resolution rows once xcodebuild announces resolution is
complete, so they don't linger until the sliding window evicts them."
  (setf (xcbuild-stats-jobs stats)
        (remove-if #'xcbuild-job-resolve (xcbuild-stats-jobs stats))))

;;; ---------------------------------------------------------------------------
;;; Styled span/line helpers
;;; ---------------------------------------------------------------------------

(defun xcbuild-space-like-p (ch)
  "True for any whitespace or non-graphic character, including the Unicode
spaces (thin space, NBSP, ...) that Swift Build uses in its strings."
  (let ((c (char-code ch)))
    (or (char= ch #\Space)
        (member c '(9 10 11 12 13 133 160 5760 8232 8233 8239 8287 8203 12288))
        (<= 8192 c 8202)
        (not (graphic-char-p ch)))))

(defun xcbuild-normalize-ws (string)
  "Replace every non-space whitespace character in STRING with a regular space
so the result is valid supercons span content. Preserves length and visual
structure; does not collapse runs or trim."
  (if (stringp string)
      (map 'string (lambda (ch)
                     (if (and (not (char= ch #\Space))
                              (xcbuild-space-like-p ch))
                         #\Space
                         ch))
           string)
      string))

(defun xcbuild-span (string &rest style-fns)
  "Make a supercons span for STRING with STYLE-FNS applied left to right
\(e.g. #'sc:green #'sc:bold). With no STYLE-FNS, an unstyled span. STRING is
normalized so any non-space whitespace (tabs, embedded newlines, NBSP, ...) is
replaced with a regular space, since supercons rejects such characters."
  (let ((string (xcbuild-normalize-ws string)))
    (if style-fns
        (sc:make-span-styled
         (reduce (lambda (acc fn) (funcall fn acc)) style-fns :initial-value string))
        (sc:make-span-unstyled string))))

(defun xcbuild-emit (console &rest spans)
  "Emit a single log line built from SPANS above CONSOLE's canvas. When
*xcbuild-emit-prefix* is set, prepend a dim [label] tag so parallel diagnostics
stay attributable."
  (when *xcbuild-emit-prefix*
    (setf spans (cons (xcbuild-span (format nil "[~A] " *xcbuild-emit-prefix*) #'sc:dim)
                      spans)))
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

(defun xcbuild-cache-hits (stats)
  "Return (values HITS TOTAL) for reused (up-to-date) tasks, or NIL when the
build-graph total is unknown (e.g. text mode). HITS is the total task count
minus the tasks that actually ran."
  (let ((total (xcbuild-stats-total-tasks stats))
        (executed (xcbuild-stats-executed stats)))
    (when (plusp total)
      (values (max 0 (- total executed)) total))))

(defun xcbuild-cache-hits-string (stats)
  "Cache-hit summary like \"Cache hits: 87% (412/470)\", or NIL when display is
disabled or no task total is known yet."
  (when (xcbuild-stats-show-cache-hits stats)
    (multiple-value-bind (hits total) (xcbuild-cache-hits stats)
      (when total
        (format nil "Cache hits: ~D% (~D/~D)" (round (* 100 hits) total) hits total)))))

(defun xcbuild-counts-string (stats)
  "buck2-style job summary, e.g.
\"Jobs: In progress: 8. Finished: 412. Warnings: 1. Errors: 0. Time elapsed: 31.3s\".
Note: target counters are deliberately NOT shown here -- BUILD_TARGET_STARTED
arrives incrementally, so a mid-build \"K/N\" ratio would have a denominator
that keeps climbing. Target counts surface in the post-build final summary."
  (format nil "~@[~A · ~]Jobs: In progress: ~D. Finished: ~D.~@[ Skipped: ~D.~]~@[ Warnings: ~D.~]~@[ Errors: ~D.~]~@[ Tests: ~A.~]~@[ Progress: ~A.~]~@[ ~A.~] Time elapsed: ~,1Fs"
          (xcbuild-phase-label stats)
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
          (xcbuild-cache-hits-string stats)
          (xcbuild-elapsed (xcbuild-stats-start-time stats))))

(defun xcbuild-final-summary (stats)
  "One-line completion summary for the final frame, e.g.
\"(417 compiled, 6 targets, 1 warning, 0 errors) in 31.3s\"."
  (format nil "(~D compiled~@[, ~D skipped~]~@[, ~D target~:P~], ~D warning~:P, ~D error~:P~@[, tests ~A~]~@[, ~A~]) in ~,1Fs"
          (xcbuild-stats-compiled stats)
          (when (plusp (xcbuild-stats-skipped stats)) (xcbuild-stats-skipped stats))
          (when (plusp (xcbuild-stats-targets-ended stats))
            (xcbuild-stats-targets-ended stats))
          (xcbuild-stats-warnings stats)
          (xcbuild-stats-errors stats)
          (when (or (plusp (xcbuild-stats-tests-passed stats))
                    (plusp (xcbuild-stats-tests-failed stats)))
            (format nil "~D✓/~D✗"
                    (xcbuild-stats-tests-passed stats)
                    (xcbuild-stats-tests-failed stats)))
          (xcbuild-cache-hits-string stats)
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
              (suffix (xcbuild-final-summary stats))
              (success-p (eq (xcbuild-stats-result stats) :success))
              (color (if success-p #'sc:green #'sc:red))
              (verb (if success-p "Succeeded" "Failed")))
         ;; The xcodebuild-style '** ACTION SUCCEEDED/FAILED **' banner is now
         ;; printed by run-xcodebuild after report-result-bundles, so the
         ;; verdict sits below any result-bundle section. The canvas closes
         ;; with the per-cell summary line only.
         (sc:make-lines
          (list (sc:make-line
                 (list (xcbuild-span (format nil "   ~A ~A " noun verb) color #'sc:bold)
                       (xcbuild-span suffix #'sc:dim))))))))))

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
      ;; Swift Package Manager resolution: the kickoff line scrolls as a bold
      ;; header; each per-package activity line drives a live row; the completion
      ;; summary retires all resolution rows. Cold (uncached) resolution is the
      ;; one pre-build phase that can run for a while with nothing else going on,
      ;; so surfacing it keeps the dashboard from looking hung.
      (:package-resolve-start
       (xcbuild-emit console (xcbuild-span text #'sc:bold)))
      (:package-fetch
       (xcbuild-bump-package stats (xcbuild-package-name text)
                             (xcbuild-package-kind text)))
      (:package-resolved
       (xcbuild-retire-packages stats))
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
       ;; running test suites, and flip the phase so the header/banner relabel.
       (setf (xcbuild-stats-jobs stats) nil)
       (setf (xcbuild-stats-phase stats) :test)
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
       ;; Retire the suite's live-dashboard row so completed suites don't linger
       ;; until the sliding window evicts them. The run-level `Executed N tests'
       ;; rollup returns NIL and is left as a scrollback-only line.
       (xcbuild-retire-suite stats (xcbuild-test-summary-suite-name text))
       (xcbuild-emit console (xcbuild-span "    ") (xcbuild-span text #'sc:dim)))
      ((:result-success :result-failure)
       (xcbuild-emit console
                     (xcbuild-span (xcbuild-result-banner-text stats text) #'sc:bold)))
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

(defparameter *xcbuild-swb-text-types*
  '(:testing-start :test-suite-start :test-case-passed :test-case-failed
    :test-suite-summary :result-success :result-failure
    :package-resolve-start :package-fetch :package-resolved)
  "Classified line types the SWB dashboard accepts from xcodebuild's text stream.
In --use-swb mode the build graph is driven by protocol events, but the
test-execution and SPM-resolution phases emit no SWB events; these line types
let those phases still drive the dashboard. Build-graph types
(:compile/:link/:warning/...) are excluded here so they are not double-counted
against the protocol events.")

(defun xcbuild-swb-text-event (line)
  "Classify a raw xcodebuild text LINE for the SWB dashboard, returning the
classified event plist only when LINE is a test-execution line the SWB path
should apply (see *xcbuild-swb-text-types*); otherwise NIL."
  (let ((event (classify-xcodebuild-line line)))
    (when (member (getf event :type) *xcbuild-swb-text-types*)
      event)))

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
      (incf (xcbuild-stats-executed stats))
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

(defun swb-diag-location (h)
  "Pull (PATH LINE COLUMN) out of a BUILD_DIAGNOSTIC_EMITTED `location' hash.
Swift's default Codable encoding for the location enum yields one of
  {\"unknown\":{}}
  {\"path\":{\"_0\":\"<file>\",\"fileLocation\":null}}
  {\"path\":{\"_0\":\"<file>\",\"fileLocation\":{\"textual\":{\"line\":N,\"column\":N}}}}
so we probe path → fileLocation → textual defensively, returning whatever
fields are present. PATH is a non-empty string or NIL; LINE/COLUMN are
positive integers or NIL."
  (let* ((loc (and (hash-table-p h) (gethash "location" h)))
         (path-cell (and (hash-table-p loc) (gethash "path" loc)))
         (path (and (hash-table-p path-cell) (gethash "_0" path-cell)))
         (file-loc (and (hash-table-p path-cell) (gethash "fileLocation" path-cell)))
         (textual (and (hash-table-p file-loc) (gethash "textual" file-loc)))
         (line (and (hash-table-p textual) (gethash "line" textual)))
         (col (and (hash-table-p textual) (gethash "column" textual))))
    (values (and (stringp path) (plusp (length path)) path)
            (and (integerp line) (plusp line) line)
            (and (integerp col) (plusp col) col))))

(defun swb-diag-location-prefix (path line column)
  "Format `path:line:col: ' (with whatever fields are non-NIL) or NIL when
PATH is NIL. Mirrors the leading prefix clang/swift bake into text-mode
diagnostics, so SWB-mode scrollback reads the same way."
  (when path
    (with-output-to-string (out)
      (write-string path out)
      (when line
        (format out ":~D" line)
        (when column (format out ":~D" column)))
      (write-string ": " out))))

(defun xcbuild-swb-diagnostic (console stats arg)
  "Handle a BUILD_DIAGNOSTIC_EMITTED payload. kind 1 = warning, 2 = error;
0 (note) is ignored to keep the scrollback quiet. When the payload carries a
source location, prepend `path:line:col: ' to the scrolled text so users see
the same `file.swift:42:10: warning: foo' shape they get in text mode."
  (let ((h (swb-arg-json arg)))
    (when h
      (let ((kind (gethash "kind" h))
            (msg (xcbuild-clean (or (gethash "message" h) ""))))
        (multiple-value-bind (path line column) (swb-diag-location h)
          (let* ((prefix (swb-diag-location-prefix path line column))
                 (full (if prefix (concatenate 'string prefix msg) msg)))
            (cond
              ((and (eql kind 1) (xcbuild-swb-new-diag-p stats kind full))
               (incf (xcbuild-stats-warnings stats))
               (when console
                 (xcbuild-emit console (xcbuild-span "    warning " #'sc:yellow #'sc:bold)
                               (xcbuild-span full))))
              ((and (eql kind 2) (xcbuild-swb-new-diag-p stats kind full))
               (incf (xcbuild-stats-errors stats))
               (when console
                 (xcbuild-emit console (xcbuild-span "      error " #'sc:red #'sc:bold)
                               (xcbuild-span full #'sc:red)))))))))))

(defun xcbuild-progress-total (string)
  "Parse the denominator from a \"completed / total\" progress STRING, or NIL.
PARSE-INTEGER skips the leading whitespace after the slash."
  (let ((slash (position #\/ string)))
    (when slash
      (ignore-errors (parse-integer string :start (1+ slash) :junk-allowed t)))))

(defun xcbuild-swb-progress (stats arg)
  "Update the overall progress string from a BUILD_PROGRESS_UPDATED payload
ARG = #(target \"completed / total\" percent ...). Also tracks the largest task
total seen, which is the build-graph size used for the cache-hit percentage."
  (when (and (vectorp arg) (>= (length arg) 2) (stringp (aref arg 1)))
    (let ((s (xcbuild-clean (aref arg 1))))
      (setf (xcbuild-stats-progress stats) s)
      (let ((total (xcbuild-progress-total s)))
        (when (and total (> total (xcbuild-stats-total-tasks stats)))
          (setf (xcbuild-stats-total-tasks stats) total))))))

(defun xcbuild-swb-build-ended (stats arg)
  "Record the build result from a BUILD_OPERATION_ENDED payload
\(JSON {\"status\":N}, 0 = success). The exit code remains authoritative."
  (let ((h (swb-arg-json arg)))
    (when h
      (let ((status (gethash "status" h)))
        (when (numberp status)
          (setf (xcbuild-stats-result stats)
                (if (zerop status) :success :failure)))))))

(defun xcbuild-swb-ensure-unknown-table (stats)
  "Allocate STATS's unknown-msgs hash-table on first use; return it. Lets the
common (no --swb-trace) path skip the allocation entirely."
  (or (xcbuild-stats-unknown-msgs stats)
      (setf (xcbuild-stats-unknown-msgs stats) (make-hash-table :test 'equal))))

(defun xcbuild-swb-unknown (console stats event)
  "Scroll a dim trace line the first time a wire message NAME is seen, and bump
its repeat count thereafter. Driven by --swb-trace / CUPERTINO_SWB_TRACE."
  (let ((name (getf event :name)))
    (when (stringp name)
      (let* ((table (xcbuild-swb-ensure-unknown-table stats))
             (prior (gethash name table 0)))
        (setf (gethash name table) (1+ prior))
        (when (and (zerop prior) console)
          (xcbuild-emit console
                        (xcbuild-span "    swb-trace " #'sc:dim #'sc:bold)
                        (xcbuild-span name #'sc:dim)))))))

(defparameter *xcbuild-trace-digest-cap* 8
  "Maximum number of (name . count) entries shown inline in the SWB trace
digest line. Overflow becomes `… (and N more)`; the full list still lives
in the --swb-trace-file transcript when one was requested.")

(defun xcbuild-stats-unknown-entries (stats)
  "Sorted ((NAME . COUNT) ...) for STATS's unknown-msgs table, hottest first.
Returns NIL if the table was never allocated."
  (let ((table (xcbuild-stats-unknown-msgs stats)))
    (when table
      (let (acc)
        (maphash (lambda (k v) (push (cons k v) acc)) table)
        (sort acc #'> :key #'cdr)))))

(defun xcbuild-trace-digest-body (entries console-outputs)
  "Format a comma-separated digest body, capped at *XCBUILD-TRACE-DIGEST-CAP*.
Returns NIL when ENTRIES is empty and CONSOLE-OUTPUTS is zero."
  (let ((all (append (mapcar (lambda (e) (format nil "~A ×~D" (car e) (cdr e)))
                             entries)
                     (when (plusp console-outputs)
                       (list (format nil "console-output ×~D" console-outputs))))))
    (cond ((null all) nil)
          ((<= (length all) *xcbuild-trace-digest-cap*)
           (format nil "~{~A~^, ~}" all))
          (t (format nil "~{~A~^, ~}, … (and ~D more)"
                     (subseq all 0 *xcbuild-trace-digest-cap*)
                     (- (length all) *xcbuild-trace-digest-cap*))))))

(defun xcbuild-emit-trace-combined (console all-stats)
  "Cross-cell digest: sum unknown-msg counts and BUILD_CONSOLE_OUTPUT counters
across every cell's stats and scroll a single 'swb-trace combined:' line. A
no-op when ALL-STATS is a single cell (the per-cell line already covers it),
or when no trace data accrued, or when CONSOLE is NIL."
  (when (and console (rest all-stats))
    (let* ((combined (make-xcbuild-stats :action ""))
           (table nil))
      (dolist (s all-stats)
        (incf (xcbuild-stats-console-outputs combined)
              (xcbuild-stats-console-outputs s))
        (let ((src (xcbuild-stats-unknown-msgs s)))
          (when src
            (unless table
              (setf table (xcbuild-swb-ensure-unknown-table combined)))
            (maphash (lambda (k v) (incf (gethash k table 0) v)) src))))
      (xcbuild-emit-trace-digest console combined
                                 :prefix "swb-trace combined: "))))

(defun xcbuild-combined-banner-text (action success-p)
  "Return the '** ACTION SUCCEEDED **' / '** ACTION FAILED **' verdict string
for ACTION (case-insensitive); ACTION is uppercased to match xcodebuild's own
banner verbiage (BUILD/TEST/CLEAN/ARCHIVE)."
  (format nil "** ~A ~A **"
          (string-upcase action)
          (if success-p "SUCCEEDED" "FAILED")))

(defun print-build-banner (action success-p &optional (stream t))
  "Print the bold green/red '** ACTION SUCCEEDED/FAILED **' verdict line to
STREAM (default *standard-output*). Called by run-xcodebuild after
report-result-bundles so the verdict is the very last line of the build."
  (let ((text (xcbuild-combined-banner-text action success-p))
        (code (if success-p "32" "31")))
    (format stream "~C[1;~Am~A~C[0m~%" #\Escape code text #\Escape)))

(defun xcbuild-emit-trace-digest (console stats &key trace-file (prefix "swb-trace digest: "))
  "After the build, scroll a one-line summary of every wire message the proxy
emitted that we did not handle, plus the BUILD_CONSOLE_OUTPUT count if nonzero.
When TRACE-FILE is non-NIL it is mentioned so the user knows where the full
transcript landed. PREFIX lets the combined-cells rollup use a distinct label.
A no-op when no trace data accrued or CONSOLE is NIL."
  (when console
    (let* ((entries (xcbuild-stats-unknown-entries stats))
           (console-outputs (xcbuild-stats-console-outputs stats))
           (body (xcbuild-trace-digest-body entries console-outputs)))
      (when (or body trace-file)
        (xcbuild-emit console
                      (xcbuild-span (concatenate 'string "    " prefix) #'sc:dim #'sc:bold)
                      (xcbuild-span (or body "(no untyped events)") #'sc:dim))
        (when trace-file
          (xcbuild-emit console
                        (xcbuild-span "    swb-trace file:   " #'sc:dim #'sc:bold)
                        (xcbuild-span (namestring trace-file) #'sc:dim)))))))

(defun xcbuild-handle-event (console stats event)
  "Apply one decoded SWB EVENT (a plist from swb:frame->event) to STATS,
emitting scrollback via CONSOLE for diagnostics. The :BUILD-STARTED event is a
no-op (the dashboard start-time is captured at stats construction). The
:CONSOLE-OUTPUT event is counted only -- under SWB mode xcodebuild's stdout
usually carries the same lines, so emitting here would double-print; the count
is surfaced in the end-of-build trace digest so users running --swb-trace can
notice if the assumption fails (e.g. a high count with empty xcodebuild output
would warrant decoding the payload)."
  (let ((arg (first (getf event :args))))
    (case (getf event :type)
      (:build-started nil)
      (:target-started (incf (xcbuild-stats-targets-started stats)))
      (:target-ended   (incf (xcbuild-stats-targets-ended stats)))
      (:task-started   (xcbuild-swb-task-started stats arg))
      (:task-ended     (xcbuild-swb-task-ended stats arg))
      (:diagnostic     (xcbuild-swb-diagnostic console stats arg))
      (:console-output (incf (xcbuild-stats-console-outputs stats)))
      (:progress       (xcbuild-swb-progress stats arg))
      (:build-ended    (xcbuild-swb-build-ended stats arg))
      (:unknown        (xcbuild-swb-unknown console stats event))
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
        ;; reading the stream errors out partway through. The verdict banner is
        ;; the second line of xcbuild-progress's :final canvas.
        (when (sc:superconsole-output console)
          (sc:superconsole-finalize console progress))))))
