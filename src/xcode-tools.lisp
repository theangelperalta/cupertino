(in-package :cupertino)

(defun resolve-destination (cmd model)
  "Resolve the xcodebuild destination from CLI options or model."
  (let ((sim (clingon:getopt cmd :sim))
        (device (clingon:getopt cmd :device)))
    (cond
      (sim (format nil "platform=iOS Simulator,id=~A" sim))
      (device "generic/platform=iOS")
      ((model-sim model)
       (format nil "platform=iOS Simulator,id=~A" (model-sim model)))
      ((model-device model) "generic/platform=iOS")
      (t nil))))

(defun resolve-project-flag (cmd model)
  "Resolve the -workspace/-project flag from CLI path or model."
  (let* ((path (first (clingon:command-arguments cmd)))
         (project-type (or (when path (get-project-file-type path))
                           (model-project-type model)))
         (project-path (or path (model-project-path model))))
    (cond
      ((and (eq project-type :workspace) project-path)
       (format nil "-workspace '~A'" project-path))
      ((and (eq project-type :project) project-path)
       (format nil "-project '~A'" project-path))
      (t ""))))

(defun run-xcodebuild (cmd action &key (scheme-accessors (list #'model-scheme)))
  "Run xcodebuild with the given action (build, test, clean, etc.).
Resolves the scheme from CLI options or model accessors in order.
When --derived-data is provided, appends -derivedDataPath to the command."
  (let* ((path (first (clingon:command-arguments cmd)))
         (model (model:make-cupertino-model path))
         (project-flag (resolve-project-flag cmd model))
         (destination (resolve-destination cmd model))
         (configuration (clingon:getopt cmd :configuration))
         (derived-data-path (clingon:getopt cmd :derived-data))
         (scheme (or (clingon:getopt cmd :scheme)
                     (some (lambda (fn) (funcall fn model)) scheme-accessors))))
    (unless scheme
      (format *error-output* "~A No scheme specified and no default scheme configured.~%"
              (colored-text "Error:" :red))
      (clingon:print-usage-and-exit cmd t))
    (unless destination
      (format *error-output* "~A No simulator or device specified and none configured.~%"
              (colored-text "Error:" :red))
      (clingon:print-usage-and-exit cmd t))
    (let ((cmd-str (format nil "xcodebuild ~A -scheme '~A' -destination '~A' -configuration ~A~@[ -derivedDataPath '~A'~] ~A"
                           project-flag scheme destination configuration derived-data-path action)))
      (format t "~A ~A~%" (colored-text "Running:" :cyan) cmd-str)
      (let* ((*xcbuild-max-jobs* (or (model-max-jobs model) *xcbuild-max-jobs*))
             (*xcbuild-slow-threshold* (or (model-slow-threshold model)
                                           *xcbuild-slow-threshold*))
             (*xcbuild-show-cache-hits* (or (clingon:getopt cmd :cache-hits)
                                            (model-cache-hits model)))
             ;; Cache-hit counts come only from SWB protocol events, so showing
             ;; them implies interception mode.
             (want-swb (or (clingon:getopt cmd :use-swb) (model-use-swb model)
                           *xcbuild-show-cache-hits*))
             (console (sc:make-superconsole))
             (real (and want-swb console (resolve-swb-service)))
             (self (and real (swb-self-executable)))
             (exit-code (cond
                          ((and console real self (swb-usable-p self))
                           (run-xcodebuild/swb cmd-str action console real self))
                          (console
                           (when want-swb
                             (format *error-output* "~A Swift Build interception unavailable; using text mode.~%"
                                     (colored-text "Note:" :yellow)))
                           (run-xcodebuild/pretty cmd-str action console))
                          (t (run-xcodebuild/raw cmd-str)))))
        (unless (zerop exit-code)
          (format *error-output* "~A xcodebuild ~A failed with exit code ~A.~%"
                  (colored-text "Error:" :red) action exit-code)
          (uiop:quit exit-code))))))

(defun run-xcodebuild/raw (cmd-str)
  "Run CMD-STR with raw interactive I/O (the non-TTY fallback). Return the exit code."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program cmd-str :output :interactive :error-output :interactive
                                :ignore-error-status t)
    (declare (ignore output error-output))
    exit-code))

;;; ---------------------------------------------------------------------------
;;; Swift Build (SWBBuildService) interception mode
;;; ---------------------------------------------------------------------------

(defun resolve-swb-service ()
  "Absolute path to the active Xcode's real SWBBuildService executable, or NIL."
  (let* ((dev (ignore-errors
                (string-trim '(#\Newline #\Space)
                             (uiop:run-program '("xcode-select" "-p")
                                               :output :string))))
         (svc (and dev (plusp (length dev))
                   (format nil "~A/../SharedFrameworks/SwiftBuild.framework/Versions/A/PlugIns/SWBBuildService.bundle/Contents/MacOS/SWBBuildService"
                           dev))))
    (when (and svc (probe-file svc))
      (namestring (truename svc)))))

(defun swb-self-executable ()
  "Path to the running Cupertino image, used as the SWBBUILDSERVICE_PATH proxy."
  (ignore-errors (namestring sb-ext:*runtime-pathname*)))

(defun swb-usable-p (self)
  "True when SELF is our dumped image (a Mach-O the build service can spawn),
not a bare Lisp runtime where proxy mode wouldn't dispatch to us."
  (and self
       (not (member (pathname-name (pathname self))
                    '("sbcl" "ros" "lisp" "ccl") :test #'equal))))

(defun swb-events-tempfile ()
  "A fresh temp path the proxy appends decoded events to."
  (merge-pathnames (format nil "cupertino-swb-~36R-~36R.events"
                           (get-universal-time) (random 1000000))
                   (uiop:temporary-directory)))

(defun swb-consume-events (path done-fn handler)
  "Tail PATH, reading one event form at a time and passing each to HANDLER.
Tolerates partial writes (a form not yet fully flushed) by rewinding and
retrying. Returns once DONE-FN is true and no complete forms remain."
  (with-open-file (s path :direction :input :if-does-not-exist :create)
    (let ((*read-eval* nil))
      (loop
        (let* ((pos (file-position s))
               (form (handler-case (read s nil :eof)
                       (end-of-file () :partial)
                       (error () :skip))))
          (cond
            ((eq form :eof)
             (when (funcall done-fn) (return))
             (sleep 0.03))
            ((eq form :partial)
             (file-position s pos)
             (if (funcall done-fn) (return) (sleep 0.03)))
            ((eq form :skip)
             (read-line s nil :eof))
            (t (ignore-errors (funcall handler form)))))))))

(defun run-xcodebuild/swb (cmd-str action console real-service self-exe)
  "Run CMD-STR with the dashboard driven by Swift Build protocol events.
Points SWBBUILDSERVICE_PATH at SELF-EXE (this image, which re-spawns
REAL-SERVICE and tees events) and renders them on CONSOLE. Returns the
xcodebuild exit code."
  (let* ((stats (make-xcbuild-stats :action action))
         (progress (make-instance 'xcbuild-progress :stats stats))
         (events-file (swb-events-tempfile))
         (env-cmd (format nil "env SWBBUILDSERVICE_PATH='~A' ~A='~A' ~A='~A' ~A"
                          self-exe
                          swb:+real-service-env+ real-service
                          swb:+events-path-env+ (namestring events-file)
                          cmd-str))
         (proc (uiop:launch-program env-cmd
                                    :output :stream
                                    :error-output :output
                                    :input :interactive))
         (out (uiop:process-info-output proc))
         (lock (bt:make-lock "xcbuild-render"))
         (done nil)
         (render-thread nil) (event-thread nil) (drain-thread nil))
    ;; Create the file up front so the reader can open it before the proxy does.
    (ignore-errors (close (open events-file :direction :output
                                            :if-does-not-exist :create
                                            :if-exists :append)))
    (flet ((render ()
             (bt:with-lock-held (lock)
               (sc:superconsole-render console progress))))
      ;; Drain xcodebuild's own stdout so its pipe never blocks; the dashboard
      ;; is driven entirely by protocol events, so the text is discarded.
      (setf drain-thread
            (bt:make-thread
             (lambda () (loop for l = (read-line out nil :eof) until (eq l :eof)))
             :name "xcbuild-drain"))
      (setf event-thread
            (bt:make-thread
             (lambda ()
               (swb-consume-events
                events-file (lambda () done)
                (lambda (event)
                  (bt:with-lock-held (lock)
                    (xcbuild-handle-event console stats event)
                    (sc:superconsole-render console progress)))))
             :name "xcbuild-events"))
      (setf render-thread
            (bt:make-thread
             (lambda ()
               (loop until done
                     do (sleep *xcbuild-render-interval*)
                        (unless done (render))))
             :name "xcbuild-render"))
      (unwind-protect
           (let ((exit-code (uiop:wait-process proc)))
             (setf done t)
             (setf (xcbuild-stats-result stats)
                   (if (zerop exit-code) :success :failure))
             exit-code)
        (setf done t)
        (ignore-errors (bt:join-thread drain-thread))
        (ignore-errors (bt:join-thread event-thread))
        (when render-thread (ignore-errors (bt:join-thread render-thread)))
        (ignore-errors (delete-file events-file))
        (when (sc:superconsole-output console)
          (sc:superconsole-finalize console progress))))))

(defun make-xcodebuild-command (name description &key (scheme-accessors (list #'model-scheme)))
  "Create a clingon command that runs xcodebuild with the given action."
  (clingon:make-command
   :name name
   :description description
   :options (xcodebuild-options name)
   :handler (lambda (cmd) (run-xcodebuild cmd name :scheme-accessors scheme-accessors))))

(defun xcodebuild-options (action-description)
  "Return common xcodebuild CLI options with ACTION-DESCRIPTION for the scheme."
  (list
   (clingon:make-option
    :string
    :description (format nil "scheme to ~A" action-description)
    :short-name #\s
    :long-name "scheme"
    :key :scheme)
   (clingon:make-option
    :string
    :description "simulator UDID destination"
    :long-name "sim"
    :key :sim)
   (clingon:make-option
    :string
    :description "device UDID destination"
    :long-name "device"
    :key :device)
   (clingon:make-option
    :string
    :description (format nil "~A configuration (Debug or Release)" action-description)
    :short-name #\c
    :long-name "configuration"
    :initial-value "Debug"
    :key :configuration)
   (clingon:make-option
    :string
    :description "custom derived data path"
    :long-name "derived-data"
    :key :derived-data)
   (clingon:make-option
    :flag
    :description "use Swift Build protocol interception for richer progress"
    :long-name "use-swb"
    :key :use-swb)
   (clingon:make-option
    :flag
    :description "show the cache-hit (up-to-date task) percentage (implies --use-swb)"
    :long-name "cache-hits"
    :key :cache-hits)))
