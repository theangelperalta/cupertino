(in-package :cupertino)

(defun resolve-destination (cmd model)
  "Resolve the xcodebuild destination from CLI options or model."
  (let ((sim (clingon:getopt cmd :sim))
        (device (clingon:getopt cmd :device)))
    (cond
      (sim (format nil "platform=iOS Simulator,name=~A" sim))
      (device (format nil "id=~A" device))
      ((model-sim model)
       (format nil "platform=iOS Simulator,name=~A" (model-sim model)))
      ((model-device model)
       (format nil "id=~A" (model-device model)))
      (t "platform=iOS Simulator,name=iPhone 16 Pro"))))

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
Resolves the scheme from CLI options or model accessors in order."
  (let* ((path (first (clingon:command-arguments cmd)))
         (model (model:make-cupertino-model path))
         (project-flag (resolve-project-flag cmd model))
         (destination (resolve-destination cmd model))
         (configuration (clingon:getopt cmd :configuration))
         (scheme (or (clingon:getopt cmd :scheme)
                     (some (lambda (fn) (funcall fn model)) scheme-accessors))))
    (unless scheme
      (format *error-output* "Error: No scheme specified and no default scheme configured.~%")
      (clingon:print-usage-and-exit cmd t))
    (let ((cmd-str (format nil "xcodebuild ~A -scheme '~A' -destination '~A' -configuration ~A ~A"
                           project-flag scheme destination configuration action)))
      (format t "Running: ~A~%" cmd-str)
      (uiop:run-program cmd-str :output :interactive :error-output :interactive))))

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
    :description "simulator destination (e.g., \"iPhone 16 Pro\")"
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
    :key :configuration)))
