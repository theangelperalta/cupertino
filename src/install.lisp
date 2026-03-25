(in-package :cupertino)

(defun get-build-setting (output key)
  "Extract a build setting value for KEY from xcodebuild -showBuildSettings OUTPUT."
  (let ((prefix (format nil "    ~A = " key)))
    (dolist (line (str:split #\Newline output))
      (when (str:starts-with-p prefix line)
        (return (str:trim (subseq line (length prefix))))))))

(defun get-build-settings (scheme configuration sdk xcworkspace-flag)
  "Run xcodebuild -showBuildSettings and return the raw output string."
  (let ((cmd (format nil "xcodebuild ~A -scheme '~A' -configuration ~A -sdk ~A -showBuildSettings"
                     xcworkspace-flag scheme configuration sdk)))
    (uiop:run-program cmd :output :string :error-output nil)))

(defun run-interactive (label cmd-str)
  "Print LABEL with CMD-STR then run CMD-STR with interactive I/O."
  (format t "~A: ~A~%" label cmd-str)
  (uiop:run-program cmd-str :output :interactive :error-output :interactive))

(defun resolve-sim-udid (cmd model)
  "Resolve the simulator UDID from CLI options or model."
  (or (clingon:getopt cmd :sim)
      (model-sim model)))

(defun resolve-device-destination (cmd model)
  "Resolve the device UDID from CLI options or model."
  (or (clingon:getopt cmd :device)
      (model-device model)))

(defun boot-simulator (udid)
  "Boot the simulator by UDID if it is not already booted."
  (format t "Booting simulator '~A'...~%" udid)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (format nil "xcrun simctl boot '~A'" udid)
                        :output :string :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (cond
      ((zerop exit-code)
       (format t "Simulator booted.~%"))
      ((search "current state: Booted" error-output)
       (format t "Simulator already booted.~%"))
      (t
       (format *error-output* "~A" error-output)
       (uiop:quit exit-code)))))

(defun install-to-simulator (udid app-path)
  "Boot the simulator if needed, then install an app bundle via xcrun simctl."
  (boot-simulator udid)
  (run-interactive "Installing to simulator"
                   (format nil "xcrun simctl install '~A' '~A'" udid (namestring app-path))))

(defun install-to-device (device-id app-path)
  "Install an app bundle to a physical device using xcrun devicectl."
  (run-interactive "Installing to device"
                   (format nil "xcrun devicectl device install app --device '~A' '~A'"
                           device-id (namestring app-path))))

(defun focus-simulator ()
  "Bring the Simulator app to the foreground."
  (uiop:run-program "open -a Simulator" :ignore-error-status t))

(defun launch-on-simulator (udid bundle-id)
  "Launch an app on a booted simulator by bundle identifier."
  (focus-simulator)
  (run-interactive "Launching on simulator"
                   (format nil "xcrun simctl launch --terminate-running-process '~A' '~A'" udid bundle-id)))

(defun launch-on-device (device-id bundle-id)
  "Launch an app on a physical device by bundle identifier."
  (run-interactive "Launching on device"
                   (format nil "xcrun devicectl device process launch --console --terminate-existing --device '~A' '~A'"
                           device-id bundle-id)))

(defun install/handler (cmd)
  "Handler for the `install' command.
Builds the project then installs the .app to the target simulator or device."
  (let* ((path (first (clingon:command-arguments cmd)))
         (model (model:make-cupertino-model path))
         (scheme (or (clingon:getopt cmd :scheme)
                     (model-scheme model)))
         (configuration (clingon:getopt cmd :configuration))
         (device-id (resolve-device-destination cmd model))
         (sim-udid (unless device-id (resolve-sim-udid cmd model)))
         (project-flag (resolve-project-flag cmd model))
         (sdk (if device-id "iphoneos" "iphonesimulator")))
    ;; Build using run-xcodebuild from xcode-tools
    (run-xcodebuild cmd "build")
    ;; Use -showBuildSettings to find app path and bundle ID
    (let* ((settings-output (get-build-settings scheme configuration sdk project-flag))
           (target-build-dir (get-build-setting settings-output "TARGET_BUILD_DIR"))
           (wrapper-name (or (get-build-setting settings-output "WRAPPER_NAME")
                             (get-build-setting settings-output "FULL_PRODUCT_NAME")))
           (bundle-id (get-build-setting settings-output "PRODUCT_BUNDLE_IDENTIFIER"))
           (app-path (when (and target-build-dir wrapper-name)
                       (merge-pathnames wrapper-name
                                        (format nil "~A/" target-build-dir)))))
      (unless app-path
        (format *error-output* "Error: Could not determine .app path from build settings.~%")
        (uiop:quit 1))
      (unless bundle-id
        (format *error-output* "Error: Could not determine bundle identifier from build settings.~%")
        (uiop:quit 1))
      (if device-id
          (progn (install-to-device device-id app-path)
                 (launch-on-device device-id bundle-id))
          (progn (install-to-simulator sim-udid app-path)
                 (launch-on-simulator sim-udid bundle-id))))))

(defun install/command ()
  "A command to build and install the app to a simulator or device."
  (clingon:make-command
   :name "install"
   :description "Build and install the app to a simulator or device"
   :options (install/options)
   :handler #'install/handler))

(defun install/options ()
  "Returns the options for the `install' command."
  (xcodebuild-options "build and install"))
