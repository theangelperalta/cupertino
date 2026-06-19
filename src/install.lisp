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
  "Print LABEL with CMD-STR then run CMD-STR with interactive I/O.
Exits with the process exit code on failure."
  (format t "~A ~A~%" (colored-text (format nil "~A:" label) :cyan) cmd-str)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program cmd-str :output :interactive :error-output :interactive
                                :ignore-error-status t)
    (declare (ignore output error-output))
    (unless (zerop exit-code)
      (cup-error "command failed with exit code ~A." exit-code)
      (uiop:quit exit-code))))

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
  (format t "~A ~A~%" (colored-text "Booting simulator" :cyan) udid)
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (format nil "xcrun simctl boot '~A'" udid)
                        :output :string :error-output :string
                        :ignore-error-status t)
    (declare (ignore output))
    (cond
      ((zerop exit-code)
       (format t "~A~%" (colored-text "Simulator booted." :green)))
      ((search "current state: Booted" error-output)
       (format t "~A~%" (colored-text "Simulator already booted." :green)))
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

;; Xcode's debug console shows print + NSLog + os_log/Logger together, in one
;; format, with private values revealed. It achieves this by launching the app
;; with OS_ACTIVITY_DT_MODE=YES, which makes the unified-logging system mirror
;; os_log/Logger output to the process's stderr (in development-tools format,
;; unredacted) instead of only the system log. We do the same: setting that env
;; var on the launch — passed through via simctl's SIMCTL_CHILD_ / devicectl's
;; DEVICECTL_CHILD_ prefix — gives Xcode-identical console output from the
;; single stdout/stderr stream that --console / --console-pty already captures.
;; This is why no separate `log stream' is needed: a side stream would use a
;; different format, add system noise, redact private values, and duplicate
;; NSLog (which already mirrors to stderr).
(defparameter +activity-dt-mode-env+ "OS_ACTIVITY_DT_MODE=YES"
  "Environment assignment that makes os_log mirror to stderr like Xcode does.")

(defun launch-on-simulator (udid bundle-id)
  "Launch an app on a booted simulator by bundle identifier.
Sets OS_ACTIVITY_DT_MODE so os_log/Logger output mirrors to the console
alongside stdout and NSLog, matching Xcode's debug console; --console-pty
provides a pty so output stays line-buffered."
  (focus-simulator)
  (run-interactive "Launching on simulator"
                   (format nil "SIMCTL_CHILD_~A xcrun simctl launch --console-pty --terminate-running-process '~A' '~A'"
                           +activity-dt-mode-env+ udid bundle-id)))

(defun launch-on-device (device-id bundle-id)
  "Launch an app on a physical device by bundle identifier.
Sets OS_ACTIVITY_DT_MODE so os_log/Logger output mirrors to the --console
stream alongside stdout and NSLog, matching Xcode's debug console."
  (run-interactive "Launching on device"
                   (format nil "DEVICECTL_CHILD_~A xcrun devicectl device process launch --console --terminate-existing --device '~A' '~A'"
                           +activity-dt-mode-env+ device-id bundle-id)))

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
        (cup-error "Could not determine .app path from build settings.")
        (uiop:quit 1))
      (unless bundle-id
        (cup-error "Could not determine bundle identifier from build settings.")
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
