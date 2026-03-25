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
  (let ((cmd-str (format nil "xcrun simctl install '~A' '~A'"
                         udid (namestring app-path))))
    (format t "Installing to simulator: ~A~%" cmd-str)
    (uiop:run-program cmd-str :output :interactive :error-output :interactive)))

(defun install-to-device (device-id app-path)
  "Install an app bundle to a physical device using xcrun devicectl."
  (let ((cmd-str (format nil "xcrun devicectl device install app --device '~A' '~A'"
                         device-id (namestring app-path))))
    (format t "Installing to device: ~A~%" cmd-str)
    (uiop:run-program cmd-str :output :interactive :error-output :interactive)))

(defun focus-simulator ()
  "Bring the Simulator app to the foreground."
  (uiop:run-program "open -a Simulator" :ignore-error-status t))

(defun launch-on-simulator (udid bundle-id)
  "Launch an app on a booted simulator by bundle identifier."
  (focus-simulator)
  (let ((cmd-str (format nil "xcrun simctl launch --terminate-running-process '~A' '~A'" udid bundle-id)))
    (format t "Launching on simulator: ~A~%" cmd-str)
    (uiop:run-program cmd-str :output :interactive :error-output :interactive)))

(defun launch-on-device (device-id bundle-id)
  "Launch an app on a physical device by bundle identifier."
  (let ((cmd-str (format nil "xcrun devicectl device process launch --console --terminate-existing --device '~A' '~A'"
                         device-id bundle-id)))
    (format t "Launching on device: ~A~%" cmd-str)
    (uiop:run-program cmd-str :output :interactive :error-output :interactive)))

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
         (derived-data-path (clingon:getopt cmd :derived-data))
         (project-flag (resolve-project-flag cmd model))
         (sdk (if device-id "iphoneos" "iphonesimulator"))
         (destination (resolve-destination cmd model)))
    (unless scheme
      (format *error-output* "Error: No scheme specified and no default scheme configured.~%")
      (clingon:print-usage-and-exit cmd t))
    (unless (or device-id sim-udid)
      (format *error-output* "Error: No simulator or device specified and none configured.~%")
      (clingon:print-usage-and-exit cmd t))
    ;; Build with derivedDataPath so we know where the .app lands
    (let ((build-cmd (format nil "xcodebuild ~A -scheme '~A' -destination '~A' -configuration ~A~@[ -derivedDataPath '~A'~] build"
                             project-flag scheme destination configuration
                             derived-data-path)))
      (format t "Building: ~A~%" build-cmd)
      (uiop:run-program build-cmd :output :interactive :error-output :interactive))
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
          (install-to-device device-id app-path)
          (install-to-simulator sim-udid app-path))
      ;; Launch the installed app
      (if device-id
          (launch-on-device device-id bundle-id)
          (launch-on-simulator sim-udid bundle-id)))))

(defun install/command ()
  "A command to build and install the app to a simulator or device."
  (clingon:make-command
   :name "install"
   :description "Build and install the app to a simulator or device"
   :options (install/options)
   :handler #'install/handler))

(defun install/options ()
  "Returns the options for the `install' command."
  (append
   (xcodebuild-options "install")
   (list
    (clingon:make-option
     :string
     :description "custom derived data path for the build"
     :long-name "derived-data"
     :key :derived-data))))
