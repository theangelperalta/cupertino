(in-package :cupertino)

(defun info/top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (info/project/command)
   (info/sim/command)
   (info/device/command)))

(defun info/top-level/handler (cmd)
  "The handler for the top-level command. Will print the usage of the app"
  (clingon:print-usage-and-exit cmd t))

(defun info/top-level/command ()
  "Returns the top-level info command"
  (clingon:make-command :name "info"
                        :description "A command to display project/device information"
                        :long-description "A command to display project/device information"
                        :handler #'info/top-level/handler
                        :options (info/top-level/options)
                        :sub-commands (info/top-level/sub-commands)))

(defun info/top-level/options ()
  "Returns the options for the `info' command"
  (list))


(defun info/project/options ()
  (list
   (clingon:make-option
    :string
    :description "project file (e.g., HelloWorld.xcodeproj)"
    :short-name #\p
    :long-name "project"
    :key :project)
   (clingon:make-option
    :string
    :description "workspace file (e.g., HelloWorld.xcworkspace)"
    :short-name #\w
    :long-name "workspace"
    :key :workspace)))

(defun info/project/handler (cmd)
  "Handler for the `device/info' command"
  (let* ((path (first (clingon:command-arguments cmd)))
         (project-file-type (get-project-file-type path))
        (project (clingon:getopt cmd :project))
        (workspace (clingon:getopt cmd :workspace)))
    (if (and project workspace)
        (format *error-output* "~A You cannot specify both an Xcode project and a workspace.~%"
                (colored-text "Error:" :red))
        (cond (project (print-project-info ':project project))
        (workspace (print-project-info ':workspace workspace))
        (project-file-type (print-project-info project-file-type path))
        (t (clingon:print-usage-and-exit cmd t))))))

(defun info/project/command ()
  "A command to display physical device information"
  (clingon:make-command
   :name "project"
   :description "Display project information"
   :options (info/project/options)
   :handler #'info/project/handler))


(defun info/sim/handler (cmd)
  "Handler for the `sim/info' command"
  (declare (ignore cmd))
  (print-sim-info))

(defun info/sim/command ()
  "A command to display simulator device information"
  (clingon:make-command
   :name "sim"
   :description "Display simulator device information"
   :handler #'info/sim/handler))

(defun info/device/handler (cmd)
  "Handler for the `device/info' command"
  (declare (ignore cmd))
  (print-device-info))

(defun info/device/command ()
  "A command to display physical device information"
  (clingon:make-command
   :name "device"
   :description "Display physical device information"
   :handler #'info/device/handler))


;; Config command

(defun parse-positive-number (string what)
  "Parse STRING as a positive real number, or exit with an error mentioning WHAT."
  (let ((n (ignore-errors
            (let ((*read-default-float-format* 'double-float))
              (read-from-string string)))))
    (unless (and (realp n) (plusp n))
      (format *error-output* "~A ~A must be a positive number, got ~S.~%"
              (colored-text "Error:" :red) what string)
      (uiop:quit 1))
    n))

(defun parse-bool-arg (string what)
  "Parse STRING as a boolean (true/false/yes/no/on/off/1/0), or exit with an error."
  (cond
    ((member string '("true" "yes" "on" "1" "t") :test #'string-equal) t)
    ((member string '("false" "no" "off" "0" "nil") :test #'string-equal) nil)
    (t (format *error-output* "~A ~A must be true or false, got ~S.~%"
               (colored-text "Error:" :red) what string)
       (uiop:quit 1))))

(defun config/handler (cmd)
  "Handler for the `config' command.
With options, updates the config file. Without options, prints the current config."
  (let* ((path (first (clingon:command-arguments cmd)))
         (scheme (clingon:getopt cmd :scheme))
         (test-scheme (clingon:getopt cmd :test-scheme))
         (sim (clingon:getopt cmd :sim))
         (device (clingon:getopt cmd :device))
         (max-jobs (clingon:getopt cmd :max-jobs))
         (slow-threshold (let ((s (clingon:getopt cmd :slow-threshold)))
                           (when s (parse-positive-number s "slow-threshold"))))
         (use-swb-raw (clingon:getopt cmd :use-swb))
         (updates (append (when scheme (list :scheme scheme))
                          (when test-scheme (list :test-scheme test-scheme))
                          (when sim (list :sim sim))
                          (when device (list :device device))
                          (when max-jobs (list :max-jobs max-jobs))
                          (when slow-threshold (list :slow-threshold slow-threshold))
                          (when use-swb-raw
                            (list :use-swb (parse-bool-arg use-swb-raw "use-swb"))))))
    (if updates
        (let ((merged (model:update-model-config path updates)))
          (format t "~A ~A:~%"
                  (colored-text "Updated config at" :green)
                  (colored-text (namestring (model:resolve-config-file path)) :blue))
          (loop for (k v) on merged by #'cddr
                do (format t "  ~A: ~A~%" (colored-text k :cyan) v)))
        ;; No options — print current config
        (let ((plist (model:load-model path)))
          (format t "~A ~A:~%"
                  (colored-text "Config at" :white)
                  (colored-text (namestring (model:resolve-config-file path)) :blue))
          (loop for (k v) on plist by #'cddr
                do (format t "  ~A: ~A~%" (colored-text k :cyan) v))))))

(defun config/command ()
  "A command to view or update the project config."
  (clingon:make-command
   :name "config"
   :description "View or update project configuration"
   :options (config/options)
   :handler #'config/handler))

(defun config/options ()
  "Returns the options for the `config' command."
  (list
   (clingon:make-option
    :string
    :description "set the default build scheme"
    :short-name #\s
    :long-name "scheme"
    :key :scheme)
   (clingon:make-option
    :string
    :description "set the default test scheme"
    :short-name #\t
    :long-name "test-scheme"
    :key :test-scheme)
   (clingon:make-option
    :string
    :description "set the default simulator UDID"
    :long-name "sim"
    :key :sim)
   (clingon:make-option
    :string
    :description "set the default device UDID"
    :long-name "device"
    :key :device)
   (clingon:make-option
    :integer
    :description "max in-progress actions shown in the build dashboard"
    :long-name "max-jobs"
    :key :max-jobs)
   (clingon:make-option
    :string
    :description "seconds before a build action is highlighted as slow"
    :long-name "slow-threshold"
    :key :slow-threshold)
   (clingon:make-option
    :string
    :description "use Swift Build protocol interception (true or false)"
    :long-name "use-swb"
    :key :use-swb)))

;; Init command — interactive project setup

(defun prompt-choice (label items &key (display-fn #'identity) (allow-none nil))
  "Display a numbered list of ITEMS and prompt the user to pick one.
Returns the selected item, or NIL if ALLOW-NONE and the user enters nothing."
  (format t "~%~A:~%" (colored-text label :cyan))
  (loop for item in items
        for i from 1
        do (format t "  ~A ~A~%"
                   (colored-text (format nil "~D." i) :yellow)
                   (colored-text (funcall display-fn item) :green)))
  (when allow-none
    (format t "  ~A ~A~%"
            (colored-text "0." :yellow)
            (colored-text "(none)" :bright-black)))
  (loop
    (format t "~A " (colored-text (format nil "Select [~:[1~;0~]]:" allow-none) :white))
    (force-output)
    (let* ((line (str:trim (read-line *standard-input* nil "")))
           (n (handler-case (parse-integer line :junk-allowed nil)
                (error () nil))))
      (cond
        ((and (string= line "") (not allow-none) items)
         (return (first items)))
        ((and (string= line "") allow-none)
         (return nil))
        ((and n (= n 0) allow-none)
         (return nil))
        ((and n (>= n 1) (<= n (length items)))
         (return (nth (1- n) items)))
        (t (format t "Invalid choice, try again.~%"))))))

(defun collect-available-sims ()
  "Return a list of (name udid state) for available simulators."
  (let ((sim-info (list-sim-info))
        (results nil))
    (when sim-info
      (let ((devices (gethash "devices" sim-info)))
        (when devices
          (maphash (lambda (platform device-list)
                     (declare (ignore platform))
                     (dolist (d device-list)
                       (when (and (gethash "isAvailable" d)
                                  (not (gethash "availabilityError" d)))
                         (push (list (gethash "name" d)
                                     (gethash "udid" d)
                                     (gethash "state" d))
                               results))))
                   devices))))
    (nreverse results)))

(defun find-project-files (dir)
  "Recursively find all .xcworkspace and .xcodeproj directories under DIR.
Excludes matches inside .build, DerivedData, and Pods directories.
Returns a list of (type path) pairs where type is :workspace or :project."
  (let ((results nil))
    (labels ((excluded-p (path-str)
               (or (search "/.build/" path-str)
                   (search "/DerivedData/" path-str)
                   (search "/Pods/" path-str)))
             (walk (current-dir)
               (dolist (sub (uiop:subdirectories current-dir))
                 (let ((name (namestring sub)))
                   (cond
                     ((excluded-p name) nil)
                     ((str:ends-with-p ".xcworkspace/" name)
                      (push (list :workspace name) results))
                     ((str:ends-with-p ".xcodeproj/" name)
                      (push (list :project name) results))
                     (t (walk sub)))))))
      (walk dir))
    (nreverse results)))

(defun init/handler (cmd)
  "Handler for the `init' command. Interactively sets up project configuration."
  (let* ((path (first (clingon:command-arguments cmd)))
         (dir (or path (uiop:getcwd)))
         (found (find-project-files dir))
         (target nil))
    (unless found
      (format *error-output* "~A No .xcodeproj or .xcworkspace found in ~A~%"
              (colored-text "Error:" :red) dir)
      (uiop:quit 1))
    (if (= (length found) 1)
        (setf target (first found))
        (setf target (prompt-choice "Multiple projects found — select one" found
                       :display-fn (lambda (f)
                                     (format nil "[~A] ~A"
                                             (get-project-file-type-str (first f))
                                             (second f))))))
    (let ((target-type (first target))
          (target-path (second target)))
      (format t "Using ~A: ~A~%" (get-project-file-type-str target-type)
              (colored-text target-path :blue))
      ;; Discover schemes
      (let* ((info (list-project-info target-type target-path))
             (key (get-project-file-type-str target-type))
             (project-info (when info (gethash key info)))
             (schemes (when project-info (gethash "schemes" project-info)))
             (scheme nil)
             (test-scheme nil)
             (sim nil))
        (if schemes
            (progn
              (setf scheme (prompt-choice "Select default build scheme" schemes))
              (setf test-scheme (prompt-choice "Select default test scheme" schemes
                                               :allow-none t)))
            (format t "~A~%" (colored-text "No schemes found in project." :yellow)))
        ;; Discover simulators
        (let ((sims (collect-available-sims)))
          (if sims
              (let ((chosen (prompt-choice "Select default simulator" sims
                              :display-fn (lambda (s)
                                            (format nil "~A [~A] (~A)"
                                                    (first s) (second s) (third s))))))
                (when chosen
                  (setf sim (second chosen))))
              (format t "~A~%" (colored-text "No available simulators found." :yellow))))
        ;; Write config
        (let ((plist (list :project-type target-type
                           :project-path (namestring target-path)
                           :sim sim
                           :device nil
                           :scheme scheme
                           :test-scheme test-scheme
                           :use-swb nil)))
          (model:update-model-config path plist)
          (format t "~%~A ~A~%"
                  (colored-text "Config written to" :green)
                  (colored-text (namestring (model:resolve-config-file path)) :blue))
          (loop for (k v) on plist by #'cddr
                do (format t "  ~A: ~A~%" (colored-text k :cyan) v)))))))

(defun init/command ()
  "A command for interactive project setup."
  (clingon:make-command
   :name "init"
   :description "Interactively set up project configuration"
   :handler #'init/handler))