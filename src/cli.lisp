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
  (list
   (clingon:make-option
    :string
    :description "Person to greet"
    :short-name #\u
    :long-name "user"
    :initial-value "stranger"
    :env-vars '("USER")
    :key :user)))


(defun info/project/options ()
  (list
   (clingon:make-option
    :string
    :description "project file (e.g., HelloWorld.xcodeproj)"
    :short-name #\p
    :long-name "project"
    :key :project)))

(defun info/workspace/options ()
  (list
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
        "Error: You cannot specify both an Xcode project and a workspace."
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
  (let ((who (clingon:getopt cmd :user)))
    (print-sim-info)))

(defun info/sim/command ()
  "A command to display simulator device information"
  (clingon:make-command
   :name "sim"
   :description "Display simualtor device information"
   :handler #'info/sim/handler))

(defun info/device/handler (cmd)
  "Handler for the `device/info' command"
  (let ((who (clingon:getopt cmd :user)))
    (print-device-info)))

(defun info/device/command ()
  "A command to display physical device information"
  (clingon:make-command
   :name "device"
   :description "Display physical device information"
   :handler #'info/device/handler))
