(in-package :cupertino)

(defun info/top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
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
