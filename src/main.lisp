 (in-package :cupertino)

(defun greet/options ()
  "Returns the options for the `greet' command"
  (list
   (clingon:make-option
    :string
    :description "Person to greet"
    :short-name #\u
    :long-name "user"
    :initial-value "stranger"
    :env-vars '("USER")
    :key :user)))

(defun greet/handler (cmd)
  "Handler for the `greet' command"
  (let ((who (clingon:getopt cmd :user)))
    (format t "Hello, ~A!~%" who)))

(defun greet/command ()
  "A command to greet someone"
  (clingon:make-command
   :name "greet"
   :description "greets people"
   :options (greet/options)
   :handler #'greet/handler))

(defun dot/command ()
  "Returns the command for the `dot' command"
  (clingon:make-command
   :name "dot"
   :description "generate tree representation in Dot format"
   :usage ""
   :handler (lambda (cmd)
              (let ((parent (clingon:command-parent cmd)))
                (clingon:print-documentation :dot parent t)))))

(defun top-level/options ()
  "Returns the options for the top-level command"
  (list
   (clingon:make-option :string
                        :long-name "persistent-opt"
                        :description "example persistent option"
                        :persistent t
                        :key :persistent-opt)
   (clingon:make-option :counter
                        :description "how noisy we want to be"
                        :short-name #\v
                        :long-name "verbose"
                        :key :verbose)))

(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (info/top-level/command)
   (greet/command)))

(defun top-level/handler (cmd)
  "The handler for the top-level command. Will print the usage of the app"
  (clingon:print-usage-and-exit cmd t))

(defun top-level/command ()
  "Returns the top-level command"
  (clingon:make-command :name (asdf:component-name (asdf:find-system "cupertino"))
                        :version (asdf:component-version (asdf:find-system "cupertino"))
                        :description (asdf:system-description (asdf:find-system "cupertino"))
                        :long-description (asdf:system-description (asdf:find-system "cupertino"))
                        :authors (list (asdf:system-author (asdf:find-system "cupertino")))
                        :license (asdf:system-license (asdf:find-system "cupertino"))
                        :handler #'top-level/handler
                        :options (top-level/options)
                        :sub-commands (top-level/sub-commands)))

(defun main (&rest argv)
  (declare (ignorable argv))
  "Entry point for CLI tool"
  (let ((app (top-level/command)))
    (clingon:run app)))
