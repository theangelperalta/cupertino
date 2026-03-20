 (in-package :cupertino)

(defun build/options ()
  "Returns the options for the `build' command"
  (xcodebuild-options "build"))

(defun build/command ()
  "A command to build the Xcode project"
  (clingon:make-command
   :name "build"
   :description "Build the Xcode project"
   :options (build/options)
   :handler #'build/handler))

(defun test/options ()
  "Returns the options for the `test' command"
  (xcodebuild-options "test"))

(defun test/command ()
  "A command to test the Xcode project"
  (clingon:make-command
   :name "test"
   :description "Run tests for the Xcode project"
   :options (test/options)
   :handler #'test/handler))

(defun clean/options ()
  "Returns the options for the `clean' command"
  (xcodebuild-options "clean"))

(defun clean/command ()
  "A command to clean the Xcode project"
  (clingon:make-command
   :name "clean"
   :description "Clean the Xcode project"
   :options (clean/options)
   :handler #'clean/handler))

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
   (clingon:make-option :counter
                        :description "how noisy we want to be"
                        :short-name #\v
                        :long-name "verbose"
                        :key :verbose)))

(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (info/top-level/command)
   (build/command)
   (test/command)
   (clean/command)))

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
  "Entry point for CLI tool"
  (declare (ignorable argv))
  (let ((app (top-level/command)))
    (clingon:run app)))
