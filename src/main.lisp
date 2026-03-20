 (in-package :cupertino)

(defun top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (info/top-level/command)
   (make-xcodebuild-command "build" "Build the Xcode project")
   (make-xcodebuild-command "test" "Run tests for the Xcode project"
                            :scheme-accessors (list #'model-test-scheme #'model-scheme))
   (make-xcodebuild-command "clean" "Clean the Xcode project")))

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
