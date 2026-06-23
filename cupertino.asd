;;;; cupertino.asd
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute))

(asdf:defsystem #:cupertino
  :description "CLI tool for Apple 🍎 platform development workflows"
  :version "0.1.0"
  :author "Angel Peralta <acort3255@gmail.com>"
  :license  "MIT"
  :pathname "src"
  :serial t
  :depends-on (#:uiop
               #:alexandria
               #:bordeaux-threads
               #:clingon
               #:cl-ppcre
               #:yason
               #:str
               #:supercons
               #:cl-messagepack
               #:flexi-streams)
  :components (
               (:file "common/package")
               (:file "common/model-interface")
               (:file "model/package")
               (:file "model/model")
               (:file "swb/package")
               (:file "swb/framing")
               (:file "swb/events")
               (:file "swb/proxy")
               (:file "package")
               (:file "utils")
               (:file "info/devices")
               (:file "info/project")
               (:file "xcbuild-parser")
               (:file "xcbuild-pretty")
               (:file "xcode-tools")
               (:file "xcbuild-parallel")
               (:file "install")
               (:file "cli")
               (:file "main")))

(asdf:defsystem #:cupertino/tests
  :description "Unit tests for cupertino"
  :depends-on (#:cupertino #:rove)
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "cell-tests")
               (:file "xcbuild-pretty-tests"))
  :perform (asdf:test-op (o c)
             (unless (uiop:symbol-call '#:rove '#:run-suite :cupertino/tests)
               (error "Tests failed"))))

