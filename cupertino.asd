;;;; cupertino.asd
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute))

(asdf:defsystem #:cupertino
  :description "CLI tool for Apple 🍎 platform development workflows"
  :version "0.1.0"
  :author "Angel Perlata <acort3255@gmail.com>"
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
               (:file "install")
               (:file "cli")
               (:file "main")))

