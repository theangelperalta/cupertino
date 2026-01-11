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
               #:clingon
               #:cl-ppcre
               #:yason
               #:str)
  :components (
               (:file "utils")
               (:file "devices")
               (:file "cli")
               (:file "main")))


(defpackage :cupertino
  (:use #:cl)
  (:export #:main))
