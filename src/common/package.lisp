;;;; package.lisp
;;;; Common utilities and data structures package definition

(defpackage :common
  (:use #:cl)
  (:export #:model-interface
           #:model-project-type
           #:model-project-path
           #:model-sim
           #:model-device
           #:model-scheme
           #:model-test-scheme))