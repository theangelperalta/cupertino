;;;; package.lisp
;;;; Model package definition

(defpackage :model
  (:use #:cl #:common #:bordeaux-threads)
  (:export #:*discover-fn*
           #:make-cupertino-model
           #:model-project-type
           #:model-project-path
           #:model-sim
           #:model-device
           #:model-scheme
           #:model-test-scheme))