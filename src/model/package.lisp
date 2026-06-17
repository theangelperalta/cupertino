;;;; package.lisp
;;;; Model package definition

(defpackage :model
  (:use #:cl #:common #:bordeaux-threads)
  (:export #:*discover-fn*
           #:make-cupertino-model
           #:update-model-config
           #:load-model
           #:resolve-config-file
           #:read-plist-file
           #:write-plist-file
           #:model-project-type
           #:model-project-path
           #:model-sim
           #:model-device
           #:model-scheme
           #:model-test-scheme
           #:model-max-jobs
           #:model-slow-threshold))