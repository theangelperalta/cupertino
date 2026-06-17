;; Cupertino package definition
(defpackage :cupertino
  (:use #:cl #:common #:model)
  (:local-nicknames (#:sc #:supercons))
  (:export #:main))