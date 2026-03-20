(in-package :cupertino)

(defun clean/handler (cmd)
  "Handler for the `clean' command"
  (let* ((path (first (clingon:command-arguments cmd)))
         (model (model:make-cupertino-model path))
         (scheme (or (clingon:getopt cmd :scheme)
                     (model-scheme model))))
    (run-xcodebuild cmd scheme "clean")))
