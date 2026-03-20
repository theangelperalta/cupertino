;;;; model.lisp
(in-package :model)

;; Cupertino model class definition
;; a concrete implementation of the model-interface

(defclass cupertino-model (model-interface)
  ((project-type :initarg :project-type :accessor model-project-type)
   (project-path :initarg :project-path :accessor model-project-path)
   (sim :initarg :sim :accessor model-sim)
   (device :initarg :device :accessor model-device)
   (scheme :initarg :scheme :accessor model-scheme)
   (test-scheme :initarg :test-scheme :accessor model-test-scheme)))

(defvar *model-loaded* nil)
(defvar *model-lock* (bt:make-lock "model-lock"))

(defun read-plist-file (path)
  "Read a plist from a file."
  (with-open-file (stream path :direction :input)
    (read stream)))

(defun write-plist-file (path plist)
  "Write a plist to a file."
  (ensure-directories-exist path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write plist :stream stream :pretty t)))

(defvar *discover-fn* nil
  "Function to call to auto-discover project info. Set by the cupertino package.")

(defun discover-project-info (dir)
  "Auto-detect project info by calling *discover-fn* if set."
  (if *discover-fn*
      (funcall *discover-fn* dir)
      (list :project-type nil :sim nil :device nil :scheme nil :test-scheme nil)))

(defun load-model (&optional path)
  "Load the plist from .cupertino/cupertino.lisp into *model-loaded*."
  (bt:with-lock-held (*model-lock*)
    (unless *model-loaded*
      (let ((file (merge-pathnames ".cupertino/cupertino.lisp" (or path (uiop:getcwd)))))
        (handler-case
            (setf *model-loaded* (read-plist-file file))
          (error ()
            (let ((plist (discover-project-info (or path (uiop:getcwd)))))
              (write-plist-file file plist)
              (format t "Generated ~A~%" file)
              (setf *model-loaded* plist))))))))

(defun make-cupertino-model (&optional path)
  "Create a new instance of the Cupertino model from the loaded plist."
  (load-model path)
  (let ((base-dir (or path (uiop:getcwd)))
        (project-path (getf *model-loaded* :project-path)))
    (make-instance 'cupertino-model
                   :project-type (getf *model-loaded* :project-type)
                   :project-path (when project-path
                                   (merge-pathnames project-path base-dir))
                   :sim (getf *model-loaded* :sim)
                   :device (getf *model-loaded* :device)
                   :scheme (getf *model-loaded* :scheme)
                   :test-scheme (getf *model-loaded* :test-scheme))))