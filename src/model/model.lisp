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
   (test-scheme :initarg :test-scheme :accessor model-test-scheme)
   (max-jobs :initarg :max-jobs :accessor model-max-jobs)
   (slow-threshold :initarg :slow-threshold :accessor model-slow-threshold)
   (use-swb :initarg :use-swb :accessor model-use-swb)
   (cache-hits :initarg :cache-hits :accessor model-cache-hits)))

(defvar *model-cache* (make-hash-table :test 'equal)
  "Per-path cache of loaded plists keyed by resolved config file path.")
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

(defun resolve-config-file (path)
  "Return the resolved config file pathname for the given project PATH."
  (merge-pathnames ".cupertino/cupertino.lisp" (or path (uiop:getcwd))))

(defun load-model (&optional path)
  "Load the plist from .cupertino/cupertino.lisp, caching per config file path."
  (let ((file (resolve-config-file path)))
    (bt:with-lock-held (*model-lock*)
      (or (gethash (namestring file) *model-cache*)
          (setf (gethash (namestring file) *model-cache*)
                (handler-case
                    (read-plist-file file)
                  (error ()
                    (let ((plist (discover-project-info (or path (uiop:getcwd)))))
                      (write-plist-file file plist)
                      (format t "Generated ~A~%" file)
                      plist))))))))

(defun make-cupertino-model (&optional path)
  "Create a new instance of the Cupertino model from the loaded plist."
  (let* ((plist (load-model path))
         (base-dir (or path (uiop:getcwd)))
         (project-path (getf plist :project-path)))
    (make-instance 'cupertino-model
                   :project-type (getf plist :project-type)
                   :project-path (when project-path
                                   (merge-pathnames project-path base-dir))
                   :sim (getf plist :sim)
                   :device (getf plist :device)
                   :scheme (getf plist :scheme)
                   :test-scheme (getf plist :test-scheme)
                   :max-jobs (getf plist :max-jobs)
                   :slow-threshold (getf plist :slow-threshold)
                   :use-swb (getf plist :use-swb)
                   :cache-hits (getf plist :cache-hits))))

(defun update-model-config (path updates)
  "Merge UPDATES (a plist of key-value pairs) into the config file at PATH.
Invalidates the cache entry so the next load-model picks up changes."
  (let* ((file (resolve-config-file path))
         (existing (handler-case (read-plist-file file)
                     (error () (discover-project-info (or path (uiop:getcwd))))))
         (merged (copy-list existing)))
    (loop for (key value) on updates by #'cddr
          do (setf (getf merged key) value))
    (write-plist-file file merged)
    (bt:with-lock-held (*model-lock*)
      (remhash (namestring file) *model-cache*))
    merged))