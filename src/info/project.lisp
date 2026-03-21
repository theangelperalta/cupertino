(in-package :cupertino)

;; Scheme

(defun get-project-file-type (path)
  (cond ((str:ends-with-p ".xcodeproj" path) ':project)
        ((str:ends-with-p ".xcworkspace" path) ':workspace)
        (t nil)))

(defun get-project-file-type-str (type)
  (cond ((equal type ':project) "project")
        ((equal type ':workspace) "workspace")
        (t nil)))

(defun print-project-info (target-type target-path)
  (progn
  (format t "Schemes in ~a: ~a~%~%" (get-project-file-type-str target-type) (colored-text target-path :blue))
  (let* ((list-project-info (list-project-info target-type target-path))
         (project-info (gethash (get-project-file-type-str target-type) list-project-info)))
    (print-project-schemes project-info))))

(defun list-project-info (target-type target-path)
  "Executes xcodebuild -list  and returns a schemes in project or workspace"
  (let* ((command (format nil "xcodebuild -list -~a '~a' -json" (get-project-file-type-str target-type) target-path))
         (output-string (uiop:run-program command :output :string :ignore-error-status t)))
    (handler-case
        (yason:parse output-string)
      (error (e)
        (format *error-output* "Error parsing JSON output: ~a~%" e)
        nil))))

(defun print-project-schemes (project-info)
  "Prints the simulator devices types and runtimes of from the parsed list."
  (let ((schemes (gethash "schemes" project-info))) ; Access the :devices key
      (dolist (scheme schemes)
        (format t "  • ~a~%" (colored-text scheme :green)))))

(defun discover-project-info (dir)
  "Auto-detect project type, scheme, and simulator from the given directory."
  (let* ((files (uiop:directory-files dir))
         (dirs (uiop:subdirectories dir))
         (all (append (mapcar #'namestring files) (mapcar #'namestring dirs)))
         (project-file (find-if (lambda (f) (str:ends-with-p ".xcodeproj/" f)) all))
         (workspace-file (find-if (lambda (f) (str:ends-with-p ".xcworkspace/" f)) all))
         (target-path (or workspace-file project-file))
         (target-type (cond (workspace-file :workspace)
                            (project-file :project)
                            (t nil)))
         (scheme nil)
         (sim nil))
    (when (and target-type target-path)
      (let ((info (list-project-info target-type target-path)))
        (when info
          (let* ((key (get-project-file-type-str target-type))
                 (project-info (gethash key info))
                 (schemes (when project-info (gethash "schemes" project-info))))
            (when (and schemes (> (length schemes) 0))
              (setf scheme (first schemes)))))))
    (let ((sim-info (list-sim-info)))
      (when sim-info
        (let ((devices (gethash "devices" sim-info)))
          (when devices
            (block find-sim
              (maphash (lambda (platform device-list)
                         (declare (ignore platform))
                         (dolist (d device-list)
                           (when (string-equal (gethash "state" d) "Booted")
                             (setf sim (gethash "udid" d))
                             (return-from find-sim))))
                       devices)
              (unless sim
                (maphash (lambda (platform device-list)
                           (declare (ignore platform))
                           (dolist (d device-list)
                             (when (and (gethash "isAvailable" d)
                                        (not (gethash "availabilityError" d)))
                               (setf sim (gethash "udid" d))
                               (return-from find-sim))))
                         devices)))))))
    (list :project-type target-type
          :project-path target-path
          :sim sim
          :device nil
          :scheme scheme
          :test-scheme nil)))

(setf model:*discover-fn* #'discover-project-info)
