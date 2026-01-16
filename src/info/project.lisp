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
  (format t "Schemes in ~a: ~a~%~%" (get-project-file-type-str target-type) target-path)
  (let* ((list-project-info (list-project-info target-type target-path))
         (project-info (gethash (get-project-file-type-str target-type) list-project-info)))
    (print-project-schemes project-info))))

(defun list-project-info (target-type target-path)
  "Executes xcodebuild -list  and returns a schemes in project or workspace"
  (let* ((command (format nil "xcodebuild -list -~a ~a -json" (get-project-file-type-str target-type) target-path))
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
        (format t "  • ~a~%" scheme))))
