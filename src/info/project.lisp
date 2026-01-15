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
  (dolist (scheme (list-project-schemes target-type target-path))
  (format t "~A~%" scheme))))

(defun list-project-schemes (target-type target-path)
  "Executes xcodebuild -list  and returns a schemes in project or workspace"
  (let* ((command (format nil "xcodebuild -list -~a ~a" (get-project-file-type-str target-type) target-path))
         (output-string (uiop:run-program command :output :string :ignore-error-status t))
         (schemes (parse-schemes (read-lines-from-string output-string))))
    schemes))

(defun trim (str)
  (string-trim '(#\Space #\Tab #\Newline #\Return) str))

(defun starts-with-indentation (line)
  "Check if LINE starts with whitespace."
  (and (plusp (length line))
       (let ((c (char line 0)))
         (or (char= c #\Space) (char= c #\Tab)))))

(defun parse-schemes (lines)
   "Return a list of scheme names parsed from LINES (in order).
We look for a line that equals \"Schemes:\" (after trimming). Then
we collect subsequent indented, non-empty lines as scheme names,
stopping when we reach an empty line or a non-indented non-empty line."
"Return a list of parsed schemes from xcodebuild output LINES."
  (let ((found nil)
        (schemes nil))
    (dolist (line lines)
      (cond
        ((and (not found)
              (string= (trim line) "Schemes:"))
         (setf found t))
        (found
         (cond
           ((or (string= (trim line) "")
                (not (starts-with-indentation line)))
            (setf found nil))
           ((starts-with-indentation line)
            (push (trim line) schemes))))))
    (nreverse schemes)))

(defun read-lines-from-string (input-string)
  "Reads lines from a string and returns them as a list."
  (with-input-from-string (stream input-string)
    (loop for line = (read-line stream nil nil) ; Read one line at a time
          while line                          ; Stop when line is NIL (EOF)
          collect line)))                      ; Collect the line into a list
