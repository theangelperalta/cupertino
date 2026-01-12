(in-package :cupertino)

;; Scheme

(defun list-sim-info ()
  "Executes 'xcrun simctl list --json' and returns the parsed Lisp object. Returns results for all of simualtor devices and runtimes available."
  (let* ((command '("xcrun" "simctl" "list" "--json"))
         (output-string (uiop:run-program command :output :string :ignore-error-status t)))
    (handler-case
        (yason:parse output-string)
      (error (e)
        (format *error-output* "Error parsing JSON output: ~a~%" e)
        nil))))

(defun list-project-schemes (workspace)
  "Executes xcodebuild -list  and returns a schemes in project or workspace"
  (let* ((command (format nil "xcodebuild -list -workspace ~a" workspace))
         (output-string (uiop:run-program command :output :string :ignore-error-status t))
         (schemes (parse-schemes (read-lines-from-string output-string))))
    (format t "~a~%" schemes)))

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
