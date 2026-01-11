(in-package :cupertino)

;;;; Color printing in Common Lisp using ANSI escape codes

;;; ANSI color codes
(defparameter *ansi-colors*
  '((:black . "30")
    (:red . "31")
    (:green . "32")
    (:yellow . "33")
    (:blue . "34")
    (:magenta . "35")
    (:cyan . "36")
    (:white . "37")
    (:bright-black . "90")
    (:bright-red . "91")
    (:bright-green . "92")
    (:bright-yellow . "93")
    (:bright-blue . "94")
    (:bright-magenta . "95")
    (:bright-cyan . "96")
    (:bright-white . "97")))

(defparameter *ansi-reset* "[0m")

;;; Basic color formatting function
(defun colored-text (text color)
  "Print TEXT in the specified COLOR using ANSI escape codes."
  (let ((color-code (cdr (assoc color *ansi-colors*))))
    (if color-code
        (format nil "~C[~Am~A~C~A"
                #\Escape color-code text #\Escape *ansi-reset*)
        (format nil "~A" text))))

;;; Determine color based on text value
(defun text-value-to-color (text)
  "Determine color based on the content/value of TEXT."
  (cond
    ;; Numbers
    ((and (stringp text) (every #'digit-char-p text))
     (let ((num (parse-integer text)))
       (cond
         ((< num 0) :red)
         ((= num 0) :yellow)
         (t :green))))
    
    ;; Keywords or symbols starting with :
    ((and (stringp text) (char= (char text 0) #\:))
     :magenta)
    
    ;; Boolean-like values
    ((member text '("true" "t" "yes" "success") :test #'string-equal)
     :green)
    ((member text '("false" "nil" "no" "failure") :test #'string-equal)
     :red)
    
    ;; Warning/Error keywords
    ((search "error" text :test #'char-equal)
     :bright-red)
    ((search "warning" text :test #'char-equal)
     :yellow)
    ((search "info" text :test #'char-equal)
     :cyan)
    ((search "success" text :test #'char-equal)
     :bright-green)
    
    ;; Default no color
    (t nil)))

;;; Print text with auto-detected color
(defun smart-color (text)
  "Creates TEXT with color automatically determined by its value."
  (let ((color (text-value-to-color text)))
    (colored-text text color)))

;;; Alternative:  Using a hash table for custom mappings
(defun make-value-color-map ()
  "Create a hash table mapping specific values to colors."
  (let ((map (make-hash-table :test 'equal)))
    (setf (gethash "critical" map) :bright-red)
    (setf (gethash "high" map) :red)
    (setf (gethash "medium" map) :yellow)
    (setf (gethash "low" map) :green)
    (setf (gethash "debug" map) :blue)
    map))

(defparameter *value-color-map* (make-value-color-map))

(defun text-color-with-map (text &optional (map *value-color-map*))
  "Create TEXT using color from MAP, or auto-detect if not found."
  (let ((color (gethash text map)))
    (if color
        (colored-text text color)
        (smart-color text))))

;; Time

(defun parse-iso8601-timestamp (timestamp-string)
  "Parse an ISO 8601 timestamp string and return a universal-time."
  (let* ((date-time (cl-ppcre:split "[T.]" timestamp-string))
         (date-parts (cl-ppcre:split "-" (first date-time)))
         (time-parts (cl-ppcre:split ":" (second date-time)))
         (year (parse-integer (first date-parts)))
         (month (parse-integer (second date-parts)))
         (day (parse-integer (third date-parts)))
         (hour (parse-integer (first time-parts)))
         (minute (parse-integer (second time-parts)))
         (second (floor (parse-integer (third time-parts)))))
    (encode-universal-time second minute hour day month year 0)))

(defun format-relative-time (timestamp-string)
  "Format a timestamp as relative time (e.g., '2 hours ago', '3 days ago')."
  (let* ((then (parse-iso8601-timestamp timestamp-string))
         (now (get-universal-time))
         (diff (- now then))
         (seconds diff)
         (minutes (floor diff 60))
         (hours (floor diff 3600))
         (days (floor diff 86400))
         (weeks (floor diff 604800))
         (months (floor diff 2592000))
         (years (floor diff 31536000)))
    (cond
      ((< seconds 60) (format nil "~D second~:P ago" seconds))
      ((< minutes 60) (format nil "~D minute~:P ago" minutes))
      ((< hours 24) (format nil "~D hour~:P ago" hours))
      ((< days 7) (format nil "~D day~:P ago" days))
      ((< weeks 4) (format nil "~D week~:P ago" weeks))
      ((< months 12) (format nil "~D month~:P ago" months))
      (t (format nil "~D year~:P ago" years)))))


;; Table
;; NOTE: DON'T USE EMOJI's because of the unicode wide/spacing issue

(defvar +CELL-FORMATS+ '(:left "~vA"
                         :center "~v:@<~A~>"
                         :right  "~v@A"))


(defun format-table (stream data &key (column-label (loop for i from 1 to (length (car data))
                                                          collect (format nil "COL~D" i)))
                                      (column-align (loop for i from 1 to (length (car data))
                                                          collect :left)))
    (let* ((col-count (length column-label))
           (strtable  (cons column-label ; table header
                          (loop for row in data ; table body with all cells as strings
                              collect (loop for cell in row
                                           collect (if (stringp cell)
                                                        cell
                                                    ;else
                                                        (format nil "~A" cell))))))
           (col-widths (loop with widths = (make-array col-count :initial-element 0)
                           for row in strtable
                           do (loop for cell in row
                                    for i from 0
                                  do (setf (aref widths i)
                                         (max (aref widths i) (length cell))))
                           finally (return widths))))
        ;------------------------------------------------------------------------------------
        ; splice in the header separator
        (setq strtable
              (nconc (list (car strtable) ; table header
                           (loop for align in column-align ; generate separator
                                 for width across col-widths
                               collect (case align
                                           (:left   (format nil ":~v@{~A~:*~}"
                                                        (1- width)  "-"))
                                           (:right  (format nil "~v@{~A~:*~}:"
                                                        (1- width)  "-"))
                                           (:center (format nil ":~v@{~A~:*~}:"
                                                        (- width 2) "-")))))
                           (cdr strtable))) ; table body
        ;------------------------------------------------------------------------------------
        ; Generate the formatted table
        (let ((row-fmt (format nil "| ~{~A~^ | ~} |~~%" ; compile the row format
                           (loop for align in column-align
                               collect (getf +CELL-FORMATS+ align))))
              (widths  (loop for w across col-widths collect w)))
            ; write each line to the given stream
            (dolist (row strtable)
                (apply #'format stream row-fmt (mapcan #'list widths row))))))