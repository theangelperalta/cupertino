(in-package :cupertino)


;; Simulator Info Contents
;; - device types
;; - runtimes
;; - devices
;; - device pairs - TODO

(defun print-sim-info ()
   (let ((sim-devices-info (list-sim-info)))
    (when sim-devices-info
       (progn
       (print-sim-device-types sim-devices-info)
       (print-sim-runtimes sim-devices-info)
       (print-sim-devices sim-devices-info)))))

(defun list-sim-info ()
  "Executes 'xcrun simctl list --json' and returns results for all simulator devices and runtimes available."
  (let* ((command '("xcrun" "simctl" "list" "--json"))
         (output-string (uiop:run-program command :output :string :ignore-error-status t)))
    (handler-case
        (yason:parse output-string)
      (error (e)
        (format *error-output* "Error parsing JSON output: ~a~%" e)
        nil))))

(defun print-sim-device-types (device-info)
  "Prints the simulator device types from the parsed list."
  (format t (colored-text "== Devices Types ==~%" :white))
  (let ((device-types (gethash "devicetypes" device-info)))
      (dolist (device device-types)
        (format t "  • ~a (~a) ~%" 
                (gethash "name" device)
                (gethash "identifier" device)))))

(defun print-sim-runtimes (device-info)
  "Prints the simulator runtimes from the parsed list."
  (format t (colored-text "== Runtimes ==~%" :white))
  (let ((runtimes (gethash "runtimes" device-info)))
      (dolist (runtime runtimes)
        (format t "  • ~a (~a - ~a) - ~a~%" 
                (gethash "name" runtime)
                (gethash "version" runtime)
                (gethash "buildversion" runtime)
                (gethash "identifier" runtime)))))

(defun make-runtime-version-map (device-info)
  "Create a hash table mapping specific runtimes to platform and version name."
  (let ((map (make-hash-table :test 'equal)) (runtimes (gethash "runtimes" device-info)))
    (dolist (runtime runtimes)
        (setf (gethash (gethash "identifier" runtime) map) (gethash "name" runtime)))
    map))

(defun sim-runtime-name (device-info runtime-id)
  "Human-readable runtime name (e.g. \"iOS 26.5\") for RUNTIME-ID in a parsed
simctl DEVICE-INFO, or NIL when absent."
  (when device-info
    (let ((runtimes (gethash "runtimes" device-info)))
      (when runtimes
        (loop for r in runtimes
              when (equal (gethash "identifier" r) runtime-id)
                return (gethash "name" r))))))

(defun print-sim-devices (device-info)
  "Prints the names and UUIDs of available devices from the parsed list."
  (format t (colored-text "== Devices ==~%" :white))
  (let ((devices (gethash "devices" device-info))) ; Access the :devices key
    (maphash (lambda (platform devices)
      (let* ((runtime-version-map (make-runtime-version-map device-info)) (human-readable-name (gethash platform runtime-version-map)))
        (if human-readable-name
        (format t "-- ~a --~%" (colored-text human-readable-name :cyan))
        (format t "-- ~a: ~a --~%" (colored-text "Unavailable" :red) (colored-text platform :red)))) ; Print platform name
      (dolist (device devices)
      (let ((name (gethash "name" device)) (udid (gethash "udid" device)) (state (gethash "state" device)) (availablility-error (gethash "availabilityError" device)))
        (if availablility-error
        (format t "  • ~a [~a] (~a)(~a)~%" 
                name
                udid
                (colored-text state (text-color-for-state state))
                (colored-text (format nil "unavailable, ~a"availablility-error) :red))
        (format t "  • ~a [~a] (~a)~%" 
                name
                udid
                (colored-text state (text-color-for-state state))))))) devices)))

(defun text-color-for-state (text)
  "Determine color based on the state of device."
  (cond
    ;; Boolean-like values
    ((member text '("booted") :test #'string-equal)
     :green)
    ((member text '("shutdown") :test #'string-equal)
     :red)
    ;; Default no color
    (t nil)))

;; Physical Device Info 

(defun print-device-info ()
  (let ((rows (list-device-names)))
    (if rows
        (format-table t rows
                      :column-label '("NAME" "OS VERSION" "HOSTNAME" "ID" "STATE" "MODEL" "LAST CONNECTED")
                      :column-align '(:left :left :left :left :left :left :left))
        (format t "No physical devices found.~%"))))

(defun list-device-info ()
  "Run the devicectl command, save output to a temporary file, extract JSON using sed,
parse with Yason, and clean up the temp file. 

Returns the parsed Lisp structure on success. Signals an error if the command fails,
no JSON block is found, or the JSON fails to parse."
  
  ;; Create temporary file path
  (let ((tmp-file (uiop:tmpize-pathname 
                   (make-pathname :directory '(:absolute "tmp")
                                  :name (format nil "devicectl-~a" (get-universal-time))
                                  :type "json"))))
    
    (unwind-protect
         (progn
           ;; Run the devicectl command and save to temp file
           (multiple-value-bind (output error-output exit-code)
               (uiop:run-program 
                (format nil "xcrun devicectl list devices --json-output ~a" 
                        (uiop:native-namestring tmp-file))
                :output :string
                :error-output :string
                :ignore-error-status t)
             (declare (ignore output))
             
             (unless (zerop exit-code)
               (error "Command failed with exit code ~a: ~a" exit-code error-output))
             
             ;; Check if temp file exists
             (unless (probe-file tmp-file)
               (error "Temporary file was not created: ~a" tmp-file))
             
             ;; Extract JSON using sed command
             (let ((json-output (uiop:read-file-string tmp-file)))
               
               ;; Parse with yason
               (handler-case
                   (with-input-from-string (stream json-output)
                     (let ((yason:*parse-object-as* :hash-table)
                           (yason:*parse-json-arrays-as-vectors* t))
                       (gethash "devices" (gethash "result" (yason:parse stream)))))
                 (error (e)
                   (error "Failed to parse JSON:  ~a~%JSON snippet: ~a"
                          e (subseq json-output 0 (min (length json-output) 500))))))))
      
      ;; Cleanup:  ensure temp file is deleted
      (when (probe-file tmp-file)
        (ignore-errors (delete-file tmp-file))))))

;;; Convenience wrapper to pretty-print the parsed structure: 
(defun fetch-and-print-pretty ()
  "Run fetch-and-parse-devices and print pretty JSON (for human-readable output).
Returns the parsed Lisp structure."
  (let ((parsed (list-device-info)))
    (format t "~&")
    (yason:encode parsed *standard-output*)
    (format t "~%")
    parsed))

;;; Helper function to get specific device info
(defun get-connected-devices ()
  "Fetch devices and return only those with tunnelState = 'connected'."
  (let* ((devices (list-device-info))
         (connected '()))
    (loop for device across devices
          for conn-props = (gethash "connectionProperties" device)
          for tunnel-state = (gethash "tunnelState" conn-props)
          when (string= tunnel-state "connected")
            do (push device connected))
    (nreverse connected)))

(defun json-seq-first (seq)
  "First element of a JSON array parsed as a list or vector, or NIL if empty.
Yason may represent `[]` as an adjustable vector whose fill-pointer is 0 but
whose backing store is still readable via AREF, so LENGTH (not AREF) is the
emptiness check."
  (when (and seq (plusp (length seq)))
    (elt seq 0)))

(defun device-reality (device)
  "devicectl `reality` value (`physical` / `simulated`), or NIL when absent."
  (or (let ((hp (gethash "hardwareProperties" device)))
        (and hp (gethash "reality" hp)))
      (let* ((props (gethash "properties" device))
             (hw (and props (gethash "hardware" props))))
        (and hw (gethash "reality" hw)))))

(defun physical-device-p (device)
  "T unless DEVICE is known to be a simulator. Unknown/missing reality is
treated as physical so older devicectl payloads still list."
  (not (equal (device-reality device) "simulated")))

(defun device-table-row (device)
  "One `info device` table row (list of cell strings/values) for DEVICE."
  (let* ((dev-props (gethash "deviceProperties" device))
         (hardware-props (gethash "hardwareProperties" device))
         (conn-props (gethash "connectionProperties" device))
         (identifier (and device (gethash "identifier" device)))
         (os-version (and dev-props (gethash "osVersionNumber" dev-props)))
         (hostname (json-seq-first (and conn-props (gethash "potentialHostnames" conn-props))))
         (model (and hardware-props (gethash "marketingName" hardware-props)))
         (product-type (and hardware-props (gethash "productType" hardware-props)))
         (name (and dev-props (gethash "name" dev-props)))
         (state (and conn-props (gethash "tunnelState" conn-props)))
         (transport-type (and conn-props (gethash "transportType" conn-props)))
         (last-connection (and conn-props (gethash "lastConnectionDate" conn-props)))
         (state-cell (if transport-type
                         (format nil "~a (~a)" (or state "") transport-type)
                         (or state "")))
         (model-cell (cond ((and model product-type)
                            (format nil "~a (~a)" model product-type))
                           (t (or model product-type "")))))
    (list (or name "")
          (or os-version "")
          (or hostname "")
          (or identifier "")
          state-cell
          model-cell
          (format-relative-time last-connection))))

(defun list-device-names (&optional (devices nil devices-supplied-p))
  "Physical-device rows for `info device'. DEVICES, when supplied, bypasses
devicectl (used by tests); otherwise LIST-DEVICE-INFO is called."
  (let* ((devices (if devices-supplied-p devices (list-device-info)))
         (parsed-devices '()))
    (map nil
         (lambda (device)
           (when (physical-device-p device)
             (push (device-table-row device) parsed-devices)))
         devices)
    (nreverse parsed-devices)))
