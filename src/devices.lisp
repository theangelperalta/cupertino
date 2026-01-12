(in-package :cupertino)


;; Simualtor Info Contents
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
  "Executes 'xcrun simctl list --json' and returns the parsed Lisp object. Returns results for all of simualtor devices and runtimes available."
  (let* ((command '("xcrun" "simctl" "list" "--json"))
         (output-string (uiop:run-program command :output :string :ignore-error-status t)))
    (handler-case
        (yason:parse output-string)
      (error (e)
        (format *error-output* "Error parsing JSON output: ~a~%" e)
        nil))))

(defun print-sim-device-types (device-info)
  "Prints the simulator devices types and runtimes of from the parsed list."
  (format t (colored-text "== Devices Types ==~%" :white))
  (let ((device-types (gethash "devicetypes" device-info))) ; Access the :devices key
      (dolist (device device-types)
        (format t "  • ~a (~a) ~%" 
                (gethash "name" device)
                (gethash "identifier" device)))))

(defun print-sim-runtimes (device-info)
  "Prints the simulator runtimes of from the parsed list."
  (format t (colored-text "== Runtimes ==~%" :white))
  (let ((runtimes (gethash "runtimes" device-info))) ; Access the :devices key
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
    (format-table t (list-device-names) 
                    :column-label '("NAME" "OS VERSION" "HOSTNAME" "ID" "STATE" "MODEL" "LAST CONNECTED")
                    :column-align '(:left :left :left :left :left :left :left)))

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

;;; Helper to get device names and states
(defun list-device-names ()
  "Fetch devices and return a list of (name . state) pairs."
  (let* ((devices (list-device-info))
         (parsed-devices '()))
    (loop for device across devices
          for identifier = (gethash "identifier" device)
          for dev-props = (gethash "deviceProperties" device)
          for hardware-props = (gethash "hardwareProperties" device)
          for os-version = (gethash "osVersionNumber" dev-props)
          for conn-props = (gethash "connectionProperties" device)
          for hostname = (aref (gethash "potentialHostnames" conn-props) 0)
          for model = (gethash "marketingName" hardware-props)
          for product-type = (gethash "productType" hardware-props)
          for name = (gethash "name" dev-props)
          for state = (gethash "tunnelState" conn-props)
          for transport-type = (gethash "transportType" conn-props)
          for last-connection-date-time = (gethash "lastConnectionDate" conn-props)

          do (push (list name os-version hostname identifier (if transport-type (format nil "~a (~a)" state transport-type) state) (format nil "~a (~a)" model product-type) (format-relative-time last-connection-date-time)) parsed-devices))
    (nreverse parsed-devices)))