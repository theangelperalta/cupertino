(in-package :cupertino)

;;;; Interactive UDID picker shared by `cupertino pick {sim,device}' and
;;;; `cupertino init'. Menus and prompts route through an explicit STREAM
;;;; keyword (default *error-output*) so handlers keep *standard-output*
;;;; clean for shell substitution.

;; Defined in cli.lisp (loaded after this file); declared here so the picker
;; entry points compile without a forward-reference warning.
(declaim (ftype function prompt-choice find-project-files))

(defun udid-short-tail (udid)
  "Last 8 characters of UDID with a leading ellipsis, for compact menu rows.
Returns the bare UDID when it is 8 chars or shorter."
  (if (and (stringp udid) (> (length udid) 8))
      (concatenate 'string "…" (subseq udid (- (length udid) 8)))
      (or udid "")))

(defun runtime-version-key (runtime-id)
  "Numeric sort key for a sim runtime id like
\"com.apple.CoreSimulator.SimRuntime.iOS-17-0\" -> 17.0d0. Falls back to 0d0
when the version segments aren't parseable so unknown runtimes sort last."
  (let ((p (and runtime-id (search "SimRuntime." runtime-id))))
    (unless p (return-from runtime-version-key 0d0))
    (let* ((rest (subseq runtime-id (+ p (length "SimRuntime."))))
           (dash (position #\- rest)))
      (unless dash (return-from runtime-version-key 0d0))
      (let* ((parts (cl-ppcre:split "-" (subseq rest (1+ dash))))
             (major (ignore-errors (parse-integer (or (first parts) "0"))))
             (minor (ignore-errors (parse-integer (or (second parts) "0")))))
        (+ (coerce (or major 0) 'double-float)
           (/ (coerce (or minor 0) 'double-float) 100d0))))))

(defun group-and-sort-sims (info devices)
  "Return a list of (RUNTIME-NAME RUNTIME-ID SORTED-DEVICE-HASHES) tuples.
Runtime groups sorted by version descending; within each group, available sims
only, Booted first, then name ascending."
  (let (groups)
    (maphash
     (lambda (runtime-id device-list)
       (let ((avail (remove-if-not
                     (lambda (d)
                       (and (gethash "isAvailable" d)
                            (not (gethash "availabilityError" d))))
                     device-list)))
         (when avail
           (let ((sorted (sort (copy-list avail)
                               (lambda (a b)
                                 (let ((sa (gethash "state" a))
                                       (sb (gethash "state" b)))
                                   (cond
                                     ((and (equal sa "Booted")
                                           (not (equal sb "Booted"))) t)
                                     ((and (equal sb "Booted")
                                           (not (equal sa "Booted"))) nil)
                                     (t (string< (or (gethash "name" a) "")
                                                 (or (gethash "name" b) "")))))))))
             (push (list (or (sim-runtime-name info runtime-id) runtime-id)
                         runtime-id sorted)
                   groups)))))
     devices)
    (sort groups #'> :key (lambda (g) (runtime-version-key (second g))))))

(defun flatten-sim-groups (groups &key current-udid)
  "Flatten GROUPS into the (:header STRING) / (:item HASH ...) shape consumed
by PROMPT-CHOICE. Each :item carries :name / :udid for downstream callers and
a preformatted :display with ANSI colour codes and a UDID tail."
  (let (out)
    (dolist (g groups)
      (let* ((rt-name (first g))
             (devs (third g)))
        (push (list :header (format nil "~A (~D)" rt-name (length devs))) out)
        (dolist (d devs)
          (let* ((name (gethash "name" d))
                 (udid (gethash "udid" d))
                 (state (or (gethash "state" d) "Unknown"))
                 (state-color (cond ((string-equal state "Booted") :green)
                                    ((string-equal state "Shutdown") :red)
                                    (t nil)))
                 (current (and current-udid (equal udid current-udid)))
                 (display (format nil "~A [~A] (~A)~@[~A~]"
                                  name
                                  (colored-text state state-color)
                                  (colored-text (udid-short-tail udid)
                                                :bright-black)
                                  (and current
                                       (colored-text " (current)" :cyan)))))
            (push (list :item d :name name :udid udid :display display) out)))))
    (nreverse out)))

(defun device-transport-rank (transport)
  "Sort rank for TRANSPORT: USB before Wi-Fi before anything else."
  (cond ((and transport (string-equal transport "USB")) 0)
        ((and transport (string-equal transport "Wi-Fi")) 1)
        (t 2)))

(defun group-and-sort-devices (devices)
  "Group DEVICES (already filtered to tunnelState=connected) by transport
type. Returns a list of (TRANSPORT-NAME SORTED-DEVICE-HASHES); USB first,
Wi-Fi second, others after, alphabetical within each group."
  (let ((buckets (make-hash-table :test 'equal)))
    (dolist (d devices)
      (let* ((cp (gethash "connectionProperties" d))
             (t-type (and cp (gethash "transportType" cp))))
        (push d (gethash (or t-type "Other") buckets))))
    (let (groups)
      (maphash (lambda (t-type devs)
                 (push (list t-type
                             (sort (copy-list devs) #'string<
                                   :key (lambda (d)
                                          (or (gethash "name"
                                                       (gethash "deviceProperties" d))
                                              ""))))
                       groups))
               buckets)
      (sort groups #'< :key (lambda (g) (device-transport-rank (first g)))))))

(defun flatten-device-groups (groups &key current-udid)
  "Flatten device GROUPS into the (:header ...) / (:item ...) shape consumed
by PROMPT-CHOICE."
  (let (out)
    (dolist (g groups)
      (let* ((t-type (first g))
             (devs (second g)))
        (push (list :header (format nil "~A (~D)" t-type (length devs))) out)
        (dolist (d devs)
          (let* ((name (gethash "name" (gethash "deviceProperties" d)))
                 (udid (gethash "identifier" d))
                 (tag-color (cond ((string-equal t-type "USB") :green)
                                  ((string-equal t-type "Wi-Fi") :cyan)
                                  (t nil)))
                 (current (and current-udid (equal udid current-udid)))
                 (display (format nil "~A [~A] (~A)~@[~A~]"
                                  name
                                  (colored-text t-type tag-color)
                                  (colored-text (udid-short-tail udid)
                                                :bright-black)
                                  (and current
                                       (colored-text " (current)" :cyan)))))
            (push (list :item d :name name :udid udid :display display) out)))))
    (nreverse out)))

(defun current-position (items current-udid)
  "1-based index of the :item in ITEMS whose :udid equals CURRENT-UDID,
counting only selectable (non-:header) entries. NIL when there is no match."
  (when current-udid
    (let ((idx 0))
      (dolist (it items)
        (unless (and (consp it) (eq (first it) :header))
          (incf idx)
          (when (and (consp it) (eq (first it) :item)
                     (equal (getf it :udid) current-udid))
            (return-from current-position idx))))
      nil)))

(defun pick-available-sims (&key (info nil info-supplied-p)
                                 current-udid
                                 (stream *error-output*))
  "Interactively pick one available simulator. Returns its UDID, or NIL on
allow-none / EOF / no-sims. Menu goes to STREAM. INFO defaults to a fresh
LIST-SIM-INFO call and is overridable for tests. CURRENT-UDID, when matching
a row, marks it `(current)' and selects it on bare Enter. When exactly one
sim survives filtering, returns it without prompting."
  (let* ((info (if info-supplied-p info (ignore-errors (list-sim-info))))
         (devices (and info (gethash "devices" info)))
         (groups (and devices (group-and-sort-sims info devices)))
         (items (and groups (flatten-sim-groups groups :current-udid current-udid)))
         (selectable (remove-if (lambda (it)
                                  (and (consp it) (eq (first it) :header)))
                                items)))
    (cond
      ((null selectable) nil)
      ((= 1 (length selectable))
       (let ((only (first selectable)))
         (format stream "~A: ~A (~A) — only available choice~%"
                 (colored-text "Using" :cyan)
                 (getf only :name) (getf only :udid))
         (getf only :udid)))
      (t
       (let* ((default-index (current-position items current-udid))
              (chosen (prompt-choice "Select simulator" items
                                     :allow-none t
                                     :prompt-stream stream
                                     :default-index default-index)))
         (and chosen (getf chosen :udid)))))))

(defun discover-project-schemes (dir stream)
  "Walk DIR for an xcodeproj/xcworkspace and return the schemes list reported
by `xcodebuild -list -json'. Prompts on STREAM to disambiguate when more than
one project file is present. Returns NIL when nothing is found or xcodebuild
returns no schemes."
  (let* ((found (find-project-files (or dir (uiop:getcwd))))
         (target (cond ((null found) nil)
                       ((= 1 (length found)) (first found))
                       (t (prompt-choice "Multiple projects found — select one"
                                         found
                                         :prompt-stream stream
                                         :display-fn
                                         (lambda (f)
                                           (format nil "[~A] ~A"
                                                   (get-project-file-type-str (first f))
                                                   (second f))))))))
    (when target
      (let* ((target-type (first target))
             (target-path (second target))
             (info (list-project-info target-type target-path))
             (key (get-project-file-type-str target-type))
             (project-info (and info (gethash key info))))
        (and project-info (gethash "schemes" project-info))))))

(defun pick-project-schemes (&key (schemes nil schemes-supplied-p)
                                  dir
                                  current-scheme
                                  (label "Select scheme")
                                  (allow-none t)
                                  (stream *error-output*))
  "Interactively pick one scheme. Returns the chosen scheme name, or NIL on
allow-none / EOF / no-schemes. Menu goes to STREAM. SCHEMES, when supplied,
bypasses project discovery (used by tests); otherwise schemes are read from
the xcodeproj/xcworkspace under DIR (defaults to *default-pathname-defaults*
via `uiop:getcwd'). CURRENT-SCHEME, when matching a row, marks it `(current)'
and selects it on bare Enter. When exactly one scheme is available, returns
it without prompting. ALLOW-NONE (default T) lets the user bail with 0 / bare
Enter; pass NIL to force a selection."
  (let ((schemes (if schemes-supplied-p
                     schemes
                     (ignore-errors (discover-project-schemes dir stream)))))
    (cond
      ((or (null schemes) (zerop (length schemes))) nil)
      ((= 1 (length schemes))
       (let ((only (elt schemes 0)))
         (format stream "~A: ~A — only available choice~%"
                 (colored-text "Using" :cyan) only)
         only))
      (t
       (let* ((items (loop for s across (coerce schemes 'vector)
                           collect (list :item s :scheme s
                                         :display
                                         (if (and current-scheme
                                                  (string= s current-scheme))
                                             (format nil "~A~A" s
                                                     (colored-text " (current)"
                                                                   :cyan))
                                             s))))
              (pos (and current-scheme
                        (position current-scheme schemes :test #'equal)))
              (default-index (and pos (1+ pos)))
              (chosen (prompt-choice label items
                                     :allow-none allow-none
                                     :prompt-stream stream
                                     :default-index default-index)))
         (and chosen (getf chosen :scheme)))))))

(defun tag-items-kind (items kind)
  "Return ITEMS with each :item plist augmented with `:kind KIND'. Header rows
pass through unchanged. Used to flag rows in the combined install picker so
the caller can branch on simulator vs. device without re-matching UDIDs."
  (mapcar (lambda (it)
            (if (and (consp it) (eq (first it) :item))
                (append it (list :kind kind))
                it))
          items))

(defun pick-install-destination (&key (sim-info nil sim-info-supplied-p)
                                      (devices nil devices-supplied-p)
                                      current-udid
                                      (stream *error-output*))
  "Interactively pick one install destination from the unified set of
available simulators and connected physical devices, mirroring Xcode's
run-destination dropdown. Returns a plist `(:kind :sim|:device :udid \"...\")'
or NIL on allow-none / EOF / no-targets. Menu goes to STREAM. SIM-INFO and
DEVICES, when supplied, bypass simctl / devicectl (used by tests).
CURRENT-UDID, when matching a row in either section, marks it `(current)' and
selects it on bare Enter. When exactly one target is available across both
sections, returns it without prompting."
  (let* ((info       (if sim-info-supplied-p
                         sim-info
                         (ignore-errors (list-sim-info))))
         (sim-devs   (and info (gethash "devices" info)))
         (sim-grps   (and sim-devs (group-and-sort-sims info sim-devs)))
         (sim-items  (tag-items-kind
                      (and sim-grps
                           (flatten-sim-groups sim-grps :current-udid current-udid))
                      :sim))
         (raw        (cond (devices-supplied-p devices)
                           (t (ignore-errors (list-device-info)))))
         (connected  (when raw
                       (let (acc)
                         (map nil
                              (lambda (d)
                                (let* ((cp (gethash "connectionProperties" d))
                                       (ts (and cp (gethash "tunnelState" cp))))
                                  (when (and ts (string= ts "connected"))
                                    (push d acc))))
                              raw)
                         (nreverse acc))))
         (dev-grps   (and connected (group-and-sort-devices connected)))
         (dev-items  (tag-items-kind
                      (and dev-grps
                           (flatten-device-groups dev-grps
                                                  :current-udid current-udid))
                      :device))
         (items      (append (when sim-items
                               (cons (list :header "Simulators") sim-items))
                             (when dev-items
                               (cons (list :header "Devices") dev-items))))
         (selectable (remove-if (lambda (it)
                                  (and (consp it) (eq (first it) :header)))
                                items)))
    (cond
      ((null selectable) nil)
      ((= 1 (length selectable))
       (let ((only (first selectable)))
         (format stream "~A: ~A (~A) — only available choice~%"
                 (colored-text "Using" :cyan)
                 (getf only :name) (getf only :udid))
         (list :kind (getf only :kind) :udid (getf only :udid))))
      (t
       (let* ((default-index (current-position items current-udid))
              (chosen (prompt-choice "Select install destination" items
                                     :allow-none t
                                     :prompt-stream stream
                                     :default-index default-index)))
         (and chosen (list :kind (getf chosen :kind)
                           :udid (getf chosen :udid))))))))

(defun pick-cell (&key (schemes nil schemes-supplied-p)
                       (sim-info nil sim-info-supplied-p)
                       (devices nil devices-supplied-p)
                       dir
                       current-scheme
                       current-udid
                       (label "Select scheme for cell")
                       (stream *error-output*))
  "Interactively build one `scheme@destination' cell string. Returns the cell
or NIL on cancellation. Walks SCHEME first (via `pick-project-schemes' with
`:allow-none nil') then DESTINATION (unified sims+devices, Xcode-style, via
`pick-install-destination'). Cell destinations use the `sim=UDID' /
`device=UDID' shorthand that `parse-cell-option' routes back to the right
platform. SCHEMES / SIM-INFO / DEVICES, when supplied, bypass discovery
(used by tests)."
  (let ((scheme (apply #'pick-project-schemes
                       (append
                        (when schemes-supplied-p (list :schemes schemes))
                        (list :dir dir
                              :current-scheme current-scheme
                              :label label
                              :allow-none nil
                              :stream stream)))))
    (when scheme
      (let ((dest (apply #'pick-install-destination
                         (append
                          (when sim-info-supplied-p (list :sim-info sim-info))
                          (when devices-supplied-p (list :devices devices))
                          (list :current-udid current-udid
                                :stream stream)))))
        (when dest
          (ecase (getf dest :kind)
            (:sim    (format nil "~A@sim=~A"    scheme (getf dest :udid)))
            (:device (format nil "~A@device=~A" scheme (getf dest :udid)))))))))

(defun pick-connected-devices (&key (devices nil devices-supplied-p)
                                    current-udid
                                    (stream *error-output*))
  "Interactively pick one connected physical device. Same conventions as
PICK-AVAILABLE-SIMS. DEVICES may be a list or vector of raw device hashes
(as returned by LIST-DEVICE-INFO); only those with tunnelState=connected are
considered. Default is to call LIST-DEVICE-INFO directly."
  (let* ((raw (cond (devices-supplied-p devices)
                    (t (ignore-errors (list-device-info)))))
         (connected (when raw
                      (let (acc)
                        (map nil
                             (lambda (d)
                               (let* ((cp (gethash "connectionProperties" d))
                                      (ts (and cp (gethash "tunnelState" cp))))
                                 (when (and ts (string= ts "connected"))
                                   (push d acc))))
                             raw)
                        (nreverse acc))))
         (groups (and connected (group-and-sort-devices connected)))
         (items (and groups (flatten-device-groups groups
                                                   :current-udid current-udid)))
         (selectable (remove-if (lambda (it)
                                  (and (consp it) (eq (first it) :header)))
                                items)))
    (cond
      ((null selectable) nil)
      ((= 1 (length selectable))
       (let ((only (first selectable)))
         (format stream "~A: ~A (~A) — only available choice~%"
                 (colored-text "Using" :cyan)
                 (getf only :name) (getf only :udid))
         (getf only :udid)))
      (t
       (let* ((default-index (current-position items current-udid))
              (chosen (prompt-choice "Select device" items
                                     :allow-none t
                                     :prompt-stream stream
                                     :default-index default-index)))
         (and chosen (getf chosen :udid)))))))
