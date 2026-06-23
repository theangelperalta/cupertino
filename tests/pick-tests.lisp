;;;; pick-tests.lisp
;;;; Unit tests for the interactive UDID picker (src/pick.lisp) and the
;;;; pick command handlers' stdout/stderr contract (src/cli.lisp).

(in-package #:cupertino/tests)

;;; -------------------------------------------------------------------------
;;; Fixtures
;;; -------------------------------------------------------------------------

(defun make-pick-sim-info (entries)
  "Build a fake parsed-simctl hash for ENTRIES, a list of
(NAME UDID STATE RUNTIME-ID &OPTIONAL (AVAILABLE-P T) ERROR) tuples. Mirrors
the shape yason produces for `xcrun simctl list --json' but with the extra
`isAvailable' / `availabilityError' / `state' fields the picker actually
filters on."
  (let ((info (make-hash-table :test 'equal))
        (devices (make-hash-table :test 'equal))
        (runtimes nil)
        (seen-rt (make-hash-table :test 'equal)))
    (dolist (e entries)
      (destructuring-bind (name udid state rt-id
                           &optional (available-p t) err) e
        (let ((dev (make-hash-table :test 'equal)))
          (setf (gethash "name" dev) name
                (gethash "udid" dev) udid
                (gethash "state" dev) state
                (gethash "isAvailable" dev) available-p)
          (when err (setf (gethash "availabilityError" dev) err))
          (setf (gethash rt-id devices)
                (append (gethash rt-id devices) (list dev))))
        (unless (gethash rt-id seen-rt)
          (setf (gethash rt-id seen-rt) t)
          (let* ((r (make-hash-table :test 'equal))
                 (rt-name (cond ((search "iOS-17-0" rt-id) "iOS 17.0")
                                ((search "iOS-16-4" rt-id) "iOS 16.4")
                                (t rt-id))))
            (setf (gethash "identifier" r) rt-id
                  (gethash "name" r) rt-name)
            (push r runtimes)))))
    (setf (gethash "devices" info) devices
          (gethash "runtimes" info) runtimes)
    info))

(defun make-pick-device (name udid transport
                        &key (tunnel-state "connected"))
  "Build a fake devicectl-shaped device hash with the keys the picker reads."
  (let ((d (make-hash-table :test 'equal))
        (cp (make-hash-table :test 'equal))
        (dp (make-hash-table :test 'equal)))
    (setf (gethash "transportType" cp) transport
          (gethash "tunnelState" cp) tunnel-state
          (gethash "name" dp) name
          (gethash "identifier" d) udid
          (gethash "deviceProperties" d) dp
          (gethash "connectionProperties" d) cp)
    d))

(defmacro with-pick-streams ((&key (input "") prompt stdout) &body body)
  "Bind *standard-input* / *error-output* / *standard-output* to fresh string
streams (capturing into the supplied symbols when provided) for BODY."
  (let ((p (or prompt (gensym "PROMPT")))
        (so (or stdout (gensym "SO"))))
    `(let* ((,p (make-string-output-stream))
            (,so (make-string-output-stream))
            (*standard-input* (make-string-input-stream ,input))
            (*error-output* ,p)
            (*standard-output* ,so))
       ,@body)))

;;; -------------------------------------------------------------------------
;;; prompt-choice — output-stream separation and default-index semantics
;;; -------------------------------------------------------------------------

(deftest prompt-choice/respects-prompt-stream
  (let* ((prompt (make-string-output-stream))
         (stdout (make-string-output-stream))
         (*standard-output* stdout)
         (*standard-input* (make-string-input-stream (format nil "1~%"))))
    (cupertino::prompt-choice "Pick" (list "a" "b") :prompt-stream prompt)
    (let ((p (get-output-stream-string prompt))
          (s (get-output-stream-string stdout)))
      (ok (search "Pick" p))
      (ok (search "a" p))
      (ok (zerop (length s))))))

(deftest prompt-choice/default-index-selects-on-empty-input
  (with-pick-streams (:input (format nil "~%"))
    (let ((r (cupertino::prompt-choice "Pick" (list "a" "b" "c")
                                       :prompt-stream *error-output*
                                       :default-index 2)))
      (ok (string= "b" r)))))

(deftest prompt-choice/empty-input-without-default-and-allow-none-returns-nil
  (with-pick-streams (:input (format nil "~%"))
    (let ((r (cupertino::prompt-choice "Pick" (list "a" "b")
                                       :prompt-stream *error-output*
                                       :allow-none t)))
      (ok (null r)))))

(deftest prompt-choice/prompt-suffix-advertises-current
  (with-pick-streams (:input (format nil "~%") :prompt p)
    (cupertino::prompt-choice "Pick" (list "a" "b")
                              :prompt-stream *error-output*
                              :allow-none t
                              :default-index 1)
    (let ((out (get-output-stream-string p)))
      (ok (search "Enter for current" out))
      (ok (search "0 to skip" out)))))

;;; -------------------------------------------------------------------------
;;; pick-available-sims
;;; -------------------------------------------------------------------------

(deftest pick-available-sims/filters-unavailable
  ;; One available + one unavailable in the same runtime: with a single
  ;; survivor the auto-pick path fires, so no menu prompt is needed.
  (with-pick-streams (:input "" :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "iPhone 17" "udid-ok" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0" t)
                        (list "iPhone OLD" "udid-bad" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"
                              nil "missing runtime"))))
           (r (cupertino::pick-available-sims :info info :stream *error-output*)))
      (ok (string= "udid-ok" r))
      (ok (null (search "iPhone OLD" (get-output-stream-string p)))))))

(deftest pick-available-sims/orders-booted-first
  (with-pick-streams (:input (format nil "1~%"))
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Booted"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "C" "udid-C" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-available-sims :info info :stream *error-output*)))
      (ok (string= "udid-B" r)))))

(deftest pick-available-sims/groups-by-runtime
  (with-pick-streams (:input (format nil "1~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "iPhone 17" "udid-17a" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "iPhone 17 Pro" "udid-17b" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "iPhone SE" "udid-16a" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-16-4")
                        (list "iPhone 14" "udid-16b" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-16-4"))))
           (_r (cupertino::pick-available-sims :info info :stream *error-output*))
           (out (get-output-stream-string p))
           (pos-17 (search "iOS 17.0" out))
           (pos-16 (search "iOS 16.4" out)))
      (declare (ignore _r))
      (ok (and pos-17 pos-16))
      (ok (< pos-17 pos-16)))))

(deftest pick-available-sims/eof-returns-nil
  (with-pick-streams (:input "")
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-available-sims :info info :stream *error-output*)))
      (ok (null r)))))

(deftest pick-available-sims/no-sims-returns-nil
  (with-pick-streams (:input "" :prompt p)
    (let* ((info (make-pick-sim-info nil))
           (r (cupertino::pick-available-sims :info info :stream *error-output*)))
      (ok (null r))
      (ok (zerop (length (get-output-stream-string p)))))))

(deftest pick-available-sims/single-option-auto-picks-without-prompt
  ;; Empty stdin: would block if the picker tried to read from it. The
  ;; auto-pick branch must short-circuit before any read-line.
  (with-pick-streams (:input "" :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "Only" "udid-only" "Booted"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-available-sims :info info :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (string= "udid-only" r))
      (ok (search "Using" out))
      (ok (search "only available choice" out)))))

(deftest pick-available-sims/current-udid-marked-and-default-on-enter
  (with-pick-streams (:input (format nil "~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "C" "udid-C" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-available-sims :info info
                                              :current-udid "udid-B"
                                              :stream *error-output*)))
      (ok (string= "udid-B" r))
      (ok (search "(current)" (get-output-stream-string p))))))

;;; -------------------------------------------------------------------------
;;; pick-connected-devices
;;; -------------------------------------------------------------------------

(deftest pick-connected-devices/filters-disconnected
  ;; A connected USB device + a disconnected one. With just the survivor the
  ;; auto-pick branch fires — no prompt, no menu entries for the disconnected.
  (with-pick-streams (:input "" :prompt p)
    (let* ((devs (list (make-pick-device "iPhone OK" "udid-ok" "USB"
                                         :tunnel-state "connected")
                       (make-pick-device "iPhone OLD" "udid-old" "USB"
                                         :tunnel-state "disconnected")))
           (r (cupertino::pick-connected-devices :devices devs
                                                 :stream *error-output*)))
      (ok (string= "udid-ok" r))
      (ok (null (search "iPhone OLD" (get-output-stream-string p)))))))

(deftest pick-connected-devices/usb-before-wifi
  (with-pick-streams (:input (format nil "1~%") :prompt p)
    (let* ((devs (list (make-pick-device "W" "udid-W" "Wi-Fi")
                       (make-pick-device "U" "udid-U" "USB")))
           (r (cupertino::pick-connected-devices :devices devs
                                                 :stream *error-output*))
           (out (get-output-stream-string p))
           (pos-usb (search "USB" out))
           (pos-wifi (search "Wi-Fi" out)))
      (ok (string= "udid-U" r))
      (ok (and pos-usb pos-wifi))
      (ok (< pos-usb pos-wifi)))))

;;; -------------------------------------------------------------------------
;;; pick-install-destination — Xcode-style unified sim+device picker
;;; -------------------------------------------------------------------------

(deftest pick-install-destination/sims-only-returns-kind-sim
  ;; Sim section present, no devices -> only sims appear, kind = :sim.
  (with-pick-streams (:input (format nil "1~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-install-destination
               :sim-info info :devices nil :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (equal r (list :kind :sim :udid "udid-A")))
      (ok (search "Simulators" out))
      (ok (null (search "Devices" out))))))

(deftest pick-install-destination/devices-only-returns-kind-device
  (with-pick-streams (:input (format nil "1~%") :prompt p)
    (let* ((devs (list (make-pick-device "iPhone U1" "udid-U1" "USB")
                       (make-pick-device "iPhone U2" "udid-U2" "USB")))
           (r (cupertino::pick-install-destination
               :sim-info (make-pick-sim-info nil)
               :devices devs :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (equal r (list :kind :device :udid "udid-U1")))
      (ok (search "Devices" out))
      (ok (null (search "Simulators" out))))))

(deftest pick-install-destination/both-sections-pick-device-from-second-block
  ;; 2 sims + 1 device; picking index 3 should land on the device with kind :device.
  (with-pick-streams (:input (format nil "3~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (devs (list (make-pick-device "iPhone U" "udid-U" "USB")))
           (r (cupertino::pick-install-destination
               :sim-info info :devices devs :stream *error-output*))
           (out (get-output-stream-string p))
           (pos-sims (search "Simulators" out))
           (pos-devs (search "Devices" out)))
      (ok (equal r (list :kind :device :udid "udid-U")))
      (ok (and pos-sims pos-devs (< pos-sims pos-devs))))))

(deftest pick-install-destination/current-marks-sim-and-default-on-enter
  (with-pick-streams (:input (format nil "~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (devs (list (make-pick-device "iPhone U" "udid-U" "USB")))
           (r (cupertino::pick-install-destination
               :sim-info info :devices devs
               :current-udid "udid-B" :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (equal r (list :kind :sim :udid "udid-B")))
      (ok (search "(current)" out)))))

(deftest pick-install-destination/current-marks-device-and-default-on-enter
  (with-pick-streams (:input (format nil "~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (devs (list (make-pick-device "iPhone U1" "udid-U1" "USB")
                       (make-pick-device "iPhone U2" "udid-U2" "USB")))
           (r (cupertino::pick-install-destination
               :sim-info info :devices devs
               :current-udid "udid-U2" :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (equal r (list :kind :device :udid "udid-U2")))
      (ok (search "(current)" out)))))

(deftest pick-install-destination/single-option-auto-picks-without-prompt
  ;; Exactly one target across both sections -> auto-pick, no prompt suffix.
  (with-pick-streams (:input "" :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "Only" "udid-only" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-install-destination
               :sim-info info :devices nil :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (equal r (list :kind :sim :udid "udid-only")))
      (ok (search "only available choice" out))
      (ok (null (search "Select install destination" out))))))

(deftest pick-install-destination/no-targets-returns-nil
  (with-pick-streams (:input "" :prompt p)
    (let ((r (cupertino::pick-install-destination
              :sim-info (make-pick-sim-info nil)
              :devices nil :stream *error-output*)))
      (ok (null r))
      (ok (zerop (length (get-output-stream-string p)))))))

(deftest pick-install-destination/eof-returns-nil
  (with-pick-streams (:input "")
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-install-destination
               :sim-info info :devices nil :stream *error-output*)))
      (ok (null r)))))

;;; -------------------------------------------------------------------------
;;; flatten-sim-groups — display shape
;;; -------------------------------------------------------------------------

(deftest flatten-sim-groups/header-includes-count
  (let* ((info (make-pick-sim-info
                (list (list "A" "udid-A" "Shutdown"
                            "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                      (list "B" "udid-B" "Shutdown"
                            "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                      (list "C" "udid-C" "Shutdown"
                            "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
         (groups (cupertino::group-and-sort-sims
                  info (gethash "devices" info)))
         (items (cupertino::flatten-sim-groups groups))
         (header (find-if (lambda (it) (eq (first it) :header)) items)))
    (ok header)
    (ok (string= "iOS 17.0 (3)" (getf header :header)))))

(deftest flatten-sim-groups/display-includes-udid-tail
  (let* ((udid "ABCDEF12-3456-7890-ABCD-EF1234567890")
         (info (make-pick-sim-info
                (list (list "iPhone X" udid "Shutdown"
                            "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
         (groups (cupertino::group-and-sort-sims
                  info (gethash "devices" info)))
         (items (cupertino::flatten-sim-groups groups))
         (item (find-if (lambda (it) (eq (first it) :item)) items)))
    (ok item)
    (ok (search "34567890" (getf item :display)))
    (ok (search (string #\…) (getf item :display)))))

(deftest flatten-sim-groups/booted-coloured-green
  (let* ((info (make-pick-sim-info
                (list (list "A" "udid-A" "Booted"
                            "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                      (list "B" "udid-B" "Shutdown"
                            "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
         (groups (cupertino::group-and-sort-sims
                  info (gethash "devices" info)))
         (items (cupertino::flatten-sim-groups groups))
         (rows (remove-if-not (lambda (it) (eq (first it) :item)) items))
         (booted (find "udid-A" rows :test #'string= :key (lambda (it) (getf it :udid))))
         (shutdown (find "udid-B" rows :test #'string= :key (lambda (it) (getf it :udid))))
         (green (format nil "~C[32m" #\Escape))
         (red (format nil "~C[31m" #\Escape)))
    (ok (search green (getf booted :display)))
    (ok (search red (getf shutdown :display)))))

;;; -------------------------------------------------------------------------
;;; pick/sim/run handler contract — UDID on stdout, prose on stderr
;;; -------------------------------------------------------------------------

(defmacro with-fdefinition-stub ((name fn) &body body)
  "Temporarily replace (symbol-function NAME) with FN for BODY."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (symbol-function ',name)))
       (unwind-protect
            (progn (setf (symbol-function ',name) ,fn) ,@body)
         (setf (symbol-function ',name) ,saved)))))

;;; -------------------------------------------------------------------------
;;; pick-project-schemes
;;; -------------------------------------------------------------------------

(deftest pick-project-schemes/single-option-auto-picks-without-prompt
  (with-pick-streams (:input "" :prompt p)
    (let* ((r (cupertino::pick-project-schemes
               :schemes (list "OnlyScheme")
               :stream *error-output*))
           (out (get-output-stream-string p)))
      (ok (string= "OnlyScheme" r))
      (ok (search "Using" out))
      (ok (search "only available choice" out)))))

(deftest pick-project-schemes/returns-chosen-by-index
  (with-pick-streams (:input (format nil "2~%"))
    (let ((r (cupertino::pick-project-schemes
              :schemes (list "Alpha" "Beta" "Gamma")
              :stream *error-output*)))
      (ok (string= "Beta" r)))))

(deftest pick-project-schemes/current-marked-and-default-on-enter
  (with-pick-streams (:input (format nil "~%") :prompt p)
    (let ((r (cupertino::pick-project-schemes
              :schemes (list "Alpha" "Beta" "Gamma")
              :current-scheme "Beta"
              :stream *error-output*)))
      (ok (string= "Beta" r))
      (ok (search "(current)" (get-output-stream-string p))))))

(deftest pick-project-schemes/no-schemes-returns-nil
  (with-pick-streams (:input "" :prompt p)
    (let ((r (cupertino::pick-project-schemes
              :schemes nil :stream *error-output*)))
      (ok (null r))
      (ok (zerop (length (get-output-stream-string p)))))))

(deftest pick-project-schemes/eof-returns-nil
  (with-pick-streams (:input "")
    (let ((r (cupertino::pick-project-schemes
              :schemes (list "Alpha" "Beta")
              :stream *error-output*)))
      (ok (null r)))))

(deftest pick-project-schemes/allow-none-nil-rejects-bare-enter
  ;; With :allow-none NIL, bare Enter (no default-index) should be rejected and
  ;; the prompt re-issued; the follow-up "2" must select Beta.
  (with-pick-streams (:input (format nil "~%2~%") :prompt p)
    (let ((r (cupertino::pick-project-schemes
              :schemes (list "Alpha" "Beta")
              :allow-none nil
              :stream *error-output*)))
      (ok (string= "Beta" r))
      (ok (search "Invalid choice" (get-output-stream-string p))))))

(deftest pick-project-schemes/label-appears-in-prompt
  (with-pick-streams (:input (format nil "1~%") :prompt p)
    (cupertino::pick-project-schemes
     :schemes (list "Alpha" "Beta")
     :label "Select test scheme"
     :stream *error-output*)
    (ok (search "Select test scheme" (get-output-stream-string p)))))

;;; -------------------------------------------------------------------------
;;; pick/scheme/run handler contract
;;; -------------------------------------------------------------------------

(deftest pick-scheme-handler/picked-line-on-stderr-but-not-stdout
  (with-pick-streams (:input "" :prompt p :stdout so)
    (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
      (with-fdefinition-stub (model:load-model
                              (lambda (&optional _) (declare (ignore _)) nil))
        (with-fdefinition-stub (cupertino::pick-project-schemes
                                (lambda (&rest _) (declare (ignore _)) "MyScheme"))
          (let* ((cmd (clingon:parse-command-line (cupertino::pick/scheme/command) nil))
                 (code (cupertino::pick/scheme/run cmd))
                 (out (get-output-stream-string so))
                 (err (get-output-stream-string p)))
            (ok (zerop code))
            (ok (string= (format nil "MyScheme~%") out))
            (ok (search "Picked" err))))))))

(deftest pick-test-scheme-handler/uses-test-scheme-key-and-label
  ;; Confirms pick/test-scheme/run wires the existing :test-scheme (NOT :scheme)
  ;; into :current-scheme and passes the "Select test scheme" label through.
  (with-pick-streams (:input "" :prompt p :stdout so)
    (let ((captured-label nil)
          (captured-current nil))
      (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
        (with-fdefinition-stub (model:load-model
                                (lambda (&optional _) (declare (ignore _))
                                  (list :test-scheme "OldTest")))
          (with-fdefinition-stub
              (cupertino::pick-project-schemes
               (lambda (&key label current-scheme &allow-other-keys)
                 (setf captured-label label
                       captured-current current-scheme)
                 "NewTest"))
            (let* ((cmd (clingon:parse-command-line
                         (cupertino::pick/test-scheme/command) nil))
                   (code (cupertino::pick/test-scheme/run cmd))
                   (out (get-output-stream-string so)))
              (ok (zerop code))
              (ok (string= (format nil "NewTest~%") out))
              (ok (string= "OldTest" (or captured-current "")))
              (ok (string= "Select test scheme" (or captured-label ""))))))))))

(deftest pick-handler/picked-line-on-stderr-but-not-stdout
  ;; Stub model:load-model so the handler doesn't touch the real
  ;; .cupertino/cupertino.lisp; stub pick-available-sims to a fixed UDID;
  ;; stub stdin-interactive-p so the TTY guard passes under a string stream.
  (with-pick-streams (:input "" :prompt p :stdout so)
    (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
      (with-fdefinition-stub (model:load-model
                              (lambda (&optional _) (declare (ignore _)) nil))
        (with-fdefinition-stub (cupertino::pick-available-sims
                                (lambda (&rest _) (declare (ignore _)) "udid-stub"))
          (let* ((cmd (clingon:parse-command-line (cupertino::pick/sim/command) nil))
                 (code (cupertino::pick/sim/run cmd))
                 (out (get-output-stream-string so))
                 (err (get-output-stream-string p)))
            (ok (zerop code))
            (ok (string= (format nil "udid-stub~%") out))
            (ok (search "Picked" err))))))))

;;; -------------------------------------------------------------------------
;;; pick-cell — `Scheme@destination' composition
;;; -------------------------------------------------------------------------

(deftest pick-cell/sim-format
  ;; Single scheme auto-picks, single sim auto-picks -> "Scheme@sim=UDID".
  (with-pick-streams (:input "" :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "Only" "udid-only" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-cell
               :schemes (list "MyApp")
               :sim-info info :devices nil
               :stream *error-output*)))
      (ok (string= "MyApp@sim=udid-only" r)))))

(deftest pick-cell/device-format
  ;; Single scheme auto-picks, single device auto-picks -> "Scheme@device=UDID".
  (with-pick-streams (:input "" :prompt p)
    (let* ((devs (list (make-pick-device "iPhone" "udid-dev" "USB")))
           (r (cupertino::pick-cell
               :schemes (list "MyApp")
               :sim-info (make-pick-sim-info nil)
               :devices devs
               :stream *error-output*)))
      (ok (string= "MyApp@device=udid-dev" r)))))

(deftest pick-cell/scheme-cancel-returns-nil
  ;; EOF on the scheme prompt -> NIL, destination picker never runs.
  (with-pick-streams (:input "" :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-cell
               :schemes (list "Alpha" "Beta")
               :sim-info info :devices nil
               :stream *error-output*)))
      (ok (null r)))))

(deftest pick-cell/dest-cancel-returns-nil
  ;; "1" picks the scheme, then EOF cancels the destination prompt -> NIL.
  (with-pick-streams (:input (format nil "1~%") :prompt p)
    (let* ((info (make-pick-sim-info
                  (list (list "A" "udid-A" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0")
                        (list "B" "udid-B" "Shutdown"
                              "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))
           (r (cupertino::pick-cell
               :schemes (list "Alpha" "Beta")
               :sim-info info :devices nil
               :stream *error-output*)))
      (ok (null r)))))

;;; -------------------------------------------------------------------------
;;; pick/cell/run + pick/test-cell/run handler contracts
;;; -------------------------------------------------------------------------

(deftest pick-cell-handler/cell-on-stdout-prose-on-stderr
  (with-pick-streams (:input "" :prompt p :stdout so)
    (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
      (with-fdefinition-stub (model:load-model
                              (lambda (&optional _) (declare (ignore _)) nil))
        (with-fdefinition-stub
            (cupertino::pick-cell
             (lambda (&rest _) (declare (ignore _)) "MyApp@sim=udid-X"))
          (let* ((cmd (clingon:parse-command-line (cupertino::pick/cell/command) nil))
                 (code (cupertino::pick/cell/run cmd))
                 (out (get-output-stream-string so))
                 (err (get-output-stream-string p)))
            (ok (zerop code))
            (ok (string= (format nil "MyApp@sim=udid-X~%") out))
            (ok (search "Picked" err))
            (ok (null (search "Picked" out)))))))))

(deftest pick-cell-handler/save-appends-to-existing-cells
  ;; --save with an existing :cells list appends; saved-line on stderr
  ;; shows the merged list.
  (let ((captured-updates nil))
    (with-pick-streams (:input "" :prompt p :stdout so)
      (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
        (with-fdefinition-stub
            (model:load-model
             (lambda (&optional _) (declare (ignore _))
               (list :cells (list "Existing@sim=udid-old"))))
          (with-fdefinition-stub
              (cupertino::pick-cell
               (lambda (&rest _) (declare (ignore _)) "New@sim=udid-new"))
            (with-fdefinition-stub
                (model:update-model-config
                 (lambda (path updates)
                   (declare (ignore path))
                   (setf captured-updates updates)
                   updates))
              (let* ((cmd (clingon:parse-command-line
                           (cupertino::pick/cell/command) (list "--save")))
                     (code (cupertino::pick/cell/run cmd)))
                (ok (zerop code))
                (ok (equal captured-updates
                           (list :cells (list "Existing@sim=udid-old"
                                              "New@sim=udid-new"))))))))))))

(deftest pick-cell-handler/save-replace-overwrites-existing
  (let ((captured-updates nil))
    (with-pick-streams (:input "" :prompt p :stdout so)
      (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
        (with-fdefinition-stub
            (model:load-model
             (lambda (&optional _) (declare (ignore _))
               (list :cells (list "Existing@sim=udid-old"))))
          (with-fdefinition-stub
              (cupertino::pick-cell
               (lambda (&rest _) (declare (ignore _)) "New@device=udid-new"))
            (with-fdefinition-stub
                (model:update-model-config
                 (lambda (path updates)
                   (declare (ignore path))
                   (setf captured-updates updates)
                   updates))
              (let* ((cmd (clingon:parse-command-line
                           (cupertino::pick/cell/command)
                           (list "--save" "--replace")))
                     (code (cupertino::pick/cell/run cmd)))
                (ok (zerop code))
                (ok (equal captured-updates
                           (list :cells (list "New@device=udid-new"))))))))))))

(deftest pick-test-cell-handler/uses-test-cells-key
  ;; pick/test-cell/run must save into :test-cells (NOT :cells).
  (let ((captured-updates nil))
    (with-pick-streams (:input "" :prompt p :stdout so)
      (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
        (with-fdefinition-stub
            (model:load-model (lambda (&optional _) (declare (ignore _)) nil))
          (with-fdefinition-stub
              (cupertino::pick-cell
               (lambda (&rest _) (declare (ignore _)) "MyTests@sim=udid-t"))
            (with-fdefinition-stub
                (model:update-model-config
                 (lambda (path updates)
                   (declare (ignore path))
                   (setf captured-updates updates)
                   updates))
              (let* ((cmd (clingon:parse-command-line
                           (cupertino::pick/test-cell/command) (list "--save")))
                     (code (cupertino::pick/test-cell/run cmd)))
                (ok (zerop code))
                (ok (equal captured-updates
                           (list :test-cells (list "MyTests@sim=udid-t"))))))))))))

(deftest pick-cell-handler/no-cell-returns-nonzero
  ;; pick-cell returns NIL -> handler exits 1 and emits no stdout.
  (with-pick-streams (:input "" :prompt p :stdout so)
    (with-fdefinition-stub (cupertino::stdin-interactive-p (lambda () t))
      (with-fdefinition-stub (model:load-model
                              (lambda (&optional _) (declare (ignore _)) nil))
        (with-fdefinition-stub
            (cupertino::pick-cell (lambda (&rest _) (declare (ignore _)) nil))
          (let* ((cmd (clingon:parse-command-line (cupertino::pick/cell/command) nil))
                 (code (cupertino::pick/cell/run cmd))
                 (out (get-output-stream-string so)))
            (ok (= 1 code))
            (ok (zerop (length out)))))))))

