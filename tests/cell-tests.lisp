;;;; cell-tests.lisp
;;;; Unit tests for cell parsing (parse-cell-option / parse-cell-destination)
;;;; and matrix-cell resolution (resolve-cells).

(in-package #:cupertino/tests)

;;; -------------------------------------------------------------------------
;;; Helpers
;;; -------------------------------------------------------------------------

(defun build-cmd (argv)
  "Parse ARGV against the matrix (build) command, returning the clingon command."
  (clingon:parse-command-line
   (cupertino::make-xcodebuild-command "build" "Build") argv))

(defun clean-cmd (argv)
  "Parse ARGV against the clean command (which carries --prune)."
  (clingon:parse-command-line
   (cupertino::make-xcodebuild-command "clean" "Clean") argv))

(defun test-model (plist)
  "Build a cupertino model from PLIST written to a throwaway temp config dir."
  (let ((dir (merge-pathnames (format nil "cup-test-~36R/" (random (expt 36 8)))
                              (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn (model:update-model-config dir plist)
                (model:make-cupertino-model dir))
      (ignore-errors (uiop:delete-directory-tree dir :validate t)))))

(defmacro with-temp-dir ((var) &body body)
  "Bind VAR to a fresh randomly-named pathname under (uiop:temporary-directory)
for BODY, then `delete-directory-tree' it on exit (tolerating absence). The
directory itself is NOT created up front -- tests opt into creation via
`ensure-directories-exist' as needed, so the same macro serves cases where
only some subpaths under VAR should actually exist on disk."
  `(let ((,var (merge-pathnames (format nil "cup-test-tmp-~36R/" (random (expt 36 8)))
                                (uiop:temporary-directory))))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree
                       (uiop:ensure-directory-pathname ,var) :validate t)))))

(defun run-resolve (cmd model &key
                                (derived-data nil)
                                (scheme-accessors (list #'model:model-schemes
                                                        #'model:model-scheme))
                                (cell-accessors (list #'model:model-cells)))
  "Invoke resolve-cells with fixed project-flag/configuration for tests."
  (cupertino::resolve-cells cmd "build" "-workspace 'X'" "Debug" derived-data
                            model scheme-accessors cell-accessors))

(defun call-with-stubbed-platform (fn thunk)
  "Run THUNK with cupertino::sim-runtime-platform temporarily replaced by FN."
  (let ((orig (fdefinition 'cupertino::sim-runtime-platform)))
    (unwind-protect
         (progn (setf (fdefinition 'cupertino::sim-runtime-platform) fn)
                (funcall thunk))
      (setf (fdefinition 'cupertino::sim-runtime-platform) orig))))

(defun make-fake-sim-info (entries)
  "Build a fake parsed-simctl hash for ENTRIES, a list of
(UDID DEVICE-NAME RUNTIME-ID RUNTIME-NAME) tuples. Mirrors the shape yason
produces for `xcrun simctl list --json' (object -> hash, array -> list)."
  (let ((info (make-hash-table :test 'equal))
        (devices (make-hash-table :test 'equal))
        (runtimes nil)
        (seen-rt (make-hash-table :test 'equal)))
    (dolist (e entries)
      (destructuring-bind (udid dev-name rt-id rt-name) e
        (let ((dev (make-hash-table :test 'equal)))
          (setf (gethash "udid" dev) udid
                (gethash "name" dev) dev-name)
          (push dev (gethash rt-id devices)))
        (unless (gethash rt-id seen-rt)
          (setf (gethash rt-id seen-rt) t)
          (let ((r (make-hash-table :test 'equal)))
            (setf (gethash "identifier" r) rt-id
                  (gethash "name" r) rt-name)
            (push r runtimes)))))
    (setf (gethash "devices" info) devices
          (gethash "runtimes" info) runtimes)
    info))

(defmacro with-stubbed-sim-info ((entries) &body body)
  "Bind cupertino::*sim-info-cache* to a fake simctl info built from ENTRIES."
  `(let ((cupertino::*sim-info-cache* (make-fake-sim-info ,entries)))
     ,@body))

;;; -------------------------------------------------------------------------
;;; parse-cell-option / parse-cell-destination
;;; -------------------------------------------------------------------------

(deftest parse-cell-option/raw-platform
  (multiple-value-bind (sch dest)
      (cupertino::parse-cell-option "MyScheme@platform=iOS Simulator,id=ABC")
    (ok (string= "MyScheme" sch))
    (ok (string= "platform=iOS Simulator,id=ABC" dest))))

(deftest parse-cell-option/device-shorthand
  (multiple-value-bind (sch dest) (cupertino::parse-cell-option "T@device=UDID9")
    (ok (string= "T" sch))
    (ok (string= "platform=iOS,id=UDID9" dest))))

(deftest parse-cell-option/no-at-returns-nil
  (ok (null (cupertino::parse-cell-option "NoSeparator"))))

(deftest parse-cell-option/splits-on-first-at
  (multiple-value-bind (sch dest) (cupertino::parse-cell-option "S@platform=iOS,id=A")
    (ok (string= "S" sch))
    (ok (string= "platform=iOS,id=A" dest))))

(deftest parse-cell-destination/raw-passthrough
  (ok (string= "platform=tvOS Simulator,id=Z"
               (cupertino::parse-cell-destination "platform=tvOS Simulator,id=Z"))))

(deftest parse-cell-destination/device
  (ok (string= "platform=iOS,id=U" (cupertino::parse-cell-destination "device=U"))))

;;; -------------------------------------------------------------------------
;;; runtime-id-platform / sim-destination (platform auto-detection)
;;; -------------------------------------------------------------------------

(deftest runtime-id-platform/ios
  (ok (string= "iOS" (cupertino::runtime-id-platform
                      "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))

(deftest runtime-id-platform/tvos
  (ok (string= "tvOS" (cupertino::runtime-id-platform
                       "com.apple.CoreSimulator.SimRuntime.tvOS-17-4"))))

(deftest runtime-id-platform/no-marker
  (ok (null (cupertino::runtime-id-platform "garbage-string"))))

(deftest sim-destination/ios
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) "iOS")
    (lambda () (ok (string= "platform=iOS Simulator,id=U1"
                            (cupertino::sim-destination "U1"))))))

(deftest sim-destination/tvos
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) "tvOS")
    (lambda () (ok (string= "platform=tvOS Simulator,id=U2"
                            (cupertino::sim-destination "U2"))))))

(deftest sim-destination/xros-maps-visionos
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) "xrOS")
    (lambda () (ok (string= "platform=visionOS Simulator,id=U3"
                            (cupertino::sim-destination "U3"))))))

(deftest sim-destination/unknown-falls-back-ios
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) nil)
    (lambda () (ok (string= "platform=iOS Simulator,id=U4"
                            (cupertino::sim-destination "U4"))))))

;;; -------------------------------------------------------------------------
;;; resolve-cells
;;; -------------------------------------------------------------------------

(deftest resolve-cells/cli-cells
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (ok (= 2 (length cells)))
    (ok (string= "iOS" (getf (first cells) :scheme)))
    (ok (string= "platform=iOS Simulator,id=AAA" (getf (first cells) :dest)))
    (ok (string= "tvOS" (getf (second cells) :scheme)))
    (ok (search "-scheme 'iOS'" (getf (first cells) :cmd)))
    (ok (search "-destination 'platform=tvOS Simulator,id=BBB'"
                (getf (second cells) :cmd)))))

(deftest resolve-cells/config-cells-fallback
  (let* ((cmd (build-cmd nil))
         (model (test-model (list :project-path "App.xcworkspace"
                                  :cells (list "iOS@platform=iOS Simulator,id=AAA"
                                               "tvOS@platform=tvOS Simulator,id=BBB"))))
         (cells (run-resolve cmd model)))
    (ok (= 2 (length cells)))
    (ok (string= "iOS" (getf (first cells) :scheme)))
    (ok (string= "tvOS" (getf (second cells) :scheme)))))

(deftest resolve-cells/test-cells-accessor
  (let* ((cmd (build-cmd nil))
         (model (test-model (list :project-path "App.xcworkspace"
                                  :test-cells (list "UITests@platform=iOS Simulator,id=AAA"))))
         (cells (run-resolve cmd model
                             :cell-accessors (list #'model:model-test-cells
                                                   #'model:model-cells))))
    (ok (= 1 (length cells)))
    (ok (string= "UITests" (getf (first cells) :scheme)))))

(deftest resolve-cells/cli-overrides-config
  (let* ((cmd (build-cmd (list "--cell" "OnlyOne@platform=iOS,id=Z")))
         (model (test-model (list :project-path "App.xcworkspace"
                                  :cells (list "A@platform=iOS,id=1"
                                               "B@platform=iOS,id=2"))))
         (cells (run-resolve cmd model)))
    (ok (= 1 (length cells)))
    (ok (string= "OnlyOne" (getf (first cells) :scheme)))))

(deftest resolve-cells/cartesian-product
  (let* ((cmd (build-cmd (list "--scheme" "A" "--scheme" "B"
                               "--device" "D1" "--device" "D2")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (ok (= 4 (length cells)))
    (ok (equal (list "A" "A" "B" "B")
               (mapcar (lambda (c) (getf c :scheme)) cells)))
    (ok (equal (list "platform=iOS,id=D1" "platform=iOS,id=D2"
                     "platform=iOS,id=D1" "platform=iOS,id=D2")
               (mapcar (lambda (c) (getf c :dest)) cells)))))


;;; -------------------------------------------------------------------------
;;; sim-device-and-runtime-name / destination-label enrichment
;;; -------------------------------------------------------------------------

(deftest sim-device-and-runtime-name/found
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (multiple-value-bind (model runtime)
        (cupertino::sim-device-and-runtime-name "UDID-1")
      (ok (string= "iPhone 17 Pro" model))
      (ok (string= "iOS 26.5" runtime)))))

(deftest sim-device-and-runtime-name/missing-returns-nils
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (multiple-value-bind (model runtime)
        (cupertino::sim-device-and-runtime-name "UNKNOWN-UDID")
      (ok (null model))
      (ok (null runtime)))))

(deftest destination-label/sim-enriched
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (ok (string= "sim:UDID-1 (iPhone 17 Pro, iOS 26.5)"
                 (cupertino::destination-label
                  "platform=iOS Simulator,id=UDID-1")))))

(deftest destination-label/sim-unknown-falls-back
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (ok (string= "sim:UNKNOWN"
                 (cupertino::destination-label
                  "platform=tvOS Simulator,id=UNKNOWN")))))

(deftest destination-label/device-unchanged
  (with-stubbed-sim-info (nil)
    (ok (string= "dev:D9"
                 (cupertino::destination-label "platform=iOS,id=D9")))))

;;; -------------------------------------------------------------------------
;;; sanitize-path-component / cell-derived-data-dir
;;; -------------------------------------------------------------------------

(deftest sanitize-path-component/preserves-safe-chars
  (ok (string= "Abc-123_v1.0"
               (cupertino::sanitize-path-component "Abc-123_v1.0"))))

(deftest sanitize-path-component/folds-unsafe-chars-to-dash
  (ok (string= "platform-iOS-Simulator-id-UDID-1"
               (cupertino::sanitize-path-component
                "platform=iOS Simulator,id=UDID-1"))))

(deftest sanitize-path-component/folds-slashes
  (ok (string= "a-b-c" (cupertino::sanitize-path-component "a/b/c"))))

(deftest sanitize-path-component/empty-string
  (ok (string= "" (cupertino::sanitize-path-component ""))))

(deftest sanitize-path-component/preserves-length
  (let ((s "platform=iOS Simulator,id=ABC/DEF"))
    (ok (= (length s) (length (cupertino::sanitize-path-component s))))))

(deftest cell-derived-data-dir/base-with-trailing-slash
  (ok (string= "/tmp/dd/SchemeA-platform-iOS-Simulator-id-AAA"
               (cupertino::cell-derived-data-dir
                "/tmp/dd/" "SchemeA" "platform=iOS Simulator,id=AAA"))))

(deftest cell-derived-data-dir/base-without-trailing-slash
  (ok (string= "/tmp/dd/SchemeA-platform-iOS-Simulator-id-AAA"
               (cupertino::cell-derived-data-dir
                "/tmp/dd" "SchemeA" "platform=iOS Simulator,id=AAA"))))

(deftest cell-derived-data-dir/stable-for-same-inputs
  (ok (string= (cupertino::cell-derived-data-dir "/base" "S" "platform=iOS,id=X")
               (cupertino::cell-derived-data-dir "/base" "S" "platform=iOS,id=X"))))

(deftest cell-derived-data-dir/distinct-for-distinct-cells
  (let ((a (cupertino::cell-derived-data-dir "/base" "iOS"  "platform=iOS,id=A"))
        (b (cupertino::cell-derived-data-dir "/base" "tvOS" "platform=tvOS,id=B")))
    (ok (not (string= a b)))))

(deftest default-isolated-derived-data-base/under-home
  (let ((base (cupertino::default-isolated-derived-data-base)))
    (ok (search "Library/Developer/Xcode/DerivedData-cupertino" base))
    (ok (char= #\/ (char base (1- (length base)))))))

;;; -------------------------------------------------------------------------
;;; resolve-cells: per-cell DerivedData isolation
;;; -------------------------------------------------------------------------

(deftest resolve-cells/single-cell-no-derived-data-omits-flag
  (let* ((cmd (build-cmd (list "--scheme" "A" "--device" "D1")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (ok (= 1 (length cells)))
    (ok (null (search "-derivedDataPath" (getf (first cells) :cmd))))))

(deftest resolve-cells/single-cell-honours-explicit-derived-data
  (let* ((cmd (build-cmd (list "--scheme" "A" "--device" "D1")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model :derived-data "/tmp/explicit")))
    (ok (= 1 (length cells)))
    ;; Single cell forwards the user's value verbatim -- no per-cell subdir.
    (ok (search "-derivedDataPath '/tmp/explicit'" (getf (first cells) :cmd)))))

(deftest resolve-cells/multi-cell-with-derived-data-base-each-cell-isolated
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model :derived-data "/tmp/dd")))
    (ok (= 2 (length cells)))
    (ok (search "-derivedDataPath '/tmp/dd/iOS-platform-iOS-Simulator-id-AAA'"
                (getf (first cells) :cmd)))
    (ok (search "-derivedDataPath '/tmp/dd/tvOS-platform-tvOS-Simulator-id-BBB'"
                (getf (second cells) :cmd)))
    (ok (not (string= (getf (first cells) :cmd)
                      (getf (second cells) :cmd))))))

(deftest resolve-cells/multi-cell-without-derived-data-uses-auto-base
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (ok (= 2 (length cells)))
    (let ((base (cupertino::default-isolated-derived-data-base)))
      (ok (search (format nil "-derivedDataPath '~AiOS-" base)
                  (getf (first cells) :cmd)))
      (ok (search (format nil "-derivedDataPath '~AtvOS-" base)
                  (getf (second cells) :cmd))))))

(deftest resolve-cells/multi-cell-paths-are-stable-across-calls
  (let* ((args (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                     "--cell" "tvOS@platform=tvOS Simulator,id=BBB"))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells-1 (run-resolve (build-cmd args) model :derived-data "/tmp/dd"))
         (cells-2 (run-resolve (build-cmd args) model :derived-data "/tmp/dd")))
    (ok (equal (mapcar (lambda (c) (getf c :cmd)) cells-1)
               (mapcar (lambda (c) (getf c :cmd)) cells-2)))))

;;; -------------------------------------------------------------------------
;;; Isolation note (printed when per-cell DerivedData kicks in)
;;; -------------------------------------------------------------------------

(deftest resolve-cells/multi-cell-prints-isolation-note
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (out (with-output-to-string (*standard-output*)
                (run-resolve cmd model :derived-data "/tmp/dd"))))
    (ok (search "per-cell DerivedData under /tmp/dd" out))))

(deftest resolve-cells/single-cell-prints-no-isolation-note
  (let* ((cmd (build-cmd (list "--scheme" "A" "--device" "D1")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (out (with-output-to-string (*standard-output*)
                (run-resolve cmd model))))
    (ok (null (search "per-cell DerivedData" out)))))

;;; -------------------------------------------------------------------------
;;; no-cells-resolvable-p (gates standalone `clean --prune')
;;; -------------------------------------------------------------------------

(deftest no-cells-resolvable-p/empty-is-true
  (let ((cmd (clean-cmd nil))
        (model (test-model (list :project-path "App.xcworkspace"))))
    (ok (cupertino::no-cells-resolvable-p
         cmd model (list #'model:model-schemes #'model:model-scheme)
         (list #'model:model-cells)))))

(deftest no-cells-resolvable-p/explicit-cell-is-false
  (let ((cmd (clean-cmd (list "--cell" "A@platform=iOS,id=Z")))
        (model (test-model (list :project-path "App.xcworkspace"))))
    (ok (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

(deftest no-cells-resolvable-p/explicit-scheme-is-false
  (let ((cmd (clean-cmd (list "--scheme" "A")))
        (model (test-model (list :project-path "App.xcworkspace"))))
    (ok (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

(deftest no-cells-resolvable-p/config-scheme-is-false
  (let ((cmd (clean-cmd nil))
        (model (test-model (list :project-path "App.xcworkspace"
                                 :scheme "ConfiguredScheme"))))
    (ok (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

(deftest no-cells-resolvable-p/config-cells-is-false
  (let ((cmd (clean-cmd nil))
        (model (test-model (list :project-path "App.xcworkspace"
                                 :cells (list "A@platform=iOS,id=Z")))))
    (ok (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

;;; -------------------------------------------------------------------------
;;; clean carries the --prune flag; build/test do not
;;; -------------------------------------------------------------------------

(deftest clean-command-has-prune-flag
  (let ((cmd (clean-cmd (list "--prune"))))
    (ok (clingon:getopt cmd :prune))))

(deftest build-command-has-no-prune-flag
  ;; --prune is clean-only; parsing it under build must fail.
  (ok (null (ignore-errors (build-cmd (list "--prune"))))))

;;; -------------------------------------------------------------------------
;;; Result bundles (.xcresult): -resultBundlePath wiring + end-of-run report
;;; -------------------------------------------------------------------------

(defun run-resolve-test (cmd model &key (derived-data nil))
  "resolve-cells with the test action, so cells carry result-bundle paths."
  (cupertino::resolve-cells cmd "test" "-workspace 'X'" "Debug" derived-data
                            model
                            (list #'model:model-schemes #'model:model-scheme)
                            (list #'model:model-test-cells #'model:model-cells)))

(deftest shell-quote-single/escapes-embedded-quote
  ;; Plain string: single quotes around the value, no escaping needed.
  (ok (string= "'/tmp/foo'" (cupertino::shell-quote-single "/tmp/foo")))
  ;; Spaces survive unmolested inside the single-quoted span.
  (ok (string= "'iPhone 16'" (cupertino::shell-quote-single "iPhone 16")))
  ;; Embedded apostrophe uses the POSIX '\'' close-reopen idiom so the shell
  ;; reads it as a literal single quote rather than mis-tokenizing the arg.
  (ok (string= "'O'\\''Brien'" (cupertino::shell-quote-single "O'Brien"))))

(deftest xcodebuild-command-string/quotes-paths-with-spaces
  ;; Each user-supplied string is single-quoted so spaces don't split args.
  (let ((s (cupertino::xcodebuild-command-string
            "-workspace 'X'" "My Scheme" "platform=iOS Simulator,id=AAA"
            "Debug" "/Users/me/Derived Data" "build" "/Users/me/r.xcresult")))
    (ok (search "-scheme 'My Scheme'" s))
    (ok (search "-destination 'platform=iOS Simulator,id=AAA'" s))
    (ok (search "-derivedDataPath '/Users/me/Derived Data'" s))
    (ok (search "-resultBundlePath '/Users/me/r.xcresult'" s))))

(deftest cell-result-bundle-path/deterministic-and-sanitized
  ;; No effective DerivedData -> fallback under the cupertino auto-base.
  (let ((p (cupertino::cell-result-bundle-path
            nil "iOS" "platform=iOS Simulator,id=AAA")))
    (ok (search (cupertino::default-isolated-derived-data-base) p))
    (ok (search "results/iOS-platform-iOS-Simulator-id-AAA.xcresult" p))
    (ok (string= p (cupertino::cell-result-bundle-path
                    nil "iOS" "platform=iOS Simulator,id=AAA"))))
  ;; Empty string is treated the same as nil (defensive against callers).
  (ok (string= (cupertino::cell-result-bundle-path nil "iOS" "D")
               (cupertino::cell-result-bundle-path "" "iOS" "D"))))

(deftest cell-result-bundle-path/co-locates-with-effective-derived-data
  ;; When the cell has an effective DerivedData (either from --derived-data or
  ;; per-cell isolation), the bundle lives next to its build artifacts so the
  ;; user's --derived-data choice is honoured and `clean --prune' clears both.
  (ok (string= "/tmp/dd/cupertino.xcresult"
               (cupertino::cell-result-bundle-path "/tmp/dd" "iOS" "D")))
  ;; Trailing slash on input is collapsed (not doubled).
  (ok (string= "/tmp/dd/cupertino.xcresult"
               (cupertino::cell-result-bundle-path "/tmp/dd/" "iOS" "D"))))

(deftest build-cell/test-action-adds-result-bundle
  ;; Single cell with no --derived-data: xcresult falls back to results/ path.
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cell (first (run-resolve-test cmd model)))
         (expected (cupertino::cell-result-bundle-path
                    nil "iOS" "platform=iOS Simulator,id=AAA")))
    (ok (string= expected (getf cell :xcresult)))
    (ok (search (format nil "-resultBundlePath '~A'" expected) (getf cell :cmd)))))

(deftest build-cell/test-action-co-locates-xcresult-with-derived-data
  ;; --derived-data is honoured: the bundle ends up under the user's chosen
  ;; DerivedData, not the cupertino auto-base.
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cell (first (run-resolve-test cmd model :derived-data "/tmp/cup-dd"))))
    (ok (search "/tmp/cup-dd/" (getf cell :xcresult)))
    (ok (search "cupertino.xcresult" (getf cell :xcresult)))
    (ok (search "-resultBundlePath '/tmp/cup-dd" (getf cell :cmd)))))

(deftest build-cell/build-action-omits-result-bundle
  ;; run-resolve uses the build action; no .xcresult should be wired in.
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cell (first (run-resolve cmd model))))
    (ok (null (getf cell :xcresult)))
    (ok (null (search "-resultBundlePath" (getf cell :cmd))))))

(deftest report-result-bundles/marks-missing-as-not-produced
  ;; A cell whose bundle exists is listed with its path; one whose bundle is
  ;; absent gets a `(not produced)' marker so the gap is unambiguous; cells
  ;; without an :xcresult at all (non-test) are ignored.
  (with-temp-dir (dir)
    (let* ((present (namestring (merge-pathnames "Present.xcresult" dir)))
           (absent (namestring (merge-pathnames "Absent.xcresult" dir))))
      (ensure-directories-exist (uiop:ensure-directory-pathname present))
      (let ((out (with-output-to-string (*standard-output*)
                   (cupertino::report-result-bundles
                    (list (list :label "iOS" :xcresult present)
                          (list :label "tvOS" :xcresult absent)
                          (list :label "noresult"))))))
        (ok (search "Result bundles:" out))
        (ok (search "iOS" out))
        (ok (search present out))
        (ok (search "tvOS" out))
        (ok (search "(not produced)" out))
        ;; The literal absent-path is not printed (replaced by the marker).
        (ok (null (search absent out)))))))

(deftest report-result-bundles/no-op-when-no-cell-has-xcresult
  ;; Non-test action: no cell carries :xcresult, so nothing is printed.
  (let ((out (with-output-to-string (*standard-output*)
               (cupertino::report-result-bundles
                (list (list :label "x") (list :label "y"))))))
    (ok (null (search "Result bundles:" out)))))

(deftest report-result-bundles/aligns-labels-in-matrix
  ;; Labels of differing widths are padded so the paths/markers line up.
  (with-temp-dir (dir)
    (let ((p (namestring (merge-pathnames "A.xcresult" dir))))
      (ensure-directories-exist (uiop:ensure-directory-pathname p))
      (let ((out (with-output-to-string (*standard-output*)
                   (cupertino::report-result-bundles
                    (list (list :label "short" :xcresult p)
                          (list :label "much-longer-label"
                                :xcresult "/missing.xcresult"))))))
        ;; Short label is padded out to the max label width before the path.
        (ok (search (format nil "short~vT" 17) out))))))

(deftest reset-result-bundle/removes-stale-and-creates-parent
  (with-temp-dir (dir)
    (let ((bundle (namestring (merge-pathnames "sub/R.xcresult" dir))))
      ;; Seed a stale bundle with a file inside.
      (ensure-directories-exist (uiop:ensure-directory-pathname bundle))
      (with-open-file (s (merge-pathnames "x" (uiop:ensure-directory-pathname bundle))
                         :direction :output :if-exists :supersede)
        (write-string "stale" s))
      (cupertino::reset-result-bundle bundle)
      ;; Stale bundle gone, but its parent directory remains for xcodebuild.
      (ok (not (uiop:directory-exists-p (uiop:ensure-directory-pathname bundle))))
      (ok (uiop:directory-exists-p
           (uiop:pathname-directory-pathname bundle))))))


;;; -------------------------------------------------------------------------
;;; Test action filters: --only-testing / --skip-testing
;;; -------------------------------------------------------------------------

(defun test-cmd (argv)
  "Parse ARGV against the matrix (test) command, returning the clingon command."
  (clingon:parse-command-line
   (cupertino::make-xcodebuild-command "test" "Test") argv))

(deftest build-test-filter-args/nil-when-both-empty
  ;; Empty inputs collapse to NIL so xcodebuild-command-string's ~@[ ~A~]
  ;; segment omits the trailing space entirely.
  (ok (null (cupertino::build-test-filter-args nil nil)))
  (ok (null (cupertino::build-test-filter-args '() '()))))

(deftest build-test-filter-args/only-testing-emits-flag-per-id
  (ok (string= "-only-testing:'FooTests'"
               (cupertino::build-test-filter-args (list "FooTests") nil)))
  (ok (string= "-only-testing:'FooTests/Bar' -only-testing:'FooTests/Baz/testQux'"
               (cupertino::build-test-filter-args
                (list "FooTests/Bar" "FooTests/Baz/testQux") nil))))

(deftest build-test-filter-args/skip-testing-alone-has-no-leading-space
  ;; A bare --skip-testing run must not produce a leading space that would
  ;; double up against xcodebuild-command-string's own format separator.
  (let ((s (cupertino::build-test-filter-args nil (list "FooTests/Slow"))))
    (ok (string= "-skip-testing:'FooTests/Slow'" s))
    (ok (not (char= #\Space (char s 0))))))

(deftest build-test-filter-args/mixed-only-then-skip
  ;; Order: every -only-testing first, then every -skip-testing, single-space
  ;; joined throughout.
  (ok (string= "-only-testing:'A' -only-testing:'B' -skip-testing:'C' -skip-testing:'D'"
               (cupertino::build-test-filter-args (list "A" "B")
                                                  (list "C" "D")))))

(deftest build-test-filter-args/shell-quotes-identifiers-with-spaces
  ;; xcodebuild identifiers normally don't contain spaces, but we still pass
  ;; user input through shell-quote-single so a stray space can't break the
  ;; assembled command line.
  (ok (string= "-only-testing:'My Tests/Foo'"
               (cupertino::build-test-filter-args (list "My Tests/Foo") nil))))

(deftest xcodebuild-command-string/appends-extra-args-after-action
  ;; Extras land after the action verb, single-space separated.
  (let ((s (cupertino::xcodebuild-command-string
            "-workspace 'X'" "S" "id=AAA" "Debug" nil "test" nil
            "-only-testing:'FooTests'")))
    (ok (search " test -only-testing:'FooTests'" s)))
  ;; Nil extras leave the action verb as the trailing token (no dangling space).
  (let ((s (cupertino::xcodebuild-command-string
            "-workspace 'X'" "S" "id=AAA" "Debug" nil "test")))
    (ok (string= " test" (subseq s (- (length s) 5)))))
  ;; Empty-string extras are treated the same as nil.
  (let ((s (cupertino::xcodebuild-command-string
            "-workspace 'X'" "S" "id=AAA" "Debug" nil "test" nil "")))
    (ok (string= " test" (subseq s (- (length s) 5))))))

(deftest test-command/forwards-only-and-skip-testing-into-cell-cmd
  ;; End-to-end: --only-testing / --skip-testing parsed from argv flow through
  ;; resolve-cells into each cell's xcodebuild command string, after the
  ;; action verb (not before it).
  (let* ((cmd (test-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                              "--only-testing" "FooTests"
                              "--only-testing" "FooTests/Bar"
                              "--skip-testing" "FooTests/Slow")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (extra-args (cupertino::build-test-filter-args
                      (alexandria:ensure-list (clingon:getopt cmd :only-testing))
                      (alexandria:ensure-list (clingon:getopt cmd :skip-testing))))
         (cells (cupertino::resolve-cells
                 cmd "test" "-workspace 'X'" "Debug" nil model
                 (list #'model:model-schemes #'model:model-scheme)
                 (list #'model:model-test-cells #'model:model-cells)
                 :extra-args extra-args))
         (cmd-str (getf (first cells) :cmd)))
    (ok (search "-only-testing:'FooTests'" cmd-str))
    (ok (search "-only-testing:'FooTests/Bar'" cmd-str))
    (ok (search "-skip-testing:'FooTests/Slow'" cmd-str))
    (let ((test-pos (search " test " cmd-str))
          (only-pos (search "-only-testing" cmd-str)))
      (ok (and test-pos only-pos (< test-pos only-pos))))))

(deftest test-command/no-filters-leaves-cmd-clean
  ;; Without --only-testing / --skip-testing, the test cell's cmd carries no
  ;; filter flags (regression guard for the extra-args plumbing).
  (let* ((cmd (test-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve-test cmd model))
         (cmd-str (getf (first cells) :cmd)))
    (ok (null (search "-only-testing" cmd-str)))
    (ok (null (search "-skip-testing" cmd-str)))))

(deftest build-command/rejects-only-testing-flag
  ;; --only-testing is a test-only option; the build subcommand must not
  ;; accept it (regression guard for the (string= action-description "test")
  ;; gate in xcodebuild-options).
  (ok (null (ignore-errors (build-cmd (list "--only-testing" "FooTests"))))))
