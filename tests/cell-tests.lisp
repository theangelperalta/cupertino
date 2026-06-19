;;;; cell-tests.lisp
;;;; Unit tests for cell parsing (parse-cell-option / parse-cell-destination)
;;;; and matrix-cell resolution (resolve-cells).

(in-package #:cupertino/tests)

(def-suite cupertino-cells
  :description "Cell parsing and scheme x destination resolution.")
(in-suite cupertino-cells)

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

(test parse-cell-option/raw-platform
  (multiple-value-bind (sch dest)
      (cupertino::parse-cell-option "MyScheme@platform=iOS Simulator,id=ABC")
    (is (string= "MyScheme" sch))
    (is (string= "platform=iOS Simulator,id=ABC" dest))))

(test parse-cell-option/device-shorthand
  (multiple-value-bind (sch dest) (cupertino::parse-cell-option "T@device=UDID9")
    (is (string= "T" sch))
    (is (string= "platform=iOS,id=UDID9" dest))))

(test parse-cell-option/no-at-returns-nil
  (is (null (cupertino::parse-cell-option "NoSeparator"))))

(test parse-cell-option/splits-on-first-at
  (multiple-value-bind (sch dest) (cupertino::parse-cell-option "S@platform=iOS,id=A")
    (is (string= "S" sch))
    (is (string= "platform=iOS,id=A" dest))))

(test parse-cell-destination/raw-passthrough
  (is (string= "platform=tvOS Simulator,id=Z"
               (cupertino::parse-cell-destination "platform=tvOS Simulator,id=Z"))))

(test parse-cell-destination/device
  (is (string= "platform=iOS,id=U" (cupertino::parse-cell-destination "device=U"))))

;;; -------------------------------------------------------------------------
;;; runtime-id-platform / sim-destination (platform auto-detection)
;;; -------------------------------------------------------------------------

(test runtime-id-platform/ios
  (is (string= "iOS" (cupertino::runtime-id-platform
                      "com.apple.CoreSimulator.SimRuntime.iOS-17-0"))))

(test runtime-id-platform/tvos
  (is (string= "tvOS" (cupertino::runtime-id-platform
                       "com.apple.CoreSimulator.SimRuntime.tvOS-17-4"))))

(test runtime-id-platform/no-marker
  (is (null (cupertino::runtime-id-platform "garbage-string"))))

(test sim-destination/ios
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) "iOS")
    (lambda () (is (string= "platform=iOS Simulator,id=U1"
                            (cupertino::sim-destination "U1"))))))

(test sim-destination/tvos
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) "tvOS")
    (lambda () (is (string= "platform=tvOS Simulator,id=U2"
                            (cupertino::sim-destination "U2"))))))

(test sim-destination/xros-maps-visionos
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) "xrOS")
    (lambda () (is (string= "platform=visionOS Simulator,id=U3"
                            (cupertino::sim-destination "U3"))))))

(test sim-destination/unknown-falls-back-ios
  (call-with-stubbed-platform (lambda (u) (declare (ignore u)) nil)
    (lambda () (is (string= "platform=iOS Simulator,id=U4"
                            (cupertino::sim-destination "U4"))))))

;;; -------------------------------------------------------------------------
;;; resolve-cells
;;; -------------------------------------------------------------------------

(test resolve-cells/cli-cells
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (is (= 2 (length cells)))
    (is (string= "iOS" (getf (first cells) :scheme)))
    (is (string= "platform=iOS Simulator,id=AAA" (getf (first cells) :dest)))
    (is (string= "tvOS" (getf (second cells) :scheme)))
    (is (search "-scheme 'iOS'" (getf (first cells) :cmd)))
    (is (search "-destination 'platform=tvOS Simulator,id=BBB'"
                (getf (second cells) :cmd)))))

(test resolve-cells/config-cells-fallback
  (let* ((cmd (build-cmd nil))
         (model (test-model (list :project-path "App.xcworkspace"
                                  :cells (list "iOS@platform=iOS Simulator,id=AAA"
                                               "tvOS@platform=tvOS Simulator,id=BBB"))))
         (cells (run-resolve cmd model)))
    (is (= 2 (length cells)))
    (is (string= "iOS" (getf (first cells) :scheme)))
    (is (string= "tvOS" (getf (second cells) :scheme)))))

(test resolve-cells/test-cells-accessor
  (let* ((cmd (build-cmd nil))
         (model (test-model (list :project-path "App.xcworkspace"
                                  :test-cells (list "UITests@platform=iOS Simulator,id=AAA"))))
         (cells (run-resolve cmd model
                             :cell-accessors (list #'model:model-test-cells
                                                   #'model:model-cells))))
    (is (= 1 (length cells)))
    (is (string= "UITests" (getf (first cells) :scheme)))))

(test resolve-cells/cli-overrides-config
  (let* ((cmd (build-cmd (list "--cell" "OnlyOne@platform=iOS,id=Z")))
         (model (test-model (list :project-path "App.xcworkspace"
                                  :cells (list "A@platform=iOS,id=1"
                                               "B@platform=iOS,id=2"))))
         (cells (run-resolve cmd model)))
    (is (= 1 (length cells)))
    (is (string= "OnlyOne" (getf (first cells) :scheme)))))

(test resolve-cells/cartesian-product
  (let* ((cmd (build-cmd (list "--scheme" "A" "--scheme" "B"
                               "--device" "D1" "--device" "D2")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (is (= 4 (length cells)))
    (is (equal (list "A" "A" "B" "B")
               (mapcar (lambda (c) (getf c :scheme)) cells)))
    (is (equal (list "platform=iOS,id=D1" "platform=iOS,id=D2"
                     "platform=iOS,id=D1" "platform=iOS,id=D2")
               (mapcar (lambda (c) (getf c :dest)) cells)))))


;;; -------------------------------------------------------------------------
;;; sim-device-and-runtime-name / destination-label enrichment
;;; -------------------------------------------------------------------------

(test sim-device-and-runtime-name/found
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (multiple-value-bind (model runtime)
        (cupertino::sim-device-and-runtime-name "UDID-1")
      (is (string= "iPhone 17 Pro" model))
      (is (string= "iOS 26.5" runtime)))))

(test sim-device-and-runtime-name/missing-returns-nils
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (multiple-value-bind (model runtime)
        (cupertino::sim-device-and-runtime-name "UNKNOWN-UDID")
      (is (null model))
      (is (null runtime)))))

(test destination-label/sim-enriched
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (is (string= "sim:UDID-1 (iPhone 17 Pro, iOS 26.5)"
                 (cupertino::destination-label
                  "platform=iOS Simulator,id=UDID-1")))))

(test destination-label/sim-unknown-falls-back
  (with-stubbed-sim-info ((list (list "UDID-1" "iPhone 17 Pro"
                                      "com.apple.CoreSimulator.SimRuntime.iOS-26-5"
                                      "iOS 26.5")))
    (is (string= "sim:UNKNOWN"
                 (cupertino::destination-label
                  "platform=tvOS Simulator,id=UNKNOWN")))))

(test destination-label/device-unchanged
  (with-stubbed-sim-info (nil)
    (is (string= "dev:D9"
                 (cupertino::destination-label "platform=iOS,id=D9")))))

;;; -------------------------------------------------------------------------
;;; sanitize-path-component / cell-derived-data-dir
;;; -------------------------------------------------------------------------

(test sanitize-path-component/preserves-safe-chars
  (is (string= "Abc-123_v1.0"
               (cupertino::sanitize-path-component "Abc-123_v1.0"))))

(test sanitize-path-component/folds-unsafe-chars-to-dash
  (is (string= "platform-iOS-Simulator-id-UDID-1"
               (cupertino::sanitize-path-component
                "platform=iOS Simulator,id=UDID-1"))))

(test sanitize-path-component/folds-slashes
  (is (string= "a-b-c" (cupertino::sanitize-path-component "a/b/c"))))

(test sanitize-path-component/empty-string
  (is (string= "" (cupertino::sanitize-path-component ""))))

(test sanitize-path-component/preserves-length
  (let ((s "platform=iOS Simulator,id=ABC/DEF"))
    (is (= (length s) (length (cupertino::sanitize-path-component s))))))

(test cell-derived-data-dir/base-with-trailing-slash
  (is (string= "/tmp/dd/SchemeA-platform-iOS-Simulator-id-AAA"
               (cupertino::cell-derived-data-dir
                "/tmp/dd/" "SchemeA" "platform=iOS Simulator,id=AAA"))))

(test cell-derived-data-dir/base-without-trailing-slash
  (is (string= "/tmp/dd/SchemeA-platform-iOS-Simulator-id-AAA"
               (cupertino::cell-derived-data-dir
                "/tmp/dd" "SchemeA" "platform=iOS Simulator,id=AAA"))))

(test cell-derived-data-dir/stable-for-same-inputs
  (is (string= (cupertino::cell-derived-data-dir "/base" "S" "platform=iOS,id=X")
               (cupertino::cell-derived-data-dir "/base" "S" "platform=iOS,id=X"))))

(test cell-derived-data-dir/distinct-for-distinct-cells
  (let ((a (cupertino::cell-derived-data-dir "/base" "iOS"  "platform=iOS,id=A"))
        (b (cupertino::cell-derived-data-dir "/base" "tvOS" "platform=tvOS,id=B")))
    (is (not (string= a b)))))

(test default-isolated-derived-data-base/under-home
  (let ((base (cupertino::default-isolated-derived-data-base)))
    (is (search "Library/Developer/Xcode/DerivedData-cupertino" base))
    (is (char= #\/ (char base (1- (length base)))))))

;;; -------------------------------------------------------------------------
;;; resolve-cells: per-cell DerivedData isolation
;;; -------------------------------------------------------------------------

(test resolve-cells/single-cell-no-derived-data-omits-flag
  (let* ((cmd (build-cmd (list "--scheme" "A" "--device" "D1")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (is (= 1 (length cells)))
    (is (null (search "-derivedDataPath" (getf (first cells) :cmd))))))

(test resolve-cells/single-cell-honours-explicit-derived-data
  (let* ((cmd (build-cmd (list "--scheme" "A" "--device" "D1")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model :derived-data "/tmp/explicit")))
    (is (= 1 (length cells)))
    ;; Single cell forwards the user's value verbatim -- no per-cell subdir.
    (is (search "-derivedDataPath '/tmp/explicit'" (getf (first cells) :cmd)))))

(test resolve-cells/multi-cell-with-derived-data-base-each-cell-isolated
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model :derived-data "/tmp/dd")))
    (is (= 2 (length cells)))
    (is (search "-derivedDataPath '/tmp/dd/iOS-platform-iOS-Simulator-id-AAA'"
                (getf (first cells) :cmd)))
    (is (search "-derivedDataPath '/tmp/dd/tvOS-platform-tvOS-Simulator-id-BBB'"
                (getf (second cells) :cmd)))
    (is (not (string= (getf (first cells) :cmd)
                      (getf (second cells) :cmd))))))

(test resolve-cells/multi-cell-without-derived-data-uses-auto-base
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells (run-resolve cmd model)))
    (is (= 2 (length cells)))
    (let ((base (cupertino::default-isolated-derived-data-base)))
      (is (search (format nil "-derivedDataPath '~AiOS-" base)
                  (getf (first cells) :cmd)))
      (is (search (format nil "-derivedDataPath '~AtvOS-" base)
                  (getf (second cells) :cmd))))))

(test resolve-cells/multi-cell-paths-are-stable-across-calls
  (let* ((args (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                     "--cell" "tvOS@platform=tvOS Simulator,id=BBB"))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cells-1 (run-resolve (build-cmd args) model :derived-data "/tmp/dd"))
         (cells-2 (run-resolve (build-cmd args) model :derived-data "/tmp/dd")))
    (is (equal (mapcar (lambda (c) (getf c :cmd)) cells-1)
               (mapcar (lambda (c) (getf c :cmd)) cells-2)))))

;;; -------------------------------------------------------------------------
;;; Isolation note (printed when per-cell DerivedData kicks in)
;;; -------------------------------------------------------------------------

(test resolve-cells/multi-cell-prints-isolation-note
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA"
                               "--cell" "tvOS@platform=tvOS Simulator,id=BBB")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (out (with-output-to-string (*standard-output*)
                (run-resolve cmd model :derived-data "/tmp/dd"))))
    (is (search "per-cell DerivedData under /tmp/dd" out))))

(test resolve-cells/single-cell-prints-no-isolation-note
  (let* ((cmd (build-cmd (list "--scheme" "A" "--device" "D1")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (out (with-output-to-string (*standard-output*)
                (run-resolve cmd model))))
    (is (null (search "per-cell DerivedData" out)))))

;;; -------------------------------------------------------------------------
;;; no-cells-resolvable-p (gates standalone `clean --prune')
;;; -------------------------------------------------------------------------

(test no-cells-resolvable-p/empty-is-true
  (let ((cmd (clean-cmd nil))
        (model (test-model (list :project-path "App.xcworkspace"))))
    (is (cupertino::no-cells-resolvable-p
         cmd model (list #'model:model-schemes #'model:model-scheme)
         (list #'model:model-cells)))))

(test no-cells-resolvable-p/explicit-cell-is-false
  (let ((cmd (clean-cmd (list "--cell" "A@platform=iOS,id=Z")))
        (model (test-model (list :project-path "App.xcworkspace"))))
    (is (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

(test no-cells-resolvable-p/explicit-scheme-is-false
  (let ((cmd (clean-cmd (list "--scheme" "A")))
        (model (test-model (list :project-path "App.xcworkspace"))))
    (is (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

(test no-cells-resolvable-p/config-scheme-is-false
  (let ((cmd (clean-cmd nil))
        (model (test-model (list :project-path "App.xcworkspace"
                                 :scheme "ConfiguredScheme"))))
    (is (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

(test no-cells-resolvable-p/config-cells-is-false
  (let ((cmd (clean-cmd nil))
        (model (test-model (list :project-path "App.xcworkspace"
                                 :cells (list "A@platform=iOS,id=Z")))))
    (is (not (cupertino::no-cells-resolvable-p
              cmd model (list #'model:model-schemes #'model:model-scheme)
              (list #'model:model-cells))))))

;;; -------------------------------------------------------------------------
;;; clean carries the --prune flag; build/test do not
;;; -------------------------------------------------------------------------

(test clean-command-has-prune-flag
  (let ((cmd (clean-cmd (list "--prune"))))
    (is (clingon:getopt cmd :prune))))

(test build-command-has-no-prune-flag
  ;; --prune is clean-only; parsing it under build must fail.
  (is (null (ignore-errors (build-cmd (list "--prune"))))))

;;; -------------------------------------------------------------------------
;;; Result bundles (.xcresult): -resultBundlePath wiring + end-of-run report
;;; -------------------------------------------------------------------------

(defun run-resolve-test (cmd model &key (derived-data nil))
  "resolve-cells with the test action, so cells carry result-bundle paths."
  (cupertino::resolve-cells cmd "test" "-workspace 'X'" "Debug" derived-data
                            model
                            (list #'model:model-schemes #'model:model-scheme)
                            (list #'model:model-test-cells #'model:model-cells)))

(test shell-quote-single/escapes-embedded-quote
  ;; Plain string: single quotes around the value, no escaping needed.
  (is (string= "'/tmp/foo'" (cupertino::shell-quote-single "/tmp/foo")))
  ;; Spaces survive unmolested inside the single-quoted span.
  (is (string= "'iPhone 16'" (cupertino::shell-quote-single "iPhone 16")))
  ;; Embedded apostrophe uses the POSIX '\'' close-reopen idiom so the shell
  ;; reads it as a literal single quote rather than mis-tokenizing the arg.
  (is (string= "'O'\\''Brien'" (cupertino::shell-quote-single "O'Brien"))))

(test xcodebuild-command-string/quotes-paths-with-spaces
  ;; Each user-supplied string is single-quoted so spaces don't split args.
  (let ((s (cupertino::xcodebuild-command-string
            "-workspace 'X'" "My Scheme" "platform=iOS Simulator,id=AAA"
            "Debug" "/Users/me/Derived Data" "build" "/Users/me/r.xcresult")))
    (is (search "-scheme 'My Scheme'" s))
    (is (search "-destination 'platform=iOS Simulator,id=AAA'" s))
    (is (search "-derivedDataPath '/Users/me/Derived Data'" s))
    (is (search "-resultBundlePath '/Users/me/r.xcresult'" s))))

(test cell-result-bundle-path/deterministic-and-sanitized
  ;; No effective DerivedData -> fallback under the cupertino auto-base.
  (let ((p (cupertino::cell-result-bundle-path
            nil "iOS" "platform=iOS Simulator,id=AAA")))
    (is (search (cupertino::default-isolated-derived-data-base) p))
    (is (search "results/iOS-platform-iOS-Simulator-id-AAA.xcresult" p))
    (is (string= p (cupertino::cell-result-bundle-path
                    nil "iOS" "platform=iOS Simulator,id=AAA"))))
  ;; Empty string is treated the same as nil (defensive against callers).
  (is (string= (cupertino::cell-result-bundle-path nil "iOS" "D")
               (cupertino::cell-result-bundle-path "" "iOS" "D"))))

(test cell-result-bundle-path/co-locates-with-effective-derived-data
  ;; When the cell has an effective DerivedData (either from --derived-data or
  ;; per-cell isolation), the bundle lives next to its build artifacts so the
  ;; user's --derived-data choice is honoured and `clean --prune' clears both.
  (is (string= "/tmp/dd/cupertino.xcresult"
               (cupertino::cell-result-bundle-path "/tmp/dd" "iOS" "D")))
  ;; Trailing slash on input is collapsed (not doubled).
  (is (string= "/tmp/dd/cupertino.xcresult"
               (cupertino::cell-result-bundle-path "/tmp/dd/" "iOS" "D"))))

(test build-cell/test-action-adds-result-bundle
  ;; Single cell with no --derived-data: xcresult falls back to results/ path.
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cell (first (run-resolve-test cmd model)))
         (expected (cupertino::cell-result-bundle-path
                    nil "iOS" "platform=iOS Simulator,id=AAA")))
    (is (string= expected (getf cell :xcresult)))
    (is (search (format nil "-resultBundlePath '~A'" expected) (getf cell :cmd)))))

(test build-cell/test-action-co-locates-xcresult-with-derived-data
  ;; --derived-data is honoured: the bundle ends up under the user's chosen
  ;; DerivedData, not the cupertino auto-base.
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cell (first (run-resolve-test cmd model :derived-data "/tmp/cup-dd"))))
    (is (search "/tmp/cup-dd/" (getf cell :xcresult)))
    (is (search "cupertino.xcresult" (getf cell :xcresult)))
    (is (search "-resultBundlePath '/tmp/cup-dd" (getf cell :cmd)))))

(test build-cell/build-action-omits-result-bundle
  ;; run-resolve uses the build action; no .xcresult should be wired in.
  (let* ((cmd (build-cmd (list "--cell" "iOS@platform=iOS Simulator,id=AAA")))
         (model (test-model (list :project-path "App.xcworkspace")))
         (cell (first (run-resolve cmd model))))
    (is (null (getf cell :xcresult)))
    (is (null (search "-resultBundlePath" (getf cell :cmd))))))

(test report-result-bundles/marks-missing-as-not-produced
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
        (is (search "Result bundles:" out))
        (is (search "iOS" out))
        (is (search present out))
        (is (search "tvOS" out))
        (is (search "(not produced)" out))
        ;; The literal absent-path is not printed (replaced by the marker).
        (is (null (search absent out)))))))

(test report-result-bundles/no-op-when-no-cell-has-xcresult
  ;; Non-test action: no cell carries :xcresult, so nothing is printed.
  (let ((out (with-output-to-string (*standard-output*)
               (cupertino::report-result-bundles
                (list (list :label "x") (list :label "y"))))))
    (is (null (search "Result bundles:" out)))))

(test report-result-bundles/aligns-labels-in-matrix
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
        (is (search (format nil "short~vT" 17) out))))))

(test reset-result-bundle/removes-stale-and-creates-parent
  (with-temp-dir (dir)
    (let ((bundle (namestring (merge-pathnames "sub/R.xcresult" dir))))
      ;; Seed a stale bundle with a file inside.
      (ensure-directories-exist (uiop:ensure-directory-pathname bundle))
      (with-open-file (s (merge-pathnames "x" (uiop:ensure-directory-pathname bundle))
                         :direction :output :if-exists :supersede)
        (write-string "stale" s))
      (cupertino::reset-result-bundle bundle)
      ;; Stale bundle gone, but its parent directory remains for xcodebuild.
      (is (not (uiop:directory-exists-p (uiop:ensure-directory-pathname bundle))))
      (is (uiop:directory-exists-p
           (uiop:pathname-directory-pathname bundle))))))
