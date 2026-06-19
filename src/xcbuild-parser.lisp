(in-package :cupertino)

;;;; Classifier for raw xcodebuild output lines.
;;;;
;;;; `classify-xcodebuild-line' turns a single line of xcodebuild output into a
;;;; plist describing what it represents, so the supercons front-end can render
;;;; a tidy summary instead of the raw firehose. It is pure (no side effects)
;;;; and handles both the legacy and the "new" build-system phrasings, mirroring
;;;; the rules used by xcbeautify/xcpretty.

(defun xcbuild-source-basename (line)
  "Return the basename of the first .swift/.m/.mm/.c/.cpp/.h path mentioned in
LINE, or NIL. Used to show `File.swift' instead of a long absolute path."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "([^/ ]+\\.(?:swift|m|mm|c|cc|cpp|cxx|h|hpp|metal|storyboard|xib))"
                                line)
    (declare (ignore groups))
    match))

(defun xcbuild-link-product (line)
  "Return the basename of the linked product in an `Ld'/`Link' LINE, or NIL.
e.g. \"Ld /build/Foo.app/Foo normal\" -> \"Foo\"."
  (let ((tokens (cl-ppcre:split "\\s+" line)))
    (when (second tokens)
      (car (last (cl-ppcre:split "/" (second tokens)))))))

(defun xcbuild-test-suite-name (line)
  "Extract the suite name from a Test case LINE:
\"Suite.method()\" -> \"Suite\" (Swift); \"-[Suite method]\" -> \"Suite\" (ObjC);
NIL if no match. The capture class deliberately excludes `.' so the Swift
`Suite.method()' form stops at the dot rather than greedily swallowing the
method name."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "Test [Cc]ase '(?:-\\[)?([A-Za-z0-9_]+)" line)
    (declare (ignore match))
    (when groups (aref groups 0))))

(defun xcbuild-test-summary-suite-name (line)
  "Extract the suite name from a `Test Suite 'X' passed/failed' LINE, or NIL.
Used to retire the suite's live-dashboard row once xcodebuild announces its
completion. Returns NIL for the run-level `Executed N tests' rollup."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings "^Test [Ss]uite '(.+?)' (?:passed|failed)" line)
    (declare (ignore match))
    (when groups (aref groups 0))))

(defun xcbuild-test-label (line)
  "Turn a `Test case 'NAME' passed/failed [on '…'] (N seconds)' LINE into a tidy
\"NAME (Ns)\". Falls back to LINE when it doesn't parse."
  (multiple-value-bind (match groups)
      (cl-ppcre:scan-to-strings
       "Test [Cc]ase '(.+?)' (?:passed|failed|errored)(?: on '[^']*')?(?: \\(([0-9.]+) seconds?\\))?"
       line)
    (if match
        (format nil "~A~@[ (~As)~]" (aref groups 0) (aref groups 1))
        line)))

(defun xcbuild-package-name (line)
  "Best-effort package name from a Swift Package Manager resolution LINE: the
quoted name in `... package 'NAME'' (straight or curly quotes) when present,
else the repo basename of a URL (`.../NAME.git' -> NAME), else NIL. The `.'
before the capture matches one opening quote char of any style."
  (or (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings "package .([A-Za-z0-9_.+-]+)" line)
        (declare (ignore match))
        (when groups (aref groups 0)))
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings "https?://\\S*?/([^/ ]+?)(?:\\.git)?/?\\s*$" line)
        (declare (ignore match))
        (when groups (aref groups 0)))))

(defun xcbuild-package-kind (line)
  "Short action verb for an SPM resolution-activity LINE, for the dashboard row:
\"fetch\"/\"clone\"/\"update\", or \"checkout\" for working-copy/checkout lines."
  (cond ((cl-ppcre:scan "^Fetching" line) "fetch")
        ((cl-ppcre:scan "^Cloning" line) "clone")
        ((cl-ppcre:scan "^Updating" line) "update")
        (t "checkout")))

(defparameter *xcbuild-rules*
  ;; Each rule is (regex . type). Evaluated top-to-bottom; first match wins.
  ;; Diagnostics (warning/error) are checked before the build-step rules so a
  ;; `... : error:` line is never mistaken for something else.
  `(("^\\*\\* (?:BUILD|TEST|CLEAN|ARCHIVE|ANALYZE) SUCCEEDED \\*\\*" . :result-success)
    ("^\\*\\* (?:BUILD|TEST|CLEAN|ARCHIVE|ANALYZE) FAILED \\*\\*"    . :result-failure)
    ;; Test reporting. Xcode 16+/26 lowercases the keyword and adds an
    ;; ` on '<device>'` clause ("Test case 'Suite/method()' passed on '…' (0.1
    ;; seconds)"); the patterns below also still match the legacy phrasing
    ;; ("Test Case '-[…]' passed (0.1 seconds)").
    ("^Testing started"                                             . :testing-start)
    ("^Test [Ss]uite '(.+?)' started"                              . :test-suite-start)
    ("^Test [Cc]ase '(.+?)' passed"                                . :test-case-passed)
    ("^Test [Cc]ase '(.+?)' (?:failed|errored)"                    . :test-case-failed)
    ("^Test [Ss]uite '.+' (?:passed|failed)|^Executed \\d+ test"   . :test-suite-summary)
    ;; Swift Package Manager resolution. Shown as live dashboard rows, not
    ;; scrollback: `Resolve Package Graph` starts it, per-package
    ;; `Fetching/Cloning/Updating/Creating working copy/Checking out` lines drive
    ;; the rows, and `Resolved source packages:` (or its lowercase one-line
    ;; summary) ends it. None of these collide with the diagnostic rules below.
    ("^Resolve Package Graph"                                       . :package-resolve-start)
    ("^(?:Fetching|Cloning|Updating|Creating working copy of|Checking out)\\b" . :package-fetch)
    ("^[Rr]esolved source packages"                                 . :package-resolved)
    (": (?:fatal )?error:|^(?:ld|clang): error:|^fatal error:"      . :error)
    (": warning:|^(?:ld|clang): warning:"                           . :warning)
    ("^(?:=== BUILD TARGET |Build target |=== ANALYZE TARGET )"     . :phase)
    ("^(?:CompileSwift|SwiftCompile|SwiftEmitModule|CompileSwiftSources|CompileC|CompileXIB|CompileStoryboard|CompileAssetCatalog|CompileMetalFile)\\b" . :compile)
    ("^(?:Ld|Link)\\b"                                              . :link)
    ("^(?:CpResource|CopySwiftLibs|PBXCp|Copy|CopyPlistFile|CopyStringsFile|ProcessInfoPlistFile)\\b" . :copy)
    ("^CodeSign\\b"                                                 . :codesign)
    ("^(?:Touch|RegisterExecutionPolicyException|CreateBuildDirectory|WriteAuxiliaryFile|ProcessProductPackaging|MkDir|GenerateDSYMFile)\\b" . :other))
  "Ordered (compiled-regex . type) rules for xcodebuild line classification.")

;; Compile the regexes once at load time.
(defparameter *xcbuild-compiled-rules*
  (mapcar (lambda (rule)
            (cons (cl-ppcre:create-scanner (car rule)) (cdr rule)))
          *xcbuild-rules*))

(defun classify-xcodebuild-line (line)
  "Classify a single xcodebuild output LINE.
Return a plist (:type KEYWORD :file STRING-OR-NIL :text LINE). TYPE is one of
:result-success :result-failure :testing-start :test-suite-start
:test-case-passed :test-case-failed :test-suite-summary :package-resolve-start
:package-fetch :package-resolved :error :warning :phase :compile :link :copy
:codesign :other, or :unknown when no rule matches."
  (let ((type (loop for (scanner . ty) in *xcbuild-compiled-rules*
                    when (cl-ppcre:scan scanner line)
                      return ty)))
    (list :type (or type :unknown)
          :file (when (member type '(:compile :link))
                  (xcbuild-source-basename line))
          :text line)))
