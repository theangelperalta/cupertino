;;;; xcbuild-pretty-tests.lisp
;;;; Unit tests for span sanitization in xcbuild-pretty.

(in-package #:cupertino/tests)

;;; -------------------------------------------------------------------------
;;; xcbuild-normalize-ws
;;; -------------------------------------------------------------------------

(deftest normalize-ws/leading-tab-becomes-space
  (ok (string= " /Users/foo/x.xcresult"
               (cupertino::xcbuild-normalize-ws
                (format nil "~C/Users/foo/x.xcresult" #\Tab)))))

(deftest normalize-ws/embedded-newline-and-cr
  (ok (string= "a b c"
               (cupertino::xcbuild-normalize-ws
                (format nil "a~Cb~Cc" #\Newline #\Return)))))

(deftest normalize-ws/nbsp-and-thin-space
  (ok (string= "x y z"
               (cupertino::xcbuild-normalize-ws
                (format nil "x~Cy~Cz" (code-char 160) (code-char 8201))))))

(deftest normalize-ws/preserves-regular-spaces-without-collapsing
  (ok (string= "a   b"
               (cupertino::xcbuild-normalize-ws "a   b"))))

(deftest normalize-ws/preserves-length
  (let ((s (format nil "~C~Ca~C~Cb~C" #\Tab #\Newline #\Return
                                       (code-char 160) #\Tab)))
    (ok (= (length s) (length (cupertino::xcbuild-normalize-ws s))))))

(deftest normalize-ws/clean-string-unchanged
  (ok (string= "no whitespace problems here"
               (cupertino::xcbuild-normalize-ws
                "no whitespace problems here"))))

(deftest normalize-ws/non-string-passthrough
  (ok (eq :keyword (cupertino::xcbuild-normalize-ws :keyword))))

;;; -------------------------------------------------------------------------
;;; xcbuild-span (the actual crash site)
;;; -------------------------------------------------------------------------

(deftest xcbuild-span/leading-tab-does-not-signal
  ;; Reproduces the original SPAN-ERROR-INVALID-WHITESPACE crash where
  ;; xcodebuild emitted a result-bundle path prefixed by a tab.
  (let* ((text (format nil "~C/Users/foo/x.xcresult" #\Tab))
         (span (finishes (cupertino::xcbuild-span text))))
    (ok (string= " /Users/foo/x.xcresult" (supercons:span-content span)))))

(deftest xcbuild-span/styled-with-mixed-whitespace
  (let* ((text (format nil "line~C~Cwith~Cws" #\Tab #\Newline #\Return))
         (span (finishes (cupertino::xcbuild-span text #'supercons:red))))
    (ok (string= "line  with ws" (supercons:span-content span)))))

(deftest xcbuild-span/clean-input-roundtrips
  (let ((span (cupertino::xcbuild-span "plain text")))
    (ok (string= "plain text" (supercons:span-content span)))))


;;; -------------------------------------------------------------------------
;;; xcbuild-test-suite-name / xcbuild-test-summary-suite-name
;;; -------------------------------------------------------------------------

(deftest test-suite-name/swift-stops-at-dot
  ;; The dashboard-row label must be the suite alone — including the method
  ;; would create one row per test case, which is the bug behind the lingering
  ;; tvOS "Suite.method -- running (1✓)" rows.
  (ok (string= "SkyAVPlayerEngineTVTests"
               (cupertino::xcbuild-test-suite-name
                "Test case 'SkyAVPlayerEngineTVTests.testStop' passed on 'Apple TV 4K (3rd generation)' (0.123 seconds)"))))

(deftest test-suite-name/objc-stops-at-space
  (ok (string= "MySuiteTests"
               (cupertino::xcbuild-test-suite-name
                "Test Case '-[MySuiteTests testFoo]' passed (0.012 seconds)"))))

(deftest test-suite-name/swift-failed-line
  (ok (string= "SuiteTests"
               (cupertino::xcbuild-test-suite-name
                "Test case 'SuiteTests.testBar()' failed (0.5 seconds)"))))

(deftest test-suite-name/no-match-returns-nil
  (ok (null (cupertino::xcbuild-test-suite-name "not a test line"))))

(deftest test-summary-suite-name/passed
  (ok (string= "SkyAVPlayerEngineTVTests"
               (cupertino::xcbuild-test-summary-suite-name
                "Test Suite 'SkyAVPlayerEngineTVTests' passed at 2026-06-18 09:59:37.000."))))

(deftest test-summary-suite-name/failed
  (ok (string= "Foo.xctest"
               (cupertino::xcbuild-test-summary-suite-name
                "Test Suite 'Foo.xctest' failed at 2026-06-18 09:59:37.000."))))

(deftest test-summary-suite-name/executed-rollup-returns-nil
  (ok (null (cupertino::xcbuild-test-summary-suite-name
             "Executed 76 tests, with 0 failures (0 unexpected) in 12.345 (12.567) seconds"))))

;;; -------------------------------------------------------------------------
;;; xcbuild-retire-suite — live-dashboard row removal on suite completion
;;; -------------------------------------------------------------------------

(defun bump (stats suite) (cupertino::xcbuild-bump-suite stats suite t))

(deftest retire-suite/removes-only-matching-test-row
  (let ((stats (cupertino::make-xcbuild-stats)))
    (bump stats "SuiteA") (bump stats "SuiteB") (bump stats "SuiteA")
    (ok (= 2 (length (cupertino::xcbuild-stats-jobs stats))))
    (cupertino::xcbuild-retire-suite stats "SuiteA")
    (let ((jobs (cupertino::xcbuild-stats-jobs stats)))
      (ok (= 1 (length jobs)))
      (ok (string= "SuiteB" (cupertino::xcbuild-job-label (first jobs)))))))

(deftest retire-suite/nil-suite-is-noop
  (let ((stats (cupertino::make-xcbuild-stats)))
    (bump stats "SuiteA")
    (cupertino::xcbuild-retire-suite stats nil)
    (ok (= 1 (length (cupertino::xcbuild-stats-jobs stats))))))

(deftest retire-suite/unknown-suite-is-noop
  (let ((stats (cupertino::make-xcbuild-stats)))
    (bump stats "SuiteA")
    (cupertino::xcbuild-retire-suite stats "Nope")
    (ok (= 1 (length (cupertino::xcbuild-stats-jobs stats))))))

(deftest retire-suite/leaves-non-test-jobs-alone
  ;; A build action that happens to share a label string with a finished suite
  ;; must not be evicted by suite retirement.
  (let* ((stats (cupertino::make-xcbuild-stats))
         (build-job (cupertino::make-xcbuild-job :label "SuiteA" :kind "compile")))
    (setf (cupertino::xcbuild-stats-jobs stats) (list build-job))
    (bump stats "SuiteA")
    (cupertino::xcbuild-retire-suite stats "SuiteA")
    (let ((jobs (cupertino::xcbuild-stats-jobs stats)))
      (ok (= 1 (length jobs)))
      (ok (null (cupertino::xcbuild-job-test (first jobs))))
      (ok (string= "compile" (cupertino::xcbuild-job-kind (first jobs)))))))

;;; -------------------------------------------------------------------------
;;; xcbuild-swb-text-event — SWB-mode gate for xcodebuild's text stream
;;;
;;; In --use-swb mode the build graph is driven by protocol events; only the
;;; test-execution phase (which emits no SWB events) is allowed to drive the
;;; dashboard from text, so build-graph lines must be filtered out here to
;;; avoid double-counting.
;;; -------------------------------------------------------------------------

(deftest swb-text-event/passes-test-case-passed
  (let ((ev (cupertino::xcbuild-swb-text-event
             "Test case 'SuiteA.testFoo()' passed (0.1 seconds)")))
    (ok (eq :test-case-passed (getf ev :type)))))

(deftest swb-text-event/passes-test-case-failed
  (let ((ev (cupertino::xcbuild-swb-text-event
             "Test case 'SuiteA.testBar()' failed (0.5 seconds)")))
    (ok (eq :test-case-failed (getf ev :type)))))

(deftest swb-text-event/passes-testing-start
  (let ((ev (cupertino::xcbuild-swb-text-event "Testing started")))
    (ok (eq :testing-start (getf ev :type)))))

(deftest swb-text-event/passes-test-suite-summary
  (let ((ev (cupertino::xcbuild-swb-text-event
             "Test Suite 'SuiteA' passed at 2026-06-18 09:59:37.000.")))
    (ok (eq :test-suite-summary (getf ev :type)))))

(deftest swb-text-event/passes-result-success
  (let ((ev (cupertino::xcbuild-swb-text-event "** TEST SUCCEEDED **")))
    (ok (eq :result-success (getf ev :type)))))

(deftest swb-text-event/excludes-compile
  ;; SWB protocol events already count compiles; the text copy must be ignored.
  (ok (null (cupertino::xcbuild-swb-text-event
             "CompileSwift normal arm64 /tmp/Foo.swift"))))

(deftest swb-text-event/excludes-warning
  (ok (null (cupertino::xcbuild-swb-text-event
             "/tmp/Foo.swift:1:1: warning: unused variable"))))

(deftest swb-text-event/excludes-plain-line
  (ok (null (cupertino::xcbuild-swb-text-event "just some chatter"))))

(deftest swb-text-event/test-line-updates-stats-build-line-does-not
  ;; Feeding a gated test line through xcbuild-handle-line bumps the test count
  ;; and adds a suite row; a build line is filtered out (no compiled bump).
  (let ((stats (cupertino::make-xcbuild-stats :action "test")))
    (ok (null (cupertino::xcbuild-swb-text-event
               "CompileSwift normal arm64 /tmp/Foo.swift")))
    (let ((ev (cupertino::xcbuild-swb-text-event
               "Test case 'SuiteA.testFoo()' passed (0.1 seconds)")))
      (cupertino::xcbuild-handle-line nil stats ev nil))
    (ok (= 1 (cupertino::xcbuild-stats-tests-passed stats)))
    (ok (= 0 (cupertino::xcbuild-stats-compiled stats)))
    (ok (= 1 (length (cupertino::xcbuild-stats-jobs stats))))
    (ok (cupertino::xcbuild-job-test (first (cupertino::xcbuild-stats-jobs stats))))))

;;; -------------------------------------------------------------------------
;;; Build vs test phase indicator + banner relabel
;;;
;;; A `test' action has two phases: building the test bundles, then running
;;; them. The dashboard prefixes a "Building"/"Testing" phase label and the
;;; build-phase `** TEST SUCCEEDED **' banner is relabelled so it is not read
;;; as the final test verdict.
;;; -------------------------------------------------------------------------

(deftest phase-label/nil-for-build-action
  (ok (null (cupertino::xcbuild-phase-label
             (cupertino::make-xcbuild-stats :action "build")))))

(deftest phase-label/building-then-testing-for-test-action
  (let ((stats (cupertino::make-xcbuild-stats :action "test")))
    (ok (string= "Building" (cupertino::xcbuild-phase-label stats)))
    (setf (cupertino::xcbuild-stats-phase stats) :test)
    (ok (string= "Testing" (cupertino::xcbuild-phase-label stats)))))

(deftest phase-label/counts-string-shows-phase-prefix
  (let ((stats (cupertino::make-xcbuild-stats :action "test")))
    (ok (search "Building · Jobs:" (cupertino::xcbuild-counts-string stats)))
    (setf (cupertino::xcbuild-stats-phase stats) :test)
    (ok (search "Testing · Jobs:" (cupertino::xcbuild-counts-string stats)))))

(deftest phase-label/counts-string-no-prefix-for-build
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (ok (null (search " · " (cupertino::xcbuild-counts-string stats))))))

(deftest banner/build-phase-test-succeeded-is-relabelled
  (let ((stats (cupertino::make-xcbuild-stats :action "test")))
    (ok (string= "** BUILD FOR TESTING SUCCEEDED **"
                 (cupertino::xcbuild-result-banner-text stats "** TEST SUCCEEDED **")))
    (ok (string= "** BUILD FOR TESTING FAILED **"
                 (cupertino::xcbuild-result-banner-text stats "** TEST FAILED **")))))

(deftest banner/test-phase-test-succeeded-passes-through
  (let ((stats (cupertino::make-xcbuild-stats :action "test")))
    (setf (cupertino::xcbuild-stats-phase stats) :test)
    (ok (string= "** TEST SUCCEEDED **"
                 (cupertino::xcbuild-result-banner-text stats "** TEST SUCCEEDED **")))))

(deftest banner/non-test-action-passes-through
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (ok (string= "** BUILD SUCCEEDED **"
                 (cupertino::xcbuild-result-banner-text stats "** BUILD SUCCEEDED **")))))

(deftest phase/testing-start-flips-phase-to-test
  ;; Driving a :testing-start line through the handler must flip phase :build ->
  ;; :test. A queue-only console (no output backend) keeps the emit side-effect
  ;; free of any terminal writes.
  (let ((stats (cupertino::make-xcbuild-stats :action "test"))
        (console (supercons:make-superconsole-with-output nil nil)))
    (ok (eq :build (cupertino::xcbuild-stats-phase stats)))
    (cupertino::xcbuild-handle-line
     console stats (cupertino::classify-xcodebuild-line "Testing started") nil)
    (ok (eq :test (cupertino::xcbuild-stats-phase stats)))))

;;; -------------------------------------------------------------------------
;;; Swift Package Manager resolution — classifier, extractors, live rows
;;;
;;; Sampled from a real Xcode 26.5 cold resolution. Note the curly quotes
;;; (U+2018/U+2019) around package names in the `package '…'` lines.
;;; -------------------------------------------------------------------------

(defun curly (name)
  "Wrap NAME in U+2018/U+2019 curly quotes, as xcodebuild prints package names."
  (format nil "~Cpackage ~C~A~C" #\Space (code-char #x2018) name (code-char #x2019)))

(deftest classify/package-resolve-start
  (ok (eq :package-resolve-start
          (getf (cupertino::classify-xcodebuild-line "Resolve Package Graph") :type))))

(deftest classify/package-fetch-variants
  (dolist (line (list "Fetching from https://github.com/apple/swift-argument-parser.git"
                      "Cloning https://github.com/Comcast/mamba.git"
                      "Updating https://github.com/foo/Bar"
                      (format nil "Creating working copy of~A" (curly "mamba"))
                      (format nil "Checking out 1.8.2 of~A" (curly "swift-argument-parser"))))
    (ok (eq :package-fetch
            (getf (cupertino::classify-xcodebuild-line line) :type)))))

(deftest classify/package-resolved-both-cases
  (ok (eq :package-resolved
          (getf (cupertino::classify-xcodebuild-line "Resolved source packages:") :type)))
  (ok (eq :package-resolved
          (getf (cupertino::classify-xcodebuild-line
                 "resolved source packages: swift-argument-parser, MiniProbe")
                :type))))

(deftest package-name/curly-quoted
  (ok (string= "swift-argument-parser"
               (cupertino::xcbuild-package-name
                (format nil "Checking out 1.8.2 of~A" (curly "swift-argument-parser"))))))

(deftest package-name/straight-quoted
  (ok (string= "mamba"
               (cupertino::xcbuild-package-name
                "Creating working copy of package 'mamba'"))))

(deftest package-name/url-with-and-without-git
  (ok (string= "swift-argument-parser"
               (cupertino::xcbuild-package-name
                "Fetching from https://github.com/apple/swift-argument-parser.git")))
  (ok (string= "Bar"
               (cupertino::xcbuild-package-name "Updating https://github.com/foo/Bar"))))

(deftest package-kind/maps-leading-verb
  (ok (string= "fetch" (cupertino::xcbuild-package-kind "Fetching from https://x/y.git")))
  (ok (string= "clone" (cupertino::xcbuild-package-kind "Cloning https://x/y.git")))
  (ok (string= "update" (cupertino::xcbuild-package-kind "Updating https://x/y")))
  (ok (string= "checkout" (cupertino::xcbuild-package-kind "Checking out 1.0 of package 'y'"))))

(deftest package/fetch-lines-create-and-refresh-rows-then-retire
  ;; Two packages -> two resolve rows; a repeat activity line for one refreshes
  ;; (does not duplicate) its row; the completion summary retires all rows.
  (let ((stats (cupertino::make-xcbuild-stats :action "build"))
        (console (supercons:make-superconsole-with-output nil nil)))
    (flet ((feed (line)
             (cupertino::xcbuild-handle-line
              console stats (cupertino::classify-xcodebuild-line line) nil)))
      (feed "Fetching from https://github.com/apple/swift-argument-parser.git")
      (feed "Cloning https://github.com/Comcast/mamba.git")
      (ok (= 2 (length (cupertino::xcbuild-stats-jobs stats))))
      (ok (every #'cupertino::xcbuild-job-resolve (cupertino::xcbuild-stats-jobs stats)))
      ;; A second line for an existing package updates its row's kind in place.
      (feed (format nil "Checking out 1.8.2 of~A" (curly "swift-argument-parser")))
      (ok (= 2 (length (cupertino::xcbuild-stats-jobs stats))))
      (let ((row (find "swift-argument-parser" (cupertino::xcbuild-stats-jobs stats)
                       :key #'cupertino::xcbuild-job-label :test #'string=)))
        (ok (string= "checkout" (cupertino::xcbuild-job-kind row))))
      ;; Completion retires every resolve row.
      (feed "Resolved source packages:")
      (ok (= 0 (length (cupertino::xcbuild-stats-jobs stats)))))))

(deftest swb-text-event/passes-package-resolution-lines
  (ok (eq :package-resolve-start
          (getf (cupertino::xcbuild-swb-text-event "Resolve Package Graph") :type)))
  (ok (eq :package-fetch
          (getf (cupertino::xcbuild-swb-text-event
                 "Fetching from https://github.com/apple/swift-argument-parser.git")
                :type)))
  (ok (eq :package-resolved
          (getf (cupertino::xcbuild-swb-text-event "Resolved source packages:") :type))))

;;; -------------------------------------------------------------------------
;;; SWB frame->event :trace-unknown and dashboard wiring
;;; -------------------------------------------------------------------------

(defun mpk-body (name) (messagepack:encode name))

(deftest frame->event/known-name-yields-typed-event-without-trace
  (let ((ev (swb:frame->event 1 (mpk-body "BUILD_TARGET_STARTED"))))
    (ok (eq :target-started (getf ev :type)))
    (ok (string= "BUILD_TARGET_STARTED" (getf ev :name)))))

(deftest frame->event/unknown-name-yields-nil-without-trace
  (ok (null (swb:frame->event 1 (mpk-body "SOMETHING_NEW_IN_XCODE_27")))))

(deftest frame->event/unknown-name-yields-unknown-event-with-trace
  (let ((ev (swb:frame->event 7 (mpk-body "SOMETHING_NEW_IN_XCODE_27")
                              :trace-unknown t)))
    (ok (eq :unknown (getf ev :type)))
    (ok (string= "SOMETHING_NEW_IN_XCODE_27" (getf ev :name)))
    (ok (= 7 (getf ev :channel)))
    (ok (null (getf ev :args)))))

(deftest frame->event/known-name-still-typed-with-trace
  ;; trace-unknown only synthesizes for *unknown* names; known ones keep their
  ;; typed dispatch so the dashboard still consumes them.
  (let ((ev (swb:frame->event 1 (mpk-body "BUILD_TARGET_ENDED") :trace-unknown t)))
    (ok (eq :target-ended (getf ev :type)))))

(deftest handle-event/target-started-and-ended-bump-counters
  (let ((stats (cupertino::make-xcbuild-stats :action "build"))
        (console (supercons:make-superconsole-with-output nil nil)))
    (cupertino::xcbuild-handle-event console stats '(:type :target-started))
    (cupertino::xcbuild-handle-event console stats '(:type :target-started))
    (cupertino::xcbuild-handle-event console stats '(:type :target-ended))
    (ok (= 2 (cupertino::xcbuild-stats-targets-started stats)))
    (ok (= 1 (cupertino::xcbuild-stats-targets-ended stats)))))

(deftest handle-event/build-started-is-noop-console-output-is-counted
  ;; :build-started is a true no-op; :console-output is counted (so the
  ;; trace digest can surface its frequency) but emits nothing.
  (let ((stats (cupertino::make-xcbuild-stats :action "build"))
        (console (supercons:make-superconsole-with-output nil nil)))
    (cupertino::xcbuild-handle-event console stats '(:type :build-started))
    (cupertino::xcbuild-handle-event console stats
                                     '(:type :console-output :args ("anything")))
    (cupertino::xcbuild-handle-event console stats
                                     '(:type :console-output :args ("more")))
    (ok (= 0 (cupertino::xcbuild-stats-finished stats)))
    (ok (= 0 (cupertino::xcbuild-stats-targets-started stats)))
    (ok (= 2 (cupertino::xcbuild-stats-console-outputs stats)))
    ;; The unknown-msgs table is allocated lazily; neither of the handled
    ;; event types should trigger that allocation.
    (ok (null (cupertino::xcbuild-stats-unknown-msgs stats)))))

(deftest handle-event/unknown-event-dedups-and-counts
  ;; First sighting records the name with count 1; subsequent sightings bump
  ;; the count without registering a new key. The hash-table is lazily
  ;; allocated on first unknown sighting.
  (let ((stats (cupertino::make-xcbuild-stats :action "build"))
        (console (supercons:make-superconsole-with-output nil nil)))
    (ok (null (cupertino::xcbuild-stats-unknown-msgs stats)))
    (dotimes (i 3)
      (cupertino::xcbuild-handle-event
       console stats '(:type :unknown :name "NEW_SERVICE_MSG" :channel 1)))
    (cupertino::xcbuild-handle-event
     console stats '(:type :unknown :name "ANOTHER_NEW_MSG" :channel 1))
    (let ((table (cupertino::xcbuild-stats-unknown-msgs stats)))
      (ok (hash-table-p table))
      (ok (= 2 (hash-table-count table)))
      (ok (= 3 (gethash "NEW_SERVICE_MSG" table)))
      (ok (= 1 (gethash "ANOTHER_NEW_MSG" table))))))

(deftest counts-string/omits-targets-segment
  ;; Target counts are deliberately surfaced in the final summary, not the
  ;; live counts string (BUILD_TARGET_STARTED arrives incrementally so the
  ;; denominator would grow over the run).
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (setf (cupertino::xcbuild-stats-targets-started stats) 4
          (cupertino::xcbuild-stats-targets-ended stats) 1)
    (ok (not (search "Targets" (cupertino::xcbuild-counts-string stats))))))

(deftest final-summary/includes-targets-when-ended
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (setf (cupertino::xcbuild-stats-targets-ended stats) 6)
    (ok (search "6 targets" (cupertino::xcbuild-final-summary stats)))))

(deftest final-summary/omits-targets-when-zero
  (ok (not (search "target"
                   (cupertino::xcbuild-final-summary
                    (cupertino::make-xcbuild-stats :action "build"))))))

;;; -------------------------------------------------------------------------
;;; trace digest formatter (xcbuild-trace-digest-body + emit-trace-digest)
;;; -------------------------------------------------------------------------

(deftest trace-digest-body/nil-when-empty
  ;; No unknown entries, no console-output count -> nothing to digest.
  (ok (null (cupertino::xcbuild-trace-digest-body nil 0))))

(deftest trace-digest-body/entries-only
  (ok (string= "FOO ×3, BAR ×1"
               (cupertino::xcbuild-trace-digest-body
                '(("FOO" . 3) ("BAR" . 1)) 0))))

(deftest trace-digest-body/console-only
  (ok (string= "console-output ×7"
               (cupertino::xcbuild-trace-digest-body nil 7))))

(deftest trace-digest-body/entries-and-console-appended
  (ok (string= "FOO ×2, console-output ×5"
               (cupertino::xcbuild-trace-digest-body '(("FOO" . 2)) 5))))

(deftest trace-digest-body/caps-at-top-n
  ;; CAP entries fit verbatim; one more triggers the overflow tail. The body
  ;; function preserves caller order (sorting is the caller's job); we feed
  ;; sorted entries so the cap drops the lowest-count tail.
  (let* ((cap cupertino::*xcbuild-trace-digest-cap*)
         (entries (loop for i from (1+ cap) downto 1
                        collect (cons (format nil "M~D" i) i)))
         (body (cupertino::xcbuild-trace-digest-body entries 0)))
    (ok (search "(and 1 more)" body))
    ;; The lowest-count entry (M1) is the one elided.
    (ok (not (search "M1 ×1" body)))))

(deftest emit-trace-digest/noop-when-console-nil
  ;; CONSOLE=NIL means the run is non-interactive; emit must be silent and
  ;; non-erroring even when both data buckets are full.
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (setf (cupertino::xcbuild-stats-console-outputs stats) 4)
    (let ((tbl (cupertino::xcbuild-swb-ensure-unknown-table stats)))
      (setf (gethash "FOO" tbl) 2))
    (finishes (cupertino::xcbuild-emit-trace-digest nil stats))))

;;; -------------------------------------------------------------------------
;;; SWB env prefix: every interpolated value must flow through shell-quote
;;; -------------------------------------------------------------------------

(deftest swb-env-prefix/quotes-all-paths
  ;; A path with an apostrophe is the canonical hostile case; the helper
  ;; must escape it via '\'' so the resulting POSIX command is well-formed.
  (let* ((config (cupertino::make-swb-runner-config
                  :real-service "/opt/O'Brien/SWBBuildService"
                  :self-exe "/opt/cup'tino/bin/cupertino"
                  :trace nil))
         (events "/tmp/foo'bar.events")
         (prefix (cupertino::swb-env-prefix config events)))
    (ok (search "'\\''" prefix))
    ;; All three single-quoted values appear, each carrying their escape.
    (ok (search "/opt/O'\\''Brien/SWBBuildService" prefix))
    (ok (search "/opt/cup'\\''tino/bin/cupertino" prefix))
    (ok (search "/tmp/foo'\\''bar.events" prefix))
    ;; No trace flag in this config -> CUPERTINO_SWB_TRACE absent.
    (ok (not (search "CUPERTINO_SWB_TRACE" prefix)))))

(deftest swb-env-prefix/trace-flag-when-on
  (let* ((config (cupertino::make-swb-runner-config
                  :real-service "/a" :self-exe "/b" :trace t))
         (prefix (cupertino::swb-env-prefix config "/c")))
    (ok (search "CUPERTINO_SWB_TRACE=1" prefix))))

(deftest pty-wrap-command/prepends-script-q-devnull
  ;; The wrapper must put script(1) in front of the command so xcodebuild's
  ;; stdout sees a pty and line-buffers; -q suppresses the startup banner and
  ;; /dev/null discards the typescript (our drain reads from the process's
  ;; own stdout, which script forwards from the pty).
  (let ((wrapped (cupertino::pty-wrap-command "env A=B xcodebuild -scheme 'Foo'")))
    (ok (string= "script -q /dev/null env A=B xcodebuild -scheme 'Foo'" wrapped))
    ;; Idempotency isn't a contract -- but the wrapper must not mutate the
    ;; tail in any way the caller didn't ask for, so the input substring
    ;; survives intact at the end of the wrapped string.
    (ok (search "env A=B xcodebuild -scheme 'Foo'" wrapped))))

;;; -------------------------------------------------------------------------
;;; Per-cell trace-file resolution
;;; -------------------------------------------------------------------------

(deftest swb-resolve-events-file/no-trace-file-yields-tempfile
  (let ((config (cupertino::make-swb-runner-config
                 :real-service "/x" :self-exe "/y" :trace nil :trace-file nil)))
    (multiple-value-bind (path persistent-p)
        (cupertino::swb-resolve-events-file config "MyScheme-iPhone16")
      (ok (null persistent-p))
      (ok (pathnamep path)))))

(deftest swb-resolve-events-file/single-cell-uses-base-path
  (let ((config (cupertino::make-swb-runner-config
                 :real-service "/x" :self-exe "/y" :trace t
                 :trace-file "/tmp/build.txt")))
    (multiple-value-bind (path persistent-p)
        (cupertino::swb-resolve-events-file config nil)
      (ok (not (null persistent-p)))
      (ok (equal (namestring (pathname "/tmp/build.txt"))
                 (namestring path))))))

(deftest swb-resolve-events-file/parallel-suffixes-per-cell
  ;; --swb-trace-file /tmp/build.txt with cell label "MyScheme iPhone:16"
  ;; -> /tmp/build-MyScheme-iPhone-16.txt (label sanitized).
  (let ((config (cupertino::make-swb-runner-config
                 :real-service "/x" :self-exe "/y" :trace t
                 :trace-file "/tmp/build.txt")))
    (multiple-value-bind (path persistent-p)
        (cupertino::swb-resolve-events-file config "MyScheme iPhone:16")
      (ok (not (null persistent-p)))
      (let ((name (pathname-name path)))
        (ok (search "build-" name))
        ;; Spaces and colons sanitized out of the cell label.
        (ok (null (search " " name)))
        (ok (null (search ":" name)))
        (ok (equal "txt" (pathname-type path)))))))


;;; -------------------------------------------------------------------------
;;; SWB diagnostic location extraction (#hyperlinked-paths follow-up)
;;;
;;; BUILD_DIAGNOSTIC_EMITTED carries a Swift-encoded `location' enum whose
;;; JSON shape is one of {unknown:{}} or {path:{_0:<file>,fileLocation:...}}.
;;; The dashboard's SWB-mode scrollback used to drop the location entirely;
;;; these tests pin down the new behavior where the file (and line/column
;;; when textual) are prepended to the message so it reads like the text-mode
;;; `file.swift:42:10: warning: foo' form.
;;; -------------------------------------------------------------------------

(defun %swb-diag-hash (&key kind message location)
  "Build a fake BUILD_DIAGNOSTIC_EMITTED payload hash for the location tests."
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "kind" h) kind
          (gethash "message" h) message)
    (when location (setf (gethash "location" h) location))
    h))

(defun %swb-loc-path-textual (path line column)
  "Build a `{path:{_0:PATH,fileLocation:{textual:{line:L,column:C}}}}' hash."
  (let ((loc (make-hash-table :test 'equal))
        (path-cell (make-hash-table :test 'equal))
        (file-loc (make-hash-table :test 'equal))
        (textual (make-hash-table :test 'equal)))
    (when line (setf (gethash "line" textual) line))
    (when column (setf (gethash "column" textual) column))
    (setf (gethash "textual" file-loc) textual
          (gethash "_0" path-cell) path
          (gethash "fileLocation" path-cell) file-loc
          (gethash "path" loc) path-cell)
    loc))

(deftest swb-diag-location/unknown-yields-nils
  (let ((h (%swb-diag-hash :kind 1 :message "x"
                           :location (let ((u (make-hash-table :test 'equal)))
                                       (setf (gethash "unknown" u)
                                             (make-hash-table :test 'equal))
                                       u))))
    (multiple-value-bind (p l c) (cupertino::swb-diag-location h)
      (ok (null p)) (ok (null l)) (ok (null c)))))

(deftest swb-diag-location/path-with-line-and-column
  (let ((h (%swb-diag-hash
            :kind 1 :message "unused"
            :location (%swb-loc-path-textual "/tmp/Foo.swift" 42 10))))
    (multiple-value-bind (p l c) (cupertino::swb-diag-location h)
      (ok (string= "/tmp/Foo.swift" p))
      (ok (eql 42 l))
      (ok (eql 10 c)))))

(deftest swb-diag-location/path-without-file-location
  ;; fileLocation absent → path only.
  (let* ((path-cell (make-hash-table :test 'equal))
         (loc (make-hash-table :test 'equal)))
    (setf (gethash "_0" path-cell) "/tmp/Foo.swift"
          (gethash "path" loc) path-cell)
    (multiple-value-bind (p l c)
        (cupertino::swb-diag-location (%swb-diag-hash :kind 1 :message "m"
                                                      :location loc))
      (ok (string= "/tmp/Foo.swift" p))
      (ok (null l))
      (ok (null c)))))

(deftest swb-diag-location-prefix/nil-path-yields-nil
  (ok (null (cupertino::swb-diag-location-prefix nil nil nil))))

(deftest swb-diag-location-prefix/path-line-column
  (ok (string= "/tmp/Foo.swift:42:10: "
               (cupertino::swb-diag-location-prefix "/tmp/Foo.swift" 42 10))))

(deftest swb-diag-location-prefix/path-line-only
  (ok (string= "/tmp/Foo.swift:42: "
               (cupertino::swb-diag-location-prefix "/tmp/Foo.swift" 42 nil))))

(deftest swb-diag-location-prefix/path-only
  (ok (string= "/tmp/Foo.swift: "
               (cupertino::swb-diag-location-prefix "/tmp/Foo.swift" nil nil))))

(defun %swb-diag-arg (hash)
  "Round-trip HASH through yason → bytes so xcbuild-swb-diagnostic's
swb-arg-json can decode it (it expects a non-string byte vector)."
  (let ((json (with-output-to-string (out) (yason:encode hash out))))
    (map '(simple-array (unsigned-byte 8) (*)) #'char-code json)))

(deftest xcbuild-swb-diagnostic/warning-without-location-counts-but-no-prefix
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (cupertino::xcbuild-swb-diagnostic
     nil stats
     (%swb-diag-arg (%swb-diag-hash :kind 1 :message "unused variable")))
    (ok (= 1 (cupertino::xcbuild-stats-warnings stats)))))

(deftest xcbuild-swb-diagnostic/error-with-location-bumps-count
  ;; End-to-end through swb-arg-json + swb-diag-location; the visible side
  ;; effect we can assert without a console is the error counter.
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (cupertino::xcbuild-swb-diagnostic
     nil stats
     (%swb-diag-arg
      (%swb-diag-hash :kind 2 :message "type mismatch"
                      :location (%swb-loc-path-textual "/p/F.swift" 7 3))))
    (ok (= 1 (cupertino::xcbuild-stats-errors stats)))))

(deftest xcbuild-swb-diagnostic/same-message-different-locations-not-deduped
  ;; Two warnings with the same text but different line numbers must count
  ;; as two — the dedup key now includes the location prefix.
  (let ((stats (cupertino::make-xcbuild-stats :action "build")))
    (cupertino::xcbuild-swb-diagnostic
     nil stats
     (%swb-diag-arg
      (%swb-diag-hash :kind 1 :message "unused"
                      :location (%swb-loc-path-textual "/p/F.swift" 1 1))))
    (cupertino::xcbuild-swb-diagnostic
     nil stats
     (%swb-diag-arg
      (%swb-diag-hash :kind 1 :message "unused"
                      :location (%swb-loc-path-textual "/p/F.swift" 9 1))))
    (ok (= 2 (cupertino::xcbuild-stats-warnings stats)))))


;;; -------------------------------------------------------------------------
;;; Combined end-of-build banner
;;;
;;; xcodebuild emits `** BUILD SUCCEEDED **' / `** BUILD FAILED **' to stdout
;;; at the end of every run; SWB mode drains and discards that line. The
;;; helper here re-emits a styled verdict just above the finalized dashboard
;;; so the user gets one consistent end-of-run line across single-cell text,
;;; single-cell SWB, and the parallel matrix runner.
;;; -------------------------------------------------------------------------

(deftest combined-banner-text/build-succeeded
  (ok (string= "** BUILD SUCCEEDED **"
               (cupertino::xcbuild-combined-banner-text "build" t))))

(deftest combined-banner-text/build-failed
  (ok (string= "** BUILD FAILED **"
               (cupertino::xcbuild-combined-banner-text "build" nil))))

(deftest combined-banner-text/uppercases-action
  ;; xcodebuild's banner verb is always uppercased; mirror that for `test',
  ;; `clean', and `archive' actions.
  (ok (string= "** TEST SUCCEEDED **"
               (cupertino::xcbuild-combined-banner-text "test" t)))
  (ok (string= "** CLEAN FAILED **"
               (cupertino::xcbuild-combined-banner-text "clean" nil)))
  (ok (string= "** ARCHIVE SUCCEEDED **"
               (cupertino::xcbuild-combined-banner-text "archive" t))))

(deftest print-build-banner/success-line
  ;; Bold-green banner with the standard '** BUILD SUCCEEDED **' verdict.
  ;; Asserts both the verdict text and the bold+green ANSI envelope so a
  ;; regression in either is caught.
  (let ((out (with-output-to-string (s)
               (cupertino::print-build-banner "build" t s))))
    (ok (search "** BUILD SUCCEEDED **" out))
    (ok (search (format nil "~C[1;32m" #\Escape) out))
    (ok (search (format nil "~C[0m" #\Escape) out))))

(deftest print-build-banner/failure-line
  ;; Bold-red banner for the failure branch.
  (let ((out (with-output-to-string (s)
               (cupertino::print-build-banner "test" nil s))))
    (ok (search "** TEST FAILED **" out))
    (ok (search (format nil "~C[1;31m" #\Escape) out))))
