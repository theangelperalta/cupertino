(in-package :cupertino)

;; Defined later in xcbuild-parallel.lisp (loaded after this file); declared here
;; so RUN-XCODEBUILD's parallel branch compiles without a forward-reference warning.
(declaim (ftype function run-xcodebuild/parallel))

(defun runtime-id-platform (runtime-id)
  "Extract the platform name (e.g. iOS, tvOS, watchOS, xrOS) from a simctl
SimRuntime identifier like \"com.apple.CoreSimulator.SimRuntime.iOS-17-0\"."
  (let ((p (search "SimRuntime." runtime-id)))
    (when p
      (let* ((rest (subseq runtime-id (+ p (length "SimRuntime."))))
             (dash (position #\- rest)))
        (if dash (subseq rest 0 dash) rest)))))

(defvar *sim-info-cache* :unbound
  "When bound to a parsed simctl JSON hash (or NIL), helpers reuse it instead
of shelling out per call. RESOLVE-CELLS binds this around the matrix so the
panel-label lookups and SIM-RUNTIME-PLATFORM calls share one simctl read.")

(defun current-sim-info ()
  "Return the cached parsed simctl info if RESOLVE-CELLS has populated it,
otherwise fetch fresh. NIL on failure."
  (if (eq *sim-info-cache* :unbound)
      (ignore-errors (list-sim-info))
      *sim-info-cache*))

(defun sim-runtime-platform (udid)
  "Look up the platform name for simulator UDID via `xcrun simctl list --json',
or NIL if it can't be found."
  (let ((info (current-sim-info)))
    (when info
      (let ((devices (gethash "devices" info)))
        (when devices
          (block found
            (maphash (lambda (runtime device-list)
                       (dolist (d device-list)
                         (when (equal (gethash "udid" d) udid)
                           (return-from found (runtime-id-platform runtime)))))
                     devices)
            nil))))))

(defun sim-device-and-runtime-name (udid)
  "Return (values DEVICE-NAME RUNTIME-NAME) for simulator UDID, e.g.
(values \"iPhone 17 Pro\" \"iOS 26.5\"). Either may be NIL when unknown; both
are NIL on lookup failure (UDID not found, simctl unavailable, etc.)."
  (handler-case
      (let ((info (current-sim-info)))
        (when info
          (let ((devices (gethash "devices" info)))
            (when devices
              (block found
                (maphash
                 (lambda (runtime-id device-list)
                   (dolist (d device-list)
                     (when (equal (gethash "udid" d) udid)
                       (return-from found
                         (values (gethash "name" d)
                                 (sim-runtime-name info runtime-id))))))
                 devices)
                (values nil nil))))))
    (error () (values nil nil))))

(defun sim-destination (udid)
  "Full xcodebuild -destination string for a simulator UDID, with the platform
auto-detected via simctl. Falls back to iOS Simulator when undetectable."
  (let* ((plat (or (sim-runtime-platform udid) "iOS"))
         (sim-platform (cond ((string-equal plat "xrOS") "visionOS Simulator")
                             (t (format nil "~A Simulator" plat)))))
    (format nil "platform=~A,id=~A" sim-platform udid)))

(defun resolve-destination (cmd model)
  "Resolve the xcodebuild destination from CLI options or model."
  (let ((sim (clingon:getopt cmd :sim))
        (device (clingon:getopt cmd :device)))
    (cond
      (sim (sim-destination sim))
      (device "generic/platform=iOS")
      ((model-sim model) (sim-destination (model-sim model)))
      ((model-device model) "generic/platform=iOS")
      (t nil))))

(defun shell-quote-single (s)
  "Wrap S in POSIX single quotes, escaping any embedded single quotes via the
``'\\''' close-reopen idiom. Lets xcodebuild command strings carry paths or
names containing spaces or single quotes (e.g. an `O'Brien' home dir) without
the shell mis-tokenizing them. Note: PROJECT-FLAG is still interpolated raw
because it carries its own quoting -- safer once that is built via this helper."
  (with-output-to-string (out)
    (write-char #\' out)
    (loop for c across s
          do (if (char= c #\')
                 (write-string "'\\''" out)
                 (write-char c out)))
    (write-char #\' out)))

(defun xcodebuild-command-string (project-flag scheme destination configuration
                                  derived-data-path action
                                  &optional result-bundle-path extra-args)
  "Build a single xcodebuild command string for one scheme/destination cell.
When RESULT-BUNDLE-PATH is given, add -resultBundlePath so the .xcresult lands
at a known location cupertino can report at the end of the run. EXTRA-ARGS, if
non-empty, is appended after the action verb (e.g. test-action filters like
-only-testing:...). User-supplied strings are run through SHELL-QUOTE-SINGLE so
embedded spaces or quotes are safe."
  (format nil "xcodebuild ~A -scheme ~A -destination ~A -configuration ~A~@[ -derivedDataPath ~A~]~@[ -resultBundlePath ~A~] ~A~@[ ~A~]"
          project-flag
          (shell-quote-single scheme)
          (shell-quote-single destination)
          configuration
          (when derived-data-path (shell-quote-single derived-data-path))
          (when result-bundle-path (shell-quote-single result-bundle-path))
          action
          (when (and extra-args (plusp (length extra-args))) extra-args)))

;;; ---------------------------------------------------------------------------
;;; Per-cell DerivedData isolation
;;; ---------------------------------------------------------------------------
;;;
;;; Parallel matrix cells (multiple scheme/destination xcodebuild processes)
;;; would otherwise share the project's default DerivedData and collide on the
;;; SQLite lock for XCBuildData/build.db with "database is locked" errors.
;;; CELL-DERIVED-DATA-DIR maps each cell to a *stable* subdirectory of a base
;;; directory, so the same (scheme, destination) always lands in the same place
;;; -- preserving incremental builds across runs while isolating concurrent
;;; cells from each other.

(defun sanitize-path-component (s)
  "Fold S to a filesystem-safe path component. Alphanumerics, `-', `_', and
`.' pass through unchanged; everything else (spaces, `/', `:', `=', `,', etc.)
becomes `-'. Length is preserved so the mapping is human-readable."
  (let ((out (make-string (length s))))
    (dotimes (i (length s) out)
      (let ((c (char s i)))
        (setf (char out i)
              (if (or (alphanumericp c) (member c '(#\- #\_ #\.) :test #'char=))
                  c
                  #\-))))))

(defun default-isolated-derived-data-base ()
  "Auto-base under which per-cell DerivedData dirs live when the user didn't
supply --derived-data. Kept out of the project's default DerivedData so it
doesn't clash with Xcode.app's own incremental cache."
  (namestring (merge-pathnames "Library/Developer/Xcode/DerivedData-cupertino/"
                               (user-homedir-pathname))))

(defun ensure-trailing-slash (s)
  "Return S with exactly one trailing `/'. Empty string passes through unchanged."
  (cond ((zerop (length s)) s)
        ((char= (char s (1- (length s))) #\/) s)
        (t (concatenate 'string s "/"))))

(defun cell-leaf (scheme destination)
  "Filesystem-safe identifier for one (SCHEME, DESTINATION) cell, used as a
leaf name in both per-cell DerivedData dirs and the fallback .xcresult path so
both stay in lockstep on the sanitization rules."
  (concatenate 'string
               (sanitize-path-component scheme) "-"
               (sanitize-path-component destination)))

(defun cell-derived-data-dir (base scheme destination)
  "Stable per-cell DerivedData directory: BASE/<scheme>-<destination>, each
component sanitized for filesystem safety. The same (SCHEME, DESTINATION) pair
deterministically maps to the same subdirectory across runs."
  (concatenate 'string (ensure-trailing-slash base) (cell-leaf scheme destination)))

(defun cell-result-bundle-path (effective-derived-data scheme destination)
  "Deterministic .xcresult bundle path for the (SCHEME, DESTINATION) cell.
Co-located with the cell's effective DerivedData (so the bundle lives next to
the build artifacts that produced it, and `--derived-data' is honoured), or in
a `results/' subdir of the cupertino auto-base when there is no DerivedData
(single cell, no --derived-data). Concurrent cells never collide because the
per-cell DerivedData is already isolated by scheme/destination upstream."
  (if (and effective-derived-data (plusp (length effective-derived-data)))
      (concatenate 'string
                   (ensure-trailing-slash effective-derived-data)
                   "cupertino.xcresult")
      (concatenate 'string
                   (default-isolated-derived-data-base) "results/"
                   (cell-leaf scheme destination) ".xcresult")))

(defun resolve-destinations (cmd model)
  "List of xcodebuild -destination strings from repeatable --sim/--device, else
the model's single destination. Devices are id-specific here (matrix), unlike the
single-path generic device destination."
  (let ((sims (let ((s (clingon:getopt cmd :sim)))    (if (consp s) s (when s (list s)))))
        (devs (let ((d (clingon:getopt cmd :device))) (if (consp d) d (when d (list d))))))
    (append
     (mapcar #'sim-destination sims)
     (mapcar (lambda (u) (format nil "platform=iOS,id=~A" u)) devs)
     (when (and (null sims) (null devs))
       (let ((d (resolve-destination cmd model))) (when d (list d)))))))

(defun destination-label (dest)
  "Short label for a -destination string used in panel headers. Simulator
destinations are enriched with the device model and runtime name (e.g.
\"sim:UDID (iPhone 17 Pro, iOS 26.5)\") when the simctl lookup succeeds;
otherwise the bare \"sim:UDID\" form is preserved."
  (let* ((p (search "id=" dest))
         (udid (when p (subseq dest (+ p 3)))))
    (cond
      ((search "Simulator" dest)
       (let ((base (format nil "sim:~A" (or udid dest))))
         (if udid
             (multiple-value-bind (model runtime) (sim-device-and-runtime-name udid)
               (cond ((and model runtime) (format nil "~A (~A, ~A)" base model runtime))
                     (model               (format nil "~A (~A)" base model))
                     (t                   base)))
             base)))
      ((search "platform=iOS" dest) (format nil "dev:~A" (or udid "device")))
      (t dest))))

(defun parse-cell-destination (spec)
  "Convert a --cell destination SPEC into a full xcodebuild -destination string.
Accepts `sim=UDID', `device=UDID', a raw `platform=...' string, or a bare UDID
(treated as a simulator). Simulator platforms are auto-detected via simctl."
  (cond
    ((str:starts-with-p "sim=" spec)      (sim-destination (subseq spec 4)))
    ((str:starts-with-p "device=" spec)   (format nil "platform=iOS,id=~A" (subseq spec 7)))
    ((str:starts-with-p "platform=" spec) spec)
    (t (sim-destination spec))))

(defun parse-cell-option (spec)
  "Parse a --cell SPEC of the form `scheme@destination' into (values scheme dest).
Returns NIL when SPEC has no `@' separator."
  (let ((at (position #\@ spec)))
    (when at
      (values (subseq spec 0 at)
              (parse-cell-destination (subseq spec (1+ at)))))))

(defun resolve-project-flag (cmd model)
  "Resolve the -workspace/-project flag from CLI path or model."
  (let* ((path (first (clingon:command-arguments cmd)))
         (project-type (or (when path (get-project-file-type path))
                           (model-project-type model)))
         (project-path (or path (model-project-path model))))
    (cond
      ((and (eq project-type :workspace) project-path)
       (format nil "-workspace '~A'" project-path))
      ((and (eq project-type :project) project-path)
       (format nil "-project '~A'" project-path))
      (t ""))))

(defun build-cell (project-flag scheme destination configuration derived-data-path
                   action labeled &key extra-args)
  "Construct one matrix-cell plist for SCHEME on DESTINATION. When LABELED, the
panel label includes the destination; otherwise it is just the scheme. For the
test action the cell carries an :xcresult path (also wired into the command as
-resultBundlePath) so the produced .xcresult can be reported at the end.
EXTRA-ARGS is forwarded to XCODEBUILD-COMMAND-STRING (e.g. -only-testing
filters)."
  (let ((xcresult (when (string= action "test")
                    (cell-result-bundle-path derived-data-path scheme destination))))
    (list :scheme scheme :dest destination
          :label (if labeled
                     (format nil "~A @ ~A" scheme (destination-label destination))
                     scheme)
          :xcresult xcresult
          :cmd (xcodebuild-command-string
                project-flag scheme destination configuration
                derived-data-path action xcresult extra-args))))

(defun resolve-cells (cmd action project-flag configuration derived-data-path
                      model scheme-accessors cell-accessors &key extra-args)
  "Resolve the list of matrix cells to run. Cells come from repeatable --cell
options, else from config (:cells/:test-cells via CELL-ACCESSORS); each is an
explicit scheme@destination pairing (no Cartesian product). With no cells, the
scheme list x destination list Cartesian product is used. Exits via
print-usage-and-exit when nothing resolves.

When more than one cell is resolved, each cell gets a stable per-cell
DerivedData directory under DERIVED-DATA-PATH (or under an auto-base when
none was supplied) so concurrent xcodebuild processes don't collide on the
shared XCBuildData/build.db SQLite lock."
  (let* ((*sim-info-cache* (ignore-errors (list-sim-info)))
         (cell-specs (or (alexandria:ensure-list (clingon:getopt cmd :cell))
                         (some (lambda (fn)
                                 (alexandria:ensure-list (funcall fn model)))
                               cell-accessors)))
         (raw-cells
           (if cell-specs
               (loop for spec in cell-specs collect
                     (multiple-value-bind (sch dest) (parse-cell-option spec)
                       (unless (and sch dest)
                         (cup-error "Invalid cell \"~A\"; expected scheme@destination." spec)
                         (clingon:print-usage-and-exit cmd t))
                       (list :scheme sch :dest dest :labeled t)))
               (let* ((destinations (resolve-destinations cmd model))
                      ;; ENSURE-LIST wraps strings too, so --scheme as a single
                      ;; string and as a repeatable list both normalise here;
                      ;; OR falls through to config accessors when the CLI flag
                      ;; was absent (ENSURE-LIST of NIL returns NIL).
                      (schemes (or (alexandria:ensure-list
                                    (clingon:getopt cmd :scheme))
                                   (some (lambda (fn)
                                           (alexandria:ensure-list
                                            (funcall fn model)))
                                         scheme-accessors)))
                      (multiple-dests (> (length destinations) 1)))
                 (unless schemes
                   (cup-error "No scheme specified and no default scheme configured.")
                   (clingon:print-usage-and-exit cmd t))
                 (unless destinations
                   (cup-error "No simulator or device specified and none configured.")
                   (clingon:print-usage-and-exit cmd t))
                 (loop for sch in schemes nconc
                       (loop for dest in destinations collect
                             (list :scheme sch :dest dest :labeled multiple-dests))))))
         (multi (> (length raw-cells) 1))
         (base-dir (when multi
                     (or derived-data-path (default-isolated-derived-data-base)))))
    (when multi
      (cup-note "per-cell DerivedData under ~A (each cell isolated)" base-dir))
    (loop for rc in raw-cells collect
          (let ((sch (getf rc :scheme))
                (dest (getf rc :dest))
                (labeled (getf rc :labeled)))
            (build-cell project-flag sch dest configuration
                        (if multi
                            (cell-derived-data-dir base-dir sch dest)
                            derived-data-path)
                        action labeled :extra-args extra-args)))))

(defun no-cells-resolvable-p (cmd model scheme-accessors cell-accessors)
  "True when CMD/MODEL supply nothing that RESOLVE-CELLS could turn into a cell:
no --cell, no config cells (via CELL-ACCESSORS), and no scheme (via --scheme or
SCHEME-ACCESSORS). Used so `clean --prune' can run standalone instead of hitting
RESOLVE-CELLS' usage-and-exit when there's no target to clean."
  (flet ((accessor-empty-p (accessors)
           ;; Any non-NIL return is "non-empty" -- both `("foo")' and `"foo"' are
           ;; truthy, NIL/() are not, which is exactly the NOTANY predicate we want.
           (notany (lambda (fn) (funcall fn model)) accessors)))
    (let ((cells (clingon:getopt cmd :cell))
          (scheme (clingon:getopt cmd :scheme)))
      (and (null cells)
           (accessor-empty-p cell-accessors)
           (null scheme)
           (accessor-empty-p scheme-accessors)))))

(defun reset-result-bundle (path)
  "Remove any stale .xcresult bundle at PATH and ensure its parent directory
exists. xcodebuild's -resultBundlePath refuses to overwrite an existing path,
so a deterministic per-cell path must be cleared before each run. NIL-safe.
A delete failure (e.g. permission denied) is logged but not raised, so the run
proceeds and xcodebuild's own clear pre-existing-path error surfaces instead of
a Lisp backtrace."
  (when path
    (let ((dir (uiop:ensure-directory-pathname path)))
      (when (uiop:directory-exists-p dir)
        (handler-case
            (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)
          (error (e)
            (cup-warn "could not remove stale ~A: ~A" path e)))))
    (ignore-errors
     (ensure-directories-exist (uiop:pathname-directory-pathname path)))))

(defun report-result-bundles (cells)
  "Print the .xcresult bundle path for each cell that has one, padding labels so
the paths align in matrix runs. Bundles that didn't materialize on disk (a cell
whose run failed before producing one) are flagged `(not produced)' rather than
silently dropped, so the absence is unambiguous. No-op when no cell carries an
:xcresult (non-test actions)."
  (let ((rows (loop for c in cells
                    for path = (getf c :xcresult)
                    when path
                      collect (list (or (getf c :label) "")
                                    path
                                    (uiop:directory-exists-p
                                     (uiop:ensure-directory-pathname path))))))
    (when rows
      (format t "~A (open with `open <path>')~%"
              (colored-text "Result bundles:" :cyan))
      (let ((width (reduce #'max rows :key (lambda (r) (length (first r))))))
        (dolist (r rows)
          (destructuring-bind (label path exists) r
            (format t "  ~vA  ~A~%" width label
                    (if exists path "(not produced)"))))))))

(defun prune-isolated-derived-data ()
  "Delete the cupertino-managed auto-base DerivedData tree
(~/Library/Developer/Xcode/DerivedData-cupertino/) where parallel matrix cells
accumulate per-cell DerivedData outside Xcode's GUI cleaner. No-op (with a note)
when the directory doesn't exist. Returns T when something was removed."
  (let ((base (default-isolated-derived-data-base)))
    (if (uiop:directory-exists-p base)
        (progn
          (uiop:delete-directory-tree (uiop:ensure-directory-pathname base)
                                      :validate t :if-does-not-exist :ignore)
          (format t "~A removed per-cell DerivedData tree at ~A~%"
                  (colored-text "Pruned:" :green) base)
          t)
        (progn
          (cup-note "no per-cell DerivedData tree at ~A" base)
          nil))))

(defun run-xcodebuild (cmd action &key (scheme-accessors (list #'model-scheme))
                                       cell-accessors)
  "Run xcodebuild with the given action (build, test, clean, etc.).
Builds a list of scheme/destination cells -- explicit --cell pairings, config
:cells/:test-cells (via CELL-ACCESSORS), or the scheme x destination Cartesian
product -- and runs them. A single cell uses the existing SWB/pretty/raw path;
multiple cells run concurrently under a combined dashboard via
run-xcodebuild/parallel. With a single cell, --derived-data is forwarded
verbatim as -derivedDataPath; with multiple cells, each cell gets its own
stable -derivedDataPath under --derived-data (or under an auto-base) to
isolate concurrent xcodebuild processes from each other.

For the clean action, --prune additionally removes the cupertino auto-base
DerivedData tree; with --prune and no scheme/destination resolvable, pruning
runs standalone (no xcodebuild clean)."
  (let* ((path (first (clingon:command-arguments cmd)))
         (model (model:make-cupertino-model path))
         (project-flag (resolve-project-flag cmd model))
         (configuration (clingon:getopt cmd :configuration))
         (derived-data-path (clingon:getopt cmd :derived-data))
         (prune (and (string= action "clean") (clingon:getopt cmd :prune)))
         (extra-args (when (string= action "test")
                       (build-test-filter-args
                        (alexandria:ensure-list (clingon:getopt cmd :only-testing))
                        (alexandria:ensure-list (clingon:getopt cmd :skip-testing))))))
    (when prune (prune-isolated-derived-data))
    (let ((cells (if (and prune (no-cells-resolvable-p cmd model
                                                       scheme-accessors
                                                       cell-accessors))
                     ;; --prune with nothing to clean: pruning was the whole job.
                     (return-from run-xcodebuild)
                     (resolve-cells cmd action project-flag configuration
                                    derived-data-path model scheme-accessors
                                    cell-accessors :extra-args extra-args))))
    (let* ((*xcbuild-max-jobs* (or (model-max-jobs model) *xcbuild-max-jobs*))
           (*xcbuild-slow-threshold* (or (model-slow-threshold model)
                                         *xcbuild-slow-threshold*))
           ;; Cache-hit counts and the message-name trace come only from SWB
           ;; protocol events, so asking for either implies interception mode.
           (want-swb (or (clingon:getopt cmd :use-swb) (model-use-swb model)
                         (clingon:getopt cmd :cache-hits) (model-cache-hits model)
                         (clingon:getopt cmd :swb-trace)
                         (clingon:getopt cmd :swb-trace-file)))
           (swb-trace (or (clingon:getopt cmd :swb-trace)
                          (clingon:getopt cmd :swb-trace-file)))
           ;; In matrix mode each cell gets its own per-cell-suffixed trace file
           ;; (see SWB-RESOLVE-EVENTS-FILE) so the user-supplied path is the base.
           (swb-trace-file (clingon:getopt cmd :swb-trace-file))
           ;; Cache hits show by default whenever SWB is active. An explicit
           ;; --cache-hits forces them on; an explicit :cache-hits in the
           ;; config (true or false) overrides the default either way.
           (*xcbuild-show-cache-hits*
             (cond ((clingon:getopt cmd :cache-hits) t)
                   ((member :cache-hits (model:load-model path)) (model-cache-hits model))
                   (t want-swb))))
      (dolist (c cells) (reset-result-bundle (getf c :xcresult)))
      ;; Show every command we're about to run, in both single and parallel
      ;; paths, before the dispatch bifurcation.
      (dolist (c cells)
        (format t "~A ~A~%" (colored-text "Running:" :cyan) (getf c :cmd)))
      (multiple-value-bind (real self swb-ok) (detect-swb-capability want-swb)
        (let ((config (when swb-ok
                        (make-swb-runner-config :real-service real :self-exe self
                                                :trace swb-trace
                                                :trace-file swb-trace-file))))
          (if (= (length cells) 1)
              ;; --- single cell: SWB / pretty / raw path ---
              (let* ((cmd-str (getf (first cells) :cmd))
                     (console (sc:make-superconsole))
                     (exit-code (cond
                                  ((and console swb-ok)
                                   (run-xcodebuild/swb cmd-str action console config))
                                  (console
                                   (when want-swb
                                     (cup-warn "Swift Build interception unavailable; using text mode."))
                                   (run-xcodebuild/pretty cmd-str action console))
                                  (t (run-xcodebuild/raw cmd-str)))))
                (report-result-bundles cells)
                ;; In raw (non-TTY) mode xcodebuild emits its own banner;
                ;; only stamp the verdict here on the TTY paths where SWB
                ;; drains stdout (or pretty mode mid-scrolls it).
                (when console
                  (print-build-banner action (zerop exit-code)))
                (unless (zerop exit-code)
                  (cup-error "xcodebuild ~A failed with exit code ~A." action exit-code)
                  (uiop:quit exit-code)))
              ;; --- parallel matrix: SWB capability already detected, shared with workers ---
              (progn
                (when (and want-swb (not swb-ok))
                  (cup-warn "Swift Build interception unavailable; using text mode."))
                (let ((exit-code
                        (run-xcodebuild/parallel
                         cells action
                         :jobs (or (clingon:getopt cmd :jobs) (length cells))
                         :fail-fast (clingon:getopt cmd :fail-fast)
                         :swb config)))
                  (report-result-bundles cells)
                  (print-build-banner action (zerop exit-code))
                  (unless (zerop exit-code)
                    (cup-error "xcodebuild ~A failed for ~D cell(s)." action exit-code)
                    (uiop:quit exit-code)))))))))))

(defun run-xcodebuild/raw (cmd-str)
  "Run CMD-STR with raw interactive I/O (the non-TTY fallback). Return the exit code."
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program cmd-str :output :interactive :error-output :interactive
                                :ignore-error-status t)
    (declare (ignore output error-output))
    exit-code))

;;; ---------------------------------------------------------------------------
;;; Swift Build (SWBBuildService) interception mode
;;; ---------------------------------------------------------------------------

(defun resolve-swb-service ()
  "Absolute path to the active Xcode's real SWBBuildService executable, or NIL."
  (let* ((dev (ignore-errors
                (string-trim '(#\Newline #\Space)
                             (uiop:run-program '("xcode-select" "-p")
                                               :output :string))))
         (svc (and dev (plusp (length dev))
                   (format nil "~A/../SharedFrameworks/SwiftBuild.framework/Versions/A/PlugIns/SWBBuildService.bundle/Contents/MacOS/SWBBuildService"
                           dev))))
    (when (and svc (probe-file svc))
      (namestring (truename svc)))))

(defun swb-self-executable ()
  "Path to the running Cupertino image, used as the SWBBUILDSERVICE_PATH proxy."
  (ignore-errors (namestring sb-ext:*runtime-pathname*)))

(defun swb-usable-p (self)
  "True when SELF is our dumped image (a Mach-O the build service can spawn),
not a bare Lisp runtime where proxy mode wouldn't dispatch to us."
  (and self
       (not (member (pathname-name (pathname self))
                    '("sbcl" "ros" "lisp" "ccl") :test #'equal))))

(defun detect-swb-capability (want-swb)
  "Probe Swift Build interception capability once. Returns (values REAL SELF
USABLE-P): REAL is the active Xcode's SWBBuildService path (or NIL), SELF is
the running cupertino image (or NIL), and USABLE-P is T iff all three checks
pass and a proxy can actually be spawned. When WANT-SWB is NIL, returns
(values nil nil nil) without probing."
  (let* ((real (and want-swb (resolve-swb-service)))
         (self (and real (swb-self-executable))))
    (values real self (and real self (swb-usable-p self)))))

(defun swb-events-tempfile ()
  "A fresh temp path the proxy appends decoded events to."
  (merge-pathnames (format nil "cupertino-swb-~36R-~36R.events"
                           (get-universal-time) (random 1000000))
                   (uiop:temporary-directory)))

(defun swb-trace-file-for-cell (base-path cell-label)
  "Derive a per-cell trace-file path from BASE-PATH by inserting a sanitized
CELL-LABEL before the file's name-extension boundary, e.g.
/tmp/build.txt + \"MyScheme-iPhone16\" -> /tmp/build-MyScheme-iPhone16.txt.
Lets parallel matrix runs honor --swb-trace-file without racing on one file."
  (let* ((path (pathname base-path))
         (name (or (pathname-name path) ""))
         (type (pathname-type path))
         (tag  (sanitize-path-component cell-label)))
    (make-pathname :defaults path
                   :name (if (plusp (length name))
                             (concatenate 'string name "-" tag)
                             tag)
                   :type type)))

;;; Runner-side configuration for the SWB pipeline. Replaces the ad-hoc
;;; plist that previously plumbed (:real :self [:trace]) through the parallel
;;; runner; making it a struct catches misnamed slot accesses at compile time
;;; and gives us a single home for new knobs (trace, trace-file, ...).
(defstruct swb-runner-config
  real-service                          ; absolute path to the real SWBBuildService
  self-exe                              ; absolute path to the cupertino image (proxy)
  (trace nil)                           ; T -> emit :UNKNOWN events for new wire names
  (trace-file nil))                     ; user-supplied persistent events path, or NIL

(defun swb-resolve-events-file (config &optional cell-label)
  "Return (values EVENTS-FILE PERSISTENT-P) for one SWB run. PERSISTENT-P is T
when the caller asked for --swb-trace-file (so the file must survive cleanup);
otherwise a fresh tempfile is returned and the caller deletes it on exit. When
CELL-LABEL is non-NIL and CONFIG specifies a trace-file, the per-cell path is
derived from the base via SWB-TRACE-FILE-FOR-CELL so concurrent cells get
distinct destinations."
  (let ((tf (swb-runner-config-trace-file config)))
    (cond ((and tf cell-label)
           (values (swb-trace-file-for-cell tf cell-label) t))
          (tf
           (values (pathname tf) t))
          (t (values (swb-events-tempfile) nil)))))

(defun swb-env-prefix (config events-file)
  "POSIX shell prefix that launches the SWB proxy in front of xcodebuild.
Every interpolated value flows through SHELL-QUOTE-SINGLE so paths containing
spaces or apostrophes survive the shell intact. Returns a string ending in one
space so callers can simply CONCATENATE it onto an xcodebuild command."
  (format nil "env ~A=~A ~A=~A ~A=~A~@[ ~A=1~] "
          swb:+service-path-env+
          (shell-quote-single (namestring (swb-runner-config-self-exe config)))
          swb:+real-service-env+
          (shell-quote-single (namestring (swb-runner-config-real-service config)))
          swb:+events-path-env+
          (shell-quote-single (namestring events-file))
          (when (swb-runner-config-trace config) swb:+trace-env+)))

(defun pty-wrap-command (cmd-str)
  "Prepend BSD script(1) so CMD-STR runs under a pseudo-TTY. xcodebuild's
stdout is libc-block-buffered when its output isn't a TTY (the default when
we capture it via a pipe), which delays xctest's per-suite finish lines
(`Test Suite '...' passed at ...') until xcodebuild itself exits. Running
under a pty makes stdout line-buffer, so the dashboard's text classifier
sees each line as it's emitted. `-q' suppresses script's startup banner;
piping the typescript to /dev/null discards the recorded transcript -- our
drain reads from the process's own stdout, which script forwards from the
pty. This is the same trick xcbeautify and xcpretty use."
  (concatenate 'string "script -q /dev/null " cmd-str))

(defun swb-consume-events (path done-fn handler)
  "Tail PATH, reading one event form at a time and passing each to HANDLER.
Tolerates partial writes (a form not yet fully flushed) by rewinding and
retrying. Returns once DONE-FN is true and no complete forms remain."
  (with-open-file (s path :direction :input :if-does-not-exist :create)
    (let ((*read-eval* nil))
      (loop
        (let* ((pos (file-position s))
               (form (handler-case (read s nil :eof)
                       (end-of-file () :partial)
                       (error () :skip))))
          (cond
            ((eq form :eof)
             (when (funcall done-fn) (return))
             (sleep 0.03))
            ((eq form :partial)
             (file-position s pos)
             (if (funcall done-fn) (return) (sleep 0.03)))
            ((eq form :skip)
             (read-line s nil :eof))
            (t (ignore-errors (funcall handler form)))))))))

(defun run-xcodebuild/swb (cmd-str action console config)
  "Run CMD-STR with the dashboard driven by Swift Build protocol events.
CONFIG is an SWB-RUNNER-CONFIG carrying the proxy paths, the trace flag, and
optionally a persistent trace-file path. Returns the xcodebuild exit code."
  (let ((stats (make-xcbuild-stats :action action)))
    (multiple-value-bind (events-file persistent-p) (swb-resolve-events-file config)
      (let* ((progress (make-instance 'xcbuild-progress :stats stats))
             (env-cmd (pty-wrap-command
                       (concatenate 'string (swb-env-prefix config events-file) cmd-str)))
             (proc (uiop:launch-program env-cmd
                                        :output :stream
                                        :error-output :output
                                        :input :interactive))
             (out (uiop:process-info-output proc))
             (lock (bt:make-lock "xcbuild-render"))
             (done nil)
             (render-thread nil) (event-thread nil) (drain-thread nil))
        ;; Create the file up front so the reader can open it before the proxy does.
        (ignore-errors (close (open events-file :direction :output
                                                :if-does-not-exist :create
                                                :if-exists :append)))
        (flet ((render ()
                 (bt:with-lock-held (lock)
                   (sc:superconsole-render console progress))))
          ;; Drain xcodebuild's own stdout so its pipe never blocks. The build graph
          ;; is driven by protocol events, but the test-execution and SPM-resolution
          ;; phases emit none, so lines matching *xcbuild-swb-text-types* (test
          ;; progress and package fetch/clone/checkout) are fed through the text
          ;; classifier to keep the dashboard live; all other lines are discarded.
          (setf drain-thread
                (bt:make-thread
                 (lambda ()
                   (loop for l = (read-line out nil :eof) until (eq l :eof)
                         do (let ((ev (xcbuild-swb-text-event l)))
                              (when ev
                                (bt:with-lock-held (lock)
                                  (xcbuild-handle-line console stats ev nil)
                                  (sc:superconsole-render console progress))))))
                 :name "xcbuild-drain"))
          (setf event-thread
                (bt:make-thread
                 (lambda ()
                   (swb-consume-events
                    events-file (lambda () done)
                    (lambda (event)
                      (bt:with-lock-held (lock)
                        (xcbuild-handle-event console stats event)
                        (sc:superconsole-render console progress)))))
                 :name "xcbuild-events"))
          (setf render-thread
                (bt:make-thread
                 (lambda ()
                   (loop until done
                         do (sleep *xcbuild-render-interval*)
                            (unless done (render))))
                 :name "xcbuild-render"))
          (unwind-protect
               (let ((exit-code (uiop:wait-process proc)))
                 (setf done t)
                 (setf (xcbuild-stats-result stats)
                       (if (zerop exit-code) :success :failure))
                 exit-code)
            (setf done t)
            (ignore-errors (bt:join-thread drain-thread))
            (ignore-errors (bt:join-thread event-thread))
            (when render-thread (ignore-errors (bt:join-thread render-thread)))
            ;; Persist the events file when the user asked for --swb-trace-file;
            ;; otherwise it was a temp file and we tidy it up.
            (unless persistent-p (ignore-errors (delete-file events-file)))
            ;; Gated on the trace flag -- BUILD_CONSOLE_OUTPUT counters accrue
            ;; on every SWB build, so without this gate the digest fired even
            ;; when --swb-trace was absent.
            (when (swb-runner-config-trace config)
              (xcbuild-emit-trace-digest console stats
                                         :trace-file (when persistent-p events-file)))
            ;; Final '** ACTION SUCCEEDED/FAILED **' banner is rendered below
            ;; the single-cell summary by xcbuild-progress's :final draw.
            (when (sc:superconsole-output console)
              (sc:superconsole-finalize console progress))))))))

(defun make-xcodebuild-command (name description
                                &key (scheme-accessors (list #'model-schemes #'model-scheme))
                                     (cell-accessors (list #'model-cells)))
  "Create a clingon command that runs xcodebuild with the given action. Uses the
repeatable (matrix) option set so multiple schemes/destinations run in parallel."
  (clingon:make-command
   :name name
   :description description
   :options (xcodebuild-options name :multi t)
   :handler (lambda (cmd) (run-xcodebuild cmd name
                                          :scheme-accessors scheme-accessors
                                          :cell-accessors cell-accessors))))

(defun xcodebuild-options (action-description &key (multi nil))
  "Return common xcodebuild CLI options with ACTION-DESCRIPTION for the scheme.
When MULTI, --scheme/--sim/--device are repeatable (clingon :list) and --jobs and
--fail-fast are added for the parallel scheme x destination matrix."
  (append
   (list
    (clingon:make-option
     (if multi :list :string)
     :description (format nil "scheme to ~A~:[~; (repeat for a matrix)~]"
                          action-description multi)
     :short-name #\s
     :long-name "scheme"
     :key :scheme)
    (clingon:make-option
     (if multi :list :string)
     :description (format nil "simulator UDID destination~:[~; (repeatable in matrix mode)~]" multi)
     :long-name "sim"
     :key :sim)
    (clingon:make-option
     (if multi :list :string)
     :description (format nil "device UDID destination~:[~; (repeatable in matrix mode)~]" multi)
     :long-name "device"
     :key :device)
    (clingon:make-option
     :string
     :description (format nil "~A configuration (Debug or Release)" action-description)
     :short-name #\c
     :long-name "configuration"
     :initial-value "Debug"
     :key :configuration)
    (clingon:make-option
     :string
     :description "derived data path (single cell) or base directory under which each cell of a parallel matrix gets its own stable subdirectory"
     :long-name "derived-data"
     :key :derived-data)
    (clingon:make-option
     :flag
     :description "use Swift Build protocol interception for richer progress"
     :long-name "use-swb"
     :key :use-swb)
    (clingon:make-option
     :flag
     :description "show the cache-hit (up-to-date task) percentage (on by default with SWB; implies --use-swb)"
     :long-name "cache-hits"
     :key :cache-hits)
    (clingon:make-option
     :flag
     :description "log every Swift Build wire message name to the dashboard scrollback (debug aid; implies --use-swb)"
     :long-name "swb-trace"
     :key :swb-trace)
    (clingon:make-option
     :string
     :description "write the full decoded SWB event stream to this path (parallel cells suffix the file name with their label; implies --swb-trace)"
     :long-name "swb-trace-file"
     :key :swb-trace-file))
   (when multi
     (list
      (clingon:make-option
       :list
       :description "explicit scheme@destination cell, e.g. MyScheme@sim=UDID, MyScheme@device=UDID, or MyScheme@platform=tvOS Simulator,id=UDID (repeatable; bypasses the scheme x destination matrix)"
       :long-name "cell"
       :key :cell)
      (clingon:make-option
       :integer
       :description "max cells (scheme x destination) to run concurrently (default: all)"
       :short-name #\j
       :long-name "jobs"
       :key :jobs)
      (clingon:make-option
       :flag
       :description "abort remaining cells on the first failure"
       :long-name "fail-fast"
       :key :fail-fast)))
   (when (string= action-description "test")
     (list
      (clingon:make-option
       :list
       :description "run only the given test identifier, formatted as TestTarget[/TestClass[/TestMethod]] (forwarded to xcodebuild as -only-testing:; repeatable)"
       :long-name "only-testing"
       :key :only-testing)
      (clingon:make-option
       :list
       :description "skip the given test identifier, formatted as TestTarget[/TestClass[/TestMethod]] (forwarded to xcodebuild as -skip-testing:; repeatable)"
       :long-name "skip-testing"
       :key :skip-testing)))
   (when (string= action-description "clean")
     (list
      (clingon:make-option
       :flag
       :description "also remove cupertino's per-cell DerivedData tree (~/Library/Developer/Xcode/DerivedData-cupertino/); runs standalone when no scheme/destination is given"
       :long-name "prune"
       :key :prune)))))

(defun build-test-filter-args (only-testing skip-testing)
  "Build the xcodebuild test-filter argument string from ONLY-TESTING and
SKIP-TESTING (each a list of TestTarget[/TestClass[/TestMethod]] identifiers).
Each identifier becomes one shell-quoted -only-testing:/-skip-testing: flag.
Returns NIL when both lists are empty."
  (let ((parts (nconc
                (loop for id in only-testing
                      collect (format nil "-only-testing:~A" (shell-quote-single id)))
                (loop for id in skip-testing
                      collect (format nil "-skip-testing:~A" (shell-quote-single id))))))
    (when parts (format nil "~{~A~^ ~}" parts))))
