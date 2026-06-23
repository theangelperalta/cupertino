(in-package :cupertino)

(defun info/top-level/sub-commands ()
  "Returns the list of sub-commands for the top-level command"
  (list
   (info/project/command)
   (info/sim/command)
   (info/device/command)))

(defun info/top-level/handler (cmd)
  "The handler for the top-level command. Will print the usage of the app"
  (clingon:print-usage-and-exit cmd t))

(defun info/top-level/command ()
  "Returns the top-level info command"
  (clingon:make-command :name "info"
                        :description "A command to display project/device information"
                        :long-description "A command to display project/device information"
                        :handler #'info/top-level/handler
                        :options (info/top-level/options)
                        :sub-commands (info/top-level/sub-commands)))

(defun info/top-level/options ()
  "Returns the options for the `info' command"
  (list))


(defun info/project/options ()
  (list
   (clingon:make-option
    :string
    :description "project file (e.g., HelloWorld.xcodeproj)"
    :short-name #\p
    :long-name "project"
    :key :project)
   (clingon:make-option
    :string
    :description "workspace file (e.g., HelloWorld.xcworkspace)"
    :short-name #\w
    :long-name "workspace"
    :key :workspace)))

(defun info/project/handler (cmd)
  "Handler for the `device/info' command"
  (let* ((path (first (clingon:command-arguments cmd)))
         (project-file-type (get-project-file-type path))
        (project (clingon:getopt cmd :project))
        (workspace (clingon:getopt cmd :workspace)))
    (if (and project workspace)
        (cup-error "You cannot specify both an Xcode project and a workspace.")
        (cond (project (print-project-info ':project project))
        (workspace (print-project-info ':workspace workspace))
        (project-file-type (print-project-info project-file-type path))
        (t (clingon:print-usage-and-exit cmd t))))))

(defun info/project/command ()
  "A command to display physical device information"
  (clingon:make-command
   :name "project"
   :description "Display project information"
   :options (info/project/options)
   :handler #'info/project/handler))


(defun info/sim/handler (cmd)
  "Handler for the `sim/info' command"
  (declare (ignore cmd))
  (print-sim-info))

(defun info/sim/command ()
  "A command to display simulator device information"
  (clingon:make-command
   :name "sim"
   :description "Display simulator device information"
   :handler #'info/sim/handler))

(defun info/device/handler (cmd)
  "Handler for the `device/info' command"
  (declare (ignore cmd))
  (print-device-info))

(defun info/device/command ()
  "A command to display physical device information"
  (clingon:make-command
   :name "device"
   :description "Display physical device information"
   :handler #'info/device/handler))


;; Config command

(defun parse-positive-number (string what)
  "Parse STRING as a positive real number, or exit with an error mentioning WHAT."
  (let ((n (ignore-errors
            (let ((*read-default-float-format* 'double-float))
              (read-from-string string)))))
    (unless (and (realp n) (plusp n))
      (cup-error "~A must be a positive number, got ~S." what string)
      (uiop:quit 1))
    n))

(defun parse-bool-arg (string what)
  "Parse STRING as a boolean (true/false/yes/no/on/off/1/0), or exit with an error."
  (cond
    ((member string '("true" "yes" "on" "1" "t") :test #'string-equal) t)
    ((member string '("false" "no" "off" "0" "nil") :test #'string-equal) nil)
    (t (cup-error "~A must be true or false, got ~S." what string)
       (uiop:quit 1))))

(defun config/handler (cmd)
  "Handler for the `config' command.
With options, updates the config file. Without options, prints the current config."
  (let* ((path (first (clingon:command-arguments cmd)))
         (scheme (clingon:getopt cmd :scheme))
         (test-scheme (clingon:getopt cmd :test-scheme))
         (schemes (clingon:getopt cmd :schemes))
         (test-schemes (clingon:getopt cmd :test-schemes))
         (cells (clingon:getopt cmd :cells))
         (test-cells (clingon:getopt cmd :test-cells))
         (sim (clingon:getopt cmd :sim))
         (device (clingon:getopt cmd :device))
         (max-jobs (clingon:getopt cmd :max-jobs))
         (slow-threshold (let ((s (clingon:getopt cmd :slow-threshold)))
                           (when s (parse-positive-number s "slow-threshold"))))
         (use-swb-raw (clingon:getopt cmd :use-swb))
         (cache-hits-raw (clingon:getopt cmd :cache-hits))
         (updates (append (when scheme (list :scheme scheme))
                          (when test-scheme (list :test-scheme test-scheme))
                          (when schemes (list :schemes schemes))
                          (when test-schemes (list :test-schemes test-schemes))
                          (when cells (list :cells cells))
                          (when test-cells (list :test-cells test-cells))
                          (when sim (list :sim sim))
                          (when device (list :device device))
                          (when max-jobs (list :max-jobs max-jobs))
                          (when slow-threshold (list :slow-threshold slow-threshold))
                          (when use-swb-raw
                            (list :use-swb (parse-bool-arg use-swb-raw "use-swb")))
                          (when cache-hits-raw
                            (list :cache-hits (parse-bool-arg cache-hits-raw "cache-hits"))))))
    (if updates
        (let ((merged (model:update-model-config path updates)))
          (format t "~A ~A:~%"
                  (colored-text "Updated config at" :green)
                  (colored-text (namestring (model:resolve-config-file path)) :blue))
          (loop for (k v) on merged by #'cddr
                do (format t "  ~A: ~A~%" (colored-text k :cyan) v)))
        ;; No options — print current config
        (let ((plist (model:load-model path)))
          (format t "~A ~A:~%"
                  (colored-text "Config at" :white)
                  (colored-text (namestring (model:resolve-config-file path)) :blue))
          (loop for (k v) on plist by #'cddr
                do (format t "  ~A: ~A~%" (colored-text k :cyan) v))))))

(defun config/command ()
  "A command to view or update the project config."
  (clingon:make-command
   :name "config"
   :description "View or update project configuration"
   :options (config/options)
   :handler #'config/handler))

(defun config/options ()
  "Returns the options for the `config' command."
  (list
   (clingon:make-option
    :string
    :description "set the default build scheme"
    :short-name #\s
    :long-name "scheme"
    :key :scheme)
   (clingon:make-option
    :string
    :description "set the default test scheme"
    :short-name #\t
    :long-name "test-scheme"
    :key :test-scheme)
   (clingon:make-option
    :list
    :description "set the default build schemes for parallel matrix runs (repeatable)"
    :long-name "schemes"
    :key :schemes)
   (clingon:make-option
    :list
    :description "set the default test schemes for parallel matrix runs (repeatable)"
    :long-name "test-schemes"
    :key :test-schemes)
   (clingon:make-option
    :list
    :description "set default build cells as scheme@destination pairings, e.g. MyScheme@sim=UDID (repeatable)"
    :long-name "cells"
    :key :cells)
   (clingon:make-option
    :list
    :description "set default test cells as scheme@destination pairings, e.g. MyTests@sim=UDID (repeatable)"
    :long-name "test-cells"
    :key :test-cells)
   (clingon:make-option
    :string
    :description "set the default simulator UDID"
    :long-name "sim"
    :key :sim)
   (clingon:make-option
    :string
    :description "set the default device UDID"
    :long-name "device"
    :key :device)
   (clingon:make-option
    :integer
    :description "max in-progress actions shown in the build dashboard"
    :long-name "max-jobs"
    :key :max-jobs)
   (clingon:make-option
    :string
    :description "seconds before a build action is highlighted as slow"
    :long-name "slow-threshold"
    :key :slow-threshold)
   (clingon:make-option
    :string
    :description "use Swift Build protocol interception (true or false)"
    :long-name "use-swb"
    :key :use-swb)
   (clingon:make-option
    :string
    :description "show cache-hit (up-to-date task) percentage; defaults to on with SWB (true or false)"
    :long-name "cache-hits"
    :key :cache-hits)))

;; Init command — interactive project setup

(defun prompt-choice (label items
                      &key (display-fn #'identity)
                           (allow-none nil)
                           (prompt-stream *standard-output*)
                           (default-index nil))
  "Display LABEL and a numbered list of ITEMS on PROMPT-STREAM and read a
selection from *standard-input*. ITEMS entries may be plain objects, or
plists shaped as (:header STRING) or (:item OBJ ...). For :item entries the
returned value is the plist itself (callers extract :item/:udid as needed);
plain objects are returned as-is. When DEFAULT-INDEX is non-NIL it is the
1-based index (over selectable entries, skipping headers) of the item picked
on bare Enter; that entry is marked `(current)' in the menu and the prompt
suffix advertises `Enter for current'. Returns the chosen object, or NIL when
ALLOW-NONE and the user enters `0' / EOF / bare Enter without a default."
  (let* ((selectable (remove-if (lambda (it)
                                  (and (consp it) (eq (first it) :header)))
                                items))
         (n-sel (length selectable)))
    (format prompt-stream "~%~A:~%" (colored-text label :cyan))
    (let ((sel-idx 0))
      (dolist (it items)
        (cond
          ((and (consp it) (eq (first it) :header))
           (format prompt-stream "  ~A~%"
                   (colored-text (format nil "-- ~A --" (getf it :header))
                                 :bright-black)))
          (t
           (incf sel-idx)
           (let ((display (cond ((and (consp it) (eq (first it) :item))
                                 (or (getf it :display)
                                     (funcall display-fn (getf it :item))))
                                (t (funcall display-fn it)))))
             (format prompt-stream "  ~A ~A~%"
                     (colored-text (format nil "~D." sel-idx) :yellow)
                     (colored-text display :green)))))))
    (when allow-none
      (format prompt-stream "  ~A ~A~%"
              (colored-text "0." :yellow)
              (colored-text "(none)" :bright-black)))
    (let ((suffix (with-output-to-string (s)
                    (write-string "Select " s)
                    (write-string (string-downcase label) s)
                    (when (plusp n-sel)
                      (format s " [1-~D" n-sel)
                      (when allow-none (write-string ", 0 to skip" s))
                      (when default-index (write-string ", Enter for current" s))
                      (write-string "]" s))
                    (write-string ": " s))))
      (loop
        (format prompt-stream "~A" (colored-text suffix :white))
        (force-output prompt-stream)
        (let* ((raw (read-line *standard-input* nil nil))
               (line (and raw (str:trim raw)))
               (n (and line
                       (handler-case (parse-integer line :junk-allowed nil)
                         (error () nil)))))
          (cond
            ;; EOF -> abort
            ((null raw) (return nil))
            ;; Bare Enter with a default -> pick the default
            ((and (string= line "") default-index
                  (>= default-index 1) (<= default-index n-sel))
             (return (nth (1- default-index) selectable)))
            ;; Bare Enter, no default, allow-none -> NIL
            ((and (string= line "") allow-none)
             (return nil))
            ((and n (= n 0) allow-none)
             (return nil))
            ((and n (>= n 1) (<= n n-sel))
             (return (nth (1- n) selectable)))
            (t (format prompt-stream "Invalid choice, try again.~%"))))))))

(defun find-project-files (dir)
  "Recursively find all .xcworkspace and .xcodeproj directories under DIR.
Excludes matches inside .build, DerivedData, and Pods directories.
Returns a list of (type path) pairs where type is :workspace or :project."
  (let ((results nil))
    (labels ((excluded-p (path-str)
               (or (search "/.build/" path-str)
                   (search "/DerivedData/" path-str)
                   (search "/Pods/" path-str)))
             (walk (current-dir)
               (dolist (sub (uiop:subdirectories current-dir))
                 (let ((name (namestring sub)))
                   (cond
                     ((excluded-p name) nil)
                     ((str:ends-with-p ".xcworkspace/" name)
                      (push (list :workspace name) results))
                     ((str:ends-with-p ".xcodeproj/" name)
                      (push (list :project name) results))
                     (t (walk sub)))))))
      (walk dir))
    (nreverse results)))

(defun init/handler (cmd)
  "Handler for the `init' command. Interactively sets up project configuration."
  (let* ((path (first (clingon:command-arguments cmd)))
         (dir (or path (uiop:getcwd)))
         (interactive (stdin-interactive-p))
         (existing (ignore-errors (model:load-model path)))
         (found (find-project-files dir))
         (target nil))
    (unless interactive
      (cup-warn "running non-interactively; skipping picker prompts"))
    (unless found
      (cup-error "No .xcodeproj or .xcworkspace found in ~A" dir)
      (uiop:quit 1))
    (if (= (length found) 1)
        (setf target (first found))
        (setf target (prompt-choice "Multiple projects found — select one" found
                       :display-fn (lambda (f)
                                     (format nil "[~A] ~A"
                                             (get-project-file-type-str (first f))
                                             (second f))))))
    (let ((target-type (first target))
          (target-path (second target)))
      (format t "Using ~A: ~A~%" (get-project-file-type-str target-type)
              (colored-text target-path :blue))
      ;; Discover schemes
      (let* ((info (list-project-info target-type target-path))
             (key (get-project-file-type-str target-type))
             (project-info (when info (gethash key info)))
             (schemes (when project-info (gethash "schemes" project-info)))
             (scheme nil)
             (test-scheme nil)
             (sim nil)
             (device nil))
        (cond
          ((not schemes)
           (format t "~A~%" (colored-text "No schemes found in project." :yellow)))
          (interactive
           ;; Build scheme is required; test scheme is optional. Both reuse
           ;; the picker so single-option auto-pick and (current) marking
           ;; apply uniformly.
           (setf scheme (pick-project-schemes
                         :schemes schemes
                         :current-scheme (getf existing :scheme)
                         :label "Select default build scheme"
                         :allow-none nil))
           (setf test-scheme (pick-project-schemes
                              :schemes schemes
                              :current-scheme (getf existing :test-scheme)
                              :label "Select default test scheme"))))
        (when interactive
          ;; Discover simulators (picker uses *error-output* for consistency with `pick').
          (setf sim (pick-available-sims :current-udid (getf existing :sim)))
          (unless sim
            (format *error-output* "~A~%"
                    (colored-text "No available simulators found." :yellow)))
          ;; Discover physical devices.
          (setf device (pick-connected-devices
                        :current-udid (getf existing :device)))
          (unless device
            (format *error-output* "~A~%"
                    (colored-text "No connected devices found." :yellow))))
        ;; Write config
        (let ((plist (list :project-type target-type
                           :project-path (namestring target-path)
                           :sim sim
                           :device device
                           :scheme scheme
                           :test-scheme test-scheme
                           :use-swb nil)))
          (model:update-model-config path plist)
          (format t "~%~A ~A~%"
                  (colored-text "Config written to" :green)
                  (colored-text (namestring (model:resolve-config-file path)) :blue))
          (loop for (k v) on plist by #'cddr
                do (format t "  ~A: ~A~%" (colored-text k :cyan) v)))))))

(defun init/command ()
  "A command for interactive project setup."
  (clingon:make-command
   :name "init"
   :description "Interactively set up project configuration"
   :handler #'init/handler))

;; Pick command — interactive UDID selection. Menus and prompts go to
;; *error-output*; the chosen UDID goes to *standard-output* so shell
;; substitution (`cupertino build --sim "$(cupertino pick sim)"`) is safe.
;; `--save' merges :sim / :device into .cupertino/cupertino.lisp via
;; model:update-model-config (which auto-creates the config on a virgin
;; project, so the flag works even before `cupertino init').

(defun pick/sim/options ()
  (list (clingon:make-option
         :flag
         :description "save the picked UDID to .cupertino/cupertino.lisp"
         :long-name "save"
         :key :save)))

(defun pick/sim/run (cmd)
  "Body of `cupertino pick sim'. Returns the exit code instead of quitting so
tests can drive it without bailing out of the Lisp image."
  (cond
    ((not (stdin-interactive-p))
     (cup-error "`cupertino pick sim' needs an interactive terminal (stdin is not a TTY).")
     1)
    (t
     (let* ((cfg-path (first (clingon:command-arguments cmd)))
            (current (getf (ignore-errors (model:load-model cfg-path)) :sim))
            (udid (pick-available-sims :current-udid current)))
       (cond
         ((null udid)
          (cup-error "No available simulators to pick from. Try `cupertino info sim'.")
          1)
         (t
          (format *error-output* "~A: ~A~%"
                  (colored-text "Picked" :cyan) udid)
          (format *standard-output* "~A~%" udid)
          (when (clingon:getopt cmd :save)
            (model:update-model-config cfg-path (list :sim udid))
            (format *error-output* "~A :sim ~A~%"
                    (colored-text "saved" :green) udid))
          0))))))

(defun pick/sim/handler (cmd)
  (uiop:quit (pick/sim/run cmd)))

(defun pick/sim/command ()
  (clingon:make-command :name "sim"
                        :description "Interactively pick a simulator UDID"
                        :options (pick/sim/options)
                        :handler #'pick/sim/handler))

(defun pick/device/options ()
  (list (clingon:make-option
         :flag
         :description "save the picked UDID to .cupertino/cupertino.lisp"
         :long-name "save"
         :key :save)))

(defun pick/device/run (cmd)
  "Body of `cupertino pick device'. Returns the exit code instead of quitting
so tests can drive it without bailing out of the Lisp image."
  (cond
    ((not (stdin-interactive-p))
     (cup-error "`cupertino pick device' needs an interactive terminal (stdin is not a TTY).")
     1)
    (t
     (let* ((cfg-path (first (clingon:command-arguments cmd)))
            (current (getf (ignore-errors (model:load-model cfg-path)) :device))
            (udid (pick-connected-devices :current-udid current)))
       (cond
         ((null udid)
          (cup-error "No connected devices to pick from. Try `cupertino info device'.")
          1)
         (t
          (format *error-output* "~A: ~A~%"
                  (colored-text "Picked" :cyan) udid)
          (format *standard-output* "~A~%" udid)
          (when (clingon:getopt cmd :save)
            (model:update-model-config cfg-path (list :device udid))
            (format *error-output* "~A :device ~A~%"
                    (colored-text "saved" :green) udid))
          0))))))

(defun pick/device/handler (cmd)
  (uiop:quit (pick/device/run cmd)))

(defun pick/device/command ()
  (clingon:make-command :name "device"
                        :description "Interactively pick a physical device UDID"
                        :options (pick/device/options)
                        :handler #'pick/device/handler))

;; `pick scheme` / `pick test-scheme` share the same skeleton as sim/device:
;; chosen scheme name on *standard-output*, prose on *error-output*, optional
;; `--save' persists to the config. The two commands differ only in which
;; config key (:scheme vs :test-scheme) and prompt label they use.

(defun pick/scheme/options ()
  (list (clingon:make-option
         :flag
         :description "save the picked scheme to .cupertino/cupertino.lisp"
         :long-name "save"
         :key :save)))

(defun pick/scheme/run-with (cmd config-key label)
  "Shared body for `pick scheme' and `pick test-scheme'. CONFIG-KEY is the
plist key (:scheme or :test-scheme) used both to seed the (current) marker
from the existing config and to persist with --save. Returns the exit code."
  (cond
    ((not (stdin-interactive-p))
     (cup-error "`cupertino pick ~A' needs an interactive terminal (stdin is not a TTY)."
                (string-downcase (symbol-name config-key)))
     1)
    (t
     (let* ((cfg-path (first (clingon:command-arguments cmd)))
            (current (getf (ignore-errors (model:load-model cfg-path)) config-key))
            (scheme (pick-project-schemes :dir cfg-path
                                          :current-scheme current
                                          :label label)))
       (cond
         ((null scheme)
          (cup-error "No schemes to pick from. Try `cupertino info project'.")
          1)
         (t
          (format *error-output* "~A: ~A~%"
                  (colored-text "Picked" :cyan) scheme)
          (format *standard-output* "~A~%" scheme)
          (when (clingon:getopt cmd :save)
            (model:update-model-config cfg-path (list config-key scheme))
            (format *error-output* "~A ~A ~A~%"
                    (colored-text "saved" :green) config-key scheme))
          0))))))

(defun pick/scheme/run (cmd)
  (pick/scheme/run-with cmd :scheme "Select scheme"))

(defun pick/scheme/handler (cmd)
  (uiop:quit (pick/scheme/run cmd)))

(defun pick/scheme/command ()
  (clingon:make-command :name "scheme"
                        :description "Interactively pick a build scheme"
                        :options (pick/scheme/options)
                        :handler #'pick/scheme/handler))

(defun pick/test-scheme/run (cmd)
  (pick/scheme/run-with cmd :test-scheme "Select test scheme"))

(defun pick/test-scheme/handler (cmd)
  (uiop:quit (pick/test-scheme/run cmd)))

(defun pick/test-scheme/command ()
  (clingon:make-command :name "test-scheme"
                        :description "Interactively pick a test scheme"
                        :options (pick/scheme/options)
                        :handler #'pick/test-scheme/handler))

;; `pick cell` / `pick test-cell` walk scheme then destination (unified sims +
;; devices) and emit the `Scheme@sim=UDID' / `Scheme@device=UDID' string the
;; matrix runner consumes via --cell. --save appends to :cells / :test-cells;
;; --save --replace overwrites the saved list with [picked-cell].

(defun pick/cell/options ()
  (list (clingon:make-option
         :flag
         :description "append the picked cell to .cupertino/cupertino.lisp"
         :long-name "save"
         :key :save)
        (clingon:make-option
         :flag
         :description "with --save, replace the saved list instead of appending"
         :long-name "replace"
         :key :replace)))

(defun pick/cell/run-with (cmd cmd-name config-key scheme-key label)
  "Shared body for `pick cell' and `pick test-cell'. CMD-NAME is the
sub-command label used in error messages (`cell' / `test-cell'). CONFIG-KEY
is the plist key (:cells / :test-cells) the picker appends to with --save.
SCHEME-KEY (:scheme / :test-scheme) seeds the scheme picker's (current)
marker from the model. Returns the exit code."
  (cond
    ((not (stdin-interactive-p))
     (cup-error "`cupertino pick ~A' needs an interactive terminal (stdin is not a TTY)." cmd-name)
     1)
    (t
     (let* ((cfg-path (first (clingon:command-arguments cmd)))
            (model-plist (ignore-errors (model:load-model cfg-path)))
            (current-scheme (getf model-plist scheme-key))
            (current-udid (or (getf model-plist :device) (getf model-plist :sim)))
            (cell (pick-cell :dir cfg-path
                             :current-scheme current-scheme
                             :current-udid current-udid
                             :label label)))
       (cond
         ((null cell)
          (cup-error "No cell picked.")
          1)
         (t
          (format *error-output* "~A: ~A~%"
                  (colored-text "Picked" :cyan) cell)
          (format *standard-output* "~A~%" cell)
          (when (clingon:getopt cmd :save)
            (let* ((existing (alexandria:ensure-list (getf model-plist config-key)))
                   (merged (if (clingon:getopt cmd :replace)
                               (list cell)
                               (append existing (list cell)))))
              (model:update-model-config cfg-path (list config-key merged))
              (format *error-output* "~A ~A ~A~%"
                      (colored-text "saved" :green) config-key merged)))
          0))))))

(defun pick/cell/run (cmd)
  (pick/cell/run-with cmd "cell" :cells :scheme "Select scheme for cell"))

(defun pick/cell/handler (cmd)
  (uiop:quit (pick/cell/run cmd)))

(defun pick/cell/command ()
  (clingon:make-command :name "cell"
                        :description "Interactively pick a scheme@destination build cell"
                        :options (pick/cell/options)
                        :handler #'pick/cell/handler))

(defun pick/test-cell/run (cmd)
  (pick/cell/run-with cmd "test-cell" :test-cells :test-scheme
                      "Select test scheme for cell"))

(defun pick/test-cell/handler (cmd)
  (uiop:quit (pick/test-cell/run cmd)))

(defun pick/test-cell/command ()
  (clingon:make-command :name "test-cell"
                        :description "Interactively pick a scheme@destination test cell"
                        :options (pick/cell/options)
                        :handler #'pick/test-cell/handler))

(defun pick/top-level/sub-commands ()
  (list (pick/sim/command) (pick/device/command)
        (pick/scheme/command) (pick/test-scheme/command)
        (pick/cell/command) (pick/test-cell/command)))

(defun pick/top-level/handler (cmd)
  (clingon:print-usage-and-exit cmd t))

(defun pick/top-level/command ()
  (clingon:make-command :name "pick"
                        :description "Interactively pick a simulator, device, scheme, or cell"
                        :handler #'pick/top-level/handler
                        :options (list)
                        :sub-commands (pick/top-level/sub-commands)))