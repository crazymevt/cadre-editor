;;;; setup.lisp — checking for the programs Cadre uses
;;;;
;;;; On the first run, and on later ones if SBCL can't be found, Cadre looks
;;;; for SBCL, Quicklisp, Git, Claude Code and the optional tools, and shows
;;;; what's missing with the command that installs it. Quicklisp can be
;;;; installed from there: Cadre downloads quicklisp.lisp, runs its
;;;; installer with SBCL and adds it to ~/.sbclrc, as quicklisp.org says to.
;;;; Check Setup (M-x check-setup) shows the same page any time.

(in-package #:cadre-ui)

(define-option *check-setup-at-startup* t boolean
  "Check for SBCL and the tools Cadre uses when it starts: on the first run, and whenever SBCL can't be found."
  :category "Session")

(defparameter *quicklisp-url* "https://beta.quicklisp.org/quicklisp.lisp")

(defparameter *setup-tools*
  '((:sbcl "SBCL" :required "Runs your code in the REPL, the compiler and the debugger")
    (:quicklisp "Quicklisp" :recommended "Libraries for your projects")
    (:git "Git" :recommended "The change gutter, Source Control and history")
    (:claude "Claude Code" :optional "The Assistant panel")
    (:racket "Racket" :optional "Racket files' REPL and tools")
    (:clojure "Clojure CLI" :optional "Clojure files' REPL")
    (:prettier "Prettier" :optional "Format Document for JavaScript, TypeScript and HTML"))
  "Each tool Check Setup looks for: (key name importance what-for).")

(defun install-command (key)
  "The shell command that installs the tool KEY here, or nil."
  (let ((mac (macos-p)))
    (case key
      (:sbcl (if mac "brew install sbcl" "sudo apt install sbcl"))
      (:git (if mac "xcode-select --install" "sudo apt install git"))
      (:claude "curl -fsSL https://claude.ai/install.sh | bash")
      (:racket (if mac "brew install --cask racket" "sudo apt install racket"))
      (:clojure (if mac "brew install clojure/tools/clojure" nil))
      (:prettier "npm install -g prettier"))))

(defun install-note (key)
  "What to do besides, or instead of, INSTALL-COMMAND for KEY."
  (case key
    (:claude "then sign in with claude auth login")
    (:clojure (and (not (macos-p)) "see clojure.org/guides/install_clojure"))
    (:prettier "needs Node.js")))

(defun program-path (program)
  "Where PROGRAM (a name looked for on PATH, or a path) is, or nil."
  (if (find #\/ program)
      (and (probe-file program) program)
      (let ((found (ignore-errors
                    (string-trim '(#\Newline #\Space)
                                 (uiop:run-program (list "/bin/sh" "-c" (format nil "command -v ~a" (uiop:escape-sh-token program)))
                                                   :output :string :ignore-error-status t)))))
        (and found (plusp (length found)) found))))

(defun program-version (path &rest arguments)
  "The first line PATH prints when run with ARGUMENTS (default --version), or nil."
  (let ((output (ignore-errors
                 (uiop:run-program (cons path (or arguments '("--version")))
                                   :output :string :error-output nil :ignore-error-status t))))
    (and output (let ((line (string-trim '(#\Space #\Return) (first (uiop:split-string output :separator '(#\Newline))))))
                  (and (plusp (length line)) line)))))

;;; Quicklisp

(defun quicklisp-setup-file (&optional (home (user-homedir-pathname)))
  (merge-pathnames "quicklisp/setup.lisp" home))

(defun sbcl-lisp-p ()
  (string-equal "sbcl" (pathname-name (pathname (first *lisp-command*)))))

(defun quicklisp-state (&key (home (user-homedir-pathname)) (sbcl (sbcl-lisp-p)))
  ":ready, :not-loaded (installed, but SBCL's init file doesn't load it) or
:missing. With another Lisp than SBCL, installed counts as ready."
  (cond ((not (probe-file (quicklisp-setup-file home))) :missing)
        ((not sbcl) :ready)
        ((let ((init (ignore-errors (uiop:read-file-string (merge-pathnames ".sbclrc" home)))))
           (and init (search "quicklisp" init)))
         :ready)
        (t :not-loaded)))

;;; Checking

(defun check-tool (key)
  "What Check Setup finds for KEY: a plist of :found (true if it's there)
and :detail (where, or which version, or what's wrong)."
  (flet ((program (program &optional version)
           (let ((path (program-path program)))
             (list :found (and path t)
                   :detail (and path (or (and version (program-version path)) path))))))
    (case key
      (:sbcl (program (first *lisp-command*) t))
      (:quicklisp (let ((state (quicklisp-state)))
                    (list :found (eq state :ready) :state state
                          :detail (case state
                                    (:ready (uiop:native-namestring (quicklisp-setup-file)))
                                    (:not-loaded "Installed, but ~/.sbclrc doesn't load it")))))
      (:git (program "git" t))
      (:claude (let ((path (find-claude-program)))
                 (list :found (and path t) :detail path)))
      (:racket (program *racket-command*))
      (:clojure (program *clojure-command*))
      (:prettier (program "prettier")))))

(defun check-setup-in-background (then)
  "Check every tool on another thread (running programs takes a moment),
then call THEN on the GTK thread with (key . result) for each."
  (sb-thread:make-thread
   (lambda ()
     (let ((results (loop for (key) in *setup-tools*
                          collect (cons key (ignore-errors (check-tool key))))))
       (glib:call-in-main-thread (lambda () (funcall then results)))))
   :name "Cadre setup check"))

(defun setup-missing (results &key (importance '(:required :recommended)))
  "The keys of tools of IMPORTANCE that RESULTS say are missing."
  (loop for (key nil level) in *setup-tools*
        when (and (member level importance) (not (getf (cdr (assoc key results)) :found)))
          collect key))

;;; Installing Quicklisp

(defun install-quicklisp (then)
  "Install Quicklisp with SBCL and load it from ~/.sbclrc, on another
thread; its output goes to the Output panel. Then call THEN with true if
it worked."
  (let ((sbcl (program-path (first *lisp-command*)))
        (state (quicklisp-state))
        (file (merge-pathnames "setup/quicklisp.lisp" (cadre::cache-directory))))
    (unless (and sbcl (sbcl-lisp-p)) (editor-error "Installing Quicklisp needs SBCL"))
    (flet ((log-line (string) (glib:call-in-main-thread (lambda () (panel-log (window-panel *window*) string))))
           (run (arguments)
             (multiple-value-bind (output error-output status)
                 (uiop:run-program arguments :output :string :error-output :output :ignore-error-status t)
               (declare (ignore error-output))
               (values output status))))
      (sb-thread:make-thread
       (lambda ()
         (let ((ok (handler-case
                       (let ((add "(let ((ql-util::*do-not-prompt* t)) (ql:add-to-init-file))"))
                         (when (eq state :missing)
                           (log-line (format nil "Downloading ~a" *quicklisp-url*))
                           (ensure-directories-exist file)
                           (multiple-value-bind (output status)
                               (run (list "curl" "-fsSL" "-o" (uiop:native-namestring file) *quicklisp-url*))
                             (unless (zerop status) (error "Downloading failed: ~a" output))))
                         (log-line "Installing Quicklisp…")
                         (multiple-value-bind (output status)
                             (run (if (eq state :missing)
                                      (list sbcl "--non-interactive" "--no-userinit" "--load" (uiop:native-namestring file)
                                            "--eval" "(quicklisp-quickstart:install)" "--eval" add)
                                      (list sbcl "--non-interactive" "--no-userinit"
                                            "--load" (uiop:native-namestring (quicklisp-setup-file)) "--eval" add)))
                           (log-line output)
                           (and (zerop status) (eq (quicklisp-state) :ready))))
                     (error (e) (log-line (format nil "~a" e)) nil))))
           (glib:call-in-main-thread (lambda () (funcall then ok)))))
       :name "Cadre installing Quicklisp"))))

;;; The page

(defun setup-row (key name importance what-for result on-install)
  (let* ((found (getf result :found))
         (command (and (not found) (install-command key)))
         (note (and (not found) (install-note key)))
         (row (make-instance 'adw:action-row
                             :title (format nil "~a~@[ (~(~a~))~]" name (and (not (eq importance :required)) importance))
                             :subtitle (if found
                                           (getf result :detail)
                                           (format nil "~a. ~a~@[. Install with ~a~]~@[ (~a)~]"
                                                   (or (getf result :detail) "Not found") what-for command note))
                             :use-markup nil :subtitle-selectable t)))
    (adw:action-row-add-prefix row (make-instance 'gtk:image
                                                  :icon-name (if found "object-select-symbolic" "dialog-warning-symbolic")
                                                  :css-classes (list (if found "success" (if (eq importance :optional) "dim-label" "warning")))))
    (cond ((and (eq key :quicklisp) (not found) on-install)
           (let ((button (make-instance 'gtk:button :label (if (eq (getf result :state) :not-loaded) "Add to SBCL" "Install")
                                                    :valign :center :css-classes '("suggested-action"))))
             (gobject:connect button :clicked (lambda (b) (funcall on-install b row)))
             (adw:action-row-add-suffix row button)))
          (command
           (let ((button (make-instance 'gtk:button :icon-name "edit-copy-symbolic" :valign :center
                                                    :tooltip-text "Copy the command" :css-classes '("flat"))))
             (gobject:connect button :clicked (lambda (b) (declare (ignore b))
                                                (copy-to-clipboard command)
                                                (message "Copied: ~a" command)))
             (adw:action-row-add-suffix row button))))
    row))

(defun show-setup-dialog (results)
  "Show what RESULTS (from check-setup-in-background) found."
  (let* ((missing (setup-missing results))
         (dialog (adw:alert-dialog-new
                  (if missing "Finish Setting Up Cadre" "Cadre Is Set Up")
                  (cond ((member :sbcl missing)
                         "Cadre needs a Lisp to run your code. Install what's missing below, then check again.")
                        (missing "Some tools Cadre uses aren't installed. Copy a command to install one in a terminal.")
                        (t "Cadre found everything it uses. Optional tools add more."))))
         (list (boxed-list)))
    (setf (gobject:property list :margin-start) 0 (gobject:property list :margin-end) 0)
    (labels ((install (button row)
               (gtk:widget-set-sensitive button nil)
               (gtk:button-set-label button "Installing…")
               (handler-case
                   (install-quicklisp
                    (lambda (ok)
                      (cond (ok (message "Quicklisp is installed; new Lisps load it")
                                (setf (gobject:property row :subtitle) (uiop:native-namestring (quicklisp-setup-file)))
                                (gtk:widget-set-visible button nil))
                            (t (message "Could not install Quicklisp: see the Output panel")
                               (gtk:button-set-label button "Try Again")
                               (gtk:widget-set-sensitive button t)))))
                 (editor-error (e)
                   (message "~a" (editor-error-message e))
                   (gtk:widget-set-sensitive button t)))))
      (loop for (key name importance what-for) in *setup-tools*
            do (gtk:list-box-append list (setup-row key name importance what-for (cdr (assoc key results))
                                                    (and (getf (cdr (assoc :sbcl results)) :found) #'install)))))
    (adw:alert-dialog-set-extra-child dialog list)
    (adw:alert-dialog-add-response dialog "check" "Check _Again")
    (adw:alert-dialog-add-response dialog "close" "_Close")
    (adw:alert-dialog-set-default-response dialog "close")
    (adw:alert-dialog-set-close-response dialog "close")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (when (string= response "check") (check-setup))))
    dialog))

(define-command check-setup ()
  "Look for SBCL, Quicklisp, Git, Claude Code and the optional tools, and show how to install what's missing."
  (message "Checking setup…")
  (check-setup-in-background #'show-setup-dialog))

(defun check-setup-at-startup ()
  "On the first run, show what's missing; on later ones, only if SBCL is."
  (when *check-setup-at-startup*
    (let ((first-run (not (setting :setup-checked))))
      (check-setup-in-background
       (lambda (results)
         (unless (setting :setup-checked) (setf (setting :setup-checked) t))
         (when (if first-run
                   (setup-missing results)
                   (setup-missing results :importance '(:required)))
           (show-setup-dialog results)))))))
