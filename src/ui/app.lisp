;;;; app.lisp — starting Cadre

(in-package #:cadre-ui)

(defparameter *application-id* "io.github.crazymevt.Cadre")

(defun install-command-action (app)
  "app.command, with a command name as its string parameter, runs that
command. Menus and buttons use it."
  (let ((action (gio:simple-action-new "command" (glib:variant-type-new "s"))))
    (gobject:connect action :activate
                     (lambda (action parameter)
                       (declare (ignore action))
                       (let* ((name (glib:variant-get-string parameter))
                              (symbol (find-symbol (string-upcase name) :cadre-ui)))
                         (if (and symbol (find-command symbol))
                             (call-command symbol)
                             (message "Unknown command: ~a" name)))))
    (gio:action-map-add-action app action)))

(defun ask-keybinding-profile (win)
  "Ask which keyboard shortcuts to use (first run only)."
  (let ((dialog (adw:alert-dialog-new
                 "Choose Keyboard Shortcuts"
                 (format nil "Standard uses familiar shortcuts: Ctrl+S to save, Ctrl+O to open, Ctrl+W to close a tab~:[~;, with Command in place of Ctrl~].~%~%Emacs uses Emacs keys: C-x C-f to open, C-x C-s to save, C-f and C-b to move.~%~%You can change this later from the menu."
                         (macos-p)))))
    (adw:alert-dialog-add-response dialog "emacs" "_Emacs")
    (adw:alert-dialog-add-response dialog "standard" "_Standard")
    (adw:alert-dialog-set-response-appearance dialog "standard" :suggested)
    (adw:alert-dialog-set-default-response dialog "standard")
    (adw:alert-dialog-set-close-response dialog "standard")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window win))
               (lambda (response)
                 (set-keybinding-profile (if (string= response "emacs") :emacs :standard))))))

(defun initial-project (project)
  (let ((path (or project (setting :last-project))))
    (and path (probe-file path) (uiop:directory-exists-p path))))

(defun activate (app &key project files)
  (install-css)
  (install-icons)
  (install-command-action app)
  (setup-theme-following)
  (let ((win (make-cadre-window app)))
    (setf *window* win
          *frontend* win)
    (let ((panel (window-panel win)))
      (panel-set-page-child panel "repl" (make-repl-widget))
      (panel-set-page-child panel "problems" (make-problems-widget))
      (panel-set-page-child panel "debugger" (make-debugger-widget))
      (panel-set-page-child panel "inspector" (make-inspector-widget))
      (panel-set-page-child panel "references" (make-references-widget))
      (panel-set-page-child panel "history" (make-history-widget))
      (panel-set-page-child panel "trace" (make-trace-widget))
      (panel-set-page-child panel "claude" (make-claude-widget))
      (panel-set-page-child panel "terminal" (make-terminal-widget))
      ;; However the Claude tab is opened (its tab, a key, the menu), find out
      ;; whether Claude Code is ready.
      (gobject:connect (panel-stack panel) "notify::visible-child"
                       (lambda (stack pspec)
                         (declare (ignore pspec))
                         (let ((name (gtk:stack-get-visible-child-name stack)))
                           (cond ((equal name "claude") (check-claude-status))
                                 ((equal name "trace") (trace-page-shown))
                                 ((equal name "terminal") (terminal-page-shown)))))))
    (update-connection-status)
    (let ((directory (initial-project project)))
      (when directory (open-project directory)))
    (if files
        (dolist (file files) (open-file-path (pathname file)))
        (restore-session win))
    (setup-clipboard)
    (gtk:window-present (window-gtk-window win))
    (let ((view (selected-view win)))
      (when view (focus-view view)))
    (unless *keybinding-profile*
      (ask-keybinding-profile win))
    ;; In the background, so the Claude tab is ready when opened.
    (check-claude-status)
    (message "Welcome to Cadre")))

(defun main (&key project files quit-after (init-file t))
  "Run Cadre. PROJECT is a folder to open (default: the last one); FILES
are files to open in tabs. QUIT-AFTER (seconds) quits automatically, for
tests. With INIT-FILE nil, init.lisp is not loaded (safe mode)."
  (load-settings)
  (when init-file (load-init-file))
  (apply-saved-options)
  (unwind-protect
       (adw:run-application *application-id*
                            (lambda (app) (activate app :project project :files files))
                            :flags '(:non-unique)
                            :quit-after quit-after)
    (stop-claude-session)))

;;; The macOS app (scripts/build-app.lisp saves an image that starts here)

(defun bundle-resource-directory ()
  "Contents/Resources/cadre/ of the app bundle this executable is in, or nil."
  (let* ((exe (and sb-ext:*runtime-pathname* (probe-file sb-ext:*runtime-pathname*)))
         (resources (and exe (merge-pathnames "../Resources/cadre/" (uiop:pathname-directory-pathname exe)))))
    (and resources (probe-file resources))))

(defun app-arguments (arguments)
  "ARGUMENTS from the command line as a folder to open and files to open:
the first folder is the project; macOS's -psn_ argument is ignored."
  (let ((paths (loop for a in arguments
                     unless (uiop:string-prefix-p "-psn_" a)
                       collect (let ((p (probe-file a))) (or p (uiop:parse-native-namestring a))))))
    (values (find-if #'uiop:directory-pathname-p paths)
            (remove-if #'uiop:directory-pathname-p paths))))

(defun app-main ()
  "Start Cadre as an application: its files from the bundle, the login
shell's environment when started from the Finder or the Dock (which give
none), and a folder or files from the command line."
  (setf *random-state* (make-random-state t))
  (setf *resource-directory* (bundle-resource-directory))
  (unless (uiop:getenv "TERM")
    (adopt-login-shell-environment))
  (multiple-value-bind (project files) (app-arguments (rest sb-ext:*posix-argv*))
    (main :project project :files (mapcar #'namestring files)
          :quit-after (let ((q (uiop:getenv "CADRE_QUIT_AFTER"))) (and q (parse-integer q :junk-allowed t))))
    0))

