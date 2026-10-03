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
      (panel-set-page-child panel "references" (make-references-widget)))
    (update-connection-status)
    (let ((directory (initial-project project)))
      (when directory (open-project directory)))
    (dolist (file files) (open-file-path (pathname file)))
    (gtk:window-present (window-gtk-window win))
    (unless *keybinding-profile*
      (ask-keybinding-profile win))
    (message "Welcome to Cadre")))

(defun main (&key project files quit-after (init-file t))
  "Run Cadre. PROJECT is a folder to open (default: the last one); FILES
are files to open in tabs. QUIT-AFTER (seconds) quits automatically, for
tests. With INIT-FILE nil, init.lisp is not loaded (safe mode)."
  (load-settings)
  (when init-file (load-init-file))
  (adw:run-application *application-id*
                       (lambda (app) (activate app :project project :files files))
                       :flags '(:non-unique)
                       :quit-after quit-after))
