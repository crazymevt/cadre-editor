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
      (panel-set-page-child panel "profiler" (make-profiler-widget))
      (panel-set-page-child panel "threads" (make-threads-widget))
      (panel-set-page-child panel "tests" (make-tests-widget))
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
                                 ((equal name "threads") (threads-page-shown))
                                 ((equal name "tests") (tests-page-shown))
                                 ((equal name "terminal") (terminal-page-shown)))))))
    (update-connection-status)
    (let ((directory (initial-project project)))
      (when directory (open-project directory)))
    (if files
        (dolist (file files) (open-file-path (pathname file)))
        (restore-session win))
    (setup-clipboard)
    (gtk:window-present (window-gtk-window win))
    ;; GTK has read GSK_DEBUG now; the programs Cadre starts don't get it.
    (glib:idle-add glib:+priority-default+ (lambda () (restore-full-redraws) nil))
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
  (start-session-log)
  (use-full-redraws)
  (unwind-protect
       (adw:run-application *application-id*
                            (lambda (app) (activate app :project project :files files))
                            :flags '(:non-unique)
                            :quit-after quit-after)
    (stop-claude-session)
    (stop-session-log)
    ;; The Lisp Cadre started goes with it. It would exit when its standard
    ;; input closes, but not while a GTK app holds its first thread.
    (when (and *inferior* (inferior-alive-p *inferior*))
      (ignore-errors (kill-inferior-lisp *inferior*)))
    ;; So does the Clojure REPL it started (a JVM outlives its input).
    (when *clojure-process*
      (ignore-errors (stop-clojure *clojure-process*)))
    (when *racket-process*
      (ignore-errors (kill-inferior-lisp *racket-process*)))))

;;; On macOS, GTK repaints only what changed into a window buffer that,
;;; once the window is maximized (no rounded corners, so drawn in opaque
;;; tiles) and a dialog has dimmed it, can come back empty: everything but
;;; the dialog's tiles shows what's behind the window. Repainting every
;;; frame whole (GSK_DEBUG=full-redraw) avoids it, for a little more work
;;; on the GPU.

(defvar *gsk-debug-before* :unset "GSK_DEBUG before Cadre set it, to give back.")

(defun use-full-redraws ()
  (when (macos-p)
    (let ((old (uiop:getenv "GSK_DEBUG")))
      (setf *gsk-debug-before* old)
      (unless (and old (search "full-redraw" old))
        (sb-posix:setenv "GSK_DEBUG" (if (plusp (length old)) (format nil "~a,full-redraw" old) "full-redraw") 1)))))

(defun restore-full-redraws ()
  (unless (eq *gsk-debug-before* :unset)
    (if *gsk-debug-before*
        (sb-posix:setenv "GSK_DEBUG" *gsk-debug-before* 1)
        (sb-posix:unsetenv "GSK_DEBUG"))
    (setf *gsk-debug-before* :unset)))

;;; The macOS app (scripts/build-app.lisp saves an image that starts here)

(defun runtime-directory ()
  "The folder of the SBCL runtime running Cadre (Contents/MacOS/ in the app)."
  (let ((exe (and sb-ext:*runtime-pathname* (probe-file sb-ext:*runtime-pathname*))))
    (and exe (uiop:pathname-directory-pathname exe))))

(defun bundle-resource-directory ()
  "Cadre's own files in a package: Contents/Resources/cadre/ of the macOS
app, or share/cadre/ beside lib/cadre/ (the runtime's folder) on Linux. Nil
when running from a source checkout."
  (let ((dir (runtime-directory)))
    (when dir
      (some (lambda (relative) (probe-file (merge-pathnames relative dir)))
            '("../Resources/cadre/" "../../share/cadre/")))))

;;; GTK bundled in the macOS app (scripts/make-app.sh --bundle)

(defvar *bundle-environment* '()
  "The variables set to point GTK at the app's own copies, with the values
they had before, so the programs Cadre starts don't get them.")

(defun set-bundle-variable (name value)
  (push (cons name (uiop:getenv name)) *bundle-environment*)
  (sb-posix:setenv name value 1))

(defun use-bundled-gtk ()
  "When the app carries its own GTK (Contents/Frameworks/), keep it from
loading Homebrew's modules, which would bring a second copy of GLib into
the process: gdk-pixbuf's image loaders come from the app (their list,
written with the app's path, goes in the state folder) and GIO's modules
from nowhere. The gtk4 bindings already pointed XDG_DATA_DIRS and
GSETTINGS_SCHEMA_DIR at Contents/Resources/share/."
  (let* ((dir (runtime-directory))
         (frameworks (and dir (probe-file (merge-pathnames "../Frameworks/" dir))))
         (template (and dir (probe-file (merge-pathnames "../Resources/gdk-pixbuf-loaders.cache" dir)))))
    (when frameworks
      ;; What the bindings set: Resources/share/, before any earlier
      ;; XDG_DATA_DIRS.
      (let ((share (probe-file (merge-pathnames "../Resources/share/" dir))))
        (when share
          (dolist (name '("XDG_DATA_DIRS" "GSETTINGS_SCHEMA_DIR"))
            (let ((value (uiop:getenv name)))
              (when (and value (uiop:string-prefix-p (namestring share) value))
                (let ((colon (position #\: value)))
                  (push (cons name (and colon (string= name "XDG_DATA_DIRS") (subseq value (1+ colon))))
                        *bundle-environment*)))))))
      (set-bundle-variable "GIO_MODULE_DIR" (namestring (merge-pathnames "gio-modules/" frameworks)))
      (when template
        (let ((cache (merge-pathnames "gdk-pixbuf-loaders.cache" (state-directory)))
              (text (uiop:frob-substrings (uiop:read-file-string template) '("@FRAMEWORKS@")
                                          (string-right-trim "/" (namestring frameworks)))))
          (unless (and (probe-file cache) (string= text (uiop:read-file-string cache)))
            (ensure-directories-exist cache)
            (with-open-file (out cache :direction :output :if-exists :supersede)
              (write-string text out)))
          (set-bundle-variable "GDK_PIXBUF_MODULE_FILE" (namestring cache))))
      ;; Read them now: GLib keeps what it read, so they can be put back.
      (cffi:foreign-funcall "g_get_system_data_dirs" :pointer)
      (cffi:foreign-funcall "g_settings_schema_source_get_default" :pointer)
      (cffi:foreign-funcall "g_vfs_get_default" :pointer)
      (cffi:foreign-funcall "g_slist_free" :pointer (cffi:foreign-funcall "gdk_pixbuf_get_formats" :pointer) :void)
      (loop for (name . value) in *bundle-environment*
            do (if value (sb-posix:setenv name value 1) (sb-posix:unsetenv name))))))

(defun app-arguments (arguments)
  "ARGUMENTS from the command line as a folder to open and files to open:
the first folder is the project; macOS's -psn_ argument is ignored."
  (let ((paths (loop for a in arguments
                     unless (uiop:string-prefix-p "-psn_" a)
                       collect (let ((p (probe-file a))) (or p (uiop:parse-native-namestring a))))))
    (values (find-if #'uiop:directory-pathname-p paths)
            (remove-if #'uiop:directory-pathname-p paths))))

(defun log-file () (merge-pathnames "cadre.log" (state-directory)))

(defun redirect-output-to-log ()
  "Started from the Finder or the Dock, standard output and error go nowhere:
send them to ~/.local/state/cadre/cadre.log, so a crash (the Lisp runtime's
fatal errors included) leaves its message. The previous run's log is kept as
cadre.log.1."
  (handler-case
      (let* ((log (log-file))
             (previous (make-pathname :name "cadre.log" :type "1" :defaults log)))
        (ensure-directories-exist log)
        (when (probe-file log) (uiop:rename-file-overwriting-target log previous))
        (let ((fd (sb-posix:open (uiop:native-namestring log)
                                 (logior sb-posix:o-wronly sb-posix:o-creat sb-posix:o-append) #o644)))
          (finish-output *standard-output*)
          (finish-output *error-output*)
          (sb-posix:dup2 fd 1)
          (sb-posix:dup2 fd 2)
          (sb-posix:close fd)
          (format *error-output* "~&;; Cadre started ~a, pid ~d~%"
                  (multiple-value-bind (s m h d mo y) (get-decoded-time)
                    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y mo d h m s))
                  (sb-posix:getpid))
          (finish-output *error-output*)))
    (error () nil)))

(defun app-main ()
  "Start Cadre as an application: its files from the bundle, the login
shell's environment when started from the Finder or the Dock (which give
none), and a folder or files from the command line."
  (setf *random-state* (make-random-state t))
  (setf *resource-directory* (bundle-resource-directory))
  (use-bundled-gtk)
  ;; Started with macOS malloc debugging (to find a crash), it is already on
  ;; for Cadre; the programs it runs needn't log too, and the CLI's notices
  ;; about it would garble the output Cadre reads.
  (sb-posix:unsetenv "MallocStackLogging")
  (unless (uiop:getenv "TERM")
    (redirect-output-to-log)
    (adopt-login-shell-environment))
  (multiple-value-bind (project files) (app-arguments (rest sb-ext:*posix-argv*))
    (main :project project :files (mapcar #'namestring files)
          :quit-after (let ((q (uiop:getenv "CADRE_QUIT_AFTER"))) (and q (parse-integer q :junk-allowed t))))
    ;; So the log tells a quit from a crash.
    (format *error-output* "~&;; Cadre exited normally~%")
    (finish-output *error-output*)
    0))

