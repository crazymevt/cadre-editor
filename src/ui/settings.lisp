;;;; settings.lisp — options, the settings Cadre saves itself, and the init file
;;;;
;;;; Two files live in the config directory (~/.config/cadre/):
;;;;   settings.sexp  written by Cadre: choices made in the UI (a plist)
;;;;   init.lisp      written by you: Lisp loaded at startup in CADRE-USER

(in-package #:cadre-ui)

(defvar *window* nil
  "The Cadre window. There is one, for now.")

(define-option *keybinding-profile* nil (member nil :standard :emacs)
  "The keyboard shortcuts: :standard (VS Code style) or :emacs. Nil means
not chosen yet; Cadre asks on first run.")

(define-option *layout* :horizontal (member :horizontal :vertical :auto)
  "Where the panel goes: :horizontal puts it below the editor, :vertical
beside it, and :auto chooses by window width (see *auto-vertical-min-width*).")

(define-option *auto-vertical-min-width* 1600 (integer 0)
  "With *layout* :auto, windows at least this wide (in pixels) use the vertical layout.")

(define-option *editor-font* "Menlo, Monaco, 'DejaVu Sans Mono', monospace 12pt" string
  "The editor's font, as a CSS font-family list followed by a size.")

(define-option *explorer-hidden-names* '(".git" ".DS_Store" ".hg" ".svn") list
  "File and folder names the explorer leaves out.")

(define-option *explorer-hidden-types* '("fasl" "dx64fsl" "ufasl" "fas" "lx64fsl") list
  "File types (extensions) the explorer leaves out.")

(defun config-directory ()
  (let ((xdg (uiop:getenv "XDG_CONFIG_HOME")))
    (merge-pathnames "cadre/" (if (and xdg (plusp (length xdg)))
                                  (uiop:ensure-directory-pathname xdg)
                                  (merge-pathnames ".config/" (user-homedir-pathname))))))

(defun settings-file () (merge-pathnames "settings.sexp" (config-directory)))
(defun init-file () (merge-pathnames "init.lisp" (config-directory)))

(defvar *settings* nil "The saved settings, a plist.")

(defun load-settings ()
  (setf *settings*
        (or (ignore-errors
             (with-open-file (in (settings-file) :if-does-not-exist nil)
               (and in (with-standard-io-syntax
                         (let ((*read-eval* nil) (*package* (find-package :keyword)))
                           (read in nil nil))))))
            '()))
  ;; Saved choices become the options' values.
  (let ((profile (getf *settings* :keybinding-profile))
        (layout (getf *settings* :layout)))
    (when (member profile '(:standard :emacs)) (setf *keybinding-profile* profile))
    (when (member layout '(:horizontal :vertical :auto)) (setf *layout* layout))))

(defun setting (key &optional default)
  (getf *settings* key default))

(defun (setf setting) (value key)
  (setf (getf *settings* key) value)
  (handler-case
      (progn
        (ensure-directories-exist (settings-file))
        (with-open-file (out (settings-file) :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*package* (find-package :keyword)))
              (format out ";;; Written by Cadre. Your own settings belong in init.lisp.~%")
              (prin1 *settings* out)
              (terpri out)))))
    (error (e) (message "Could not save settings: ~a" e)))
  value)

(defun load-init-file ()
  "Load init.lisp, if there is one. Errors are reported, not fatal."
  (let ((file (init-file)))
    (when (probe-file file)
      (handler-case
          (let ((*package* (find-package :cadre-user)))
            (load file)
            t)
        (error (e)
          (format *error-output* "~&Error in ~a: ~a~%" file e)
          nil)))))
