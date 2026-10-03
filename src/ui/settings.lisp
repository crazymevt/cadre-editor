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
not chosen yet; Cadre asks on first run."
  :category "Keyboard")

(define-option *layout* :horizontal (member :horizontal :vertical :auto)
  "Where the panel goes: :horizontal puts it below the editor, :vertical
beside it, and :auto chooses by window width (see *auto-vertical-min-width*)."
  :category "Appearance")

(define-option *auto-vertical-min-width* 1600 (integer 0)
  "With *layout* :auto, windows at least this wide (in pixels) use the vertical layout."
  :category "Appearance")

(define-option *editor-font* "Menlo, Monaco, 'DejaVu Sans Mono', monospace 12pt" string
  "The editor's font, as a CSS font-family list followed by a size."
  :category "Appearance")

(define-option *explorer-hidden-names* '(".git" ".DS_Store" ".hg" ".svn") list
  "File and folder names the explorer leaves out."
  :category "Explorer")

(define-option *explorer-hidden-types* '("fasl" "dx64fsl" "ufasl" "fas" "lx64fsl") list
  "File types (extensions) the explorer leaves out."
  :category "Explorer")

(define-option *highlight-from-image* t boolean
  "Color symbols by what the connected Lisp knows: macros, special variables,
constants, and calls to functions that are not defined."
  :category "Lisp")

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
    (when (member layout '(:horizontal :vertical :auto)) (setf *layout* layout))
    (setf *paredit* (and (getf *settings* :paredit) t))))

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

;;; Options changed in the settings page are saved under :options, by
;;; name, and applied after init.lisp loads (the page's choice wins).

(defun option-key (name)
  (format nil "~a::~a" (package-name (symbol-package name)) (symbol-name name)))

(defun save-option (name)
  "Remember option NAME's current value in settings.sexp."
  (let ((options (copy-list (setting :options))))
    (setf (getf options (option-key name)) (symbol-value name))
    ;; GETF on string keys needs EQUAL: rebuild without duplicates.
    (setf (setting :options)
          (loop with seen = '()
                for (k v) on (append (list (option-key name) (symbol-value name)) options) by #'cddr
                unless (member k seen :test #'equal)
                  do (push k seen) and append (list k v)))))

(defun forget-option (name)
  (setf (setting :options)
        (loop for (k v) on (setting :options) by #'cddr
              unless (equal k (option-key name)) append (list k v))))

(defun saved-option-value (name)
  "The value saved for option NAME, and whether there is one."
  (loop for (k v) on (setting :options) by #'cddr
        when (equal k (option-key name)) return (values v t)
        finally (return (values nil nil))))

(defun apply-saved-options ()
  "Set the options saved from the settings page."
  (loop for (key value) on (setting :options) by #'cddr
        do (let* ((colons (search "::" key))
                  (symbol (and colons (find-package (subseq key 0 colons))
                               (find-symbol (subseq key (+ colons 2)) (subseq key 0 colons))))
                  (option (and symbol (find-option symbol))))
             (when (and option (valid-option-value-p option value))
               (setf (symbol-value symbol) value)))))
