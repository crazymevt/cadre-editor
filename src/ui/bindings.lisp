;;;; bindings.lisp — the keybinding profiles, and routing key presses to commands
;;;;
;;;; Each profile has a global keymap, active everywhere in the window, and
;;;; an editing keymap, active while an editor has the focus. The current
;;;; buffer's major mode keymap comes first while an editor has the focus.
;;;; Keys no keymap binds go to the focused widget as usual, so typing,
;;;; input methods and GtkTextView's own shortcuts keep working.

(in-package #:cadre-ui)

(defvar *standard-global-keymap* (make-keymap :standard-global))
(defvar *standard-editing-keymap* (make-keymap :standard-editing))
(defvar *emacs-global-keymap* (make-keymap :emacs-global))
(defvar *emacs-editing-keymap* (make-keymap :emacs-editing))

(defun bind-keys (keymap &rest pairs)
  (loop for (keys command) on pairs by #'cddr
        do (bind-key keymap keys command)))

;;; Standard: VS Code's keys. On macOS, Command works as Control.
(bind-keys *standard-global-keymap*
  "C-n" 'new-file
  "C-o" 'open-file
  "C-k C-o" 'open-folder
  "C-s" 'save-buffer
  "C-S-s" 'save-buffer-as
  "C-k s" 'save-all
  "C-w" 'close-tab
  "C-TAB" 'next-tab
  "C-S-TAB" 'previous-tab
  "C-Page_Down" 'next-tab
  "C-Page_Up" 'previous-tab
  "C-b" 'toggle-sidebar
  "C-j" 'toggle-panel
  "C-k C-l" 'toggle-layout
  "C-S-u" 'show-output
  "C-q" 'quit)

(bind-keys *standard-editing-keymap*
  "C-z" 'undo
  "C-S-z" 'redo
  "C-y" 'redo
  "ESC" 'keyboard-quit)

;;; Emacs
(bind-keys *emacs-global-keymap*
  "C-x C-f" 'open-file
  "C-x d" 'open-folder
  "C-x C-s" 'save-buffer
  "C-x C-w" 'save-buffer-as
  "C-x s" 'save-all
  "C-x k" 'close-tab
  "C-x Right" 'next-tab
  "C-x Left" 'previous-tab
  "C-TAB" 'next-tab
  "C-S-TAB" 'previous-tab
  "C-x t s" 'toggle-sidebar
  "C-x t p" 'toggle-panel
  "C-c C-z" 'toggle-panel
  "C-x t l" 'toggle-layout
  "C-x C-c" 'quit
  "C-g" 'keyboard-quit)

(bind-keys *emacs-editing-keymap*
  "C-f" 'forward-char
  "C-b" 'backward-char
  "C-n" 'next-line
  "C-p" 'previous-line
  "M-f" 'forward-word
  "M-b" 'backward-word
  "C-a" 'beginning-of-line
  "C-e" 'end-of-line
  "C-v" 'scroll-down-page
  "M-v" 'scroll-up-page
  "M-<" 'beginning-of-buffer
  "M->" 'end-of-buffer
  "C-SPC" 'set-mark
  "C-d" 'delete-char
  "C-k" 'kill-line
  "C-w" 'cut
  "M-w" 'copy
  "C-y" 'paste
  "C-/" 'undo
  "C-_" 'undo
  "C-x u" 'undo
  "C-?" 'redo
  "C-M-_" 'redo)

(defun profile-keymaps (profile)
  (ecase profile
    ((:standard nil) (values *standard-global-keymap* *standard-editing-keymap*))
    (:emacs (values *emacs-global-keymap* *emacs-editing-keymap*))))

(defun editor-focused-p (win)
  (let ((view (selected-view win))
        (focus (gtk:root-get-focus (window-gtk-window win))))
    (and view focus (eq focus (view-text-view view)))))

(defun active-keymaps (win)
  "The keymaps that apply now, most important first."
  (multiple-value-bind (global editing) (profile-keymaps *keybinding-profile*)
    (if (editor-focused-p win)
        (list (major-mode-keymap (find-major-mode (buffer-major-mode (view-buffer (selected-view win)))))
              editing global)
        (list global))))

(defun set-keybinding-profile (profile)
  (setf *keybinding-profile* profile
        (setting :keybinding-profile) profile)
  (message "Keyboard shortcuts: ~:[Standard~;Emacs~]" (eq profile :emacs)))

(defun show-pending-keys (win keys)
  (gtk:label-set-text (window-status-keys win) (if keys (format nil "~a –" (keys-string keys)) "")))

(defun base-keyval (keycode state)
  "The keyval KEYCODE gives without Alt: on macOS, Option+f types ƒ, but
Emacs keys want M-f."
  (multiple-value-bind (ok keyval)
      (gdk:display-translate-key (gdk:display-get-default) keycode
                                 (remove :alt-mask (modifier-list state)) 0)
    (and ok keyval)))

(defun handle-key (win keyval state &optional keycode)
  "Route a key press to a command. Returns t if Cadre used the key."
  (when (and keycode (macos-p) (eq *keybinding-profile* :emacs)
             (member :alt-mask (modifier-list state)))
    (setf keyval (or (base-keyval keycode state) keyval)))
  (let ((key (event-key keyval state
                        :super-as-control (and (macos-p) (not (eq *keybinding-profile* :emacs)))))
        (dispatcher (window-dispatcher win)))
    (when (and key
               (or (dispatcher-pending dispatcher) (not (plain-key-p key))
                   (string= key "ESC")))
      (multiple-value-bind (action keys command)
          (dispatch-key dispatcher key (active-keymaps win))
        (ecase action
          (:prefix (show-pending-keys win keys) t)
          (:command (show-pending-keys win nil) (call-command command) t)
          (:undefined (show-pending-keys win nil)
           (message "~a is undefined" (keys-string keys)) t)
          (:unbound nil))))))

(defun setup-keys (win)
  (let ((controller (gtk:event-controller-key-new)))
    (gtk:event-controller-set-propagation-phase controller :capture)
    (gobject:connect controller :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore c))
                       (handler-case (handle-key win keyval state keycode)
                         (error (e) (message "Key error: ~a" e) nil))))
    (gtk:widget-add-controller (window-gtk-window win) controller)))
