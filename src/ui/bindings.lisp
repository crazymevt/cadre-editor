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

(defvar *mode-profile-keymaps* (make-hash-table :test 'equal)
  "(mode . profile) → the keymap of that mode's keys in that profile.")

(defun mode-profile-keymap (mode profile)
  "The keymap for MODE's keys that differ between profiles."
  (let ((key (cons mode profile)))
    (or (gethash key *mode-profile-keymaps*)
        (setf (gethash key *mode-profile-keymaps*) (make-keymap (list mode profile))))))

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
  "C-S-p" 'execute-command
  "F1" 'execute-command
  "C-p" 'quick-open
  "C-f" 'find-text
  "F3" 'find-next
  "S-F3" 'find-previous
  "C-g" 'go-to-line
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
  "M-x" 'execute-command
  "C-x b" 'switch-to-buffer
  "C-x p f" 'quick-open
  "C-s" 'find-text
  "C-r" 'find-previous
  "M-g g" 'go-to-line
  "M-g M-g" 'go-to-line
  "C-g" 'keyboard-quit)

;;; Lisp mode, in both profiles
(bind-keys (major-mode-keymap (find-major-mode 'lisp-mode))
  "C-M-f" 'forward-sexp
  "C-M-b" 'backward-sexp
  "C-M-u" 'backward-up-list
  "C-M-d" 'down-list
  "C-M-a" 'beginning-of-defun
  "C-M-e" 'end-of-defun
  "C-M-SPC" 'mark-sexp
  "C-M-q" 'indent-defun
  "C-M-\\" 'indent-region
  "TAB" 'indent-line
  "RET" 'newline-and-indent)

;;; Talking to the Lisp. Emacs keys follow SLIME/SLY. Standard keys avoid
;;; Ctrl+C and Ctrl+X prefixes, which would take over copying and cutting.
(bind-keys (mode-profile-keymap 'lisp-mode :emacs)
  "C-c C-c" 'compile-defun
  "C-M-x" 'eval-defun
  "C-x C-e" 'eval-last-expression
  "C-c C-r" 'eval-region
  "C-c C-k" 'compile-and-load-file
  "C-c C-l" 'load-file
  "C-c C-z" 'show-repl
  "M-." 'edit-definition
  "M-," 'pop-definition
  "C-c C-d d" 'describe-symbol
  "C-c C-d C-d" 'describe-symbol
  "C-M-i" 'complete-symbol
  "M-TAB" 'complete-symbol
  "M-n" 'next-note
  "M-p" 'previous-note
  "C-c C-b" 'interrupt-lisp)

(bind-keys (mode-profile-keymap 'lisp-mode :standard)
  "C-RET" 'compile-or-eval-defun
  "C-S-RET" 'eval-expression-or-region
  "F5" 'compile-and-load-file
  "F12" 'edit-definition
  "C-M--" 'pop-definition
  "C-k C-i" 'describe-symbol
  "C-SPC" 'complete-symbol
  "F8" 'next-note
  "S-F8" 'previous-note)

(bind-keys (major-mode-keymap (find-major-mode 'repl-mode))
  "RET" 'repl-return
  "M-p" 'repl-previous-input
  "M-n" 'repl-next-input
  "C-Up" 'repl-previous-input
  "C-Down" 'repl-next-input
  "TAB" 'complete-symbol)

(bind-keys (mode-profile-keymap 'repl-mode :emacs)
  "C-c C-c" 'interrupt-lisp
  "C-c M-o" 'clear-repl
  "C-M-i" 'complete-symbol)

(bind-keys (mode-profile-keymap 'repl-mode :standard)
  "C-l" 'clear-repl
  "C-SPC" 'complete-symbol)

(bind-keys *standard-global-keymap*
  "C-`" 'show-repl
  "F6" 'load-project)

(bind-keys *emacs-global-keymap*
  "C-c L" 'load-project)

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

(defun focused-view (win)
  "The view (a tab's, or the REPL's) whose text has the keyboard focus, or nil."
  (let ((focus (gtk:root-get-focus (window-gtk-window win))))
    (and focus
         (or (loop for view being the hash-values of (window-views win)
                   when (eq focus (view-text-view view)) return view)
             (let ((repl (repl-view)))
               (and repl (eq focus (view-text-view repl)) repl))))))

(defun editor-focused-p (win)
  (and (focused-view win) t))

(defun active-keymaps (win)
  "The keymaps that apply now, most important first."
  (multiple-value-bind (global editing) (profile-keymaps *keybinding-profile*)
    (let ((view (focused-view win)))
      (if view
          (let ((mode (buffer-major-mode (view-buffer view))))
            (list (mode-profile-keymap mode (or *keybinding-profile* :standard))
                  (major-mode-keymap (find-major-mode mode))
                  editing global))
          (list global)))))

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
  (when (and (null (dispatcher-pending (window-dispatcher win))) (completion-key keyval))
    (return-from handle-key t))
  (let ((mods (modifier-list state)))
    ;; On macOS, Option changes the character typed; for Meta and for
    ;; chords like Ctrl+Option+F, use the key's character without it.
    (when (and keycode (macos-p) (member :alt-mask mods)
               (or (eq *keybinding-profile* :emacs)
                   (member :control-mask mods) (member :super-mask mods) (member :meta-mask mods)))
      (setf keyval (or (base-keyval keycode state) keyval))))
  (let ((key (event-key keyval state
                        :super-as-control (and (macos-p) (not (eq *keybinding-profile* :emacs)))))
        (dispatcher (window-dispatcher win)))
    (when key
      (multiple-value-bind (action keys command)
          (dispatch-key dispatcher key
                        (if (or (dispatcher-pending dispatcher) (not (plain-key-p key)) (string= key "ESC"))
                            (active-keymaps win)
                            ;; Plain keys (typing) only go to the major mode's
                            ;; keymaps, and only in an editor: RET and TAB in Lisp.
                            (and (editor-focused-p win) (subseq (active-keymaps win) 0 2))))
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
