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
  "C-h" 'find-replace
  "C-\\" 'split-editor
  "C-1" 'focus-first-group
  "C-2" 'focus-second-group
  "C-3" 'focus-third-group
  "C-k C-\\" 'split-below
  "C-k w" 'delete-group
  "C-k C-w" 'delete-other-groups
  "C-M-Right" 'move-tab-to-next-group
  "C-S-r" 'editor-repl
  "C-k C-k" 'describe-key
  "C-," 'settings
  "C-S-f" 'find-in-project
  "C-S-h" 'replace-in-project
  "F2" 'rename-symbol
  "C-k C-t" 'choose-theme
  "C-q" 'quit)

(bind-keys *standard-editing-keymap*
  "C-/" 'toggle-comment
  "C-S-k" 'kill-whole-line
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
  "C-s" 'isearch-forward
  "C-r" 'isearch-backward
  "M-%" 'query-replace
  "C-u" 'universal-argument
  "M--" 'negative-argument
  "M-0" 'digit-argument "M-1" 'digit-argument "M-2" 'digit-argument "M-3" 'digit-argument
  "M-4" 'digit-argument "M-5" 'digit-argument "M-6" 'digit-argument "M-7" 'digit-argument
  "M-8" 'digit-argument "M-9" 'digit-argument
  "C-x (" 'start-kbd-macro
  "C-x )" 'end-kbd-macro
  "C-x e" 'call-last-kbd-macro
  "F3" 'start-or-end-kbd-macro
  "F4" 'end-or-call-kbd-macro
  "C-x 0" 'delete-group
  "C-x 1" 'delete-other-groups
  "C-x 2" 'split-below
  "C-x 3" 'split-right
  "C-x o" 'other-group
  "C-x C-b" 'switch-to-buffer
  "M-:" 'eval-expression
  "C-h k" 'describe-key
  "C-h f" 'describe-command
  "C-h x" 'describe-command
  "C-h w" 'where-is-command
  "C-h b" 'describe-bindings
  "C-c R" 'editor-repl
  "C-c C-a g" 'claude-agent
  "C-c s" 'find-in-project
  "C-c S" 'replace-in-project
  "C-c r" 'rename-symbol
  "M-g n" 'next-note
  "M-g p" 'previous-note
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
  "C-c C-b" 'interrupt-lisp
  "C-c I" 'inspect-value
  "C-c C-m" 'expand-macro-once
  "C-c RET" 'expand-macro-once
  "C-c M-m" 'expand-macro-all
  "C-c C-w c" 'who-calls
  "C-c C-w w" 'list-callees
  "C-c C-w r" 'who-references
  "C-c C-w b" 'who-binds
  "C-c C-w s" 'who-sets
  "C-c C-w m" 'who-macroexpands
  "C-c C-w a" 'who-specializes
  "C-c <" 'list-callers
  "C-c >" 'list-callees
  "M-?" 'find-references
  "C-c C-x f" 'extract-function
  "C-c C-x v" 'extract-variable
  "C-c C-x s" 'find-symbol-in-project)

(bind-keys (mode-profile-keymap 'lisp-mode :standard)
  "C-RET" 'compile-or-eval-defun
  "C-S-RET" 'eval-expression-or-region
  "F5" 'compile-and-load-file
  "F12" 'edit-definition
  "C-M--" 'pop-definition
  "C-k C-i" 'describe-symbol
  "C-SPC" 'complete-symbol
  "F8" 'next-note
  "S-F8" 'previous-note
  "S-F12" 'find-references
  "C-k i" 'inspect-value
  "C-k C-m" 'expand-macro-once
  "C-k C-a" 'expand-macro-all
  "C-k C-f" 'find-symbol-in-project
  "C-k e f" 'extract-function
  "C-k e v" 'extract-variable)

(defparameter *emacs-structural-keys*
  '("C-)" slurp-forward "C-Right" slurp-forward "C-}" barf-forward "C-Left" barf-forward
    "C-(" slurp-backward "C-M-Left" slurp-backward "C-{" barf-backward "C-M-Right" barf-backward
    "M-r" raise-sexp "M-s" splice-sexp "M-Up" splice-sexp-killing-backward
    "M-Down" splice-sexp-killing-forward "M-(" wrap-round "M-S" split-sexp "M-J" join-sexps
    "C-M-k" kill-sexp "C-M-DEL" backward-kill-sexp))

(defparameter *standard-structural-keys*
  '("C-M-S-Right" slurp-forward "C-M-S-Left" barf-forward "C-M-S-Up" raise-sexp
    "C-M-S-Down" splice-sexp "C-M-S-9" wrap-round "C-M-S-k" kill-sexp))

(dolist (mode '(lisp-mode repl-mode editor-repl-mode))
  (apply #'bind-keys (mode-profile-keymap mode :emacs) *emacs-structural-keys*)
  (apply #'bind-keys (mode-profile-keymap mode :standard) *standard-structural-keys*))

;;; Paredit mode's typing keys
(bind-keys (minor-mode-keymap (find-minor-mode 'paredit-mode))
  "(" 'paredit-open-round
  ")" 'paredit-close-round
  "\"" 'paredit-doublequote
  "DEL" 'paredit-backward-delete
  "C-d" 'paredit-forward-delete
  "Delete" 'paredit-forward-delete)

(bind-keys (major-mode-keymap (find-major-mode 'repl-mode))
  "RET" 'repl-return
  "M-p" 'repl-previous-input
  "M-n" 'repl-next-input
  "C-Up" 'repl-previous-input
  "C-Down" 'repl-next-input
  "TAB" 'complete-symbol)

(bind-keys (mode-profile-keymap 'repl-mode :emacs)
  "C-c I" 'inspect-value
  "C-c C-c" 'interrupt-lisp
  "C-c M-o" 'clear-repl
  "C-M-i" 'complete-symbol)

(bind-keys (mode-profile-keymap 'repl-mode :standard)
  "C-k i" 'inspect-value
  "C-l" 'clear-repl
  "C-SPC" 'complete-symbol)

(bind-keys *standard-global-keymap*
  "C-M-i" 'claude
  "C-`" 'show-repl
  "F6" 'load-project
  "C-S-e" 'show-explorer)

(bind-keys *emacs-global-keymap*
  "C-c L" 'load-project
  "C-c C-a a" 'claude
  "C-c C-a p" 'ask-claude-about-problems)

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
  "C-w" 'kill-region
  "M-w" 'copy-region-as-kill
  "C-y" 'yank
  "M-y" 'yank-pop
  "M-d" 'kill-word
  "M-DEL" 'backward-kill-word
  "C-DEL" 'backward-kill-word
  "C-S-DEL" 'kill-whole-line
  "C-x C-x" 'exchange-point-and-mark
  "C-x h" 'mark-whole-buffer
  "C-o" 'open-line
  "C-t" 'transpose-chars
  "M-u" 'upcase-word
  "M-l" 'downcase-word
  "M-c" 'capitalize-word
  "C-x C-u" 'upcase-region
  "C-x C-l" 'downcase-region
  "M-\\" 'delete-horizontal-space
  "M-SPC" 'just-one-space
  "M-^" 'delete-indentation
  "M-m" 'back-to-indentation
  "C-l" 'recenter
  "M-;" 'comment-dwim
  "M-/" 'dabbrev-expand
  "M-%" 'query-replace
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

(defun view-keymaps (view)
  "The keymaps of VIEW's buffer that come before the profile's: the major
mode's keys for this profile, the minor modes', then the major mode's."
  (let* ((buffer (view-buffer view))
         (mode (buffer-major-mode buffer)))
    (append (list (mode-profile-keymap mode (or *keybinding-profile* :standard)))
            (buffer-minor-mode-keymaps buffer)
            (list (major-mode-keymap (find-major-mode mode))))))

(defun active-keymaps (win)
  "The keymaps that apply now, most important first."
  (multiple-value-bind (global editing) (profile-keymaps *keybinding-profile*)
    ;; During an incremental search, keys act as in the editor (ending the search).
    (let ((view (or (focused-view win) (and (isearch-active-p) (selected-view win)))))
      (if view
          (append (view-keymaps view) (list editing global))
          (list global)))))

(defun set-keybinding-profile (profile)
  (setf *keybinding-profile* profile
        (setting :keybinding-profile) profile)
  (save-option '*keybinding-profile*)
  (message "Keyboard shortcuts: ~:[Standard~;Emacs~]" (eq profile :emacs)))

;;; Prefix arguments (C-u, M-0 … M-9)

(defvar *pending-prefix-arg* nil "The prefix argument typed so far, for the next command.")
(defvar *prefix-collecting* nil
  ":universal just after C-u, :digits while digits follow it, else nil.")
(defvar *this-command-keys* '() "The keys that ran the current command.")

(defun prefix-arg-string (arg)
  (cond ((null arg) "")
        ((consp arg) (format nil "C-u~v@{ C-u~:*~}" (1- (round (log (car arg) 4))) nil))
        (t (format nil "C-u ~a" arg))))

(defun show-pending-keys (win keys)
  (gtk:label-set-text (window-status-keys win)
                      (string-trim " " (format nil "~a~@[ ~a –~]"
                                               (prefix-arg-string *pending-prefix-arg*)
                                               (and keys (keys-string keys))))))

(defun clear-prefix-arg ()
  (setf *pending-prefix-arg* nil *prefix-collecting* nil))

(defun collect-prefix-digit (key)
  "While a prefix argument is being typed, add KEY to it if it is a digit or
a minus sign. Returns t if it was."
  (when (and *prefix-collecting* (= (length key) 1))
    (let ((c (char key 0)))
      (cond ((digit-char-p c)
             (setf *pending-prefix-arg*
                   (cond ((eq *pending-prefix-arg* '-) (- (digit-char-p c)))
                         ((and (eq *prefix-collecting* :digits) (integerp *pending-prefix-arg*))
                          (+ (* 10 *pending-prefix-arg*) (if (minusp *pending-prefix-arg*)
                                                              (- (digit-char-p c))
                                                              (digit-char-p c))))
                         (t (digit-char-p c)))
                   *prefix-collecting* :digits)
             t)
            ((and (char= c #\-) (eq *prefix-collecting* :universal))
             (setf *pending-prefix-arg* '- *prefix-collecting* :digits)
             t)))))

(define-command universal-argument ()
  "Give the next command a numeric argument: C-u alone means 4, C-u C-u 16,
and digits after C-u give that number. Most editing commands repeat that many times."
  (setf *pending-prefix-arg* (if (consp *prefix-arg*) (list (* 4 (car *prefix-arg*))) '(4))
        *prefix-collecting* :universal))

(define-command digit-argument ()
  "Start or continue a numeric argument with the digit typed (M-0 … M-9)."
  (let* ((key (car (last *this-command-keys*)))
         (digit (digit-char-p (char key (1- (length key))))))
    (setf *pending-prefix-arg* (if (integerp *prefix-arg*) (+ (* 10 *prefix-arg*) digit) digit)
          *prefix-collecting* :digits)))

(define-command negative-argument ()
  "Start a negative numeric argument (M--)."
  (setf *pending-prefix-arg* '- *prefix-collecting* :digits))

(defparameter *prefix-commands* '(universal-argument digit-argument negative-argument)
  "Commands that build the prefix argument rather than use it.")

;;; Reading keys for a command (describe-key, query-replace)

(defvar *key-reader* nil
  "A function given each key before the keymaps. It returns t if it used
the key; nil lets the key go on as usual.")

;;; Processing a key

(defun base-keyval (keycode state)
  "The keyval KEYCODE gives without Alt: on macOS, Option+f types ƒ, but
Emacs keys want M-f."
  (multiple-value-bind (ok keyval)
      (gdk:display-translate-key (gdk:display-get-default) keycode
                                 (remove :alt-mask (modifier-list state)) 0)
    (and ok keyval)))

(defun keymaps-for-key (win key)
  (let ((dispatcher (window-dispatcher win)))
    (cond ((or (dispatcher-pending dispatcher) (not (plain-key-p key)) (string= key "ESC"))
           (active-keymaps win))
          ;; Plain keys (typing) only go to the buffer's own keymaps, and
          ;; only in an editor: RET and TAB in Lisp, ( and ) with paredit.
          ((focused-view win) (view-keymaps (focused-view win)))
          (t '()))))

(defun run-key-command (win command keys)
  "Run COMMAND, typed as KEYS, with the prefix argument typed before it."
  (let ((*prefix-arg* *pending-prefix-arg*)
        (*this-command-keys* keys))
    (unless (member command *prefix-commands*)
      (clear-prefix-arg))
    (when (and (isearch-active-p) (not (member command *isearch-commands*)))
      (isearch-exit))
    (call-command command)
    (show-pending-keys win nil)))

(defun process-key (win key text &key replaying)
  "Act on KEY (a canonical key) typed in WIN; TEXT is the character it
types, if any. Returns t if Cadre used the key, nil to let the focused
widget have it. When REPLAYING a keyboard macro, keys nothing binds are
typed into the focused widget here."
  (record-macro-key key text)
  (let ((dispatcher (window-dispatcher win)))
    (cond
      ((and *key-reader* (funcall *key-reader* key)) t)
      ((and (null (dispatcher-pending dispatcher)) (null *pending-prefix-arg*) (wrap-selection-key win key text)) t)
      ((and (null (dispatcher-pending dispatcher)) (collect-prefix-digit key))
       (show-pending-keys win nil)
       t)
      (t
       (multiple-value-bind (action keys command) (dispatch-key dispatcher key (keymaps-for-key win key))
         (ecase action
           (:prefix (show-pending-keys win keys) t)
           (:command (run-key-command win command keys) t)
           (:undefined (clear-prefix-arg) (show-pending-keys win nil)
            (message "~a is undefined" (keys-string keys)) t)
           (:unbound (unbound-key win key text :replaying replaying))))))))

(defun unbound-key (win key text &key replaying)
  "A key no keymap binds: typing. It ends a run of kills. With a prefix
argument, a printable key is typed that many times."
  (setf *last-command-kind* nil)
  (let ((count (and *pending-prefix-arg* (prefix-numeric-value *pending-prefix-arg*))))
    (clear-prefix-arg)
    (show-pending-keys win nil)
    (cond ((and count text (plain-key-p key))
           (dotimes (i (max 0 count)) (type-into-focus win key text))
           t)
          (replaying (type-into-focus win key text) t)
          (t nil))))

(defun handle-key (win keyval state &optional keycode)
  "Route a key press to a command. Returns t if Cadre used the key."
  (when (and (null (dispatcher-pending (window-dispatcher win))) (null *key-reader*)
             (completion-key keyval))
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
        (text (let ((code (gdk:keyval-to-unicode keyval)))
                (and (>= code 32) (/= code 127) (string (code-char code))))))
    (when key
      (process-key win key text))))

(defun setup-keys (win)
  (let ((controller (gtk:event-controller-key-new)))
    (gtk:event-controller-set-propagation-phase controller :capture)
    (gobject:connect controller :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore c))
                       (handler-case (handle-key win keyval state keycode)
                         (error (e) (message "Key error: ~a" e) nil))))
    (gtk:widget-add-controller (window-gtk-window win) controller)))
