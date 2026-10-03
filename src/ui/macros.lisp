;;;; macros.lisp — keyboard macros, and help about keys and commands
;;;;
;;;; A keyboard macro is the keys typed between C-x ( and C-x ) (or F3 and
;;;; F4). C-x e (or F4) types them again; e right after repeats. Keys are
;;;; replayed through the same path as typing (PROCESS-KEY), so they run
;;;; the same commands. Keys nothing binds, which GTK would have handled
;;;; (typing, arrows, Backspace), are typed into the focused widget here.

(in-package #:cadre-ui)

(defvar *macro-recording-p* nil "True while a keyboard macro is being defined.")
(defvar *macro-keys* '() "The keys of the macro being defined, newest first, as (key . text).")
(defvar *last-macro* nil "The last keyboard macro defined, as a list of (key . text).")
(defvar *macro-replaying* nil "True while a keyboard macro runs.")

(defun record-macro-key (key text)
  (when (and *macro-recording-p* (not *macro-replaying*))
    (push (cons key text) *macro-keys*)))

(defun show-macro-state ()
  (when *window*
    (gtk:widget-set-visible (window-status-macro *window*) *macro-recording-p*)))

(define-command start-kbd-macro ()
  "Start defining a keyboard macro: the keys typed until C-x ) (or F4)."
  (when *macro-recording-p* (editor-error "Already defining a keyboard macro"))
  (setf *macro-recording-p* t *macro-keys* '())
  (show-macro-state)
  (message "Defining keyboard macro…"))

(define-command end-kbd-macro ()
  "Finish defining the keyboard macro."
  (unless *macro-recording-p* (editor-error "Not defining a keyboard macro"))
  ;; Leave out the keys that ran this command.
  (setf *last-macro* (reverse (nthcdr (length *this-command-keys*) *macro-keys*))
        *macro-recording-p* nil
        *macro-keys* '())
  (show-macro-state)
  (message "Keyboard macro defined: ~d key~:p" (length *last-macro*)))

(defun cancel-kbd-macro ()
  (when *macro-recording-p*
    (setf *macro-recording-p* nil *macro-keys* '())
    (show-macro-state)
    (message "Keyboard macro cancelled")))

(defun replay-macro (macro times)
  (let ((*macro-replaying* t))
    (dotimes (i times)
      (loop for (key . text) in macro
            do (process-key *window* key text :replaying t)))))

(defun macro-repeat-key (key)
  "After C-x e: e runs the macro again."
  (cond ((string= key "e") (replay-macro *last-macro* 1) t)
        (t (setf *key-reader* nil) nil)))

(define-command call-last-kbd-macro ()
  "Run the last keyboard macro (N times with a numeric argument). Typing e
right after runs it again."
  (when *macro-recording-p* (editor-error "Can't run a keyboard macro while defining one"))
  (unless *last-macro* (editor-error "No keyboard macro defined"))
  (replay-macro *last-macro* (max 1 (prefix-numeric-value)))
  (when (and (not *macro-replaying*) (equal (last *this-command-keys*) '("e")))
    (setf *key-reader* 'macro-repeat-key)
    (message "Type e to run the macro again")))

(define-command start-or-end-kbd-macro ()
  "Start defining a keyboard macro, or end the one being defined (F3)."
  (if *macro-recording-p* (call-command 'end-kbd-macro) (call-command 'start-kbd-macro)))

(define-command end-or-call-kbd-macro ()
  "End the keyboard macro being defined, or run the last one (F4)."
  (if *macro-recording-p* (call-command 'end-kbd-macro) (call-command 'call-last-kbd-macro)))

;;; Typing a key the way GTK would have

(defparameter *cursor-keys*
  '(("Left" :logical-positions -1) ("Right" :logical-positions 1)
    ("Up" :display-lines -1) ("Down" :display-lines 1)
    ("Home" :paragraph-ends -1) ("End" :paragraph-ends 1)
    ("Page_Up" :pages -1) ("Page_Down" :pages 1)))

(defun type-into-focus (win key text)
  "Do what KEY does in the focused widget when Cadre doesn't bind it."
  (let ((focus (gtk:root-get-focus (window-gtk-window win))))
    (cond
      ((typep focus 'gtk:text-view)
       (let ((gtk-buffer (gtk:text-view-get-buffer focus))
             (motion (assoc key *cursor-keys* :test #'string=)))
         (cond (motion (gobject:emit focus :move-cursor (second motion) (third motion) nil))
               ((string= key "DEL") (gobject:emit focus :backspace))
               ((string= key "Delete") (gobject:emit focus :delete-from-cursor :chars 1))
               ((string= key "RET") (gtk:text-buffer-insert-interactive-at-cursor gtk-buffer (string #\Newline) -1 t))
               ((string= key "TAB") (gtk:text-buffer-insert-interactive-at-cursor gtk-buffer (string #\Tab) -1 t))
               ((string= key "SPC") (gtk:text-buffer-insert-interactive-at-cursor gtk-buffer " " -1 t))
               ((and text (plain-key-p key))
                (gtk:text-buffer-insert-interactive-at-cursor gtk-buffer text -1 t)))))
      ((typep focus 'gtk:editable)
       (let ((position (gtk:editable-get-position focus))
             (string (gtk:editable-get-text focus)))
         (cond ((string= key "RET") (gtk:widget-activate focus))
               ((string= key "DEL")
                (when (plusp position) (gtk:editable-delete-text focus (1- position) position)))
               ((string= key "Left") (gtk:editable-set-position focus (max 0 (1- position))))
               ((string= key "Right") (gtk:editable-set-position focus (1+ position)))
               ((and (or text (string= key "SPC")) (plain-key-p key))
                (let ((text (if (string= key "SPC") " " text)))
                  (gtk:editable-set-text focus (concatenate 'string (subseq string 0 position) text
                                                            (subseq string position)))
                  (gtk:editable-set-position focus (+ position (length text)))))))))))

;;; Help

(defun binding-description (keys command)
  (let ((c (find-command command)))
    (format nil "~a runs the command ~(~a~)~@[ (~a)~].~%~%~a"
            keys command (and c (command-title c))
            (or (and c (command-documentation c)) "Not documented."))))

(defun describe-key-reader ()
  "A key reader that collects one key sequence and describes its binding."
  (let ((dispatcher (make-key-dispatcher)))
    (lambda (key)
      (multiple-value-bind (action keys command) (dispatch-key dispatcher key (active-keymaps *window*))
        (case action
          (:prefix (show-pending-keys *window* keys))
          (t (setf *key-reader* nil)
           (show-pending-keys *window* nil)
           (if (eq action :command)
               (show-help (format nil "Key: ~a" (keys-string keys)) (binding-description (keys-string keys) command))
               (message "~a is undefined" (keys-string keys)))))
        t))))

(define-command describe-key ()
  "Show what the next key (or key sequence) typed does."
  (setf *key-reader* (describe-key-reader))
  (message "Describe key: type a key…"))

(defun command-help-text (command)
  (let ((keys (where-is (command-name command) (active-keymaps *window*))))
    (format nil "~(~a~) — ~a~%~%~a~%~%~:[Not bound to any key.~;Keys: ~:*~{~a~^, ~}~]~@[~%Applies in: ~(~{~a~^, ~}~)~]"
            (command-name command) (command-title command)
            (or (command-documentation command) "Not documented.")
            keys (command-modes command))))

(define-command describe-command ()
  "Choose a command and show its documentation and keys."
  (open-picker (window-picker *window*)
               :items (list-commands :all t)
               :label (lambda (c) (string-downcase (symbol-name (command-name c))))
               :detail #'command-title
               :placeholder "Describe command"
               :on-choose (lambda (c) (show-help (format nil "Command: ~(~a~)" (command-name c))
                                                 (command-help-text c)))))

(define-command where-is-command ()
  "Choose a command and show the keys that run it."
  (open-picker (window-picker *window*)
               :items (list-commands :all t)
               :label (lambda (c) (string-downcase (symbol-name (command-name c))))
               :detail #'command-title
               :placeholder "Where is command"
               :on-choose (lambda (c)
                            (let ((keys (where-is (command-name c) (active-keymaps *window*))))
                              (message "~(~a~) is ~:[not on any key~;on ~:*~{~a~^, ~}~]" (command-name c) keys)))))

(define-command describe-bindings ()
  "List every key binding that applies here."
  (let ((seen (make-hash-table :test 'equal))
        (lines '()))
    (labels ((walk (map prefix)
               (maphash (lambda (key binding)
                          (let ((keys (append prefix (list key))))
                            (cond ((typep binding 'keymap) (walk binding keys))
                                  ((not (gethash (keys-string keys) seen))
                                   (setf (gethash (keys-string keys) seen) t)
                                   (push (format nil "~22a ~(~a~)" (keys-string keys) binding) lines)))))
                        (cadre::keymap-bindings map))
               (when (cadre::keymap-parent map) (walk (cadre::keymap-parent map) prefix))))
      (dolist (map (active-keymaps *window*)) (walk map '())))
    (show-help "Key bindings" (format nil "~{~a~%~}" (sort lines #'string<)))))
