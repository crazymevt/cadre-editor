;;;; keymaps.lisp — keys, keymaps and multi-key sequences
;;;;
;;;; Keys are written as in Emacs: modifiers C- (Control), M- (Meta/Alt),
;;;; s- (Super, Command on macOS) and S- (Shift), then a key name. A key name
;;;; is a character ("a", "/", "?") or a named key: RET, TAB, SPC, ESC, DEL,
;;;; or a GDK name such as F5, Page_Down, Left, Home, Delete.
;;;;
;;;; Inside Cadre a key is its canonical string: modifiers in the order
;;;; C- M- s- S-, letters in lower case with S- for Shift. "C-S-p", "C-x",
;;;; "M-<" and "C-Page_Down" are canonical.

(in-package #:cadre)

;;; Keys

(defparameter *key-name-aliases*
  '(("return" . "RET") ("enter" . "RET") ("tab" . "TAB") ("space" . "SPC")
    ("escape" . "ESC") ("esc" . "ESC") ("backspace" . "DEL")
    ("ret" . "RET") ("spc" . "SPC") ("del" . "DEL"))
  "Other spellings of the special key names, in lower case.")

(defun make-key (name &key control meta super shift)
  "The canonical key for NAME with the given modifiers."
  (let* ((name (if (and (> (length name) 2)
                        (char= (char name 0) #\<)
                        (char= (char name (1- (length name))) #\>))
                   (subseq name 1 (1- (length name)))
                   name))
         (alias (cdr (assoc name *key-name-aliases* :test #'string-equal))))
    (cond (alias (setf name alias))
          ((and (= (length name) 1) (upper-case-p (char name 0)))
           (setf name (string-downcase name) shift t))
          ((and (= (length name) 1) (not (alpha-char-p (char name 0))))
           ;; A punctuation character already includes Shift's effect ("?").
           (setf shift nil)))
    (format nil "~:[~;C-~]~:[~;M-~]~:[~;s-~]~:[~;S-~]~a" control meta super shift name)))

(defun canonical-key (string)
  "The canonical form of one key written as STRING, such as \"C-S-p\" or \"C-P\"."
  (let ((control nil) (meta nil) (super nil) (shift nil) (rest string))
    (loop while (and (> (length rest) 2) (char= (char rest 1) #\-))
          do (ecase (char rest 0)
               (#\C (setf control t))
               (#\M (setf meta t))
               (#\s (setf super t))
               (#\S (setf shift t)))
             (setf rest (subseq rest 2)))
    (make-key rest :control control :meta meta :super super :shift shift)))

(defun parse-keys (keys)
  "A key sequence as a list of canonical keys. KEYS is a string of keys
separated by spaces, such as \"C-x C-f\", or a list of key strings."
  (mapcar #'canonical-key
          (if (stringp keys)
              (loop with start = 0
                    for space = (position #\Space keys :start start)
                    for token = (subseq keys start space)
                    unless (string= token "") collect token
                    while space do (setf start (1+ space)))
              keys)))

(defun keys-string (keys)
  "KEYS, a list of canonical keys, written as one string."
  (format nil "~{~a~^ ~}" keys))

;;; Keymaps

(defclass keymap ()
  ((name :initarg :name :initform nil :reader keymap-name)
   (bindings :initform (make-hash-table :test 'equal) :reader keymap-bindings)
   (parent :initarg :parent :initform nil :accessor keymap-parent
           :documentation "A keymap consulted for keys this one does not bind.")))

(defmethod print-object ((keymap keymap) stream)
  (print-unreadable-object (keymap stream :type t :identity t)
    (format stream "~@[~a ~](~d)" (keymap-name keymap)
            (hash-table-count (keymap-bindings keymap)))))

(defun make-keymap (&optional name parent)
  (make-instance 'keymap :name name :parent parent))

(defun bind-key (keymap keys command)
  "Bind the key sequence KEYS (see parse-keys) in KEYMAP to COMMAND, a symbol.
The keys before the last become prefix keys."
  (let ((keys (parse-keys keys))
        (map keymap))
    (loop for (key . more) on keys
          do (if more
                 (let ((next (gethash key (keymap-bindings map))))
                   (unless (typep next 'keymap)
                     (setf next (make-keymap)
                           (gethash key (keymap-bindings map)) next))
                   (setf map next))
                 (setf (gethash key (keymap-bindings map)) command)))
    command))

(defun unbind-key (keymap keys)
  "Remove KEYMAP's binding for KEYS."
  (let* ((keys (parse-keys keys))
         (prefix (butlast keys))
         (map (if prefix (keymap-lookup keymap prefix) keymap)))
    (when (typep map 'keymap)
      (remhash (car (last keys)) (keymap-bindings map)))))

(defun keymap-lookup (keymap keys)
  "What KEYMAP binds the key sequence KEYS (canonical keys) to: a command
symbol, a keymap (KEYS is a prefix), or nil."
  (let ((binding (loop with map = keymap
                       for (key . more) on keys
                       for b = (gethash key (keymap-bindings map))
                       do (cond ((null more) (return b))
                                ((typep b 'keymap) (setf map b))
                                (t (return nil))))))
    (or binding
        (and (keymap-parent keymap) (keymap-lookup (keymap-parent keymap) keys)))))

(defun lookup-keys (keymaps keys)
  "The first binding for KEYS among KEYMAPS, most important first."
  (some (lambda (map) (keymap-lookup map keys)) keymaps))

(defun where-is (command keymaps)
  "The key sequences (as strings) bound to COMMAND in KEYMAPS, shortest first."
  (let ((found '()))
    (labels ((walk (map prefix)
               (maphash (lambda (key binding)
                          (let ((keys (append prefix (list key))))
                            (cond ((typep binding 'keymap) (walk binding keys))
                                  ((eq binding command) (push (keys-string keys) found)))))
                        (keymap-bindings map))
               (when (keymap-parent map) (walk (keymap-parent map) prefix))))
      (dolist (map keymaps) (walk map '())))
    (sort (remove-duplicates found :test #'string=) #'< :key #'length)))

;;; Reading key sequences one key at a time

(defstruct (key-dispatcher (:conc-name dispatcher-))
  (pending '()))                    ; the prefix keys typed so far

(defun reset-dispatcher (dispatcher)
  (setf (dispatcher-pending dispatcher) '()))

(defun dispatch-key (dispatcher key keymaps)
  "Feed KEY (a canonical key) to DISPATCHER. Returns an action, the key
sequence typed so far and, for :command, the command:
  :command          — the sequence is bound to a command (the third value)
  :prefix           — the sequence is a prefix; wait for more keys
  :undefined        — a prefix followed by an unbound key
  :unbound          — a single key no keymap binds; let the widget have it"
  (let* ((sequence (append (dispatcher-pending dispatcher) (list key)))
         (binding (lookup-keys keymaps sequence)))
    (cond ((typep binding 'keymap)
           (setf (dispatcher-pending dispatcher) sequence)
           (values :prefix sequence))
          (binding
           (reset-dispatcher dispatcher)
           (values :command sequence binding))
          ((rest sequence)
           (reset-dispatcher dispatcher)
           (values :undefined sequence))
          (t (values :unbound sequence)))))
