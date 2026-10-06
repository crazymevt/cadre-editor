;;;; indent.lisp — Lisp indentation, in the style of SLIME's common-lisp-indent
;;;;
;;;; A line is indented by the list it is in:
;;;;   - data (quoted lists, vectors, lists headed by a list, keyword or
;;;;     number): under the first element;
;;;;   - forms whose head has an indentation spec N (WHEN 1, LET 1, DEFUN 2,
;;;;     and any DEF…, WITH-… or DO-… head): the first N arguments are
;;;;     "distinguished" and indented 4, the body 2;
;;;;   - other calls: under the first argument when it is on the head's line,
;;;;     otherwise 1 past the paren.
;;;; Local functions in FLET, LABELS and MACROLET indent like DEFUN.

(in-package #:cadre)

(defvar *indentation-specs* (make-hash-table :test 'equal)
  "Head symbol name (lower case, without package) → number of distinguished arguments.")

(defmacro define-indentation (name spec)
  "Indent forms headed by NAME (a symbol or string) with SPEC distinguished
arguments: they get 4 extra columns, the body after them 2."
  `(setf (gethash (string-downcase (string ',name)) *indentation-specs*) ,spec))

(loop for (name spec) in
      '((block 1) (case 1) (ccase 1) (ecase 1) (typecase 1) (ctypecase 1) (etypecase 1)
        (catch 1) (defun 2) (defmacro 2) (defmethod 2) (defgeneric 2) (defclass 2)
        (defstruct 1) (defpackage 1) (define-condition 2) (defsetf 2) (deftype 2)
        (defvar 1) (defparameter 1) (defconstant 1)
        (destructuring-bind 2) (multiple-value-bind 2) (do 2) (do* 2) (dolist 1) (dotimes 1)
        (do-symbols 1) (do-external-symbols 1) (do-all-symbols 1) (eval-when 1)
        (flet 1) (labels 1) (macrolet 1) (symbol-macrolet 1)
        (handler-bind 1) (handler-case 1) (restart-case 1) (restart-bind 1)
        (lambda 1) (let 1) (let* 1) (locally 0) (multiple-value-prog1 1) (prog1 1) (prog2 2)
        (progn 0) (progv 2) (return-from 1) (tagbody 0) (the 1) (unless 1) (when 1)
        (unwind-protect 1) (with-accessors 2) (with-slots 2) (with-open-file 1)
        (with-open-stream 1) (with-output-to-string 1) (with-input-from-string 1)
        (with-standard-io-syntax 0) (with-simple-restart 1) (with-hash-table-iterator 1)
        (with-package-iterator 1) (with-compilation-unit 1) (print-unreadable-object 1)
        (pprint-logical-block 1) (in-package 0))
      do (setf (gethash (string-downcase (symbol-name name)) *indentation-specs*) spec))

(defvar *learned-indentation* (make-hash-table :test 'equal)
  "Names whose spec came from the Lisp (learn-indentation), not the table above.")

(defun body-position (indent)
  "The number of arguments before &body, from INDENT as Swank reports it:
a number, or (with its indentation contrib) an Emacs spec such as (4 4 &body).
Nil for anything else."
  (cond ((integerp indent) indent)
        ((consp indent)
         (position-if (lambda (x)
                        (let ((name (cond ((stringp x) x)
                                          ((remote-symbol-p x) (remote-symbol-name x)))))
                          (and name (string= "&body" (symbol-base-name name)))))
                      indent))))

(defun learn-indentation (updates)
  "Use the indentation the Lisp reports for macros with &body: Swank's
:indentation-update sends (name indent packages) for each, where indent
gives the position of &body, or is nil when a macro no longer has one."
  (dolist (update updates)
    (when (and (consp update) (stringp (first update)) (consp (rest update)))
      (let ((name (string-downcase (first update)))
            (indent (body-position (second update))))
        (cond ((integerp indent)
               (setf (gethash name *indentation-specs*) indent
                     (gethash name *learned-indentation*) t))
              ((and (null indent) (gethash name *learned-indentation*))
               (remhash name *indentation-specs*)
               (remhash name *learned-indentation*)))))))

(defun symbol-base-name (string)
  "STRING without any package prefix, in lower case."
  (string-downcase (subseq string (1+ (or (position #\: string :from-end t) -1)))))

(defun indentation-spec (name)
  "The number of distinguished arguments for a form headed by NAME, or nil
for an ordinary call."
  (let ((base (symbol-base-name name)))
    (or (gethash base *indentation-specs*)
        (cond ((and (> (length base) 3) (string= "def" base :end2 3)) 2)
              ((and (> (length base) 5) (string= "with-" base :end2 5)) 1)
              ((and (> (length base) 3) (string= "do-" base :end2 3)) 1)))))

(defun list-elements (syntax open-line open-index &optional limit-line)
  "The elements of the list opened at (OPEN-LINE, OPEN-INDEX), as a list of
(line column token), stopping before LIMIT-LINE if given."
  (let* ((open (token-ref syntax open-line open-index))
         (inner (1+ (token-depth open)))
         (elements '())
         (prefix nil))                  ; (line column) of prefixes before the next element
    (multiple-value-bind (l i) (next-token syntax open-line open-index)
      (loop
        (when (or (null l) (and limit-line (>= l limit-line))) (return))
        (let ((tk (token-ref syntax l i)))
          (when (and (eq (token-type tk) :close) (< (token-depth tk) inner)) (return))
          (when (and (= (token-depth tk) inner)
                     (not (member (token-type tk) '(:close :comment :invalid)))
                     (not (continues-from-previous-line-p syntax l i)))
            (if (member (token-type tk) *prefix-token-types*)
                (unless prefix (setf prefix (list l (token-start tk))))
                (progn
                  (push (list (if prefix (first prefix) l) (if prefix (second prefix) (token-start tk)) tk)
                        elements)
                  (setf prefix nil)))))
        (multiple-value-setq (l i) (next-token syntax l i))))
    (nreverse elements)))

(defun open-place (syntax line column)
  "The token place of the open paren at (LINE, COLUMN)."
  (let ((i (position-if (lambda (tk) (and (eq (token-type tk) :open) (= (token-start tk) column)))
                        (line-tokens syntax line))))
    (and i (values line i))))

(defun quoted-open-p (syntax line index)
  "True if the open paren at (LINE, INDEX) is a vector, or quoted."
  (let ((tk (token-ref syntax line index)))
    (or (> (- (token-end tk) (token-start tk)) 1)        ; #(
        (multiple-value-bind (l i) (previous-token syntax line index)
          (and l (let ((p (token-ref syntax l i)))
                   (and (eq (token-type p) :quote) (= l line) (= (token-end p) (token-start tk))
                        (member (char (text-line-string (syntax-text syntax) l) (token-start p))
                                '(#\' #\`)))))))))

(defun token-text (syntax line token)
  (subseq (text-line-string (syntax-text syntax) line) (token-start token) (token-end token)))

(defun local-function-list-p (syntax line column)
  "True if the list opened at (LINE, COLUMN) defines a local function in an
FLET, LABELS or MACROLET."
  (multiple-value-bind (bl bc) (up-list-position syntax line column)
    (when bl
      (multiple-value-bind (fl fc) (up-list-position syntax bl bc)
        (when fl
          (multiple-value-bind (ol oi) (open-place syntax fl fc)
            (let ((elements (list-elements syntax ol oi)))
              (and (>= (length elements) 2)
                   (eq (token-type (third (first elements))) :symbol)
                   (member (symbol-base-name (token-text syntax (first (first elements))
                                                         (third (first elements))))
                           '("flet" "labels" "macrolet") :test #'string=)
                   (destructuring-bind (l c tk) (second elements)
                     (declare (ignore tk))
                     (and (= l bl) (= c bc)))))))))))

(defun lisp-indentation (syntax line)
  "The column LINE should be indented to, or nil if it should be left alone
(it starts inside a string or comment)."
  (let ((state (line-start-state syntax line)))
    (cond
      ((not (eq (state-mode state) :code)) nil)
      ((zerop (state-depth state)) 0)
      (t
       (multiple-value-bind (ol oc) (up-list-position syntax line 0)
         (if (null ol)
             0
             (multiple-value-bind (ol oi) (open-place syntax ol oc)
               (let* ((open (token-ref syntax ol oi))
                      (paren (1- (token-end open)))
                      (elements (list-elements syntax ol oi line))
                      (head (first elements)))
                 (flet ((under-first ()
                          (if (and head (= (first head) ol)) (second head) (1+ paren))))
                   (cond
                     ((or (null head) (quoted-open-p syntax ol oi)
                          (not (eq (token-type (third head)) :symbol)))
                      (under-first))
                     (t
                      (let ((spec (if (local-function-list-p syntax ol oc)
                                      1
                                      (indentation-spec (token-text syntax (first head) (third head)))))
                            (arguments (1- (length elements))))
                        (cond
                          (spec (if (< arguments spec) (+ paren 4) (+ paren 2)))
                          ((and (second elements) (= (first (second elements)) (first head)))
                           (second (second elements)))
                          (t (1+ paren)))))))))))))))
