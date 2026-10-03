;;;; faces.lisp — what each token looks like
;;;;
;;;; TOKEN-FACE names a face for a token: the GUI maps faces to colors.
;;;; Symbols are classified lexically: definers (DEF…) and their names,
;;;; Common Lisp's macros and special operators, keywords, lambda-list
;;;; keywords, *special* variables and +constants+. Colors that depend on
;;;; what the running image knows come later (design doc 7.3).

(in-package #:cadre)

(defparameter *faces*
  '(:comment :string :number :character :keyword :builtin :definer :definition-name
    :special-variable :constant :lambda-keyword :reader-conditional :quote :invalid)
  "Every face TOKEN-FACE returns, apart from (:paren n).")

(defparameter *paren-face-count* 6
  "Rainbow parentheses cycle through this many colors.")

(defvar *cl-operator-cache* (make-hash-table :test 'equal))

(defun cl-operator-p (name)
  "True if NAME (lower case, no package) names a Common Lisp macro or special operator."
  (multiple-value-bind (value found) (gethash name *cl-operator-cache*)
    (if found
        value
        (setf (gethash name *cl-operator-cache*)
              (let ((symbol (find-symbol (string-upcase name) :common-lisp)))
                (and symbol (or (special-operator-p symbol) (macro-function symbol)) t))))))

(defun package-prefix (string)
  (let ((colon (position #\: string)))
    (and colon (plusp colon) (string-downcase (subseq string 0 colon)))))

(defun earmuffs-p (base)
  (and (> (length base) 2) (char= (char base 0) #\*) (char= (char base (1- (length base))) #\*)))

(defun symbol-face (text subtype)
  (let ((base (symbol-base-name text))
        (prefix (package-prefix text)))
    (cond
      ((eq subtype :feature) :reader-conditional)
      ((and (eq subtype :name) (earmuffs-p base)) :special-variable)   ; (defvar *x* …)
      ((eq subtype :name) :definition-name)
      ((and (eq subtype :head) (definer-name-p text)) :definer)
      ((and (eq subtype :head)
            (or (null prefix) (member prefix '("cl" "common-lisp") :test #'string=))
            (cl-operator-p base))
       :builtin)
      ((and (plusp (length base)) (char= (char base 0) #\&)) :lambda-keyword)
      ((earmuffs-p base) :special-variable)
      ((or (and (> (length base) 2) (char= (char base 0) #\+) (char= (char base (1- (length base))) #\+))
           (member base '("t" "nil") :test #'string=))
       :constant))))

(defun token-face (token line)
  "The face for TOKEN, which is on LINE (a string): a keyword from *faces*,
(:paren depth), or nil for the default look."
  (case (token-type token)
    ((:open :close) (list :paren (mod (token-depth token) *paren-face-count*)))
    (:symbol (symbol-face (subseq line (token-start token) (token-end token)) (token-subtype token)))
    ((:comment :string :number :character :keyword :reader-conditional :invalid :quote)
     (token-type token))
    (:reader :quote)))
