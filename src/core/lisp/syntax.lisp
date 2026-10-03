;;;; syntax.lisp — a buffer's Lisp syntax, kept up to date line by line
;;;;
;;;; A LISP-SYNTAX caches, for each line of a text, the lexer state at its
;;;; start and end and its tokens. When lines change, only they are marked
;;;; dirty; lexing resumes at the first dirty line and stops as soon as a
;;;; line ends in the state it ended in before, so typing re-lexes a line or
;;;; two even in a large file. Lexing is lazy: nothing past the last line
;;;; asked for is lexed.
;;;;
;;;; Positions here are (line, column) pairs, both from 0.

(in-package #:cadre)

(defstruct (line-info (:constructor make-line-info ()))
  (start-state nil)
  (end-state nil)
  (tokens #() :type simple-vector)
  (length 0)
  (dirty t)
  (highlighted nil))                    ; for the GUI: tags applied since the last lex

(defclass lisp-syntax ()
  ((text :initarg :text :reader syntax-text)
   (lines :reader syntax-lines)
   (first-unchecked :initform 0 :accessor syntax-first-unchecked
                    :documentation "The first line whose start state may be stale, or nil.")
   (max-dirty :initform -1 :accessor syntax-max-dirty
              :documentation "The last dirty line, or -1.")))

(defun make-lisp-syntax (text)
  "A syntax cache for TEXT, an object implementing the text protocol."
  (let ((syntax (make-instance 'lisp-syntax :text text)))
    (reset-syntax syntax)
    syntax))

(defun reset-syntax (syntax)
  "Forget everything and re-lex on demand."
  (let* ((n (text-line-count (syntax-text syntax)))
         (lines (make-array n :adjustable t :fill-pointer n)))
    (dotimes (i n) (setf (aref lines i) (make-line-info)))
    (setf (slot-value syntax 'lines) lines
          (syntax-first-unchecked syntax) 0
          (syntax-max-dirty syntax) (1- n))
    syntax))

(defun syntax-line-count (syntax)
  (length (syntax-lines syntax)))

(defun syntax-lines-changed (syntax first old-count new-count)
  "Note that lines FIRST to FIRST+OLD-COUNT-1 are replaced by NEW-COUNT lines."
  (let* ((lines (syntax-lines syntax))
         (n (length lines))
         (first (min first n))
         (old-count (min old-count (- n first)))
         (tail (subseq lines (+ first old-count)))
         (max-dirty (syntax-max-dirty syntax)))
    (setf (fill-pointer lines) first)
    (dotimes (i new-count) (vector-push-extend (make-line-info) lines))
    (loop for info across tail do (vector-push-extend info lines))
    (setf (syntax-first-unchecked syntax) (min first (or (syntax-first-unchecked syntax) first))
          (syntax-max-dirty syntax) (max (if (>= max-dirty (+ first old-count))
                                             (+ max-dirty (- new-count old-count))
                                             max-dirty)
                                         (+ first new-count -1)))
    syntax))

(defun ensure-lexed (syntax through)
  "Make sure lines up to THROUGH have current tokens."
  (let* ((lines (syntax-lines syntax))
         (n (length lines))
         (i (syntax-first-unchecked syntax)))
    (when (and i (>= i n))
      (setf (syntax-first-unchecked syntax) nil (syntax-max-dirty syntax) -1
            i nil))
    (when (and i (<= i through))
      (let ((previous (if (zerop i) +initial-state+ (line-info-end-state (aref lines (1- i))))))
        (loop
          (let ((info (aref lines i)))
            (if (and (not (line-info-dirty info)) (equal (line-info-start-state info) previous))
                (when (> i (syntax-max-dirty syntax))
                  ;; Past every change, and this line starts as it did: the
                  ;; rest of the cache is still right.
                  (setf (syntax-first-unchecked syntax) nil (syntax-max-dirty syntax) -1)
                  (return))
                (let ((string (text-line-string (syntax-text syntax) i)))
                  (multiple-value-bind (tokens end) (lex-line string previous)
                    (setf (line-info-start-state info) previous
                          (line-info-tokens info) (coerce tokens 'simple-vector)
                          (line-info-end-state info) end
                          (line-info-length info) (length string)
                          (line-info-dirty info) nil
                          (line-info-highlighted info) nil))))
            (setf previous (line-info-end-state info))
            (incf i)
            (cond ((>= i n)
                   (setf (syntax-first-unchecked syntax) nil (syntax-max-dirty syntax) -1)
                   (return))
                  ((> i through)
                   (setf (syntax-first-unchecked syntax) i)
                   (return)))))))
    syntax))

(defun line-info (syntax line)
  (ensure-lexed syntax line)
  (aref (syntax-lines syntax) line))

(defun line-tokens (syntax line)
  (line-info-tokens (line-info syntax line)))

(defun line-start-state (syntax line)
  (line-info-start-state (line-info syntax line)))

(defun line-end-state (syntax line)
  (line-info-end-state (line-info syntax line)))

;;; Walking tokens across lines. A token's place is (line, index).

(defun token-ref (syntax line index)
  (aref (line-tokens syntax line) index))

(defun next-token (syntax line index)
  "The place of the token after (LINE, INDEX), or nil."
  (if (< (1+ index) (length (line-tokens syntax line)))
      (values line (1+ index))
      (loop for l from (1+ line) below (syntax-line-count syntax)
            when (plusp (length (line-tokens syntax l))) return (values l 0))))

(defun previous-token (syntax line index)
  "The place of the token before (LINE, INDEX), or nil."
  (if (plusp index)
      (values line (1- index))
      (loop for l from (1- line) downto 0
            for n = (length (line-tokens syntax l))
            when (plusp n) return (values l (1- n)))))

(defun token-at-or-after (syntax line column)
  "The place of the first token ending after COLUMN on LINE, or later."
  (let ((tokens (line-tokens syntax line)))
    (let ((i (position-if (lambda (tk) (> (token-end tk) column)) tokens)))
      (if i
          (values line i)
          (loop for l from (1+ line) below (syntax-line-count syntax)
                when (plusp (length (line-tokens syntax l))) return (values l 0))))))

(defun token-before (syntax line column)
  "The place of the last token starting before COLUMN on LINE, or earlier."
  (let* ((tokens (line-tokens syntax line))
         (i (position-if (lambda (tk) (< (token-start tk) column)) tokens :from-end t)))
    (if i
        (values line i)
        (previous-token syntax line 0))))

(defun continued-mode-p (state)
  (member (state-mode state) '(:string :bar :block)))

(defun continues-to-next-line-p (syntax line index)
  "True if the token at (LINE, INDEX) carries on into the next line (a
multi-line string, |symbol| or block comment)."
  (let ((info (line-info syntax line)))
    (and (= index (1- (length (line-info-tokens info))))
         (continued-mode-p (line-info-end-state info))
         (< (1+ line) (syntax-line-count syntax)))))

(defun continues-from-previous-line-p (syntax line index)
  (and (zerop index) (plusp line)
       (continued-mode-p (line-start-state syntax line))))

(defparameter *prefix-token-types* '(:quote :reader-conditional :reader)
  "Tokens that belong to the element after them: ' ` , #' #+ and such.")

;;; Positions inside the text

(defun depth-at (syntax line column)
  "How many lists are open at (LINE, COLUMN)."
  (let ((depth (state-depth (line-start-state syntax line))))
    (loop for tk across (line-tokens syntax line)
          while (<= (token-end tk) column)
          do (case (token-type tk)
               (:open (setf depth (1+ (token-depth tk))))
               (:close (setf depth (token-depth tk)))))
    depth))

(defun context-at (syntax line column)
  "Whether (LINE, COLUMN) is in :code, a :string or a :comment."
  (let ((tk (find-if (lambda (tk) (< (token-start tk) column (token-end tk)))
                     (line-tokens syntax line))))
    (cond ((and tk (eq (token-type tk) :string)) :string)
          ((and tk (eq (token-type tk) :comment)) :comment)
          ((and (null tk) (= column (line-info-length (line-info syntax line)))
                (let ((last (let ((ts (line-tokens syntax line)))
                              (and (plusp (length ts)) (aref ts (1- (length ts)))))))
                  (and last (= (token-end last) column)
                       (case (token-type last)
                         (:comment :comment)
                         (:string (and (eq (state-mode (line-end-state syntax line)) :string)
                                       :string)))))))
          (t :code))))

;;; Moving over s-expressions

(defun matching-close (syntax line index)
  "The place of the close paren matching the open paren at (LINE, INDEX), or nil."
  (let ((depth (token-depth (token-ref syntax line index))))
    (loop
      (multiple-value-setq (line index) (next-token syntax line index))
      (unless line (return nil))
      (let ((tk (token-ref syntax line index)))
        (when (and (eq (token-type tk) :close) (= (token-depth tk) depth))
          (return (values line index)))))))

(defun matching-open (syntax line index)
  "The place of the open paren matching the close paren at (LINE, INDEX), or nil."
  (let ((depth (token-depth (token-ref syntax line index))))
    (loop
      (multiple-value-setq (line index) (previous-token syntax line index))
      (unless line (return nil))
      (let ((tk (token-ref syntax line index)))
        (when (and (eq (token-type tk) :open) (= (token-depth tk) depth))
          (return (values line index)))))))

(defun element-end (syntax line index)
  "The position just after the element whose first token is at (LINE, INDEX)."
  (let ((tk (token-ref syntax line index)))
    (if (eq (token-type tk) :open)
        (multiple-value-bind (l i) (matching-close syntax line index)
          (and l (values l (token-end (token-ref syntax l i)))))
        (progn
          (loop while (continues-to-next-line-p syntax line index)
                do (setf line (1+ line) index 0))
          (values line (token-end (token-ref syntax line index)))))))

(defun element-start-place (syntax line index)
  "The place of the first token of the element whose last token is at
(LINE, INDEX), including any prefixes before it."
  (let ((tk (token-ref syntax line index)))
    (if (eq (token-type tk) :close)
        (multiple-value-setq (line index) (matching-open syntax line index))
        (loop while (continues-from-previous-line-p syntax line index)
              do (setf line (1- line)
                       index (1- (length (line-tokens syntax line))))))
    (when line
      (loop
        (multiple-value-bind (l i) (previous-token syntax line index)
          (if (and l (member (token-type (token-ref syntax l i)) *prefix-token-types*))
              (setf line l index i)
              (return))))
      (values line index))))

(defun forward-sexp-position (syntax line column)
  "The position after the s-expression following (LINE, COLUMN), or nil at
the end of a list or of the text."
  (multiple-value-bind (l i) (token-at-or-after syntax line column)
    (loop
      (unless l (return nil))
      (let ((tk (token-ref syntax l i)))
        (case (token-type tk)
          ((:comment :invalid :quote :reader-conditional :reader)
           (multiple-value-setq (l i) (next-token syntax l i)))
          (:close (return nil))
          (t (return (element-end syntax l i))))))))

(defun backward-sexp-position (syntax line column)
  "The position of the start of the s-expression before (LINE, COLUMN), or
nil at the start of a list or of the text."
  (multiple-value-bind (l i) (token-before syntax line column)
    (loop
      (unless l (return nil))
      (let ((tk (token-ref syntax l i)))
        (case (token-type tk)
          ((:comment :invalid :quote :reader-conditional :reader)
           (multiple-value-setq (l i) (previous-token syntax l i)))
          (:open (return nil))
          (t (multiple-value-bind (sl si) (element-start-place syntax l i)
               (return (and sl (values sl (token-start (token-ref syntax sl si))))))))))))

(defun up-list-position (syntax line column)
  "The position of the open paren of the list around (LINE, COLUMN), or nil."
  (let ((depth (depth-at syntax line column)))
    (when (plusp depth)
      (multiple-value-bind (l i) (token-before syntax line (1+ column))
        (loop
          (unless l (return nil))
          (let ((tk (token-ref syntax l i)))
            (when (and (eq (token-type tk) :open) (= (token-depth tk) (1- depth))
                       (or (< l line) (< (token-start tk) column)))
              (return (values l (token-start tk)))))
          (multiple-value-setq (l i) (previous-token syntax l i)))))))

(defun down-list-position (syntax line column)
  "The position just inside the next list after (LINE, COLUMN) at this level, or nil."
  (let ((depth (depth-at syntax line column)))
    (multiple-value-bind (l i) (token-at-or-after syntax line column)
      (loop
        (unless l (return nil))
        (let ((tk (token-ref syntax l i)))
          (cond ((and (eq (token-type tk) :close) (< (token-depth tk) depth)) (return nil))
                ((and (eq (token-type tk) :open) (= (token-depth tk) depth)
                      (or (> l line) (>= (token-start tk) column)))
                 (return (values l (token-end tk))))))
        (multiple-value-setq (l i) (next-token syntax l i))))))

(defun toplevel-open-before (syntax line column &key inclusive)
  "The place of the nearest top-level open paren before (LINE, COLUMN), or
at it if INCLUSIVE."
  (multiple-value-bind (l i) (token-before syntax line (if inclusive (1+ column) column))
    (loop
      (unless l (return nil))
      (let ((tk (token-ref syntax l i)))
        (when (and (eq (token-type tk) :open) (zerop (token-depth tk)))
          (return (values l i))))
      (multiple-value-setq (l i) (previous-token syntax l i)))))

(defun beginning-of-defun-position (syntax line column)
  "The start of the top-level form before (LINE, COLUMN), or nil."
  (multiple-value-bind (l i) (toplevel-open-before syntax line column)
    (and l (multiple-value-bind (sl si) (element-start-place syntax l i)
             (values sl (token-start (token-ref syntax sl si)))))))

(defun toplevel-form-bounds (syntax line column)
  "The start and end positions of the top-level form at or just before
(LINE, COLUMN), as four values (start-line start-column end-line end-column), or nil."
  (multiple-value-bind (l i) (toplevel-open-before syntax line column :inclusive t)
    (when l
      (multiple-value-bind (el ec) (element-end syntax l i)
        (when el
          (values l (token-start (token-ref syntax l i)) el ec))))))

(defun end-of-defun-position (syntax line column)
  "The end of the top-level form around (LINE, COLUMN), or of the next one."
  (flet ((after-point-p (l c) (or (> l line) (and (= l line) (> c column)))))
    (multiple-value-bind (sl sc el ec) (toplevel-form-bounds syntax line column)
      (declare (ignore sl sc))
      (if (and el (after-point-p el ec))
          (values el ec)
          (multiple-value-bind (l i) (token-at-or-after syntax line column)
            (loop
              (unless l (return nil))
              (let ((tk (token-ref syntax l i)))
                (when (and (eq (token-type tk) :open) (zerop (token-depth tk)))
                  (return (element-end syntax l i))))
              (multiple-value-setq (l i) (next-token syntax l i))))))))

(defun paren-match-at (syntax line column)
  "For a cursor at (LINE, COLUMN) just after a close paren or just before an
open paren: the column ranges of both parens, as (values l1 c1 l2 c2 matched),
where (l2, c2) is the paren next to the cursor. Nil if the cursor is not at a paren."
  (let* ((tokens (line-tokens syntax line))
         (before (find-if (lambda (tk) (and (= (token-end tk) column)
                                            (member (token-type tk) '(:close :invalid))
                                            (= 1 (- (token-end tk) (token-start tk)))))
                          tokens))
         (after (find-if (lambda (tk) (and (= (token-start tk) column) (eq (token-type tk) :open)))
                         tokens)))
    (flet ((paren-column (tk) (1- (token-end tk))))   ; the "(" of "#(" is its last char
      (cond (before
             (if (eq (token-type before) :invalid)
                 (values line (token-start before) line (token-start before) nil)
                 (multiple-value-bind (l i) (matching-open syntax line (position before tokens))
                   (if l
                       (values l (paren-column (token-ref syntax l i)) line (token-start before) t)
                       (values line (token-start before) line (token-start before) nil)))))
            (after
             (multiple-value-bind (l i) (matching-close syntax line (position after tokens))
               (if l
                   (values l (token-start (token-ref syntax l i)) line (paren-column after) t)
                   (values line (paren-column after) line (paren-column after) nil))))))))
