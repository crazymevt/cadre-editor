;;;; paredit.lisp — structural editing: slurp, barf, raise, splice, wrap, and
;;;; the balanced insertion and deletion of paredit mode
;;;;
;;;; Each operation looks at a LISP-SYNTAX and a cursor position and returns
;;;; what to do, without changing anything: a list of edits, each (START END
;;;; STRING) to replace the text from START to END (offsets) with STRING, and
;;;; where the cursor goes, as an offset in the text *before* the edits plus
;;;; whether it sticks :before or :after text inserted at that offset.
;;;; APPLY-EDITS carries them out on any text. Operations that cannot apply
;;;; signal an editor-error.

(in-package #:cadre)

;;; Positions

(defun offset-of (syntax line column)
  (text-line-position (syntax-text syntax) line column))

(defun line-column-of (syntax offset)
  (text-position-line (syntax-text syntax) offset))

(defun text-char-or-nil (text position)
  (and (<= 0 position) (< position (text-length text)) (text-char text position)))

;;; Edits

(defun map-offset (offset edits &optional (stick :before))
  "Where OFFSET (in the text before EDITS) ends up after them. Text inserted
exactly at OFFSET goes before it if STICK is :after."
  (let ((shift 0))
    (loop for (start end string) in edits
          do (cond ((or (< end offset) (and (= end offset) (< start end)))
                    (incf shift (- (length string) (- end start))))
                   ((and (= start end offset) (eq stick :after))
                    (incf shift (length string)))
                   ((and (= start offset) (< start end) (eq stick :after))
                    (incf shift (length string)))
                   ((< start offset end)
                    ;; Inside a deleted range: to its start (plus the replacement if :after).
                    (setf shift (+ shift (- start offset) (if (eq stick :after) (length string) 0))))))
    (+ offset shift)))

(defun apply-edits (text edits &optional point (stick :before) (delta 0))
  "Make EDITS to TEXT, from the end backwards so offsets stay valid, then
move the cursor to POINT (an offset before the edits) plus DELTA. Nil
for POINT means where the cursor is now, staying before inserted text."
  (let ((new-point (+ (map-offset (or point (text-point text)) edits stick) (or delta 0))))
    (dolist (edit (sort (copy-list edits)
                        (lambda (a b) (or (> (first a) (first b))
                                          (and (= (first a) (first b)) (> (second a) (second b)))))))
      (destructuring-bind (start end string) edit
        (when (< start end) (text-delete text start end))
        (when (plusp (length string)) (text-insert text start string))))
    (setf (text-point text) new-point)
    new-point))

;;; Lists around a position

(defun token-index-at (syntax line column type)
  "The index of the token of TYPE starting at COLUMN on LINE, or nil."
  (position-if (lambda (tk) (and (= (token-start tk) column) (eq (token-type tk) type)))
               (line-tokens syntax line)))

(defstruct (list-bounds (:conc-name lb-))
  open-start open-end close-start close-end)   ; offsets

(defun list-bounds-at (syntax line index)
  "The bounds of the list whose open paren is the token at (LINE, INDEX)."
  (let ((open (token-ref syntax line index)))
    (multiple-value-bind (cl ci) (matching-close syntax line index)
      (unless cl (editor-error "This list is not closed"))
      (let ((close (token-ref syntax cl ci)))
        (make-list-bounds :open-start (offset-of syntax line (token-start open))
                          :open-end (offset-of syntax line (token-end open))
                          :close-start (offset-of syntax cl (token-start close))
                          :close-end (offset-of syntax cl (token-end close)))))))

(defun enclosing-list (syntax line column &optional (levels 1))
  "The bounds of the list around (LINE, COLUMN), LEVELS lists out. Signals
an editor-error at top level."
  (let ((l line) (c column))
    (dotimes (i levels)
      (multiple-value-setq (l c) (up-list-position syntax l c))
      (unless l (editor-error "Not inside a list")))
    (list-bounds-at syntax l (token-index-at syntax l c :open))))

(defun list-element-offsets (syntax bounds)
  "The (start . end) offsets of the elements of the list BOUNDS, in order."
  (multiple-value-bind (l c) (line-column-of syntax (lb-open-end bounds))
    (loop for end = (multiple-value-bind (el ec) (forward-sexp-position syntax l c)
                      (and el (offset-of syntax el ec)))
          while end
          collect (multiple-value-bind (el ec) (line-column-of syntax end)
                    (multiple-value-bind (sl sc) (backward-sexp-position syntax el ec)
                      (setf l el c ec)
                      (cons (offset-of syntax sl sc) end))))))

(defun bounds-text (syntax start end)
  (text-string (syntax-text syntax) start end))

;;; Slurping and barfing

(defun paredit-slurp-forward (syntax line column)
  "(a b|) c → (a b| c): the list around the cursor takes in the next expression."
  (let ((bounds (enclosing-list syntax line column)))
    (multiple-value-bind (cl cc) (line-column-of syntax (lb-close-end bounds))
      (multiple-value-bind (el ec) (forward-sexp-position syntax cl cc)
        (unless el (editor-error "Nothing to slurp"))
        (values (list (list (lb-close-start bounds) (lb-close-end bounds) "")
                      (list (offset-of syntax el ec) (offset-of syntax el ec)
                            (bounds-text syntax (lb-close-start bounds) (lb-close-end bounds))))
                nil)))))

(defun paredit-barf-forward (syntax line column)
  "(a b| c) → (a b|) c: the list around the cursor lets go of its last expression."
  (let* ((bounds (enclosing-list syntax line column))
         (elements (list-element-offsets syntax bounds)))
    (unless elements (editor-error "Nothing to barf"))
    (let ((new-close (if (rest elements)
                         (cdr (car (last elements 2)))
                         (lb-open-end bounds))))
      (values (list (list (lb-close-start bounds) (lb-close-end bounds) "")
                    (list new-close new-close
                          (bounds-text syntax (lb-close-start bounds) (lb-close-end bounds))))
              nil))))

(defun paredit-slurp-backward (syntax line column)
  "a (b| c) → (a b| c): the list around the cursor takes in the expression before it."
  (let ((bounds (enclosing-list syntax line column)))
    (multiple-value-bind (ol oc) (line-column-of syntax (lb-open-start bounds))
      (multiple-value-bind (pl pc) (backward-sexp-position syntax ol oc)
        (unless pl (editor-error "Nothing to slurp"))
        (let ((at (offset-of syntax pl pc)))
          (values (list (list (lb-open-start bounds) (lb-open-end bounds) "")
                        (list at at (bounds-text syntax (lb-open-start bounds) (lb-open-end bounds))))
                  nil))))))

(defun paredit-barf-backward (syntax line column)
  "(a b| c) → a (b| c): the list around the cursor lets go of its first expression."
  (let* ((bounds (enclosing-list syntax line column))
         (elements (list-element-offsets syntax bounds)))
    (unless elements (editor-error "Nothing to barf"))
    (let ((new-open (if (rest elements)
                        (car (second elements))
                        (cdr (first elements)))))
      (values (list (list (lb-open-start bounds) (lb-open-end bounds) "")
                    (list new-open new-open
                          (bounds-text syntax (lb-open-start bounds) (lb-open-end bounds))))
              nil))))

;;; The expression at the cursor

(defun sexp-at (syntax line column)
  "The (start . end) offsets of the expression after (LINE, COLUMN), or the
one before it if none follows in this list; nil if there is neither."
  (multiple-value-bind (el ec) (forward-sexp-position syntax line column)
    (if el
        (multiple-value-bind (sl sc) (backward-sexp-position syntax el ec)
          (cons (offset-of syntax sl sc) (offset-of syntax el ec)))
        (multiple-value-bind (sl sc) (backward-sexp-position syntax line column)
          (and sl (multiple-value-bind (el ec) (forward-sexp-position syntax sl sc)
                    (cons (offset-of syntax sl sc) (offset-of syntax el ec))))))))

(defun paredit-raise (syntax line column)
  "(a |b c) → |b: replace the list around the cursor with the expression at the cursor."
  (let ((bounds (enclosing-list syntax line column))
        (sexp (or (sexp-at syntax line column) (editor-error "No expression to raise"))))
    (values (list (list (lb-open-start bounds) (lb-close-end bounds)
                        (bounds-text syntax (car sexp) (cdr sexp))))
            (lb-open-start bounds))))

(defun paredit-splice (syntax line column)
  "(a (b| c) d) → (a b| c d): remove the parentheses of the list around the cursor."
  (let ((bounds (enclosing-list syntax line column)))
    (values (list (list (lb-close-start bounds) (lb-close-end bounds) "")
                  (list (lb-open-start bounds) (lb-open-end bounds) ""))
            nil)))

(defun paredit-splice-killing-backward (syntax line column)
  "(a b |c d) → |c d: splice, deleting what comes before the cursor in the list."
  (let ((bounds (enclosing-list syntax line column))
        (point (offset-of syntax line column)))
    (values (list (list (lb-close-start bounds) (lb-close-end bounds) "")
                  (list (lb-open-start bounds) point ""))
            point)))

(defun paredit-splice-killing-forward (syntax line column)
  "(a b| c d) → a b|: splice, deleting what comes after the cursor in the list."
  (let ((bounds (enclosing-list syntax line column))
        (point (offset-of syntax line column)))
    (values (list (list point (lb-close-end bounds) "")
                  (list (lb-open-start bounds) (lb-open-end bounds) ""))
            point)))

(defun paredit-wrap (syntax line column &key (open "(") (close ")") region)
  "|a b → (|a) b: wrap the next expression (or REGION, a (start . end) of
offsets) in parentheses; with nothing to wrap, insert a pair."
  (let ((target (or region
                    (multiple-value-bind (el ec) (forward-sexp-position syntax line column)
                      (and el (multiple-value-bind (sl sc) (backward-sexp-position syntax el ec)
                                (cons (offset-of syntax sl sc) (offset-of syntax el ec))))))))
    (if target
        (values (list (list (cdr target) (cdr target) close)
                      (list (car target) (car target) open))
                (car target) :after)
        (let ((point (offset-of syntax line column)))
          (values (list (list point point (concatenate 'string open close)))
                  point :before (length open))))))

(defun paredit-split (syntax line column)
  "(a b| c) → (a b)| (c), and \"ab|cd\" → \"ab\"| \"cd\"."
  (let* ((text (syntax-text syntax))
         (point (offset-of syntax line column)))
    (if (eq (context-at syntax line column) :string)
        (values (list (list point point "\" \"")) point :before 1)
        (let* ((bounds (enclosing-list syntax line column))
               (open (bounds-text syntax (lb-open-start bounds) (lb-open-end bounds)))
               (open (subseq open (1- (length open))))          ; "#(" splits into "(" … ")"
               (close (bounds-text syntax (lb-close-start bounds) (lb-close-end bounds)))
               ;; Swallow the spaces around the cursor.
               (start (loop for i downfrom point
                            while (and (> i (lb-open-end bounds))
                                       (member (text-char text (1- i)) '(#\Space #\Tab)))
                            finally (return i)))
               (end (loop for i from point
                          while (and (< i (lb-close-start bounds))
                                     (member (text-char text i) '(#\Space #\Tab)))
                          finally (return i))))
          (values (list (list start end (format nil "~a ~a" close open)))
                  start :before (length close))))))

(defun paredit-join (syntax line column)
  "(a b) |(c d) → (a b |c d): join the expressions before and after the cursor."
  (multiple-value-bind (pl pc) (backward-sexp-position syntax line column)
    (multiple-value-bind (nl nc) (forward-sexp-position syntax line column)
      (unless (and pl nl) (editor-error "Nothing to join"))
      (let* ((before-end (multiple-value-bind (l c) (forward-sexp-position syntax pl pc)
                           (offset-of syntax l c)))
             (after-start (multiple-value-bind (l c) (backward-sexp-position syntax nl nc)
                            (offset-of syntax l c)))
             (text (syntax-text syntax))
             (a (text-char text (1- before-end)))
             (b (text-char text after-start)))
        (cond ((and (char= a #\)) (char= b #\())
               (values (list (list after-start (1+ after-start) "")
                             (list (1- before-end) before-end ""))
                       after-start))
              ((and (char= a #\") (char= b #\"))
               (values (list (list (1- before-end) (1+ after-start) "")) (1- before-end)))
              (t (editor-error "These expressions cannot be joined")))))))

;;; Balanced typing (paredit mode)

(defun after-character-escape-p (text point)
  "True if the text just before POINT is #\\ (so the next character is a character name)."
  (and (>= point 2)
       (char= (text-char text (1- point)) #\\)
       (char= (text-char text (- point 2)) #\#)))

(defun paredit-open (syntax line column &key (open "(") (close ")"))
  "Typing (: insert a pair in code, a lone ( in strings and comments."
  (let ((point (offset-of syntax line column)))
    (if (or (member (context-at syntax line column) '(:string :comment))
            (after-character-escape-p (syntax-text syntax) point))
        (values (list (list point point open)) point :after)
        (values (list (list point point (concatenate 'string open close))) point :before
                ;; The cursor goes between the two: one character after POINT.
                (length open)))))

(defun paredit-close (syntax line column &key (close ")"))
  "Typing ): move past the end of the list, removing spaces before it."
  (let ((point (offset-of syntax line column)))
    (if (or (member (context-at syntax line column) '(:string :comment))
            (after-character-escape-p (syntax-text syntax) point))
        (values (list (list point point close)) point :after)
        (multiple-value-bind (ol oc) (up-list-position syntax line column)
          (if (null ol)
              (editor-error "Unbalanced: no list to close")
              (let* ((bounds (list-bounds-at syntax ol (token-index-at syntax ol oc :open)))
                     (text (syntax-text syntax))
                     (space-start (loop for i downfrom (lb-close-start bounds)
                                        while (and (> i point)
                                                   (whitespace-char-p (text-char text (1- i))))
                                        finally (return i))))
                (values (and (< space-start (lb-close-start bounds))
                             (list (list space-start (lb-close-start bounds) "")))
                        (lb-close-end bounds))))))))

(defun string-token-at (syntax line column)
  "The string token around or ending at (LINE, COLUMN), or nil."
  (find-if (lambda (tk) (and (eq (token-type tk) :string)
                             (<= (token-start tk) column (token-end tk))))
           (line-tokens syntax line)))

(defun closing-quote-p (syntax line column)
  "True if the character at (LINE, COLUMN) is the quote ending a string."
  (let ((tk (string-token-at syntax line column)))
    (and tk (= (token-end tk) (1+ column))
         (not (continues-to-next-line-p syntax line (position tk (line-tokens syntax line))))
         (char= #\" (text-char (syntax-text syntax) (offset-of syntax line column)))
         (or (> column (token-start tk)) (continues-from-previous-line-p
                                          syntax line (position tk (line-tokens syntax line)))))))

(defun paredit-quote (syntax line column)
  "Typing \": a pair of quotes in code; in a string, move past its end or insert \\\"."
  (let ((point (offset-of syntax line column)))
    (case (context-at syntax line column)
      (:string (if (closing-quote-p syntax line column)
                   (values '() (1+ point))
                   (values (list (list point point "\\\"")) point :after)))
      (:comment (values (list (list point point "\"")) point :after))
      (t (if (after-character-escape-p (syntax-text syntax) point)
             (values (list (list point point "\"")) point :after)
             (values (list (list point point "\"\"")) point :before 1))))))

(defun escape-before-p (text point)
  "True if the character before POINT is escaped by a backslash (in a string)."
  (let ((n (loop for i downfrom (- point 2) to 0
                 while (char= (text-char text i) #\\)
                 count t)))
    (oddp n)))

(defun paredit-delete-before (syntax line column)
  "DEL in paredit mode: delete the character before the cursor, but move
over parentheses and quotes instead of unbalancing them; an empty pair is
deleted whole."
  (let* ((text (syntax-text syntax))
         (point (offset-of syntax line column)))
    (when (zerop point) (editor-error "Beginning of buffer"))
    (flet ((delete-1 () (values (list (list (1- point) point "")) nil))
           (move-to (offset) (values '() offset)))
      (if (zerop column)
          (delete-1)
          (let* ((tokens (line-tokens syntax line))
                 (before (find column tokens :key #'token-end)))
            (case (context-at syntax line column)
              (:comment (delete-1))
              (:string
               (let ((tk (string-token-at syntax line column)))
                 (cond ((and tk (= (token-start tk) (1- column))
                             (not (continues-from-previous-line-p syntax line (position tk tokens))))
                        ;; Just after the opening quote.
                        (if (closing-quote-p syntax line column)
                            (values (list (list (1- point) (1+ point) "")) nil)
                            (move-to (1- point))))
                       ((char= (text-char text (1- point)) #\\)
                        (values (list (list (1- point) (1+ point) "")) nil))
                       ((escape-before-p text point)
                        (values (list (list (- point 2) point "")) nil))
                       (t (delete-1)))))
              (t
               (cond ((and before (eq (token-type before) :close)) (move-to (1- point)))
                     ((and before (eq (token-type before) :string)) (move-to (1- point)))
                     ((and before (eq (token-type before) :open))
                      (let ((next (find column tokens :key #'token-start)))
                        (if (and next (eq (token-type next) :close))
                            (values (list (list (offset-of syntax line (token-start before))
                                                (1+ point) ""))
                                    nil)
                            (move-to (offset-of syntax line (token-start before))))))
                     (t (delete-1))))))))))

(defun paredit-delete-after (syntax line column)
  "C-d in paredit mode: like PAREDIT-BACKWARD-DELETE, forwards."
  (let* ((text (syntax-text syntax))
         (point (offset-of syntax line column)))
    (when (>= point (text-length text)) (editor-error "End of buffer"))
    (flet ((delete-1 () (values (list (list point (1+ point) "")) nil))
           (move-to (offset) (values '() offset)))
      (let* ((tokens (line-tokens syntax line))
             (after (find column tokens :key #'token-start)))
        (case (context-at syntax line column)
          (:comment (delete-1))
          (:string
           (cond ((closing-quote-p syntax line column)
                  (let ((tk (string-token-at syntax line column)))
                    (if (and tk (= (token-start tk) (1- column))
                             (not (continues-from-previous-line-p syntax line (position tk tokens))))
                        (values (list (list (1- point) (1+ point) "")) (1- point))
                        (move-to (1+ point)))))
                 ((char= (text-char text point) #\\)
                  (values (list (list point (min (+ point 2) (text-length text)) "")) nil))
                 ((and (plusp point) (char= (text-char text (1- point)) #\\)
                       (not (escape-before-p text point)))
                  (values (list (list (1- point) (1+ point) "")) (1- point)))
                 (t (delete-1))))
          (t
           (cond ((null after) (delete-1))
                 ((eq (token-type after) :open)
                  (let ((next (find (token-end after) tokens :key #'token-start)))
                    (if (and next (eq (token-type next) :close))
                        (values (list (list point (offset-of syntax line (token-end next)) "")) nil)
                        (move-to (offset-of syntax line (token-end after))))))
                 ((eq (token-type after) :close) (move-to (1+ point)))
                 ((and (eq (token-type after) :string) (char= (text-char text point) #\"))
                  (if (= (token-end after) (+ column 2))
                      (values (list (list point (+ point 2) "")) nil)
                      (move-to (1+ point))))
                 (t (delete-1)))))))))

(defun paredit-kill-end (syntax line column)
  "Where C-k in paredit mode kills to from (LINE, COLUMN): the end of the
expressions that start on this line, stopping before the end of the list
around them, or the end of the string; at the end of a line, the line break."
  (let* ((text (syntax-text syntax))
         (point (offset-of syntax line column))
         (line-end (offset-of syntax line (length (text-line-string text line)))))
    (case (context-at syntax line column)
      (:comment line-end)
      (:string
       (let* ((tk (string-token-at syntax line column))
              (index (and tk (position tk (line-tokens syntax line)))))
         (if (and tk (not (continues-to-next-line-p syntax line index)))
             (offset-of syntax line (1- (token-end tk)))
             line-end)))
      (t
       (if (= point line-end)
           (min (1+ point) (text-length text))
           (let ((end point) (l line) (c column))
             (loop
               (multiple-value-bind (el ec) (forward-sexp-position syntax l c)
                 (unless el (return))
                 (multiple-value-bind (sl) (backward-sexp-position syntax el ec)
                   (unless (= sl line) (return)))
                 (setf end (offset-of syntax el ec) l el c ec)))
             (multiple-value-bind (ol oc) (up-list-position syntax line column)
               (let ((close (and ol (lb-close-start (list-bounds-at syntax ol (token-index-at syntax ol oc :open))))))
                 (cond ((and close (<= close line-end)) close)
                       ((and (<= end line-end)
                             (every (lambda (ch) (whitespace-char-p ch)) (text-string text end line-end)))
                        line-end)
                       ((<= end line-end)
                        ;; Only a comment can follow on this line.
                        (let ((rest (string-left-trim '(#\Space #\Tab) (text-string text end line-end))))
                          (if (and (plusp (length rest)) (char= (char rest 0) #\;)) line-end end)))
                       (t end))))))))))
