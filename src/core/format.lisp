;;;; format.lisp — laying out JSON and CSS
;;;;
;;;; Both work on tokens, never on values: strings, numbers and comments come
;;;; out exactly as they went in, keys keep their order, and text that isn't
;;;; quite valid still comes out laid out as well as it can be.
;;;;
;;;; JSON: each value of an object or array on its own line, indented;
;;;; empty ones stay {} and []; "key": value. Comments (JSONC) are kept.
;;;; CSS: one declaration per line, "property: value;"; selectors on one line
;;;; with ", " between them; a blank line between top-level rules.

(in-package #:cadre)

(defun indentation (level width)
  (make-string (* level width) :initial-element #\Space))

(defun skip-string (text i)
  "The position after the string starting at I (whose quote is at I)."
  (let ((quote (char text i)))
    (loop with j = (1+ i)
          while (< j (length text))
          do (let ((c (char text j)))
               (cond ((char= c #\\) (incf j 2))
                     ((char= c quote) (return (1+ j)))
                     (t (incf j))))
          finally (return (length text)))))

(defun skip-comment (text i)
  "If a comment starts at I (// or /* */), the position after it; else nil."
  (when (and (< (1+ i) (length text)) (char= (char text i) #\/))
    (case (char text (1+ i))
      (#\/ (or (position #\Newline text :start i) (length text)))
      (#\* (let ((end (search "*/" text :start2 (+ i 2))))
             (if end (+ end 2) (length text)))))))

(defun json-space-p (c) (member c '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun next-significant (text i)
  "The position of the next character at or after I that isn't white space."
  (or (position-if-not #'json-space-p text :start i) (length text)))

(defun format-json (text &key (indent 2))
  "TEXT, JSON (or JSONC), laid out with INDENT spaces a level."
  (let ((level 0) (i 0) (n (length text)))
    (with-output-to-string (out)
      (flet ((newline () (terpri out) (write-string (indentation level indent) out)))
        (loop
          (setf i (next-significant text i))
          (when (>= i n) (return))
          (let ((c (char text i)))
            (cond ((char= c #\")
                   (let ((end (skip-string text i)))
                     (write-string text out :start i :end end)
                     (setf i end)))
                  ((skip-comment text i)
                   (let ((end (skip-comment text i)))
                     (write-string (string-right-trim '(#\Space #\Tab #\Return) (subseq text i end)) out)
                     (setf i end)
                     ;; Whatever follows a comment starts a line, unless it closes.
                     (let ((next (next-significant text i)))
                       (when (and (< next n) (not (member (char text next) '(#\} #\]))))
                         (newline)))))
                  ((member c '(#\{ #\[))
                   (let ((next (next-significant text (1+ i)))
                         (close (if (char= c #\{) #\} #\])))
                     (if (and (< next n) (char= (char text next) close))
                         (progn (write-char c out) (write-char close out) (setf i (1+ next)))
                         (progn (write-char c out) (incf level) (newline) (incf i)))))
                  ((member c '(#\} #\]))
                   (setf level (max 0 (1- level)))
                   (newline)
                   (write-char c out)
                   (incf i))
                  ((char= c #\,)
                   (write-char c out)
                   (incf i)
                   ;; A comment after the comma stays on its line.
                   (let ((next (next-significant text i)))
                     (if (and (< next n) (skip-comment text next)
                              (not (find #\Newline text :start i :end next)))
                         (write-char #\Space out)
                         (newline))))
                  ((char= c #\:)
                   (write-string ": " out)
                   (incf i))
                  (t
                   ;; A number, true, false, null, or something that isn't JSON: as it is.
                   (let ((end (or (position-if (lambda (ch) (or (json-space-p ch) (member ch '(#\{ #\} #\[ #\] #\, #\: #\"))))
                                               text :start i)
                                  n)))
                     (write-string text out :start i :end (max end (1+ i)))
                     (setf i (max end (1+ i))))))))
        (terpri out)))))

;;; CSS

(defun css-statements (text)
  "TEXT split into (kind text) items: (:open selector) before a {, (:declaration
text) ending in ; or before a }, (:close) for }, and (:comment text)."
  (let ((items '()) (start 0) (i 0) (n (length text)) (parens 0))
    (flet ((piece (end) (string-trim '(#\Space #\Tab #\Newline #\Return) (subseq text start end))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((member c '(#\" #\')) (setf i (skip-string text i)))
                       ((and (char= c #\/) (< (1+ i) n) (char= (char text (1+ i)) #\*))
                        (let ((before (piece i)) (end (skip-comment text i)))
                          (when (plusp (length before)) (push (list :declaration before) items))
                          (push (list :comment (subseq text i end)) items)
                          (setf i end start end)))
                       ((char= c #\() (incf parens) (incf i))
                       ((char= c #\)) (setf parens (max 0 (1- parens))) (incf i))
                       ((plusp parens) (incf i))
                       ((char= c #\{) (push (list :open (piece i)) items) (setf start (1+ i)) (incf i))
                       ((char= c #\;)
                        (let ((p (piece i))) (when (plusp (length p)) (push (list :declaration p) items)))
                        (setf start (1+ i)) (incf i))
                       ((char= c #\})
                        (let ((p (piece i))) (when (plusp (length p)) (push (list :declaration p) items)))
                        (push (list :close) items)
                        (setf start (1+ i)) (incf i))
                       (t (incf i)))))
      (let ((rest (piece n))) (when (plusp (length rest)) (push (list :declaration rest) items))))
    (nreverse items)))

(defun collapse-space (string)
  "STRING with each run of white space (outside strings) as one space."
  (with-output-to-string (out)
    (loop with i = 0 and space = nil
          while (< i (length string))
          do (let ((c (char string i)))
               (cond ((member c '(#\" #\'))
                      (when space (write-char #\Space out) (setf space nil))
                      (let ((end (skip-string string i))) (write-string string out :start i :end end) (setf i end)))
                     ((json-space-p c) (setf space t) (incf i))
                     (t (when space (write-char #\Space out) (setf space nil))
                        (write-char c out) (incf i)))))))

(defun css-selector (text)
  "A selector list, one line: ', ' between selectors."
  (let ((one (collapse-space text)))
    (format nil "~{~a~^, ~}" (mapcar (lambda (s) (string-trim " " s)) (split-top-level one #\,)))))

(defun split-top-level (string char)
  "STRING split at CHAR outside strings and parentheses."
  (let ((parts '()) (start 0) (parens 0) (i 0))
    (loop while (< i (length string))
          do (let ((c (char string i)))
               (cond ((member c '(#\" #\')) (setf i (skip-string string i)))
                     (t (case c
                          (#\( (incf parens))
                          (#\) (setf parens (max 0 (1- parens))))
                          (t (when (and (char= c char) (zerop parens))
                               (push (subseq string start i) parts)
                               (setf start (1+ i)))))
                        (incf i)))))
    (push (subseq string start) parts)
    (nreverse parts)))

(defun css-declaration (text)
  "property: value, from a declaration's text."
  (let* ((one (collapse-space text))
         (colon (position #\: one)))
    (if (and colon (not (char= (char one 0) #\@)))
        (format nil "~a: ~a" (string-trim " " (subseq one 0 colon)) (string-trim " " (subseq one (1+ colon))))
        one)))

(defun format-css (text &key (indent 2))
  "TEXT, CSS, laid out with INDENT spaces a level."
  (let ((level 0) (lines '()) (previous nil))
    (flet ((line (string) (push (concatenate 'string (indentation level indent) string) lines)))
      (dolist (item (css-statements text))
        (ecase (first item)
          (:open
           ;; A blank line between top-level rules.
           (when (and (zerop level) previous (not (eq previous :comment))) (push "" lines))
           (line (format nil "~a {" (css-selector (second item))))
           (incf level))
          (:declaration
           (line (format nil "~a;" (css-declaration (second item)))))
          (:close
           (setf level (max 0 (1- level)))
           (line "}"))
          (:comment
           (when (and (zerop level) previous (not (eq previous :comment))) (push "" lines))
           (line (second item))))
        (setf previous (first item))))
    (format nil "~{~a~%~}" (nreverse lines))))
