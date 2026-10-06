;;;; format.lisp — laying out JSON, CSS and XML
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

;;; XML: each element on its own line, indented by its depth. An element
;;; holding only text stays on one line (<a>text</a>), and so does an empty
;;; one. Tags are kept as written, attributes and all; comments, CDATA,
;;; processing instructions and the doctype come out as they went in. Text
;;; is trimmed, so white space between elements in mixed content may change.

(defun xml-tokens (text)
  "TEXT's parts, as (kind string): :open, :close and :empty tags, :text,
and :other (comments, CDATA, processing instructions, the doctype)."
  (let ((tokens '()) (i 0) (n (length text)))
    (flet ((upto (end) (min n end))
           (tag-end (start)
             ;; The > closing the tag at START, outside quoted attribute values.
             (loop with quote = nil
                   for j from (1+ start) below n
                   for c = (char text j)
                   do (cond (quote (when (char= c quote) (setf quote nil)))
                            ((member c '(#\" #\'))  (setf quote c))
                            ((char= c #\>) (return (1+ j))))
                   finally (return n)))
           (doctype-end (start)
             ;; The doctype's >, after any internal subset in [ ].
             (loop with depth = 0
                   for j from start below n
                   for c = (char text j)
                   do (case c
                        (#\[ (incf depth))
                        (#\] (decf depth))
                        (#\> (when (<= depth 0) (return (1+ j)))))
                   finally (return n))))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (if (char/= c #\<)
                     (let ((end (or (position #\< text :start i) n)))
                       (push (list :text (subseq text i end)) tokens)
                       (setf i end))
                     (multiple-value-bind (kind end)
                         (cond ((string= "<!--" text :start2 i :end2 (upto (+ i 4)))
                                (values :other (let ((e (search "-->" text :start2 (+ i 4)))) (if e (+ e 3) n))))
                               ((string= "<![CDATA[" text :start2 i :end2 (upto (+ i 9)))
                                (values :other (let ((e (search "]]>" text :start2 (+ i 9)))) (if e (+ e 3) n))))
                               ((string= "<?" text :start2 i :end2 (upto (+ i 2)))
                                (values :other (let ((e (search "?>" text :start2 (+ i 2)))) (if e (+ e 2) n))))
                               ((string= "<!" text :start2 i :end2 (upto (+ i 2)))
                                (values :other (doctype-end i)))
                               ((string= "</" text :start2 i :end2 (upto (+ i 2)))
                                (values :close (tag-end i)))
                               (t (let ((end (tag-end i)))
                                    (values (if (and (>= (- end i) 2) (char= #\/ (char text (- end 2)))) :empty :open)
                                            end))))
                       (push (list kind (subseq text i end)) tokens)
                       (setf i end))))))
    (nreverse tokens)))

(defun format-xml (text &key (indent 2))
  "TEXT, XML, laid out with INDENT spaces a level."
  (let* ((space '(#\Space #\Tab #\Newline #\Return))
         (tokens (remove-if (lambda (token) (and (eq (first token) :text)
                                                 (every (lambda (c) (member c space)) (second token))))
                            (xml-tokens text)))
         (level 0)
         (lines '()))
    (flet ((line (string) (push (concatenate 'string (indentation level indent) string) lines)))
      (loop while tokens
            do (destructuring-bind (kind string) (pop tokens)
                 (case kind
                   (:open
                    (let ((next (first tokens)) (after (second tokens)))
                      (cond ((eq (first next) :close)
                             ;; <a></a>
                             (line (concatenate 'string string (second (pop tokens)))))
                            ((and (eq (first next) :text) (eq (first after) :close))
                             ;; <a>text</a>
                             (pop tokens) (pop tokens)
                             (line (concatenate 'string string (string-trim space (second next)) (second after))))
                            (t (line string) (incf level)))))
                   (:close (setf level (max 0 (1- level))) (line string))
                   (:text (line (string-trim space string)))
                   (t (line string))))))
    (format nil "~{~a~%~}" (nreverse lines))))
