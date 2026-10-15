;;;; edn.lisp — reading EDN: deps.edn, bb.edn and project.clj
;;;;
;;;; Clojure's project files are read before any Clojure runs, so the
;;;; Project page can list their dependencies and aliases. READ-EDN reads
;;;; one value; READ-EDN-FORMS every value in a string (project.clj is a
;;;; (defproject …) form, which reads as a list). Values become:
;;;;   {…}  (:map (key . value) …)      […]  (:vector item …)
;;;;   (…)  (:list item …)              #{…} (:set item …)
;;;;   :kw  (:keyword "kw")             sym  (:symbol "sym")
;;;;   "s"  a string, numbers numbers, nil/true/false :nil :true :false
;;;; A tagged value (#inst "…") is its value; #_ discards the next one; a
;;;; regex #"…" and a character \c read as strings. Nothing is evaluated.

(in-package #:cadre)

(define-condition edn-error (error)
  ((position :initarg :position :reader edn-error-position))
  (:report (lambda (c s) (format s "Malformed EDN at ~d" (edn-error-position c)))))

(defun edn-delimiter-p (c)
  (or (whitespace-char-p c) (member c '(#\, #\( #\) #\[ #\] #\{ #\} #\" #\;))))

(defun read-edn (string &optional (start 0))
  "Read one EDN value from STRING at START. Returns it and the position after
it, or :eof and the length when only whitespace and comments are left."
  (let ((i start) (n (length string)))
    (labels ((fail () (error 'edn-error :position i))
             (peek () (if (< i n) (char string i) (fail)))
             (skip ()
               (loop while (< i n)
                     do (let ((c (char string i)))
                          (cond ((or (whitespace-char-p c) (char= c #\,)) (incf i))
                                ((char= c #\;) (loop while (and (< i n) (char/= (char string i) #\Newline)) do (incf i)))
                                (t (return))))))
             (value ()
               (skip)
               (let ((c (peek)))
                 (case c
                   (#\( (incf i) (cons :list (items #\))))
                   (#\[ (incf i) (cons :vector (items #\])))
                   (#\{ (incf i) (cons :map (pairs (items #\}))))
                   (#\" (incf i) (text))
                   (#\\ (incf i) (let ((start i)) (incf i) (loop while (and (< i n) (not (edn-delimiter-p (char string i)))) do (incf i))
                                   (subseq string start i)))
                   (#\# (incf i) (dispatch))
                   ((#\) #\] #\}) (fail))
                   (t (atom-value)))))
             (items (close)
               (let ((list '()))
                 (loop
                   (skip)
                   (when (char= (peek) close) (incf i) (return (nreverse list)))
                   (let ((v (value)))
                     (unless (eq v :discarded) (push v list))))))
             (pairs (items)
               (loop for (k v) on items by #'cddr collect (cons k v)))
             (text ()
               (with-output-to-string (out)
                 (loop (let ((c (peek)))
                         (incf i)
                         (case c
                           (#\" (return))
                           (#\\ (let ((e (peek)))
                                  (incf i)
                                  (write-char (case e (#\n #\Newline) (#\t #\Tab) (#\r #\Return) (t e)) out)))
                           (t (write-char c out)))))))
             (dispatch ()
               (let ((c (peek)))
                 (case c
                   (#\{ (incf i) (cons :set (items #\})))
                   (#\_ (incf i) (value) :discarded)
                   (#\" (incf i) (text))                  ; a regex
                   (t (atom-value) (value)))))            ; a tag, then its value
             (atom-value ()
               (let ((start i))
                 (loop while (and (< i n) (not (edn-delimiter-p (char string i)))) do (incf i))
                 (let ((text (subseq string start i)))
                   (when (string= text "") (fail))
                   (cond ((string= text "nil") :nil)
                         ((string= text "true") :true)
                         ((string= text "false") :false)
                         ((char= (char text 0) #\:) (list :keyword (string-left-trim ":" text)))
                         ((number-string-p text)
                          (let ((*read-eval* nil)) (ignore-errors (read-from-string text))))
                         (t (list :symbol text)))))))
      (skip)
      (if (>= i n)
          (values :eof n)
          (let ((v (value)))
            (if (eq v :discarded)
                (read-edn string i)
                (values v i)))))))

(defun read-edn-forms (string)
  "Every EDN value in STRING, in order."
  (loop with i = 0
        for (value next) = (multiple-value-list (read-edn string i))
        until (eq value :eof)
        collect value
        do (setf i next)))

(defun edn-name (value)
  "The name of a keyword or symbol VALUE, or a string VALUE itself."
  (cond ((stringp value) value)
        ((and (consp value) (member (first value) '(:keyword :symbol))) (second value))))

(defun edn-get (map key)
  "The value in the EDN MAP of the keyword KEY (a string such as \"deps\"), or nil."
  (and (consp map) (eq (first map) :map)
       (cdr (find-if (lambda (pair) (and (consp (car pair)) (eq (first (car pair)) :keyword)
                                         (string= (second (car pair)) key)))
                     (rest map)))))
