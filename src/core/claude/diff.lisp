;;;; diff.lisp — line diffs, for reviewing the edits Claude proposes
;;;;
;;;; DIFF-LINES compares two lists of lines: the common start and end are
;;;; skipped, and the rest is compared with a longest-common-subsequence
;;;; table, which is fine for the size of an edit.

(in-package #:cadre)

(defun split-lines (string)
  "STRING's lines, without newlines. A final newline doesn't add an empty line."
  (let ((lines (uiop:split-string string :separator '(#\Newline))))
    (if (and (rest lines) (string= (car (last lines)) ""))
        (butlast lines)
        lines)))

(defun diff-lines (old new)
  "How lists of strings OLD become NEW: a list of (:same line), (:removed
line) and (:added line), in order."
  (let* ((old (coerce old 'vector)) (new (coerce new 'vector))
         (n (length old)) (m (length new))
         (prefix (loop for i from 0 below (min n m)
                       while (string= (aref old i) (aref new i)) count t))
         (suffix (loop for i from 1 to (- (min n m) prefix)
                       while (string= (aref old (- n i)) (aref new (- m i))) count t))
         (a (subseq old prefix (- n suffix)))
         (b (subseq new prefix (- m suffix)))
         (la (length a)) (lb (length b))
         (table (make-array (list (1+ la) (1+ lb)) :element-type 'fixnum :initial-element 0))
         (middle '()))
    (loop for i from (1- la) downto 0
          do (loop for j from (1- lb) downto 0
                   do (setf (aref table i j)
                            (if (string= (aref a i) (aref b j))
                                (1+ (aref table (1+ i) (1+ j)))
                                (max (aref table (1+ i) j) (aref table i (1+ j)))))))
    (let ((i 0) (j 0))
      (loop while (or (< i la) (< j lb))
            do (cond ((and (< i la) (< j lb) (string= (aref a i) (aref b j)))
                      (push (list :same (aref a i)) middle) (incf i) (incf j))
                     ((and (< i la) (or (= j lb) (>= (aref table (1+ i) j) (aref table i (1+ j)))))
                      (push (list :removed (aref a i)) middle) (incf i))
                     (t (push (list :added (aref b j)) middle) (incf j)))))
    (append (loop for i below prefix collect (list :same (aref old i)))
            (nreverse middle)
            (loop for i from (- n suffix) below n collect (list :same (aref old i))))))

(defun diff-stats (diff)
  "The numbers of added and removed lines in DIFF."
  (values (count :added diff :key #'first) (count :removed diff :key #'first)))

(defun replace-unique (text old new)
  "TEXT with the one occurrence of OLD replaced by NEW. Signals a tool error
if OLD is missing or occurs more than once."
  (let ((start (search old text)))
    (cond ((or (null start) (string= old "")) (tool-error "The text to replace was not found."))
          ((search old text :start2 (1+ start))
           (tool-error "The text to replace occurs more than once; include more of the surrounding text."))
          (t (concatenate 'string (subseq text 0 start) new (subseq text (+ start (length old))))))))
