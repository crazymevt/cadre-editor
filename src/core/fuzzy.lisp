;;;; fuzzy.lisp — fuzzy matching for the command palette and quick open
;;;;
;;;; A pattern matches a string when its characters appear in order, ignoring
;;;; case. Matches score higher when characters are consecutive, start a
;;;; word, or match the case typed, and when the string is short.

(in-package #:cadre)

(defun word-start-p (string i)
  (or (zerop i)
      (let ((before (char string (1- i))))
        (or (member before '(#\- #\_ #\Space #\/ #\. #\: #\\))
            (and (lower-case-p before) (upper-case-p (char string i)))))))

(defun fuzzy-match (pattern string)
  "A score if PATTERN matches STRING (higher is better), and the positions
matched; nil if it does not match. The empty pattern matches everything."
  (let ((positions '())
        (score 0)
        (previous -2)
        (start 0))
    (loop for p across pattern
          for i = (position p string :start start :test #'char-equal)
          do (unless i (return-from fuzzy-match nil))
             ;; Prefer a word start a little further on to a mid-word match here.
             (let ((word (loop for j from i below (length string)
                               when (and (char-equal (char string j) p) (word-start-p string j))
                                 return j)))
               (when (and word (/= word i) (/= i (1+ previous)))
                 (setf i word)))
             (incf score 1)
             (when (= i (1+ previous)) (incf score 5))
             (when (word-start-p string i) (incf score 8))
             (when (char= p (char string i)) (incf score 1))
             (push i positions)
             (setf previous i start (1+ i)))
    (values (- score (/ (length string) 100)) (nreverse positions))))

(defun fuzzy-filter (pattern items &key (key #'identity) limit)
  "The ITEMS matching PATTERN, best first, at most LIMIT of them."
  (let* ((scored (loop for item in items
                       for score = (fuzzy-match pattern (funcall key item))
                       when score collect (cons score item)))
         (sorted (stable-sort scored #'> :key #'car)))
    (mapcar #'cdr (if (and limit (> (length sorted) limit)) (subseq sorted 0 limit) sorted))))
