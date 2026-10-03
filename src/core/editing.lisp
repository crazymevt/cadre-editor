;;;; editing.lisp — the kill ring, replacing with case, and other editing
;;;; logic that needs no GUI
;;;;
;;;; The kill ring holds killed (cut) text, newest first. Kills made by
;;;; consecutive kill commands join into one entry, as in Emacs. The GUI
;;;; keeps the newest entry and the system clipboard in step.

(in-package #:cadre)

(define-option *kill-ring-max* 120 (integer 1)
  "How many killed texts the kill ring keeps.")

(defvar *kill-ring* '() "Killed texts, newest first.")
(defvar *kill-ring-yank-index* 0 "Which entry M-y yanked last.")

(define-hook *kill-hook* "Called with the newest kill-ring entry after each kill.")

(defun kill-new (string)
  "Put STRING on the kill ring as its newest entry."
  (unless (and *kill-ring* (string= string (first *kill-ring*)))
    (push string *kill-ring*)
    (when (> (length *kill-ring*) *kill-ring-max*)
      (setf *kill-ring* (subseq *kill-ring* 0 *kill-ring-max*))))
  (setf *kill-ring-yank-index* 0)
  string)

(defun kill-text (string &key (direction :forward))
  "Record STRING as killed. If the last command killed too, join STRING to
that kill: after it for :forward kills, before it for :backward ones."
  (if (and (eq *last-command-kind* :kill) *kill-ring*)
      (setf (first *kill-ring*) (if (eq direction :backward)
                                    (concatenate 'string string (first *kill-ring*))
                                    (concatenate 'string (first *kill-ring*) string))
            *kill-ring-yank-index* 0)
      (kill-new string))
  (setf *this-command-kind* :kill)
  (run-hook '*kill-hook* (first *kill-ring*))
  (first *kill-ring*))

(defun current-kill (&optional (n 0))
  "The kill-ring entry N after the one last yanked, which becomes the one
last yanked. Signals an editor-error if the ring is empty."
  (unless *kill-ring* (editor-error "The kill ring is empty"))
  (setf *kill-ring-yank-index* (mod (+ *kill-ring-yank-index* n) (length *kill-ring*)))
  (nth *kill-ring-yank-index* *kill-ring*))

;;; Matching and replacing

(defun case-fold-p (pattern)
  "Searches ignore case unless PATTERN has an upper-case letter."
  (notany #'upper-case-p pattern))

(defun find-all (pattern string &key (case-fold (case-fold-p pattern)) (start 0) end)
  "The (start . end) of each occurrence of PATTERN in STRING."
  (let ((end (or end (length string)))
        (n (length pattern)))
    (if (zerop n)
        '()
        (loop with test = (if case-fold #'char-equal #'char=)
              for i = (search pattern string :test test :start2 start :end2 end)
                then (search pattern string :test test :start2 (+ i n) :end2 end)
              while i
              collect (cons i (+ i n))))))

(defun replacement-for (match replacement &key (case-fold t))
  "REPLACEMENT, in the case MATCH is written in, as Emacs does when
ignoring case: all capitals, or a capital first letter, carry over to a
lower-case REPLACEMENT."
  (cond ((or (not case-fold) (some #'upper-case-p replacement)
             (notany #'alpha-char-p match))
         replacement)
        ((and (every (lambda (c) (or (not (alpha-char-p c)) (upper-case-p c))) match)
              (> (count-if #'alpha-char-p match) 1))
         (string-upcase replacement))
        ((upper-case-p (char match (position-if #'alpha-char-p match)))
         (let ((r (copy-seq replacement))
               (i (position-if #'alpha-char-p replacement)))
           (when i (setf (char r i) (char-upcase (char r i))))
           r))
        (t replacement)))

(defun replace-all (pattern replacement string &key (case-fold (case-fold-p pattern)))
  "STRING with every occurrence of PATTERN replaced, and the count."
  (let ((matches (find-all pattern string :case-fold case-fold)))
    (values (with-output-to-string (out)
              (let ((pos 0))
                (loop for (start . end) in matches
                      do (write-string string out :start pos :end start)
                         (write-string (replacement-for (subseq string start end) replacement
                                                        :case-fold case-fold)
                                       out)
                         (setf pos end))
                (write-string string out :start pos)))
            (length matches))))

;;; Words

(defun word-char-p (c)
  (or (alphanumericp c) (char= c #\_)))

(defun word-bounds-after (string position)
  "The start and end of the next word at or after POSITION in STRING, or nil."
  (let* ((start (position-if #'word-char-p string :start (min position (length string)))))
    (and start (values start (or (position-if-not #'word-char-p string :start start) (length string))))))

(defun capitalize-string (word)
  (let ((s (string-downcase word))
        (i (position-if #'alpha-char-p word)))
    (when i (setf (char s i) (char-upcase (char s i))))
    s))

(defun symbol-constituent-p (c)
  (and (graphic-char-p c) (not (terminating-char-p c)) (char/= c #\|)))

(defun dabbrev-candidates (prefix string point)
  "The words in STRING that start with PREFIX (and are longer), nearest to
POINT first: searching back from POINT, then forward."
  (let ((before '()) (after '()) (n (length prefix)))
    (when (plusp n)
      (loop with i = 0
            while (< i (length string))
            do (if (symbol-constituent-p (char string i))
                   (let ((end (or (position-if-not #'symbol-constituent-p string :start i) (length string))))
                     (when (and (> (- end i) n)
                                (string-equal prefix string :start2 i :end2 (+ i n))
                                (not (<= i point end)))
                       (if (< i point)
                           (push (subseq string i end) before)
                           (push (subseq string i end) after)))
                     (setf i end))
                   (incf i))))
    (remove-duplicates (append before (nreverse after)) :test #'string= :from-end t)))
