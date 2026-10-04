;;;; folding.lisp — which lines can be folded away
;;;;
;;;; A fold range is (first last): line FIRST stays visible and lines after
;;;; it up to LAST are hidden. In Lisp, every list that ends on a later line
;;;; than it starts is a range (one per line: the longest starting there).
;;;; In Markdown, a heading's section runs to the next heading at its level
;;;; or above, and a fenced code block from fence to fence.

(in-package #:cadre)

(defun lisp-fold-ranges (syntax)
  "The fold ranges of the Lisp text SYNTAX describes, by first line."
  (let ((longest (make-hash-table))
        (opens '()))
    (dotimes (line (syntax-line-count syntax))
      (loop for tk across (line-tokens syntax line)
            do (case (token-type tk)
                 (:open (push line opens))
                 (:close (let ((first (pop opens)))
                           (when (and first (< first line))
                             (setf (gethash first longest) (max line (gethash first longest 0)))))))))
    (sort (loop for first being the hash-keys of longest using (hash-value last)
                collect (list first last))
          #'< :key #'first)))

(defun section-blank-line-p (line)
  (every (lambda (c) (member c '(#\Space #\Tab))) line))

(defun markdown-fold-ranges (text)
  "The fold ranges of the Markdown document TEXT: sections under headings
(without the blank lines before the next heading) and fenced code blocks."
  (let* ((lines (coerce (split-text-lines text) 'vector))
         (n (length lines))
         (ranges '())
         (headings '())                 ; (level line), outside code blocks
         (fence nil))                   ; (char count line) while in a block
    (dotimes (i n)
      (let ((line (aref lines i)))
        (if fence
            (when (fence-close-p line (first fence) (second fence))
              (when (< (third fence) i) (push (list (third fence) i) ranges))
              (setf fence nil))
            (multiple-value-bind (char count) (fence-open line)
              (if char
                  (setf fence (list char count i))
                  (let ((level (heading-level line)))
                    (when level (push (list level i) headings))))))))
    (let ((headings (nreverse headings)))
      (loop for (level line) in headings
            for rest on (rest headings)
            do (let* ((next (find-if (lambda (h) (<= (first h) level)) rest))
                      (last (1- (if next (second next) n))))
                 (loop while (and (> last line) (section-blank-line-p (aref lines last))) do (decf last))
                 (when (> last line) (push (list line last) ranges))))
      ;; The last heading has no REST to loop over.
      (let ((final (car (last headings))))
        (when final
          (let ((last (1- n)))
            (loop while (and (> last (second final)) (section-blank-line-p (aref lines last))) do (decf last))
            (when (> last (second final)) (push (list (second final) last) ranges))))))
    (sort (remove-duplicates ranges :test #'equal) #'< :key #'first)))

(defun fold-range-at (ranges line)
  "The innermost of RANGES that LINE is in: the one starting on LINE, or else
the one starting nearest above it that reaches LINE."
  (let ((best nil))
    (dolist (range ranges best)
      (destructuring-bind (first last) range
        (when (and (<= first line last)
                   (or (null best) (> first (first best))))
          (setf best range))))))
