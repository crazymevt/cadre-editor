;;;; everyday.lisp — small things used every day: zoom, word wrap, moving
;;;; and copying lines, recent projects and files, rectangles, and the
;;;; REPL's history (arrow keys at the prompt, kept between sessions)

(in-package #:cadre-ui)

;;; Zoom

(define-option *editor-zoom* 0 (integer -8 24)
  "Points added to the editor font's size. Ctrl+= and Ctrl+- change it; Ctrl+0 resets it."
  :category "Appearance")

(defun font-size-points (font)
  "The size in points at the end of FONT (\"Iosevka 13pt\", \"Menlo 12\"), or nil."
  (let* ((space (position #\Space font :from-end t))
         (size (and space (string-right-trim "pt" (subseq font (1+ space))))))
    (and size (plusp (length size)) (every #'digit-char-p size) (parse-integer size))))

(defun zoomed-font-size ()
  "The editors' font size with *EDITOR-ZOOM* added, as CSS, or nil for GTK's own."
  (let ((base (font-size-points *editor-font*)))
    (cond ((and (null base) (zerop *editor-zoom*)) nil)
          (t (format nil "~dpt" (max 6 (+ (or base 12) *editor-zoom*)))))))

(defun set-zoom (zoom)
  (setf *editor-zoom* (max -8 (min 24 zoom)))
  (save-option '*editor-zoom*)
  (install-font-css)
  (when *window*
    (loop for view being the hash-values of (window-views *window*)
          when (view-gutter view) do (update-gutter-width view)))
  (message "Text size: ~:[~@d~;normal~*~]" (zerop *editor-zoom*) *editor-zoom*))

(define-command zoom-in ()
  "Make the editors' text bigger."
  (set-zoom (1+ *editor-zoom*)))

(define-command zoom-out ()
  "Make the editors' text smaller."
  (set-zoom (1- *editor-zoom*)))

(define-command zoom-reset ()
  "Show the editors' text at its normal size."
  (set-zoom 0))

;;; Word wrap

(define-command toggle-word-wrap ()
  "Wrap long lines in this editor, or stop wrapping them."
  (let* ((text-view (view-text-view (current-view)))
         (wrap (eq (gtk:text-view-get-wrap-mode text-view) :none)))
    (gtk:text-view-set-wrap-mode text-view (if wrap :word-char :none))
    (message "Word wrap ~:[off~;on~]" wrap)))

;;; Moving and copying lines

(defun line-start-offset (gtk-buffer line) (gtk:text-iter-get-offset (line-iter gtk-buffer line)))
(defun line-end-offset (gtk-buffer line) (gtk:text-iter-get-offset (line-end-iter gtk-buffer line)))

(defun selection-places (view)
  "The cursor's and the selection bound's (line . column)."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (flet ((place (mark) (let ((it (gtk:text-buffer-get-iter-at-mark gtk-buffer mark)))
                           (cons (gtk:text-iter-get-line it) (gtk:text-iter-get-line-offset it)))))
      (values (place (gtk:text-buffer-get-insert gtk-buffer))
              (place (gtk:text-buffer-get-selection-bound gtk-buffer))))))

(defun restore-selection (view insert bound delta)
  "Put the cursor and selection bound back at INSERT and BOUND, DELTA lines on."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (flet ((iter (place) (line-iter gtk-buffer (+ (car place) delta) (cdr place))))
      (gtk:text-buffer-select-range gtk-buffer (iter insert) (iter bound)))
    (scroll-to-cursor view)))

(defun replace-lines (gtk-buffer first last lines)
  "Replace lines FIRST to LAST with LINES (strings)."
  (replace-text-between gtk-buffer (line-start-offset gtk-buffer first) (line-end-offset gtk-buffer last)
                        (format nil "~{~a~^~%~}" lines)))

(defun buffer-lines (gtk-buffer first last)
  (loop for l from first to last collect (text-line-string gtk-buffer l)))

(defun move-lines (delta)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (count (gtk:text-buffer-get-line-count gtk-buffer)))
    (multiple-value-bind (first last) (selected-lines view)
      (when (if (minusp delta) (plusp first) (< last (1- count)))
        (multiple-value-bind (insert bound) (selection-places view)
          (let ((block (buffer-lines gtk-buffer first last)))
            (with-user-action (gtk-buffer)
              (if (minusp delta)
                  (replace-lines gtk-buffer (1- first) last
                                 (append block (list (text-line-string gtk-buffer (1- first)))))
                  (replace-lines gtk-buffer first (1+ last)
                                 (cons (text-line-string gtk-buffer (1+ last)) block))))
            (restore-selection view insert bound delta)))))))

(define-command move-lines-up ()
  "Move this line (or the selected lines) up one line."
  (:repeat t)
  (move-lines -1))

(define-command move-lines-down ()
  "Move this line (or the selected lines) down one line."
  (:repeat t)
  (move-lines 1))

(defun duplicate-lines (below)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (first last) (selected-lines view)
      (multiple-value-bind (insert bound) (selection-places view)
        (let ((block (buffer-lines gtk-buffer first last)))
          (with-user-action (gtk-buffer)
            (replace-lines gtk-buffer first last (append block block)))
          ;; Below: the cursor goes with the new copy; above: it stays on the upper one.
          (restore-selection view insert bound (if below (1+ (- last first)) 0)))))))

(define-command duplicate-lines-down ()
  "Copy this line (or the selected lines) below, and move to the copy."
  (duplicate-lines t))

(define-command duplicate-lines-up ()
  "Copy this line (or the selected lines) above."
  (duplicate-lines nil))

;;; Recent projects and files

(defparameter *recent-projects-max* 15)
(defparameter *recent-files-max* 30)

(defun remember-recent (key path max)
  (let ((name (uiop:native-namestring path)))
    (setf (setting key)
          (let ((list (cons name (remove name (setting key) :test #'string=))))
            (if (> (length list) max) (subseq list 0 max) list)))))

(defun remember-recent-project (directory) (remember-recent :recent-projects directory *recent-projects-max*))

(defun remember-recent-file (buffer)
  (when (buffer-file buffer)
    (remember-recent :recent-files (buffer-file buffer) *recent-files-max*)))

(add-hook '*buffer-created-hook* 'remember-recent-file)

(define-command open-recent-project ()
  "Open a folder you had open before."
  (let ((projects (remove-if-not #'uiop:directory-exists-p (setting :recent-projects))))
    (unless projects (editor-error "No recent folders"))
    (open-picker (window-picker *window*)
                 :items projects
                 :label (lambda (p) (car (last (pathname-directory (uiop:ensure-directory-pathname p)))))
                 :detail #'identity
                 :placeholder "Open a recent folder"
                 :on-choose (lambda (p) (open-project (uiop:ensure-directory-pathname p))))))

(define-command open-recent-file ()
  "Open a file you had open before."
  (let* ((open (open-file-names))
         (files (remove-if (lambda (f) (or (not (probe-file f)) (member f open :test #'string=)))
                           (setting :recent-files))))
    (unless files (editor-error "No recent files that aren't open"))
    (open-picker (window-picker *window*)
                 :items files
                 :label #'file-namestring
                 :detail (lambda (f)
                           (let ((project (and *window* (window-project *window*))))
                             (if (and project (uiop:subpathp f project))
                                 (enough-namestring f project)
                                 f)))
                 :placeholder "Open a recent file"
                 :on-choose (lambda (f) (open-file-path (pathname f))))))

;;; Rectangles: the columns between the mark and the cursor, on each line

(defvar *killed-rectangle* nil "The last rectangle killed or copied, as a list of strings.")

(defun rectangle-bounds (view)
  "The rectangle between the region's ends: first and last line, left and right column."
  (multiple-value-bind (start end) (region-bounds view)
    (let ((gtk-buffer (view-gtk-buffer view)))
      (multiple-value-bind (l1 c1) (iter-line-column (iter-at gtk-buffer start))
        (multiple-value-bind (l2 c2) (iter-line-column (iter-at gtk-buffer end))
          (values l1 l2 (min c1 c2) (max c1 c2)))))))

(defun pad-to (string column)
  (if (< (length string) column)
      (concatenate 'string string (make-string (- column (length string)) :initial-element #\Space))
      string))

(defun rectangle-strings (gtk-buffer first last left right)
  (loop for l from first to last
        for line = (text-line-string gtk-buffer l)
        collect (subseq (pad-to line right) left right)))

(defun map-rectangle-lines (view function)
  "Replace each line of the rectangle with (FUNCTION line left right), as one undo step."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (first last left right) (rectangle-bounds view)
      (let ((lines (loop for l from first to last
                         collect (string-right-trim " " (funcall function (text-line-string gtk-buffer l) left right)))))
        (with-user-action (gtk-buffer)
          (replace-lines gtk-buffer first last lines))
        (deactivate-mark view)
        (gtk:text-buffer-place-cursor gtk-buffer (line-iter gtk-buffer first (min left (length (text-line-string gtk-buffer first)))))))))

(defun cut-columns (line left right &optional (insert ""))
  (let ((padded (pad-to line right)))
    (concatenate 'string (subseq padded 0 left) insert (subseq padded right))))

(define-command copy-rectangle ()
  "Copy the rectangle between the mark and the cursor, for C-x r y."
  (let ((view (current-view)))
    (multiple-value-bind (first last left right) (rectangle-bounds view)
      (setf *killed-rectangle* (rectangle-strings (view-gtk-buffer view) first last left right))
      (kill-new (format nil "~{~a~^~%~}" *killed-rectangle*))
      (deactivate-mark view)
      (message "Copied a rectangle of ~d line~:p" (length *killed-rectangle*)))))

(define-command kill-rectangle ()
  "Cut out the rectangle between the mark and the cursor, for C-x r y."
  (let ((view (current-view)))
    (multiple-value-bind (first last left right) (rectangle-bounds view)
      (setf *killed-rectangle* (rectangle-strings (view-gtk-buffer view) first last left right))
      (kill-new (format nil "~{~a~^~%~}" *killed-rectangle*)))
    (map-rectangle-lines view #'cut-columns)))

(define-command delete-rectangle ()
  "Delete the rectangle between the mark and the cursor."
  (map-rectangle-lines (current-view) #'cut-columns))

(define-command clear-rectangle ()
  "Blank out the rectangle between the mark and the cursor with spaces."
  (map-rectangle-lines (current-view)
                       (lambda (line left right)
                         (cut-columns line left right (make-string (- right left) :initial-element #\Space)))))

(define-command open-rectangle ()
  "Push the rectangle between the mark and the cursor right, leaving spaces in its place."
  (map-rectangle-lines (current-view)
                       (lambda (line left right)
                         (let ((padded (pad-to line left)))
                           (concatenate 'string (subseq padded 0 left)
                                        (make-string (- right left) :initial-element #\Space)
                                        (subseq padded left))))))

(define-command string-rectangle ()
  "Replace each line of the rectangle between the mark and the cursor with a string."
  (let ((view (current-view)))
    (rectangle-bounds view)             ; complain now if there's no region
    (open-picker (window-picker *window*)
                 :placeholder "Replace the rectangle with:"
                 :on-choose (lambda (string)
                              (map-rectangle-lines view (lambda (line left right) (cut-columns line left right string)))))))

(define-command yank-rectangle ()
  "Insert the last killed rectangle with its top left at the cursor."
  (unless *killed-rectangle* (editor-error "No rectangle has been killed"))
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (with-user-action (gtk-buffer)
        ;; Lines the rectangle runs past the end of the text are added.
        (let ((missing (- (+ line (length *killed-rectangle*)) (gtk:text-buffer-get-line-count gtk-buffer))))
          (when (plusp missing)
            (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer)
                                    (make-string missing :initial-element #\Newline) -1)))
        (loop for piece in *killed-rectangle*
              for l from line
              do (let ((text (text-line-string gtk-buffer l)))
                   (replace-lines gtk-buffer l l
                                  (list (string-right-trim " " (concatenate 'string (subseq (pad-to text column) 0 column)
                                                                            piece (subseq (pad-to text column) column)))))))))))

;;; The REPL's history: arrows at the prompt, and kept between sessions

(defun repl-history-file () (merge-pathnames "repl-history.sexp" (state-directory)))

(defparameter *repl-history-max* 500)

(defun repl-history-key ()
  (if (repl-evaluator *repl*)
      "cadre-repl"
      (or (and *window* (window-project *window*) (uiop:native-namestring (window-project *window*))) "none")))

(defun read-repl-histories ()
  (ignore-errors
   (with-open-file (in (repl-history-file) :if-does-not-exist nil)
     (and in (with-standard-io-syntax (let ((*read-eval* nil)) (read in nil nil)))))))

(defun ensure-repl-history-loaded ()
  "Bring in the history saved for this REPL and project, once."
  (let ((buffer (repl-buffer *repl*)))
    (unless (equal (buffer-local buffer :history-loaded) (repl-history-key))
      (setf (buffer-local buffer :history-loaded) (repl-history-key))
      (let ((saved (cdr (assoc (repl-history-key) (read-repl-histories) :test #'equal)))
            (history (repl-history *repl*)))
        (let ((now (coerce history 'list)))
          (setf (fill-pointer history) 0)
          (dolist (input (append (remove-if (lambda (s) (member s now :test #'string=)) saved) now))
            (vector-push-extend input history)))))))

(defun save-repl-history ()
  (let* ((history (coerce (repl-history *repl*) 'list))
         (history (last history *repl-history-max*))
         (all (cons (cons (repl-history-key) history)
                    (remove (repl-history-key) (read-repl-histories) :key #'car :test #'equal))))
    (ignore-errors
     (ensure-directories-exist (repl-history-file))
     (with-open-file (out (repl-history-file) :direction :output :if-exists :supersede)
       (with-standard-io-syntax (prin1 all out))))))

(defun input-line-bounds ()
  "Whether the cursor is on the input's first line, and on its last."
  (let* ((gtk-buffer (repl-gtk-buffer))
         (input-start (mark-iter (repl-input-mark *repl*)))
         (cursor (cursor-iter gtk-buffer)))
    (values (and (>= (gtk:text-iter-get-offset cursor) (gtk:text-iter-get-offset input-start))
                 (= (gtk:text-iter-get-line cursor) (gtk:text-iter-get-line input-start)))
            (and (>= (gtk:text-iter-get-offset cursor) (gtk:text-iter-get-offset input-start))
                 (= (gtk:text-iter-get-line cursor) (gtk:text-iter-get-line (gtk:text-buffer-get-end-iter gtk-buffer)))))))

(define-command repl-up ()
  "On the input's first line, the previous input from the history; elsewhere, up a line."
  (:modes repl-mode editor-repl-mode)
  (with-buffer-repl ()
    (if (and *repl* (nth-value 0 (input-line-bounds)))
        (repl-history-step -1)
        (move :display-lines -1))))

(define-command repl-down ()
  "On the input's last line, the next input from the history; elsewhere, down a line."
  (:modes repl-mode editor-repl-mode)
  (with-buffer-repl ()
    (if (and *repl* (nth-value 1 (input-line-bounds)) (repl-history-index *repl*))
        (repl-history-step 1)
        (move :display-lines 1))))
