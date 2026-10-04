;;;; folding.lisp — hiding a form or a section behind its first line
;;;;
;;;; The ranges that can fold come from the core (lisp-fold-ranges,
;;;; markdown-fold-ranges), worked out again a moment after each edit. A
;;;; fold hides its lines after the first, newlines included (so they take
;;;; no room), with an invisible tag between two marks, so it follows edits
;;;; elsewhere. At the end of the text, where the last line has no newline,
;;;; it hides from the end of the first line instead. The gutter shows ▸ on folded lines, and ▾ on lines that can
;;;; fold while the pointer is over it; clicking either toggles.
;;;;
;;;; Folded text is never edited unseen: moving the cursor into it (by
;;;; searching, going to a line or a definition…) or changing it unfolds it.

(in-package #:cadre-ui)

(defstruct (fold (:constructor make-fold (start end tail)))
  start end                             ; marks around the hidden text
  tail)                                 ; true if it starts at the end of the first line

(defun buffer-folds (buffer) (buffer-local buffer :folds))

(defun folded-tag (gtk-buffer)
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (or (gtk:text-tag-table-lookup table "cadre-folded")
        (let ((tag (make-instance 'gtk:text-tag :name "cadre-folded" :invisible t)))
          (gtk:text-tag-table-add table tag)
          tag))))

(defun fold-bounds (gtk-buffer fold)
  "FOLD's hidden text as two offsets."
  (values (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-start fold)))
          (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-end fold)))))

(defun fold-first-line (gtk-buffer fold)
  (let ((line (gtk:text-iter-get-line (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-start fold)))))
    (if (fold-tail fold) line (1- line))))

(defun fold-hides-offset-p (gtk-buffer fold offset)
  "True if the cursor at OFFSET would be in FOLD's hidden text."
  (multiple-value-bind (start end) (fold-bounds gtk-buffer fold)
    (if (fold-tail fold)
        (< start offset (1+ end))
        (<= start offset (1- end)))))

(defun fold-at-line (buffer line)
  "The fold whose first line is LINE, or nil."
  (let ((gtk-buffer (buffer-text buffer)))
    (find line (buffer-folds buffer) :key (lambda (f) (fold-first-line gtk-buffer f)))))

;;; The ranges that can fold

(defun compute-fold-ranges (buffer)
  (let ((syntax (buffer-syntax buffer)))
    (cond (syntax (lisp-fold-ranges syntax))
          ((buffer-ts-document buffer) (ts-fold-ranges (fresh-ts-document buffer)))
          ((eq (buffer-major-mode buffer) 'markdown-mode) (markdown-fold-ranges (buffer-string buffer)))
          (t '()))))

(defun fold-ranges (buffer &key fresh)
  "BUFFER's fold ranges: the ones worked out last, or, with FRESH, up to date."
  (when (or fresh (eq (buffer-local buffer :fold-ranges) :unknown) (null (buffer-local buffer :fold-ranges-by-line)))
    (let ((ranges (compute-fold-ranges buffer))
          (by-line (make-hash-table)))
      (dolist (r ranges) (setf (gethash (first r) by-line) r))
      (setf (buffer-local buffer :fold-ranges) ranges
            (buffer-local buffer :fold-ranges-by-line) by-line)))
  (buffer-local buffer :fold-ranges))

(defun fold-range-starting (buffer line)
  (fold-ranges buffer)
  (gethash line (buffer-local buffer :fold-ranges-by-line)))

(defparameter *fold-ranges-delay* 400 "Milliseconds after an edit before fold ranges are worked out again.")

(defun schedule-fold-ranges (buffer)
  (let ((timer (buffer-local buffer :fold-ranges-timer)))
    (when timer (glib:source-remove timer)))
  (setf (buffer-local buffer :fold-ranges-timer)
        (glib:timeout-add glib:+priority-default-idle+ *fold-ranges-delay*
                          (lambda ()
                            (setf (buffer-local buffer :fold-ranges-timer) nil)
                            (when (member buffer (buffer-list))
                              (fold-ranges buffer :fresh t)
                              (redraw-gutters buffer))
                            nil))))

;;; Folding and unfolding

(defun header-bounds (gtk-buffer fold)
  "FOLD's first line, as two iters."
  (let* ((line (fold-first-line gtk-buffer fold))
         (start (line-iter gtk-buffer line))
         (end (line-iter gtk-buffer line)))
    (unless (gtk:text-iter-ends-line end) (gtk:text-iter-forward-to-line-end end))
    (values start end)))

(defun apply-folds (buffer)
  "Hide the text of every fold in BUFFER (after one was removed, as folds may
nest), and mark their first lines."
  (let* ((gtk-buffer (buffer-text buffer))
         (tag (folded-tag gtk-buffer))
         (header (ensure-face-tag gtk-buffer "cadre-fold-header" :fold-header)))
    (dolist (fold (buffer-folds buffer))
      (gtk:text-buffer-apply-tag gtk-buffer tag
                                 (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-start fold))
                                 (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-end fold)))
      (multiple-value-bind (start end) (header-bounds gtk-buffer fold)
        (gtk:text-buffer-apply-tag gtk-buffer header start end)))))

(defun fold-lines (buffer first last)
  "Hide lines FIRST+1 to LAST of BUFFER behind line FIRST."
  (let* ((gtk-buffer (buffer-text buffer))
         (tail (>= (1+ last) (gtk:text-buffer-get-line-count gtk-buffer)))
         (start (if tail
                    (let ((it (line-iter gtk-buffer first)))
                      (unless (gtk:text-iter-ends-line it) (gtk:text-iter-forward-to-line-end it))
                      it)
                    (line-iter gtk-buffer (1+ first))))
         (end (if tail (gtk:text-buffer-get-end-iter gtk-buffer) (line-iter gtk-buffer (1+ last))))
         (header-end (let ((it (line-iter gtk-buffer first)))
                       (unless (gtk:text-iter-ends-line it) (gtk:text-iter-forward-to-line-end it))
                       it)))
    (when (fold-at-line buffer first) (unfold buffer (fold-at-line buffer first)))
    (let ((fold (make-fold (gtk:text-buffer-create-mark gtk-buffer nil start nil)
                           (gtk:text-buffer-create-mark gtk-buffer nil end t)
                           tail)))
      ;; The cursor can't stay in text about to be hidden.
      (when (fold-hides-offset-p gtk-buffer fold (gtk:text-iter-get-offset (cursor-iter gtk-buffer)))
        (gtk:text-buffer-place-cursor gtk-buffer header-end))
      (push fold (buffer-local buffer :folds)))
    (apply-folds buffer)
    (redraw-gutters buffer)))

(defun unfold (buffer fold)
  (let ((gtk-buffer (buffer-text buffer)))
    (setf (buffer-local buffer :folds) (remove fold (buffer-folds buffer)))
    (unless (gtk:text-mark-get-deleted (fold-start fold))
      (multiple-value-bind (start end) (header-bounds gtk-buffer fold)
        (gtk:text-buffer-remove-tag-by-name gtk-buffer "cadre-fold-header" start end))
      (gtk:text-buffer-remove-tag gtk-buffer (folded-tag gtk-buffer)
                                  (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-start fold))
                                  (gtk:text-buffer-get-iter-at-mark gtk-buffer (fold-end fold)))
      (gtk:text-buffer-delete-mark gtk-buffer (fold-start fold))
      (gtk:text-buffer-delete-mark gtk-buffer (fold-end fold)))
    (apply-folds buffer)
    (redraw-gutters buffer)))

(defun unfold-all-in (buffer)
  (dolist (fold (buffer-folds buffer)) (unfold buffer fold)))

(defun unfold-overlapping (buffer from to)
  "Unfold the folds whose hidden text overlaps the offsets FROM to TO."
  (let ((gtk-buffer (buffer-text buffer)))
    (dolist (fold (buffer-folds buffer))
      (multiple-value-bind (start end) (fold-bounds gtk-buffer fold)
        (when (and (< from end) (> to start))
          (unfold buffer fold))))))

(defun unfold-at-offset (buffer offset)
  "Unfold the folds hiding OFFSET."
  (let ((gtk-buffer (buffer-text buffer)))
    (dolist (fold (buffer-folds buffer))
      (when (fold-hides-offset-p gtk-buffer fold offset)
        (unfold buffer fold)))))

(defun attach-folding (buffer)
  "Keep BUFFER's fold ranges current, and unfold what the cursor or an edit reaches."
  (let ((gtk-buffer (buffer-text buffer)))
    (when (typep gtk-buffer 'gtk:text-buffer)
      (setf (buffer-local buffer :fold-ranges) :unknown)
      (gobject:connect gtk-buffer :changed
                       (lambda (b) (declare (ignore b)) (schedule-fold-ranges buffer)))
      (gobject:connect gtk-buffer "notify::cursor-position"
                       (lambda (b pspec) (declare (ignore pspec))
                         (when (buffer-folds buffer)
                           (unfold-at-offset buffer (gtk:text-iter-get-offset (cursor-iter b))))))
      (gobject:connect gtk-buffer :insert-text
                       (lambda (b location text length)
                         (declare (ignore b text length))
                         (when (buffer-folds buffer)
                           (unfold-at-offset buffer (gtk:text-iter-get-offset location)))))
      (gobject:connect gtk-buffer :delete-range
                       (lambda (b start end)
                         (declare (ignore b))
                         (when (buffer-folds buffer)
                           (unfold-overlapping buffer (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end))))))))

(add-hook '*buffer-created-hook* 'attach-folding)

;;; The gutter

(defun fold-zone-p (view x)
  "True if X, in VIEW's gutter, is where the fold arrows are."
  (let ((gutter (view-gutter view)))
    (and gutter (>= x (- (gtk:widget-get-width gutter) *fold-gutter-width* 4)))))

(defun draw-fold-arrow (view cr layout line y width)
  "Draw ▸ if LINE is folded, or ▾ if it can fold and the pointer is over the gutter."
  (let* ((buffer (view-buffer view))
         (folded (and (buffer-folds buffer) (fold-at-line buffer line))))
    (when (or folded (and (buffer-local buffer :gutter-hover) (fold-range-starting buffer line)))
      (let ((color (gtk:widget-get-color (view-gutter view))))
        (pango:layout-set-text layout (if folded "▸" "▾") -1)
        (cairo:set-source-rgba cr (gdk:rgba-red color) (gdk:rgba-green color) (gdk:rgba-blue color)
                               (if folded 0.9d0 0.5d0))
        (cairo:move-to cr (float (- width *fold-gutter-width*) 1d0) (float y 1d0))
        (pango-cairo:show-layout cr layout)))))

(defun setup-fold-gutter (view)
  "Clicking a fold arrow toggles it; the arrows of lines that can fold show
while the pointer is over the gutter."
  (let ((gutter (view-gutter view)))
    (when gutter
      (let ((click (gtk:gesture-click-new))
            (motion (gtk:event-controller-motion-new)))
        (gobject:connect click :pressed
                         (lambda (gesture n x y)
                           (declare (ignore n))
                           (when (fold-zone-p view x)
                             (let* ((text-view (view-text-view view))
                                    (by (nth-value 1 (gtk:text-view-window-to-buffer-coords text-view :left 0 (round y))))
                                    (line (gtk:text-iter-get-line (gtk:text-view-get-line-at-y text-view by))))
                               (when (toggle-fold-at view line)
                                 (gtk:gesture-set-state gesture :claimed))))))
        (gobject:connect motion :enter (lambda (c x y) (declare (ignore c x y))
                                         (setf (buffer-local (view-buffer view) :gutter-hover) t)
                                         (gtk:widget-queue-draw gutter)))
        (gobject:connect motion :leave (lambda (c) (declare (ignore c))
                                         (setf (buffer-local (view-buffer view) :gutter-hover) nil)
                                         (gtk:widget-queue-draw gutter)))
        (gtk:widget-add-controller gutter click)
        (gtk:widget-add-controller gutter motion)))))

;;; Commands

(defun toggle-fold-at (view line)
  "Unfold the fold on LINE, or fold the innermost range around it. True if
something changed."
  (let* ((buffer (view-buffer view))
         (fold (fold-at-line buffer line)))
    (if fold
        (progn (unfold buffer fold) t)
        (let ((range (fold-range-at (fold-ranges buffer :fresh t) line)))
          (when range
            (fold-lines buffer (first range) (second range))
            t)))))

(defun foldable-buffer-p (buffer)
  (or (buffer-syntax buffer) (eq (buffer-major-mode buffer) 'markdown-mode) (buffer-ts-document buffer)))

(defun cursor-line (view)
  (gtk:text-iter-get-line (cursor-iter (view-gtk-buffer view))))

(define-command toggle-fold ()
  "Fold the form or section around the cursor, or unfold it if it is folded."
  (let ((view (current-view)))
    (unless (toggle-fold-at view (cursor-line view))
      (editor-error "Nothing to fold here"))))

(define-command fold-block ()
  "Fold the form or section around the cursor; again, fold the one around that."
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (line (cursor-line view))
         (ranges (fold-ranges buffer :fresh t))
         ;; Already folded here: fold the range around this one.
         (range (if (fold-at-line buffer line)
                    (fold-range-at (remove line ranges :key #'first) line)
                    (fold-range-at ranges line))))
    (unless range (editor-error "Nothing to fold here"))
    (fold-lines buffer (first range) (second range))))

(define-command unfold-block ()
  "Unfold the fold on the cursor's line."
  (let* ((view (current-view))
         (fold (fold-at-line (view-buffer view) (cursor-line view))))
    (unless fold (editor-error "Nothing folded here"))
    (unfold (view-buffer view) fold)))

(define-command fold-all ()
  "Fold every top-level form (or every top section of a Markdown file)."
  (let* ((buffer (view-buffer (current-view)))
         (ranges (fold-ranges buffer :fresh t))
         (outermost (remove-if (lambda (r)
                                 (find-if (lambda (o) (and (not (eq o r)) (<= (first o) (first r)) (>= (second o) (second r))))
                                          ranges))
                               ranges)))
    (unless (foldable-buffer-p buffer) (editor-error "This buffer has nothing to fold"))
    (unfold-all-in buffer)
    (dolist (range outermost) (fold-lines buffer (first range) (second range)))
    (message "Folded ~d" (length outermost))))

(define-command unfold-all ()
  "Unfold everything in this buffer."
  (let ((buffer (view-buffer (current-view))))
    (unfold-all-in buffer)))
