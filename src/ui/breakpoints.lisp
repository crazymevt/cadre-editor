;;;; breakpoints.lisp — breakpoints in the gutter, compiled in
;;;;
;;;; Clicking left of a Common Lisp file's line numbers (or F9) sets a
;;;; breakpoint: a red dot on the line of the statement it stops at (see
;;;; core/lisp/breakpoints.lisp). If a Lisp is connected, the top-level form
;;;; around it is compiled again at once, with (break …) before that
;;;; statement, so running it opens the debugger there; Continue goes on.
;;;; Compiling the form or the file later keeps the breaks in, and removing
;;;; a form's last breakpoint compiles it again without.
;;;;
;;;; Breakpoints are marks at the start of their lines, so they move with
;;;; edits. Positions Swank reports in code compiled with breaks in (notes,
;;;; the debugger's frames) are mapped back past the inserted text.

(in-package #:cadre-ui)

(defun breakpoint-lines (buffer)
  "The lines with breakpoints in BUFFER, in order."
  (let ((gtk-buffer (buffer-text buffer)))
    (sort (remove-duplicates
           (mapcar (lambda (mark) (gtk:text-iter-get-line (gtk:text-buffer-get-iter-at-mark gtk-buffer mark)))
                   (buffer-local buffer :breakpoints)))
          #'<)))

(defun breakpoint-marks-at (buffer line)
  (let ((gtk-buffer (buffer-text buffer)))
    (remove-if-not (lambda (mark) (= line (gtk:text-iter-get-line (gtk:text-buffer-get-iter-at-mark gtk-buffer mark))))
                   (buffer-local buffer :breakpoints))))

(defun add-breakpoint-mark (buffer line)
  (let ((gtk-buffer (buffer-text buffer)))
    (push (gtk:text-buffer-create-mark gtk-buffer nil (line-iter gtk-buffer line) t)
          (buffer-local buffer :breakpoints))))

(defun remove-breakpoint-marks (buffer marks)
  (dolist (mark marks)
    (gtk:text-buffer-delete-mark (buffer-text buffer) mark))
  (setf (buffer-local buffer :breakpoints)
        (set-difference (buffer-local buffer :breakpoints) marks)))

;;; Compiling with breaks in

(defun breakpoint-text (buffer line)
  (format nil "(cl:break ~s) " (format nil "Breakpoint: ~a, line ~d" (buffer-name buffer) (1+ line))))

(defun form-breakpoint-insertions (buffer sl sc el ec)
  "For the form from (SL, SC) to (EL, EC): (offset . text) for each
breakpoint in it, at buffer offsets. A break goes before its statement, or
around it, (progn (break …) statement), where it isn't in a body. Closing
parens come first, for a statement that ends where another starts."
  (let ((syntax (buffer-syntax buffer))
        (text (buffer-text buffer)))
    (when syntax
      (let ((places (remove-duplicates
                     (loop for line in (breakpoint-lines buffer)
                           for place = (and (<= sl line el) (breakpoint-place syntax line))
                           when (and place
                                     (or (> (first place) sl) (and (= (first place) sl) (> (second place) sc)))
                                     (or (< (first place) el) (and (= (first place) el) (< (second place) ec))))
                             collect place)
                     :test #'equal))
            (opens '())
            (closes '()))
        (dolist (place places)
          (destructuring-bind (line column &optional end-line end-column) place
            (let ((start (text-line-position text line column)))
              (if end-line
                  (progn
                    (push (cons start (format nil "(cl:progn ~a" (breakpoint-text buffer line))) opens)
                    (push (cons (text-line-position text end-line end-column) ")") closes))
                  (push (cons start (breakpoint-text buffer line)) opens)))))
        (append (sort closes #'< :key #'car) (sort opens #'< :key #'car))))))

(defun breakpoint-form-text (buffer sl sc el ec text)
  "TEXT, the form from (SL, SC) to (EL, EC) in BUFFER, with a break before
each of its breakpoints' statements. Remembers what went in, to map
positions back."
  (let* ((gtk-buffer (buffer-text buffer))
         (start (text-line-position gtk-buffer sl sc))
         (end (text-line-position gtk-buffer el ec))
         (insertions (form-breakpoint-insertions buffer sl sc el ec)))
    (setf (buffer-local buffer :breakpoint-insertions)
          (append (and insertions (list (list start end insertions)))
                  (remove-if (lambda (group) (<= start (first group) end))
                             (buffer-local buffer :breakpoint-insertions))))
    (if insertions
        (insert-at-offsets text (mapcar (lambda (i) (cons (- (car i) start) (cdr i))) insertions))
        text)))

(defun breakpoint-unshift (buffer position)
  "POSITION, reported by the Lisp for BUFFER's code, as a position in BUFFER:
past any breaks compiled into the form it's in."
  (let ((group (find-if (lambda (group)
                          (destructuring-bind (start end insertions) group
                            (<= start position (+ end (reduce #'+ insertions :key (lambda (i) (length (cdr i))))))))
                        (buffer-local buffer :breakpoint-insertions))))
    (if group (unshift-position position (third group)) position)))

(defun breakpoint-unshift-notes (buffer notes)
  (if (buffer-local buffer :breakpoint-insertions)
      (mapcar (lambda (note)
                (let ((location (copy-list (compiler-note-location note))))
                  (when (getf location :position)
                    (setf (getf location :position) (breakpoint-unshift buffer (getf location :position))))
                  (make-compiler-note (compiler-note-severity note) (compiler-note-message note) location)))
              notes)
      notes))

(defun buffer-view (buffer)
  "A view showing BUFFER, or nil."
  (and *window* (first (buffer-views *window* buffer))))

(defun compile-breakpoint-form (buffer line &key (what "Compiled"))
  "Compile the top-level form around LINE again, with its breakpoints in,
if a Lisp is connected. If the form's package isn't in the Lisp yet (the
file isn't loaded), wait: the breakpoint goes in once something is loaded
that makes the package (APPLY-WAITING-BREAKPOINTS)."
  (let ((syntax (buffer-syntax buffer))
        (view (buffer-view buffer)))
    (unless (connected-p)
      ;; Put in when a Lisp connects (connecting changes the image too).
      (setf (buffer-local buffer :breakpoints-waiting) t))
    (when (and syntax view (connected-p))
      (multiple-value-bind (sl sc el ec) (toplevel-form-bounds syntax line 0)
        (when (and sl (<= sl line el))
          (let ((package (and (not (buffer-local buffer :package)) (buffer-package-name syntax sl))))
            (flet ((compile-it () (compile-form-region view sl sc el ec :what what)))
              (if (null package)
                  (compile-it)
                  (rex *connection* (swank-call "swank:eval-and-grab-output"
                                                (format nil "(cl:if (cl:find-package ~a) :present :absent)"
                                                        (cadre::lisp-string package)))
                       :package "COMMON-LISP-USER"
                       :on-ok (lambda (reply)
                                (if (search "PRESENT" (string-upcase (second reply)))
                                    (compile-it)
                                    (progn
                                      (setf (buffer-local buffer :breakpoints-waiting) t)
                                      (when what
                                        (message "Breakpoint set; it goes in when ~a is loaded (the Lisp has no package ~a yet)"
                                                 (buffer-name buffer) package))))))))))))))

(defun apply-waiting-breakpoints ()
  "After the image changed, put in the breakpoints of files whose package
wasn't there when they were set."
  (dolist (buffer (buffer-list))
    (when (buffer-local buffer :breakpoints-waiting)
      (setf (buffer-local buffer :breakpoints-waiting) nil)
      (when (buffer-local buffer :breakpoints)
        (reapply-breakpoints buffer)))))

(add-hook '*image-changed-hook* 'apply-waiting-breakpoints)

(defun reapply-breakpoints (buffer)
  "Compile again each form in BUFFER with breakpoints, after loading the
file replaced them."
  (let ((syntax (buffer-syntax buffer)))
    (when syntax
      (let ((done '()))
        (dolist (line (breakpoint-lines buffer))
          (let ((start (toplevel-form-bounds syntax line 0)))
            (when (and start (not (member start done)))
              (push start done)
              (compile-breakpoint-form buffer line :what nil))))))))

;;; Setting and clearing them

(defun toggle-breakpoint-at (view line)
  "Set a breakpoint on LINE in VIEW, at the statement it's in, or clear the one there."
  (let* ((buffer (view-buffer view))
         (syntax (or (buffer-syntax buffer) (editor-error "Breakpoints are for Common Lisp files")))
         (existing (breakpoint-marks-at buffer line)))
    (if existing
        (progn (remove-breakpoint-marks buffer existing)
               (message "Breakpoint on line ~d cleared" (1+ line))
               (compile-breakpoint-form buffer line :what "Breakpoint cleared; compiled"))
        (let* ((place (or (breakpoint-place syntax line)
                          (editor-error "Line ~d isn't in a function's body" (1+ line))))
               (target (first place)))
          (if (breakpoint-marks-at buffer target)
              (message "Line ~d already has the breakpoint for this line" (1+ target))
              (progn
                (add-breakpoint-mark buffer target)
                (message "Breakpoint on line ~d~:[~*~;, the statement line ~d is in~]~:[ (it takes effect when the form is compiled)~;~]"
                         (1+ target) (/= target line) (1+ line) (connected-p))
                (compile-breakpoint-form buffer target :what "Breakpoint set; compiled")))))
    (when (view-gutter view) (gtk:widget-queue-draw (view-gutter view)))))

(define-command toggle-breakpoint ()
  "Set a breakpoint at the statement on the cursor's line, or clear it. Running it opens the debugger."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (toggle-breakpoint-at view (cursor-line-column view))))

(define-command clear-breakpoints ()
  "Clear every breakpoint in this file and compile its forms again without them."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (lines (breakpoint-lines buffer)))
    (unless lines (editor-error "No breakpoints in ~a" (buffer-name buffer)))
    (let ((syntax (buffer-syntax buffer))
          (forms '()))
      (dolist (line lines)
        (pushnew (and syntax (toplevel-form-bounds syntax line 0)) forms))
      (remove-breakpoint-marks buffer (buffer-local buffer :breakpoints))
      (dolist (line (remove nil forms))
        (compile-breakpoint-form buffer line :what nil))
      (message "Cleared ~d breakpoint~:p" (length lines)))
    (when (view-gutter view) (gtk:widget-queue-draw (view-gutter view)))))

;;; In the gutter

(defun breakpoint-zone-left (view) (max 6 (blame-width view)))

(defun breakpoint-zone-p (view x)
  "True if X in VIEW's gutter is where a click sets a breakpoint: between
the change marks (and blame) and the fold arrows, in Common Lisp."
  (and (buffer-syntax (view-buffer view))
       (>= x (breakpoint-zone-left view))
       (not (fold-zone-p view x))))

(defun draw-breakpoint (view cr wy height)
  "Draw a breakpoint's dot in VIEW's gutter, for the line drawn at WY."
  (cairo:set-source-rgba cr 0.89d0 0.26d0 0.2d0 1d0)
  (cairo:arc cr (float (+ (breakpoint-zone-left view) 5) 1d0) (float (+ wy (/ height 2)) 1d0)
             4.5d0 0d0 (* 2 pi))
  (cairo:fill cr))

(defun setup-breakpoint-clicks (view)
  "Clicking left of the line numbers sets or clears a breakpoint."
  (let ((gutter (view-gutter view)))
    (when gutter
      (let ((click (gtk:gesture-click-new)))
        (gobject:connect click :pressed
                         (lambda (gesture n x y)
                           (declare (ignore n))
                           (when (breakpoint-zone-p view x)
                             (let ((line (gutter-line-at view y)))
                               (gtk:gesture-set-state gesture :claimed)
                               (handler-case (toggle-breakpoint-at view line)
                                 (editor-error (e) (message "~a" (editor-error-message e))))))))
        (gtk:widget-add-controller gutter click)))))
