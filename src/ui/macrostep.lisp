;;;; macrostep.lisp — expanding macros in place, in the source
;;;;
;;;; Like Emacs's macrostep: Expand in Place replaces the macro form at the
;;;; cursor with its expansion (once), shaded, right in the file; expanding
;;;; a form inside an expansion steps further in. Collapse folds the last
;;;; expansion back, and Collapse All all of them. While anything is
;;;; expanded the buffer is read-only, so the expansions are the only
;;;; changes: each is one undo step, and collapsing undoes it. Undo collapses
;;;; too, the history before stays, and saving collapses first, so an
;;;; expansion never reaches the file.

(in-package #:cadre-ui)

(defstruct (macrostep (:constructor make-macrostep (start end)))
  start end)                            ; marks around the expansion

(defvar *macrostepping* nil "True while Cadre itself expands or collapses, so the change isn't the user's.")

(defun macrosteps (buffer) (buffer-local buffer :macrosteps))

(defun macrostep-tag (gtk-buffer)
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (or (gtk:text-tag-table-lookup table "cadre-macrostep")
        (let ((tag (make-instance 'gtk:text-tag :name "cadre-macrostep"
                                                :paragraph-background "rgba(110,150,255,0.13)")))
          (gtk:text-tag-table-add table tag)
          tag))))

(defun set-buffer-views-editable (buffer editable)
  (dolist (view (buffer-views *window* buffer))
    (when (view-text-view view)
      (gtk:text-view-set-editable (view-text-view view) editable))))

(defun macrostep-expand-at (view sl sc el ec expansion)
  "Replace the form from (SL, SC) to (EL, EC) with EXPANSION, as one undo step."
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (first (null (macrosteps buffer))))
    (when first
      (setf (buffer-local buffer :macrostep-modified) (buffer-modified-p buffer))
      (unless (buffer-local buffer :macrostep-watching)
        (setf (buffer-local buffer :macrostep-watching) t)
        (gobject:connect gtk-buffer :changed
                         (lambda (b) (declare (ignore b))
                           (unless *macrostepping* (setf (buffer-local buffer :macrostep-redo) nil))))))
    (let ((start (gtk:text-buffer-create-mark gtk-buffer nil (line-iter gtk-buffer sl sc) t))
          (*macrostepping* t))
      (with-user-action (gtk-buffer)
        (gtk:text-buffer-delete gtk-buffer (line-iter gtk-buffer sl sc) (line-iter gtk-buffer el ec))
        (gtk:text-buffer-insert gtk-buffer (line-iter gtk-buffer sl sc) expansion -1)
        (indent-lines view sl (+ sl (count #\Newline expansion))))
      (let* ((start-iter (gtk:text-buffer-get-iter-at-mark gtk-buffer start))
             (end-line (+ sl (count #\Newline expansion)))
             (syntax (buffer-sexp-syntax buffer))
             (end-iter (multiple-value-bind (fl fc) (forward-sexp-position syntax sl (gtk:text-iter-get-line-offset start-iter))
                         (if fl (line-iter gtk-buffer fl fc) (line-iter gtk-buffer end-line 0))))
             (end (gtk:text-buffer-create-mark gtk-buffer nil end-iter nil)))
        (gtk:text-buffer-apply-tag gtk-buffer (macrostep-tag gtk-buffer) start-iter end-iter)
        (push (make-macrostep start end) (buffer-local buffer :macrosteps))))
    (set-buffer-views-editable buffer nil)
    (goto-line-column view sl sc :extend nil)))

(defun macrostep-collapse-last (buffer)
  "Undo the last expansion. True if there was one."
  (let ((step (pop (buffer-local buffer :macrosteps)))
        (gtk-buffer (buffer-text buffer)))
    (when step
      (let ((*macrostepping* t))
        (gtk:text-buffer-undo gtk-buffer))
      ;; Redo would bring the expansion back as plain text: not until the user changes something.
      (setf (buffer-local buffer :macrostep-redo) t)
      (gtk:text-buffer-delete-mark gtk-buffer (macrostep-start step))
      (gtk:text-buffer-delete-mark gtk-buffer (macrostep-end step))
      (when (null (macrosteps buffer))
        (set-buffer-views-editable buffer t)
        (setf (buffer-modified-p buffer) (buffer-local buffer :macrostep-modified)))
      t)))

(defun macrostep-collapse-all (buffer)
  (loop while (macrostep-collapse-last buffer) count t))

(defun macrostep-expand (view)
  "Expand the macro form at the cursor in place."
  (multiple-value-bind (sl sc el ec) (form-for-expansion view)
    (unless sl (editor-error "No form at the cursor"))
    (let ((text (view-region-text view sl sc el ec)))
      (with-connection (connection :language (view-language view))
        (request #'backend-macroexpand connection text :package (view-package view)
                 :on-ok (lambda (expansion)
                          (let ((expansion (string-trim '(#\Newline #\Space) expansion)))
                            (cond ((string= (cadre::collapse-whitespace expansion) (cadre::collapse-whitespace text))
                                   (message "Not a macro form: it expands to itself"))
                                  ;; The text may have changed while the Lisp was asked.
                                  ((not (equal text (ignore-errors (view-region-text view sl sc el ec))))
                                   (message "The form changed; expand again"))
                                  (t (macrostep-expand-at view sl sc el ec expansion)
                                     (message "Expanded in place (read-only until collapsed): Collapse or Undo folds it back"))))))))))

;;; Saving, undo and closing

(add-hook '*before-save-hook* (lambda (buffer)
                                (when (macrosteps buffer)
                                  (macrostep-collapse-all buffer)
                                  (message "Collapsed the macro expansions to save"))))

(add-hook '*buffer-killed-hook* (lambda (buffer) (setf (buffer-local buffer :macrosteps) nil)))

(defun macrostep-undo (buffer)
  "Undo while expanded: collapse the last expansion. True if it did."
  (and (macrosteps buffer) (macrostep-collapse-last buffer)))

(defun macrostep-blocks-redo-p (buffer)
  "True if Redo would only bring back an expansion that was collapsed."
  (or (macrosteps buffer) (buffer-local buffer :macrostep-redo)))

;;; Commands

(define-command macrostep-expand-in-place ()
  "Expand the macro form at the cursor in place, in the source: shaded,
read-only, and folded back with Collapse. Expand forms inside it to step in."
  (:modes lisp-mode racket-mode clojure-mode)
  (macrostep-expand (current-view)))

(define-command macrostep-collapse ()
  "Fold the last macro expansion back into the form it came from."
  (:modes lisp-mode racket-mode clojure-mode)
  (unless (macrostep-collapse-last (current-buffer))
    (message "Nothing is expanded")))

(define-command macrostep-collapse-all-expansions ()
  "Fold every macro expansion in the buffer back."
  (:modes lisp-mode racket-mode clojure-mode)
  (let ((n (macrostep-collapse-all (current-buffer))))
    (if (plusp n)
        (message "Collapsed ~d expansion~:p" n)
        (message "Nothing is expanded"))))
