;;;; macroexpand.lisp — showing what a macro form expands into
;;;;
;;;; The expansion of the form at the cursor goes to a *Macroexpansion* tab,
;;;; in Lisp mode, read in the original buffer's package. Expanding again
;;;; inside that tab expands the form there in place, so you can step into
;;;; an expansion.

(in-package #:cadre-ui)

(defun form-for-expansion (view)
  "The bounds of the form to expand: the list starting at the cursor, the
one just before it, or else the list around it."
  (let ((syntax (or (buffer-sexp-syntax (view-buffer view)) (editor-error "Not a Lisp buffer")))
        (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (let* ((offset (text-point gtk-buffer))
             (after (and (< offset (text-length gtk-buffer)) (text-char gtk-buffer offset)))
             (before (and (plusp offset) (text-char gtk-buffer (1- offset)))))
        (cond ((eql after #\()
               (multiple-value-bind (el ec) (forward-sexp-position syntax line column)
                 (and el (values line column el ec))))
              ((eql before #\))
               (multiple-value-bind (sl sc) (backward-sexp-position syntax line column)
                 (and sl (values sl sc line column))))
              (t (multiple-value-bind (ol oc) (up-list-position syntax line column)
                   (when ol
                     (multiple-value-bind (el ec) (forward-sexp-position syntax ol oc)
                       (and el (values ol oc el ec)))))))))))

(defun macroexpansion-buffer (&optional (mode 'lisp-mode))
  "The *Macroexpansion* tab, in MODE (the expanded code's language)."
  (let ((buffer (or (find-buffer "*Macroexpansion*")
                    (make-buffer :name "*Macroexpansion*" :text (make-gtk-text) :major-mode mode))))
    (unless (eq (buffer-major-mode buffer) mode)
      (setf (buffer-major-mode buffer) mode)
      (attach-syntax buffer))
    buffer))

(defun expand-form (&rest kind)
  "Expand the form at the cursor; KIND is passed to BACKEND-MACROEXPAND (:all t, :compiler t)."
  (let ((view (current-view)))
    (multiple-value-bind (sl sc el ec) (form-for-expansion view)
      (unless sl (editor-error "No form at the cursor"))
      (let ((text (view-region-text view sl sc el ec))
            (package (view-package view))
            (in-place (eq (view-buffer view) (find-buffer "*Macroexpansion*"))))
        (flash-region view sl sc el ec)
        (with-connection (connection :language (view-language view))
          (apply #'request #'backend-macroexpand connection text :package package
                 :on-ok (lambda (expansion)
                          (let ((expansion (string-right-trim '(#\Newline #\Space) expansion)))
                            (if in-place
                                (replace-with-expansion view sl sc el ec expansion)
                                (show-expansion expansion package (buffer-major-mode (view-buffer view))))))
                 kind))))))

(defun show-expansion (expansion package &optional (mode 'lisp-mode))
  (let ((buffer (macroexpansion-buffer (if (lisp-family-mode-p mode) mode 'lisp-mode))))
    (setf (buffer-local buffer :package) package)
    (text-replace-contents (buffer-text buffer) (format nil "~a~%" expansion))
    (let ((view (show-buffer *window* buffer)))
      (gtk:text-view-set-editable (view-text-view view) nil)
      (gtk:text-buffer-place-cursor (view-gtk-buffer view) (gtk:text-buffer-get-start-iter (view-gtk-buffer view))))))

(defun replace-with-expansion (view sl sc el ec expansion)
  (let ((gtk-buffer (view-gtk-buffer view)))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete gtk-buffer (line-iter gtk-buffer sl sc) (line-iter gtk-buffer el ec))
      (gtk:text-buffer-insert gtk-buffer (line-iter gtk-buffer sl sc) expansion -1))
    (indent-lines view sl (+ sl (count #\Newline expansion)))
    (goto-line-column view sl sc :extend nil)))

(define-command expand-macro-once ()
  "Expand the macro form at the cursor once."
  (:modes lisp-mode racket-mode clojure-mode)
  (expand-form))

(define-command expand-macro-all ()
  "Expand the form at the cursor completely, including every macro inside it."
  (:modes lisp-mode racket-mode clojure-mode)
  (expand-form :all t))

(define-command expand-compiler-macro ()
  "Expand the form at the cursor with its compiler macro, if it has one."
  (:modes lisp-mode)
  (expand-form :compiler t))
