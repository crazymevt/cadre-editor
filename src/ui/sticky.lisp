;;;; sticky.lisp — the first line of the form being read, kept at the top
;;;;
;;;; When the start of the top-level form at the top of an editor has
;;;; scrolled out of sight, its first line stays pinned above the text, as
;;;; VS Code's sticky scroll does. In Lisp the form is the top-level one;
;;;; in JavaScript, TypeScript and CSS, it's the unindented line that opens
;;;; the block. Clicking the header goes to that line. *sticky-scroll* turns
;;;; it off.
;;;;
;;;; The header is an overlay child of the text view, moved to the top of
;;;; the visible part whenever the view scrolls.

(in-package #:cadre-ui)

(define-option *sticky-scroll* t boolean
  "Keep the first line of the definition you're reading at the top of the editor as you scroll past it."
  :category "Editing")

(defparameter *sticky-indentation-modes* '(javascript-mode typescript-mode tsx-mode css-mode)
  "Modes whose sticky header is the unindented line that opens the block.")

(defparameter *sticky-max-characters* 200)

(defun sticky-line (view top-line)
  "The line to pin above TOP-LINE in VIEW, or nil."
  (let* ((buffer (view-buffer view))
         (syntax (buffer-sexp-syntax buffer)))
    (cond (syntax (enclosing-toplevel-line syntax top-line))
          ((member (buffer-major-mode buffer) *sticky-indentation-modes*)
           (indentation-context-line (view-gtk-buffer view) top-line)))))

(defun sticky-text (view line)
  (let* ((string (substitute #\Space #\Tab (string-right-trim '(#\Space #\Tab #\Return)
                                                             (text-line-string (view-gtk-buffer view) line)))))
    (if (> (length string) *sticky-max-characters*)
        (concatenate 'string (subseq string 0 (1- *sticky-max-characters*)) "…")
        string)))

(defun ensure-sticky-header (view)
  "VIEW's header widget, made the first time it is needed."
  (or (view-sticky view)
      (let* ((label (make-instance 'gtk:label :xalign 0f0 :single-line-mode t
                                              :css-classes '("monospace")))
             (box (make-instance 'gtk:box :css-classes '("cadre-sticky") :visible nil
                                          :overflow :hidden
                                          :tooltip-text "Go to the start of this definition"))
             (click (gtk:gesture-click-new)))
        (gtk:box-append box label)
        (gobject:connect click :pressed
                         (lambda (gesture n x y)
                           (declare (ignore n x y))
                           (let ((line (third (view-sticky view))))
                             (when line
                               (gtk:gesture-set-state gesture :claimed)
                               (goto-line-column view line 0 :extend nil)
                               (gtk:widget-grab-focus (view-text-view view))))))
        (gtk:widget-add-controller box click)
        (gtk:text-view-add-overlay (view-text-view view) box 0 0)
        (setf (view-sticky view) (list box label nil)))))

(defun hide-sticky-header (view)
  (let ((sticky (view-sticky view)))
    (when (and sticky (third sticky))
      (setf (third sticky) nil)
      (gtk:widget-set-visible (first sticky) nil)
      (when (view-gutter view) (gtk:widget-queue-draw (view-gutter view))))))

(defun update-sticky-header (view)
  "Show, move or hide VIEW's header for what is at the top of it now."
  (let* ((text-view (view-text-view view))
         (rect (gtk:text-view-get-visible-rect text-view))
         (top (gdk:rectangle-y rect))
         (top-line (and *sticky-scroll* (plusp top)
                        (gtk:text-iter-get-line (gtk:text-view-get-line-at-y text-view top))))
         (line (and top-line (ignore-errors (sticky-line view top-line)))))
    (if (null line)
        (hide-sticky-header view)
        (destructuring-bind (box label shown) (ensure-sticky-header view)
          (unless (eql line shown)
            (gtk:label-set-text label (sticky-text view line))
            (setf (third (view-sticky view)) line)
            (when (view-gutter view) (gtk:widget-queue-draw (view-gutter view))))
          (gtk:widget-set-margin-start label (gtk:text-view-get-left-margin text-view))
          (gtk:widget-set-size-request box (gdk:rectangle-width rect) -1)
          (gtk:text-view-move-overlay text-view box (gdk:rectangle-x rect) top)
          (gtk:widget-set-visible box t)))))

(defvar *sticky-updates-pending* (make-hash-table :test 'eq :weakness :key)
  "Views whose header is to be brought up to date when GTK is idle.")

(defun schedule-sticky-update (view)
  "Update VIEW's header after an edit, once GTK has laid the text out again."
  (when (and *sticky-scroll* (view-source-controllers-p view)
             (not (gethash view *sticky-updates-pending*)))
    (setf (gethash view *sticky-updates-pending*) t)
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (remhash view *sticky-updates-pending*)
                     (update-sticky-header view)
                     nil))))

(defun setup-sticky-header (view)
  "Keep VIEW's sticky header in step with its scrolling and size."
  (let ((adjustment (gtk:scrolled-window-get-vadjustment (view-widget view))))
    (gobject:connect adjustment :value-changed
                     (lambda (a) (declare (ignore a)) (update-sticky-header view)))
    (gobject:connect adjustment :changed
                     (lambda (a) (declare (ignore a)) (schedule-sticky-update view)))))

(defun refresh-sticky-headers ()
  "Apply *sticky-scroll* to every editor (after it changes)."
  (when *window*
    (loop for view being the hash-values of (window-views *window*)
          when (typep view 'editor-view) do (update-sticky-header view))))
