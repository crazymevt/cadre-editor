;;;; editor-view.lisp — a view of a buffer: a text view with a line-number gutter
;;;;
;;;; A view shows one buffer; several views may show the same buffer. The
;;;; view's widget (a scrolled window) is what a tab holds.

(in-package #:cadre-ui)

(defclass editor-view ()
  ((buffer :initarg :buffer :reader view-buffer)
   (text-view :reader view-text-view)
   (gutter :reader view-gutter)
   (widget :reader view-widget :documentation "The scrolled window holding the text view.")
   (gutter-digits :initform 0 :accessor view-gutter-digits)
   (on-cursor-moved :initarg :on-cursor-moved :initform nil :accessor view-on-cursor-moved
                    :documentation "Called with the view when its cursor moves.")))

(defmethod print-object ((view editor-view) stream)
  (print-unreadable-object (view stream :type t :identity t)
    (prin1 (buffer-name (view-buffer view)) stream)))

(defun view-gtk-buffer (view)
  (buffer-text (view-buffer view)))

(defun make-editor-view (buffer &key on-cursor-moved)
  "A new view of BUFFER, whose text must be a gtk:text-buffer."
  (let* ((view (make-instance 'editor-view :buffer buffer :on-cursor-moved on-cursor-moved))
         (text-view (make-instance 'gtk:text-view
                                   :buffer (buffer-text buffer)
                                   :monospace t :wrap-mode :none
                                   :left-margin 8 :right-margin 8
                                   :top-margin 4 :bottom-margin 200
                                   :css-classes '("cadre-editor")))
         (gutter (make-instance 'gtk:drawing-area :css-classes '("cadre-gutter")))
         (scrolled (make-instance 'gtk:scrolled-window :child text-view
                                                       :hexpand t :vexpand t)))
    (setf (slot-value view 'text-view) text-view
          (slot-value view 'gutter) gutter
          (slot-value view 'widget) scrolled)
    (gtk:text-view-set-gutter text-view :left gutter)
    (gtk:drawing-area-set-draw-func gutter (lambda (area cr width height)
                                             (declare (ignore height))
                                             (draw-line-numbers view area cr width)))
    (update-gutter-width view)
    (let ((gtk-buffer (buffer-text buffer)))
      (gobject:connect gtk-buffer :changed
                       (lambda (b) (declare (ignore b))
                         (update-gutter-width view)
                         (gtk:widget-queue-draw gutter)))
      (gobject:connect gtk-buffer "notify::cursor-position"
                       (lambda (b pspec) (declare (ignore b pspec))
                         (gtk:widget-queue-draw gutter)
                         (when (view-on-cursor-moved view)
                           (funcall (view-on-cursor-moved view) view)))))
    (gobject:connect (gtk:scrolled-window-get-vadjustment scrolled) :value-changed
                     (lambda (adjustment) (declare (ignore adjustment))
                       (gtk:widget-queue-draw gutter)))
    view))

(defun view-cursor-line-column (view)
  "The cursor's line and column in VIEW, both counted from 1."
  (let ((iter (cursor-iter (view-gtk-buffer view))))
    (values (1+ (gtk:text-iter-get-line iter))
            (1+ (gtk:text-iter-get-line-offset iter)))))

(defun focus-view (view)
  (gtk:widget-grab-focus (view-text-view view)))

(defun scroll-to-cursor (view)
  (let ((buffer (view-gtk-buffer view)))
    (gtk:text-view-scroll-to-mark (view-text-view view) (gtk:text-buffer-get-insert buffer)
                                  0.1d0 nil 0d0 0d0)))

;;; The gutter

(defun digit-count (n)
  (length (princ-to-string n)))

(defun update-gutter-width (view)
  "Make the gutter wide enough for the largest line number."
  (let ((digits (max 3 (digit-count (gtk:text-buffer-get-line-count (view-gtk-buffer view))))))
    (unless (= digits (view-gutter-digits view))
      (setf (view-gutter-digits view) digits)
      (let ((layout (gtk:widget-create-pango-layout (view-text-view view)
                                                    (make-string digits :initial-element #\8))))
        (gtk:widget-set-size-request (view-gutter view)
                                     (+ (pango:layout-get-pixel-size layout) 20) -1)))))

(defun draw-line-numbers (view area cr width)
  (let* ((text-view (view-text-view view))
         (buffer (view-gtk-buffer view))
         (rect (gtk:text-view-get-visible-rect text-view))
         (top (gdk:rectangle-y rect))
         (bottom (+ top (gdk:rectangle-height rect)))
         (cursor-line (gtk:text-iter-get-line (cursor-iter buffer)))
         (layout (gtk:widget-create-pango-layout text-view nil))
         (color (gtk:widget-get-color area)))
    (flet ((draw-line (iter)
             (let ((line (gtk:text-iter-get-line iter))
                   (y (gtk:text-view-get-line-yrange text-view iter)))
               (multiple-value-bind (wx wy)
                   (gtk:text-view-buffer-to-window-coords text-view :left 0 y)
                 (declare (ignore wx))
                 (pango:layout-set-text layout (princ-to-string (1+ line)) -1)
                 (cairo:set-source-rgba cr (gdk:rgba-red color) (gdk:rgba-green color)
                                        (gdk:rgba-blue color) (if (= line cursor-line) 0.9d0 0.4d0))
                 (cairo:move-to cr (- width (pango:layout-get-pixel-size layout) 10) wy)
                 (pango-cairo:show-layout cr layout))
               y)))
      (let ((iter (gtk:text-view-get-line-at-y text-view top)))
        (loop
          (let ((line (gtk:text-iter-get-line iter)))
            (when (> (draw-line iter) bottom) (return))
            (unless (gtk:text-iter-forward-line iter)
              ;; At the end. A final empty line (after a newline) still gets a number.
              (when (> (gtk:text-iter-get-line iter) line)
                (draw-line iter))
              (return))))))))
