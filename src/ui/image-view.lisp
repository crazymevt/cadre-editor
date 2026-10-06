;;;; image-view.lisp — pictures open in a tab, to be looked at
;;;;
;;;; A picture (image-mode's file types) opens in a read-only buffer that
;;;; holds the picture, scaled, and a line saying its size. The buffer has no
;;;; file, so nothing saves it or reloads it as text; its :image-file is the
;;;; picture's. Scaling up keeps the pixels sharp (pixel art), and
;;;; transparent parts show a checkerboard. The picture is shown again when
;;;; its file changes, as when a drawing program saves it.

(in-package #:cadre-ui)

(defparameter *image-zooms* '(1/8 1/4 1/3 1/2 2/3 1 2 3 4 6 8 12 16 24 32)
  "The scales Zoom In and Zoom Out step through in a picture.")

(defparameter *image-fit-size* 256
  "A small picture first shows scaled up to about this many pixels.")

(defparameter *image-max-size* 8192
  "No side of a shown picture gets bigger than this.")

(defun image-buffer-p (buffer) (and (buffer-local buffer :image-file) t))

(defun find-image-buffer (pathname)
  (find-if (lambda (b) (let ((file (buffer-local b :image-file)))
                         (and file (same-file-p file pathname))))
           (buffer-list)))

(defun initial-image-zoom (width height)
  "Scale a small picture up by a whole number until it's about *IMAGE-FIT-SIZE*."
  (max 1 (min 16 (floor *image-fit-size* (max width height 1)))))

(defun load-image-pixbuf (buffer)
  "Read BUFFER's picture. Returns nil, with a message, if it can't be read."
  (let ((file (buffer-local buffer :image-file)))
    (handler-case (gdk-pixbuf:pixbuf-new-from-file (uiop:native-namestring file))
      (error (e)
        (message "Can't show ~a: ~a" (file-namestring file)
                 (if (typep e 'glib:glib-error) (glib:glib-error-message e) e))
        nil))))

(defun checkerboard-colors ()
  (if (adw:dark-p) (values #x3a3a3a #x2c2c2c) (values #xffffff #xd8d8d8)))

(defun render-image (buffer)
  "Show BUFFER's picture at its zoom, with a line giving its size."
  (let* ((pixbuf (buffer-local buffer :pixbuf))
         (gtk-buffer (buffer-text buffer))
         (zoom (buffer-local buffer :zoom 1))
         (file (buffer-local buffer :image-file)))
    (text-replace-contents gtk-buffer "")
    (when pixbuf
      (let* ((width (gdk-pixbuf:pixbuf-get-width pixbuf))
             (height (gdk-pixbuf:pixbuf-get-height pixbuf))
             (scale (min zoom (/ *image-max-size* (max width height))))
             (shown-width (max 1 (round (* width scale))))
             (shown-height (max 1 (round (* height scale)))))
        (multiple-value-bind (light dark) (checkerboard-colors)
          (let ((shown (gdk-pixbuf:pixbuf-composite-color-simple
                        pixbuf shown-width shown-height (if (>= scale 1) :nearest :bilinear)
                        255 (max 4 (min 16 (round (* 8 (max 1 scale))))) light dark)))
            (gtk:text-buffer-insert-paintable gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer)
                                              (gdk:texture-new-for-pixbuf shown))))
        (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer)
                                (format nil "~%~%~a · ~d × ~d pixels · ~d%"
                                        (file-namestring file) width height (round (* 100 scale)))
                                -1)))
    (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer))
    (gtk:text-buffer-set-modified gtk-buffer nil)))

(defun reload-image (buffer)
  (when (member buffer (buffer-list))
    (let ((pixbuf (load-image-pixbuf buffer)))
      (when pixbuf
        (setf (buffer-local buffer :pixbuf) pixbuf)
        (render-image buffer)))))

(defun watch-image-file (buffer)
  "Show BUFFER's picture again whenever its file changes."
  (let ((monitor (ignore-errors
                  (gio:file-monitor-file (gio:file-new-for-path (uiop:native-namestring (buffer-local buffer :image-file)))
                                         '(:none)))))
    (when monitor
      (gio:file-monitor-set-rate-limit monitor 300)
      (setf (buffer-local buffer :monitor)
            (cons monitor (gobject:connect monitor :changed
                                           (lambda (m file other event)
                                             (declare (ignore m file other))
                                             (when (member event '(:changes-done-hint :created))
                                               (reload-image buffer)))))))))

(defun make-image-buffer (pathname)
  "A buffer showing the picture at PATHNAME, or nil if it can't be read."
  (let ((buffer (make-buffer :name (file-namestring pathname) :text (make-gtk-text) :major-mode 'image-mode)))
    (setf (buffer-local buffer :image-file) (pathname pathname))
    (let ((pixbuf (load-image-pixbuf buffer)))
      (cond (pixbuf
             (setf (buffer-local buffer :pixbuf) pixbuf
                   (buffer-local buffer :zoom) (initial-image-zoom (gdk-pixbuf:pixbuf-get-width pixbuf)
                                                                   (gdk-pixbuf:pixbuf-get-height pixbuf)))
             (render-image buffer)
             (watch-image-file buffer)
             buffer)
            (t (kill-buffer buffer) nil)))))

(defun open-image-path (pathname &key then (group (window-active-group *window*)))
  "Show the picture at PATHNAME in a tab, or select its tab if it is open."
  (let ((buffer (or (find-image-buffer pathname) (make-image-buffer pathname))))
    (when buffer
      (let ((view (show-buffer *window* buffer :group group)))
        (when then (funcall then view))
        view))))

(defun setup-image-view (view)
  "Make VIEW, of a picture, read-only and centred, with no cursor."
  (let ((text-view (view-text-view view)))
    (gtk:text-view-set-editable text-view nil)
    (gtk:text-view-set-cursor-visible text-view nil)
    (gtk:text-view-set-monospace text-view nil)
    (gtk:text-view-set-justification text-view :center)
    (gtk:text-view-set-top-margin text-view 24)))

(defun forget-image (buffer)
  (let ((monitor (buffer-local buffer :monitor)))
    (when (and monitor (image-buffer-p buffer))
      (gio:file-monitor-cancel (car monitor))
      (setf (buffer-local buffer :monitor) nil))))

(add-hook '*buffer-killed-hook* 'forget-image)

;;; Zooming

(defun current-image-buffer ()
  (let ((buffer (view-buffer (current-view))))
    (unless (image-buffer-p buffer) (editor-error "Not a picture"))
    buffer))

(defun set-image-zoom (buffer zoom)
  (setf (buffer-local buffer :zoom) zoom)
  (render-image buffer))

(define-command image-zoom-in ()
  "Show the picture bigger."
  (:modes image-mode)
  (let* ((buffer (current-image-buffer))
         (zoom (buffer-local buffer :zoom 1)))
    (set-image-zoom buffer (or (find-if (lambda (z) (> z zoom)) *image-zooms*) zoom))))

(define-command image-zoom-out ()
  "Show the picture smaller."
  (:modes image-mode)
  (let* ((buffer (current-image-buffer))
         (zoom (buffer-local buffer :zoom 1)))
    (set-image-zoom buffer (or (find-if (lambda (z) (< z zoom)) *image-zooms* :from-end t) zoom))))

(define-command image-actual-size ()
  "Show the picture at its own size, one screen pixel for each of its pixels."
  (:modes image-mode)
  (set-image-zoom (current-image-buffer) 1))
