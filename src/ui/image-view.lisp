;;;; image-view.lisp — pictures open in a tab, to be looked at
;;;;
;;;; A picture (image-mode's file types) opens in a read-only buffer that
;;;; holds the picture, scaled, and a line saying its size. The buffer has no
;;;; file, so nothing saves it or reloads it as text; its :image-file is the
;;;; picture's. Scaling up keeps the pixels sharp (pixel art), and
;;;; transparent parts show a checkerboard. The picture is shown again when
;;;; its file changes, as when a drawing program saves it.
;;;;
;;;; For measuring out sprites: a grid of cells (16 × 16, or W × H from an
;;;; offset X, Y) can be drawn over the picture, and the status bar gives the
;;;; pixel under the pointer, with its color.

(in-package #:cadre-ui)

(defparameter *image-zooms* '(1/8 1/4 1/3 1/2 2/3 1 2 3 4 6 8 12 16 24 32)
  "The scales Zoom In and Zoom Out step through in a picture.")

(defparameter *image-fit-size* 256
  "A small picture first shows scaled up to about this many pixels.")

(defparameter *image-max-size* 8192
  "No side of a shown picture gets bigger than this.")

(defparameter *image-grid-color* #xff40ffff
  "The grid's lines, as #xRRGGBBAA.")

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

(defun set-image-pixbuf (buffer pixbuf)
  "Make PIXBUF BUFFER's picture, keeping its pixels for reading colors."
  (setf (buffer-local buffer :pixbuf) pixbuf
        (buffer-local buffer :pixels) (ignore-errors (gdk-pixbuf:pixbuf-get-pixels pixbuf))))

(defun image-pixel-color (buffer x y)
  "The color of the picture's pixel X, Y as \"#rrggbb\" (\"#rrggbbaa\" if
it has alpha), or nil."
  (let ((pixbuf (buffer-local buffer :pixbuf))
        (pixels (buffer-local buffer :pixels)))
    (when (and pixbuf pixels)
      (let* ((channels (gdk-pixbuf:pixbuf-get-n-channels pixbuf))
             (start (+ (* y (gdk-pixbuf:pixbuf-get-rowstride pixbuf)) (* x channels))))
        (when (<= (+ start channels) (length pixels))
          (format nil "#~{~(~2,'0x~)~}" (coerce (subseq pixels start (+ start channels)) 'list)))))))

;;; The grid: (width height x y), cells of WIDTH × HEIGHT pixels from X, Y.

(defun default-image-grid () (or (setting :image-grid) (list 16 16 0 0)))

(defun parse-image-grid (text)
  "A grid from TEXT such as \"16\", \"16x28\" or \"16x28+128+100\", or nil."
  (multiple-value-bind (match groups)
      (ppcre:scan-to-strings "^\\s*(\\d+)(?:\\s*[xX×*]\\s*(\\d+))?(?:\\s*\\+\\s*(\\d+)\\s*\\+\\s*(\\d+))?\\s*$" text)
    (when match
      (flet ((n (i) (and (aref groups i) (parse-integer (aref groups i)))))
        (let ((width (n 0)))
          (when (plusp width)
            (let ((height (or (n 1) width)))
              (when (plusp height)
                (list width height (or (n 2) 0) (or (n 3) 0))))))))))

(defun format-image-grid (grid)
  (destructuring-bind (width height x y) grid
    (format nil "~dx~d~:[~;+~d+~d~]" width height (or (plusp x) (plusp y)) x y)))

(defun draw-grid-lines (shown grid scale width height)
  "Draw GRID over SHOWN, the picture (WIDTH × HEIGHT pixels) scaled by SCALE.
Cells too small to see get no lines."
  (destructuring-bind (cell-width cell-height x0 y0) grid
    (let ((shown-width (gdk-pixbuf:pixbuf-get-width shown))
          (shown-height (gdk-pixbuf:pixbuf-get-height shown)))
      (flet ((lines (cell start size shown-size vertical)
               (when (>= (* cell scale) 3)
                 (loop for at from (mod start cell) to size by cell
                       for screen = (min (1- shown-size) (round (* at scale)))
                       do (gdk-pixbuf:pixbuf-fill
                           (if vertical
                               (gdk-pixbuf:pixbuf-new-subpixbuf shown screen 0 1 shown-height)
                               (gdk-pixbuf:pixbuf-new-subpixbuf shown 0 screen shown-width 1))
                           *image-grid-color*)))))
        (lines cell-width x0 width shown-width t)
        (lines cell-height y0 height shown-height nil)))))

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
            (when (buffer-local buffer :grid-on)
              (draw-grid-lines shown (buffer-local buffer :grid) scale width height))
            (gtk:text-buffer-insert-paintable gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer)
                                              (gdk:texture-new-for-pixbuf shown))))
        (setf (buffer-local buffer :shown-size) (list shown-width shown-height))
        (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer)
                                (format nil "~%~%~a · ~d × ~d pixels · ~d%~@[ · grid ~a~]"
                                        (file-namestring file) width height (round (* 100 scale))
                                        (and (buffer-local buffer :grid-on)
                                             (format-image-grid (buffer-local buffer :grid))))
                                -1)))
    (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer))
    (gtk:text-buffer-set-modified gtk-buffer nil)))

(defun reload-image (buffer)
  (when (member buffer (buffer-list))
    (let ((pixbuf (load-image-pixbuf buffer)))
      (when pixbuf
        (set-image-pixbuf buffer pixbuf)
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
             (set-image-pixbuf buffer pixbuf)
             (setf (buffer-local buffer :grid) (default-image-grid)
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
    (gtk:text-view-set-top-margin text-view 24)
    (let ((motion (gtk:event-controller-motion-new)))
      (gobject:connect motion :motion
                       (lambda (controller x y)
                         (declare (ignore controller))
                         (show-image-pointer view (image-pixel-at view x y))))
      (gobject:connect motion :leave
                       (lambda (controller)
                         (declare (ignore controller))
                         (show-image-pointer view nil)))
      (gtk:widget-add-controller text-view motion))))

;;; The pixel under the pointer

(defun image-pixel-at (view x y)
  "The picture's pixel at X, Y in VIEW's text view, as (x y), or nil."
  (let* ((buffer (view-buffer view))
         (pixbuf (buffer-local buffer :pixbuf))
         (shown (buffer-local buffer :shown-size))
         (text-view (view-text-view view)))
    (when (and pixbuf shown)
      (let ((rect (gtk:text-view-get-iter-location text-view (gtk:text-buffer-get-start-iter (buffer-text buffer))))
            (width (gdk-pixbuf:pixbuf-get-width pixbuf))
            (height (gdk-pixbuf:pixbuf-get-height pixbuf)))
        (multiple-value-bind (bx by) (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
          (let ((px (floor (* (- bx (gdk:rectangle-x rect)) width) (first shown)))
                (py (floor (* (- by (gdk:rectangle-y rect)) height) (second shown))))
            (when (and (< -1 px width) (< -1 py height))
              (list px py))))))))

(defun image-status-text (buffer &optional pixel)
  "The status bar's position for a picture: the pixel under the pointer and
its color, or else the picture's size."
  (let ((pixbuf (buffer-local buffer :pixbuf)))
    (cond (pixel
           (destructuring-bind (x y) pixel
             (format nil "x ~d, y ~d~@[ · ~a~]" x y (image-pixel-color buffer x y))))
          (pixbuf
           (format nil "~d × ~d" (gdk-pixbuf:pixbuf-get-width pixbuf) (gdk-pixbuf:pixbuf-get-height pixbuf)))
          (t ""))))

(defun show-image-pointer (view pixel)
  (when (and *window* (eq view (selected-view *window*)))
    (gtk:label-set-text (window-status-position *window*) (image-status-text (view-buffer view) pixel))))

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

;;; The grid

(define-command image-toggle-grid ()
  "Show or hide a grid over the picture, for measuring out sprites (Set Image Grid sets its cells)."
  (:modes image-mode)
  (let ((buffer (current-image-buffer)))
    (setf (buffer-local buffer :grid-on) (not (buffer-local buffer :grid-on)))
    (render-image buffer)
    (message "Grid ~:[off~;on: ~:*~a~]"
             (and (buffer-local buffer :grid-on) (format-image-grid (buffer-local buffer :grid))))))

(define-command image-set-grid ()
  "Set the grid's cells: 16 (16 × 16), 16x28, or 16x28+128+100 to start them at x 128, y 100."
  (:modes image-mode)
  (let ((buffer (current-image-buffer)))
    (open-picker (window-picker *window*)
                 :placeholder "Grid cells: 16, 16x28, or 16x28+X+Y to start at X, Y"
                 :text (format-image-grid (buffer-local buffer :grid (default-image-grid)))
                 :on-choose (lambda (text)
                              (let ((grid (parse-image-grid text)))
                                (unless grid (editor-error "Not a grid: ~a (try 16, 16x28 or 16x28+128+100)" text))
                                (setf (setting :image-grid) grid
                                      (buffer-local buffer :grid) grid
                                      (buffer-local buffer :grid-on) t)
                                (render-image buffer)
                                (message "Grid ~a" (format-image-grid grid)))))))
