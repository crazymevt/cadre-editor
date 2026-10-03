;;;; gtk-text.lisp — the text protocol for gtk:text-buffer

(in-package #:cadre-ui)

(defun iter-at (buffer offset)
  (gtk:text-buffer-get-iter-at-offset buffer offset))

(defun cursor-iter (buffer)
  (gtk:text-buffer-get-iter-at-mark buffer (gtk:text-buffer-get-insert buffer)))

(defmethod text-length ((text gtk:text-buffer))
  (gtk:text-buffer-get-char-count text))

(defmethod text-string ((text gtk:text-buffer) &optional (start 0) end)
  (gtk:text-buffer-get-text text (iter-at text start)
                            (if end (iter-at text end) (gtk:text-buffer-get-end-iter text))
                            t))

(defmethod text-insert ((text gtk:text-buffer) position string)
  (gtk:text-buffer-insert text (iter-at text position) string -1))

(defmethod text-delete ((text gtk:text-buffer) start end)
  (gtk:text-buffer-delete text (iter-at text start) (iter-at text end)))

(defmethod text-point ((text gtk:text-buffer))
  (gtk:text-iter-get-offset (cursor-iter text)))

(defmethod (setf text-point) (position (text gtk:text-buffer))
  (gtk:text-buffer-place-cursor text (iter-at text position))
  position)

(defmethod text-modified-p ((text gtk:text-buffer))
  (gtk:text-buffer-get-modified text))

(defmethod (setf text-modified-p) (value (text gtk:text-buffer))
  (gtk:text-buffer-set-modified text (and value t))
  value)

(defmethod text-replace-contents ((text gtk:text-buffer) string)
  (gtk:text-buffer-begin-irreversible-action text)
  (gtk:text-buffer-set-text text string -1)
  (gtk:text-buffer-end-irreversible-action text)
  (gtk:text-buffer-place-cursor text (gtk:text-buffer-get-start-iter text))
  (gtk:text-buffer-set-modified text nil))

(defun make-gtk-text (&optional (contents ""))
  (let ((text (gtk:text-buffer-new nil)))
    (text-replace-contents text contents)
    text))
