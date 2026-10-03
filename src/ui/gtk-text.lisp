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

;;; Lines

(defun line-iter (buffer line &optional (column 0))
  "An iterator at COLUMN on LINE of BUFFER (clamped to the line's end)."
  (nth-value 1 (gtk:text-buffer-get-iter-at-line-offset buffer line column)))

(defun line-end-iter (buffer line)
  (let ((iter (line-iter buffer line)))
    (unless (gtk:text-iter-ends-line iter)
      (gtk:text-iter-forward-to-line-end iter))
    iter))

(defmethod text-line-count ((text gtk:text-buffer))
  (gtk:text-buffer-get-line-count text))

(defmethod text-line-string ((text gtk:text-buffer) line)
  (gtk:text-buffer-get-text text (line-iter text line) (line-end-iter text line) t))

(defmethod text-line-position ((text gtk:text-buffer) line &optional (column 0))
  (gtk:text-iter-get-offset (line-iter text line column)))

(defmethod text-position-line ((text gtk:text-buffer) position)
  (let ((iter (iter-at text position)))
    (values (gtk:text-iter-get-line iter) (gtk:text-iter-get-line-offset iter))))

(defun iter-line-column (iter)
  (values (gtk:text-iter-get-line iter) (gtk:text-iter-get-line-offset iter)))

(defmacro with-user-action ((buffer) &body body)
  "Run BODY as one user action: one step for undo."
  (let ((b (gensym)))
    `(let ((,b ,buffer))
       (gtk:text-buffer-begin-user-action ,b)
       (unwind-protect (progn ,@body)
         (gtk:text-buffer-end-user-action ,b)))))
