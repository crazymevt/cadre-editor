;;;; buffers.lisp — buffers: text with a name, maybe a file, and a mode
;;;;
;;;; A buffer exists whether or not any tab shows it. Buffers have unique
;;;; names; a file's buffer is named after the file, with <2>, <3>, … added
;;;; when two files share a name.

(in-package #:cadre)

(defvar *buffers* '()
  "Every live buffer, most recently created first.")

(defclass buffer ()
  ((name :initarg :name :accessor buffer-name)
   (file :initarg :file :initform nil :accessor buffer-file
         :documentation "The pathname the buffer visits, or nil.")
   (text :initarg :text :reader buffer-text
         :documentation "An object implementing the text protocol.")
   (major-mode :initarg :major-mode :accessor buffer-major-mode)
   (locals :initform (make-hash-table :test 'eq) :reader buffer-locals)))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :type t)
    (prin1 (buffer-name buffer) stream)))

(defun unique-buffer-name (name)
  (if (not (find-buffer name))
      name
      (loop for i from 2
            for candidate = (format nil "~a<~d>" name i)
            unless (find-buffer candidate) return candidate)))

(defun file-display-name (pathname)
  (let ((name (file-namestring pathname)))
    (if (string= name "") (car (last (pathname-directory pathname))) name)))

(defun make-buffer (&key name file (text (make-string-text)) major-mode)
  "Create a buffer and add it to the buffer list. NAME defaults to FILE's
name, made unique; MAJOR-MODE defaults to the mode for FILE."
  (let ((buffer (make-instance 'buffer
                               :name (unique-buffer-name
                                      (or name (and file (file-display-name file)) "untitled"))
                               :file (and file (pathname file))
                               :text text
                               :major-mode (or major-mode (major-mode-for-file file)))))
    (push buffer *buffers*)
    (run-hook '*buffer-created-hook* buffer)
    buffer))

(defun kill-buffer (buffer)
  "Remove BUFFER from the buffer list."
  (when (member buffer *buffers*)
    (run-hook '*buffer-killed-hook* buffer)
    (setf *buffers* (remove buffer *buffers*)))
  nil)

(defun buffer-list ()
  (copy-list *buffers*))

(defun find-buffer (name)
  "The buffer named NAME, or nil."
  (find name *buffers* :key #'buffer-name :test #'string=))

(defun same-file-p (a b)
  (let ((ta (or (probe-file a) (merge-pathnames a)))
        (tb (or (probe-file b) (merge-pathnames b))))
    (equal (namestring ta) (namestring tb))))

(defun find-file-buffer (pathname)
  "The buffer visiting PATHNAME, or nil."
  (find-if (lambda (b) (and (buffer-file b) (same-file-p (buffer-file b) pathname)))
           *buffers*))

(defun buffer-local (buffer key &optional default)
  "BUFFER's local value for KEY. Settable with setf."
  (gethash key (buffer-locals buffer) default))

(defun (setf buffer-local) (value buffer key)
  (setf (gethash key (buffer-locals buffer)) value))

(defun buffer-display-name (buffer)
  "BUFFER's name for tabs and titles."
  (buffer-name buffer))

;;; The text protocol, through the buffer

(defun buffer-string (buffer &optional (start 0) end)
  (text-string (buffer-text buffer) start end))

(defun buffer-length (buffer)
  (text-length (buffer-text buffer)))

(defun buffer-point (buffer)
  (text-point (buffer-text buffer)))

(defun (setf buffer-point) (position buffer)
  (setf (text-point (buffer-text buffer)) position))

(defun buffer-insert (buffer string &optional (position (buffer-point buffer)))
  (text-insert (buffer-text buffer) position string))

(defun buffer-delete (buffer start end)
  (text-delete (buffer-text buffer) start end))

(defun buffer-modified-p (buffer)
  (text-modified-p (buffer-text buffer)))

(defun (setf buffer-modified-p) (value buffer)
  (setf (text-modified-p (buffer-text buffer)) value))
