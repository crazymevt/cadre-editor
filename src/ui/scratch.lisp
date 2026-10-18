;;;; scratch.lisp — a scratch buffer for each project
;;;;
;;;; A Lisp buffer for trying things out, kept from one session to the next
;;;; but not in the project: its file lives in the config directory
;;;; (~/.config/cadre/scratch/), named after the project. It saves itself a
;;;; moment after each change, and when it closes or Cadre quits, so it
;;;; never asks to be saved.

(in-package #:cadre-ui)

(defparameter *scratch-save-delay* 800 "Milliseconds after a change before the scratch buffer saves itself.")

(defun scratch-directory ()
  (merge-pathnames "scratch/" (config-directory)))

(defun scratch-file (project)
  (merge-pathnames (scratch-file-name project) (scratch-directory)))

(defun scratch-file-p (pathname)
  (and pathname
       (equal (namestring (uiop:pathname-directory-pathname pathname))
              (namestring (scratch-directory)))))

(defun scratch-buffer-p (buffer)
  (and buffer (buffer-local buffer :scratch)))

(defun scratch-template (project)
  (format nil ";;;; Scratch~@[ for ~a~]: kept between sessions, not part of the project.
;;;; Evaluate a form with C-RET (C-c C-c in the Emacs keys).

" (and project (car (last (pathname-directory (uiop:ensure-directory-pathname project)))))))

(defun save-scratch (buffer)
  "Write BUFFER, a scratch buffer, to its file if it changed."
  (when (and (buffer-modified-p buffer) (buffer-file buffer))
    (ensure-directories-exist (buffer-file buffer))
    (let ((text (buffer-string buffer)))
      (write-private-state (buffer-file buffer) (lambda (out) (write-string text out))))
    (setf (buffer-modified-p buffer) nil)
    (remember-disk-text buffer)))

(defun save-scratch-buffers ()
  (dolist (buffer (buffer-list))
    (when (scratch-buffer-p buffer)
      (ignore-errors (save-scratch buffer)))))

(defun schedule-scratch-save (buffer)
  "Save BUFFER a moment from now, once changes stop."
  (let ((timer (buffer-local buffer :scratch-timer)))
    (when timer (glib:source-remove timer)))
  (setf (buffer-local buffer :scratch-timer)
        (glib:timeout-add glib:+priority-default+ *scratch-save-delay*
                          (lambda ()
                            (setf (buffer-local buffer :scratch-timer) nil)
                            (handler-case (save-scratch buffer)
                              (error (e) (message "Could not save the scratch buffer: ~a" e)))
                            nil))))

(defun attach-scratch (buffer)
  "Make a buffer of a scratch file a scratch buffer: named *scratch*,
saving itself."
  (when (scratch-file-p (buffer-file buffer))
    (setf (buffer-local buffer :scratch) t
          (buffer-name buffer) (cadre::unique-buffer-name "*scratch*"))
    (gobject:connect (buffer-text buffer) :changed
                     (lambda (b) (declare (ignore b)) (schedule-scratch-save buffer)))))

(add-hook '*buffer-created-hook* 'attach-scratch)
(add-hook '*buffer-killed-hook* (lambda (buffer)
                                  (when (scratch-buffer-p buffer)
                                    (let ((timer (buffer-local buffer :scratch-timer)))
                                      (when timer
                                        (glib:source-remove timer)
                                        (setf (buffer-local buffer :scratch-timer) nil)))
                                    (ignore-errors (save-scratch buffer)))))

(define-command scratch ()
  "Open the project's scratch buffer: Lisp to try things in, kept between
sessions but not in the project's folder."
  (let* ((project (and *window* (window-project *window*)))
         (file (scratch-file project)))
    (unless (probe-file file)
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :external-format :utf-8)
        (write-string (scratch-template project) out)))
    (open-file-path file :then (lambda (view)
                                 (let ((buffer (view-gtk-buffer view)))
                                   (gtk:text-buffer-place-cursor buffer (gtk:text-buffer-get-end-iter buffer)))
                                 (focus-view view)))))
