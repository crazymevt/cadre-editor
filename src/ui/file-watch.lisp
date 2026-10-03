;;;; file-watch.lisp — noticing when an open file changes on disk
;;;;
;;;; Each buffer visiting a file watches it. When the file changes to
;;;; something other than what Cadre last read or wrote: a buffer with no
;;;; unsaved changes reloads quietly (as one undo step, keeping the cursor);
;;;; a buffer with unsaved changes shows a bar offering to reload, keep
;;;; yours, or compare the two as an inline diff.

(in-package #:cadre-ui)

(define-option *reload-changed-files* t boolean
  "Reload open files that change on disk (if they have no unsaved changes)."
  :category "Editing")

(defun read-file-text (pathname)
  (ignore-errors
   (with-open-file (in pathname :element-type '(unsigned-byte 8))
     (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
       (read-sequence v in)
       (decode-file-contents v)))))

(defun remember-disk-text (buffer)
  (when (buffer-file buffer)
    (setf (buffer-local buffer :disk-text) (buffer-string buffer))))

(defun watch-buffer-file (buffer)
  "Start watching BUFFER's file (again, if it changed)."
  (let ((old (buffer-local buffer :monitor)))
    (when old (gio:file-monitor-cancel (car old)))
    (setf (buffer-local buffer :monitor) nil)
    (when (and (buffer-file buffer) (typep (buffer-text buffer) 'gtk:text-buffer))
      (let ((monitor (ignore-errors
                      (gio:file-monitor-file (gio:file-new-for-path (uiop:native-namestring (buffer-file buffer)))
                                             '(:none)))))
        (when monitor
          (gio:file-monitor-set-rate-limit monitor 300)
          (setf (buffer-local buffer :monitor)
                (cons monitor (gobject:connect monitor :changed
                                               (lambda (m file other event)
                                                 (declare (ignore m file other))
                                                 (when (member event '(:changes-done-hint :created :changed))
                                                   (schedule-disk-check buffer)))))))))))

(defun schedule-disk-check (buffer)
  ;; Several events arrive for one save; check once they settle.
  (unless (buffer-local buffer :disk-check-pending)
    (setf (buffer-local buffer :disk-check-pending) t)
    (glib:timeout-add glib:+priority-default+ 150
                      (lambda ()
                        (setf (buffer-local buffer :disk-check-pending) nil)
                        (when (member buffer (buffer-list)) (check-disk buffer))
                        nil))))

(defun check-disk (buffer)
  "Compare BUFFER's file with what Cadre last read or wrote, and act on a change."
  (let ((text (and (buffer-file buffer) (read-file-text (buffer-file buffer)))))
    (when (and text (not (equal text (buffer-local buffer :disk-text))))
      (cond ((equal text (buffer-string buffer))
             (setf (buffer-local buffer :disk-text) text))
            ((and *reload-changed-files* (not (buffer-modified-p buffer)) (not (review-buffer-p buffer)))
             (reload-buffer-text buffer text)
             (message "Reloaded ~a: it changed on disk" (buffer-name buffer)))
            (t (show-disk-conflict buffer text))))))

(defun reload-buffer-text (buffer text)
  "Make BUFFER's text TEXT, changing only the part that differs, as one undo step."
  (let* ((gtk-buffer (buffer-text buffer))
         (old (text-string gtk-buffer))
         (prefix (or (mismatch old text) (length old)))
         (suffix (let ((m (mismatch old text :from-end t))) (if m (- (length old) m) 0)))
         (suffix (min suffix (- (length old) prefix) (- (length text) prefix))))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer prefix) (iter-at gtk-buffer (- (length old) suffix)))
      (gtk:text-buffer-insert gtk-buffer (iter-at gtk-buffer prefix) (subseq text prefix (- (length text) suffix)) -1))
    (setf (buffer-local buffer :disk-text) text
          (buffer-modified-p buffer) nil)))

;;; The conflict bar

(defvar *conflict-bar* nil)
(defvar *conflict-label* nil)
(defvar *conflicts* '() "(buffer . disk text) for files changed under unsaved edits, the shown one first.")

(defun make-conflict-bar ()
  (setf *conflict-label* (make-instance 'gtk:label :xalign 0.0 :hexpand t :wrap t))
  (setf *conflict-bar*
        (gtk:build
          (gtk:revealer :reveal-child nil :transition-type :slide-down
            (gtk:box :spacing 10 :margin-start 10 :margin-end 10 :margin-top 6 :margin-bottom 6
                     :css-classes '("cadre-conflict-bar")
              *conflict-label*
              (gtk:button :label "_Compare" :use-underline t
                          :on-clicked (lambda (b) (declare (ignore b)) (resolve-conflict :compare)))
              (gtk:button :label "_Keep Mine" :use-underline t
                          :on-clicked (lambda (b) (declare (ignore b)) (resolve-conflict :keep)))
              (gtk:button :label "_Reload" :use-underline t :css-classes '("destructive-action")
                          :on-clicked (lambda (b) (declare (ignore b)) (resolve-conflict :reload))))))))

(defun show-disk-conflict (buffer text)
  (setf *conflicts* (append (remove buffer *conflicts* :key #'car) (list (cons buffer text))))
  (show-first-conflict))

(defun show-first-conflict ()
  (let ((conflict (first *conflicts*)))
    (if conflict
        (progn
          (gtk:label-set-text *conflict-label*
                              (format nil "~a changed on disk, and has unsaved changes here."
                                      (buffer-name (car conflict))))
          (gtk:revealer-set-reveal-child *conflict-bar* t))
        (gtk:revealer-set-reveal-child *conflict-bar* nil))))

(defun resolve-conflict (how)
  (let ((conflict (pop *conflicts*)))
    (when conflict
      (destructuring-bind (buffer . text) conflict
        (when (member buffer (buffer-list))
          (ecase how
            (:reload (reload-buffer-text buffer text) (message "Reloaded ~a" (buffer-name buffer)))
            (:keep (setf (buffer-local buffer :disk-text) text)
             (message "Kept your version of ~a; saving will overwrite the file" (buffer-name buffer)))
            (:compare
             (setf (buffer-local buffer :disk-text) text)
             (propose-edit buffer text "The version on disk: Accept takes it, Reject keeps yours"
                           (lambda (outcome) (declare (ignore outcome)))))))))
    (show-first-conflict)))

(add-hook '*buffer-created-hook* (lambda (buffer) (remember-disk-text buffer) (watch-buffer-file buffer)))
(add-hook '*after-save-hook* (lambda (buffer)
                               (remember-disk-text buffer)
                               ;; Saving as another file watches that one.
                               (watch-buffer-file buffer)))
(add-hook '*buffer-killed-hook* (lambda (buffer)
                                  (let ((m (buffer-local buffer :monitor)))
                                    (when m (gio:file-monitor-cancel (car m))))))
