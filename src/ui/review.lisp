;;;; review.lisp — reviewing an edit Claude proposes, inline in its tab
;;;;
;;;; While an edit is under review, the tab shows a read-only copy of the
;;;; buffer with the change merged in: removed lines struck out on red,
;;;; added lines on green. A bar above offers Accept and Reject. Accepting
;;;; changes the real buffer in one undo step (and saves it if it had no
;;;; unsaved changes before); the buffer itself is untouched until then.
;;;; Edits arriving while one is shown wait their turn.

(in-package #:cadre-ui)

(defstruct (review (:conc-name rv-))
  buffer view old new explanation resolve
  gtk-buffer)                           ; the merged copy shown during review

(defvar *reviews* '() "Reviews waiting, the one shown first.")
(defvar *review-bar* nil)
(defvar *review-label* nil)
(defvar *review-detail* nil)

(defun make-review-bar ()
  (setf *review-label* (make-instance 'gtk:label :xalign 0.0 :css-classes '("heading"))
        *review-detail* (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end
                                                  :css-classes '("dim-label")))
  (setf *review-bar*
        (gtk:build
          (gtk:revealer :reveal-child nil :transition-type :slide-down
            (gtk:box :spacing 10 :margin-start 10 :margin-end 10 :margin-top 6 :margin-bottom 6
                     :css-classes '("cadre-review-bar")
              *review-label*
              *review-detail*
              (gtk:button :label "_Reject" :use-underline t
                          :on-clicked (lambda (b) (declare (ignore b)) (call-command 'reject-edit)))
              (gtk:button :label "_Accept" :use-underline t :css-classes '("suggested-action")
                          :on-clicked (lambda (b) (declare (ignore b)) (call-command 'accept-edit))))))))

(defun ensure-diff-tags (gtk-buffer)
  (ensure-face-tag gtk-buffer "cadre-diff-added" :diff-added)
  (ensure-face-tag gtk-buffer "cadre-diff-removed" :diff-removed))

(defun merged-review-buffer (buffer diff)
  "A new gtk:text-buffer showing DIFF (from diff-lines), highlighted like BUFFER."
  (let* ((gtk-buffer (make-gtk-text))
         (lines (mapcar #'second diff)))
    (ensure-tags gtk-buffer)
    (ensure-diff-tags gtk-buffer)
    (text-replace-contents gtk-buffer (format nil "~{~a~^~%~}" lines))
    (when (eq (buffer-major-mode buffer) 'lisp-mode)
      (let ((syntax (make-lisp-syntax gtk-buffer)))
        (ensure-lexed syntax (1- (syntax-line-count syntax)))
        (dotimes (line (syntax-line-count syntax))
          (highlight-line gtk-buffer syntax line))))
    (loop for (kind) in diff
          for line from 0
          unless (eq kind :same)
            do (gtk:text-buffer-apply-tag-by-name gtk-buffer (if (eq kind :added) "cadre-diff-added" "cadre-diff-removed")
                                                  (line-iter gtk-buffer line)
                                                  (let ((end (line-iter gtk-buffer line)))
                                                    (unless (gtk:text-iter-forward-line end)
                                                      (setf end (gtk:text-buffer-get-end-iter gtk-buffer)))
                                                    end)))
    gtk-buffer))

;;; Starting a review

(defun propose-edit (buffer new explanation resolve)
  "Show the change of BUFFER's text to NEW for review. RESOLVE is called
with :accepted, :saved (accepted and written to its file) or :rejected."
  (setf *reviews* (append *reviews* (list (make-review :buffer buffer :old (text-string (buffer-text buffer))
                                                       :new new :explanation explanation :resolve resolve))))
  (when (null (rest *reviews*))
    (show-review (first *reviews*))))

(defun show-review (review)
  (let* ((buffer (rv-buffer review))
         (view (show-buffer *window* buffer))
         (diff (diff-lines (split-lines (rv-old review)) (split-lines (rv-new review))))
         (merged (merged-review-buffer buffer diff)))
    (clear-inline-result buffer)
    (setf (rv-view review) view
          (rv-gtk-buffer review) merged)
    (gtk:text-view-set-buffer (view-text-view view) merged)
    (gtk:text-view-set-editable (view-text-view view) nil)
    (when (view-gutter view) (gtk:widget-set-visible (view-gutter view) nil))
    (multiple-value-bind (added removed) (diff-stats diff)
      (gtk:label-set-text *review-label* (format nil "Claude's edit to ~a  +~d −~d" (buffer-name buffer) added removed)))
    (gtk:label-set-text *review-detail* (or (rv-explanation review) ""))
    (gtk:widget-set-tooltip-text *review-detail* (or (rv-explanation review) ""))
    (gtk:revealer-set-reveal-child *review-bar* t)
    ;; Show the first change.
    (let ((first (position :same diff :key #'first :test-not #'eq)))
      (when first
        (let ((iter (line-iter merged (max 0 (1- first)))))
          (gtk:text-buffer-place-cursor merged iter)
          (glib:idle-add glib:+priority-default-idle+
                         (lambda ()
                           (gtk:text-view-scroll-to-iter (view-text-view view) (line-iter merged first) 0.2d0 nil 0d0 0d0)
                           nil)))))
    (message "Review Claude's edit: Accept or Reject")))

(defun end-review (outcome)
  "Restore the tab, apply the edit if OUTCOME is :accept, and show the next review."
  (let ((review (or (pop *reviews*) (editor-error "No edit to review"))))
    (let ((view (rv-view review)))
      (when view
        (gtk:text-view-set-buffer (view-text-view view) (buffer-text (rv-buffer review)))
        (gtk:text-view-set-editable (view-text-view view) t)
        (when (view-gutter view) (gtk:widget-set-visible (view-gutter view) t))))
    (gtk:revealer-set-reveal-child *review-bar* nil)
    (if (eq outcome :accept)
        (apply-review review)
        (funcall (rv-resolve review) :rejected))
    (when *reviews* (show-review (first *reviews*)))))

(defun apply-review (review)
  (let* ((buffer (rv-buffer review))
         (gtk-buffer (buffer-text buffer))
         (current (text-string gtk-buffer))
         (old (rv-old review))
         (new (rv-new review))
         (was-modified (buffer-modified-p buffer)))
    (if (string/= current old)
        (progn (message "The buffer changed during the review; the edit was not applied")
               (funcall (rv-resolve review) :changed))
        ;; Replace only the part that differs, so marks and undo stay sensible.
        (let* ((prefix (or (mismatch old new) (length old)))
               (suffix (let ((m (mismatch old new :from-end t)))
                         (if m (- (length old) m) 0)))
               (suffix (min suffix (- (length old) prefix) (- (length new) prefix))))
          (with-user-action (gtk-buffer)
            (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer prefix) (iter-at gtk-buffer (- (length old) suffix)))
            (gtk:text-buffer-insert gtk-buffer (iter-at gtk-buffer prefix) (subseq new prefix (- (length new) suffix)) -1))
          (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer prefix))
          (when (rv-view review) (scroll-to-cursor (rv-view review)))
          (if (and (buffer-file buffer) (not was-modified))
              (write-buffer buffer (buffer-file buffer)
                            (lambda (ok) (funcall (rv-resolve review) (if ok :saved :accepted))))
              (funcall (rv-resolve review) :accepted))
          (message "Applied Claude's edit to ~a" (buffer-name buffer))))))

(define-command accept-edit ()
  "Accept the edit under review."
  (end-review :accept))

(define-command reject-edit ()
  "Reject the edit under review."
  (end-review :reject))

(defun review-buffer-p (buffer)
  (find buffer *reviews* :key #'rv-buffer))
