;;;; Responsiveness regressions without a window or display.
(in-package #:cadre-ui-tests)

(define-test responsiveness)

(define-test transcript-scroll-requests-are-coalesced :parent responsiveness
  (let ((ui::*chat* (ui::make-chat :scroller :scroller))
        (callbacks nil) (scrolls 0))
    (with-assistant-functions
        ((glib:idle-add (lambda (priority callback)
                          (declare (ignore priority)) (push callback callbacks) 1))
         (gtk:scrolled-window-get-vadjustment (lambda (scroller) (declare (ignore scroller)) :adjustment))
         (gtk:adjustment-get-upper (lambda (adj) (declare (ignore adj)) 100.0))
         (gtk:adjustment-get-page-size (lambda (adj) (declare (ignore adj)) 20.0))
         (gtk:adjustment-set-value (lambda (adj value) (declare (ignore adj))
                                     (is = 80.0 value) (incf scrolls))))
      (dotimes (i 500) (ui::chat-scroll-to-end))
      (is = 1 (length callbacks))
      (funcall (pop callbacks))
      (is = 1 scrolls)
      (ui::chat-scroll-to-end)
      (is = 1 (length callbacks)))))

(define-test project-root-stops-at-symlinked-open-folder :parent responsiveness
  (let* ((root (merge-pathnames (format nil "cadre-root-~a/" (c:make-uuid)) (uiop:temporary-directory)))
         (real (merge-pathnames "real/" root))
         (alias (merge-pathnames "alias/" root))
         (file (merge-pathnames "example.lisp" alias))
         (visited nil))
    (ensure-directories-exist real)
    (with-open-file (out (merge-pathnames "example.lisp" real) :direction :output) (write-line "(values)" out))
    (sb-posix:symlink (namestring real) (string-right-trim "/" (namestring alias)))
    (unwind-protect
         (with-assistant-functions
             ((ui::project-folder-p (lambda (dir) (push dir visited) nil)))
           (is equal real (ui::find-project-root file (truename real) real))
           (is = 1 (length visited))
           (is equal alias (first visited)))
      (delete-file (string-right-trim "/" (namestring alias)))
      (uiop:delete-directory-tree root :validate t))))

(define-test background-folds-discard-edited-snapshots :parent responsiveness
  (let* ((c::*buffer-created-hook* nil)
         (buffer (c:make-buffer :text (c:make-string-text (format nil "(foo~% bar)"))))
         (syntax (c:make-lisp-syntax (c:buffer-text buffer)))
         (workers nil) (callbacks nil) (redraws 0))
    (setf (c:buffer-local buffer :syntax) syntax)
    (unwind-protect
         (with-assistant-functions
             ((sb-thread:make-thread (lambda (fn &key name) (declare (ignore name)) (push fn workers)))
              (glib:call-in-main-thread (lambda (fn) (push fn callbacks)))
              (ui::redraw-gutters (lambda (b) (declare (ignore b)) (incf redraws))))
           (ui::refresh-fold-ranges buffer)
           (funcall (pop workers))
           ;; The worker finishes, but its source changes before GTK receives it.
           (setf (c:buffer-local buffer :fold-generation) (1+ (c:buffer-local buffer :fold-generation 0)))
           (c:text-replace-contents (c:buffer-text buffer) "(foo)")
           (funcall (pop callbacks))
           (false (c:buffer-local buffer :fold-ranges-by-line))
           (is = 0 redraws)
           (ui::refresh-fold-ranges buffer)
           (funcall (pop workers))
           (funcall (pop callbacks))
           (true (hash-table-p (c:buffer-local buffer :fold-ranges-by-line)))
           (false (c:buffer-local buffer :fold-ranges))
           (is = 1 redraws))
      (let ((c::*buffer-killed-hook* nil)) (c:kill-buffer buffer)))))

(define-test gutter-fold-discovery-is-deferred :parent responsiveness
  (let* ((c:*buffer-created-hook* nil)
         (buffer (c:make-buffer :text (c:make-string-text "(foo)")))
         (scheduled 0))
    (unwind-protect
         (with-assistant-functions
             ((glib:timeout-add (lambda (&rest args) (declare (ignore args)) (incf scheduled)))
              (ui::compute-fold-ranges (lambda (b) (declare (ignore b))
                                        (error "Gutter drawing must not compute folds"))))
           (dotimes (line 50) (false (ui::fold-range-starting buffer line)))
           (is = 1 scheduled))
      (let ((c:*buffer-killed-hook* nil)) (c:kill-buffer buffer)))))

(define-test fold-workers-run-one-at-a-time :parent responsiveness
  (let* ((c::*buffer-created-hook* nil)
         (buffer (c:make-buffer :text (c:make-string-text "(foo)")))
         (workers nil) (callbacks nil))
    (setf (c:buffer-local buffer :syntax) (c:make-lisp-syntax (c:buffer-text buffer)))
    (unwind-protect
         (with-assistant-functions
             ((sb-thread:make-thread (lambda (fn &key name) (declare (ignore name)) (push fn workers)))
              (glib:call-in-main-thread (lambda (fn) (push fn callbacks)))
              (ui::redraw-gutters (lambda (b) (declare (ignore b)))))
           (ui::refresh-fold-ranges buffer)
           (ui::refresh-fold-ranges buffer)
           (ui::refresh-fold-ranges buffer)
           (is = 1 (length workers))
           (funcall (pop workers))
           (funcall (pop callbacks))
           ;; The refreshes that waited start a single worker afterwards.
           (is = 1 (length workers))
           (funcall (pop workers))
           (funcall (pop callbacks))
           (is = 0 (length workers))
           (false (c:buffer-local buffer :fold-refresh-pending)))
      (let ((c::*buffer-killed-hook* nil)) (c:kill-buffer buffer)))))

(define-test failed-fold-workers-wait-for-an-edit :parent responsiveness
  (let* ((c::*buffer-created-hook* nil)
         (buffer (c:make-buffer :text (c:make-string-text "(foo)")))
         (workers nil) (callbacks nil) (messages 0) (scheduled 0))
    (setf (c:buffer-local buffer :syntax) (c:make-lisp-syntax (c:buffer-text buffer)))
    (unwind-protect
         (with-assistant-functions
             ((sb-thread:make-thread (lambda (fn &key name) (declare (ignore name)) (push fn workers)))
              (glib:call-in-main-thread (lambda (fn) (push fn callbacks)))
              (glib:timeout-add (lambda (&rest args) (declare (ignore args)) (incf scheduled)))
              (ui::lisp-fold-ranges (lambda (syntax) (declare (ignore syntax)) (error "lexer failed")))
              (ui::message (lambda (&rest args) (declare (ignore args)) (incf messages)))
              (ui::redraw-gutters (lambda (b) (declare (ignore b)))))
           (ui::refresh-fold-ranges buffer)
           (funcall (pop workers))
           (funcall (pop callbacks))
           (is = 1 messages)
           (dotimes (line 50) (false (ui::fold-range-starting buffer line)))
           (is = 0 scheduled))
      (let ((c::*buffer-killed-hook* nil)) (c:kill-buffer buffer)))))

(define-test hidden-api-page-refreshes-when-shown :parent responsiveness
  (let ((ui::*window* nil) (ui::*api-page-stale* nil) (ui::*api-captured* '(:old)) (refreshes 0))
    (with-assistant-functions
        ((ui::refresh-api-page (lambda () (setf ui::*api-page-stale* nil) (incf refreshes))))
      (ui::api-project-changed)
      (is = 0 refreshes)
      (true ui::*api-page-stale*)
      (ui::refresh-api-page-if-stale)
      (is = 1 refreshes)
      (ui::refresh-api-page-if-stale)
      (is = 1 refreshes))))
