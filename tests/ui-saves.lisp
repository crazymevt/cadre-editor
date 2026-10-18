;;;; Exercise the real save queue with manually completed asynchronous writes.
(defpackage #:cadre-ui-tests
  (:use #:cl #:parachute)
  (:local-nicknames (#:c #:cadre) (#:ui #:cadre-ui)))
(in-package #:cadre-ui-tests)

(define-test saves)

(defmacro with-save-fixture (&body body)
  `(let* ((c::*buffers* nil)
          (c:*buffer-created-hook* nil)
          (c:*before-save-hook* nil)
          (c:*after-save-hook* nil)
          (c:*frontend* nil)
          (buffer (c:make-buffer :file (merge-pathnames "cadre-save-test.txt"
                                                        (truename (uiop:temporary-directory)))
                                 :text (c:make-string-text "old")))
          (writes nil) (disk nil) (results nil)
          (messages (make-string-output-stream))
          (*standard-output* messages)
          (original (symbol-function 'ui::write-buffer-contents)))
     (setf (c:buffer-modified-p buffer) t)
     (unwind-protect
          (progn
            (setf (symbol-function 'ui::write-buffer-contents)
                  (lambda (path text success failure)
                    (push (list path text
                                (lambda () (setf disk text) (funcall success))
                                failure)
                          writes)))
            (flet ((save (&optional (done (lambda (ok) (push ok results))))
                     (ui::write-buffer buffer (c:buffer-file buffer) done)))
              ,@body))
       (setf (symbol-function 'ui::write-buffer-contents) original))))

(defun complete-write (write &optional failure)
  (if failure
      (funcall (fourth write) (make-condition 'simple-error :format-control "Write failed"))
      (funcall (third write))))

(define-test current-save :parent saves
  (with-save-fixture
    (save)
    (true (c:buffer-modified-p buffer))
    (complete-write (first writes))
    (is equal '(t) results)
    (is equal "old" disk)
    (false (c:buffer-modified-p buffer))
    (false (c:buffer-local buffer :write-in-flight))
    (is equal "old" (c:buffer-local buffer :disk-text))))

(define-test edits-during-save :parent saves
  (with-save-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (complete-write (first writes))
    (is equal '(nil) results)
    (true (c:buffer-modified-p buffer))
    (is equal "old" disk)
    (is equal "old" (c:buffer-local buffer :disk-text))
    (true (search "newer edits remain unsaved" (get-output-stream-string messages)))))

(define-test overlapping-saves :parent saves
  (with-save-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (save)
    (is = 1 (length writes))
    (complete-write (first writes))
    (is = 2 (length writes))
    (false results)
    (true (c:buffer-modified-p buffer))
    (is equal "old" disk)
    (complete-write (first writes))
    (is equal '(t t) results)
    (is equal "old new" disk)
    (false (c:buffer-modified-p buffer))
    (false (c:buffer-local buffer :write-in-flight))))

(define-test repeated-saves-share-writes :parent saves
  (with-save-fixture
    (dotimes (i 5) (save))
    (is = 1 (length writes))
    (complete-write (first writes))
    (is equal '(t t t t t) results))
  (with-save-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (dotimes (i 5) (save))
    (is = 1 (length (c:buffer-local buffer :pending-writes)))
    (complete-write (first writes))
    (complete-write (first writes))
    (is = 2 (length writes))
    (is equal '(t t t t t t) results)))

(define-test hook-error-does-not-block-saves :parent saves
  (with-save-fixture
    (setf c:*after-save-hook* (list (lambda (b) (declare (ignore b)) (error "Hook failed"))))
    (save)
    (c:buffer-insert buffer " new" 3)
    (save)
    (complete-write (first writes))
    (is = 2 (length writes))
    (complete-write (first writes))
    (is equal '(t t) results)
    (false (c:buffer-local buffer :write-in-flight))
    (is equal "old new" (c:buffer-local buffer :disk-text))
    (save)
    (complete-write (first writes))
    (is = 3 (length writes))
    (is equal '(t t t) results)))

(define-test write-failure-advances-queue :parent saves
  (with-save-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (save)
    (complete-write (first writes) t)
    (is equal '(nil) results)
    (true (c:buffer-modified-p buffer))
    (false disk)
    (complete-write (first writes))
    (is equal '(t nil) results)
    (false (c:buffer-modified-p buffer))))

(define-test write-start-error-releases-queue :parent saves
  (with-save-fixture
    (setf (symbol-function 'ui::write-buffer-contents)
          (lambda (&rest args) (declare (ignore args)) (error "Cannot start write")))
    (save)
    (is equal '(nil) results)
    (true (c:buffer-modified-p buffer))
    (false (c:buffer-local buffer :write-in-flight))))

(define-test continuation-error-and-reentrant-save :parent saves
  (with-save-fixture
    (save (lambda (ok) (declare (ignore ok)) (error "Callback failed")))
    (save)
    (complete-write (first writes))
    (is equal '(t) results)
    (false (c:buffer-local buffer :write-in-flight)))
  (with-save-fixture
    (save (lambda (ok)
            (push ok results)
            (ui::write-buffer buffer (c:buffer-file buffer) (lambda (next) (push next results)))))
    (complete-write (first writes))
    (is = 2 (length writes))
    (complete-write (first writes))
    (is equal '(t t) results)
    (false (c:buffer-local buffer :write-in-flight))))

(define-test undo-with-queued-save :parent saves
  (with-save-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (save)
    (c:buffer-delete buffer 3 7)
    (complete-write (first writes))
    (false (c:buffer-modified-p buffer))
    (false (ui::buffer-needs-saving-p buffer))
    (false results)
    (complete-write (first writes))
    (true (c:buffer-modified-p buffer))
    (is equal '(nil nil) results)
    (is equal "old new" disk)))

(define-test close-keeps-unsaved-decision-visible :parent saves
  (with-save-fixture
    (let ((original-choose (symbol-function 'ui::choose-save-response))
          (dialogs nil) (closed nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ui::choose-save-response)
                   (lambda (win buffers retry success failure)
                     (declare (ignore win buffers failure))
                     (push (list retry success) dialogs)))
             (ui::confirm-close-buffers nil (list buffer) (lambda (ok) (setf closed ok)))
             (funcall (second (first dialogs)) "save")
             (c:buffer-insert buffer " new" 3)
             (complete-write (first writes))
             (is = 2 (length dialogs))
             (true (first (first dialogs)))
             (false closed)
             (funcall (second (first dialogs)) "discard")
             (true closed)
             (setf closed nil)
             (save)
             (complete-write (first writes))
             (ui::confirm-close-buffers nil (list buffer) (lambda (ok) (setf closed ok)))
             (is = 2 (length dialogs))
             (true closed))
        (setf (symbol-function 'ui::choose-save-response) original-choose)))))

(define-test clean-title-during-save-completion :parent saves
  (with-save-fixture
    (let ((title nil))
      ;; modified-changed fires while the write is still marked active.
      (setf c:*after-save-hook* (list (lambda (b) (setf title (ui::tab-title b)))))
      (save)
      (complete-write (first writes))
      (is equal (c:buffer-display-name buffer) title)
      (false (search "●" (ui::tab-title buffer))))))

(define-test clean-close-waits-without-a-prompt :parent saves
  (with-save-fixture
    (let ((original-choose (symbol-function 'ui::choose-save-response))
          (asked nil) (closed nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ui::choose-save-response)
                   (lambda (&rest args) (declare (ignore args)) (setf asked t)))
             (save)
             (setf (c:buffer-modified-p buffer) nil)
             (false (ui::buffer-needs-saving-p buffer))
             (ui::confirm-close-buffers nil (list buffer) (lambda (ok) (setf closed ok)))
             (false asked)
             (false closed)
             (complete-write (first writes))
             (false asked)
             (true closed)
             (false (c:buffer-local buffer :write-waiters)))
        (setf (symbol-function 'ui::choose-save-response) original-choose)))))

(define-test cancelled-save-as-cancels-close :parent saves
  (with-save-fixture
    (let ((original-response (symbol-function 'ui::choose-save-response))
          (original-show (symbol-function 'ui::show-buffer))
          (original-choose (symbol-function 'ui::save-buffer-to-chosen-file))
          (dialogs 0) (success nil) (close-results nil))
      (unwind-protect
           (progn
             (setf (c:buffer-file buffer) nil
                   (symbol-function 'ui::choose-save-response)
                   (lambda (win buffers retry done failure)
                     (declare (ignore win buffers retry failure))
                     (incf dialogs) (setf success done))
                   (symbol-function 'ui::show-buffer)
                   (lambda (&rest args) (declare (ignore args)))
                   (symbol-function 'ui::save-buffer-to-chosen-file)
                   (lambda (win b continuation &key on-cancel on-error)
                     (declare (ignore win b continuation on-error))
                     (funcall on-cancel)))
             (ui::confirm-close-buffers nil (list buffer) (lambda (ok) (push ok close-results)))
             (funcall success "save")
             (is equal '(nil) close-results)
             (is = 1 dialogs)
             (true (c:buffer-modified-p buffer))
             (false (search "did not finish" (get-output-stream-string messages))))
        (setf (symbol-function 'ui::choose-save-response) original-response
              (symbol-function 'ui::show-buffer) original-show
              (symbol-function 'ui::save-buffer-to-chosen-file) original-choose)))))

(define-test gio-error-shows-only-the-message :parent saves
  (with-save-fixture
    (save)
    (funcall (fourth (first writes))
             (make-condition 'glib:glib-error :domain "test-domain" :code 123 :message "Disk full"))
    (let ((output (get-output-stream-string messages)))
      (true (search ": Disk full" output))
      (false (search "test-domain" output)))
    (is equal '(nil) results)
    (false (c:buffer-local buffer :write-in-flight))))

(define-test build-waits-for-newer-save :parent saves
  (with-save-fixture
    (let ((built nil))
      (ui::save-project-then (uiop:pathname-directory-pathname (c:buffer-file buffer))
                             (lambda () (setf built disk)))
      (is = 1 (length writes))
      (c:buffer-insert buffer " new" 3)
      (save)
      (complete-write (first writes))
      (false built)
      (false results)
      (complete-write (first writes))
      (is equal "old new" built)
      (is equal '(t) results)
      (false (search "couldn't be saved" (get-output-stream-string messages))))))

(define-test shared-save-copies-text-once-for-notification :parent saves
  (with-save-fixture
    (dotimes (i 10) (save))
    (let ((original-string (symbol-function 'c:buffer-string))
          (copies 0))
      (unwind-protect
           (progn
             (setf (symbol-function 'c:buffer-string)
                   (lambda (&rest args) (incf copies) (apply original-string args)))
             (complete-write (first writes))
             ;; The modified flag and every callback share one live-text comparison.
             (is = copies 1)
             (is = 10 (length results))
             (true (every #'identity results)))
        (setf (symbol-function 'c:buffer-string) original-string)))))

(define-test build-during-clean-write-waits :parent saves
  (with-save-fixture
    (let ((built nil))
      (save)
      (c:buffer-insert buffer " new" 3)
      (save)
      (c:buffer-delete buffer 3 7)
      (complete-write (first writes))
      (false (c:buffer-modified-p buffer))
      (ui::save-project-then (uiop:pathname-directory-pathname (c:buffer-file buffer))
                             (lambda () (setf built disk)))
      (false built)
      (is = 2 (length writes))
      (complete-write (first writes))
      (false built)
      (is = 3 (length writes))
      (complete-write (first writes))
      (is equal "old" built)
      (false (c:buffer-modified-p buffer)))))

(define-test rename-error-still-runs-hooks-and-watch :parent saves
  (with-save-fixture
    (let ((original-attach (symbol-function 'ui::attach-syntax))
          (original-watch (symbol-function 'ui::watch-buffer-file))
          (watched nil)
          (target (make-pathname :name "cadre-renamed-save-test" :type "lisp"
                                 :defaults (c:buffer-file buffer))))
      (unwind-protect
           (progn
             (setf (symbol-function 'ui::attach-syntax)
                   (lambda (b) (declare (ignore b)) (error "Syntax update failed"))
                   (symbol-function 'ui::watch-buffer-file)
                   (lambda (b) (setf watched (c:buffer-file b)))
                   ;; Oldest hook fails; the later file watcher must still run.
                   c:*after-save-hook*
                   (list (lambda (b) (ui::watch-buffer-file b))
                         (lambda (b) (declare (ignore b)) (error "Hook failed"))))
             (ui::write-buffer buffer target (lambda (ok) (push ok results)))
             (complete-write (first writes))
             (is equal target watched)
             (is equal target (c:buffer-file buffer))
             (is equal "old" (c:buffer-local buffer :disk-text))
             (is equal '(t) results)
             (false (c:buffer-modified-p buffer))
             (false (c:buffer-local buffer :write-in-flight))
             (true (search "Syntax update failed" (get-output-stream-string messages))))
        (setf (symbol-function 'ui::attach-syntax) original-attach
              (symbol-function 'ui::watch-buffer-file) original-watch)))))

(define-test failed-write-notifies-before-next-write-starts :parent saves
  (with-save-fixture
    (let ((callback-start-count nil))
      (save (lambda (ok)
              (push ok results)
              (setf callback-start-count (length writes))))
      (c:buffer-insert buffer " new" 3)
      (save)
      (complete-write (first writes) t)
      (is = 1 callback-start-count)
      (is = 2 (length writes))
      (is equal '(nil) results)
      (complete-write (first writes))
      (is equal '(t nil) results))))

(define-test reentrant-save-waits-for-all-current-callers :parent saves
  (with-save-fixture
    (let ((sibling-start-count nil))
      (save (lambda (ok)
              (push ok results)
              (ui::write-buffer buffer (c:buffer-file buffer)
                                (lambda (next) (push next results)))))
      (save (lambda (ok)
              (push ok results)
              (setf sibling-start-count (length writes))))
      (complete-write (first writes))
      (is = 1 sibling-start-count)
      (is = 2 (length writes))
      (is equal '(t t) results)
      (complete-write (first writes))
      (is equal '(t t t) results))))

(define-test hook-edits-are-unsaved-with-one-snapshot-comparison :parent saves
  (with-save-fixture
    (setf c:*after-save-hook*
          (list (lambda (b) (c:buffer-insert b " from hook" (c:buffer-length b)))))
    (save)
    (complete-write (first writes))
    (true (c:buffer-modified-p buffer))
    (is equal '(nil) results)
    (is equal "old" disk)
    (is equal "old" (c:buffer-local buffer :disk-text))
    (true (search "newer edits remain unsaved" (get-output-stream-string messages)))))

(defmacro with-window-close-fixture (&body body)
  `(with-save-fixture
     ;; Allocate the model without constructing its GTK widgets.
     (let ((win (allocate-instance (find-class 'ui::cadre-window)))
           (sessions 0) (dialogs nil) (destroyed 0)
           (original-session (symbol-function 'ui::save-session))
           (original-scratch (symbol-function 'ui::save-scratch-buffers))
           (original-response (symbol-function 'ui::choose-save-response))
           (original-destroy (symbol-function 'gtk:window-destroy)))
       (setf (ui::window-close-pending-p win) nil
             (slot-value win 'ui::window) :test-window)
       (unwind-protect
            (progn
              (setf (symbol-function 'ui::save-session)
                    (lambda (w) (declare (ignore w)) (incf sessions))
                    (symbol-function 'ui::save-scratch-buffers) (lambda ())
                    (symbol-function 'ui::choose-save-response)
                    (lambda (w buffers retry success failure)
                      (declare (ignore w buffers retry failure))
                      (push (lambda (proceed) (funcall success (if proceed "discard" "cancel"))) dialogs))
                    (symbol-function 'gtk:window-destroy)
                    (lambda (w) (declare (ignore w)) (incf destroyed)))
              ,@body)
         (setf (symbol-function 'ui::save-session) original-session
               (symbol-function 'ui::save-scratch-buffers) original-scratch
               (symbol-function 'ui::choose-save-response) original-response
               (symbol-function 'gtk:window-destroy) original-destroy)))))

(define-test repeated-window-close-shares-one-decision :parent saves
  (with-window-close-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (dotimes (i 3) (true (ui::request-close win)))
    (true (ui::window-close-pending-p win))
    (is = 1 sessions)
    (is = 1 (length (c:buffer-local buffer :write-waiters)))
    (false dialogs)
    (complete-write (first writes))
    (is = 1 (length dialogs))
    (true (ui::request-close win))
    (is = 1 (length dialogs))
    ;; Cancel releases the guard, so a later close can ask again.
    (funcall (first dialogs) nil)
    (false (ui::window-close-pending-p win))
    (is = 0 destroyed)
    (true (ui::request-close win))
    (is = 2 sessions)
    (is = 2 (length dialogs))
    (funcall (first dialogs) t)
    (is = 1 destroyed)
    (false (ui::window-close-pending-p win))))

(define-test close-guard-clears-on-errors-and-clean-close :parent saves
  (with-window-close-fixture
    (setf (symbol-function 'ui::save-session)
          (lambda (w) (declare (ignore w)) (error "Session failed")))
    (fail (ui::request-close win) 'error)
    (false (ui::window-close-pending-p win))
    (setf (symbol-function 'ui::save-session)
          (lambda (w) (declare (ignore w)) (incf sessions)))
    (true (ui::request-close win))
    (setf (symbol-function 'gtk:window-destroy)
          (lambda (w) (declare (ignore w)) (error "Destroy failed")))
    (fail (funcall (first dialogs) t) 'error)
    (false (ui::window-close-pending-p win))
    (false (ui::request-close win))
    (false (ui::window-close-pending-p win))))

(define-test prompt-error-after-waiting-releases-close-guard :parent saves
  (with-window-close-fixture
    (save)
    (c:buffer-insert buffer " new" 3)
    (true (ui::request-close win))
    (setf (symbol-function 'ui::choose-save-response)
          (lambda (&rest args) (declare (ignore args)) (error "Cannot create save dialog")))
    ;; The writer catches waiter errors; the close must already be cancelled.
    (complete-write (first writes))
    (false (ui::window-close-pending-p win))
    (setf (symbol-function 'ui::choose-save-response)
          (lambda (w buffers retry success failure)
            (declare (ignore w buffers retry failure))
            (push (lambda (proceed) (funcall success (if proceed "discard" "cancel"))) dialogs)))
    (true (ui::request-close win))
    (is = 1 (length dialogs))
    (funcall (first dialogs) nil)
    (false (ui::window-close-pending-p win))))

(define-test dialog-error-cancels-close-and-ignores-late-response :parent saves
  (with-window-close-fixture
    (let ((original-choose (symbol-function 'ui::choose-save-response))
          (success nil) (failure nil))
      (unwind-protect
           (progn
             ;; Use the real controller with a manually completed dialog backend.
             (setf (symbol-function 'ui::choose-save-response)
                   (lambda (w buffers retry done failed)
                     (declare (ignore w buffers retry))
                     (setf success done failure failed)))
             (true (ui::request-close win))
             (let ((old-success success))
               (funcall failure (make-condition 'simple-error))
               (false (ui::window-close-pending-p win))
               (true (ui::request-close win))
               (funcall old-success "discard")
               (is = 0 destroyed)
               (true (ui::window-close-pending-p win))
               (funcall success "cancel")
               (false (ui::window-close-pending-p win))))
        (setf (symbol-function 'ui::choose-save-response) original-choose)))))

(define-test before-save-error-from-dialog-releases-close-guard :parent saves
  (with-window-close-fixture
    (let ((original-choose (symbol-function 'ui::choose-save-response))
          (success nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ui::choose-save-response)
                   (lambda (w buffers retry done failed)
                     (declare (ignore w buffers retry failed)) (setf success done))
                   c:*before-save-hook* (list (lambda (b) (declare (ignore b)) (error "Hook failed"))))
             (true (ui::request-close win))
             (fail (funcall success "save") 'error)
             (false (ui::window-close-pending-p win))
             (is = 0 destroyed)
             (true (ui::request-close win))
             (funcall success "cancel")
             (false (ui::window-close-pending-p win)))
        (setf (symbol-function 'ui::choose-save-response) original-choose)))))

(define-test later-buffer-save-error-delivers-abort :parent saves
  (with-save-fixture
    (let ((second-buffer (c:make-buffer :file #p"/tmp/cadre-other-save-test.txt"
                                        :text (c:make-string-text "other")))
          (aborted 0))
      (setf c:*before-save-hook*
            (list (lambda (b) (when (eq b second-buffer) (error "Second save failed")))))
      (ui::save-buffers nil (list buffer second-buffer)
                        (lambda (ok) (push ok results)) :on-error (lambda () (incf aborted)))
      (complete-write (first writes))
      (is = 1 aborted)
      (false results)
      (false (c:buffer-local buffer :write-in-flight)))))

(define-test tab-close-caller-error-finishes-once :parent saves
  (with-save-fixture
    (let ((original-choose (symbol-function 'ui::choose-save-response))
          (response nil) (dialog-error nil) (calls nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ui::choose-save-response)
                   (lambda (win buffers retry success failure)
                     (declare (ignore win buffers retry))
                     (setf response success dialog-error failure)))
             ;; This is the same callback boundary used by close-page-request.
             (ui::confirm-close-buffers
              nil (list buffer)
              (lambda (proceed) (push proceed calls) (error "close-page-finish failed")))
             (fail (funcall response "discard") 'error)
             (is equal '(t) calls)
             (funcall dialog-error (make-condition 'simple-error))
             (funcall response "cancel")
             (is equal '(t) calls))
        (setf (symbol-function 'ui::choose-save-response) original-choose)))))

(define-test tab-close-save-error-cancels-once :parent saves
  (with-save-fixture
    (let ((original-choose (symbol-function 'ui::choose-save-response))
          (response nil) (calls nil))
      (unwind-protect
           (progn
             (setf (symbol-function 'ui::choose-save-response)
                   (lambda (win buffers retry success failure)
                     (declare (ignore win buffers retry failure)) (setf response success))
                   c:*before-save-hook*
                   (list (lambda (b) (declare (ignore b)) (error "Save failed"))))
             (ui::confirm-close-buffers nil (list buffer) (lambda (proceed) (push proceed calls)))
             (fail (funcall response "save") 'error)
             (is equal '(nil) calls)
             (funcall response "discard")
             (is equal '(nil) calls))
        (setf (symbol-function 'ui::choose-save-response) original-choose)))))

;;; Saves wait for their preparation (format on save's external formatter),
;;; one at a time and in order, and so do those waiting for the saves.

(define-test prepared-saves-keep-their-order :parent saves
  (with-save-fixture
    (let* ((preparing '())
           (ui::*save-preparer* (lambda (buffer then)
                                  (declare (ignore buffer))
                                  (setf preparing (append preparing (list then)))))
           (waited nil))
      (save)
      (true (ui::buffer-save-busy-p buffer))
      (false writes)
      (c:buffer-insert buffer " new" 3)
      (save)
      ;; The second save waits for the first one's preparation.
      (is = 1 (length preparing))
      (ui::wait-for-buffer-writes (list buffer) (lambda () (setf waited t)))
      (funcall (pop preparing))
      (is = 1 (length writes))
      (is equal "old new" (second (first writes)))
      (is = 1 (length preparing))
      ;; A preparation answers once, however often it's called.
      (funcall (first preparing))
      (funcall (pop preparing))
      (false waited)
      (complete-write (first writes))
      (true waited)
      (is equal '(t t) results)
      (is equal "old new" disk)
      (false (ui::buffer-save-busy-p buffer)))))

(define-test failed-preparation-still-saves :parent saves
  (with-save-fixture
    (let ((ui::*save-preparer* (lambda (buffer then)
                                 (declare (ignore buffer then))
                                 (error "No formatter"))))
      (save)
      (complete-write (first writes))
      (is equal '(t) results)
      (false (ui::buffer-save-busy-p buffer)))))

(defun run-formatter (command text &key (timeout 10))
  (let ((buffer (c:make-buffer :name "format-test" :text (c:make-string-text text)))
        (done (sb-thread:make-semaphore))
        (result nil))
    (ui::run-formatter-process buffer text command
                               (lambda (formatted problem) (setf result (list formatted problem)))
                               :deliver (lambda (function) (funcall function) (sb-thread:signal-semaphore done))
                               :timeout timeout)
    (true (sb-thread:wait-on-semaphore done :timeout 15))
    result))

(define-test external-formatter-process :parent saves
  (let ((c::*buffers* nil) (c:*buffer-created-hook* nil))
    (is equal '("HELLO λ" nil) (run-formatter '("tr" "a-z" "A-Z") "hello λ"))
    (destructuring-bind (formatted problem) (run-formatter '("sh" "-c" "echo bad input >&2; exit 2") "x")
      (false formatted)
      (true (search "couldn't format it: bad input" problem)))
    (destructuring-bind (formatted problem) (run-formatter '("sleep" "10") "x" :timeout 0.3)
      (false formatted)
      (true (search "took longer" problem)))
    ;; A formatter that ignores SIGTERM is killed after the grace period, and
    ;; one that catches it and exits 0 still counts as stopped.
    (let ((ui::*formatter-stop-grace* 0.3))
      (dolist (script '("trap '' TERM; while :; do sleep 0.05; done"
                        "trap 'echo partial; exit 0' TERM; while :; do sleep 0.05; done"))
        (let ((start (get-internal-real-time)))
          (destructuring-bind (formatted problem) (run-formatter (list "sh" "-c" script) "x" :timeout 0.3)
            (false formatted)
            (true (search "took longer" problem)))
          (true (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 2.5)))))
    (destructuring-bind (formatted problem) (run-formatter '("cadre-no-such-formatter") "x")
      (false formatted)
      (true (search "Could not run" problem)))))
