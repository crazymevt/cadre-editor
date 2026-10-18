;;;; local-history.lisp — private saved revisions and unsaved-buffer checkpoints.
(in-package #:cadre-ui)

(define-option *local-history* t boolean
  "Keep saved file revisions outside the project for comparison and restore."
  :category "Session")
(define-option *local-history-revisions* 20 integer
  "Maximum saved revisions per file (at least one)." :category "Session")
(define-option *local-history-total* 200 integer
  "Maximum saved revisions across all files (at least one)." :category "Session")
(define-option *recover-unsaved-buffers* t boolean
  "Checkpoint unsaved buffers outside the project and offer recovery after an unexpected exit."
  :category "Session")
(define-option *snapshot-max-characters* 1048576 integer
  "Largest buffer retained in local history or recovery; larger buffers are skipped."
  :category "Session")
(defparameter *recovery-delay* 1000)
(defvar *recovery-session* nil)
(defvar *snapshot-dialog* nil)

(defun snapshot-directory (kind)
  (merge-pathnames (ecase kind (:history "local-history/") (:recovery "recovery/")) (state-directory)))

(defun snapshot-key (name)
  (format nil "~(~{~2,'0x~}~)" (coerce (sb-md5:md5sum-string name :external-format :utf-8) 'list)))

(defun snapshot-time ()
  (multiple-value-bind (seconds micros) (sb-ext:get-time-of-day) (+ (* seconds 1000000) micros)))

(defun snapshot-time-label (snapshot)
  (multiple-value-bind (s m h day month year)
      (decode-universal-time (+ 2208988800 (floor (getf snapshot :time) 1000000)))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day h m s)))

(defun write-private-state (path writer)
  "Atomically replace private state. An interrupted write leaves the old copy intact."
  (let ((temp (make-pathname :name (format nil "~a-~a" (pathname-name path) (make-uuid))
                             :type "tmp" :defaults path)))
    (ensure-directories-exist path)
    (sb-posix:chmod (uiop:pathname-directory-pathname path) #o700)
    (unwind-protect
         (progn
           (with-open-file (out temp :direction :output :if-exists :error :external-format :utf-8)
             (sb-posix:chmod temp #o600)
             (funcall writer out))
           (uiop:rename-file-overwriting-target temp path))
      (when (probe-file temp) (delete-file temp))))
  path)

(defun write-snapshot (path data)
  (write-private-state path
                       (lambda (out)
                         (with-standard-io-syntax
                           (let ((*package* (find-package :keyword))) (prin1 data out) (terpri out))))))

(defun read-snapshot (path)
  (ignore-errors
    (with-open-file (in path :external-format :utf-8)
      (when (< (file-length in) (+ 65536 (* 8 (max 1 *snapshot-max-characters*))))
        (with-standard-io-syntax
          (let* ((*read-eval* nil) (*package* (find-package :keyword)) (data (read in nil nil)))
            (when (and (listp data) (eql 1 (getf data :version))
                       (stringp (getf data :name)) (integerp (getf data :time))
                       (or (null (getf data :file)) (stringp (getf data :file)))
                       (stringp (getf data :text)))
              data)))))))

(defun snapshot-files (directory)
  (when (uiop:directory-exists-p directory)
    (append (uiop:directory-files directory "*.sexp")
            (mapcan #'snapshot-files (uiop:subdirectories directory)))))

(defun saved-revisions (file)
  (let* ((key (snapshot-key (namestring (uiop:ensure-absolute-pathname file))))
         (directory (merge-pathnames (format nil "~a/" key) (snapshot-directory :history))))
    (sort (loop for path in (snapshot-files directory)
                for data = (read-snapshot path) when data collect (cons path data))
          #'> :key (lambda (entry) (getf (cdr entry) :time)))))

(defun snapshot-path-time (path)
  (or (ignore-errors (parse-integer (pathname-name path) :junk-allowed t)) 0))

(defun prune-snapshots (paths limit)
  (let ((sorted (sort (copy-list paths) #'> :key #'snapshot-path-time)))
    (dolist (path (nthcdr (max 1 limit) sorted)) (delete-file path))))

(defun record-file-revision (file text)
  (when (and *local-history* file (stringp text) (<= (length text) (max 0 *snapshot-max-characters*)))
    (let* ((file (uiop:ensure-absolute-pathname file))
           (dir (merge-pathnames (format nil "~a/" (snapshot-key (namestring file))) (snapshot-directory :history)))
           (latest (first (sort (snapshot-files dir) #'> :key #'snapshot-path-time)))
           (previous (and latest (read-snapshot latest))))
      (unless (and previous (string= text (getf previous :text)))
        (let* ((time (snapshot-time))
               (dir (merge-pathnames (format nil "~a/" (snapshot-key (namestring file))) (snapshot-directory :history)))
               (path (merge-pathnames (format nil "~d-~a.sexp" time (make-uuid)) dir)))
          (write-snapshot path (list :version 1 :file (namestring file) :name (file-namestring file) :time time :text text))
          ;; Metadata time orders revisions even when several saves share a file timestamp.
          (prune-snapshots (snapshot-files dir) *local-history-revisions*)
          (prune-snapshots (snapshot-files (snapshot-directory :history)) *local-history-total*)
          path)))))

;;; A process's recovery folder is in use while the process holds a write
;;; lock on the folder's lock file. The system drops the lock when the
;;; process ends, however it ends; a process ID alone could belong to some
;;; other process by then. (A process drops its own fcntl locks on a file
;;; when it closes any descriptor of it, so it never tests its own.)

(defvar *recovery-lock* nil "The descriptor of this process's recovery lock file, while it holds the lock.")
(defvar *own-recovery-sessions* '() "The recovery folders this process made (namestrings).")

(defun recovery-lock-file (directory) (merge-pathnames "lock" directory))

(defun whole-file-lock (type)
  (make-instance 'sb-posix:flock :type type :whence sb-posix:seek-set :start 0 :len 0))

(defun lock-recovery-session (directory)
  (handler-case
      (let ((fd (sb-posix:open (namestring (recovery-lock-file directory))
                               (logior sb-posix:o-creat sb-posix:o-rdwr) #o600)))
        (sb-posix:fcntl fd sb-posix:f-setlk (whole-file-lock sb-posix:f-wrlck))
        (setf *recovery-lock* fd))
    (error (e) (message "Could not lock the recovery folder; telling it from others by process ID: ~a" e))))

(defun unlock-recovery-session ()
  (when *recovery-lock*
    (ignore-errors (sb-posix:close *recovery-lock*))
    (setf *recovery-lock* nil)))

(defun recovery-session-directory ()
  (or *recovery-session*
      (let ((directory (merge-pathnames (format nil "~d-~a/" (sb-posix:getpid) (make-uuid))
                                        (snapshot-directory :recovery))))
        (ensure-directories-exist directory)
        (sb-posix:chmod directory #o700)
        (lock-recovery-session directory)
        (push (namestring directory) *own-recovery-sessions*)
        (setf *recovery-session* directory))))

(defun recovery-lock-held-p (lock-file)
  "Whether another process holds LOCK-FILE's lock; t if that can't be told."
  (handler-case
      (let ((fd (sb-posix:open (namestring lock-file) sb-posix:o-rdwr)))
        (unwind-protect
             (let ((lock (whole-file-lock sb-posix:f-wrlck)))
               (sb-posix:fcntl fd sb-posix:f-getlk lock)
               (/= (sb-posix:flock-type lock) sb-posix:f-unlck))
          (sb-posix:close fd)))
    (error () t)))

(defun recovery-session-active-p (directory)
  "Whether DIRECTORY's checkpoints belong to a running Cadre process: this
one, or one holding the folder's lock. A folder without a lock file (from
before there was one) goes by the process ID its name starts with."
  (let ((lock-file (recovery-lock-file directory)))
    (cond ((member (namestring (uiop:ensure-directory-pathname directory)) *own-recovery-sessions*
                   :test #'string=)
           t)
          ((probe-file lock-file) (recovery-lock-held-p lock-file))
          (t (let ((pid (ignore-errors (parse-integer (car (last (pathname-directory directory))) :junk-allowed t))))
               (and pid
                    (handler-case (progn (sb-posix:kill pid 0) t)
                      (sb-posix:syscall-error (e) (not (= (sb-posix:syscall-errno e) sb-posix:esrch))))))))))

(defun recoverable-snapshots ()
  (sort (loop for path in (snapshot-files (snapshot-directory :recovery))
              unless (recovery-session-active-p (uiop:pathname-directory-pathname path))
                append (let ((data (read-snapshot path))) (when data (list (cons path data)))))
        #'> :key (lambda (entry) (getf (cdr entry) :time))))

(defun remove-recovery-origin (buffer)
  (let ((path (buffer-local buffer :recovery-origin)))
    (when (and path (not (recovery-session-active-p (uiop:pathname-directory-pathname path))))
      (when (probe-file path) (delete-file path))
      (setf (buffer-local buffer :recovery-origin) nil))))

(defun remove-buffer-checkpoint (buffer)
  (let ((path (buffer-local buffer :checkpoint-path)))
    (when (and path (probe-file path)) (delete-file path))))

(defun checkpoint-buffer (buffer)
  (when (buffer-local buffer :checkpoint-attached)
    (if (and *recover-unsaved-buffers* (buffer-modified-p buffer))
        (let ((text (buffer-string buffer)))
          (if (> (length text) (max 0 *snapshot-max-characters*))
              (unless (buffer-local buffer :checkpoint-size-warning)
                (setf (buffer-local buffer :checkpoint-size-warning) t)
                (message "Recovery skipped ~a: larger than the snapshot size limit" (buffer-name buffer)))
              (let ((path (or (buffer-local buffer :checkpoint-path)
                              (setf (buffer-local buffer :checkpoint-path)
                                    (merge-pathnames (format nil "~a.sexp" (make-uuid)) (recovery-session-directory))))))
                (write-snapshot path (list :version 1 :name (buffer-name buffer)
                                           :file (and (buffer-file buffer) (namestring (buffer-file buffer)))
                                           :time (snapshot-time) :text text))
                (remove-recovery-origin buffer))))
        (progn (remove-buffer-checkpoint buffer)
               (unless (buffer-modified-p buffer) (remove-recovery-origin buffer))))))

(defun schedule-checkpoint (buffer)
  ;; Do not reset this timer: continuous typing must also be checkpointed.
  (unless (buffer-local buffer :checkpoint-timer)
    (setf (buffer-local buffer :checkpoint-timer)
          (glib:timeout-add glib:+priority-default+ *recovery-delay*
                            (lambda ()
                              (setf (buffer-local buffer :checkpoint-timer) nil)
                              (when (member buffer (buffer-list))
                                (handler-case (checkpoint-buffer buffer)
                                  (error (e) (message "Could not checkpoint ~a: ~a" (buffer-name buffer) e))))
                              nil)))))

(defun attach-checkpoint (buffer)
  (when (and (typep (buffer-text buffer) 'gtk:text-buffer)
             (or (buffer-file buffer)
                 (let ((name (buffer-name buffer)))
                   (not (and (plusp (length name)) (char= #\* (char name 0)))))))
    (setf (buffer-local buffer :checkpoint-attached) t)
    (gobject:connect (buffer-text buffer) :changed
                     (lambda (text) (declare (ignore text)) (schedule-checkpoint buffer)))))

(defun history-before-save (buffer)
  (handler-case (record-file-revision (buffer-file buffer) (buffer-local buffer :disk-text))
    (error (e) (message "Could not keep local history: ~a" e))))

(defun history-after-save (buffer)
  (handler-case
      (progn (record-file-revision (buffer-file buffer) (buffer-local buffer :disk-text))
             (checkpoint-buffer buffer))
    (error (e) (message "Could not update local history or recovery: ~a" e))))

(defun forget-buffer-checkpoint (buffer)
  (let ((timer (buffer-local buffer :checkpoint-timer)))
    (when timer (glib:source-remove timer) (setf (buffer-local buffer :checkpoint-timer) nil)))
  (handler-case
      (if (and (scratch-buffer-p buffer) (buffer-modified-p buffer))
          ;; A failed scratch autosave must not turn closing the tab into data loss.
          (checkpoint-buffer buffer)
          (progn (remove-buffer-checkpoint buffer) (remove-recovery-origin buffer)))
    (error (e) (message "Could not update recovery checkpoint: ~a" e))))

(defun discard-session-checkpoints ()
  "Call only after a normal close was accepted (including explicit Don't Save)."
  (handler-case
      (progn
        (dolist (buffer (buffer-list)) (forget-buffer-checkpoint buffer))
        ;; Leave failed scratch autosaves for recovery after this process exits.
        (when (and *recovery-session* (uiop:directory-exists-p *recovery-session*)
                   (null (snapshot-files *recovery-session*)))
          (uiop:delete-directory-tree *recovery-session* :validate t))
        ;; Checkpoints left behind stay locked until this process ends.
        (unless (and *recovery-session* (uiop:directory-exists-p *recovery-session*))
          (unlock-recovery-session))
        (setf *recovery-session* nil))
    (error (e) (message "Could not clear this session's recovery checkpoints: ~a" e))))

(defun open-snapshot-copy (entry)
  (let* ((data (cdr entry))
         (buffer (make-buffer :name (format nil "Recovered ~a" (getf data :name))
                              :text (make-gtk-text (getf data :text)))))
    (setf (buffer-modified-p buffer) t
          (buffer-local buffer :recovery-origin) (car entry))
    ;; Transfer ownership only after this process's checkpoint is on disk.
    (show-buffer *window* buffer)
    (handler-case (checkpoint-buffer buffer)
      (error (e) (message "Could not checkpoint the recovered copy; the original checkpoint is kept: ~a" e)))
    (message "Recovered a separate copy of ~a; Save As chooses where to write it" (getf data :name))
    buffer))

(defun restore-history-text (buffer text)
  "Restore only the live buffer, as one undo action. Never write its file."
  (unless (member buffer (buffer-list)) (editor-error "The original buffer was closed"))
  (let ((gtk-buffer (buffer-text buffer)))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer) (gtk:text-buffer-get-end-iter gtk-buffer))
      (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) text -1)))
  (setf (buffer-modified-p buffer) t)
  (checkpoint-buffer buffer))

(defun show-snapshot (entry &optional target)
  (let* ((data (cdr entry))
         (dialog (make-instance 'adw:dialog :title (if target "Local History" "Recover Unsaved Buffer")
                                            :content-width 760 :content-height 520))
         (copy-buffer nil)
         (copy (make-instance 'gtk:button :label "Open Copy"))
         (restore (make-instance 'gtk:button :label "Restore to Buffer" :sensitive (and target t)))
         (discard (make-instance 'gtk:button :label "Discard Checkpoint" :visible (null target)))
         (close (make-instance 'gtk:button :label "Close")))
    (setf *snapshot-dialog* dialog)
    (gobject:connect dialog :closed (lambda (d) (declare (ignore d))
                                     (when (eq dialog *snapshot-dialog*) (setf *snapshot-dialog* nil))
                                     (when (and copy-buffer (member copy-buffer (buffer-list)))
                                       (show-buffer *window* copy-buffer))))
    (labels ((text-pane (title text)
               (gtk:build (gtk:box :orientation :vertical :hexpand t :vexpand t
                            (gtk:label :label title :margin-top 6 :margin-bottom 6)
                            (gtk:scrolled-window :vexpand t
                              :child (make-instance 'gtk:text-view :editable nil :monospace t :wrap-mode :word-char
                                                                  :buffer (make-gtk-text text)))))))
      (adw:dialog-set-child dialog
                           (gtk:build
                             (gtk:box :orientation :vertical :spacing 8 :margin-start 12 :margin-end 12 :margin-top 12 :margin-bottom 12
                               (gtk:label :label (format nil "~a · ~a~%~a" (getf data :name) (snapshot-time-label data)
                                                         (or (getf data :file) "Untitled buffer"))
                                          :wrap t :wrap-mode :word-char :selectable t)
                               (gtk:paned :orientation :horizontal :vexpand t
                                          :start-child (text-pane (if target "Current buffer" "Current file on disk")
                                                                  (if target (buffer-string target)
                                                                      (or (and (getf data :file) (read-file-text (getf data :file))) "")))
                                          :end-child (text-pane "Snapshot" (getf data :text)))
                               (gtk:box :spacing 6 copy restore discard close)))))
    (gobject:connect close :clicked (lambda (b) (declare (ignore b)) (adw:dialog-close dialog)))
    (gobject:connect copy :clicked
                     (lambda (b) (declare (ignore b))
                       ;; Saved history is retained; only recovery records transfer ownership.
                       (setf copy-buffer (open-snapshot-copy (if target (cons nil data) entry)))
                       (adw:dialog-close dialog)))
    (gobject:connect restore :clicked
                     (lambda (b) (declare (ignore b))
                       (restore-history-text target (getf data :text))
                       (adw:dialog-close dialog)
                       (message "Restored ~a in the buffer; review and Save to write it" (buffer-name target))))
    (gobject:connect discard :clicked
                     (lambda (b) (declare (ignore b))
                       (when (probe-file (car entry)) (delete-file (car entry)))
                       (adw:dialog-close dialog)))
    (adw:dialog-present dialog (window-gtk-window *window*))))

(define-command local-history ()
  "Compare a saved revision with the current file, or restore it as an unsaved edit."
  (let* ((buffer (view-buffer (current-tab-view)))
         (entries (and (buffer-file buffer) (saved-revisions (buffer-file buffer)))))
    (unless entries (editor-error "No saved local history for this file yet"))
    (open-picker (window-picker *window*) :items entries
                 :label (lambda (entry) (format nil "~a · ~d characters" (snapshot-time-label (cdr entry)) (length (getf (cdr entry) :text))))
                 :placeholder "Compare which saved revision?"
                 :on-choose (lambda (entry) (show-snapshot entry buffer)))))

(define-command recover-buffers ()
  "Review checkpoints from exited Cadre processes and open separate recovered copies."
  (let ((entries (recoverable-snapshots)))
    (unless entries (editor-error "No unsaved buffers to recover"))
    (open-picker (window-picker *window*) :items entries
                 :label (lambda (entry) (format nil "~a · ~a · ~a" (snapshot-time-label (cdr entry))
                                                  (getf (cdr entry) :name) (or (getf (cdr entry) :file) "Untitled")))
                 :placeholder "Recover which buffer?"
                 :on-choose #'show-snapshot)))

(define-command clear-local-history ()
  "Delete saved local revisions after confirmation; recovery checkpoints are kept."
  (let ((dialog (adw:alert-dialog-new "Clear local history?" "Delete all saved local revisions for all projects. Project files and unsaved recovery checkpoints are kept.")))
    (adw:alert-dialog-add-response dialog "cancel" "Cancel")
    (adw:alert-dialog-add-response dialog "clear" "Clear History")
    (adw:alert-dialog-set-response-appearance dialog "clear" :destructive)
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (when (string= response "clear")
                   (uiop:delete-directory-tree (snapshot-directory :history) :validate t :if-does-not-exist :ignore)
                   (message "Cleared saved local history"))))))

(defun offer-buffer-recovery ()
  (when (and *recover-unsaved-buffers* (recoverable-snapshots))
    (let ((dialog (adw:alert-dialog-new "Unsaved buffers are available" "Cadre found checkpoints from an earlier process. Review them and open recovered copies without overwriting files.")))
      (adw:alert-dialog-add-response dialog "later" "Later")
      (adw:alert-dialog-add-response dialog "review" "Review Buffers")
      (adw:alert-dialog-set-close-response dialog "later")
      (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                 (lambda (response) (when (string= response "review") (call-command 'recover-buffers)))))))

(add-hook '*buffer-created-hook* 'attach-checkpoint)
(add-hook '*buffer-killed-hook* 'forget-buffer-checkpoint)
(add-hook '*before-save-hook* 'history-before-save)
(add-hook '*after-save-hook* 'history-after-save)
