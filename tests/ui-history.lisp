(in-package #:cadre-ui-tests)
(define-test local-history)
(defvar *snapshot-reader-ran* nil)

(defmacro with-history-fixture (&body body)
  `(let* ((directory (merge-pathnames (format nil "cadre-history-test-~a/" (c:make-uuid)) (uiop:temporary-directory)))
          (ui::*recovery-session* nil) (ui::*recovery-lock* nil) (ui::*own-recovery-sessions* '())
          (ui::*local-history* t) (ui::*recover-unsaved-buffers* t)
          (ui::*local-history-revisions* 20) (ui::*local-history-total* 200)
          (ui::*snapshot-max-characters* 1048576)
          (c::*buffers* nil) (c:*buffer-created-hook* nil) (c:*buffer-killed-hook* nil))
     (ensure-directories-exist directory)
     (unwind-protect
          (with-assistant-functions ((ui::state-directory (lambda () directory))) ,@body)
       (ui::unlock-recovery-session)
       (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defun fixture-checkpoint-buffer (file text)
  (let ((buffer (c:make-buffer :file file :text (c:make-string-text text))))
    (setf (c:buffer-local buffer :checkpoint-attached) t (c:buffer-modified-p buffer) t)
    buffer))

(define-test history-deduplication-and-retention :parent local-history
  (with-history-fixture
    (let ((file (merge-pathnames "source.lisp" directory))
          (other (merge-pathnames "other.lisp" directory))
          (ui::*local-history-revisions* 2) (ui::*local-history-total* 3)
          (clock 100))
      (with-assistant-functions ((ui::snapshot-time (lambda () (incf clock))))
        (ui::record-file-revision file "one")
        (ui::record-file-revision file "one")
        (is = 1 (length (ui::saved-revisions file)))
        (ui::record-file-revision file "two")
        (ui::record-file-revision file "three")
        (is equal '("three" "two") (mapcar (lambda (e) (getf (cdr e) :text)) (ui::saved-revisions file)))
        (ui::record-file-revision other "other one")
        (ui::record-file-revision other "other two")
        (is = 3 (length (ui::snapshot-files (ui::snapshot-directory :history))))
        (is equal '("three") (mapcar (lambda (e) (getf (cdr e) :text)) (ui::saved-revisions file)))))))

(define-test atomic-snapshot-failure-keeps-last-copy :parent local-history
  (with-history-fixture
    (let* ((path (merge-pathnames "state.sexp" directory))
           (data '(:version 1 :name "name" :file nil :time 123 :text "safe")))
      (ui::write-snapshot path data)
      (with-assistant-functions
          ((uiop:rename-file-overwriting-target (lambda (&rest args) (declare (ignore args)) (error "injected rename failure"))))
        (fail (ui::write-snapshot path '(:text "replacement")) error))
      (is equal data (ui::read-snapshot path))
      (false (uiop:directory-files directory "*.tmp"))
      (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat path)))))))

(define-test snapshot-reader-does-not-evaluate :parent local-history
  (with-history-fixture
    (let ((path (merge-pathnames "malformed.sexp" directory)) (*snapshot-reader-ran* nil))
      (with-open-file (out path :direction :output) (write-string "#.(setf cadre-ui-tests::*snapshot-reader-ran* t)" out))
      (false (ui::read-snapshot path))
      (false *snapshot-reader-ran*))))

(define-test checkpoints-follow-actual-save-state :parent local-history
  (with-history-fixture
    (let* ((buffer (fixture-checkpoint-buffer (merge-pathnames "draft.lisp" directory) "unsaved")))
      (ui::checkpoint-buffer buffer)
      (let ((path (c:buffer-local buffer :checkpoint-path)))
        (true (probe-file path))
        (is equal "unsaved" (getf (ui::read-snapshot path) :text))
        ;; A save finished while newer edits remain: checkpoint the new edits.
        (setf (c:buffer-local buffer :disk-text) "older saved text")
        (ui::history-after-save buffer)
        (true (probe-file path))
        (is equal "older saved text" (getf (cdar (ui::saved-revisions (c:buffer-file buffer))) :text))
        (setf (c:buffer-modified-p buffer) nil)
        (ui::history-after-save buffer)
        (false (probe-file path))))))

(define-test recovery-ignores-live-sessions :parent local-history
  (with-history-fixture
    (let* ((buffer (fixture-checkpoint-buffer nil "new untitled text"))
           (dead (merge-pathnames "99999999-dead/draft.sexp" (ui::snapshot-directory :recovery))))
      (ui::checkpoint-buffer buffer)
      (ui::write-snapshot dead '(:version 1 :file nil :name "untitled" :time 100 :text "orphan"))
      (let ((entries (ui::recoverable-snapshots)))
        (is = 1 (length entries))
        (is equal dead (caar entries)))
      (ui::forget-buffer-checkpoint buffer)
      (false (probe-file (c:buffer-local buffer :checkpoint-path)))
      (true (probe-file dead)))))

(define-test recovery-goes-by-lock-not-process-id :parent local-history
  (with-history-fixture
    (let* ((recovery (ui::snapshot-directory :recovery))
           ;; Named for a live process (this one), as a crashed Cadre's
           ;; folder would be once its process ID is reused, but unlocked.
           (reused (merge-pathnames (format nil "~d-crashed/" (sb-posix:getpid)) recovery))
           (locked (merge-pathnames "1-other-cadre/" recovery))
           (holder nil))
      (dolist (folder (list reused locked))
        (ui::write-snapshot (merge-pathnames "draft.sexp" folder)
                            '(:version 1 :file nil :name "untitled" :time 100 :text "orphan"))
        (with-open-file (out (ui::recovery-lock-file folder) :direction :output :if-does-not-exist :create)))
      (unwind-protect
           (progn
             ;; Another process holds the lock of the second.
             (setf holder (uiop:launch-program
                           (list "python3" "-c" "import fcntl,sys,time; f=open(sys.argv[1],'r+'); fcntl.lockf(f, fcntl.LOCK_EX); print('locked', flush=True); time.sleep(30)"
                                 (namestring (ui::recovery-lock-file locked)))
                           :output :stream))
             (is string= "locked" (read-line (uiop:process-info-output holder)))
             (false (ui::recovery-session-active-p reused))
             (true (ui::recovery-session-active-p locked))
             (is equal (list (merge-pathnames "draft.sexp" reused))
                 (mapcar #'car (ui::recoverable-snapshots)))
             ;; This process's own folder is in use, and checking it keeps its lock.
             (let ((own (ui::recovery-session-directory)))
               (true (ui::recovery-session-active-p own))
               (true (ui::recovery-session-active-p own))
               (true ui::*recovery-lock*))
             (uiop:terminate-process holder)
             (uiop:wait-process holder)
             (setf holder nil)
             (false (ui::recovery-session-active-p locked)))
        (when holder (uiop:terminate-process holder) (uiop:wait-process holder))))))

(define-test recovery-copy-preserves-newer-disk-file :parent local-history
  (with-history-fixture
    (let* ((file (merge-pathnames "newer.lisp" directory))
           (old (merge-pathnames "99999999-dead/draft.sexp" (ui::snapshot-directory :recovery)))
           (data (list :version 1 :file (namestring file) :name "newer.lisp" :time 100 :text "recovered edits"))
           (c:*buffer-created-hook* (list (lambda (buffer) (setf (c:buffer-local buffer :checkpoint-attached) t)))))
      (with-open-file (out file :direction :output) (write-string "newer disk contents" out))
      (ui::write-snapshot old data)
      (with-assistant-functions
          ((ui::make-gtk-text (lambda (&optional (text "")) (c:make-string-text text)))
           (ui::show-buffer (lambda (&rest args) (declare (ignore args))))
           (ui::message (lambda (&rest args) (declare (ignore args)))))
        (let ((buffer (ui::open-snapshot-copy (cons old data))))
          (false (c:buffer-file buffer))
          (true (c:buffer-modified-p buffer))
          (is equal "recovered edits" (c:buffer-string buffer))
          (is equal "newer disk contents" (uiop:read-file-string file))
          (true (probe-file (c:buffer-local buffer :checkpoint-path)))
          (false (probe-file old)))))))

(define-test disabled-and-oversized-snapshots :parent local-history
  (with-history-fixture
    (let ((ui::*snapshot-max-characters* 3)
          (file (merge-pathnames "file.lisp" directory)))
      (false (ui::record-file-revision file "too large"))
      (let ((ui::*local-history* nil)) (false (ui::record-file-revision file "ok")))
      (false (ui::snapshot-files (ui::snapshot-directory :history))))))

(define-test checkpoint-cleanup-keeps-unreviewed-recovery :parent local-history
  (with-history-fixture
    (let* ((buffer (fixture-checkpoint-buffer nil "current draft"))
           (orphan (merge-pathnames "99999999-dead/other.sexp" (ui::snapshot-directory :recovery))))
      (ui::checkpoint-buffer buffer)
      (ui::write-snapshot orphan '(:version 1 :name "other" :file nil :time 100 :text "unreviewed"))
      (ui::discard-session-checkpoints)
      (false (probe-file (c:buffer-local buffer :checkpoint-path)))
      (true (probe-file orphan))
      (false ui::*recovery-session*))))

(define-test snapshots-roundtrip-unicode-and-reader-characters :parent local-history
  (with-history-fixture
    (let* ((path (merge-pathnames "unicode.sexp" directory))
           (text (format nil "λ 日本語~%\"quoted\" #.(never-evaluate) \\"))
           (data (list :version 1 :file nil :name "untitled" :time 100 :text text)))
      (ui::write-snapshot path data)
      (is equal data (ui::read-snapshot path)))))

(define-test cancelled-close-keeps-recovery-until-discard-is-accepted :parent local-history
  (with-history-fixture
    (with-window-close-fixture
      (setf (c:buffer-local buffer :checkpoint-attached) t)
      (ui::checkpoint-buffer buffer)
      (let ((path (c:buffer-local buffer :checkpoint-path)))
        (true (ui::request-close win))
        (true (probe-file path))
        (funcall (first dialogs) nil)
        (true (probe-file path))
        (true (ui::request-close win))
        (funcall (first dialogs) t)
        (false (probe-file path))
        (is = 1 destroyed)))))

(define-test close-setup-error-does-not-clear-recovery :parent local-history
  (with-history-fixture
    (with-window-close-fixture
      (setf (c:buffer-local buffer :checkpoint-attached) t)
      (ui::checkpoint-buffer buffer)
      (setf (symbol-function 'ui::save-session) (lambda (w) (declare (ignore w)) (error "session failure")))
      (fail (ui::request-close win) error)
      (true (probe-file (c:buffer-local buffer :checkpoint-path))))))

(define-test failed-scratch-autosave-keeps-checkpoint-on-close :parent local-history
  (with-history-fixture
    (let ((buffer (fixture-checkpoint-buffer nil "scratch edits that could not be saved")))
      (setf (c:buffer-local buffer :scratch) t)
      (ui::checkpoint-buffer buffer)
      (let ((path (c:buffer-local buffer :checkpoint-path)))
        (ui::forget-buffer-checkpoint buffer)
        (true (probe-file path))
        (ui::discard-session-checkpoints)
        (true (probe-file path))
        (is equal "scratch edits that could not be saved" (getf (ui::read-snapshot path) :text))))))

(define-test learned-indentation-by-project :parent local-history
  (with-history-fixture
    (let ((project-a (merge-pathnames "a/" directory))
          (project-b (merge-pathnames "b/" directory))
          (ui::*remember-learned-indentation* t))
      (unwind-protect
           (progn
             (c:learn-indentation '(("frobnicate-thing" (4 4 "&body") ("A"))))
             (ui::save-project-indentation project-a)
             (c:learn-indentation '(("frobnicate-thing" nil ("A"))))
             (is eq nil (c:indentation-spec "frobnicate-thing"))
             (ui::restore-project-indentation project-b)
             (is eq nil (c:indentation-spec "frobnicate-thing") "another project's macros stay out")
             (ui::restore-project-indentation project-a)
             (is = 2 (c:indentation-spec "frobnicate-thing"))
             (c:learn-indentation '(("frobnicate-thing" nil ("A"))))
             (let ((ui::*remember-learned-indentation* nil))
               (ui::restore-project-indentation project-a)
               (is eq nil (c:indentation-spec "frobnicate-thing") "the setting turns it off")))
        (c:learn-indentation '(("frobnicate-thing" nil ())))))))

(define-test learned-indentation-switching-projects :parent local-history
  (with-history-fixture
    (let ((project-a (merge-pathnames "a/" directory))
          (project-b (merge-pathnames "b/" directory))
          (ui::*remember-learned-indentation* t)
          (ui::*indentation-save-pending* nil))
      (unwind-protect
           (progn
             (c:learn-indentation '(("frob-around" (4 "&body") ("A"))))
             (ui::save-project-indentation project-a)
             (c:forget-learned-indentation)
             (c:learn-indentation '(("frob-around" (4 4 4 "&body") ("B"))))
             (ui::save-project-indentation project-b)
             (ui::restore-project-indentation project-a)
             (is = 1 (c:indentation-spec "frob-around"))
             (ui::restore-project-indentation project-b)
             (is = 3 (c:indentation-spec "frob-around") "the new project's spec replaces the old one's")
             (c:learn-indentation '(("only-in-b" (4 "&body") ("B"))))
             (setf ui::*indentation-save-pending* project-b)
             (ui::restore-project-indentation project-a)
             (false (c:indentation-spec "only-in-b") "switching forgets what the other project learned")
             (is equal 1 (cdr (assoc "only-in-b" (cdr (assoc (ui::project-key project-b) (ui::read-saved-indentation)
                                                             :test #'equal))
                                     :test #'equal))
                 "and saves it for that project first"))
        (c:forget-learned-indentation)))))
