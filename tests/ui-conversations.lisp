;;;; Assistant conversations kept across sessions (conversations.lisp).
(in-package #:cadre-ui-tests)

(define-test conversations)

(defmacro with-conversations-fixture ((&key (provider :claude)) &body body)
  `(let ((state (merge-pathnames (format nil "cadre-conversations-~a/" (c:make-uuid)) (uiop:temporary-directory)))
         (ui::*chat* (ui::make-chat :provider ,provider :session-id "session-1" :directory #P"/tmp/project/"
                                    :codex-thread-id "thread-1" :codex-directory #P"/tmp/project/")))
     (ensure-directories-exist state)
     (unwind-protect
          ;; Saves wait for a timer that never fires here; tests save explicitly.
          (with-assistant-functions ((ui::state-directory (lambda () state))
                                     (glib:timeout-add (lambda (&rest args) (declare (ignore args)) 1))
                                     (glib:source-remove (lambda (id) (declare (ignore id)) t)))
            ,@body)
       (uiop:delete-directory-tree state :validate t :if-does-not-exist :ignore))))

(defun only-saved-conversation ()
  (let ((records (ui::saved-conversations)))
    (is = 1 (length records))
    (first records)))

(define-test conversation-is-recorded :parent conversations
  (with-conversations-fixture ()
    ;; Nothing before the user's first message is recorded.
    (false (ui::record-chat-entry :note :text "Chat mode."))
    (ui::record-chat-entry :user :text (format nil "Why does ~a fail?~%More detail" (make-string 90 :initial-element #\x)))
    (ui::record-chat-entry :thinking :text "Looking")
    (ui::record-tool-start "t1" "⚙ read_buffer  foo.lisp")
    (ui::record-tool-start "t1" "⚙ read_buffer  foo.lisp (full)")
    (ui::record-tool-result "t1" (make-string 5000 :initial-element #\r) nil)
    (let ((approval (ui::record-chat-entry :approval :title "Assistant wants to evaluate" :detail "(+ 1 2)" :answer nil)))
      (ui::update-chat-entry approval :answer "Allowed for this conversation"))
    (let* ((buffer (c:make-buffer :name "foo.lisp" :text (c:make-string-text "")))
           (edit (ui::record-proposed-edit buffer (format nil "a~%b~%c~%") (format nil "a~%B~%c~%") "Capitalize b")))
      (ui::update-chat-entry edit :outcome :rejected))
    (ui::record-chat-entry :assistant :text "Because **x**.")
    (ui::save-conversation-now (ui::current-conversation))
    (let* ((record (only-saved-conversation))
           (entries (getf record :entries)))
      (is eq :claude (getf record :provider))
      (is equal "session-1" (getf record :session-id))
      (is equal "/tmp/project/" (getf record :directory))
      ;; The title is the first line of the first message, shortened.
      (is = 81 (length (getf record :title)))
      (is equal '(:user :thinking :tool :approval :edit :assistant) (mapcar #'first entries))
      (let ((tool (rest (third entries))))
        (is equal "⚙ read_buffer  foo.lisp (full)" (getf tool :title))
        (is = 4001 (length (getf tool :result))))
      (is equal "Allowed for this conversation" (getf (rest (fourth entries)) :answer))
      (let ((edit (rest (fifth entries))))
        (is eq :rejected (getf edit :outcome))
        (is = 1 (getf edit :added))
        (is = 1 (getf edit :removed))
        (true (search (format nil "- b~%+ B") (getf edit :diff)))))))

(define-test ended-conversation-keeps-its-session :parent conversations
  (with-conversations-fixture ()
    (ui::record-chat-entry :user :text "first")
    (let ((first-id (ui::cv-id (ui::current-conversation))))
      ;; A new conversation: the old one is saved before the session changes.
      (ui::end-current-conversation :claude)
      (setf (ui::chat-session-id ui::*chat*) "session-2")
      (false (ui::current-conversation))
      (false (ui::record-chat-entry :note :text "after"))
      (ui::record-chat-entry :user :text "second")
      (false (equal first-id (ui::cv-id (ui::current-conversation))))
      (ui::save-conversation-now (ui::current-conversation))
      (let ((records (ui::saved-conversations)))
        (is equal '("session-1" "session-2")
            (sort (mapcar (lambda (r) (getf r :session-id)) records) #'string<))
        (is equal '(("first") ("second"))
            (sort (mapcar (lambda (r) (mapcar (lambda (e) (getf (rest e) :text)) (getf r :entries))) records)
                  #'string< :key #'first))))))

(define-test codex-conversation-records-its-thread :parent conversations
  (with-conversations-fixture (:provider :codex)
    (setf (ui::chat-codex-thread-id ui::*chat*) nil)
    (ui::record-chat-entry :user :text "hello")
    ;; The thread is known once Codex starts it.
    (setf (ui::chat-codex-thread-id ui::*chat*) "thread-9")
    (ui::record-chat-entry :assistant :text "hi")
    (ui::end-current-conversation :codex)
    (let ((record (only-saved-conversation)))
      (is eq :codex (getf record :provider))
      (is equal "thread-9" (getf record :thread-id)))))

(define-test bad-conversation-files-are-skipped :parent conversations
  (with-conversations-fixture ()
    (ensure-directories-exist (ui::conversations-directory))
    (with-open-file (out (ui::conversation-file "bad") :direction :output)
      (write-string "(:version 1 :id \"bad\" :provider :claude :entries #.(error \"evaluated\"))" out))
    (with-open-file (out (ui::conversation-file "wrong") :direction :output)
      (prin1 '(:version 1 :id "wrong" :provider :other :started 1 :updated 1 :entries ()) out))
    (false (ui::saved-conversations))))

(define-test old-conversations-are-pruned :parent conversations
  (with-conversations-fixture ()
    (let ((ui::*assistant-conversations-kept* 2))
      (dotimes (n 3)
        (ui::record-chat-entry :user :text (format nil "conversation ~d" n))
        (let ((conversation (ui::current-conversation)))
          ;; Activity order is independent of filesystem time and flushes.
          (setf (ui::cv-started conversation) (+ 1000000000 n)
                (ui::cv-updated conversation) (+ 1000000000 n))
          (ui::save-conversation-now conversation))
        (ui::end-current-conversation :claude))
      (ui::prune-conversations)
      (is equal '("conversation 1" "conversation 2")
          (sort (mapcar (lambda (r) (getf r :title)) (ui::saved-conversations)) #'string<)))))

(define-test idle-flush-does-not-displace-recent-history :parent conversations
  (with-conversations-fixture ()
    (let ((ui::*assistant-conversations-kept* 2))
      (dolist (spec '(("old-idle" 100 400) ("recent" 200 200) ("current" 300 300)))
        (destructuring-bind (id updated mtime) spec
          (let* ((conversation (ui::make-conversation :id id :provider :claude :title id
                                                       :started updated :updated updated))
                 (file (ui::conversation-file id)))
            (ui::write-snapshot file (ui::conversation-record conversation))
            (sb-posix:utimes (namestring file) mtime mtime))))
      (ui::prune-conversations)
      (is equal '("current" "recent")
          (mapcar (lambda (r) (getf r :title)) (ui::saved-conversations))))))

(define-test backend-start-persists-before-any-reply :parent conversations
  (with-conversations-fixture (:provider :codex)
    (let ((server (c::make-codex-app-server)))
      (setf (ui::chat-codex-process ui::*chat*) server
            (ui::chat-codex-thread-id ui::*chat*) nil
            (ui::chat-codex-directory ui::*chat*) nil)
      (ui::record-chat-entry :user :text "hello")
      (ui::save-current-conversations)
      (false (ui::cv-save-timer (ui::current-conversation)))
      (setf (ui::chat-codex-directory ui::*chat*) #P"/tmp/backend-project/")
      (ui::handle-codex-chat-event server (c::make-codex-event :kind :thread-started :thread-id "new-thread"))
      (true (ui::cv-save-timer (ui::current-conversation)))
      (is equal "new-thread" (ui::cv-thread-id (ui::current-conversation)))
      (ui::save-current-conversations)
      (let ((record (only-saved-conversation)))
        (is equal "new-thread" (getf record :thread-id))
        (is equal "/tmp/backend-project/" (getf record :directory))
        (is = 1 (length (getf record :entries)))))))

(define-test claude-init-persists-its-session :parent conversations
  (with-conversations-fixture ()
    (ui::record-chat-entry :user :text "hello")
    (ui::save-current-conversations)
    (ui::handle-claude-event (c::make-claude-event :kind :init :session-id "backend-session"
                                                  :data '(("cadre" . "connected"))))
    (true (ui::cv-save-timer (ui::current-conversation)))
    (ui::save-current-conversations)
    (is equal "backend-session" (getf (only-saved-conversation) :session-id))))

(define-test final-flush-refreshes-only-the-current-conversation :parent conversations
  (with-conversations-fixture (:provider :codex)
    (setf (ui::chat-codex-thread-id ui::*chat*) nil)
    (ui::record-chat-entry :user :text "before thread start")
    (let ((ended (ui::current-conversation)))
      (setf (ui::chat-codex-thread-id ui::*chat*) "first-thread")
      (ui::save-current-conversations)
      (is equal "first-thread" (getf (only-saved-conversation) :thread-id))
      (ui::end-current-conversation :codex)
      (setf (ui::chat-codex-thread-id ui::*chat*) "second-thread")
      (ui::record-chat-entry :user :text "new conversation")
      (ui::save-conversation-now ended)
      (is equal "first-thread" (getf (ui::read-conversation (ui::conversation-file (ui::cv-id ended))) :thread-id)))))

(define-test malformed-conversation-records-do-not-hide-valid-history :parent conversations
  (with-conversations-fixture ()
    (ui::record-chat-entry :user :text "valid")
    (ui::save-current-conversations)
    (dolist (text '("(:version 1 :id)" "(:version 1 . broken)" "#1=(:version 1 . #1#)"
                    "(:version 1 :id \"broken\" :provider :claude :started 1 :updated 1 :entries ((:user :text)))"))
      (with-open-file (out (ui::conversation-file "broken") :direction :output :if-exists :supersede)
        (write-string text out))
      (false (ui::read-conversation (ui::conversation-file "broken")))
      (is equal '("valid") (mapcar (lambda (r) (getf r :title)) (ui::saved-conversations))))))

(define-test diff-excerpts :parent conversations
  (let ((diff (c::diff-lines (loop for i below 20 collect (format nil "line ~d" i))
                             (loop for i below 20 collect (if (member i '(3 15)) (format nil "LINE ~d" i) (format nil "line ~d" i))))))
    (is equal (format nil "  line 1~%  line 2~%- line 3~%+ LINE 3~%  line 4~%  line 5~%⋯~%  line 13~%  line 14~%- line 15~%+ LINE 15~%  line 16~%  line 17~%")
        (ui::diff-excerpt diff))
    (is = 3 (count #\Newline (ui::diff-excerpt diff :limit 2)))))

(define-test pruning-reads-records-only-when-over-the-limit :parent conversations
  (with-conversations-fixture ()
    (let ((ui::*assistant-conversations-kept* 3)
          (reads 0))
      (dotimes (n 3)
        (ui::write-snapshot (ui::conversation-file (format nil "c~d" n))
                            (ui::conversation-record (ui::make-conversation :id (format nil "c~d" n) :provider :claude
                                                                            :title (format nil "c~d" n)
                                                                            :started n :updated n))))
      (let ((original (symbol-function 'ui::read-conversation)))
        (with-assistant-functions ((ui::read-conversation (lambda (file) (incf reads) (funcall original file))))
          ;; At the limit: nothing is read.
          (ui::prune-conversations)
          (is = 0 reads)
          ;; Over it: the oldest by activity goes.
          (ui::write-snapshot (ui::conversation-file "c3")
                              (ui::conversation-record (ui::make-conversation :id "c3" :provider :claude :title "c3"
                                                                              :started 3 :updated 3)))
          (ui::prune-conversations)
          (is = 4 reads)))
      (is equal '("c3" "c2" "c1") (mapcar (lambda (r) (getf r :title)) (ui::saved-conversations))))))
