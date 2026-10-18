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
          (ui::save-conversation-now conversation)
          ;; Distinct times, oldest first.
          (let ((time (+ 1000000000 n)))
            (sb-posix:utimes (namestring (ui::conversation-file (ui::cv-id conversation))) time time)))
        (ui::end-current-conversation :claude))
      (ui::prune-conversations)
      (is equal '("conversation 1" "conversation 2")
          (sort (mapcar (lambda (r) (getf r :title)) (ui::saved-conversations)) #'string<)))))

(define-test diff-excerpts :parent conversations
  (let ((diff (c::diff-lines (loop for i below 20 collect (format nil "line ~d" i))
                             (loop for i below 20 collect (if (member i '(3 15)) (format nil "LINE ~d" i) (format nil "line ~d" i))))))
    (is equal (format nil "  line 1~%  line 2~%- line 3~%+ LINE 3~%  line 4~%  line 5~%⋯~%  line 13~%  line 14~%- line 15~%+ LINE 15~%  line 16~%  line 17~%")
        (ui::diff-excerpt diff))
    (is = 3 (count #\Newline (ui::diff-excerpt diff :limit 2)))))
