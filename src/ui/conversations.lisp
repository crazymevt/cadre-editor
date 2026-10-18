;;;; conversations.lisp — assistant conversations kept across sessions
;;;;
;;;; Cadre records each conversation the assistant panel holds, Claude's or
;;;; Codex's, as it goes: the user's messages, replies, thinking, tool rows
;;;; with their results, plans, notes, the approvals asked for with the
;;;; user's answers, and proposed edits with an excerpt of their diff and
;;;; what became of them. Each is a file in the state directory
;;;; (conversations/ID.sexp, private to the user).
;;;;
;;;; Conversations (the panel's history button, or M-x
;;;; assistant-conversations) lists them; choosing one shows its transcript
;;;; again and makes it the provider's current conversation, so the next
;;;; message resumes it: Claude Code's session (--resume, in the folder it
;;;; ran in) or Codex's thread (thread/resume). Restoring never restores what
;;;; was allowed "for this conversation", and runs nothing again: tools,
;;;; evaluations and edits are shown as they were.

(in-package #:cadre-ui)

(define-option *assistant-conversations-kept* 100 integer
  "The most assistant conversations kept for Conversations; older ones are deleted."
  :category "Assistant")

(defparameter *conversation-tool-result-limit* 4000
  "Characters of a tool's result kept in a conversation's record.")

(defvar *replaying-conversation* nil
  "True while a restored conversation's transcript is shown, so showing it
doesn't record it again.")

(defstruct (conversation (:conc-name cv-))
  (id (make-uuid)) provider session-id thread-id directory title
  (started (get-universal-time)) (updated (get-universal-time))
  (entries (make-array 0 :adjustable t :fill-pointer 0))
  ;; Tool call ID → its entry, while the conversation is current.
  (tools (make-hash-table :test 'equal))
  (save-timer nil))

;;; The current conversation of each provider

(defun conversations-directory () (merge-pathnames "conversations/" (state-directory)))

(defun conversation-file (id) (merge-pathnames (format nil "~a.sexp" id) (conversations-directory)))

(defun current-conversation (&optional (provider (and *chat* (chat-provider *chat*))))
  (and *chat* (chat-conversations *chat*) (gethash provider (chat-conversations *chat*))))

(defun start-conversation (provider)
  (let ((conversation (make-conversation :provider provider)))
    (setf (gethash provider (chat-conversations *chat*)) conversation)))

(defun end-current-conversation (provider)
  "PROVIDER's conversation ends (a new one starts, or another is restored):
save it now, and record the next one apart."
  (let ((conversation (current-conversation provider)))
    (when conversation
      (save-conversation-now conversation)
      (remhash provider (chat-conversations *chat*)))))

(defun conversation-entry (kind &rest plist)
  (list* kind plist))

(defun record-chat-entry (kind &rest plist)
  "Add an entry of KIND to the conversation shown in the panel: a :user
message starts one if there is none. Returns the entry, a list whose plist
part may be changed later (a tool's result, an approval's answer)."
  (when (and *chat* (not *replaying-conversation*))
    (let ((conversation (or (current-conversation)
                            (and (eq kind :user) (start-conversation (chat-provider *chat*))))))
      (when conversation
        (let ((entry (apply #'conversation-entry kind plist)))
          (when (and (eq kind :user) (null (cv-title conversation)))
            (let* ((text (string-trim '(#\Space #\Newline #\Tab) (getf plist :text)))
                   (line (subseq text 0 (or (position #\Newline text) (length text)))))
              (setf (cv-title conversation) (if (> (length line) 80) (concatenate 'string (subseq line 0 80) "…") line))))
          (vector-push-extend entry (cv-entries conversation))
          (conversation-changed conversation)
          entry)))))

(defun update-chat-entry (entry &rest plist)
  "Change ENTRY's fields (an entry record-chat-entry returned), and save."
  (when entry
    (loop for (key value) on plist by #'cddr
          do (setf (getf (rest entry) key) value))
    (let ((conversation (current-conversation)))
      (when conversation (conversation-changed conversation)))))

(defun record-tool-start (id title)
  (let ((conversation (current-conversation)))
    (when (and conversation (not *replaying-conversation*))
      (let ((entry (gethash id (cv-tools conversation))))
        (if entry
            (update-chat-entry entry :title title)
            (setf (gethash id (cv-tools conversation)) (record-chat-entry :tool :title title)))))))

(defun record-tool-result (id text error)
  (let* ((conversation (current-conversation))
         (entry (and conversation (gethash id (cv-tools conversation)))))
    (when (and entry (not *replaying-conversation*))
      (update-chat-entry entry
                         :result (if (> (length text) *conversation-tool-result-limit*)
                                     (concatenate 'string (subseq text 0 *conversation-tool-result-limit*) "…")
                                     text)
                         :error (and error t)))))

(defun diff-excerpt (diff &key (context 2) (limit 300))
  "DIFF (from diff-lines) as text: changed lines marked + and -, with CONTEXT
unchanged lines around them, at most LIMIT lines."
  (let* ((lines (coerce diff 'vector))
         (keep (make-array (length lines) :initial-element nil)))
    (loop for i below (length lines)
          unless (eq (first (aref lines i)) :same)
            do (loop for j from (max 0 (- i context)) to (min (1- (length lines)) (+ i context))
                     do (setf (aref keep j) t)))
    (with-output-to-string (out)
      (let ((count 0) (gap nil))
        (loop for i below (length lines)
              do (cond ((not (aref keep i)) (setf gap t))
                       ((>= count limit) (format out "…~%") (return))
                       (t (when (and gap (plusp count)) (format out "⋯~%"))
                          (setf gap nil)
                          (incf count)
                          (destructuring-bind (kind line) (aref lines i)
                            (format out "~a ~a~%" (ecase kind (:same " ") (:added "+") (:removed "-")) line)))))))))

(defun record-proposed-edit (buffer old new explanation)
  "Record an edit the assistant proposes to BUFFER (OLD text to NEW); its
outcome is filled in when the user answers."
  (let ((diff (diff-lines (split-lines old) (split-lines new))))
    (multiple-value-bind (added removed) (diff-stats diff)
      (record-chat-entry :edit :file (if (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)) (buffer-name buffer))
                               :explanation explanation :added added :removed removed
                               :diff (diff-excerpt diff) :outcome nil))))

;;; Saving

(defun note-conversation-ids (conversation)
  "Copy into CONVERSATION, the provider's current one, the session or thread
it runs in and its folder, as the chat has them now."
  (multiple-value-bind (id directory)
      (ecase (cv-provider conversation)
        (:claude (values (chat-session-id *chat*) (chat-directory *chat*)))
        (:codex (values (chat-codex-thread-id *chat*) (chat-codex-directory *chat*))))
    (when id
      (if (eq (cv-provider conversation) :codex)
          (setf (cv-thread-id conversation) id)
          (setf (cv-session-id conversation) id)))
    (when directory (setf (cv-directory conversation) directory))))

(defun conversation-record (conversation)
  "CONVERSATION as plain data for its file."
  (list :version 1 :id (cv-id conversation) :provider (cv-provider conversation)
        :session-id (cv-session-id conversation) :thread-id (cv-thread-id conversation)
        :directory (and (cv-directory conversation) (namestring (cv-directory conversation)))
        :title (cv-title conversation) :started (cv-started conversation) :updated (cv-updated conversation)
        :entries (coerce (cv-entries conversation) 'list)))

(defun conversation-changed (conversation)
  "Note a change, and save CONVERSATION a moment from now (once for a burst
of changes)."
  (setf (cv-updated conversation) (get-universal-time))
  (when (and *chat* (eq conversation (current-conversation (cv-provider conversation))))
    (note-conversation-ids conversation))
  (unless (cv-save-timer conversation)
    (setf (cv-save-timer conversation)
          (glib:timeout-add glib:+priority-default+ 500
                            (lambda ()
                              (setf (cv-save-timer conversation) nil)
                              (save-conversation-now conversation)
                              nil)))))

(defun save-conversation-now (conversation)
  (let ((timer (cv-save-timer conversation)))
    (when timer
      (glib:source-remove timer)
      (setf (cv-save-timer conversation) nil)))
  (handler-case
      (progn
        (write-snapshot (conversation-file (cv-id conversation)) (conversation-record conversation))
        (prune-conversations))
    (error (e) (message "Could not save the assistant conversation: ~a" e))))

(defun save-current-conversations ()
  "Save the providers' conversations now (when quitting)."
  (when (and *chat* (chat-conversations *chat*))
    (loop for conversation being the hash-values of (chat-conversations *chat*)
          do (save-conversation-now conversation))))

(defun conversation-files ()
  (when (uiop:directory-exists-p (conversations-directory))
    (uiop:directory-files (conversations-directory) "*.sexp")))

(defun prune-conversations ()
  "Delete the oldest conversation files beyond *ASSISTANT-CONVERSATIONS-KEPT*."
  (let ((files (sort (conversation-files) #'> :key (lambda (f) (or (ignore-errors (file-write-date f)) 0)))))
    (dolist (file (nthcdr (max 1 *assistant-conversations-kept*) files))
      (ignore-errors (delete-file file)))))

;;; Reading

(defun valid-conversation-entry-p (entry)
  (and (consp entry) (keywordp (first entry)) (listp (rest entry))
       (evenp (length (rest entry)))))

(defun read-conversation (file)
  "The conversation record in FILE, or nil if it isn't one."
  (let ((data (ignore-errors
               (with-open-file (in file :external-format :utf-8)
                 (with-standard-io-syntax
                   (let ((*read-eval* nil) (*package* (find-package :keyword)))
                     (read in nil nil)))))))
    (when (and (listp data) (eql 1 (getf data :version))
               (stringp (getf data :id)) (member (getf data :provider) '(:claude :codex))
               (integerp (getf data :started)) (integerp (getf data :updated))
               (typep (getf data :title) '(or null string))
               (typep (getf data :directory) '(or null string))
               (typep (getf data :session-id) '(or null string))
               (typep (getf data :thread-id) '(or null string))
               (listp (getf data :entries)) (every #'valid-conversation-entry-p (getf data :entries)))
      data)))

(defun saved-conversations ()
  "The saved conversation records, the most recently used first."
  (sort (remove nil (mapcar #'read-conversation (conversation-files))) #'> :key (lambda (r) (getf r :updated))))

;;; Showing a restored conversation

(defun universal-time-label (time)
  (multiple-value-bind (s m h day month year) (decode-universal-time time)
    (declare (ignore s))
    (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d" year month day h m)))

(defun conversation-label (record)
  (format nil "~a · ~a~@[ · ~a~] · ~a"
          (universal-time-label (getf record :updated))
          (if (eq (getf record :provider) :codex) "Codex" "Claude")
          (and (getf record :directory)
               (car (last (pathname-directory (uiop:ensure-directory-pathname (getf record :directory))))))
          (or (getf record :title) "Untitled")))

(defun edit-outcome-text (outcome)
  (case outcome
    (:saved "Accepted and saved")
    (:accepted "Accepted")
    (:rejected "Rejected")
    (:changed "Not applied: the file changed during the review")
    (t "Not answered")))

(defun show-past-tool (title result error)
  (let* ((result-label (make-instance 'gtk:label :label (or result "") :xalign 0.0 :wrap t :selectable t
                                                 :wrap-mode :char :css-classes '("monospace" "dim-label")))
         (text (format nil "~a~:[~; — failed~]" title error))
         (expander (make-instance 'gtk:expander
                                  :label-widget (make-instance 'gtk:label :label text :xalign 0.0 :ellipsize :end :hexpand t)
                                  :child result-label :css-classes '("cadre-chat-tool") :tooltip-text text)))
    (setf (chat-stream-box *chat*) nil)
    (chat-append expander)))

(defun show-past-approval (title detail answer)
  (let ((card (make-instance 'gtk:box :orientation :vertical :spacing 6 :css-classes '("cadre-chat-approval"))))
    (gtk:box-append card (make-instance 'gtk:label :label title :xalign 0.0 :wrap t :css-classes '("heading")))
    (gtk:box-append card (make-instance 'gtk:label :label detail :xalign 0.0 :wrap t :selectable t
                                                   :wrap-mode :char :css-classes '("monospace")))
    (gtk:box-append card (make-instance 'gtk:label :label (or answer "Not answered") :xalign 0.0 :css-classes '("dim-label")))
    (setf (chat-stream-box *chat*) nil)
    (chat-append card)))

(defun show-past-edit (entry)
  (let* ((plist (rest entry))
         (card (make-instance 'gtk:box :orientation :vertical :spacing 6 :css-classes '("cadre-chat-approval")))
         (diff (make-instance 'gtk:label :label (or (getf plist :diff) "") :xalign 0.0 :selectable t
                                         :wrap t :wrap-mode :char :css-classes '("monospace"))))
    (gtk:box-append card (make-instance 'gtk:label
                                        :label (format nil "Proposed edit to ~a  +~d −~d"
                                                       (getf plist :file) (or (getf plist :added) 0) (or (getf plist :removed) 0))
                                        :xalign 0.0 :wrap t :wrap-mode :char :css-classes '("heading")))
    (when (getf plist :explanation)
      (gtk:box-append card (make-instance 'gtk:label :label (getf plist :explanation) :xalign 0.0 :wrap t)))
    (gtk:box-append card (make-instance 'gtk:expander :label "Changes" :child diff))
    (gtk:box-append card (make-instance 'gtk:label :label (edit-outcome-text (getf plist :outcome))
                                                   :xalign 0.0 :css-classes '("dim-label")))
    (setf (chat-stream-box *chat*) nil)
    (chat-append card)))

(defun show-conversation-entry (entry)
  (let ((plist (rest entry)))
    (case (first entry)
      (:user (chat-user-message (getf plist :text)))
      (:assistant (chat-finish-text (getf plist :text)))
      (:thinking (chat-thinking (getf plist :text)))
      (:tool (show-past-tool (getf plist :title) (getf plist :result) (getf plist :error)))
      (:plan (setf (chat-plan-box *chat*) nil)
       (show-plan (ignore-errors (parse-json (getf plist :input))))
       (setf (chat-plan-box *chat*) nil))
      (:note (apply #'chat-note (getf plist :text) (getf plist :classes)))
      (:approval (show-past-approval (getf plist :title) (getf plist :detail) (getf plist :answer)))
      (:edit (show-past-edit entry)))))

(defun restore-conversation (record)
  "Show RECORD's transcript in the panel and make it its provider's current
conversation, so the next message resumes it. What was allowed for it isn't
restored, and nothing in it runs again."
  (when (chat-busy *chat*) (editor-error "Wait for the current assistant turn to finish"))
  (let ((provider (getf record :provider))
        (directory (and (getf record :directory) (uiop:ensure-directory-pathname (getf record :directory)))))
    (unless (eq provider (chat-provider *chat*))
      (gtk:drop-down-set-selected (chat-provider-dropdown *chat*) (if (eq provider :codex) 1 0)))
    (unless (eq provider (chat-provider *chat*)) (editor-error "Could not switch to ~(~a~)" provider))
    ;; A fresh start for the provider: its process stops, its approvals go.
    (claude-new-chat)
    (clear-box (chat-messages *chat*))
    (let ((*replaying-conversation* t))
      (dolist (entry (getf record :entries))
        (handler-case (show-conversation-entry entry)
          (error (e) (chat-note (format nil "(An entry couldn't be shown: ~a)" e))))))
    (let* ((resume-id (if (eq provider :codex) (getf record :thread-id) (getf record :session-id)))
           (conversation (make-conversation :id (getf record :id) :provider provider
                                            :session-id (getf record :session-id) :thread-id (getf record :thread-id)
                                            :directory directory :title (getf record :title)
                                            :started (getf record :started) :updated (getf record :updated))))
      (dolist (entry (getf record :entries))
        (vector-push-extend (copy-tree entry) (cv-entries conversation)))
      (setf (gethash provider (chat-conversations *chat*)) conversation)
      (cond ((null resume-id)
             (setf (gethash provider (chat-conversations *chat*)) nil)
             (let ((*replaying-conversation* t))
               (chat-note "This conversation can't be continued (it never reached the assistant); a new message starts a new one.")))
            ((eq provider :codex)
             (setf (chat-codex-thread-id *chat*) resume-id
                   (chat-codex-directory *chat*) directory))
            (t
             (setf (chat-session-id *chat*) resume-id
                   (chat-started *chat*) t
                   (chat-directory *chat*) directory)))
      (when resume-id
        (let ((*replaying-conversation* t))
          (chat-note (format nil "Restored the conversation of ~a~@[ in ~a~]. A new message continues it. What was allowed for it isn't allowed again until you say so, and nothing in it runs again."
                             (universal-time-label (getf record :updated))
                             (and directory (uiop:native-namestring directory))))))
      (chat-update-status)
      (chat-scroll-to-end))))

(define-command assistant-conversations ()
  "Browse the assistant conversations Cadre kept, Claude's and Codex's, and
restore one to continue it."
  (let ((records (saved-conversations)))
    (unless records (editor-error "No assistant conversations have been kept yet"))
    (show-claude-page)
    (open-picker (window-picker *window*) :items records
                 :label #'conversation-label
                 :placeholder "Continue which conversation?"
                 :on-choose (lambda (record)
                              (handler-case (restore-conversation record)
                                (editor-error (e) (message "~a" (editor-error-message e))))))))
