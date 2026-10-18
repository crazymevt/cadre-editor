;;;; codex.lisp — Codex in the assistant panel, through app-server

(in-package #:cadre-ui)

(defvar *codex-status* nil)
(defvar *codex-checking* nil)
(defvar *codex-recheck-requested* nil)
(defvar *codex-models-requested* nil)
(defvar *codex-when-checked* '())
(defvar *codex-model-catalog* '())
(defvar *codex-rate-limits* nil)
(defvar *codex-rate-limits-error* nil)

(defun read-codex-rate-limits-safely (thunk)
  "Return rate-limit data and an explanation when the app-server read fails."
  (handler-case
      (let ((limits (funcall thunk)))
        (if limits
            (values limits nil)
            (values nil "Codex returned no rate-limit windows")))
    (error (condition)
      (values nil (princ-to-string condition)))))

(defun codex-rate-window-label (minutes)
  (cond ((and (integerp minutes) (zerop (mod minutes 1440)))
         (format nil "~dd" (/ minutes 1440)))
        ((and (integerp minutes) (zerop (mod minutes 60)))
         (format nil "~dh" (/ minutes 60)))
        ((integerp minutes) (format nil "~dm" minutes))
        (t nil)))

(defun codex-usage-summary ()
  "A compact summary of remaining usage across Codex quota windows."
  (let ((windows
          (loop for (bucket-id . bucket) in *codex-rate-limits*
                for multi-bucket-p = (> (length *codex-rate-limits*) 1)
                append (loop for window-name in '("primary" "secondary")
                             for window = (jget bucket window-name)
                             for used = (jget window "usedPercent")
                             when (numberp used)
                               collect (format nil "~a~d%~@[ ~a~]"
                                               (if multi-bucket-p (format nil "~a " bucket-id) "")
                                               (round (max 0 (min 100 (- 100 used))))
                                               (codex-rate-window-label
                                                (jget window "windowDurationMins")))))))
    (and windows (format nil "~{~a~^ / ~}" windows))))

(defun refresh-codex-rate-limits (&optional server)
  "Refresh usage in the background, using SERVER when its app-server is live."
  (when (and (codex-ready-p) *chat* (eq (chat-provider *chat*) :codex))
    (let ((program (codex-status-program *codex-status*))
          (directory (or (and *window* (window-project *window*)) (user-homedir-pathname))))
      (sb-thread:make-thread
       (lambda ()
         (multiple-value-bind (limits problem)
             (read-codex-rate-limits-safely
              (lambda ()
                (if (and server (codex-alive-p server))
                    (codex-rate-limits-from-response
                     (cadre::codex-request server "account/rateLimits/read" (jobj)))
                    (codex-read-rate-limits program directory))))
           (glib:call-in-main-thread
            (lambda ()
              (when (and *chat* (eq (chat-provider *chat*) :codex))
                (setf *codex-rate-limits* limits
                      *codex-rate-limits-error* problem)
                (chat-update-status)))))
       :name "refresh Codex usage")))))

(defun codex-ready-p-for (status)
  (and status (codex-status-program status) (codex-status-logged-in status)))

(defparameter *codex-system-prompt*
  "You are working inside Cadre, a Common Lisp editor. Use the provided Cadre tools to inspect the user's open buffers and running Lisp image. Read files with read_buffer so you see unsaved changes. Make all file changes with propose_edit so the user can review them inline. Do not edit files or run shell commands directly. Cadre asks the user before evaluations, compilation, loading, and tests.")

(defparameter *codex-agent-prompt*
  "You are in agent mode. Carry the user's task through to completion. Read before changing, keep edits small, use propose_edit for all changes, and use Cadre's tools to compile or test after the user approves those actions. Finish with a short summary.")

(defun codex-ready-p ()
  (and *codex-status* (codex-status-program *codex-status*) (codex-status-logged-in *codex-status*)))

(defun check-codex-status (&key force then fetch-models)
  "Check Codex CLI availability and saved-login status off the GTK thread."
  (when fetch-models (setf *codex-models-requested* t))
  (when *codex-checking*
    (when then (setf *codex-when-checked* (append *codex-when-checked* (list then))))
    (when (or force (and *chat* (eq (chat-provider *chat*) :codex)
                         (null *codex-model-catalog*)))
      (setf *codex-recheck-requested* t))
    (return-from check-codex-status))
  (when (or force (null *codex-status*))
    (setf *codex-checking* t
          *codex-recheck-requested* nil)
    (when then (setf *codex-when-checked* (append *codex-when-checked* (list then))))
    (when (and *chat* (eq (chat-provider *chat*) :codex)) (show-codex-status))
    (let ((directory (or (and *window* (window-project *window*)) (user-homedir-pathname)))
          (deliver-models (or *codex-models-requested*
                              (and *chat* (eq (chat-provider *chat*) :codex)))))
     (sb-thread:make-thread
     (lambda ()
       (let* ((status (check-codex))
              (models (when (and deliver-models (codex-ready-p-for status))
                        (ignore-errors (codex-list-models (codex-status-program status) directory))))
              (usage-result
                (when (and deliver-models (codex-ready-p-for status))
                  (multiple-value-list
                   (read-codex-rate-limits-safely
                    (lambda () (codex-read-rate-limits (codex-status-program status) directory)))))))
         (glib:call-in-main-thread
          (lambda ()
            (setf *codex-status* status *codex-checking* nil)
            (when deliver-models
              (setf *codex-rate-limits* (first usage-result)
                    *codex-rate-limits-error* (second usage-result)))
            (when deliver-models (setf *codex-models-requested* nil))
            (when (and deliver-models models)
              (let ((had-models *codex-model-catalog*))
                (setf *codex-model-catalog* models)
                (refresh-assistant-model-dropdown :preserve (not (null had-models)))))
            (when deliver-models (refresh-codex-model-setting))
            (when (and *chat* (eq (chat-provider *chat*) :codex)) (show-codex-status))
            ;; A provider switch may have happened after this worker decided
            ;; whether to load models. Retry once with the current selection;
            ;; a failed model fetch itself must not cause an endless retry.
            (let ((recheck (or *codex-recheck-requested*
                               (and (not deliver-models) (codex-ready-p)
                                    *chat* (eq (chat-provider *chat*) :codex)
                                    (null *codex-model-catalog*)))))
              (setf *codex-recheck-requested* nil)
              (if recheck
                  (check-codex-status :force t)
                  (let ((pending *codex-when-checked*))
                    (setf *codex-when-checked* '())
                    (when (codex-ready-p) (mapc #'funcall pending)))))))))
     :name "check Codex CLI"))
    ;; A queued continuation runs only after the asynchronous check finishes.
    (return-from check-codex-status))
  (when (and *chat* (eq (chat-provider *chat*) :codex)) (show-codex-status))
  (when (and then (codex-ready-p)) (funcall then)))

(defun assistant-effort-choices (provider model)
  "Default followed by the provider's available thinking levels."
  (cons nil
        (if (eq provider :claude)
            '("low" "medium" "high" "xhigh" "max")
            (remove-duplicates
             (loop for entry in (cadre::jlist (jget model "supportedReasoningEfforts"))
                   for effort = (jget entry "reasoningEffort")
                   when (stringp effort) collect effort)
             :test #'equal))))

(defun refresh-assistant-effort-dropdown ()
  (when (and *chat* (chat-effort-dropdown *chat*))
    (let* ((provider (chat-provider *chat*))
           (model (when (eq provider :codex)
                    (nth (gtk:drop-down-get-selected (chat-model-dropdown *chat*)) *codex-model-catalog*)))
           (choices (assistant-effort-choices provider model))
           (chosen (if (eq provider :claude) *claude-effort* *codex-effort*))
           (dropdown (chat-effort-dropdown *chat*)))
      (setf (chat-refreshing-effort *chat*) t)
      (unwind-protect
           (progn
             (setf (chat-effort-choices *chat*) choices)
             (gtk:drop-down-set-model dropdown
                                      (gtk:string-list-new (mapcar (lambda (x) (format nil "Thinking: ~a" (if x (string-capitalize x) "Default"))) choices)))
             (gtk:drop-down-set-selected dropdown (or (position chosen choices :test #'equal) 0))
             (gtk:widget-set-sensitive dropdown (> (length choices) 1)))
        (setf (chat-refreshing-effort *chat*) nil)))))

(defun refresh-assistant-model-dropdown (&key (preserve t))
  (when (and *chat* (typep (chat-model-dropdown *chat*) 'gtk:drop-down))
    (let* ((dropdown (chat-model-dropdown *chat*))
           (selected (gtk:drop-down-get-selected dropdown))
           (old-id (and preserve (eq (chat-provider *chat*) :codex)
                        (nth selected *codex-model-catalog*)
                        (or (jget (nth selected *codex-model-catalog*) "id")
                            (jget (nth selected *codex-model-catalog*) "model")))))
      (if (eq (chat-provider *chat*) :codex)
          (gtk:drop-down-set-model dropdown
                                   (gtk:string-list-new
                                    (if *codex-model-catalog*
                                        (mapcar (lambda (model) (or (jget model "displayName")
                                                                    (jget model "model")
                                                                    (jget model "id")))
                                                *codex-model-catalog*)
                                        '("Codex default"))))
          (gtk:drop-down-set-model dropdown (gtk:string-list-new *claude-models*)))
      (gtk:drop-down-set-selected
       dropdown
       (if (eq (chat-provider *chat*) :claude)
           (or (position *claude-model* *claude-models* :test #'equal) 0)
           (or (and old-id
                    (position old-id *codex-model-catalog*
                              :key (lambda (model) (or (jget model "id") (jget model "model")))
                              :test #'equal))
               (and *codex-model*
                    (position *codex-model* *codex-model-catalog*
                              :key (lambda (model) (or (jget model "id") (jget model "model")))
                              :test #'equal))
               (position-if (lambda (model) (eq (jget model "isDefault") t)) *codex-model-catalog*)
               0)))
      (refresh-assistant-effort-dropdown))))

(defun selected-codex-model ()
  (let* ((model (nth (gtk:drop-down-get-selected (chat-model-dropdown *chat*)) *codex-model-catalog*))
         (id (and model (or (jget model "id") (jget model "model"))))
         (effort (or (and (member *codex-effort* (assistant-effort-choices :codex model) :test #'equal)
                          *codex-effort*)
                     (and model (jget model "defaultReasoningEffort")))))
    (values id effort)))

(defun check-selected-assistant-status (&key force then)
  (if (and *chat* (eq (chat-provider *chat*) :codex))
      (check-codex-status :force force :then then)
      (check-claude-status :force force :then then)))

(defun codex-status-buttons (visible)
  (when *claude-status-buttons*
    (gtk:widget-set-visible *claude-status-buttons* visible)))

(defun show-codex-status ()
  (when *chat*
    ;; These controls belong to Claude's browser-based sign-in flow.
    (when *sign-in-link* (gtk:widget-set-visible *sign-in-link* nil))
    (when *sign-in-box* (gtk:widget-set-visible *sign-in-box* nil))
    (when *assistant-sign-in-button*
      (gtk:button-set-label *assistant-sign-in-button* "Sign In…")
      (gtk:widget-set-tooltip-text *assistant-sign-in-button* "Sign in to Codex CLI in the Terminal"))
    (let ((status *codex-status*))
      (codex-status-buttons (and status (not *codex-checking*)))
      (cond
        ((or (null status) *codex-checking*)
         (gtk:label-set-text *claude-status-label* "Checking Codex CLI…")
         (gtk:label-set-text *claude-status-detail* "")
         (unless (codex-ready-p) (gtk:stack-set-visible-child-name (chat-stack *chat*) "status")))
        ((null (codex-status-program status))
         (gtk:label-set-text *claude-status-label*
                             "Codex CLI isn't installed. Install Codex CLI, then choose Check Again.")
         (gtk:label-set-text *claude-status-detail* (or (codex-status-error status) ""))
         (gtk:stack-set-visible-child-name (chat-stack *chat*) "status"))
        ((not (codex-status-logged-in status))
         (gtk:label-set-text *claude-status-label*
                             (format nil "Codex CLI ~a is installed but not signed in.~%~%Sign in with `codex login`, then check again."
                                     (codex-status-version status)))
         (gtk:label-set-text *claude-status-detail* (or (codex-status-error status) ""))
         (gtk:stack-set-visible-child-name (chat-stack *chat*) "status"))
        (t (gtk:stack-set-visible-child-name (chat-stack *chat*) "chat")))
      (gtk:widget-set-visible (chat-composer *chat*) (codex-ready-p))
      (chat-update-status))))

(defun assistant-provider-selected (provider)
  "Switch providers and show the matching conversation transcript."
  (when *chat*
    (let ((old (chat-provider *chat*)))
      (cond
        ((eq old provider) nil)
        ((chat-busy *chat*)
         (gtk:drop-down-set-selected (chat-provider-dropdown *chat*) (if (eq old :codex) 1 0))
         (message "Wait for the current assistant turn to finish before switching providers"))
        (t
         (setf (gethash old (chat-message-boxes *chat*)) (chat-messages *chat*)
               (chat-provider *chat*) provider)
         (gtk:widget-set-visible (chat-model-dropdown *chat*) t)
         (let* ((messages (gethash provider (chat-message-boxes *chat*)))
                (new (null messages)))
           (unless messages
             (setf messages (make-instance 'gtk:box :orientation :vertical :spacing 10
                                                         :margin-start 12 :margin-end 12
                                                         :margin-top 8 :margin-bottom 8)
                   (gethash provider (chat-message-boxes *chat*)) messages))
           (setf (chat-messages *chat*) messages)
           (gtk:scrolled-window-set-child (chat-scroller *chat*) messages)
           ;; Replacing the scroller child creates a fresh GtkViewport. Keep
           ;; selectable replies from pulling the conversation to their row.
           (let ((viewport (gtk:scrolled-window-get-child (chat-scroller *chat*))))
             (when (typep viewport 'gtk:viewport)
               (gtk:viewport-set-scroll-to-focus viewport nil)))
           (when new
             (if (eq provider :codex) (show-codex-intro) (show-claude-intro))))
         (refresh-assistant-model-dropdown :preserve nil)
         (show-claude-status)
         (when (and (eq provider :codex) *codex-model-catalog*)
           (refresh-codex-rate-limits))
         (check-selected-assistant-status :force (and (eq provider :codex)
                                                     (null *codex-model-catalog*))))))))

(defun show-codex-intro ()
  (chat-append (make-instance 'gtk:label :wrap t :xalign 0.0 :css-classes '("dim-label")
                              :label (format nil "Ask Codex about your code. It can use Cadre's editor and Lisp tools, and propose edits for review.~%~%Return sends; Shift+Return starts a new line."))))

(defun handle-codex-chat-event (server event)
  (when (and *chat* (eq server (chat-codex-process *chat*))
             (or (null (codex-event-turn-id event))
                 (null (codex-turn-id server))
                 (equal (codex-event-turn-id event) (codex-turn-id server))))
    (case (codex-event-kind event)
      (:thread-started (setf (chat-codex-thread-id *chat*) (codex-event-thread-id event)))
      (:text-delta (chat-append-delta (codex-event-text event)))
      (:tool-start (chat-tool-row (codex-event-id event) (codex-event-name event) (codex-event-input event)))
      (:tool-result (chat-tool-result (codex-event-id event) (codex-event-text event) (codex-event-error event)))
      (:activity (chat-tool-row (codex-event-id event) (codex-event-name event) (codex-event-data event)))
      (:activity-completed (chat-tool-result (codex-event-id event)
                                             (json-string (codex-event-data event))
                                             (codex-event-error event)))
      (:permission-denied
       (chat-note "Codex requested a direct file or shell action. Cadre denied it; use the Cadre tools so edits remain reviewable."))
      (:protocol-error (chat-note (format nil "Codex protocol error: ~a" (codex-event-text event)) "error"))
      (:stderr (panel-log-raw (window-panel *window*) (format nil "codex: ~a" (codex-event-text event))))
      (:result
       ;; Codex streams plain text deltas but does not send Claude's separate
       ;; finalized :text event. Replace the streaming label with the shared
       ;; Markdown rendering when the turn completes.
       (when (chat-stream-box *chat*)
         (chat-finish-text (chat-stream-text *chat*)))
       (setf (chat-busy *chat*) nil (chat-stream-box *chat*) nil)
       (when (codex-event-error event)
         (chat-note (or (codex-event-text event) "Codex turn failed") "error"))
       (chat-update-status)
       (refresh-codex-rate-limits server)
       (claude-reply-ended)))))

(defun codex-process-exited (server code)
  (when (and *chat* (eq server (chat-codex-process *chat*)))
    (setf (chat-codex-process *chat*) nil)
    (when (chat-busy *chat*)
      (setf (chat-busy *chat*) nil)
      (unless (codex-stopped server)
        (chat-note (format nil "Codex app-server exited (status ~a). See Output for its messages." code) "error")))
    (claude-reply-ended)
    (chat-update-status)))

(defun codex-chat-stop ()
  "Interrupt Codex, then terminate its process if it does not finish promptly."
  (let* ((server (and *chat* (chat-codex-process *chat*)))
         (thread-id (and server (codex-thread-id server)))
         (turn-id (and server (codex-turn-id server)))
         (generation (and *chat* (chat-codex-turn-generation *chat*))))
    (when server
      (codex-cancel-tools server)
      (deny-pending-approvals)
      ;; Capture the target before scheduling: this worker may run after a
      ;; subsequent send has changed the server's active turn.
      (sb-thread:make-thread
       (lambda () (ignore-errors (codex-interrupt server :thread-id thread-id :turn-id turn-id)))
       :name "interrupt Codex")
      (glib:timeout-add-seconds
       glib:+priority-default+ 3
       (lambda ()
         (when (and *chat* (chat-busy *chat*) (eq (chat-provider *chat*) :codex)
                    (eq server (chat-codex-process *chat*))
                    (= generation (chat-codex-turn-generation *chat*)))
           (stop-codex-app-server server)
           (setf (chat-busy *chat*) nil)
           (chat-note "Stopped.")
           (claude-reply-ended)
           (chat-update-status))
         nil)))))

(defun codex-process-for-chat ()
  (or (and (codex-alive-p (chat-codex-process *chat*)) (chat-codex-process *chat*))
      (let ((program (and *codex-status* (codex-status-program *codex-status*)))
            (directory (or (chat-codex-directory *chat*) (window-project *window*) (user-homedir-pathname)))
            (server nil))
        (unless program (editor-error "Codex CLI is not ready"))
        (setf server (start-codex-app-server
                      program directory :deliver #'deliver-to-gui
                      :on-event (lambda (event) (handle-codex-chat-event server event))
                      :on-exit (lambda (code) (codex-process-exited server code))))
        (setf (chat-codex-directory *chat*) directory
              (chat-codex-process *chat*) server)
        server)))

(defun codex-send-worker (server text model effort mode &optional (tool-token (cadre::codex-tool-token server)))
  (handler-case
      (progn
        (unless (codex-thread-id server)
          (let* ((saved-thread-id (chat-codex-thread-id *chat*))
                 (resumed (when saved-thread-id
                            (handler-case
                                (codex-resume-thread server saved-thread-id
                                                     :instructions *codex-system-prompt*)
                              (error (e)
                                (deliver-to-gui
                                 (lambda ()
                                   (when (and *chat* (eq server (chat-codex-process *chat*)))
                                     (chat-note (format nil "Could not resume the saved Codex conversation (~a). Starting a new conversation." e)))))
                                nil))))
                 (thread-id (or resumed
                                (codex-start-thread server :model model :instructions *codex-system-prompt*))))
            (setf (chat-codex-thread-id *chat*) thread-id)))
        (codex-start-turn server
                          (format nil "<cadre-mode-instructions>~%~a~%</cadre-mode-instructions>~%~%~a"
                                  (if (eq mode :agent) *codex-agent-prompt*
                                      "Answer the user's question directly. Use Cadre tools when editor context or live Lisp state would help.")
                                  text)
                          :model model :effort effort :tool-token tool-token))
    (error (e)
      (deliver-to-gui
       (lambda ()
         (when (and *chat* (eq server (chat-codex-process *chat*)))
           (setf (chat-busy *chat*) nil)
           (unless (codex-stopped server)
             (chat-note (format nil "Could not start Codex: ~a" e) "error"))
           (claude-reply-ended)
           (chat-update-status)))))))

(defun send-to-codex (text &key shown)
  "Send TEXT, with editor context, through Codex app-server."
  (let ((context (message-context)))
    (chat-append (gtk:build
                   (gtk:box :halign :end :css-classes '("cadre-chat-user")
                     (gtk:label :label (or shown text) :wrap t :xalign 0.0 :selectable t :wrap-mode :word-char))))
    (incf (chat-codex-turn-generation *chat*))
    (setf (chat-busy *chat*) t)
    (chat-update-status)
    (handler-case
        (multiple-value-bind (model effort) (selected-codex-model)
          (let* ((server (codex-process-for-chat))
               (tool-token (codex-prepare-tools server))
               (prompt (if (plusp (length context))
                           (format nil "<editor-context>~%~a</editor-context>~%~%~a" context text)
                           text)))
            (sb-thread:make-thread (lambda () (codex-send-worker server prompt model effort (chat-mode *chat*) tool-token))
                                   :name "Codex turn")))
      (error (e)
        (setf (chat-busy *chat*) nil)
        (chat-note (format nil "Could not start Codex: ~a" e) "error")
        (chat-update-status)))))

(define-command codex ()
  "Show the assistant panel with Codex selected."
  (gtk:drop-down-set-selected (chat-provider-dropdown *chat*) 1)
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "claude")
  (check-selected-assistant-status))

(define-command codex-sign-in ()
  "Open a terminal to sign in to the local Codex CLI."
  (let ((program (or (and *codex-status* (codex-status-program *codex-status*))
                     (find-codex-program)
                     (editor-error "Codex CLI was not found"))))
    (show-terminal-page)
    (start-terminal :command (list program "login") :title "Codex Sign In")))
