;;;; Assistant lifecycle regressions with explicitly scheduled workers and callbacks.
(in-package #:cadre-ui-tests)

(define-test assistants)

(define-test gtk-launch-form-evaluates-entry :parent assistants
  (let ((source (ui::gtk-app-launch-form #P"/tmp/" "ai-bible" "ai-bible:main")))
    ;; The hook stays quoted until gtk4 has loaded, then must be evaluated so
    ;; that the entry point actually runs.
    (true (search "(eval (read-from-string" source))
    (true (search "APPLICATION-RUN" source))
    (true (search ":window-added" source))
    (true (search "(gtk:window-present window)" source))
    (true (search "(apply original-run application arguments)" source))
    (false (search "(apply original application arguments)" source))
    (true (ignore-errors (read-from-string source)))))

(defmacro with-assistant-functions (bindings &body body)
  (let ((originals (gensym "ORIGINALS")))
    `(sb-ext:with-unlocked-packages (:sb-thread)
       (let ((,originals (list ,@(loop for (name nil) in bindings
                                      collect `(cons ',name (symbol-function ',name))))))
         (unwind-protect
              (progn
                ,@(loop for (name replacement) in bindings
                        collect `(setf (symbol-function ',name) ,replacement))
                ,@body)
           (dolist (entry ,originals)
             (setf (symbol-function (car entry)) (cdr entry))))))))

(define-test inactive-conversation-drops-approvals :parent assistants
  (let* ((ui::*chat* (ui::make-chat :provider :codex :started t :session-id "old-session"
                                  :directory #P"/tmp/assistant-old/"
                                  :message-boxes (make-hash-table)))
         (ui::*session-allowed* (make-hash-table :test 'equal))
         (denied 0))
    (setf (gethash '(:claude . "eval") ui::*session-allowed*) t
          (gethash '(:codex . "eval") ui::*session-allowed*) t
          (ui::chat-pending-approvals ui::*chat*) (list (lambda () (incf denied))))
    (with-assistant-functions ((ui::chat-note (lambda (&rest args) (declare (ignore args)))))
      (ui::claude-follow-folder #P"/tmp/assistant-new/"))
    ;; Only Claude's conversation moved, so only what Claude was allowed goes.
    (is equal '((:codex . "eval")) (loop for key being the hash-keys of ui::*session-allowed* collect key))
    (is = 1 denied)
    (false (ui::chat-pending-approvals ui::*chat*))
    (false (ui::chat-started ui::*chat*))
    (false (equal "old-session" (ui::chat-session-id ui::*chat*)))
    (is eq :codex (ui::chat-provider ui::*chat*))))

(defmacro with-codex-stop-fixture (&body body)
  `(let* ((server (c::make-codex-app-server))
          (ui::*chat* (ui::make-chat :provider :codex :busy t :codex-process server
                                    :codex-turn-generation 1))
          (timer nil)
          (interrupt-worker nil)
          (interrupts nil)
          (stops 0))
     (with-assistant-functions
         ((sb-thread:make-thread (lambda (function &rest args)
                                   (declare (ignore args)) (setf interrupt-worker function)))
          (glib:timeout-add-seconds (lambda (priority seconds callback)
                                      (declare (ignore priority seconds)) (setf timer callback)))
          (c::codex-request (lambda (server method &optional params)
                              (declare (ignore server))
                              (push (list method (c:jget params "threadId")
                                          (c:jget params "turnId")) interrupts)))
          (c:stop-codex-app-server (lambda (server) (setf (c:codex-stopped server) t) (incf stops)))
          (ui::chat-note (lambda (&rest args) (declare (ignore args))))
          (ui::chat-update-status (lambda ())))
       ,@body)))

(define-test stop-before-turn-started :parent assistants
  (with-codex-stop-fixture
    (ui::codex-chat-stop)
    ;; The same send receives its turn ID after the user pressed Stop.
    (setf (c:codex-turn-id server) "late-start")
    (funcall interrupt-worker)
    (false interrupts)
    (funcall timer)
    (is = 1 stops)
    (false (ui::chat-busy ui::*chat*))))

(define-test stop-does-not-kill-a-new-send :parent assistants
  (with-codex-stop-fixture
    (setf (c:codex-turn-id server) "old-turn")
    (ui::codex-chat-stop)
    ;; A new send can be pending before its turn/started notification arrives.
    (incf (ui::chat-codex-turn-generation ui::*chat*))
    (funcall timer)
    (is = 0 stops)
    (true (ui::chat-busy ui::*chat*))))

(define-test delayed-interrupt-targets-original-turn :parent assistants
  (with-codex-stop-fixture
    (setf (c:codex-thread-id server) "original-thread"
          (c:codex-turn-id server) "original-turn")
    (ui::codex-chat-stop)
    (incf (ui::chat-codex-turn-generation ui::*chat*))
    (setf (c:codex-thread-id server) "new-thread"
          (c:codex-turn-id server) "new-turn")
    (funcall interrupt-worker)
    (is equal '(("turn/interrupt" "original-thread" "original-turn")) interrupts)
    (funcall timer)
    (is = 0 stops)
    (true (ui::chat-busy ui::*chat*))))

(define-test interrupted-codex-turn-is-a-stop :parent assistants
  (let* ((server (c::make-codex-app-server))
         (ui::*chat* (ui::make-chat :provider :codex :busy t :codex-process server))
         (notes '()))
    (with-assistant-functions
        ((ui::chat-note (lambda (text &rest classes) (push (cons text classes) notes)))
         (ui::chat-update-status (lambda ()))
         (ui::refresh-codex-rate-limits (lambda (&optional server) (declare (ignore server))))
         (ui::claude-reply-ended (lambda ())))
      (ui::handle-codex-chat-event
       server (c::make-codex-event :kind :result :error t :data (c:jobj "id" "t1" "status" "interrupted")))
      (is equal '(("Stopped.")) notes)
      (false (ui::chat-busy ui::*chat*))
      (setf notes '() (ui::chat-busy ui::*chat*) t)
      (ui::handle-codex-chat-event
       server (c::make-codex-event :kind :result :error t :text "Boom" :data (c:jobj "id" "t2" "status" "failed")))
      (is equal '(("Boom" "error")) notes))))

(define-test codex-worker-leaves-chat-state-to-the-gtk-thread :parent assistants
  ;; The worker resumes the conversation it was given, read on the GTK
  ;; thread, and doesn't write the chat's thread ID itself.
  (let* ((server (c::make-codex-app-server))
         (ui::*chat* (ui::make-chat :provider :codex :busy t :codex-process server
                                   :codex-thread-id "cleared-meanwhile"))
         (resumed nil) (started nil))
    (with-assistant-functions
        ((c:codex-resume-thread (lambda (server thread-id &rest args)
                                  (declare (ignore args))
                                  (setf resumed thread-id (c:codex-thread-id server) thread-id)))
         (c:codex-start-thread (lambda (&rest args) (declare (ignore args)) (setf started t)))
         (c:codex-start-turn (lambda (&rest args) (declare (ignore args)) t)))
      (ui::codex-send-worker server "prompt" nil nil :chat :saved-thread-id "saved-thread"))
    (is equal "saved-thread" resumed)
    (false started)
    (is equal "cleared-meanwhile" (ui::chat-codex-thread-id ui::*chat*))))

(define-test failed-send-applies-pending-folder :parent assistants
  (dolist (failure '(:thread :turn))
    (let* ((server (c::make-codex-app-server :thread-id (when (eq failure :turn) "thread")))
           (ui::*chat* (ui::make-chat :provider :codex :busy t :codex-process server
                                     :pending-directory #P"/tmp/assistant-new/"))
           (followed nil))
      (with-assistant-functions
          ((c:codex-start-thread (lambda (&rest args) (declare (ignore args))
                                   (error "Initialization failed")))
           (c:codex-start-turn (lambda (&rest args) (declare (ignore args))
                                 (error "Turn start timed out")))
           (ui::deliver-to-gui #'funcall)
           (ui::chat-note (lambda (&rest args) (declare (ignore args))))
           (ui::chat-update-status (lambda ()))
           (ui::claude-follow-folder (lambda (directory)
                                       (setf followed (list directory (ui::chat-busy ui::*chat*))))))
        (ui::codex-send-worker server "prompt" nil nil :chat))
      (false (ui::chat-busy ui::*chat*))
      (false (ui::chat-pending-directory ui::*chat*))
      (is equal '(#P"/tmp/assistant-new/" nil) followed))))

(define-test stopped-tools-cannot-show-or-use-approvals :parent assistants
  (dolist (timing '(:before-card :after-card :session))
    (with-codex-stop-fixture
      (let ((ui::*session-allowed* (make-hash-table :test 'equal))
            (shown 0) (executed nil) (stopped-once nil) (sync-calls 0) (replies nil))
        (when (eq timing :session) (setf (gethash '(:codex . "test") ui::*session-allowed*) t))
        (with-assistant-functions
            ((glib:call-in-main-thread
              (lambda (function &key wait)
                (when wait (incf sync-calls))
                (flet ((stop () (setf stopped-once t) (ui::codex-chat-stop)))
                  (when (and (not stopped-once)
                             (or (and (not wait) (eq timing :before-card))
                                 (and wait (= sync-calls 2) (eq timing :session))))
                    (stop))
                  (prog1 (funcall function)
                    (when (and (not stopped-once) (not wait) (eq timing :after-card))
                      (stop))))))
             (ui::chat-show-approval (lambda (title detail done)
                                       (declare (ignore title detail))
                                       (incf shown) (funcall done :once)))
             (c:call-mcp-tool (lambda (name arguments)
                               (declare (ignore name arguments))
                               (ui::require-approval "test" "Approve" "test")
                               (ui::on-main (setf executed t))
                               (values "done" nil)))
             (c::codex-send-json (lambda (server object)
                                   (declare (ignore server)) (push object replies))))
          (flet ((turn-started (id)
                   (c::codex-handle-notification
                    server (c:jobj "method" "turn/started" "params" (c:jobj "turn" (c:jobj "id" id))))))
          (turn-started "old")
          (let ((request (c:jobj "id" 1 "params" (c:jobj "turnId" "old" "tool" "test"))))
            (c::codex-dynamic-tool-call server request)
            (true stopped-once)
            (false executed)
            (is = (if (eq timing :after-card) 1 0) shown)
            (true (c:jget (first replies) "error"))
            ;; A new send cannot revive requests belonging to the old turn.
            (c:codex-prepare-tools server)
            (c::codex-dynamic-tool-call server request)
            (false executed)
            (true (c:jget (first replies) "error"))
            (turn-started "new")
            (c::codex-dynamic-tool-call
             server (c:jobj "id" 2 "params" (c:jobj "turnId" "new" "tool" "test")))
            (true executed)
            (false (c:jget (first replies) "error")))))))))

(define-test session-approvals-are-per-provider :parent assistants
  (let ((ui::*session-allowed* (make-hash-table :test 'equal))
        (shown 0))
    (with-assistant-functions
        ((glib:call-in-main-thread (lambda (function &key wait) (declare (ignore wait)) (funcall function)))
         (ui::chat-show-approval (lambda (title detail done)
                                   (declare (ignore title detail))
                                   (incf shown) (funcall done :session))))
      ;; Claude is allowed to evaluate for the session...
      (is eq :session (ui::ask-approval "eval" "Evaluate" "(+ 1 2)"))
      (is = 1 shown)
      (is eq :session (ui::ask-approval "eval" "Evaluate" "(+ 1 2)"))
      (is = 1 shown)
      ;; ...which doesn't let Codex: it asks.
      (let ((c:*tool-provider* :codex))
        (is eq :session (ui::ask-approval "eval" "Evaluate" "(+ 1 2)"))
        (is = 2 shown)
        (is eq :session (ui::ask-approval "eval" "Evaluate" "(+ 1 2)"))
        (is = 2 shown))
      ;; A new Codex conversation forgets only what Codex was allowed.
      (ui::forget-session-approvals :codex)
      (is equal '((:claude . "eval")) (loop for key being the hash-keys of ui::*session-allowed* collect key)))))

(define-test stop-during-startup-cancels-turn-request :parent assistants
  (with-codex-stop-fixture
    (let ((token (c:codex-prepare-tools server)))
      (ui::codex-chat-stop)
      (setf (c:codex-thread-id server) "late-thread")
      (fail (c:codex-start-turn server "prompt" :tool-token token) 'error)
      (false interrupts))))

(defmacro with-codex-check-fixture (&body body)
  `(let* ((ui::*chat* (ui::make-chat :provider :claude))
          (ui::*window* nil)
          (ui::*codex-status* nil)
          (ui::*codex-checking* nil)
          (ui::*codex-recheck-requested* nil)
          (ui::*codex-when-checked* nil)
          (ui::*codex-model-catalog* nil)
          (workers nil) (deliveries nil) (fetches 0) (continued 0)
          (models (list (c:jobj "id" "test-model" "isDefault" t))))
     (with-assistant-functions
         ((sb-thread:make-thread (lambda (function &rest args)
                                   (declare (ignore args)) (push function workers)))
          (glib:call-in-main-thread (lambda (function &rest args)
                                      (declare (ignore args)) (push function deliveries)))
          (c:check-codex (lambda () (c::make-codex-status :program "fake" :logged-in t)))
          (c:codex-list-models (lambda (&rest args)
                                (declare (ignore args)) (incf fetches) models))
          (ui::show-codex-status (lambda ()))
          (ui::refresh-assistant-model-dropdown (lambda (&rest args) (declare (ignore args)))))
       ,@body)))

(define-test provider-switch-during-startup-check :parent assistants
  (with-codex-check-fixture
    (ui::check-codex-status)
    (setf (ui::chat-provider ui::*chat*) :codex)
    (ui::check-codex-status :force t :then (lambda () (incf continued)))
    ;; Repeated requests coalesce into one follow-up check.
    (ui::check-codex-status :force t)
    (funcall (pop workers))
    (funcall (pop deliveries))
    (is = 0 continued)
    (is = 1 (length workers))
    (funcall (pop workers))
    (funcall (pop deliveries))
    (is = 1 fetches)
    (is = 1 continued)
    (is equal models ui::*codex-model-catalog*)
    (false ui::*codex-checking*)
    (false workers)))

(define-test forced-check-continuation-runs-once :parent assistants
  (with-codex-check-fixture
    (setf ui::*codex-status* (c::make-codex-status :program "cached" :logged-in t)
          (ui::chat-provider ui::*chat*) :codex)
    (ui::check-codex-status :force t :then (lambda () (incf continued)))
    (is = 0 continued)
    (funcall (pop workers))
    (funcall (pop deliveries))
    (is = 1 continued)))

(define-test missing-models-do-not-loop :parent assistants
  (with-codex-check-fixture
    (ui::check-codex-status)
    (setf (ui::chat-provider ui::*chat*) :codex models nil)
    ;; The completion itself notices the provider switch without another call.
    (funcall (pop workers))
    (funcall (pop deliveries))
    (funcall (pop workers))
    (funcall (pop deliveries))
    (is = 1 fetches)
    (false workers)
    (false ui::*codex-checking*)))

(define-test assistant-thinking-levels :parent assistants
  (let* ((model (c:jobj "id" "test" "defaultReasoningEffort" "medium"
                        "supportedReasoningEfforts"
                        (vector (c:jobj "reasoningEffort" "low")
                                (c:jobj "reasoningEffort" "medium")))))
    (is equal '(nil "low" "medium") (ui::assistant-effort-choices :codex model))
    (is equal '(nil) (ui::assistant-effort-choices :codex nil))
    (true (member "high" (ui::assistant-effort-choices :claude nil) :test #'equal))))

(define-test codex-thinking-selection-and-fallback :parent assistants
  (let* ((ui::*chat* (ui::make-chat :provider :codex))
         (ui::*codex-model-catalog*
           (list (c:jobj "id" "test" "defaultReasoningEffort" "medium"
                         "supportedReasoningEfforts"
                         (vector (c:jobj "reasoningEffort" "low")
                                 (c:jobj "reasoningEffort" "medium"))))))
    (with-assistant-functions ((gtk:drop-down-get-selected (lambda (dropdown) (declare (ignore dropdown)) 0)))
      (dolist (selection '(nil "low" "unsupported"))
        (let ((ui::*codex-effort* selection))
          (multiple-value-bind (model effort) (ui::selected-codex-model)
            (is equal "test" model)
            (is equal (if (equal selection "low") "low" "medium") effort)))))))

(define-test claude-remaining-usage :parent assistants
  (let ((ui::*claude-rate-limits* nil))
    (false (ui::claude-usage-summary))
    (setf ui::*claude-rate-limits*
          (cadre:parse-json "{\"unifiedWindows\":{\"five_hour\":{\"utilization\":0.28},\"seven_day\":{\"utilization\":0.6},\"unknown\":{}}}"))
    (is string= "72% 5h / 40% 7d" (ui::claude-usage-summary))
    (setf ui::*claude-rate-limits*
          (cadre:parse-json "{\"unifiedWindows\":{\"five_hour\":{\"utilization\":0.999},\"seven_day\":{\"utilization\":1.2}}}"))
    (is string= "<1% 5h / 0% 7d" (ui::claude-usage-summary))))

(define-test stale-codex-startup-error-preserves-new-turn :parent assistants
  (let* ((server (c::make-codex-app-server :thread-id "thread"))
         (ui::*chat* (ui::make-chat :provider :codex :busy t :codex-process server))
         (callback nil) (notes nil) (ended nil))
    (with-assistant-functions
        ((c:codex-start-turn (lambda (&rest args) (declare (ignore args)) (error "Late failure")))
         (ui::deliver-to-gui (lambda (function) (setf callback function)))
         (ui::chat-note (lambda (&rest args) (push args notes)))
         (ui::claude-reply-ended (lambda () (setf ended t)))
         (ui::chat-update-status (lambda ())))
      (ui::codex-send-worker server "old" nil nil :chat)
      (true callback)
      (c:codex-prepare-tools server)
      (funcall callback)
      (true (ui::chat-busy ui::*chat*))
      (false notes)
      (false ended))))

(define-test chat-scroll-follows-layout-unless-reading-history :parent assistants
  (let* ((ui::*chat* (ui::make-chat :scroller :scroller))
         (callbacks (make-hash-table))
         (idle nil) (upper 100d0) (value 50d0) (scheduled 0))
    (with-assistant-functions
        ((gobject:connect (lambda (object signal callback &rest args)
                            (declare (ignore object args))
                            (setf (gethash signal callbacks) callback)))
         (glib:idle-add (lambda (priority function)
                          (declare (ignore priority))
                          (incf scheduled) (setf idle function)))
         (gtk:scrolled-window-get-vadjustment (lambda (scroller) (declare (ignore scroller)) :adjustment))
         (gtk:adjustment-get-value (lambda (adj) (declare (ignore adj)) value))
         (gtk:adjustment-get-upper (lambda (adj) (declare (ignore adj)) upper))
         (gtk:adjustment-get-page-size (lambda (adj) (declare (ignore adj)) 50d0))
         (gtk:adjustment-set-value (lambda (adj new) (declare (ignore adj)) (setf value new))))
      (ui::connect-chat-scrolling ui::*chat*)
      (flet ((signal-range () (funcall (gethash :changed callbacks) :adjustment))
             (signal-value () (funcall (gethash :value-changed callbacks) :adjustment)))
        (signal-value)
        ;; Layout grows after the initial append idle has already run.
        (setf upper 200d0)
        (signal-range)
        (signal-range)
        (is = 1 scheduled)
        (funcall idle)
        (is = 150d0 value)
        ;; Reading up disables both range following and streaming requests.
        (setf value 20d0)
        (signal-value)
        (setf upper 300d0)
        (signal-range)
        (ui::chat-scroll-to-end)
        (is = 1 scheduled)
        (is = 20d0 value)
        ;; Returning to the bottom enables following again.
        (setf value 250d0)
        (signal-value)
        (setf upper 400d0)
        (signal-range)
        (is = 2 scheduled)
        ;; A manual scroll before the idle runs cancels that pending move.
        (setf value 10d0)
        (signal-value)
        (funcall idle)
        (is = 10d0 value)
        ;; Earlier-page rendering shares the follow state with the real chat.
        (setf value 350d0)
        (signal-value)
        (let ((page-chat (ui::copy-chat ui::*chat*)))
          (setf (car (ui::chat-scroll-follow page-chat)) nil))
        (setf upper 500d0)
        (signal-range)
        (is = 2 scheduled)
        (is = 350d0 value)))))

(define-test sending-from-history-resumes-following :parent assistants
  (dolist (provider '(:claude :codex))
    (let* ((ui::*chat* (ui::make-chat :provider provider :scroller :scroller
                                    :scroll-follow (list nil)))
           (server (c::make-codex-app-server))
           (idle nil) (value 20d0) (shown nil))
      (with-assistant-functions
          ((ui::message-context (lambda () ""))
           (ui::chat-user-message
            (lambda (text)
              ;; Follow must be enabled before the message is appended.
              (true (car (ui::chat-scroll-follow ui::*chat*)))
              (setf shown text)
              (ui::chat-scroll-to-end)))
           (ui::chat-update-status (lambda ()))
           (ui::ensure-claude-process (lambda () :process))
           (c:claude-send (lambda (&rest args) (declare (ignore args))))
           (ui::selected-codex-model (lambda () (values nil nil)))
           (ui::codex-process-for-chat (lambda () server))
           (sb-thread:make-thread (lambda (&rest args) (declare (ignore args))))
           (glib:idle-add (lambda (priority callback)
                             (declare (ignore priority)) (setf idle callback)))
           (gtk:scrolled-window-get-vadjustment (lambda (scroller) (declare (ignore scroller)) :adjustment))
           (gtk:adjustment-get-upper (lambda (adj) (declare (ignore adj)) 500d0))
           (gtk:adjustment-get-page-size (lambda (adj) (declare (ignore adj)) 50d0))
           (gtk:adjustment-set-value (lambda (adj new) (declare (ignore adj)) (setf value new))))
        ;; The shared entry point dispatches to either provider's send path.
        (ui::send-to-claude "prompt" :shown "message")
        (is string= "message" shown)
        (true idle)
        (when idle (funcall idle))
        (is = 450d0 value)))))
