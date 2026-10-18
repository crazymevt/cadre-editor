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
    (setf (gethash "eval" ui::*session-allowed*) t
          (ui::chat-pending-approvals ui::*chat*) (list (lambda () (incf denied))))
    (with-assistant-functions ((ui::chat-note (lambda (&rest args) (declare (ignore args)))))
      (ui::claude-follow-folder #P"/tmp/assistant-new/"))
    (is = 0 (hash-table-count ui::*session-allowed*))
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
        (when (eq timing :session) (setf (gethash "test" ui::*session-allowed*) t))
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
            (c::codex-dynamic-tool-call
             server (c:jobj "id" 2 "params" (c:jobj "turnId" "new" "tool" "test")))
            (true executed)
            (false (c:jget (first replies) "error"))))))))

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
