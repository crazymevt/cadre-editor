;;;; claude.lisp — the Claude panel: a chat with Claude Code, which can use
;;;; the editor through Cadre's MCP tools
;;;;
;;;; The first message starts `claude -p` (stream-json) in the project
;;;; folder, with Cadre's MCP server, its permission prompt tool, Claude
;;;; Code's Edit and Write turned off (edits go through propose_edit), and a
;;;; note about the editor appended to the system prompt. The process stays
;;;; up between messages; if it exits, the next message resumes the session.

(in-package #:cadre-ui)

(defparameter *claude-system-prompt*
  "You are working inside Cadre, a Common Lisp editor, through its MCP tools (mcp__cadre__…).
The user's Lisp image is running and connected: ask it instead of guessing. Use describe_symbol,
arglist, find_definitions, who_calls, macroexpand and apropos to learn about code, get_problems
for compiler errors and warnings, get_backtrace when the debugger is active, and current_context
for what the user is looking at. Read files with read_buffer: it includes unsaved changes.
Change files only with propose_edit: the user reviews each edit as an inline diff. Keep each
edit small and focused, one call per change. After an edit is accepted and saved, you can
compile_file to check it. eval runs code in the user's image after they approve it.")

(defparameter *claude-allowed-tools*
  '("Read" "Grep" "Glob" "ToolSearch" "mcp__cadre__list_buffers" "mcp__cadre__read_buffer" "mcp__cadre__current_context"
    "mcp__cadre__get_problems" "mcp__cadre__get_backtrace" "mcp__cadre__describe_symbol"
    "mcp__cadre__arglist" "mcp__cadre__find_definitions" "mcp__cadre__who_calls"
    "mcp__cadre__who_references" "mcp__cadre__macroexpand" "mcp__cadre__apropos"
    "mcp__cadre__list_systems" "mcp__cadre__propose_edit" "mcp__cadre__eval" "mcp__cadre__compile_file")
  "Tools Claude may use without Claude Code asking first. propose_edit, eval and
compile_file ask in Cadre themselves.")

(defparameter *claude-disallowed-tools* '("Edit" "Write" "NotebookEdit" "MultiEdit"))

;;; State

(defvar *mcp-server* nil)
(defvar *mcp-config* nil)
(defvar *claude-status* nil "A claude-status, once checked.")
(defvar *claude-checking* nil)

(defstruct (chat (:conc-name chat-))
  process session-id (started nil) (busy nil) (cost 0)
  messages scroller input send-button status model-dropdown stack
  context-file context-problems context-debugger
  (stream-box nil)                      ; the box of the reply being streamed
  (stream-label nil)
  (stream-text "")
  (tools (make-hash-table :test 'equal)) ; tool call id → (expander . result label)
  (pending-approvals '()))              ; functions that deny the waiting approvals

(defvar *chat* nil)

(defun ensure-mcp-server ()
  (unless *mcp-server*
    (setf *mcp-server* (start-mcp-server)
          *mcp-config* (write-mcp-config *mcp-server*
                                         (merge-pathnames (format nil "run-~d/" (sb-posix:getpid))
                                                          (cadre::cache-directory))))))

(defun stop-claude-session ()
  "Stop Claude's process and the MCP server (when quitting)."
  (when *chat* (stop-claude (chat-process *chat*)))
  (stop-mcp-server *mcp-server*)
  (setf *mcp-server* nil))

;;; The widget

(defparameter *claude-models* '("sonnet" "opus" "haiku"))

(defun make-claude-widget ()
  (let* ((messages (make-instance 'gtk:box :orientation :vertical :spacing 10
                                           :margin-start 12 :margin-end 12 :margin-top 8 :margin-bottom 8))
         (scroller (make-instance 'gtk:scrolled-window :child messages :vexpand t :hscrollbar-policy :never))
         (input (make-instance 'gtk:text-view :wrap-mode :word-char :top-margin 6 :bottom-margin 6
                                              :left-margin 8 :right-margin 8 :accepts-tab nil))
         (send (make-instance 'gtk:button :label "Send" :valign :end :css-classes '("suggested-action")))
         (status (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end :css-classes '("dim-label")))
         (models (gtk:drop-down-new-from-strings (coerce *claude-models* 'list)))
         (file (make-instance 'gtk:toggle-button :label "File" :active t :css-classes '("flat")
                                                 :tooltip-text "Tell Claude which file you are in, where the cursor is, and the selection"))
         (problems (make-instance 'gtk:toggle-button :label "Problems" :css-classes '("flat")
                                                     :tooltip-text "Attach the compiler's notes"))
         (debugger (make-instance 'gtk:toggle-button :label "Debugger" :css-classes '("flat")
                                                     :tooltip-text "Attach the debugger's condition and backtrace"))
         (stack (make-instance 'gtk:stack :vexpand t))
         (chat (make-chat :messages messages :scroller scroller :input input :send-button send :status status
                          :model-dropdown models :stack stack :context-file file :context-problems problems
                          :context-debugger debugger :session-id (make-uuid))))
    (setf *chat* chat)
    (let ((model (position *claude-model* *claude-models* :test #'string=)))
      (when model (gtk:drop-down-set-selected models model)))
    (gobject:connect send :clicked (lambda (b) (declare (ignore b))
                                     (if (chat-busy chat) (call-command 'claude-stop) (call-command 'chat-send))))
    (let ((keys (gtk:event-controller-key-new)))
      (gobject:connect keys :key-pressed
                       (lambda (c keyval keycode state)
                         (declare (ignore c keycode))
                         (when (and (member (gdk:keyval-name keyval) '("Return" "KP_Enter") :test #'string=)
                                    (not (member :shift-mask (modifier-list state))))
                           (unless (chat-busy chat) (call-command 'chat-send))
                           t)))
      (gtk:widget-add-controller input keys))
    (gtk:stack-add-named stack (claude-status-page) "status")
    (gtk:stack-add-named stack scroller "chat")
    (show-claude-intro)
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :spacing 6 :margin-start 10 :margin-end 6 :margin-top 2 :margin-bottom 2
          status
          models
          (gtk:button :icon-name "cadre-document-new-symbolic" :tooltip-text "New conversation"
                      :css-classes '("flat")
                      :on-clicked (lambda (b) (declare (ignore b)) (call-command 'claude-new-chat))))
        (gtk:separator)
        stack
        (gtk:separator)
        (gtk:box :spacing 2 :margin-start 6 :margin-top 2
          (gtk:label :label "Context:" :css-classes '("dim-label") :margin-end 4)
          file problems debugger)
        (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-bottom 6
          (gtk:frame :hexpand t
            (gtk:scrolled-window :max-content-height 160 :propagate-natural-height t
                                 :hscrollbar-policy :never :child input))
          send)))))

(defvar *claude-status-label* nil)

(defun claude-status-page ()
  (setf *claude-status-label* (make-instance 'gtk:label :wrap t :justify :center :max-width-chars 60
                                                        :selectable t))
  (gtk:build
    (gtk:box :orientation :vertical :spacing 12 :valign :center :halign :center :margin-start 20 :margin-end 20
      (gtk:label :label "Claude" :css-classes '("title-2"))
      *claude-status-label*
      (gtk:box :spacing 8 :halign :center
        (gtk:button :label "Sign In…" :tooltip-text "Run claude auth login in a terminal"
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'claude-sign-in)))
        (gtk:button :label "Check Again" :css-classes '("suggested-action")
                    :on-clicked (lambda (b) (declare (ignore b)) (check-claude-status :force t)))))))

;;; Checking the CLI

(defun check-claude-status (&key force then)
  "Find the CLI and whether it is signed in (on a thread), then show the
chat or what is missing. THEN is called if Claude is ready."
  (when (or force (null *claude-status*))
    (unless *claude-checking*
      (setf *claude-checking* t)
      (chat-set-status "Checking Claude Code…")
      (sb-thread:make-thread
       (lambda ()
         (let ((status (check-claude)))
           (glib:call-in-main-thread
            (lambda ()
              (setf *claude-status* status *claude-checking* nil)
              (show-claude-status)
              (when (and then (claude-ready-p)) (funcall then))))))
       :name "check claude")
      (return-from check-claude-status)))
  (show-claude-status)
  (when (and then (claude-ready-p)) (funcall then)))

(defun claude-ready-p ()
  (and *claude-status* (claude-status-program *claude-status*) (claude-status-logged-in *claude-status*)))

(defun show-claude-status ()
  (when *chat*
    (let ((s *claude-status*))
      (cond ((null s))
            ((null (claude-status-program s))
             (gtk:label-set-text *claude-status-label*
                                 (format nil "Claude Code isn't installed.~%~%Install it from https://claude.com/claude-code, or set *claude-program* to its path, then Check Again."))
             (gtk:stack-set-visible-child-name (chat-stack *chat*) "status"))
            ((not (claude-status-logged-in s))
             (gtk:label-set-text *claude-status-label*
                                 (format nil "Claude Code ~a is installed but not signed in.~%~%Sign in with your Claude subscription or Console account (claude auth login), then Check Again."
                                         (claude-status-version s)))
             (gtk:stack-set-visible-child-name (chat-stack *chat*) "status"))
            (t (gtk:stack-set-visible-child-name (chat-stack *chat*) "chat")))
      (chat-update-status))))

(defun chat-set-status (text)
  (when *chat* (gtk:label-set-text (chat-status *chat*) text)))

(defun chat-update-status ()
  (when *chat*
    (chat-set-status
     (cond ((chat-busy *chat*) "Claude is working…")
           ((not (claude-ready-p)) (if *claude-status* "Claude Code is not ready" ""))
           (t (format nil "~a~@[ · $~,4f this conversation~]" (claude-status-version *claude-status*)
                      (and (plusp (chat-cost *chat*)) (chat-cost *chat*))))))
    (gtk:button-set-label (chat-send-button *chat*) (if (chat-busy *chat*) "Stop" "Send"))
    (gtk:widget-set-css-classes (chat-send-button *chat*)
                                (if (chat-busy *chat*) '("destructive-action") '("suggested-action")))))

;;; Adding to the conversation

(defun chat-append (widget)
  (gtk:box-append (chat-messages *chat*) widget)
  (chat-scroll-to-end)
  widget)

(defun chat-scroll-to-end ()
  (let ((scroller (chat-scroller *chat*)))
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (let ((adj (gtk:scrolled-window-get-vadjustment scroller)))
                       (gtk:adjustment-set-value adj (- (gtk:adjustment-get-upper adj) (gtk:adjustment-get-page-size adj))))
                     nil))))

(defun show-claude-intro ()
  (chat-append (make-instance 'gtk:label :wrap t :xalign 0.0 :css-classes '("dim-label")
                                         :label (format nil "Ask Claude about your code. It can read your buffers, look things up in your running Lisp, and propose edits, which you review in the editor before they are applied.~%~%Return sends; Shift+Return starts a new line."))))

(defun chat-note (text &rest classes)
  (chat-append (make-instance 'gtk:label :label text :wrap t :xalign 0.0 :selectable t
                                         :css-classes (or classes '("dim-label")))))

;;; Rendering replies: Markdown, simply

(defun inline-markup (text)
  "Pango markup for a line of Markdown: `code`, **bold**, *italic*."
  (let ((escaped (glib:markup-escape-text text -1)))
    (flet ((pairs (s delim open close)
             (with-output-to-string (out)
               (loop with start = 0
                     for a = (search delim s :start2 start)
                     for b = (and a (search delim s :start2 (+ a (length delim))))
                     while (and a b (> b (+ a (length delim))))
                     do (write-string s out :start start :end a)
                        (format out "~a~a~a" open (subseq s (+ a (length delim)) b) close)
                        (setf start (+ b (length delim)))
                     finally (write-string s out :start start)))))
      (pairs (pairs (pairs escaped "`" "<tt>" "</tt>") "**" "<b>" "</b>") "*" "<i>" "</i>"))))

(defun prose-markup (text)
  (with-output-to-string (out)
    (loop for (line . rest) on (split-lines text)
          do (let ((trimmed (string-left-trim " " line)))
               (cond ((and (plusp (length trimmed)) (char= (char trimmed 0) #\#))
                      (format out "<b>~a</b>" (inline-markup (string-left-trim "# " trimmed))))
                     ((and (> (length trimmed) 1) (member (char trimmed 0) '(#\- #\*)) (char= (char trimmed 1) #\Space))
                      (format out "  • ~a" (inline-markup (subseq trimmed 2))))
                     (t (write-string (inline-markup line) out))))
             (when rest (terpri out)))))

(defun markdown-segments (text)
  "TEXT split into (:prose string) and (:code string language) segments at ``` fences."
  (let ((segments '()) (lines '()) (in-code nil) (language nil))
    (flet ((flush (kind)
             (let ((s (format nil "~{~a~^~%~}" (reverse lines))))
               (when (or (eq kind :code) (plusp (length (string-trim '(#\Space #\Newline) s))))
                 (push (if (eq kind :code) (list :code s language) (list :prose s)) segments)))
             (setf lines '())))
      (dolist (line (split-lines text))
        (if (and (>= (length (string-left-trim " " line)) 3)
                 (string= "```" (string-left-trim " " line) :end2 3))
            (if in-code
                (progn (flush :code) (setf in-code nil))
                (progn (flush :prose) (setf in-code t language (string-trim " `" line))))
            (push line lines)))
      (flush (if in-code :code :prose)))
    (nreverse segments)))

(defun code-block-widget (code)
  (let ((copy (make-instance 'gtk:button :label "Copy" :css-classes '("flat")))
        (insert (make-instance 'gtk:button :label "Insert" :css-classes '("flat")
                                           :tooltip-text "Insert at the cursor in the current file")))
    (gobject:connect copy :clicked (lambda (b) (declare (ignore b))
                                     (gdk:clipboard-set-text (gtk:widget-get-clipboard (chat-messages *chat*)) code)
                                     (message "Copied")))
    (gobject:connect insert :clicked (lambda (b) (declare (ignore b))
                                       (let ((view (current-tab-view)))
                                         (if view
                                             (with-user-action ((view-gtk-buffer view))
                                               (gtk:text-buffer-insert-at-cursor (view-gtk-buffer view) code -1))
                                             (message "No file is open")))))
    (gtk:build
      (gtk:box :orientation :vertical :css-classes '("cadre-chat-code")
        (gtk:label :label code :xalign 0.0 :selectable t :wrap t :wrap-mode :char :css-classes '("monospace"))
        (gtk:box :halign :end copy insert)))))

(defun render-markdown (box text)
  "Fill BOX with TEXT, rendered."
  (dolist (segment (markdown-segments text))
    (gtk:box-append box (if (eq (first segment) :code)
                            (code-block-widget (second segment))
                            (let ((label (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t
                                                                   :wrap-mode :word-char)))
                              (gtk:label-set-markup label (prose-markup (second segment)))
                              label)))))

;;; Events from the CLI

(defun chat-append-delta (text)
  (unless (chat-stream-box *chat*)
    (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6))
          (label (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t :wrap-mode :word-char)))
      (gtk:box-append box label)
      (setf (chat-stream-box *chat*) (chat-append box)
            (chat-stream-label *chat*) label
            (chat-stream-text *chat*) "")))
  (setf (chat-stream-text *chat*) (concatenate 'string (chat-stream-text *chat*) text))
  (gtk:label-set-text (chat-stream-label *chat*) (chat-stream-text *chat*))
  (chat-scroll-to-end))

(defun chat-finish-text (text)
  "A finished block of the reply: render it in place of what was streamed."
  (let ((box (or (chat-stream-box *chat*)
                 (chat-append (make-instance 'gtk:box :orientation :vertical :spacing 6)))))
    (clear-box box)
    (render-markdown box text)
    (setf (chat-stream-box *chat*) nil (chat-stream-label *chat*) nil)
    (chat-scroll-to-end)))

(defun tool-summary (name input)
  (let ((short (tool-display-name name)))
    (format nil "~a~@[  ~a~]" short
            (and (jobject-p input)
                 (let ((v (or (jget input "buffer") (jget input "file") (jget input "symbol") (jget input "form")
                              (jget input "file_path") (jget input "pattern") (jget input "command"))))
                   (and (stringp v) (substitute #\Space #\Newline (if (> (length v) 80) (subseq v 0 80) v))))))))

(defun chat-tool-row (id name input)
  (let ((entry (gethash id (chat-tools *chat*))))
    (if entry
        (gtk:expander-set-label (car entry) (format nil "⚙ ~a" (tool-summary name input)))
        (let* ((result (make-instance 'gtk:label :xalign 0.0 :wrap t :selectable t :wrap-mode :char
                                                 :css-classes '("monospace" "dim-label")))
               (expander (make-instance 'gtk:expander :label (format nil "⚙ ~a" (tool-summary name input))
                                                      :child result :css-classes '("cadre-chat-tool"))))
          (setf (chat-stream-box *chat*) nil)
          (setf (gethash id (chat-tools *chat*)) (cons expander result))
          (chat-append expander)))))

(defun chat-tool-result (id text error)
  (let ((entry (gethash id (chat-tools *chat*))))
    (when entry
      (gtk:label-set-text (cdr entry) (if (> (length text) 4000) (concatenate 'string (subseq text 0 4000) "…") text))
      (when error
        (gtk:expander-set-label (car entry) (format nil "~a — failed" (gtk:expander-get-label (car entry))))))))

(defun handle-claude-event (event)
  (unless (event-subagent event)
    (case (event-kind event)
      (:init (setf (chat-session-id *chat*) (event-session-id event))
       (let ((cadre (cdr (assoc "cadre" (event-data event) :test #'equal))))
         (unless (equal cadre "connected")
           (chat-note (format nil "Cadre's tools are not available to Claude (MCP status: ~a)." (or cadre "missing"))))))
      (:message-start (setf (chat-stream-box *chat*) nil))
      (:text-delta (chat-append-delta (event-text event)))
      (:text (chat-finish-text (event-text event)))
      (:tool-start (chat-tool-row (event-id event) (event-name event) nil))
      (:tool-use (chat-tool-row (event-id event) (event-name event) (event-input event)))
      (:tool-result (chat-tool-result (event-id event) (event-text event) (event-error event)))
      (:result
       (setf (chat-busy *chat*) nil
             (chat-stream-box *chat*) nil)
       (when (numberp (event-cost event)) (setf (chat-cost *chat*) (event-cost event)))
       (when (and (event-error event) (plusp (length (event-text event))))
         (chat-note (event-text event) "error"))
       (chat-update-status)))))

(defun claude-exited (process code)
  (when (and *chat* (eq process (chat-process *chat*)))
    (setf (chat-process *chat*) nil)
    (when (chat-busy *chat*)
      (setf (chat-busy *chat*) nil)
      (unless (cp-stopped process)
        (chat-note (format nil "Claude Code exited (status ~a). See the Output tab for its messages." code) "error")))
    (deny-pending-approvals)
    (chat-update-status)))

(defun ensure-claude-process ()
  "The conversation's process, started (or resumed) if needed."
  (or (and (claude-alive-p (chat-process *chat*)) (chat-process *chat*))
      (progn
        (ensure-mcp-server)
        (let* ((resume (and (chat-started *chat*) (chat-session-id *chat*)))
               (model (nth (gtk:drop-down-get-selected (chat-model-dropdown *chat*)) *claude-models*))
               (process nil))
          (setf process
                (start-claude (claude-arguments :session-id (unless resume (chat-session-id *chat*))
                                                :resume resume
                                                :model model :effort *claude-effort*
                                                :mcp-config *mcp-config*
                                                :permission-tool "mcp__cadre__approve"
                                                :allowed-tools *claude-allowed-tools*
                                                :disallowed-tools *claude-disallowed-tools*
                                                :system-prompt *claude-system-prompt*
                                                :isolated *claude-isolated*)
                              :program (claude-status-program *claude-status*)
                              :directory (or (window-project *window*) (user-homedir-pathname))
                              :environment (list "MCP_TOOL_TIMEOUT=3600000")
                              :deliver #'deliver-to-gui
                              :on-event #'handle-claude-event
                              :on-stderr (lambda (line) (panel-log-raw (window-panel *window*) (format nil "claude: ~a" line)))
                              :on-exit (lambda (code) (claude-exited process code))))
          (setf (chat-process *chat*) process
                (chat-started *chat*) t)
          process))))

;;; Context sent with a message

(defun message-context ()
  (with-output-to-string (out)
    (let ((view (current-tab-view)))
      (when (and view (gtk:toggle-button-get-active (chat-context-file *chat*)))
        (let ((buffer (view-buffer view)))
          (multiple-value-bind (line column) (view-cursor-line-column view)
            (format out "The user is in ~a~@[ (~a)~], line ~d, column ~d~:[~;, with unsaved changes~].~%"
                    (buffer-name buffer) (and (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)))
                    line column (buffer-modified-p buffer)))
          (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
            (when has
              (format out "Selected text:~%```~%~a~%```~%" (gtk:text-buffer-get-text (view-gtk-buffer view) start end t)))))))
    (when (and (gtk:toggle-button-get-active (chat-context-problems *chat*)) *notes*)
      (format out "Compiler notes:~%~a~%" (call-mcp-tool "get_problems" (jobj))))
    (when (and (gtk:toggle-button-get-active (chat-context-debugger *chat*)) *debug-levels*)
      (format out "The debugger is active:~%~a~%" (call-mcp-tool "get_backtrace" (jobj))))))

(defun send-to-claude (text)
  (let ((context (message-context)))
    (chat-append (gtk:build
                   (gtk:box :halign :end :css-classes '("cadre-chat-user")
                     (gtk:label :label text :wrap t :xalign 0.0 :selectable t :wrap-mode :word-char))))
    (setf (chat-busy *chat*) t)
    (chat-update-status)
    (claude-send (ensure-claude-process)
                 (if (plusp (length context))
                     (format nil "<editor-context>~%~a</editor-context>~%~%~a" context text)
                     text))))

;;; Approvals

(defun chat-show-approval (title detail done)
  "Ask, in the chat, whether Claude may do something; call DONE with :once, :session or :deny."
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "claude")
  (let* ((card (make-instance 'gtk:box :orientation :vertical :spacing 6 :css-classes '("cadre-chat-approval")))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (answered nil)
         (deny nil))
    (labels ((answer (value label)
               (unless answered
                 (setf answered t
                       (chat-pending-approvals *chat*) (remove deny (chat-pending-approvals *chat*)))
                 (gtk:box-remove card buttons)
                 (gtk:box-append card (make-instance 'gtk:label :label label :xalign 0.0 :css-classes '("dim-label")))
                 (funcall done value))))
      (setf deny (lambda () (answer :deny "Denied")))
      (push deny (chat-pending-approvals *chat*))
      (gtk:box-append card (make-instance 'gtk:label :label title :xalign 0.0 :css-classes '("heading")))
      (gtk:box-append card (make-instance 'gtk:label :label detail :xalign 0.0 :wrap t :selectable t
                                                     :wrap-mode :char :css-classes '("monospace")))
      (flet ((button (label value note &optional classes)
               (let ((b (make-instance 'gtk:button :label label :css-classes classes)))
                 (gobject:connect b :clicked (lambda (x) (declare (ignore x)) (answer value note)))
                 (gtk:box-append buttons b))))
        (button "Deny" :deny "Denied")
        (button "Allow for This Conversation" :session "Allowed for this conversation")
        (button "Allow Once" :once "Allowed" '("suggested-action")))
      (gtk:box-append card buttons)
      (setf (chat-stream-box *chat*) nil)
      (chat-append card))))

(defun deny-pending-approvals ()
  (when *chat*
    (mapc #'funcall (copy-list (chat-pending-approvals *chat*)))
    (setf (chat-pending-approvals *chat*) '())))

;;; Commands

(defun show-claude-page (&key focus)
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "claude")
  (check-claude-status)
  (when focus (gtk:widget-grab-focus (chat-input *chat*))))

(define-command claude ()
  "Show the Claude panel, to chat with Claude about your code."
  (show-claude-page :focus t))

(define-command chat-send ()
  "Send what is typed in the Claude panel."
  (let* ((input (gtk:text-view-get-buffer (chat-input *chat*)))
         (text (string-trim '(#\Space #\Newline #\Tab) (text-string input))))
    (cond ((string= text "") (message "Type a message for Claude first"))
          ((chat-busy *chat*) (message "Claude is still working"))
          (t (check-claude-status
              :then (lambda ()
                      (text-replace-contents input "")
                      (send-to-claude text)))))))

(define-command claude-stop ()
  "Stop what Claude is doing."
  (let ((process (chat-process *chat*)))
    (when process
      (deny-pending-approvals)
      (claude-interrupt process)
      ;; If the turn hasn't ended shortly, stop the process; the next
      ;; message resumes the session.
      (glib:timeout-add-seconds glib:+priority-default+ 3
                                (lambda ()
                                  (when (and (chat-busy *chat*) (eq process (chat-process *chat*)))
                                    (stop-claude process)
                                    (setf (chat-busy *chat*) nil)
                                    (chat-note "Stopped.")
                                    (chat-update-status))
                                  nil)))))

(define-command claude-new-chat ()
  "Start a new conversation with Claude."
  (when (chat-process *chat*) (stop-claude (chat-process *chat*)))
  (deny-pending-approvals)
  (clrhash *session-allowed*)
  (clrhash (chat-tools *chat*))
  (setf (chat-process *chat*) nil (chat-busy *chat*) nil (chat-started *chat*) nil
        (chat-session-id *chat*) (make-uuid) (chat-cost *chat*) 0 (chat-stream-box *chat*) nil)
  (clear-box (chat-messages *chat*))
  (show-claude-intro)
  (chat-update-status))

(define-command claude-sign-in ()
  "Sign in to Claude Code, in a terminal window."
  (let ((program (and *claude-status* (claude-status-program *claude-status*))))
    (unless program (editor-error "Claude Code was not found"))
    (let ((command (format nil "'~a' auth login" program)))
      (cond ((macos-p)
             (uiop:launch-program (list "osascript" "-e"
                                        (format nil "tell application \"Terminal\" to do script ~s" command)
                                        "-e" "tell application \"Terminal\" to activate")))
            (t (uiop:launch-program (list "x-terminal-emulator" "-e" "sh" "-c" command))))
      (message "Sign in in the terminal, then press Check Again"))))

(define-command ask-claude-about-error ()
  "Ask Claude to explain the error in the debugger."
  (show-claude-page)
  (gtk:toggle-button-set-active (chat-context-debugger *chat*) t)
  (text-replace-contents (gtk:text-view-get-buffer (chat-input *chat*))
                         "Explain this error and how to fix it.")
  (chat-send))

(define-command ask-claude-about-problems ()
  "Ask Claude to fix the compiler's errors and warnings."
  (show-claude-page)
  (gtk:toggle-button-set-active (chat-context-problems *chat*) t)
  (text-replace-contents (gtk:text-view-get-buffer (chat-input *chat*))
                         "Fix the compiler problems.")
  (chat-send))
