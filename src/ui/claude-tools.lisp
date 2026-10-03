;;;; claude-tools.lisp — the editor's tools, offered to Claude over MCP
;;;;
;;;; Tool calls arrive on the MCP server's threads. Anything touching GTK
;;;; or the editor's state hops to the main thread; Swank requests are sent
;;;; from there too, and the tool's thread waits for the answer. Tools that
;;;; change things (eval, compile_file, propose_edit, and Claude Code's own
;;;; tools through `approve`) wait for the user's answer in the chat.
;;;; Nothing here evaluates in the editor's own Lisp.

(in-package #:cadre-ui)

(defmacro on-main (&body body)
  "Run BODY on the GTK thread and return its values."
  `(glib:call-in-main-thread (lambda () ,@body) :wait t))

(defun wait-for (setup &key (timeout nil))
  "Call SETUP on the main thread with a function that takes the result;
wait for that result here."
  (let ((semaphore (sb-thread:make-semaphore)) (result nil))
    (glib:call-in-main-thread
     (lambda () (funcall setup (lambda (value) (setf result value) (sb-thread:signal-semaphore semaphore)))))
    (unless (sb-thread:wait-on-semaphore semaphore :timeout timeout)
      (tool-error "Timed out"))
    result))

;;; The Lisp

(defun ensure-lisp ()
  "Start a Lisp if there is none, and wait until it is connected."
  (unless (on-main (connected-p))
    (let ((ok (wait-for (lambda (done)
                          (call-with-connection (lambda (c) (declare (ignore c)) (funcall done t)))
                          (glib:timeout-add-seconds glib:+priority-default+ 180
                                                    (lambda () (funcall done nil) nil)))
                        :timeout 200)))
      (unless ok (tool-error "No Lisp is connected, and starting one failed.")))))

(defun swank-sync (form &key package (thread t) (timeout 60))
  "Send FORM to the connected Lisp and wait for its value."
  (ensure-lisp)
  (let ((outcome (wait-for (lambda (done)
                             (rex *connection* form :package (or package (connection-package *connection*))
                                                    :thread thread
                                                    :on-ok (lambda (v) (funcall done (list :ok v)))
                                                    :on-abort (lambda (r) (funcall done (list :abort r)))))
                           :timeout timeout)))
    (if (eq (first outcome) :ok)
        (second outcome)
        (tool-error "The Lisp aborted the request~@[: ~a~]" (second outcome)))))

(defun tool-package (args)
  (or (tool-argument args "package")
      (on-main (let ((view (selected-view *window*)))
                 (if view (view-package view) (if (connected-p) (connection-package *connection*) "COMMON-LISP-USER"))))))

;;; Buffers and files

(defun resolve-path (name)
  "NAME as a pathname: absolute, or relative to the open folder."
  (let ((path (pathname name)))
    (if (or (uiop:absolute-pathname-p path) (null (window-project *window*)))
        path
        (merge-pathnames path (window-project *window*)))))

(defun tool-buffer (name)
  "The open buffer named NAME, or visiting the file NAME. Main thread."
  (or (find-buffer name)
      (let ((path (resolve-path name)))
        (and (probe-file path) (find-file-buffer (truename path))))))

(defun numbered-lines (text &optional (start 1) end)
  (with-output-to-string (out)
    (loop for line in (split-lines text)
          for n from 1
          when (and (>= n start) (or (null end) (<= n end)))
            do (format out "~6d	~a~%" n line))))

;;; Asking for the user's approval

(defvar *session-allowed* (make-hash-table :test 'equal)
  "Tools the user allowed for this conversation.")

(defun ask-approval (key title detail)
  "Ask in the chat whether Claude may do something. KEY names what a \"for
this session\" answer covers. Returns :once, :session or :deny."
  (if (on-main (gethash key *session-allowed*))
      :session
      (let ((answer (wait-for (lambda (done) (chat-show-approval title detail done)))))
        (when (eq answer :session)
          (on-main (setf (gethash key *session-allowed*) t)))
        answer)))

(defun require-approval (key title detail)
  (when (eq (ask-approval key title detail) :deny)
    (tool-error "The user declined.")))

;;; Tools that read

(define-mcp-tool "list_buffers" (args)
  "List the buffers open in Cadre: name, file, whether they have unsaved changes, and mode."
  ()
  (on-main
    (with-output-to-string (out)
      (dolist (buffer (buffer-list))
        (unless (string= (buffer-name buffer) "*repl*")
          (format out "~a~@[  ~a~]~:[~;  (unsaved changes)~]  [~(~a~)]~%"
                  (buffer-name buffer) (and (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)))
                  (buffer-modified-p buffer) (buffer-major-mode buffer)))))))

(define-mcp-tool "read_buffer" (args)
  "Read a buffer's text as the editor has it, including unsaved changes, with line
numbers. BUFFER is a buffer name or a file path (relative to the project, or absolute);
a file that isn't open is read from disk. Prefer this to reading files directly."
  (("buffer" "string" "Buffer name or file path" :required t)
   ("start_line" "integer" "First line to return (from 1)")
   ("end_line" "integer" "Last line to return"))
  (let* ((name (tool-argument args "buffer" :required t))
         (text (on-main (let ((buffer (tool-buffer name)))
                          (and buffer (buffer-string buffer)))))
         (text (or text
                   (let ((path (on-main (resolve-path name))))
                     (if (probe-file path)
                         (uiop:read-file-string path)
                         (tool-error "No buffer or file named ~a" name))))))
    (numbered-lines text (or (tool-argument args "start_line") 1) (tool-argument args "end_line"))))

(define-mcp-tool "current_context" (args)
  "What the user is looking at: the current file, cursor line and column, the selection,
the code's package, the project folder, and the connected Lisp."
  ()
  (on-main
    (let ((view (selected-view *window*)))
      (with-output-to-string (out)
        (format out "Project: ~a~%" (if (window-project *window*) (uiop:native-namestring (window-project *window*)) "none"))
        (format out "Lisp: ~a~%" (if (connected-p) (connection-implementation *connection*) "not connected"))
        (if (null view)
            (format out "No file is open.~%")
            (let ((buffer (view-buffer view)))
              (multiple-value-bind (line column) (view-cursor-line-column view)
                (format out "Buffer: ~a~@[ (~a)~]~:[~; — has unsaved changes~]~%Cursor: line ~d, column ~d~%Package: ~a~%"
                        (buffer-name buffer) (and (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)))
                        (buffer-modified-p buffer) line column (view-package view)))
              (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
                (when has
                  (format out "Selection (lines ~d–~d):~%~a~%" (1+ (gtk:text-iter-get-line start))
                          (1+ (gtk:text-iter-get-line end))
                          (gtk:text-buffer-get-text (view-gtk-buffer view) start end t))))))))))

(define-mcp-tool "get_problems" (args)
  "The compiler's current errors, warnings and notes, with file and line."
  ()
  (on-main
    (if (null *notes*)
        "No problems."
        (with-output-to-string (out)
          (dolist (s *notes*)
            (format out "~(~a~) ~a: ~a~%" (compiler-note-severity (sn-note s)) (note-place-string s)
                    (compiler-note-message (sn-note s))))))))

(define-mcp-tool "get_backtrace" (args)
  "The debugger's state, if an error is waiting there: the condition, the restarts and the backtrace."
  ()
  (on-main
    (let ((level (first *debug-levels*)))
      (if (null level)
          "The debugger is not active."
          (with-output-to-string (out)
            (format out "~{~a~^ ~}~%~%Restarts:~%" (dl-condition level))
            (loop for (name description) in (dl-restarts level) for i from 0
                  do (format out "  ~d: [~a] ~a~%" i name description))
            (format out "~%Backtrace:~%")
            (dolist (frame (dl-frames level))
              (format out "  ~d: ~a~%" (frame-number frame) (frame-description frame))))))))

(define-mcp-tool "describe_symbol" (args)
  "Describe a symbol in the running Lisp image: what it names, its documentation, arglist, value."
  (("symbol" "string" "The symbol, e.g. \"my-pkg::foo\"" :required t)
   ("package" "string" "The package to read it in (default: the current file's)"))
  (swank-sync (swank-call "swank:describe-symbol" (tool-argument args "symbol" :required t))
              :package (tool-package args)))

(define-mcp-tool "arglist" (args)
  "The arglist of a function or macro in the running image."
  (("symbol" "string" "The operator's name" :required t)
   ("package" "string" "The package to read it in"))
  (let ((package (tool-package args)))
    (or (swank-sync (swank-call "swank:operator-arglist" (tool-argument args "symbol" :required t) package)
                    :package package)
        "Not a function or macro.")))

(defun locations-text (entries)
  "ENTRIES, (label location) pairs from Swank, as lines of text."
  (with-output-to-string (out)
    (loop for (label location) in entries
          for place = (parse-location location)
          do (format out "~a — ~a~%" label
                     (if (getf place :error)
                         (getf place :error)
                         (format nil "~a~@[:~d~]" (or (getf place :file) (getf place :buffer) "?")
                                 (let ((line (on-main (location-line place)))) (and line (1+ line)))))))))

(define-mcp-tool "find_definitions" (args)
  "Where a symbol is defined (functions, methods, classes, variables), from the running image."
  (("symbol" "string" "The symbol" :required t)
   ("package" "string" "The package to read it in"))
  (let ((found (swank-sync (swank-call "swank:find-definitions-for-emacs" (tool-argument args "symbol" :required t))
                           :package (tool-package args))))
    (if found (locations-text found) "No definitions found.")))

(defun xref-tool (kind args)
  (let ((found (swank-sync (swank-call "swank:xref" kind (tool-argument args "symbol" :required t))
                           :package (tool-package args))))
    (cond ((eq found :not-implemented) "This Lisp cannot answer that.")
          ((null found) "None found.")
          (t (locations-text found)))))

(define-mcp-tool "who_calls" (args)
  "The functions that call a function, from the running image."
  (("symbol" "string" "The function" :required t)
   ("package" "string" "The package to read it in"))
  (xref-tool :calls args))

(define-mcp-tool "who_references" (args)
  "The code that refers to a global variable, from the running image."
  (("symbol" "string" "The variable" :required t)
   ("package" "string" "The package to read it in"))
  (xref-tool :references args))

(define-mcp-tool "macroexpand" (args)
  "Expand a macro form in the running image."
  (("form" "string" "The form, as source text" :required t)
   ("all" "boolean" "Expand every macro inside it, not just the outer one")
   ("package" "string" "The package to read it in"))
  (swank-sync (swank-call (if (jtrue-p (tool-argument args "all")) "swank:swank-macroexpand-all" "swank:swank-macroexpand-1")
                          (tool-argument args "form" :required t))
              :package (tool-package args)))

(define-mcp-tool "apropos" (args)
  "Search the running image's symbols whose names contain a string."
  (("pattern" "string" "Part of the name" :required t)
   ("package" "string" "Only symbols in this package"))
  (let ((found (swank-sync (swank-call "swank:apropos-list-for-emacs" (tool-argument args "pattern" :required t)
                                       nil nil (tool-argument args "package"))
                           :timeout 30)))
    (if found
        (format nil "~{~a~%~}" (loop for entry in found for i below 200 collect (getf entry :designator)))
        "None found.")))

(define-mcp-tool "list_systems" (args)
  "The ASDF systems defined in the project folder's .asd files."
  ()
  (let ((directory (on-main (window-project *window*))))
    (if (null directory)
        "No folder is open."
        (format nil "~:{~a (~a)~%~}"
                (mapcar (lambda (e) (list (first e) (file-namestring (second e))))
                        (project-system-entries directory))))))

;;; Tools that change things

(define-mcp-tool "propose_edit" (args)
  "Propose a change to a file. The user reviews it as an inline diff in the editor and
accepts or rejects it; this returns their answer. Give OLD_TEXT (exact text that occurs
once in the file, as shown by read_buffer without line numbers) and NEW_TEXT to replace
it; or omit OLD_TEXT to give the whole new content of the file (for a new file).
Use this for every change to files: there is no other way to edit."
  (("file" "string" "The file path (relative to the project, or absolute) or buffer name" :required t)
   ("old_text" "string" "The exact text to replace")
   ("new_text" "string" "The replacement, or the whole new content" :required t)
   ("explanation" "string" "One line saying what the change does"))
  (let* ((name (tool-argument args "file" :required t))
         (old-text (tool-argument args "old_text"))
         (new-text (tool-argument args "new_text" :required t))
         (buffer (or (on-main (tool-buffer name))
                     (let ((path (on-main (resolve-path name))))
                       (if (probe-file path)
                           (wait-for (lambda (done)
                                       (open-file-path path :then (lambda (view) (funcall done (view-buffer view))))))
                           (on-main
                             (let ((buffer (make-buffer :name (file-namestring path) :file path :text (make-gtk-text)
                                                        :major-mode (major-mode-for-file (namestring path)))))
                               (show-buffer *window* buffer)
                               buffer))))))
         (current (on-main (buffer-string buffer)))
         (new (if old-text (replace-unique current old-text new-text) new-text)))
    (when (string= new current) (tool-error "That doesn't change anything."))
    (ecase (wait-for (lambda (done) (propose-edit buffer new (tool-argument args "explanation") done)))
      (:saved (format nil "The user accepted the edit; ~a was saved." (on-main (buffer-name buffer))))
      (:accepted (format nil "The user accepted the edit. ~a has unsaved changes." (on-main (buffer-name buffer))))
      (:rejected "The user rejected the edit. Ask what they would like instead.")
      (:changed "The file changed while the user was reviewing; the edit was not applied. Read it again."))))

(defun guarded-source (form)
  "FORM (source text) wrapped so an error returns its message instead of
entering the debugger."
  (format nil "(cl:handler-case (cl:progn ~a~%) (cl:error (#1=#:e) (cl:format nil \"Error: ~~a\" #1#)))" form))

(define-mcp-tool "eval" (args)
  "Evaluate a form in the user's running Lisp (not the editor's) and return its output and
value. The user approves each evaluation, unless they allowed it for the session."
  (("form" "string" "The form, as source text" :required t)
   ("package" "string" "The package to evaluate it in"))
  (let ((form (tool-argument args "form" :required t))
        (package (tool-package args)))
    (require-approval "eval" "Claude wants to evaluate" form)
    (destructuring-bind (output value)
        (swank-sync (swank-call "swank:eval-and-grab-output" (guarded-source form)) :package package :timeout 300)
      (on-main (image-changed))
      (format nil "~@[Output:~%~a~%~]Value: ~a" (and (plusp (length output)) output) value))))

(define-mcp-tool "compile_file" (args)
  "Compile and load a file in the user's running Lisp; returns the compiler's notes. The file
must be saved. The user approves this unless they allowed it for the session."
  (("file" "string" "The file path (relative to the project, or absolute)" :required t))
  (let* ((path (on-main (resolve-path (tool-argument args "file" :required t))))
         (buffer (on-main (and (probe-file path) (find-file-buffer (truename path))))))
    (unless (probe-file path) (tool-error "No file ~a" path))
    (when (and buffer (on-main (buffer-modified-p buffer)))
      (tool-error "~a has unsaved changes; ask the user to save it." (file-namestring path)))
    (require-approval "compile" "Claude wants to compile and load" (uiop:native-namestring path))
    (let ((result (swank-sync (swank-call "swank:compile-file-for-emacs" (uiop:native-namestring path) t) :timeout 300)))
      (multiple-value-bind (notes successp) (parse-compilation-result result)
        (when (and successp (fifth result) (sixth result))
          (swank-sync (swank-call "swank:load-file" (sixth result)) :timeout 300))
        (on-main (show-notes notes :buffer buffer :replace-lines (and buffer :all))
                 (image-changed))
        (with-output-to-string (out)
          (format out "~:[Compilation failed~;Compiled and loaded~]. ~d note~:p.~%" successp (length notes))
          (dolist (note notes)
            (let ((place (compiler-note-location note)))
              (format out "~(~a~)~@[ at offset ~d~]: ~a~%" (compiler-note-severity note) (getf place :position)
                      (compiler-note-message note)))))))))

;;; Claude Code's permission prompts

(defun summarize-tool-input (tool input)
  (or (and (jobject-p input)
           (or (jget input "command") (jget input "url") (jget input "query") (jget input "file_path")))
      (json-string input)
      tool))

(define-mcp-tool "approve" (args)
  "Internal: answers Claude Code's permission prompts by asking the user."
  (("tool_name" "string" "The tool Claude wants to use" :required t)
   ("input" "object" "Its input")
   ("tool_use_id" "string" "The tool call's id"))
  (let* ((tool (tool-argument args "tool_name" :required t))
         (input (or (tool-argument args "input") (jobj)))
         (answer (ask-approval tool (format nil "Claude wants to use ~a" (tool-display-name tool))
                               (summarize-tool-input tool input))))
    (json-string (if (eq answer :deny)
                     (jobj "behavior" "deny" "message" "The user declined.")
                     (jobj "behavior" "allow" "updatedInput" input)))))
