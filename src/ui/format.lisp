;;;; format.lisp — Format Document
;;;;
;;;; Lays out the whole buffer again, as one undo step, keeping the cursor on
;;;; its line: JSON, CSS and XML with Cadre's own formatters (core format.lisp),
;;;; Lisp by indenting every line, and other languages with an external
;;;; formatter (*formatter-commands*: Prettier for JavaScript, TypeScript and
;;;; HTML), which reads the text on standard input and writes the result.

(in-package #:cadre-ui)

(define-option *format-on-save* nil boolean
  "Format files with Cadre's or the configured formatter whenever they are saved."
  :category "Editing")

(define-option *formatter-commands*
    '((javascript-mode "prettier" "--stdin-filepath" "{file}")
      (typescript-mode "prettier" "--stdin-filepath" "{file}")
      (tsx-mode "prettier" "--stdin-filepath" "{file}")
      (html-mode "prettier" "--stdin-filepath" "{file}"))
    list
  "External formatters, as (major-mode program argument…): the program
reads the text on standard input and writes it formatted; {file} stands for
the file's name (Prettier uses it to choose the language). A mode listed here
uses its program even if Cadre has a formatter of its own (JSON, CSS)."
  :category "Languages")

(defun replace-buffer-with-formatted (buffer text &key view report)
  "Make BUFFER hold TEXT as one undo step, preserving its cursor line and column."
  (let* ((gtk-buffer (buffer-text buffer))
         (old (buffer-string buffer)))
    (unless (string= old text)
      (multiple-value-bind (line column) (text-position-line gtk-buffer (text-point gtk-buffer))
        (with-user-action (gtk-buffer)
          (gtk:text-buffer-delete gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer)
                                  (gtk:text-buffer-get-end-iter gtk-buffer))
          (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer) text -1))
        (let ((line (min line (1- (gtk:text-buffer-get-line-count gtk-buffer)))))
          (gtk:text-buffer-place-cursor gtk-buffer
                                        (line-iter gtk-buffer line
                                                   (min column (length (text-line-string gtk-buffer line))))))
        (when view (scroll-to-cursor view))
        (when report (message "Formatted"))
        t))))

(defun replace-with-formatted (view text)
  "Make VIEW's buffer hold TEXT as one undo step, with the cursor on its line."
  (unless (replace-buffer-with-formatted (view-buffer view) text :view view :report t)
    (message "Already formatted")))

(defun external-formatter (mode)
  (cdr (assoc mode *formatter-commands*)))

(defparameter *formatter-timeout* 20
  "Seconds an external formatter may take before it is stopped.")

(defparameter *formatter-stop-grace* 1
  "Seconds a timed-out formatter has to end after SIGTERM before SIGKILL.")

(defun stop-formatter-process (process)
  "End PROCESS: SIGTERM, then, if it is still running after
*FORMATTER-STOP-GRACE* seconds (it may ignore SIGTERM), SIGKILL."
  (sb-ext:process-kill process 15)
  (let ((deadline (+ (get-internal-real-time) (* *formatter-stop-grace* internal-time-units-per-second))))
    (loop while (and (sb-ext:process-alive-p process) (< (get-internal-real-time) deadline))
          do (sleep 0.02)))
  (when (sb-ext:process-alive-p process)
    (sb-ext:process-kill process 9))
  (sb-ext:process-wait process))

(defun run-formatter-process (buffer text command then &key (deliver #'glib:call-in-main-thread)
                                                            (timeout *formatter-timeout*))
  "Run COMMAND, an external formatter (*formatter-commands*), on TEXT, from
BUFFER, in the background. Call THEN through DELIVER with the formatted text,
or with nil and why not. A formatter still running after TIMEOUT seconds is
stopped."
  (let* ((file (if (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)) (buffer-name buffer)))
         (directory (and (buffer-file buffer)
                         (uiop:native-namestring (uiop:pathname-directory-pathname (buffer-file buffer)))))
         (program (first command))
         (arguments (mapcar (lambda (a) (cl-ppcre:regex-replace-all "\\{file\\}" a file)) (rest command))))
    (flet ((temporary-file ()
             (uiop:with-temporary-file (:pathname path :keep t :prefix "cadre-format") path)))
      (let ((input (temporary-file)) (output (temporary-file)) (errors (temporary-file)))
        (with-open-file (out input :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string text out))
        (sb-thread:make-thread
         (lambda ()
           (let ((result
                   (handler-case
                       (let ((process (sb-ext:run-program program arguments
                                                          :search t :wait nil :directory directory
                                                          :input input
                                                          :output output :if-output-exists :supersede
                                                          :error errors :if-error-exists :supersede))
                             (deadline (+ (get-internal-real-time)
                                          (* timeout internal-time-units-per-second))))
                         (unwind-protect
                              (let ((timed-out nil))
                                (loop while (sb-ext:process-alive-p process)
                                      do (when (> (get-internal-real-time) deadline)
                                           (setf timed-out t)
                                           (stop-formatter-process process)
                                           (return))
                                         (sleep 0.02))
                                (sb-ext:process-wait process)
                                (let ((code (sb-ext:process-exit-code process)))
                                  ;; However it ended once stopped (a formatter may catch
                                  ;; SIGTERM and exit 0), its output is incomplete.
                                  (cond (timed-out
                                         (list nil (format nil "~a took longer than ~d s and was stopped" program timeout)))
                                        ((eql code 0)
                                         (list (uiop:read-file-string output :external-format :utf-8) nil))
                                        (t (let ((error (string-trim '(#\Newline #\Space)
                                                                     (uiop:read-file-string errors :external-format :utf-8))))
                                             (list nil (format nil "~a couldn't format it: ~a" program
                                                               (subseq error 0 (or (position #\Newline error) (length error))))))))))
                           (sb-ext:process-close process)))
                     (error (e) (list nil (format nil "Could not run ~a: ~a" program e))))))
             (dolist (path (list input output errors)) (ignore-errors (delete-file path)))
             (funcall deliver (lambda () (apply then result)))))
         :name "cadre format")))))

(defun format-externally-before-save (buffer then)
  "The save preparer (*save-preparer*): with *FORMAT-ON-SAVE*, run BUFFER's
external formatter in the background, then call THEN to save. The text is
saved unformatted if the formatter fails, takes too long, or the text changes
meanwhile."
  (let ((command (and *format-on-save* (external-formatter (buffer-major-mode buffer)))))
    (if (null command)
        (funcall then)
        (let ((text (buffer-string buffer)))
          (message "Formatting with ~a before saving…" (first command))
          (run-formatter-process
           buffer text command
           (lambda (formatted problem)
             (cond (problem (message "~a; saving without formatting" problem))
                   ((not (string= text (buffer-string buffer)))
                    (message "The text changed while it was being formatted; saving without formatting"))
                   (t (replace-buffer-with-formatted buffer formatted)))
             (funcall then)))))))

(setf *save-preparer* 'format-externally-before-save)

(defun format-lisp-buffer (buffer)
  "Indent all Lisp-family lines in BUFFER as one undo step."
  (indent-buffer-lines buffer 0 (1- (text-line-count (buffer-text buffer)))))

(defun format-buffer-before-save (buffer)
  "Format BUFFER before saving when *FORMAT-ON-SAVE* is enabled, with Cadre's
own formatters (format-externally-before-save runs the external ones)."
  (when *format-on-save*
    (handler-case
        (let ((mode (buffer-major-mode buffer)))
          (cond
            ((external-formatter mode))
            ((eq mode 'json-mode)
             (replace-buffer-with-formatted buffer (format-json (buffer-string buffer) :indent *code-indent-width*)))
            ((eq mode 'css-mode)
             (replace-buffer-with-formatted buffer (format-css (buffer-string buffer) :indent *code-indent-width*)))
            ((eq mode 'xml-mode)
             (replace-buffer-with-formatted buffer (format-xml (buffer-string buffer) :indent *code-indent-width*)))
            ((lisp-family-mode-p mode) (format-lisp-buffer buffer))))
      (error (e)
        (message "Could not format ~a; saving without formatting: ~a" (buffer-name buffer) e)))))

;; This file loads before app.lisp, which registers the whitespace hook after
;; this one. Saves therefore format first and trim any resulting line endings last.
(add-hook '*before-save-hook* #'format-buffer-before-save)

(defun run-external-formatter (view command)
  "Format VIEW's buffer with COMMAND in the background; apply the result if the text hasn't changed meanwhile."
  (let* ((buffer (view-buffer view))
         (text (buffer-string buffer))
         (program (first command)))
    (unless (ignore-errors (uiop:run-program (list "/bin/sh" "-c" (format nil "command -v ~a" program))
                                             :output :string))
      (editor-error "Format Document for ~a uses ~a, which isn't installed~:[~; (npm install -g prettier)~]; *formatter-commands* chooses the program"
                    (major-mode-title (find-major-mode (buffer-major-mode buffer))) program (string= program "prettier")))
    (message "Formatting with ~a…" program)
    (run-formatter-process
     buffer text command
     (lambda (formatted problem)
       (cond (problem
              (panel-log (window-panel *window*) problem)
              (message "~a" problem))
             ((not (string= text (buffer-string buffer)))
              (message "The text changed while it was being formatted; Format Document again"))
             ((member view (buffer-views *window* buffer))
              (replace-with-formatted view formatted)))))))

(define-command format-document ()
  "Lay out the whole file again: JSON, CSS and XML with Cadre's formatters, Lisp
by indenting every line, other languages with *formatter-commands* (Prettier
for JavaScript, TypeScript and HTML)."
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (mode (buffer-major-mode buffer))
         (external (external-formatter mode)))
    (cond (external (run-external-formatter view external))
          ((eq mode 'json-mode)
           (replace-with-formatted view (format-json (buffer-string buffer) :indent *code-indent-width*)))
          ((eq mode 'css-mode)
           (replace-with-formatted view (format-css (buffer-string buffer) :indent *code-indent-width*)))
          ((eq mode 'xml-mode)
           (replace-with-formatted view (format-xml (buffer-string buffer) :indent *code-indent-width*)))
          ((lisp-family-mode-p mode)          ; Lisp, Racket, Clojure: indent every line
           (let ((before (buffer-string buffer)))
             (indent-lines view 0 (1- (gtk:text-buffer-get-line-count (view-gtk-buffer view))))
             (message (if (string= before (buffer-string buffer)) "Already formatted" "Formatted"))))
          (t (editor-error "No formatter for ~a: *formatter-commands* can name one"
                           (major-mode-title (find-major-mode mode)))))))
