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

(defun external-format-buffer-text (buffer command)
  "Return the formatted text and success flag for BUFFER using COMMAND."
  (let* ((text (buffer-string buffer))
         (file (if (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)) (buffer-name buffer)))
         (program (first command))
         (arguments (mapcar (lambda (a) (cl-ppcre:regex-replace-all "\\{file\\}" a file)) (rest command))))
    (handler-case
        (multiple-value-bind (output error code)
            (with-input-from-string (in text)
              (uiop:run-program (cons program arguments) :input in :output :string :error-output :string
                                                        :ignore-error-status t
                                                        :directory (and (buffer-file buffer)
                                                                        (uiop:pathname-directory-pathname
                                                                         (buffer-file buffer)))))
          (if (zerop code)
              (values output t)
              (progn
                (message "~a couldn't format it; saving without formatting: ~a"
                         program (string-trim '(#\Newline) error))
                (values nil nil))))
      (error (e)
        (message "Could not run formatter ~a; saving without formatting: ~a" program e)
        (values nil nil)))))

(defun format-lisp-buffer (buffer)
  "Indent all Lisp-family lines in BUFFER as one undo step."
  (let* ((gtk-buffer (buffer-text buffer))
         (syntax (buffer-sexp-syntax buffer)))
    (with-user-action (gtk-buffer)
      (loop for line below (text-line-count gtk-buffer)
            for column = (lisp-indentation syntax line)
            when (and column (plusp (length (string-trim '(#\Space #\Tab)
                                                         (text-line-string gtk-buffer line)))))
              do (indent-line-to gtk-buffer line column)))))

(defun format-buffer-before-save (buffer)
  "Format BUFFER before saving when *FORMAT-ON-SAVE* is enabled."
  (when *format-on-save*
    (handler-case
        (let* ((mode (buffer-major-mode buffer))
               (external (external-formatter mode)))
          (cond
            (external
             (multiple-value-bind (text ok) (external-format-buffer-text buffer external)
               (when ok (replace-buffer-with-formatted buffer text))))
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
  "Format VIEW's buffer with COMMAND on a thread; apply the result if the text hasn't changed meanwhile."
  (let* ((buffer (view-buffer view))
         (text (buffer-string buffer))
         (file (if (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)) (buffer-name buffer)))
         (program (first command))
         (arguments (mapcar (lambda (a) (cl-ppcre:regex-replace-all "\\{file\\}" a file)) (rest command))))
    (unless (ignore-errors (uiop:run-program (list "/bin/sh" "-c" (format nil "command -v ~a" program))
                                             :output :string))
      (editor-error "Format Document for ~a uses ~a, which isn't installed~:[~; (npm install -g prettier)~]; *formatter-commands* chooses the program"
                    (major-mode-title (find-major-mode (buffer-major-mode buffer))) program (string= program "prettier")))
    (message "Formatting with ~a…" program)
    (sb-thread:make-thread
     (lambda ()
       (multiple-value-bind (output error code)
           (with-input-from-string (in text)
             (uiop:run-program (cons program arguments) :input in :output :string :error-output :string
                                                       :ignore-error-status t
                                                       :directory (and (buffer-file buffer)
                                                                       (uiop:pathname-directory-pathname (buffer-file buffer)))))
         (glib:call-in-main-thread
          (lambda ()
            (cond ((not (zerop code))
                   (panel-log (window-panel *window*) (format nil "~a: ~a" program (string-trim '(#\Newline) error)))
                   (message "~a couldn't format it: ~a" program
                            (let ((first-line (subseq error 0 (or (position #\Newline error) (length error)))))
                              first-line)))
                  ((not (string= text (buffer-string buffer)))
                   (message "The text changed while it was being formatted; Format Document again"))
                  ((member view (buffer-views *window* buffer))
                   (replace-with-formatted view output)))))))
     :name "cadre format")))

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
