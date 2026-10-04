;;;; format.lisp — Format Document
;;;;
;;;; Lays out the whole buffer again, as one undo step, keeping the cursor on
;;;; its line: JSON and CSS with Cadre's own formatters (core format.lisp),
;;;; Lisp by indenting every line, and other languages with an external
;;;; formatter (*formatter-commands*: Prettier for JavaScript, TypeScript and
;;;; HTML), which reads the text on standard input and writes the result.

(in-package #:cadre-ui)

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

(defun replace-with-formatted (view text)
  "Make VIEW's buffer hold TEXT, as one undo step, with the cursor on the same line."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (old (buffer-string (view-buffer view))))
    (if (string= old text)
        (message "Already formatted")
        (multiple-value-bind (line column) (cursor-line-column view)
          (with-user-action (gtk-buffer)
            (gtk:text-buffer-delete gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer) (gtk:text-buffer-get-end-iter gtk-buffer))
            (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer) text -1))
          (let ((line (min line (1- (gtk:text-buffer-get-line-count gtk-buffer)))))
            (goto-line-column view line (min column (length (text-line-string gtk-buffer line))) :extend nil))
          (scroll-to-cursor view)
          (message "Formatted")))))

(defun external-formatter (mode)
  (cdr (assoc mode *formatter-commands*)))

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
  "Lay out the whole file again: JSON and CSS with Cadre's formatters, Lisp
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
          ((eq mode 'lisp-mode)
           (let ((before (buffer-string buffer)))
             (indent-lines view 0 (1- (gtk:text-buffer-get-line-count (view-gtk-buffer view))))
             (message (if (string= before (buffer-string buffer)) "Already formatted" "Formatted"))))
          (t (editor-error "No formatter for ~a: *formatter-commands* can name one"
                           (major-mode-title (find-major-mode mode)))))))
