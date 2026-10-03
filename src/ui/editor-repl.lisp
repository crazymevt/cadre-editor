;;;; editor-repl.lisp — a REPL in Cadre's own image, for extending the editor
;;;;
;;;; The *cadre-repl* buffer evaluates in the image Cadre runs in, in the
;;;; CADRE-USER package, on the GUI thread, so it can call any editor
;;;; function: (cadre-ui::current-view), define commands, bind keys. A form
;;;; that never returns freezes the editor, as it would in Emacs. Errors
;;;; print their condition and a short backtrace instead of entering a
;;;; debugger. M-: (eval-expression) evaluates one form the same way.

(in-package #:cadre-ui)

(define-major-mode editor-repl-mode (:title "Cadre REPL")
  "A REPL in Cadre's own image.")

;; Keys not bound here come from the REPL's keymap.
(setf (cadre::keymap-parent (major-mode-keymap (find-major-mode 'editor-repl-mode)))
      (major-mode-keymap (find-major-mode 'repl-mode)))

(defvar *editor-repl-package* (find-package :cadre-user))
(defvar *editor-repl* nil "The editor REPL's buffer, once made.")

(defun editor-repl-prompt ()
  (format nil "~a> " (package-name *editor-repl-package*)))

(defun eval-in-editor (string)
  "Read and evaluate the forms in STRING in Cadre's image. Returns the
output, the last form's values (a list), and the error (or nil) with a
backtrace as a string."
  (let ((output (make-string-output-stream))
        (values '())
        (error nil)
        (backtrace nil))
    (block eval
      (handler-bind ((error (lambda (e)
                              (setf error e
                                    backtrace (with-output-to-string (s)
                                                (ignore-errors (sb-debug:print-backtrace :stream s :count 25))))
                              (return-from eval))))
        (let ((*standard-output* output)
              (*error-output* output)
              (*trace-output* output)
              (*package* *editor-repl-package*))
          (with-input-from-string (in string)
            (loop with eof = in
                  for form = (read in nil eof)
                  until (eq form eof)
                  do (setf values (multiple-value-list (eval form)))
                     (shiftf *** ** * (first values))
                     (shiftf /// // / values)
                     (shiftf +++ ++ + form)))
          (setf *editor-repl-package* *package*))))
    (values (get-output-stream-string output) values error backtrace)))

(defun editor-repl-evaluate (input)
  (multiple-value-bind (output values error backtrace) (eval-in-editor input)
    (when (plusp (length output))
      (repl-insert output))
    (repl-fresh-line)
    (if error
        (progn
          (repl-insert (format nil "; Error: ~a~%" error) "cadre-repl-note")
          (repl-insert (format nil "~{; ~a~%~}" (subseq-lines (backtrace-from-signal backtrace) 10)) "cadre-repl-note"))
        (repl-insert (if values
                         (format nil "~{~s~^~%~}~%" values)
                         (format nil "; No values~%"))
                     "cadre-repl-result"))
    (repl-show-prompt)))

(defun backtrace-from-signal (backtrace)
  "BACKTRACE without the frames of the handler that printed it."
  (let ((at (search "%SIGNAL" backtrace)))
    (if at
        (subseq backtrace (min (length backtrace) (1+ (or (position #\Newline backtrace :start at) (length backtrace)))))
        backtrace)))

(defun subseq-lines (string n)
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) string) :separator '(#\Newline))))
    (subseq lines 0 (min n (length lines)))))

(defun ensure-editor-repl ()
  (or (and *editor-repl* (member *editor-repl* (buffer-list)) *editor-repl*)
      (let ((buffer (make-buffer :name "*cadre-repl*" :text (make-gtk-text) :major-mode 'editor-repl-mode)))
        (setf *editor-repl* buffer)
        (let ((*repl* (make-repl-for-buffer buffer :evaluator 'editor-repl-evaluate
                                                   :prompter 'editor-repl-prompt)))
          (repl-insert (format nil "; Cadre's own image. Forms here run in the editor, in ~a.~%; (cadre-ui::current-view), (cadre:list-commands), define-command and bind-key all work.~%"
                               (package-name *editor-repl-package*))
                       "cadre-repl-note")
          (repl-show-prompt))
        buffer)))

(define-command editor-repl ()
  "Open a REPL in Cadre's own image, for trying out and extending the editor."
  (let* ((buffer (ensure-editor-repl))
         (view (show-buffer *window* buffer)))
    (gtk:text-view-set-wrap-mode (view-text-view view) :word-char)
    (let ((gtk-buffer (buffer-text buffer)))
      (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer)))))

(define-command eval-expression ()
  "Evaluate a form in Cadre's own image and show the result."
  (open-picker (window-picker *window*)
               :placeholder (format nil "Eval in Cadre (~a):" (package-name *editor-repl-package*))
               :on-choose (lambda (text)
                            (multiple-value-bind (output values error) (eval-in-editor text)
                              (when (plusp (length output)) (panel-log (window-panel *window*) output))
                              (if error
                                  (message "Error: ~a" error)
                                  (message "~{~s~^, ~}" values))))))

;;; Completion from the editor's image

(defun editor-symbol-completions (prefix)
  (let ((results '())
        (upper (string-upcase prefix)))
    (do-symbols (symbol *editor-repl-package*)
      (let ((name (symbol-name symbol)))
        (when (and (>= (length name) (length upper)) (string= upper name :end2 (length upper)))
          (pushnew (string-downcase name) results :test #'string=))))
    (sort results #'string<)))

(define-command editor-complete-symbol ()
  "Complete the symbol before the cursor from the symbols Cadre's image knows."
  (:modes editor-repl-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (point (point-offset view))
         (start (loop for i downfrom point
                      while (and (> i 0) (symbol-constituent-p (text-char gtk-buffer (1- i))))
                      finally (return i)))
         (prefix (text-string gtk-buffer start point))
         (candidates (and (plusp (length prefix)) (editor-symbol-completions prefix))))
    (cond ((null candidates) (message "No completions for ~a" prefix))
          ((null (rest candidates)) (replace-text-between gtk-buffer start point (first candidates)))
          (t (open-picker (window-picker *window*)
                          :items candidates :placeholder (format nil "Complete ~a" prefix)
                          :on-choose (lambda (c) (replace-text-between gtk-buffer start point c)))))))

(bind-key (major-mode-keymap (find-major-mode 'editor-repl-mode)) "TAB" 'editor-complete-symbol)
