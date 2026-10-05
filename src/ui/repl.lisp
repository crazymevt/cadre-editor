;;;; repl.lisp — the REPL, in the panel
;;;;
;;;; The REPL is a buffer (*repl*, in REPL mode) shown in the panel. Text
;;;; before the input is read-only. Output goes at the OUTPUT mark: just
;;;; before the prompt while idle, at the end while evaluating. RET sends
;;;; the input when it is a complete form; otherwise it starts a new line.

(in-package #:cadre-ui)

(define-major-mode repl-mode (:title "REPL")
  "The REPL of the connected Lisp.")

(defstruct (repl (:conc-name repl-))
  buffer editor-view output-mark input-mark
  (history (make-array 0 :adjustable t :fill-pointer 0))
  (history-index nil)
  (busy nil)
  (reading nil)                         ; (thread tag) while the Lisp reads a line
  evaluator                             ; nil: the connected Lisp; else a function of the input
  prompter)                             ; nil: the connected Lisp's prompt; else a function

(defvar *repl* nil
  "The REPL of the connected Lisp. Commands in another REPL buffer (the
editor REPL) bind it to that buffer's REPL.")

(defmacro with-buffer-repl (() &body body)
  "Run BODY with *REPL* the current buffer's REPL."
  `(let ((*repl* (or (let ((b (current-buffer))) (and b (buffer-local b :repl))) *repl*)))
     ,@body))

(defun make-repl-for-buffer (buffer &key editor-view evaluator prompter)
  "Set up BUFFER (whose text is a gtk:text-buffer) as a REPL."
  (let ((gtk-buffer (buffer-text buffer)))
    (ensure-repl-tags gtk-buffer)
    (attach-syntax buffer)
    (setf (buffer-local buffer :repl)
          (make-repl :buffer buffer :editor-view editor-view :evaluator evaluator :prompter prompter
                     :output-mark (gtk:text-buffer-create-mark gtk-buffer "cadre-output"
                                                               (gtk:text-buffer-get-end-iter gtk-buffer) nil)
                     :input-mark (gtk:text-buffer-create-mark gtk-buffer "cadre-input"
                                                              (gtk:text-buffer-get-end-iter gtk-buffer) t)))))

(defun repl-gtk-buffer () (buffer-text (repl-buffer *repl*)))

(defun ensure-repl-tags (gtk-buffer)
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (unless (gtk:text-tag-table-lookup table "cadre-repl-readonly")
      (gtk:text-tag-table-add table (make-instance 'gtk:text-tag :name "cadre-repl-readonly" :editable nil)))
    (ensure-face-tag gtk-buffer "cadre-repl-prompt" :repl-prompt)
    (ensure-face-tag gtk-buffer "cadre-repl-result" :repl-result)
    (ensure-face-tag gtk-buffer "cadre-repl-note" :repl-note)))

(defun make-repl-widget ()
  "Create the REPL (once) and return the widget for the panel's REPL page."
  (let* ((buffer (make-buffer :name "*repl*" :text (make-gtk-text) :major-mode 'repl-mode))
         (view (make-editor-view buffer :gutter nil)))
    (setf *repl* (make-repl-for-buffer buffer :editor-view view))
    (setup-context-menu view)
    (setup-presentation-clicks view)
    (gtk:text-view-set-wrap-mode (view-text-view view) :word-char)
    (repl-insert (format nil "; Not connected. Evaluate something, or press ~a, to start a Lisp.~%"
                         "the ● button below")
                 "cadre-repl-note")
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :spacing 6 :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2
          (command-button "media-playback-stop-symbolic" "Interrupt the evaluation" 'interrupt-lisp)
          (command-button "edit-clear-all-symbolic" "Clear the REPL" 'clear-repl)
          (command-button "view-refresh-symbolic" "Restart the Lisp" 'restart-lisp)
          (command-button "document-open-symbolic" "Load the project's ASDF system" 'load-project))
        (view-widget view)))))

(defun repl-view ()
  (and *repl* (or (repl-editor-view *repl*)
                  (and *window* (first (buffer-views *window* (repl-buffer *repl*)))))))

(defun mark-iter (mark)
  (gtk:text-buffer-get-iter-at-mark (repl-gtk-buffer) mark))

(defun repl-insert (string &rest tags)
  "Insert STRING at the output mark, read-only, with TAGS."
  (let* ((gtk-buffer (repl-gtk-buffer))
         (start-offset (gtk:text-iter-get-offset (mark-iter (repl-output-mark *repl*)))))
    (gtk:text-buffer-insert gtk-buffer (mark-iter (repl-output-mark *repl*)) string -1)
    (let ((start (iter-at gtk-buffer start-offset))
          (end (mark-iter (repl-output-mark *repl*))))
      (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-repl-readonly" start end)
      (dolist (tag tags) (gtk:text-buffer-apply-tag-by-name gtk-buffer tag start end)))
    (repl-scroll-to-end)))

(defun repl-scroll-to-end ()
  ;; The buffer is found now: by the time the idle runs, *REPL* may be
  ;; another REPL (the editor REPL binds it while it works).
  (let ((view (repl-view))
        (gtk-buffer (repl-gtk-buffer)))
    (when view
      (glib:idle-add glib:+priority-default-idle+
                     (lambda ()
                       (when (eq (gtk:text-view-get-buffer (view-text-view view)) gtk-buffer)
                         (gtk:text-view-scroll-to-iter (view-text-view view)
                                                       (gtk:text-buffer-get-end-iter gtk-buffer)
                                                       0d0 nil 0d0 0d0))
                       nil)))))

(defun at-line-start-p (iter)
  (gtk:text-iter-starts-line iter))

(defun repl-fresh-line ()
  (unless (at-line-start-p (mark-iter (repl-output-mark *repl*)))
    (repl-insert (string #\Newline))))

(defun repl-show-prompt ()
  "Insert a prompt at the end; output then goes before it, input after it."
  (let ((gtk-buffer (repl-gtk-buffer)))
    (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-output-mark *repl*) (gtk:text-buffer-get-end-iter gtk-buffer))
    (repl-fresh-line)
    (let ((prompt (cond ((repl-prompter *repl*) (funcall (repl-prompter *repl*)))
                        (t (format nil "~a> " (if (connected-p) (connection-prompt *connection*) "CL-USER")))))
          (start (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter gtk-buffer))))
      (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) prompt -1)
      (let ((prompt-start (iter-at gtk-buffer start))
            (end (gtk:text-buffer-get-end-iter gtk-buffer)))
        (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-repl-readonly" prompt-start end)
        (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-repl-prompt" prompt-start end)
        (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-output-mark *repl*) prompt-start)
        (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-input-mark *repl*) end)
        (gtk:text-buffer-place-cursor gtk-buffer end)))
    (setf (buffer-modified-p (repl-buffer *repl*)) nil)
    (repl-scroll-to-end)))

(defun repl-input ()
  (let ((gtk-buffer (repl-gtk-buffer)))
    (gtk:text-buffer-get-text gtk-buffer (mark-iter (repl-input-mark *repl*))
                              (gtk:text-buffer-get-end-iter gtk-buffer) t)))

(defun set-repl-input (string)
  (let ((gtk-buffer (repl-gtk-buffer)))
    (gtk:text-buffer-delete gtk-buffer (mark-iter (repl-input-mark *repl*)) (gtk:text-buffer-get-end-iter gtk-buffer))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) string -1)
    (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer))))

(defun complete-form-p (string)
  "True if STRING holds at least one form and no unclosed list, string or comment."
  (let ((state '(:code 0)) (any nil))
    (dolist (line (uiop:split-string string :separator '(#\Newline)))
      (multiple-value-bind (tokens end) (lex-line line state)
        (when (find-if-not (lambda (tk) (eq (token-type tk) :comment)) tokens) (setf any t))
        (setf state end)))
    (and any (equal state '(:code 0)))))

(defun repl-freeze-input ()
  "Make the input read-only and move output to the end."
  (let ((gtk-buffer (repl-gtk-buffer)))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) (string #\Newline) -1)
    (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-repl-readonly"
                                       (mark-iter (repl-input-mark *repl*)) (gtk:text-buffer-get-end-iter gtk-buffer))
    (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-output-mark *repl*) (gtk:text-buffer-get-end-iter gtk-buffer))
    (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-input-mark *repl*) (gtk:text-buffer-get-end-iter gtk-buffer))))

(defun repl-eval (string &key on-done)
  "Evaluate STRING in the REPL, as if typed. ON-DONE is called once it
returns or is aborted."
  (when (repl-evaluator *repl*)
    (return-from repl-eval (funcall (repl-evaluator *repl*) string)))
  (with-connection (connection)
    (setf (repl-busy *repl*) t)
    (rex connection (swank-call "swank-repl:listener-eval" (gtk-thread-source string))
         :thread :repl-thread
         :on-ok (lambda (value)
                  (declare (ignore value))
                  (setf (repl-busy *repl*) nil)
                  (repl-show-prompt)
                  (image-changed)
                  (when on-done (funcall on-done)))
         :on-abort (lambda (reason)
                     (setf (repl-busy *repl*) nil)
                     (repl-fresh-line)
                     (repl-insert (format nil "; Evaluation aborted~@[: ~a~]~%" reason) "cadre-repl-note")
                     (repl-show-prompt)
                     (when on-done (funcall on-done))))))

;;; Following the code's package: evaluating or compiling code from a file
;;; switches the REPL to that code's package, so what was just defined can be
;;; named in the REPL without a prefix.

(define-option *repl-follows-buffer-package* t boolean
  "When code from a file is evaluated or compiled, switch the REPL to the
package of that code (from the file's IN-PACKAGE)."
  :category "Lisp")

(defun same-package-name-p (a b)
  (string-equal (string-left-trim ":#" a) (string-left-trim ":#" b)))

(defun repl-follow-package (package)
  "Switch the connected Lisp's REPL to PACKAGE (a name), if it is idle."
  (when (and *repl-follows-buffer-package* package *repl* (connected-p)
             (null (repl-evaluator *repl*))
             (not (repl-busy *repl*)) (not (repl-reading *repl*))
             (not (same-package-name-p package (connection-package *connection*))))
    (let ((repl *repl*))
      (rex *connection* (swank-call "swank:set-package" package)
           :thread :repl-thread
           :on-ok (lambda (reply)
                    (destructuring-bind (name prompt) reply
                      (let ((changed (not (same-package-name-p name (connection-package *connection*)))))
                        (setf (connection-package *connection*) name
                              (connection-prompt *connection*) prompt)
                        (update-connection-status)
                        (let ((*repl* repl))
                          (when (and changed (not (repl-busy *repl*)) (not (repl-reading *repl*)))
                            (repl-replace-prompt))))))
           ;; The package may not exist yet: the file isn't loaded.
           :on-abort (lambda (reason) (declare (ignore reason)))))))

(defun repl-replace-prompt ()
  "Note the new package and show a fresh prompt, keeping what was typed."
  (let ((input (repl-input))
        (gtk-buffer (repl-gtk-buffer)))
    (set-repl-input "")
    (gtk:text-buffer-move-mark gtk-buffer (repl-output-mark *repl*) (gtk:text-buffer-get-end-iter gtk-buffer))
    (repl-fresh-line)
    (repl-insert (format nil "; Package ~a, the evaluated code's~%" (connection-package *connection*))
                 "cadre-repl-note")
    (repl-show-prompt)
    (when (plusp (length input)) (set-repl-input input))))

;;; From the Lisp

(defun repl-output (string target)
  (when *repl*
    ;; While idle, output goes just before the prompt: keep the prompt on a
    ;; line of its own.
    (let ((string (if (and (not (repl-busy *repl*))
                           (plusp (length string))
                           (char/= (char string (1- (length string))) #\Newline)
                           (not (gtk:text-iter-is-end (mark-iter (repl-output-mark *repl*)))))
                      (format nil "~a~%" string)
                      string)))
      (if (eq target :repl-result)
          (repl-insert string "cadre-repl-result")
          (repl-insert string)))))

(defun repl-connected (connection)
  (repl-fresh-line)
  (repl-insert (format nil "; Connected to ~a (~a:~d)~%" (connection-implementation connection)
                       (connection-host connection) (connection-port connection))
               "cadre-repl-note")
  (unless (repl-busy *repl*) (repl-show-prompt)))

(defun repl-disconnected (reason)
  (when *repl*
    (setf (repl-busy *repl*) nil (repl-reading *repl*) nil)
    (repl-fresh-line)
    (repl-insert (format nil "; Disconnected: ~a~%" reason) "cadre-repl-note")))

(defun repl-read-string (connection thread tag)
  "The Lisp wants a line of input: the next RET sends it."
  (declare (ignore connection))
  (setf (repl-reading *repl*) (list thread tag))
  (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-input-mark *repl*) (gtk:text-buffer-get-end-iter (repl-gtk-buffer)))
  (show-repl-page :focus t))

(defun repl-read-aborted ()
  (setf (repl-reading *repl*) nil))

;;; Commands

(define-command repl-return ()
  "Send the input if it is complete; otherwise start a new line."
  (:modes repl-mode editor-repl-mode)
  (with-buffer-repl ()
   (let ((input (repl-input)))
    (cond
      ((repl-reading *repl*)
       (destructuring-bind (thread tag) (repl-reading *repl*)
         (setf (repl-reading *repl*) nil)
         (repl-freeze-input)
         (swank-send *connection* (list :emacs-return-string thread tag (format nil "~a~%" input)))))
      ((repl-busy *repl*)
       (message "The Lisp is busy; interrupt it with the stop button"))
      ((complete-form-p input)
       (let ((history (repl-history *repl*))
             ;; Results copied into the input go to the Lisp as the objects themselves.
             (for-lisp (if (repl-evaluator *repl*) input (repl-input-for-lisp))))
         (ensure-repl-history-loaded)
         (when (or (zerop (length history)) (string/= input (aref history (1- (length history)))))
           (vector-push-extend input history)
           (save-repl-history))
         (setf (repl-history-index *repl*) nil)
         (repl-freeze-input)
         (repl-eval for-lisp)))
      (t (gtk:text-buffer-insert-at-cursor (repl-gtk-buffer) (string #\Newline) -1))))))

(defun repl-history-step (delta)
  "Show the input DELTA steps back (negative) or on in the history. Going on
past the newest brings back what was being typed before."
  (ensure-repl-history-loaded)
  (let* ((history (repl-history *repl*))
         (n (length history))
         (buffer (repl-buffer *repl*)))
    (when (plusp n)
      (unless (repl-history-index *repl*)
        (setf (buffer-local buffer :history-draft) (repl-input)))
      (let ((index (if (repl-history-index *repl*)
                       (+ (repl-history-index *repl*) delta)
                       (if (minusp delta) (1- n) n))))
        (cond ((>= index n)
               (setf (repl-history-index *repl*) nil)
               (set-repl-input (or (buffer-local buffer :history-draft) "")))
              (t (setf index (max 0 index)
                       (repl-history-index *repl*) index)
                 (set-repl-input (aref history index))))))))

(define-command repl-previous-input ()
  "Replace the input with the previous one from the history."
  (:modes repl-mode editor-repl-mode)
  (with-buffer-repl () (repl-history-step -1)))

(define-command repl-next-input ()
  "Replace the input with the next one from the history."
  (:modes repl-mode editor-repl-mode)
  (with-buffer-repl () (repl-history-step 1)))

(define-command clear-repl ()
  "Clear the REPL's output."
  (with-buffer-repl ()
   (when *repl*
    (let ((gtk-buffer (repl-gtk-buffer)))
      (forget-presentations)
      (gtk:text-buffer-delete gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer)
                              (mark-iter (repl-output-mark *repl*)))))))

(defun show-repl-page (&key focus)
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "repl")
  (when (and focus (repl-view))
    (focus-view (repl-view))))

(define-command show-repl ()
  "Show the REPL and put the cursor in it, starting a Lisp if needed."
  (show-repl-page :focus t)
  (unless (or (connected-p) *connecting*) (start-lisp)))
