;;;; language-repl.lisp — the REPLs of Lisps other than Common Lisp
;;;;
;;;; Clojure (clojure.lisp) and Racket (racket.lisp) each have a REPL, made
;;;; like the editor REPL: what is typed there goes to the language's
;;;; connection through BACKEND-REPL-EVAL, and the connection's output,
;;;; values and namespace changes come back as the events Swank sends, which
;;;; HANDLE-LANGUAGE-EVENT shows. It is on the panel's REPL page, which shows
;;;; the REPL of the selected tab's language (repl.lisp's SHOW-REPL-LANGUAGE).

(in-package #:cadre-ui)

(defstruct (language-repl-spec (:conc-name lrs-))
  mode buffer-name title prompt-format restart-command)

(defparameter *language-repl-specs* '()
  "Language → its REPL's spec: the REPL's major mode, its buffer's name, the
language's title, and the prompt (a format control given the connection's
prompt).")

(defun define-language-repl (language &key mode buffer-name title prompt-format restart-command)
  (setf *language-repl-specs*
        (cons (cons language (make-language-repl-spec :mode mode :buffer-name buffer-name :title title
                                                      :prompt-format prompt-format
                                                      :restart-command restart-command))
              (remove language *language-repl-specs* :key #'car))))

(defun language-repl-spec (language)
  (or (cdr (assoc language *language-repl-specs*)) (error "No REPL for ~s" language)))

(defvar *language-repl-buffers* (make-hash-table) "Language → its REPL tab's buffer.")

(defun language-repl-prompt (language)
  (let ((connection (language-connection language)))
    (format nil (lrs-prompt-format (language-repl-spec language))
            (if connection (connection-prompt connection) (lrs-title (language-repl-spec language))))))

(defun language-repl (language)
  "LANGUAGE's REPL struct, made with its buffer the first time."
  (let ((buffer (gethash language *language-repl-buffers*))
        (spec (language-repl-spec language)))
    (unless (and buffer (member buffer (buffer-list)))
      (setf buffer (make-buffer :name (lrs-buffer-name spec) :text (make-gtk-text) :major-mode (lrs-mode spec))
            (gethash language *language-repl-buffers*) buffer)
      (let ((*repl* (make-repl-for-buffer buffer
                                          :evaluator (lambda (input) (language-repl-evaluate language input))
                                          :prompter (lambda () (language-repl-prompt language)))))
        (repl-insert (format nil "; ~a. Forms typed here go to the connected ~:*~a.~%" (lrs-title spec))
                     "cadre-repl-note")))
    (buffer-local buffer :repl)))

(defun language-repl-evaluate (language input)
  (let ((repl (language-repl language)))
    (with-connection (connection :language language)
      (setf (repl-busy repl) t)
      (request #'backend-repl-eval connection input
               :on-ok (lambda (value)
                        (declare (ignore value))
                        (let ((*repl* repl))
                          (setf (repl-busy *repl*) nil)
                          (repl-show-prompt)))
               :on-abort (lambda (reason)
                           (let ((*repl* repl))
                             (setf (repl-busy *repl*) nil)
                             (repl-fresh-line)
                             (repl-insert (format nil "; Evaluation aborted~@[: ~a~]~%" reason) "cadre-repl-note")
                             (repl-show-prompt)))))))

(defun make-language-repl-widget (language)
  "LANGUAGE's REPL for the REPL page (repl.lisp): its view, and buttons that
interrupt, clear and restart it."
  (let* ((repl (language-repl language))
         (spec (language-repl-spec language))
         (view (make-editor-view (repl-buffer repl) :gutter nil)))
    (setf (repl-editor-view repl) view)
    (setup-context-menu view)
    (gtk:text-view-set-wrap-mode (view-text-view view) :word-char)
    (flet ((button (icon tooltip action)
             (let ((b (make-instance 'gtk:button :icon-name icon :tooltip-text tooltip :css-classes '("flat"))))
               (gobject:connect b :clicked (lambda (b) (declare (ignore b))
                                             (handler-case (funcall action)
                                               (editor-error (e) (message "~a" (editor-error-message e))))))
               b)))
      (gtk:build
        (gtk:box :orientation :vertical
          (gtk:box :spacing 6 :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2
            (button "media-playback-stop-symbolic" (format nil "Interrupt the ~a evaluation" (lrs-title spec))
                    (lambda ()
                      (let ((connection (language-connection language)))
                        (if connection
                            (progn (backend-interrupt connection) (message "Interrupted"))
                            (message "~a isn't running" (lrs-title spec))))))
            (button "edit-clear-all-symbolic" (format nil "Clear the ~a REPL" (lrs-title spec))
                    (lambda () (let ((*repl* repl)) (clear-repl-output))))
            (button "view-refresh-symbolic" (format nil "Restart ~a" (lrs-title spec))
                    (lambda () (call-command (lrs-restart-command spec)))))
          (view-widget view))))))

(defun language-repl-views ()
  "The views of the Racket and Clojure REPLs made for the REPL page."
  (loop for buffer being the hash-values of *language-repl-buffers*
        for repl = (buffer-local buffer :repl)
        when (and repl (repl-editor-view repl)) collect (repl-editor-view repl)))

(defun show-language-repl (language &key (focus t))
  "Show LANGUAGE's REPL on the panel's REPL page; with FOCUS, put the cursor at its end."
  (show-repl-language language)
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "repl")
  (let ((view (repl-editor-view (language-repl language))))
    (when (and view focus)
      (gtk:text-buffer-place-cursor (view-gtk-buffer view) (gtk:text-buffer-get-end-iter (view-gtk-buffer view)))
      (focus-view view))
    view))

(defun language-repl-note (language text)
  "Show TEXT as a note in LANGUAGE's REPL where its prompt was, then a new
prompt (another module's or namespace's, perhaps), keeping what is typed there."
  (let ((*repl* (language-repl language)))
    (unless (repl-busy *repl*)
      (let ((input (repl-input))
            (gtk-buffer (repl-gtk-buffer)))
        (gtk:text-buffer-delete gtk-buffer (mark-iter (repl-output-mark *repl*)) (gtk:text-buffer-get-end-iter gtk-buffer))
        (repl-fresh-line)
        (repl-insert text "cadre-repl-note")
        (repl-show-prompt)
        (when (plusp (length input)) (set-repl-input input))))))

(defun handle-language-event (language connection event)
  "Show EVENT (Swank-shaped) from LANGUAGE's CONNECTION."
  (let ((*repl* (language-repl language)))
    (case (first event)
      (:write-string (capture-project-run-output connection (second event))
                      (repl-output (second event) (third event)))
      (:post-mortem (destructuring-bind (text type frames) (rest event)
                      (debugger-post-mortem connection text type frames)))
      (:read-string
       (repl-start-reading connection (second event) (third event))
       (show-language-repl language)
       (message "The program is reading input: type a line in the ~a REPL" (lrs-title (language-repl-spec language))))
      (:new-package
       (setf (connection-package connection) (second event)
             (connection-prompt connection) (third event))
       (language-repl-note language (format nil "; In ~a~%" (third event)))
       (update-connection-status))
      (:disconnected
       (disconnect-project-runs connection (second event))
       (tests-disconnected connection)
       (when (eq connection (getf *language-app* :connection))
         (language-app-finished "Program disconnected"))
       (when (eq (gethash language *connections*) connection)
         (remhash language *connections*))
       (repl-disconnected (second event))
       (update-connection-status)
       (refresh-project-languages)
       (message "~a disconnected: ~a" (lrs-title (language-repl-spec language)) (second event))))))

(defun language-repl-connected (language connection)
  (let ((*repl* (language-repl language)))
    (repl-connected connection)))
