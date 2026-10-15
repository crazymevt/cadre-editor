;;;; language-repl.lisp — the REPL tabs of Lisps other than Common Lisp
;;;;
;;;; Clojure (clojure.lisp) and Racket (racket.lisp) each have a REPL in a
;;;; tab, made like the editor REPL: what is typed there goes to the
;;;; language's connection through BACKEND-REPL-EVAL, and the connection's
;;;; output, values and namespace changes come back as the events Swank
;;;; sends, which HANDLE-LANGUAGE-EVENT shows.

(in-package #:cadre-ui)

(defstruct (language-repl-spec (:conc-name lrs-))
  mode buffer-name title prompt-format)

(defparameter *language-repl-specs* '()
  "Language → its REPL's spec: the REPL's major mode, its buffer's name, the
language's title, and the prompt (a format control given the connection's
prompt).")

(defun define-language-repl (language &key mode buffer-name title prompt-format)
  (setf *language-repl-specs*
        (cons (cons language (make-language-repl-spec :mode mode :buffer-name buffer-name :title title
                                                      :prompt-format prompt-format))
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

(defun show-language-repl (language &key (focus t))
  (let* ((repl (language-repl language))
         (view (show-buffer *window* (repl-buffer repl))))
    (gtk:text-view-set-wrap-mode (view-text-view view) :word-char)
    (when focus
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
      (:write-string (repl-output (second event) (third event)))
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
       (when (eq (gethash language *connections*) connection)
         (remhash language *connections*))
       (repl-disconnected (second event))
       (update-connection-status)
       (refresh-project-languages)
       (message "~a disconnected: ~a" (lrs-title (language-repl-spec language)) (second event))))))

(defun language-repl-connected (language connection)
  (let ((*repl* (language-repl language)))
    (repl-connected connection)))
