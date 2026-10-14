;;;; backend.lisp — Racket, through Cadre's own back end (vendor/cadre-racket/)
;;;;
;;;; The back end is a Racket program that frames its messages as Swank
;;;; does, so a RACKET-CONNECTION is a FRAMED-CONNECTION: requests are
;;;; (:emacs-rex (:op arg …) package thread id), and output, REPL values and
;;;; module changes arrive as Swank's events. Its answers are plain data,
;;;; which the methods here put in the shapes backend.lisp describes.
;;;;
;;;; A Racket "package" is the path of a module file: code from a file is
;;;; evaluated in its module's namespace, which the back end makes by running
;;;; the module the first time. Definitions, references, documentation and
;;;; problems come from drracket/check-syntax on the buffer's text, so they
;;;; need neither a saved file nor a run module.

(in-package #:cadre)

(define-option *racket-command* "racket" string
  "The Racket program, for running Racket code."
  :category "Racket")

(define-option *raco-command* "raco" string
  "Racket's raco, for raco make and raco test."
  :category "Racket")

(define-option *racket-startup-timeout* 60 (integer 1)
  "Seconds to wait for Racket to start Cadre's back end."
  :category "Racket")

(defclass racket-connection (framed-connection)
  ((implementation :initform "Racket" :accessor racket-implementation))
  (:default-initargs :package "racket" :prompt "racket")
  (:documentation "A connection to Cadre's Racket back end."))

(defmethod connection-language ((connection racket-connection)) :racket)

(defmethod connection-implementation ((connection racket-connection))
  (racket-implementation connection))

(defmethod backend-supports-p ((connection racket-connection) feature)
  (member feature '(:eval :compile :compile-file :load-file :describe :arglist :autodoc :completions
                    :definitions :xref :macroexpand :repl :interrupt)))

(defun racket-backend-file ()
  (resource-pathname "vendor/cadre-racket/main.rkt"))

(defun start-racket (directory &key (deliver #'funcall) on-output on-port on-exit)
  "Start Racket with Cadre's back end in DIRECTORY. Calls, through DELIVER:
ON-OUTPUT with each line Racket prints, ON-PORT with the back end's port,
and ON-EXIT with a message if it exits first or takes too long."
  (start-inferior-lisp :command (list *racket-command*)
                       :arguments (lambda (port-file)
                                    (list (uiop:native-namestring (racket-backend-file))
                                          "--port-file" (uiop:native-namestring port-file)))
                       :directory directory :server "Cadre's Racket back end"
                       :timeout *racket-startup-timeout*
                       :deliver deliver :on-output on-output :on-port on-port :on-exit on-exit))

(defun racket-connect (host port &key (deliver #'funcall) handler on-ready on-failure)
  "Connect to the Racket back end at HOST:PORT; call ON-READY with the
connection once it has said what it is."
  (let ((connection (swank-connect host port :deliver deliver :handler handler :class 'racket-connection)))
    (swank-rex connection '(:connection-info)
               :on-abort (or on-failure #'default-abort)
               :on-ok (lambda (info)
                        (setf (racket-implementation connection) (getf info :implementation)
                              (connection-package connection) (getf info :package)
                              (connection-prompt connection) (getf info :prompt))
                        (when on-ready (funcall on-ready connection))))
    connection))

;;; Requests

(defun racket-request (connection op arguments package on-ok on-abort)
  "Send (OP . ARGUMENTS) to be done with PACKAGE's namespace (nil: the REPL's)."
  (swank-rex connection (cons op arguments) :package package :thread t
                                            :on-ok (or on-ok #'identity) :on-abort (or on-abort #'default-abort)))

(defun racket-values-text (values)
  (format nil "=> ~:[; No value~;~:*~{~a~^, ~}~]" values))

(defmethod backend-eval ((connection racket-connection) string &key package on-ok on-abort)
  (racket-request connection :eval (list string) package
                  (lambda (values) (when on-ok (funcall on-ok (racket-values-text values))))
                  on-abort))

(defmethod backend-eval-region ((connection racket-connection) string &key package on-ok on-abort)
  (backend-eval connection string :package package :on-ok on-ok :on-abort on-abort))

(defun racket-compilation-result (result start fallback-location)
  "A Swank-shaped compilation result from the back end's (:ok) or (:error
message file line column)."
  (let ((failed (eq (first result) :error)))
    (list :compilation-result
          (and failed
               (destructuring-bind (message &optional file line column) (rest result)
                 (list (list :severity :error :message message
                             :location (if (and file line (uiop:absolute-pathname-p file))
                                           (list :location (list :file file) (list :line line (or column 0)) nil)
                                           fallback-location)))))
          (not failed)
          (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)
          nil nil)))

(defmethod backend-compile-form ((connection racket-connection) string
                                 &key buffer-name position line column file package on-ok on-abort
                                 &allow-other-keys)
  (let ((start (get-internal-real-time)))
    (racket-request connection :compile-form
                    ;; Racket counts columns from 0.
                    (list string file line (and column (max 0 (1- column))) position)
                    package
                    (lambda (result)
                      (when on-ok
                        (funcall on-ok (racket-compilation-result
                                        result start
                                        (list :location (list :buffer buffer-name)
                                              (list :position (or position 1)) nil)))))
                    on-abort)))

(defun racket-run-file (connection path on-done on-abort)
  "Run the module in the file at PATH and enter it: ON-DONE gets the back end's result."
  (racket-request connection :run-file (list path) nil on-done on-abort))

(defmethod backend-compile-file ((connection racket-connection) path &key on-ok on-abort on-loaded &allow-other-keys)
  ;; Running a module compiles and loads it.
  (let ((start (get-internal-real-time)))
    (racket-run-file connection path
                     (lambda (result)
                       (when on-ok
                         (funcall on-ok (racket-compilation-result
                                         result start (list :location (list :file path) (list :line 1 0) nil))))
                       (when (and on-loaded (eq (first result) :ok)) (funcall on-loaded)))
                     on-abort)))

(defmethod backend-load-file ((connection racket-connection) path &key on-ok on-abort &allow-other-keys)
  (racket-run-file connection path
                   (lambda (result)
                     (if (eq (first result) :error)
                         (when on-abort (funcall on-abort (second result)))
                         (when on-ok (funcall on-ok path))))
                   on-abort))

(defmethod backend-repl-eval ((connection racket-connection) string &key on-ok on-abort &allow-other-keys)
  (swank-rex connection (list :repl-eval string) :package nil :thread :repl-thread
                                                 :on-ok (lambda (v) (declare (ignore v)) (when on-ok (funcall on-ok nil)))
                                                 :on-abort (or on-abort #'default-abort)))

(defmethod backend-set-package ((connection racket-connection) name &key on-ok on-abort)
  (racket-request connection :set-package (list name) nil
                  (lambda (reply)
                    (destructuring-bind (package prompt) reply
                      (setf (connection-package connection) package (connection-prompt connection) prompt)
                      (when on-ok (funcall on-ok reply))))
                  on-abort))

(defmethod backend-interrupt ((connection racket-connection))
  (swank-interrupt connection))

(defmethod backend-disconnect ((connection racket-connection))
  (swank-disconnect connection))

;;; Looking things up

(defmethod backend-describe ((connection racket-connection) name &key package on-ok on-abort)
  (racket-request connection :describe (list name) package on-ok on-abort))

(defmethod backend-arglist ((connection racket-connection) name &key package on-ok on-abort)
  (racket-request connection :arglist (list name) package on-ok on-abort))

(defun racket-arglist-with-current (arglist index)
  "ARGLIST (\"(map proc lst ...+)\") with the argument at INDEX (from 0)
marked ===> as Swank's autodoc marks it; a last \"...\" or \"...+\" repeats
the parameter before it. Nil if the call can't take that many."
  (let* ((inside (string-trim "()" arglist))
         (words (arglist-words inside))
         (name (first words))
         (params (rest words))
         (repeat (position-if (lambda (p) (member p '("..." "...+") :test #'string=)) params))
         (target (cond ((and repeat (>= index (1- repeat))) (1- repeat))
                       ((< index (length params)) index))))
    (when (and target (>= target 0) (< target (length params)))
      (format nil "(~a~{ ~a~})" name
              (loop for p in params for i from 0
                    collect (if (= i target) (format nil "===> ~a <===" p) p))))))

(defun arglist-words (text)
  "TEXT split at spaces outside brackets: \"f [x 1] y\" → (\"f\" \"[x 1]\" \"y\")."
  (let ((words '()) (start nil) (depth 0))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (cond ((and (char= c #\Space) (zerop depth))
                    (when start (push (subseq text start i) words) (setf start nil)))
                   (t (unless start (setf start i))
                      (case c ((#\( #\[ #\{) (incf depth)) ((#\) #\] #\}) (decf depth))))))
    (when start (push (subseq text start) words))
    (nreverse words)))

(defmethod backend-autodoc ((connection racket-connection) form &key package on-ok on-abort)
  (multiple-value-bind (call index) (innermost-call form)
    (let ((operator (and call (stringp (first call)) (first call))))
      (if (or (null operator) (minusp index))
          (when on-ok (funcall on-ok (list nil nil)))
          (racket-request connection :arglist (list operator) package
                          (lambda (arglist)
                            (when on-ok
                              (funcall on-ok (list (and arglist (or (racket-arglist-with-current arglist index) arglist))
                                                   t))))
                          on-abort)))))

(defmethod backend-completions ((connection racket-connection) prefix &key package on-ok on-abort)
  (racket-request connection :completions (list prefix) package
                  (lambda (found)
                    (when on-ok
                      (funcall on-ok (list (loop for (name kind) in found collect (list name 0 nil kind)) nil))))
                  on-abort))

(defun racket-location (file line column)
  (list :location (list :file file) (list :line line column) nil))

(defmethod backend-find-definitions ((connection racket-connection) name
                                     &key package text offset on-ok on-abort &allow-other-keys)
  ;; TEXT and OFFSET: the buffer's text and the identifier's place in it, for
  ;; check-syntax; PACKAGE is its file.
  (racket-request connection :find-definitions (list name package text offset) package
                  (lambda (found)
                    (when on-ok
                      (funcall on-ok (loop for (label file line column) in found
                                           collect (list label (racket-location file line column))))))
                  on-abort))

(defmethod backend-xref ((connection racket-connection) kinds name &key package text offset on-ok on-abort
                         &allow-other-keys)
  (declare (ignore name))
  ;; check-syntax knows the uses of a binding in its own file.
  (if (or (rest kinds) (not (member (first kinds) '(:calls :references))) (null text) (null package))
      (when on-ok (funcall on-ok :not-implemented))
      (racket-request connection :references (list package text offset) package
                      (lambda (found)
                        (when on-ok
                          (funcall on-ok (loop for (label file line column) in found
                                               collect (list label (racket-location file line column))))))
                      on-abort)))

(defmethod backend-macroexpand ((connection racket-connection) string &key all compiler package on-ok on-abort)
  (if compiler
      (backend-unsupported connection "expand compiler macros")
      (racket-request connection :macroexpand (list string all) package on-ok on-abort)))

;;; Racket's own requests

(defun racket-documentation-url (connection name &key package text offset on-ok on-abort)
  "The URL of NAME's documentation in the installed Racket docs, or nil."
  (racket-request connection :documentation (list name package text offset) package on-ok on-abort))

(defun racket-problems (connection path text &key on-ok on-abort)
  "Check TEXT, the source of the module in PATH, without running it: ON-OK
gets compiler notes for its errors and unused requires."
  (racket-request connection :problems (list path text) nil
                  (lambda (found)
                    (when on-ok
                      (funcall on-ok
                               (loop for (severity message line column) in found
                                     collect (make-compiler-note severity message
                                                                 (parse-location (racket-location path line column)))))))
                  on-abort))

;;; Projects

(defun racket-project (start)
  "The Racket package START (a file or folder) is in: the nearest folder
above with an info.rkt, or nil."
  (loop for dir = (uiop:pathname-directory-pathname start) then (uiop:pathname-parent-directory-pathname dir)
        repeat 50
        do (when (probe-file (merge-pathnames "info.rkt" dir))
             (return dir))
           (when (equal dir (uiop:pathname-parent-directory-pathname dir)) (return nil))))
