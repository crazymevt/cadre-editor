;;;; backend.lisp — nREPL's answers to the editor's requests (../backend.lisp)
;;;;
;;;; nREPL itself evaluates, loads files, completes, looks symbols up and
;;;; interrupts. cider-nrepl, when the server has it, adds documentation
;;;; with file and line (info), cross-references (fn-refs, fn-deps) and
;;;; macroexpansion; without it, documentation and macroexpansion fall back
;;;; to evaluating clojure.repl and clojure.core functions. Each answer is put
;;;; in the shape Swank's has, as backend.lisp describes.

(in-package #:cadre)

(defmethod backend-supports-p ((connection nrepl-connection) feature)
  (case feature
    ((:eval :compile :compile-file :load-file :describe :arglist :autodoc :completions
      :macroexpand :repl :interrupt)
     t)
    (:definitions (or (nrepl-op-p connection "info") (nrepl-op-p connection "lookup")))
    (:xref (nrepl-op-p connection "fn-refs"))
    (t nil)))

;;; Evaluating

(defun first-line (string)
  (let ((string (string-trim '(#\Newline #\Space) string)))
    (subseq string 0 (or (position #\Newline string) (length string)))))

(defun nrepl-eval-request (connection code &key ns file line column repl on-done on-abort)
  "Evaluate CODE. Output goes to the handler as it comes; with REPL, values
and namespace changes do too. ON-DONE is called with the values (strings),
the error output, and whether it failed."
  (let ((values '()) (err (make-string-output-stream)) (failed nil) (id nil))
    (setf id (nrepl-send connection
                         (append (list (cons "op" "eval") (cons "code" code))
                                 (and ns (list (cons "ns" ns)))
                                 (and file (list (cons "file" file)))
                                 (and line (list (cons "line" line)))
                                 (and column (list (cons "column" column))))
                         :on-abort on-abort
                         :on-message
                         (lambda (message)
                           (let ((out (dict-get message "out"))
                                 (error-output (dict-get message "err"))
                                 (value (dict-get message "value"))
                                 (new-ns (dict-get message "ns")))
                             (when out (nrepl-event connection (list :write-string out nil)))
                             (when error-output
                               (write-string error-output err)
                               (nrepl-event connection (list :write-string error-output nil)))
                             (when value
                               (push value values)
                               (when repl (nrepl-event connection (list :write-string (format nil "~a~%" value) :repl-result))))
                             (when (and repl new-ns (string/= new-ns (connection-package connection)))
                               (setf (connection-package connection) new-ns (connection-prompt connection) new-ns)
                               (nrepl-event connection (list :new-package new-ns new-ns)))
                             (when (nrepl-status-p message "eval-error") (setf failed t))
                             ;; The code reads *in*: ask the REPL for a line.
                             (when (nrepl-status-p message "need-input")
                               (nrepl-event connection (list :read-string nil nil)))
                             (when (nrepl-status-p message "namespace-not-found")
                               (setf failed t)
                               (format err "Namespace ~a isn't loaded: evaluate its ns form, or load the file." ns))))
                         :on-done (lambda (responses)
                                    (declare (ignore responses))
                                    (funcall on-done (reverse values) (get-output-stream-string err) failed))))
    (push id (nrepl-eval-ids connection))
    id))

(defun eval-error-reason (error-output)
  (if (plusp (length error-output)) (string-trim '(#\Newline #\Space) error-output) "Evaluation failed"))

(defun nrepl-value-callbacks (on-ok on-abort)
  (lambda (values error-output failed)
    (if failed
        (when on-abort (funcall on-abort (eval-error-reason error-output)))
        (when on-ok (funcall on-ok (format nil "=> ~a" (or (car (last values)) "nil")))))))

(defmethod backend-eval ((connection nrepl-connection) string &key package on-ok on-abort)
  (nrepl-eval-request connection string :ns package :on-abort on-abort
                      :on-done (nrepl-value-callbacks on-ok on-abort)))

(defmethod backend-eval-region ((connection nrepl-connection) string &key package on-ok on-abort)
  (nrepl-eval-request connection string :ns package :on-abort on-abort
                      :on-done (nrepl-value-callbacks on-ok on-abort)))

(defun clojure-error-location (error-output)
  "The place a Clojure compiler error names, \"… at (/path/f.clj:5:17)\", as
a Swank location, or nil."
  (let* ((start (search " at (" error-output))
         (end (and start (position #\) error-output :start start))))
    (when end
      (let* ((place (subseq error-output (+ start 5) end))
             (parts (uiop:split-string place :separator ":")))
        (when (and (>= (length parts) 3)
                   (every (lambda (p) (and (plusp (length p)) (every #'digit-char-p p))) (last parts 2)))
          (let ((file (format nil "~{~a~^:~}" (butlast parts 2))))
            (when (uiop:absolute-pathname-p file)
              (list :location (list :file file)
                    (list :line (parse-integer (first (last parts 2))) (parse-integer (car (last parts))))
                    nil))))))))

(defun compilation-result (failed error-output start fallback-location)
  "A Swank-shaped compilation result: no notes if it worked, else one note
with the error at its place (or FALLBACK-LOCATION)."
  (list :compilation-result
        (and failed
             (list (list :severity :error
                         :message (eval-error-reason error-output)
                         :location (or (clojure-error-location error-output) fallback-location))))
        (not failed)
        (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)
        nil nil))

(defmethod backend-compile-form ((connection nrepl-connection) string
                                 &key buffer-name position line column file package on-ok on-abort
                                 &allow-other-keys)
  (let ((start (get-internal-real-time)))
    (nrepl-eval-request connection string :ns package :file file :line line :column column :on-abort on-abort
                        :on-done (lambda (values error-output failed)
                                   (declare (ignore values))
                                   (when on-ok
                                     (funcall on-ok (compilation-result
                                                     failed error-output start
                                                     (list :location (list :buffer buffer-name)
                                                           (list :position (or position 1)) nil))))))))

(defun nrepl-load-file (connection path &key on-done on-abort)
  "Load the file at PATH, its current text read from disk; ON-DONE gets
the values, the error output and whether it failed."
  (let ((err (make-string-output-stream)) (values '()) (failed nil))
    (nrepl-send connection
                (list (cons "op" "load-file") (cons "file" (uiop:read-file-string path))
                      (cons "file-path" path) (cons "file-name" (file-namestring path)))
                :on-abort on-abort
                :on-message (lambda (message)
                              (let ((out (dict-get message "out")) (error-output (dict-get message "err")))
                                (when out (nrepl-event connection (list :write-string out nil)))
                                (when error-output
                                  (write-string error-output err)
                                  (nrepl-event connection (list :write-string error-output nil)))
                                (when (dict-get message "value") (push (dict-get message "value") values))
                                (when (nrepl-status-p message "eval-error") (setf failed t))))
                :on-done (lambda (responses)
                           (declare (ignore responses))
                           (funcall on-done (reverse values) (get-output-stream-string err) failed)))))

(defmethod backend-compile-file ((connection nrepl-connection) path &key on-ok on-abort on-loaded &allow-other-keys)
  ;; Clojure compiles as it loads: one load-file does both.
  (let ((start (get-internal-real-time)))
    (nrepl-load-file connection path :on-abort on-abort
                     :on-done (lambda (values error-output failed)
                                (declare (ignore values))
                                (when on-ok
                                  (funcall on-ok (compilation-result failed error-output start
                                                                     (list :location (list :file path) (list :line 1 0) nil))))
                                (when (and (not failed) on-loaded) (funcall on-loaded))))))

(defmethod backend-load-file ((connection nrepl-connection) path &key on-ok on-abort &allow-other-keys)
  (nrepl-load-file connection path :on-abort on-abort
                   :on-done (lambda (values error-output failed)
                              (if failed
                                  (when on-abort (funcall on-abort (eval-error-reason error-output)))
                                  (when on-ok (funcall on-ok (car (last values))))))))

(defmethod backend-repl-eval ((connection nrepl-connection) string &key on-ok on-abort &allow-other-keys)
  ;; Errors were already shown as the REPL's error output.
  (nrepl-eval-request connection string :ns (connection-package connection) :repl t :on-abort on-abort
                      :on-done (lambda (values error-output failed)
                                 (declare (ignore values error-output failed))
                                 (when on-ok (funcall on-ok nil)))))

(defmethod backend-set-package ((connection nrepl-connection) name &key on-ok on-abort)
  (nrepl-eval-request connection (format nil "(in-ns '~a)" name) :on-abort on-abort
                      :on-done (lambda (values error-output failed)
                                 (declare (ignore values))
                                 (if failed
                                     (when on-abort (funcall on-abort (eval-error-reason error-output)))
                                     (progn
                                       (setf (connection-package connection) name (connection-prompt connection) name)
                                       (when on-ok (funcall on-ok (list name name))))))))

(defmethod backend-send-input ((connection nrepl-connection) thread tag string)
  (declare (ignore thread tag))
  (nrepl-send connection (list (cons "op" "stdin") (cons "stdin" string))))

(defmethod backend-interrupt ((connection nrepl-connection))
  (dolist (id (nrepl-eval-ids connection))
    (nrepl-send connection (list (cons "op" "interrupt") (cons "interrupt-id" id)))))

(defmethod backend-disconnect ((connection nrepl-connection))
  (nrepl-disconnect connection))

;;; Looking things up

(defun clojure-read-string (text)
  "The string a printed Clojure string literal TEXT stands for, or TEXT."
  (if (and (>= (length text) 2) (char= (char text 0) #\") (char= (char text (1- (length text))) #\"))
      (with-output-to-string (out)
        (loop with i = 1
              while (< i (1- (length text)))
              do (let ((c (char text i)))
                   (if (and (char= c #\\) (< (1+ i) (1- (length text))))
                       (progn (incf i)
                              (write-char (case (char text i) (#\n #\Newline) (#\t #\Tab) (t (char text i))) out))
                       (write-char c out))
                   (incf i))))
      text))

(defun nrepl-info (connection name ns on-info on-abort)
  "Call ON-INFO with what cider-nrepl's info (or nREPL's lookup) says about
NAME in NS, as an alist, or nil if it knows nothing."
  (let ((op (if (nrepl-op-p connection "info") "info" "lookup")))
    (nrepl-send connection (list (cons "op" op) (cons "sym" name) (cons "ns" (or ns "user")))
                :session :tooling :on-abort on-abort
                :on-done (lambda (responses)
                           (let ((reply (find-if (lambda (m) (or (dict-get m "name") (dict-get m "info"))) responses)))
                             (funcall on-info (if (equal op "lookup") (dict-get reply "info") reply)))))))

(defun info-arglists (info)
  "The arglists from INFO, as strings such as \"[f coll]\"."
  (let ((text (or (dict-get info "arglists-str") (dict-get info "arglists"))))
    (and (stringp text) (remove "" (uiop:split-string text :separator '(#\Newline)) :test #'string=))))

(defun info-qualified-name (info name)
  (let ((ns (dict-get info "ns")) (n (or (dict-get info "name") name)))
    (if ns (format nil "~a/~a" ns n) n)))

(defmethod backend-describe ((connection nrepl-connection) name &key package on-ok on-abort)
  (if (or (nrepl-op-p connection "info") (nrepl-op-p connection "lookup"))
      (nrepl-info connection name package
                  (lambda (info)
                    (when on-ok
                      (funcall on-ok
                               (if (null info)
                                   (format nil "~a is not defined." name)
                                   (format nil "~a~@[~%~{  ~a~^~%~}~]~@[~%~%~a~]~@[~%~%Defined in ~a~@[, line ~a~]~]"
                                           (info-qualified-name info name)
                                           (info-arglists info)
                                           (dict-get info "doc")
                                           (dict-get info "file") (dict-get info "line"))))))
                  on-abort)
      (nrepl-eval-request connection (format nil "(with-out-str (clojure.repl/doc ~a))" name) :ns package
                          :on-abort on-abort
                          :on-done (lambda (values error-output failed)
                                     (if failed
                                         (when on-abort (funcall on-abort (eval-error-reason error-output)))
                                         (when on-ok (funcall on-ok (clojure-read-string (or (car (last values)) "")))))))))

(defmethod backend-arglist ((connection nrepl-connection) name &key package on-ok on-abort)
  (nrepl-info connection name package
              (lambda (info)
                (let ((arglists (info-arglists info)))
                  (when on-ok
                    (funcall on-ok (and arglists
                                        (format nil "(~a ~{~a~^ ~})" (or (dict-get info "name") name) arglists))))))
              on-abort))

(defun innermost-call (form)
  "In FORM (from RAW-FORM-AT, the cursor marked), the innermost list holding
the cursor, and the index of the argument the cursor is on (from 0)."
  (labels ((walk (list)
             (loop for element in list
                   for i from 0
                   do (cond ((and (remote-symbol-p element) (search "cursor-marker" (remote-symbol-name element)))
                             (return-from walk (values list (1- i))))
                            ((consp element)
                             (multiple-value-bind (inner index) (walk element)
                               (when inner (return-from walk (values inner index)))))))))
    (walk form)))

(defun arglist-with-current (arglist index)
  "ARGLIST (\"[f c1 & colls]\") with the parameter at INDEX marked ===> as
Swank's autodoc marks it, or nil if it can't take that many."
  (let* ((params (remove "" (uiop:split-string (string-trim "[]" arglist) :separator " ") :test #'string=))
         (rest (position "&" params :test #'string=))
         (target (cond ((< index (or rest (length params))) index)
                       (rest (1+ rest)))))
    (when (and target (< target (length params)))
      (format nil "[~{~a~^ ~}]"
              (loop for p in params for i from 0
                    collect (if (= i target) (format nil "===> ~a <===" p) p))))))

(defmethod backend-autodoc ((connection nrepl-connection) form &key package on-ok on-abort)
  (multiple-value-bind (call index) (innermost-call form)
    (let ((operator (and call (stringp (first call)) (first call))))
      (if (or (null operator) (minusp index))
          (when on-ok (funcall on-ok (list nil nil)))
          (nrepl-info connection operator package
                      (lambda (info)
                        (let* ((arglists (info-arglists info))
                               (marked (or (some (lambda (a) (arglist-with-current a index)) arglists)
                                           (first arglists))))
                          (when on-ok
                            (funcall on-ok (list (and marked (format nil "(~a ~a)" operator marked)) t)))))
                      on-abort)))))

(defun completion-type-kind (type)
  (cdr (assoc type '(("function" . :function) ("macro" . :macro) ("special-form" . :special)
                     ("var" . :variable) ("namespace" . :package) ("class" . :class)
                     ("keyword" . :keyword) ("method" . :method) ("static-method" . :method)
                     ("field" . :variable) ("static-field" . :variable) ("local" . :variable))
              :test #'equal)))

(defmethod backend-completions ((connection nrepl-connection) prefix &key package on-ok on-abort)
  (let ((op (if (nrepl-op-p connection "complete") "complete" "completions")))
    (nrepl-send connection (list (cons "op" op) (cons "prefix" prefix) (cons "ns" (or package "user")))
                :session :tooling :on-abort on-abort
                :on-done (lambda (responses)
                           (let ((candidates (some (lambda (m) (dict-get m "completions")) responses)))
                             (when on-ok
                               (funcall on-ok
                                        (list (loop for c in candidates
                                                    collect (list (dict-get c "candidate") 0 nil
                                                                  (completion-type-kind (dict-get c "type"))))
                                              nil))))))))

(defun clojure-file-location (file line column)
  "A Swank location for FILE (as cider-nrepl gives it: a path, file: URL or
jar: URL) at LINE and COLUMN, or an :error one for code inside a jar."
  (cond ((null file) (list :error "No source location"))
        ((and (> (length file) 4) (string= "jar:" file :end2 4))
         (list :error (format nil "The source is inside a jar: ~a" (subseq file 4))))
        (t (list :location
                 (list :file (if (and (> (length file) 5) (string= "file:" file :end2 5)) (subseq file 5) file))
                 (list :line (or line 1) (max 0 (1- (or column 1))))
                 nil))))

(defmethod backend-find-definitions ((connection nrepl-connection) name &key package on-ok on-abort)
  (nrepl-info connection name package
              (lambda (info)
                (when on-ok
                  (funcall on-ok
                           (and info (dict-get info "file")
                                (list (list (format nil "(def ~a)" (info-qualified-name info name))
                                            (clojure-file-location (dict-get info "file") (dict-get info "line")
                                                                   (dict-get info "column"))))))))
              on-abort))

(defmethod backend-xref ((connection nrepl-connection) kinds name &key package on-ok on-abort)
  (let ((op (case (first kinds)
              ((:calls :callers :references) "fn-refs")
              ((:calls-who :callees) "fn-deps"))))
    (if (or (rest kinds) (null op) (not (nrepl-op-p connection op)))
        (when on-ok (funcall on-ok :not-implemented))
        (nrepl-send connection (list (cons "op" op) (cons "sym" name) (cons "ns" (or package "user")))
                    :session :tooling :on-abort on-abort
                    :on-done (lambda (responses)
                               (let ((found (some (lambda (m) (dict-get m op)) responses)))
                                 (when on-ok
                                   (funcall on-ok
                                            (loop for entry in found
                                                  collect (list (dict-get entry "name")
                                                                (clojure-file-location (dict-get entry "file")
                                                                                       (dict-get entry "line")
                                                                                       (dict-get entry "column"))))))))))))

(defmethod backend-macroexpand ((connection nrepl-connection) string &key all compiler package on-ok on-abort)
  (cond (compiler (backend-unsupported connection "expand compiler macros"))
        ((nrepl-op-p connection "macroexpand")
         (nrepl-send connection (list (cons "op" "macroexpand") (cons "code" string) (cons "ns" (or package "user"))
                                      (cons "expander" (if all "macroexpand-all" "macroexpand-1"))
                                      (cons "display-namespaces" "tidy"))
                     :session :tooling :on-abort on-abort
                     :on-done (lambda (responses)
                                (let ((expansion (some (lambda (m) (dict-get m "expansion")) responses))
                                      (error-output (some (lambda (m) (dict-get m "err")) responses)))
                                  (if expansion
                                      (when on-ok (funcall on-ok expansion))
                                      (when on-abort (funcall on-abort (eval-error-reason (or error-output "")))))))))
        (t (nrepl-eval-request connection
                               (format nil "(with-out-str (clojure.pprint/pprint (~a '~a)))"
                                       (if all "clojure.walk/macroexpand-all" "clojure.core/macroexpand-1") string)
                               :ns package :on-abort on-abort
                               :on-done (lambda (values error-output failed)
                                          (if failed
                                              (when on-abort (funcall on-abort (eval-error-reason error-output)))
                                              (when on-ok (funcall on-ok (clojure-read-string (or (car (last values)) ""))))))))))
