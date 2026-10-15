(in-package #:cadre-tests)

;;; Racket through Cadre's back end (src/core/racket/, vendor/cadre-racket/)

(define-test racket-helpers :parent cadre-tests
  ;; Argument hints mark the argument the cursor is on; "..." repeats the
  ;; parameter before it.
  (is string= "(map ===> proc <=== lst ...+)" (c:racket-arglist-with-current "(map proc lst ...+)" 0))
  (is string= "(map proc ===> lst <=== ...+)" (c:racket-arglist-with-current "(map proc lst ...+)" 1))
  (is string= "(map proc ===> lst <=== ...+)" (c:racket-arglist-with-current "(map proc lst ...+)" 4))
  (is string= "(displayln datum ===> [out] <===)" (c:racket-arglist-with-current "(displayln datum [out])" 1))
  (is eq nil (c:racket-arglist-with-current "(first lst)" 1))
  ;; A Racket package is the nearest folder with an info.rkt.
  (let* ((root (merge-pathnames (format nil "cadre-rkt-~d/" (random 1000000)) (uiop:temporary-directory)))
         (file (merge-pathnames "private/util.rkt" root)))
    (ensure-directories-exist file)
    (unwind-protect
         (progn
           (is eq nil (c:racket-project file))
           (with-open-file (o (merge-pathnames "info.rkt" root) :direction :output) (write-line "#lang info" o))
           (is equal (truename root) (truename (c:racket-project file))))
      (uiop:delete-directory-tree root :validate t))))

;;; A new Racket package (New Project)

(define-test racket-new-project :parent cadre-tests
  (let ((files (c:project-files-for "greet" :kind :racket-package :author "Pat" :license "MIT")))
    (is equal '("info.rkt" "main.rkt" "README.md" ".gitignore" "LICENSE") (mapcar #'car files))
    (true (search "(define collection \"greet\")" (cdr (assoc "info.rkt" files :test #'string=))))
    (true (search "(define license 'MIT)" (cdr (assoc "info.rkt" files :test #'string=))))
    (true (search "A Racket package." (cdr (assoc "main.rkt" files :test #'string=))))
    (is string= "main.rkt" (c:project-main-file :racket-package)))
  (false (search "license" (cdr (assoc "info.rkt" (c:project-files-for "greet" :kind :racket-package :license nil)
                                       :test #'string=))))
  ;; Its own tests pass, if Racket is here to run them.
  (if (ignore-errors (uiop:run-program (list c:*raco-command* "help") :output nil :ignore-error-status t) t)
      (let ((dir (c:create-lisp-project (uiop:temporary-directory) (format nil "greet~d" (random 1000000))
                                        :kind :racket-package)))
        (unwind-protect
             (multiple-value-bind (output error code)
                 (uiop:run-program (list c:*raco-command* "test" (uiop:native-namestring dir))
                                   :output :string :error-output :string :ignore-error-status t)
               (declare (ignore error))
               (is eql 0 code)
               (true (search "1 test passed" output)))
          (uiop:delete-directory-tree dir :validate t)))
      (progn (require-language "Racket") (skip "Racket isn't installed" (true nil)))))

;;; Against a real Racket, if it is installed

(defun start-test-racket (dir)
  "A connection to Cadre's Racket back end started in DIR, and its process;
nil if there is no Racket."
  (when (ignore-errors (uiop:run-program (list c:*racket-command* "--version") :output nil) t)
    (let ((ready (sb-thread:make-semaphore)) (port nil) (failure nil))
      (let ((process (c:start-racket dir
                                     :on-port (lambda (p) (setf port p) (sb-thread:signal-semaphore ready))
                                     :on-exit (lambda (why) (setf failure why) (sb-thread:signal-semaphore ready)))))
        (sb-thread:wait-on-semaphore ready :timeout 60)
        (when failure (error "Racket failed to start: ~a" failure))
        (let* ((done (sb-thread:make-semaphore))
               (conn (c:racket-connect "localhost" port
                                       :handler (lambda (c event) (declare (ignore c))
                                                  (sb-thread:with-mutex (*events-lock*) (push event *events*)))
                                       :on-ready (lambda (c) (declare (ignore c)) (sb-thread:signal-semaphore done)))))
          (unless (sb-thread:wait-on-semaphore done :timeout 30) (error "The Racket back end didn't answer"))
          (values conn process))))))

(defparameter *racket-demo*
  "#lang racket/base
(require racket/list)
(provide area)

(define (area w h)
  (* w h))

(define (twice x)
  (area x (first (list 2))))

(define (unused-helper y) y)
")

(defun offset-of (text needle &optional (start 0))
  (search needle text :start2 start))

(define-test racket-integration :parent cadre-tests
  (setf *events* '())
  (let* ((dir (merge-pathnames (format nil "cadre-racket-~d/" (random 1000000)) (uiop:temporary-directory)))
         (file (merge-pathnames "demo.rkt" dir))
         (path (namestring file))
         (text *racket-demo*))
    (ensure-directories-exist file)
    (with-open-file (o file :direction :output :if-exists :supersede) (write-string text o))
    (multiple-value-bind (conn process) (start-test-racket dir)
      (if (null conn)
          (progn (require-language "Racket") (skip "Racket isn't installed" (true nil)))
          (unwind-protect
               (progn
                 (is eq :racket (c:connection-language conn))
                 (true (search "Racket " (c:connection-implementation conn)))
                 (is string= "racket" (c:connection-package conn))
                 (true (c:backend-supports-p conn :definitions))
                 (false (c:backend-supports-p conn :debugger))
                 ;; Evaluating in the REPL's namespace (racket): the values, and output as events.
                 (is string= "=> 3" (backend-value #'c:backend-eval conn "(displayln \"hi\") (+ 1 2)"))
                 (is string= "=> 1, 2" (backend-value #'c:backend-eval conn "(values 1 2)"))
                 (is string= "=> '(1 \"a\")" (backend-value #'c:backend-eval conn "(list 1 \"a\")"))
                 (true (wait-for (lambda () (find-if (lambda (e) (and (eq (first e) :write-string) (search "hi" (second e))))
                                                     *events*))))
                 (true (search "division by zero"
                               (handler-case (progn (backend-value #'c:backend-eval conn "(/ 1 0)") "")
                                 (error (e) (princ-to-string e))))
                       "an error aborts the request")
                 ;; Code from a file runs in its module, which is run first.
                 (is string= "=> 12" (backend-value #'c:backend-eval conn "(area 3 4)" :package path))
                 (is string= "=> 10" (backend-value #'c:backend-eval conn "(twice 5)" :package path))
                 ;; A run-time error's stack comes as a :post-mortem event.
                 (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
                 (ignore-errors (backend-value #'c:backend-eval conn "(list (area 'x 2))" :package path))
                 (true (wait-for (lambda () (find :post-mortem *events* :key #'first))))
                 (let ((event (find :post-mortem *events* :key #'first)))
                   (true (search "contract" (third event)) "the exception's type")
                   (true (listp (fourth event)) "its frames")
                   (false (find-if (lambda (f) (search "body of" (first f))) (fourth event))
                          "without the module body's frames"))
                 ;; Redefining a function in the module changes its callers.
                 (multiple-value-bind (notes ok)
                     (c:parse-compilation-result
                      (backend-value #'c:backend-compile-form conn "(define (area w h) (+ w h))"
                                     :buffer-name "demo.rkt" :position 50 :line 5 :column 1 :file path :package path))
                   (true ok)
                   (is eq nil notes))
                 (is string= "=> 7" (backend-value #'c:backend-eval conn "(twice 5)" :package path))
                 ;; An error is a note at the place Racket names.
                 (multiple-value-bind (notes ok)
                     (c:parse-compilation-result
                      (backend-value #'c:backend-compile-form conn "(define (broken) (if))"
                                     :buffer-name "demo.rkt" :position 120 :line 14 :column 1 :file path :package path))
                   (false ok)
                   (true (search "if: bad syntax" (c:compiler-note-message (first notes))))
                   (is eql 13 (getf (c:compiler-note-location (first notes)) :line) "line 14, from 0")
                   (is eql 17 (getf (c:compiler-note-location (first notes)) :column)))
                 ;; Running the file enters its module, and redefinitions are gone.
                 (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
                 (true (third (backend-value #'c:backend-compile-file conn path)))
                 (true (wait-for (lambda () (find :new-package *events* :key #'first))))
                 (backend-value #'c:backend-repl-eval conn "(twice 5)")
                 (true (wait-for (lambda () (find-if (lambda (e) (and (eq (third e) :repl-result) (search "10" (second e))))
                                                     *events*))))
                 ;; Looking things up: documentation, arglists from the docs or the source.
                 (is string= "(map proc lst ...+)" (backend-value #'c:backend-arglist conn "map" :package path))
                 (is string= "(area w h)" (backend-value #'c:backend-arglist conn "area" :package path))
                 (is string= "(first ===> lst <===)"
                     (first (backend-value #'c:backend-autodoc conn
                                           (list "first" (c:remote-symbol "swank::%cursor-marker%"))
                                           :package path)))
                 (true (search "pairs.html" (backend-value #'c:backend-describe conn "map" :package path)))
                 (true (search "pairs.html" (backend-value #'c:racket-documentation-url conn "first" :package path)))
                 (true (find "string-append" (first (backend-value #'c:backend-completions conn "string-app" :package path))
                             :key #'first :test #'string=))
                 (true (search "if" (backend-value #'c:backend-macroexpand conn "(when x y)" :package path)))
                 ;; Definitions and references from check-syntax on the text.
                 (let* ((use (offset-of text "area x"))
                        (defs (backend-value #'c:backend-find-definitions conn "area"
                                             :package path :text text :offset use))
                        (location (c:parse-location (second (first defs)))))
                   (is equal path (getf location :file))
                   (is eql 4 (getf location :line) "line 5, from 0")
                   (is eql 9 (getf location :column))
                   (let ((refs (backend-value #'c:backend-xref conn '(:calls) "area" :package path :text text :offset use)))
                     (is eql 2 (length refs) "provide and twice")
                     (is equal "(area x (first (list 2))))" (first (second refs)))))
                 (let* ((defs (backend-value #'c:backend-find-definitions conn "first"
                                             :package path :text text :offset (offset-of text "first (")))
                        (location (c:parse-location (second (first defs)))))
                   (true (search "list.rkt" (getf location :file)) "an import's definition is in its library"))
                 ;; Renaming: the binding and its uses, last first; not another x.
                 (let* ((text (format nil "#lang racket/base~%(define (f x) (+ x 1))~%(define (g x) x)~%"))
                        (spans (backend-value #'c:racket-binding-spans conn path text (search "x 1" text))))
                   (is equal (list (list (search "x 1" text) (1+ (search "x 1" text)))
                                   (list (search "x)" text) (1+ (search "x)" text))))
                       spans)
                   (let ((outcome (handler-case (progn (backend-value #'c:racket-binding-spans conn path text
                                                                    (search "+" text))
                                                     "")
                                  (error (e) (princ-to-string e)))))
                     (true (search "imported" outcome) outcome)))
                 ;; Problems: errors and unused requires, without running anything.
                 (let ((notes (backend-value #'c:racket-problems conn path
                                             (concatenate 'string text (format nil "(define (bad) (nosuch 1))~%")))))
                   (is eql 1 (length notes))
                   (true (search "nosuch" (c:compiler-note-message (first notes)))))
                 (let ((notes (backend-value #'c:racket-problems conn path
                                             (format nil "#lang racket/base~%(require racket/string)~%(define x 1)~%"))))
                   (true (search "Unused require" (c:compiler-note-message (first notes)))))
                 ;; Input: the program asks for a line, and gets what is typed.
                 (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
                 (let ((done (sb-thread:make-semaphore)) (value nil))
                   (c:backend-eval conn "(string-upcase (read-line))" 
                                     :on-ok (lambda (v) (setf value v) (sb-thread:signal-semaphore done))
                                     :on-abort (lambda (r) (setf value r) (sb-thread:signal-semaphore done)))
                   (let ((ask (progn (wait-for (lambda () (find :read-string *events* :key #'first)))
                                       (find :read-string *events* :key #'first))))
                     (true ask "it asks for input")
                     (when ask (c:backend-send-input conn (second ask) (third ask) (format nil "typed here~%"))))
                   (true (sb-thread:wait-on-semaphore done :timeout 20))
                   (is string= "=> \"TYPED HERE\"" value))
                 ;; Interrupting a long evaluation.
                 (let ((done (sb-thread:make-semaphore)) (outcome nil))
                   (c:backend-eval conn "(sleep 60)"
                                   :on-ok (lambda (v) (setf outcome v) (sb-thread:signal-semaphore done))
                                   :on-abort (lambda (r) (setf outcome (list :aborted r)) (sb-thread:signal-semaphore done)))
                   (sleep 0.5)
                   (c:backend-interrupt conn)
                   (true (sb-thread:wait-on-semaphore done :timeout 20) "the interrupt ends it")
                   (is equal '(:aborted "Interrupted") outcome)))
            (c:backend-disconnect conn)
            (c:kill-inferior-lisp process)
            (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))

;;; raco's output, as notes

(define-test raco-places :parent cadre-tests
  (let ((note (c:raco-places "/p/a.rkt:3:4: f: unbound identifier")))
    (is eq :error (c:compiler-note-severity note))
    (is equal '(:file "/p/a.rkt" :line 2 :column 4) (c:compiler-note-location note)))
  (let ((note (c:raco-places "location:   /p/a_test.rkt:10:2")))
    (is eq :warning (c:compiler-note-severity note)))
  (is eq nil (c:raco-places "raco test: \"main.rkt\"")))
