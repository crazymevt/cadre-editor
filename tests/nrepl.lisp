(in-package #:cadre-tests)

;;; Clojure through nREPL (src/core/nrepl/)

(define-test bencode :parent cadre-tests
  (is equalp (sb-ext:string-to-octets "i42e") (c:bencode 42))
  (is equalp (sb-ext:string-to-octets "i-3e") (c:bencode -3))
  (is equalp (sb-ext:string-to-octets "5:hello") (c:bencode "hello"))
  (is equalp (sb-ext:string-to-octets "2:λ" :external-format :utf-8) (c:bencode "λ") "lengths count bytes")
  (is equalp (sb-ext:string-to-octets "l1:ai1ee") (c:bencode '("a" 1)))
  (is equalp (sb-ext:string-to-octets "le") (c:bencode '()))
  (is equalp (sb-ext:string-to-octets "d2:id1:12:op4:evale") (c:bencode '(("op" . "eval") ("id" . "1")))
      "dictionary keys are sorted")
  (is equal '(("d" ("x" . "λ")) ("ids" "1" "2") ("n" . 7) ("op" . "eval"))
      (c:bdecode (c:bencode '(("op" . "eval") ("ids" "1" "2") ("n" . 7) ("d" ("x" . "λ")))))
      "a round trip, keys sorted")
  (is string= "eval" (c:dict-get (c:bdecode (sb-ext:string-to-octets "d2:op4:evale")) "op"))
  (fail (c:bdecode (sb-ext:string-to-octets "d2:op")) 'end-of-file)
  (fail (c:bdecode (sb-ext:string-to-octets "ixe")) 'c:bencode-error))

(define-test clojure-helpers :parent cadre-tests
  (is eql 54321 (c:nrepl-port-in "nREPL server started on port 54321 on host 127.0.0.1 - nrepl://127.0.0.1:54321"))
  (is eql 1667 (c:nrepl-port-in "Started nREPL server at 127.0.0.1:1667"))
  (is eq nil (c:nrepl-port-in "Downloading: nrepl/nrepl/1.7.0/nrepl-1.7.0.jar from clojars"))
  (is equal '(:location (:file "/tmp/a.clj") (:line 5 17) nil)
      (c:clojure-error-location (format nil "Syntax error compiling at (/tmp/a.clj:5:17).~%Unable to resolve symbol")))
  (is eq nil (c:clojure-error-location "Execution error (ArithmeticException) at user/eval1 (REPL:1)."))
  (is string= "[f ===> coll <===]" (c:arglist-with-current "[f coll]" 1))
  (is string= "[f c1 & ===> colls <===]" (c:arglist-with-current "[f c1 & colls]" 4))
  (is eq nil (c:arglist-with-current "[f]" 2))
  (let ((c:*cider-nrepl* t) (c:*clojure-aliases* ":dev"))
    (let ((command (c:clojure-command :deps)))
      (is string= "clojure" (first command))
      (true (member "-M:dev" command :test #'string=))
      (true (search "cider/cider-nrepl" (third command)))
      (true (member "[cider.nrepl/cider-middleware]" command :test #'string=))))
  (let ((c:*cider-nrepl* nil))
    (false (search "cider" (format nil "~{~a ~}" (c:clojure-command :deps))))
    (is equal '("bb" "nrepl-server" "0") (c:clojure-command :babashka)))
  ;; The project is the nearest folder with deps.edn (or project.clj, bb.edn).
  (let* ((root (merge-pathnames (format nil "cadre-clj-~d/" (random 1000000)) (uiop:temporary-directory)))
         (file (merge-pathnames "src/app/core.clj" root)))
    (ensure-directories-exist file)
    (unwind-protect
         (progn
           (is eq nil (c:clojure-project file))
           (with-open-file (o (merge-pathnames "deps.edn" root) :direction :output) (write-string "{}" o))
           (multiple-value-bind (dir kind) (c:clojure-project file)
             (is equal (truename root) (truename dir))
             (is eq :deps kind)))
      (uiop:delete-directory-tree root :validate t))))

;;; Against a real nREPL with cider-nrepl, if the Clojure CLI is installed

(defun start-test-clojure ()
  "A connection to a fresh nREPL (with cider-nrepl) in a scratch folder, and
the process; nil if there is no Clojure CLI."
  (when (ignore-errors (uiop:run-program '("clojure" "-Sdescribe") :output nil) t)
    (let* ((dir (merge-pathnames (format nil "cadre-nrepl-~d/" (random 1000000)) (uiop:temporary-directory)))
           (ready (sb-thread:make-semaphore)) (port nil) (failure nil))
      (ensure-directories-exist dir)
      (let ((process (c:start-clojure dir nil
                                      :on-port (lambda (p) (setf port p) (sb-thread:signal-semaphore ready))
                                      :on-exit (lambda (why) (setf failure why) (sb-thread:signal-semaphore ready)))))
        (sb-thread:wait-on-semaphore ready :timeout 300)
        (when failure (error "Clojure failed to start: ~a" failure))
        (let* ((done (sb-thread:make-semaphore))
               (conn (c:nrepl-connect "localhost" port
                                      :handler (lambda (c event) (declare (ignore c))
                                                 (sb-thread:with-mutex (*events-lock*) (push event *events*)))
                                      :on-ready (lambda (c) (declare (ignore c)) (sb-thread:signal-semaphore done)))))
          (unless (sb-thread:wait-on-semaphore done :timeout 60) (error "nREPL session setup timed out"))
          (values conn process dir))))))

(define-test nrepl-integration :parent cadre-tests
  (setf *events* '())
  (multiple-value-bind (conn process dir) (start-test-clojure)
    (if (null conn)
        (progn (require-language "The Clojure CLI") (skip "The Clojure CLI isn't installed" (true nil)))
        (unwind-protect
             (progn
               (is eq :clojure (c:connection-language conn))
               (true (search "Clojure 1." (c:connection-implementation conn)))
               (true (c:nrepl-op-p conn "info") "cider-nrepl is there")
               (true (c:backend-supports-p conn :definitions))
               (false (c:backend-supports-p conn :debugger))
               ;; Evaluating: the last value, and output as events.
               (is string= "=> 3" (backend-value #'c:backend-eval conn "(println \"hi\") (+ 1 2)"))
               (true (wait-for (lambda () (find-if (lambda (e) (and (eq (first e) :write-string) (search "hi" (second e))))
                                                   *events*))))
               (true (search "Divide by zero"
                             (handler-case (progn (backend-value #'c:backend-eval conn "(/ 1 0)") "")
                               (error (e) (princ-to-string e))))
                     "an error aborts the request")
               ;; Loading a file, then looking at what it defines.
               (let ((file (merge-pathnames "probe/core.clj" dir)))
                 (ensure-directories-exist file)
                 (with-open-file (o file :direction :output :if-exists :supersede)
                   (format o "(ns probe.core)~%~%(defn area~%  \"Width times height.\"~%  [w h]~%  (* w h))~%(defn twice [] (area 2 3))~%"))
                 (let ((result (backend-value #'c:backend-compile-file conn (namestring file))))
                   (true (third result) "it compiled")
                   (is eq nil (second result) "with no notes"))
                 (true (search "Width times height" (backend-value #'c:backend-describe conn "area" :package "probe.core")))
                 (is string= "(area [w h])" (backend-value #'c:backend-arglist conn "area" :package "probe.core"))
                 (let* ((defs (backend-value #'c:backend-find-definitions conn "area" :package "probe.core"))
                        (location (c:parse-location (second (first defs)))))
                   (is equal (namestring (truename file)) (namestring (truename (getf location :file))))
                   (is eql 2 (getf location :line) "line 3, from 0"))
                 (let ((doc (backend-value #'c:backend-autodoc conn
                                           (list "area" "1" (c:remote-symbol "swank::%cursor-marker%"))
                                           :package "probe.core")))
                   (is string= "(area [w ===> h <===])" (first doc))))
               ;; A compile error is a note at the place Clojure names.
               (multiple-value-bind (notes ok)
                   (c:parse-compilation-result
                    (backend-value #'c:backend-compile-form conn "(defn broken [] (no-such-thing))"
                                   :buffer-name "core.clj" :position 1 :line 9 :column 1
                                   :file "/tmp/probe-core.clj" :package "probe.core"))
                 (false ok)
                 (true (search "no-such-thing" (c:compiler-note-message (first notes))))
                 (is eql 8 (getf (c:compiler-note-location (first notes)) :line)))
               ;; Completion and macroexpansion.
               (true (find "mapcat" (first (backend-value #'c:backend-completions conn "mapc" :package "user"))
                           :key #'first :test #'string=))
               (true (search "if" (backend-value #'c:backend-macroexpand conn "(when x y)" :package "user")))
               ;; The REPL: values arrive as :repl-result output, and namespace changes as :new-package.
               (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
               (backend-value #'c:backend-repl-eval conn "(* 6 7)")
               (true (find-if (lambda (e) (and (eq (first e) :write-string) (eq (third e) :repl-result)
                                               (search "42" (second e))))
                              *events*))
               (is equal '("probe.core" "probe.core") (backend-value #'c:backend-set-package conn "probe.core"))
               (is string= "probe.core" (c:connection-package conn))
               ;; A macro's :style/indent arrives in cider-nrepl's state and
               ;; makes its forms indent as body forms.
               (remhash "with-probe" c:*clojure-indent-rules*)
               (backend-value #'c:backend-eval conn
                              "(defmacro with-probe {:style/indent 1} [x & body] `(do ~x ~@body))"
                              :package "user")
               (true (wait-for (lambda () (gethash "with-probe" c:*clojure-indent-rules*)) 20)
                     "learned from :style/indent")
               ;; Input: the program asks for a line, and gets what is typed.
               (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
               (let ((done (sb-thread:make-semaphore)) (value nil))
                 (c:backend-eval conn "(clojure.string/upper-case (read-line))" :package "user"
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
                 (c:backend-eval conn "(Thread/sleep 60000)"
                                 :on-ok (lambda (v) (setf outcome v) (sb-thread:signal-semaphore done))
                                 :on-abort (lambda (r) (setf outcome (list :aborted r)) (sb-thread:signal-semaphore done)))
                 (sleep 0.5)
                 (c:backend-interrupt conn)
                 (true (sb-thread:wait-on-semaphore done :timeout 20) "the interrupt ends it")))
          (c:nrepl-disconnect conn)
          (c:stop-clojure process)
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))

;;; A new Clojure project (New Project)

(define-test clojure-new-project :parent cadre-tests
  (let ((files (c:project-files-for "my-app" :kind :clojure-project :author "Pat")))
    (is equal '("deps.edn" "src/my_app/core.clj" "test/my_app/core_test.clj" "README.md" ".gitignore" "LICENSE")
        (mapcar #'car files))
    (true (search "(ns my-app.core" (cdr (assoc "src/my_app/core.clj" files :test #'string=))))
    (is string= "src/my_app/core.clj" (c:project-main-file :clojure-project "my-app")))
  ;; Its own tests pass, if the Clojure CLI is here to run them.
  (if (ignore-errors (uiop:run-program '("clojure" "-Sdescribe") :output nil) t)
      (let ((dir (c:create-lisp-project (uiop:temporary-directory) (format nil "my-app~d" (random 1000000))
                                        :kind :clojure-project)))
        (unwind-protect
             (multiple-value-bind (output error code)
                 (uiop:run-program '("clojure" "-M:test") :directory dir
                                   :output :string :error-output :string :ignore-error-status t)
               (is eql 0 code (format nil "~a~a" output error))
               (true (search "0 failures, 0 errors" output)))
          (uiop:delete-directory-tree dir :validate t)))
      (progn (require-language "The Clojure CLI") (skip "The Clojure CLI isn't installed" (true nil)))))

;;; clj-kondo's findings

(define-test clj-kondo-findings :parent cadre-tests
  (let ((notes (c:clj-kondo-notes (format nil "<stdin>:3:5: warning: unused binding x~%~
                                               <stdin>:7:1: error: Unresolved symbol: frob~%~
                                               linting took 12ms, errors: 1, warnings: 1~%")
                                  "/p/src/a.clj")))
    (is eql 2 (length notes))
    (is eq :warning (c:compiler-note-severity (first notes)))
    (is string= "unused binding x" (c:compiler-note-message (first notes)))
    (is equal '(:file "/p/src/a.clj" :line 2 :column 4) (c:compiler-note-location (first notes)))
    (is eq :error (c:compiler-note-severity (second notes))))
  (is equal '("--lint" "-" "--filename" "/p/a.clj") (c:clj-kondo-arguments "/p/a.clj")))

;;; Indentation from .cljfmt.edn and :style/indent

(define-test clojure-indent-rules :parent cadre-tests
  (is equal '("my-macro" "my.ns/with-thing" "frob")
      (c:cljfmt-indented-names
       "{:indents {my-macro [[:block 1]], my.ns/with-thing [[:inner 0]], #\"^def\" [[:inner 0]]}
         :extra-indents {frob [[:block 0]]}}"))
  ;; A cider-nrepl state message: namespace → interns → var → metadata.
  (let* ((deffoo '("deffoo" . (("style/indent" . "1"))))
         (plain '("plain" . (("arglists" . "([x])"))))
         (changed (list (cons "demo.core" (list (cons "interns" (list deffoo plain)))))))
    (is equal '("deffoo") (c:style-indent-names changed)))
  (let ((c:*clojure-indent-rules* (make-hash-table :test 'equal))
        (text (format nil "(frobnicate a~%b)")))
    (flet ((indent () (c:lisp-indentation (c:make-lisp-syntax (c:make-string-text text) :dialect c:*clojure-dialect*) 1)))
      (is eql 12 (indent) "a call lines up under its first argument")
      (c:learn-clojure-indentation '("my.ns/frobnicate"))
      (is eql 2 (indent) "a learned body form indents 2"))))
