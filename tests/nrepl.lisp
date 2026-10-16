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
  (fail (c:bdecode (sb-ext:string-to-octets "ixe")) 'c:bencode-error)
  (fail (c:bdecode (sb-ext:string-to-octets "-5:hello")) 'c:bencode-error))

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
               (true (wait-for (lambda () (find :post-mortem *events* :key #'first)))
                     "its stack comes from cider-nrepl")
               (let ((event (find :post-mortem *events* :key #'first)))
                 (is string= "java.lang.ArithmeticException" (third event))
                 (true (fourth event) "with frames"))
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
               ;; The inspector: cider-nrepl's replies in Swank's shape.
               (let ((inspection (c:parse-inspection (backend-value #'c:nrepl-inspect conn "{:a 1 :b [1 2]}" "user"))))
                 (is string= "clojure.lang.PersistentArrayMap" (c:inspection-title inspection))
                 (true (find '(:value ":a" 1) (c:inspection-parts inspection) :test #'equal)))
               (let ((inspection (c:parse-inspection (backend-value #'c:nrepl-inspect-op conn "inspect-push" :index 2))))
                 (is string= "java.lang.Long" (c:inspection-title inspection)))
               (let ((inspection (c:parse-inspection (backend-value #'c:nrepl-inspect-op conn "inspect-pop"))))
                 (is string= "clojure.lang.PersistentArrayMap" (c:inspection-title inspection)))
               ;; Tests: the summary, and each failure with its line.
               (backend-value #'c:backend-eval conn
                              "(ns probe.t (:require [clojure.test :refer [deftest is]])) (deftest ok (is (= 1 1))) (deftest bad (is (= 1 2)))"
                              :package "user")
               (let ((done (sb-thread:make-semaphore)) (summary nil) (failures nil))
                 (c:nrepl-run-tests conn "probe.t" :on-done (lambda (s f) (setf summary s failures f)
                                                              (sb-thread:signal-semaphore done)))
                 (true (sb-thread:wait-on-semaphore done :timeout 30))
                 (is equal '(:test 2 :pass 1 :fail 1 :error 0) summary)
                 (is string= "bad" (getf (first failures) :var))
                 (is string= "fail" (getf (first failures) :type)))
               ;; The Clojure docset: every public var's arglists and docstring.
               (let ((vars (backend-value #'c:request-clojure-docs-vars conn))
                     (dir (merge-pathnames (format nil "cadre-cljdocs-~d/" (random 1000000)) (uiop:temporary-directory))))
                 (true (find-if (lambda (v) (and (equal (first v) "clojure.core") (equal (second v) "map"))) vars))
                 (unwind-protect
                      (progn
                        (true (> (c:write-clojure-docset vars :directory dir) 500))
                        (let ((record (find "clojure.core" (docset:find-records (docset:open-docset dir) "map")
                                            :key #'docset:record-package :test #'equal)))
                          (true (search "lazy seq" (docset:record-summary record)) (docset:record-summary record))))
                   (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
               ;; A project's docset: just its namespaces, reloaded.
               (backend-value #'c:backend-eval conn "(ns probe.docs) (defn documented \"Does a thing.\" [x] x)" :package "user")
               (let ((vars (backend-value #'c:request-clojure-docs-vars conn :code (c:clojure-project-docs-code '("probe.docs")))))
                 (is equal '(("probe.docs" "documented" "function" ("[x]") "Does a thing.")) vars))
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

;;; The Clojure docset, from docstrings and (when downloaded) ClojureDocs

(define-test clojure-docset-entries :parent cadre-tests
  (let* ((vars '(("clojure.core" "map" "function" ("[f coll]" "[f c1 c2]") "Returns a lazy sequence.")
                 ("clojure.core" "map-indexed" "function" ("[f coll]") "")))
         (export (c:parse-json "{\"vars\": [{\"ns\": \"clojure.core\", \"name\": \"map\", \"href\": \"/clojure.core/map\",
                                \"examples\": [{\"body\": \"(map inc [1 2])\\n;;=> (2 3)\"}],
                                \"see-alsos\": [{\"to-var\": {\"ns\": \"clojure.core\", \"name\": \"map-indexed\"}}],
                                \"notes\": []}]}"))
         (plain (first (c:clojure-docset-entries vars)))
         (merged (first (c:clojure-docset-entries vars (c::clojuredocs-index export)))))
    (is string= "clojure.core/map" (second plain))
    (is equal '("Syntax" "Description") (mapcar #'first (getf (cddr plain) :sections)))
    (is equal (list :code-block "lisp" (format nil "(map f coll)~%(map f c1 c2)"))
        (second (assoc "Syntax" (getf (cddr plain) :sections) :test #'string=)))
    (is equal '("Syntax" "Description" "Examples (ClojureDocs)" "See Also (ClojureDocs)" "ClojureDocs")
        (mapcar #'first (getf (cddr merged) :sections)))
    (true (find '(:ref "clojure.core/map-indexed" "clojure.core/map-indexed")
                (second (assoc "See Also (ClojureDocs)" (getf (cddr merged) :sections) :test #'string=))
                :test #'equal)
          "a see-also in the docset is a link")))

;;; Project files: EDN, deps.edn, project.clj

(define-test edn-reading :parent cadre-tests
  (is equal '(:map ((:keyword "a") . 1) ((:keyword "b") :vector "x" (:symbol "y") :nil))
      (c:read-edn "{:a 1, :b [\"x\" y nil]}"))
  (is equal '(:set (:keyword "k")) (c:read-edn "#{:k}"))
  (is equal '(:vector 1 3) (c:read-edn "[1 #_2 3] ; a comment"))
  (is equal "2026-01-01" (c:read-edn "#inst \"2026-01-01\""))
  (is equal '((:list (:symbol "a")) (:list (:symbol "b"))) (c:read-edn-forms "(a) (b)"))
  (fail (c:read-edn "{:a") 'c:edn-error))

(define-test clojure-project-files :parent cadre-tests
  (let ((dir (merge-pathnames (format nil "cadre-cljproj-~d/" (random 1000000)) (uiop:temporary-directory))))
    (ensure-directories-exist dir)
    (unwind-protect
         (progn
           (with-open-file (o (merge-pathnames "deps.edn" dir) :direction :output :if-exists :supersede)
             (write-string "{:paths [\"src\"]
 :deps {org.clojure/clojure {:mvn/version \"1.12.0\"}
        io.github.x/lib {:git/tag \"v1.0\" :git/sha \"abcdef0123\"}
        my/local {:local/root \"../local\"}}
 :aliases {:dev {:extra-paths [\"dev\"]} :test {}}}" o))
           (let ((project (c:read-clojure-project-file dir :deps)))
             (is equal '(("org.clojure/clojure" . "1.12.0") ("io.github.x/lib" . "v1.0") ("my/local" . "local ../local"))
                 (getf project :deps))
             (is equal '("dev" "test") (getf project :aliases)))
           (with-open-file (o (merge-pathnames "project.clj" dir) :direction :output :if-exists :supersede)
             (write-string "(defproject my-app \"0.1.0\"
  :description \"x\"
  :dependencies [[org.clojure/clojure \"1.12.0\"] [ring \"1.9.0\"]]
  :profiles {:dev {:dependencies []} :uberjar {}})" o))
           (let ((project (c:read-clojure-project-file dir :lein)))
             (is string= "my-app" (getf project :name))
             (is string= "0.1.0" (getf project :version))
             (is equal '(("org.clojure/clojure" . "1.12.0") ("ring" . "1.9.0")) (getf project :deps))
             (is equal '("dev" "uberjar") (getf project :aliases))))
      (uiop:delete-directory-tree dir :validate t)))
  (is equal (list c:*clojure-command* "-P" "-M:dev:test") (c:clojure-fetch-command :deps '("dev" "test")))
  (is equal (list c:*clojure-command* "-P") (c:clojure-fetch-command :deps nil))
  (is equal (list c:*leiningen-command* "with-profile" "+dev" "deps") (c:clojure-fetch-command :lein '("dev"))))

;;; The program the ▶ button runs: Racket's main submodule, Clojure's -main

(define-test project-programs :parent cadre-tests
  (let ((dir (merge-pathnames (format nil "cadre-apps-~d/" (random 1000000)) (uiop:temporary-directory))))
    (flet ((put (name text)
             (let ((file (merge-pathnames name dir)))
               (ensure-directories-exist file)
               (with-open-file (o file :direction :output :if-exists :supersede) (write-string text o)))))
      (unwind-protect
           (progn
             (is eq nil (c:racket-project-app dir))
             (put "util.rkt" "#lang racket/base (provide f) (define (f) 1)")
             (is eq nil (c:racket-project-app dir) "a module with no main submodule isn't a program")
             (put "tool.rkt" (format nil "#lang racket/base~%(module+ main (displayln 1))"))
             (is string= "tool.rkt" (getf (c:racket-project-app dir) :title))
             (put "main.rkt" (format nil "#lang racket/base~%(module main racket/base (displayln 2))"))
             (is string= "main.rkt" (getf (c:racket-project-app dir) :title) "main.rkt comes first")
             (is eq nil (c:clojure-project-app dir) "no project file, no Clojure program")
             (put "deps.edn" "{:paths [\"src\"]}")
             (put "src/app/core.clj" (format nil "(ns app.core)~%(defn -main [& args] (println :hi))"))
             (put "src/app/other.clj" (format nil "(ns app.other)~%(defn -main [] nil)"))
             (is string= "app.core" (getf (c:clojure-project-app dir) :namespace) "the one under src/ with -main")
             (put "deps.edn" "{:paths [\"src\"] :aliases {:run {:main-opts [\"-m\" \"app.other\"]}}}")
             (is string= "app.other" (getf (c:clojure-project-app dir) :namespace) "an alias's -m comes first")
             (delete-file (merge-pathnames "deps.edn" dir))
             (put "project.clj" "(defproject app \"0.1\" :main app.core)")
             (is string= "app.core/-main" (getf (c:clojure-project-app dir) :title)))
        (uiop:delete-directory-tree dir :validate t)))))
