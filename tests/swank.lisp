(in-package #:cadre-tests)

(define-test sexp-codec :parent cadre-tests
  (let ((msg (c:read-sexp "(:return (:ok (\"a \\\"q\\\"\" 1 2.5 -3/4 nil t swank::foo |Odd Sym| #\\a #\\Space)) 12)")))
    (is eq :return (first msg))
    (is equal "a \"q\"" (first (second (second msg))))
    (destructuring-bind (s i f r n tt sym odd ch sp) (second (second msg))
      (declare (ignore s))
      (is = 1 i) (is = 2.5d0 f) (is = -3/4 r) (is eq nil n) (is eq t tt)
      (is string= "swank::foo" (c:remote-symbol-name sym))
      (true (c:remote-symbol= sym "FOO"))
      (is string= "|Odd Sym|" (c:remote-symbol-name odd))
      (is char= #\a ch) (is char= #\Space sp))
    (is = 12 (third msg)))
  (is equal '(1 . 2) (c:read-sexp "(1 . 2)"))
  (is string= "#<FOO {1}>" (c:read-sexp "#<FOO {1}>"))
  (fail (c:read-sexp "(1 2") 'error)
  (is string= "(:emacs-rex (swank:connection-info) \"CL-USER\" t 1)"
      (c:sexp-to-string (list :emacs-rex (list (c:remote-symbol "swank:connection-info")) "CL-USER" t 1)))
  (is string= "(quote (:a \"b\\\\c\"))" (c:sexp-to-string '(quote (:a "b\\c"))))
  (fail (c:sexp-to-string '(cadre-tests::foo)) 'error)
  (is equalp (concatenate '(vector (unsigned-byte 8))
                          (map 'vector #'char-code "000009")
                          (sb-ext:string-to-octets "(:a \"é\")" :external-format :utf-8))
      (cadre::encode-message '(:a "é")) "the length counts UTF-8 octets"))

(defun marker-p (x) (and (c:remote-symbol-p x) (search "cursor-marker" (c:remote-symbol-name x))))

(defun raw-form (string &optional (line 0) column)
  (let ((s (syntax-of string)))
    (c:raw-form-at s line (or column (length (c:text-line-string (c::syntax-text s) line))))))

(defun simplify-form (form)
  (mapcar (lambda (x) (cond ((marker-p x) :cursor) ((consp x) (simplify-form x)) (t x))) form))

(define-test autodoc-forms :parent cadre-tests
  (is equal '("format" "t" "" :cursor) (simplify-form (raw-form "(format t ")))
  (is equal '("format" :cursor) (simplify-form (raw-form "(format")))
  (is equal '("defun" "foo" "(x)" ("+" "x" "" :cursor)) (simplify-form (raw-form "(defun foo (x) (+ x ")))
  (is equal '("list" "(a b)" "" :cursor) (simplify-form (raw-form "(list (a b) ")))
  (is equal nil (raw-form "foo")))

(define-test buffer-packages :parent cadre-tests
  (let ((s (syntax-of (format nil "(defpackage #:p (:use :cl))~%(in-package #:cadre-ui)~%(defun x ())~%(in-package \"Other\")~%y"))))
    (is eq nil (c:buffer-package-name s 0))
    (is string= "CADRE-UI" (c:buffer-package-name s 2))
    (is string= "Other" (c:buffer-package-name s 4)))
  (let ((s (syntax-of "(foo bar:baz :key)")))
    (is string= "bar:baz" (c:symbol-at s 0 8))
    (is equal '("ba" 5) (multiple-value-list (c:symbol-prefix-at s 0 7)))
    (is equal '(":k" 13) (multiple-value-list (c:symbol-prefix-at s 0 15)))))

(define-test locations :parent cadre-tests
  (let ((l (c:parse-location '(:location (:file "/a.lisp") (:position 10) nil))))
    (is equal "/a.lisp" (getf l :file))
    (is = 9 (getf l :position)))
  (let ((l (c:parse-location '(:location (:buffer "b") (:offset 5 20) nil))))
    (is equal "b" (getf l :buffer))
    (is = 24 (getf l :position)))
  (is equal '(:error "nope") (c:parse-location '(:error "nope"))))

;;; Against a real Lisp, started with the bundled Swank

(defvar *events* '())
(defvar *events-lock* (sb-thread:make-mutex))

(defun start-test-lisp ()
  "A connection to a fresh SBCL running the bundled Swank, and its process."
  (let ((ready (sb-thread:make-semaphore)) (port nil) (failure nil) (inferior nil))
    (setf inferior (c:start-inferior-lisp
                    :command '("sbcl" "--noinform" "--no-userinit")
                    :on-port (lambda (p) (setf port p) (sb-thread:signal-semaphore ready))
                    :on-exit (lambda (why) (setf failure why) (sb-thread:signal-semaphore ready))))
    (sb-thread:wait-on-semaphore ready :timeout 180)
    (when failure (error "Lisp failed to start: ~a" failure))
    (unless port (error "Swank never started"))
    (let ((conn (c:swank-connect "localhost" port
                                 :handler (lambda (c event) (declare (ignore c))
                                            (sb-thread:with-mutex (*events-lock*) (push event *events*))))))
      (setf (c:connection-process conn) inferior)
      (let ((done (sb-thread:make-semaphore)))
        (c:swank-start-session conn :on-ready (lambda (c) (declare (ignore c)) (sb-thread:signal-semaphore done)))
        (unless (sb-thread:wait-on-semaphore done :timeout 120) (error "Session setup timed out")))
      conn)))

(defun swank (conn name &rest args)
  (c:swank-eval-sync conn (apply #'c:swank-call name args)))

(defun wait-for (predicate &optional (timeout 10))
  (loop repeat (* timeout 20)
        when (funcall predicate) return t
        do (sleep 0.05)))

(define-test swank-integration :parent cadre-tests
  ;; The handler runs on the reader thread, so it sees the global *EVENTS*.
  (setf *events* '())
  (let ((conn (start-test-lisp)))
    (unwind-protect
         (progn
           (true (search "SBCL" (c:connection-implementation conn)))
           (is string= "COMMON-LISP-USER" (c:connection-package conn))
           ;; Evaluation
           (is string= "=> 3 (2 bits, #x3, #o3, #b11)" (swank conn "swank:interactive-eval" "(+ 1 2)" 3 120))
           ;; Compiling a definition reports notes.
           (multiple-value-bind (notes ok)
               (c:parse-compilation-result
                (swank conn "swank:compile-string-for-emacs"
                       "(defun cadre-test-f (x) (+ x undefined-var))" "test.lisp"
                       '((:position 1) (:line 1 1)) nil nil))
             (true ok)
             (true (find :warning notes :key #'c:compiler-note-severity))
             (true (search "UNDEFINED-VAR" (c:compiler-note-message (first notes))))
             (is string= "test.lisp" (getf (c:compiler-note-location (first notes)) :buffer)))
           ;; Argument hints
           (let ((doc (swank conn "swank:autodoc"
                             (list "format" "t" "" (c:remote-symbol "swank::%cursor-marker%"))
                             :print-right-margin 80)))
             (true (search "===> control-string <===" (first doc)) (first doc)))
           ;; Completion
           (let ((completions (first (swank conn "swank:fuzzy-completions" "mvb" "COMMON-LISP-USER"
                                            :limit 10 :time-limit-in-msec 1000))))
             (true (find "multiple-value-bind" completions :key #'first :test #'string-equal)))
           ;; Definitions, from a compiled file
           (let ((file (merge-pathnames (format nil "cadre-def-~d.lisp" (random 100000)) (uiop:temporary-directory))))
             (with-open-file (o file :direction :output :if-exists :supersede)
               (format o "(defpackage #:cadre-def (:use :cl))~%(in-package #:cadre-def)~%~%(defun target (x)~%  x)~%"))
             (unwind-protect
                  (progn
                    (let ((result (swank conn "swank:compile-file-for-emacs" (namestring file) t)))
                      (true (third result))
                      ;; Compiling only compiles; the client loads the fasl.
                      (swank conn "swank:load-file" (sixth result)))
                    (let* ((defs (swank conn "swank:find-definitions-for-emacs" "cadre-def::target"))
                           (location (c:parse-location (second (first defs)))))
                      (is equal (namestring (truename file)) (namestring (truename (getf location :file)))
                          "~s" location)
                      ;; The position is the start of "(defun target", line 3 (from 0).
                      (let ((text (uiop:read-file-string file)))
                        (true (search "(defun target" text :start2 (getf location :position))
                              "position ~a" (getf location :position))
                        (is = (search "(defun target" text) (getf location :position)))))
               (ignore-errors (delete-file file))
               (ignore-errors (delete-file (compile-file-pathname file)))))
           ;; The REPL: output and results arrive as :write-string events.
           (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
           (c:swank-eval-sync conn (c:swank-call "swank-repl:listener-eval" "(progn (princ \"hi\") (* 6 7))")
                              :thread :repl-thread)
           (true (wait-for (lambda () (find-if (lambda (e) (and (eq (first e) :write-string)
                                                                (search "42" (second e))))
                                               *events*))))
           (true (find-if (lambda (e) (and (eq (first e) :write-string) (search "hi" (second e)))) *events*))
           ;; A macro defined in the REPL: Swank reports where its &body is.
           (c:swank-eval-sync conn (c:swank-call "swank-repl:listener-eval"
                                                 "(defmacro cadre-test-around (x &body body) `(progn ,x ,@body))")
                              :thread :repl-thread)
           (true (wait-for (lambda () (find-if (lambda (e) (and (eq (first e) :indentation-update)
                                                                (assoc "cadre-test-around" (second e) :test #'equal)))
                                               *events*))))
           (unwind-protect
                (progn
                  (dolist (e *events*)
                    (when (eq (first e) :indentation-update) (c:learn-indentation (second e))))
                  (is eql 1 (c:indentation-spec "cadre-test-around")))
             (c:learn-indentation '(("cadre-test-around" nil ()))))
           ;; An error enters the debugger: a :debug event with restarts.
           (sb-thread:with-mutex (*events-lock*) (setf *events* '()))
           (c:swank-rex conn (list (c:remote-symbol "swank:interactive-eval") "(error \"boom\")" 3 120)
                        :on-ok #'identity :on-abort #'identity)
           (true (wait-for (lambda () (find :debug *events* :key #'first))))
           (let ((debug (find :debug *events* :key #'first)))
             (destructuring-bind (thread level condition restarts &rest more) (rest debug)
               (declare (ignore more))
               (true (search "boom" (first condition)))
               (true (plusp (length restarts)))
               (swank-eval-in-thread conn thread level))))
      (c:swank-disconnect conn)
      (c:kill-inferior-lisp (c:connection-process conn)))))

(defun swank-eval-in-thread (conn thread level)
  ;; Leave the debugger with its abort restart.
  (c:swank-rex conn (list (c:remote-symbol "swank:throw-to-toplevel")) :thread thread
               :on-ok #'identity :on-abort #'identity)
  (true (wait-for (lambda () (find-if (lambda (e) (and (eq (first e) :debug-return) (= (third e) level)))
                                      *events*)))))

;;; M3: inspector, cross-references, debugger, image classification

(define-test inspector-replies :parent cadre-tests
  (let ((i (c:parse-inspection
            (c:read-sexp "(:title \"#<CONS {1}>\" :id 0 :content ((\"A list\" \"
\" \"Car: \" (:value \"1\" 1) (:action \"[remove]\" 0) (:label \"Slots\")) 6 0 500))"))))
    (is string= "#<CONS {1}>" (c:inspection-title i))
    (is equal '((:text "A list") (:text "
") (:text "Car: ") (:value "1" 1) (:action "[remove]" 0) (:label "Slots"))
        (c:inspection-parts i))
    (is = 6 (c:inspection-next i))
    (false (c:inspection-more i)))
  (multiple-value-bind (parts next more) (c:parse-inspector-range '(("x") 1500 0 500))
    (is equal '((:text "x")) parts)
    (is = 1500 next)
    (true more))
  (is eq nil (c:parse-inspection nil)))

(define-test xref-replies :parent cadre-tests
  (let ((xrefs (c:parse-xrefs-groups
                (c:read-sexp "((:calls (\"(defun bar)\" (:location (:file \"/tmp/a.lisp\") (:position 11) nil))
                                         (\"baz\" (:error \"No source\")))
                               (:references (\"quux\" (:location (:buffer \"x\") (:position 1) nil))))"))))
    (is = 3 (length xrefs))
    (is eq :calls (c:xref-kind (first xrefs)))
    (is string= "(defun bar)" (c:xref-name (first xrefs)))
    (is equal "/tmp/a.lisp" (getf (c:xref-location (first xrefs)) :file))
    (is = 10 (getf (c:xref-location (first xrefs)) :position))
    (true (getf (c:xref-location (second xrefs)) :error))
    (is eq :references (c:xref-kind (third xrefs))))
  (is eq :not-implemented (c:parse-xrefs :calls :not-implemented))
  (is string= "Calls" (c:xref-kind-heading :calls)))

(define-test debugger-replies :parent cadre-tests
  (let ((frames (c:parse-frames (c:read-sexp "((0 \"(/ 1 0)\" (:restartable t)) (1 \"(FOO)\"))"))))
    (is = 2 (length frames))
    (is string= "(/ 1 0)" (c:frame-description (first frames)))
    (true (c:frame-restartable (first frames)))
    (false (c:frame-restartable (second frames))))
  (multiple-value-bind (locals tags)
      (c:parse-frame-locals (c:read-sexp "(((:name \"X\" :id 0 :value \"1\") (:name \"Y\" :id 0 :value \"NIL\")) (\"tag\"))"))
    (is equal '(("X" "1") ("Y" "NIL")) locals)
    (is equal '("tag") tags)))

(define-test stepper-condition :parent cadre-tests
  (true (c:stepper-condition-p '("Evaluating call:" "   [Condition of type SB-EXT:STEP-FORM-CONDITION]" nil)))
  (false (c:stepper-condition-p '("division by zero" "   [Condition of type DIVISION-BY-ZERO]" nil))))

(define-test trace-dialog-replies :parent cadre-tests
  (is string= "foo" (c:trace-spec-name (c:read-sexp "common-lisp-user::foo")))
  (is string= "(setf foo)" (c:trace-spec-name (c:read-sexp "(cl:setf cl-user::foo)")))
  (let ((tree (c:make-trace-tree)))
    ;; A call to F that has not returned, with a finished call to G inside it.
    (is = 2 (c:trace-tree-add tree (c:read-sexp "((0 nil cl-user::f ((0 \"3\")) ((0 \"SWANK-TRACE-DIALOG::STILL-INSIDE\")))
                                                  (1 0 cl-user::g ((0 \"3\") (1 \"4\")) ((0 \"7\"))))")))
    (let ((f (gethash 0 (c:trace-tree-calls tree))))
      (is eq :running (c:trace-call-state f))
      (is string= "(f 3) …" (c:trace-call-text f))
      (is equal '(("f" 0) ("g" 1))
          (mapcar (lambda (l) (list (c:trace-call-name (first l)) (second l))) (c:trace-tree-lines tree)))
      ;; F returns two values; reported again, it is updated, not added.
      (is = 0 (c:trace-tree-add tree (c:read-sexp "((0 nil cl-user::f ((0 \"3\")) ((0 \"8\") (1 \"NIL\"))))")))
      (is string= "(f 3) ⇒ 8, NIL" (c:trace-call-text f))
      (let ((collapsed (make-hash-table)))
        (setf (gethash 0 collapsed) t)
        (is = 1 (length (c:trace-tree-lines tree :collapsed collapsed)))))
    (c:trace-tree-add tree (c:read-sexp "((2 nil cl-user::h () ((0 \":EXITED-NON-LOCALLY\"))) (3 nil cl-user::k () ()))"))
    (is eq :unwound (c:trace-call-state (gethash 2 (c:trace-tree-calls tree))))
    (is string= "(k) ⇒ nothing" (c:trace-call-text (gethash 3 (c:trace-tree-calls tree))))
    (is = 4 (c:trace-tree-count tree))
    (is = 2 (length (c:trace-tree-lines tree :limit 2)))))

(defpackage #:cadre-tests-image (:use #:cl) (:export #:a-function))
(defmacro cadre-tests-image::a-macro () nil)
(defun cadre-tests-image:a-function () nil)
(defvar cadre-tests-image::*a-var* 1)
(defconstant cadre-tests-image::+a-constant+ 2)

(define-test image-classification :parent cadre-tests
  (let* ((names '("a-macro" "a-function" "*a-var*" "+a-constant+" "no-such-thing" "car" "if"
                  "cadre-tests-image:a-function" "no-such-package:x" "cadre-tests-image::a-macro"))
         (source (c:image-classify-source "CADRE-TESTS-IMAGE" names))
         (value (eval (read-from-string source))))
    (is equal '(:macro :function :special-variable :constant :unknown :function :special-operator
                :function :no-package :macro)
        value)
    (is equal value (c:parse-image-classes (list "" (format nil "~s" value)) (length names)))
    (is equal '(nil nil) (c:parse-image-classes (list "" "junk(") 2))
    (false (find-symbol "NO-SUCH-THING" "CADRE-TESTS-IMAGE") "classifying never interns"))
  (is equal '(:no-package :no-package)
      (eval (read-from-string (c:image-classify-source "NO-SUCH-PACKAGE" '("x" "y")))))
  (true (c:classifiable-name-p "foo:bar"))
  (false (c:classifiable-name-p "|odd|"))
  (false (c:classifiable-name-p ":key")))

(defun called-p (string line name &optional (function-p (constantly nil)))
  "Is the head symbol NAME on LINE of STRING surely a call?"
  (let* ((s (syntax-of string))
         (tk (find-if (lambda (tk) (and (eq (c:token-type tk) :symbol)
                                        (string-equal name (c:token-text s line tk))))
                      (c:line-tokens s line))))
    (c:surely-called-p s line tk function-p (c:local-function-names s line))))

(define-test surely-called :parent cadre-tests
  (true (called-p "(foo 1)" 0 "foo") "a top-level form")
  (true (called-p "(defun f (x) (foo x))" 0 "foo") "a DEFUN body")
  (false (called-p "(defun f (foo) foo)" 0 "foo") "not a lambda list (not a head)")
  (true (called-p "(let ((x 1)) (foo x))" 0 "foo") "a LET body")
  (false (called-p "(let ((x 1)) (foo x))" 0 "x") "not a LET binding")
  (true (called-p "(let ((x (foo))) x)" 0 "foo") "an argument of a function (LIST-like binding value)"
        )
  (false (called-p "(case k (foo 1))" 0 "foo") "not a CASE key")
  (true (called-p "(case k (a (foo)))" 0 "foo") "a CASE clause body")
  (true (called-p "(cond ((foo) 1))" 0 "foo") "a COND test")
  (false (called-p "'(foo 1)" 0 "foo") "quoted")
  (false (called-p "(list '(foo 1))" 0 "foo") "quoted argument")
  (false (called-p "(defclass c () ((foo :initarg :foo)))" 0 "foo") "a slot specifier")
  (false (called-p "(flet ((foo () 1)) (foo))" 0 "foo") "a local function")
  (true (called-p "(bar (foo))" 0 "foo" (lambda (name) (string-equal name "bar")))
        "an argument of a function the image knows")
  (false (called-p "(bar (foo))" 0 "foo") "an argument of an unknown operator")
  (true (called-p "(handler-case (foo) (error () 1))" 0 "foo") "HANDLER-CASE's form")
  (false (called-p "(handler-case (foo) (my-error () 1))" 0 "my-error") "not a HANDLER-CASE clause"))
