(in-package #:cadre-tests)

;;; A project's libraries (core libraries.lisp): the .asd's :depends-on,
;;; edited as text, and searching Quicklisp's systems.

(defparameter *asd* "(defsystem \"app\"
  :description \"An app; with (parens) in a string\"
  :depends-on (\"alexandria\"
               #:cl-ppcre ; regular expressions
               #+sbcl \"sb-posix\"
               (:version \"str\" \"0.19\")
               (:feature :unix \"osicat\"))
  :components ((:file \"main\")))

#| (defsystem \"commented-out\") |#
(asdf:defsystem #:app/tests
  :depends-on (#:app #:parachute)
  :components ((:file \"tests\")))

(defsystem :bare :components ((:file \"x\")))
")

(define-test asd-dependencies :parent cadre-tests
  (is equal '("app" "app/tests" "bare") (mapcar #'first (c:asd-systems *asd*)))
  (is equal '("alexandria" "cl-ppcre" "sb-posix" "str" "osicat") (c:asd-dependency-names *asd* "app"))
  (is equal '("app" "parachute") (c:asd-dependency-names *asd* "APP/TESTS"))
  (is equal '() (c:asd-dependency-names *asd* "bare"))
  (fail (c:asd-dependency-names *asd* "nowhere") c:editor-error)
  ;; Adding: on a line of its own, lined up, written as the first one is.
  (let ((added (c:apply-edit *asd* (c:asd-add-dependency *asd* "app" "Dexador"))))
    (true (search "(:feature :unix \"osicat\")
               \"dexador\")" added) added)
    (is equal '("alexandria" "cl-ppcre" "sb-posix" "str" "osicat" "dexador") (c:asd-dependency-names added "app")))
  ;; On one line, it stays one line, in the style there (#:).
  (let ((added (c:apply-edit *asd* (c:asd-add-dependency *asd* "app/tests" "fiveam"))))
    (true (search ":depends-on (#:app #:parachute #:fiveam)" added) added))
  (is equal nil (c:asd-add-dependency *asd* "app" "cl-ppcre"))
  ;; No :depends-on yet: one after the name.
  (let ((added (c:apply-edit *asd* (c:asd-add-dependency *asd* "bare" "alexandria"))))
    (true (search "(defsystem :bare :depends-on (\"alexandria\") :components" added) added))
  (let* ((text "(defsystem \"x\"
  :version \"1\")")
         (added (c:apply-edit text (c:asd-add-dependency text "x" "a"))))
    (is string= "(defsystem \"x\"
  :depends-on (\"a\")
  :version \"1\")" added))
  (let* ((text "(defsystem \"x\" :depends-on ())")
         (added (c:apply-edit text (c:asd-add-dependency text "x" "a"))))
    (is string= "(defsystem \"x\" :depends-on (\"a\"))" added))
  ;; Removing takes the space before it too, or after it for the first.
  (let ((removed (c:apply-edit *asd* (c:asd-remove-dependency *asd* "app" "cl-ppcre"))))
    (true (search "(\"alexandria\" ; regular expressions" removed) removed)
    (is equal '("alexandria" "sb-posix" "str" "osicat") (c:asd-dependency-names removed "app")))
  (let ((removed (c:apply-edit *asd* (c:asd-remove-dependency *asd* "app/tests" "app"))))
    (true (search ":depends-on (#:parachute)" removed) removed))
  (let ((removed (c:apply-edit *asd* (c:asd-remove-dependency *asd* "app" "str"))))
    (is equal '("alexandria" "cl-ppcre" "sb-posix" "osicat") (c:asd-dependency-names removed "app")))
  (is equal nil (c:asd-remove-dependency *asd* "app" "nothing"))
  ;; Taking out the only one takes out :depends-on.
  (let ((text "(defsystem \"x\"
  :depends-on (\"a\")
  :version \"1\")"))
    (is string= "(defsystem \"x\"
  :version \"1\")" (c:apply-edit text (c:asd-remove-dependency text "x" "a")))))

(define-test library-search :parent cadre-tests
  (let ((libraries (c:parse-quicklisp-systems '(("cl-ppcre-unicode" "cl-ppcre" 0) ("cl-ppcre" "cl-ppcre" 1)
                                                ("ppcre-tests" "ppcre-tests" 0) ("dexador" "dexador" 0)))))
    (is equal '("cl-ppcre" "cl-ppcre-unicode" "dexador" "ppcre-tests") (mapcar #'c:library-name libraries))
    (true (c:library-installed (first libraries)))
    (is equal '("ppcre-tests" "cl-ppcre" "cl-ppcre-unicode")
        (mapcar #'c:library-name (c:search-libraries libraries "ppcre")))
    (is equal '("cl-ppcre" "cl-ppcre-unicode")
        (mapcar #'c:library-name (c:search-libraries libraries "CL-ppcre")))
    (is = 2 (length (c:search-libraries libraries "" :limit 2))))
  (is eq :no-quicklisp (c:parse-quicklisp-systems :no-quicklisp)))
