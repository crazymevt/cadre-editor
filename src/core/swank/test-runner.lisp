;;;; test-runner.lisp — a project's tests, for the Tests page
;;;;
;;;; SOURCE-TESTS finds the tests in Lisp source (parachute's define-test,
;;;; FiveAM's test), so the page lists them before anything is loaded, each
;;;; with where it is. The other Lisp runs them through a small helper Cadre
;;;; sends it (test-runner-remote.lisp, kept as text in +TEST-RUNNER-HELPER+),
;;;; which answers with each test's status and its failures' messages.

(in-package #:cadre)

(defparameter +test-runner-helper+
  #.(uiop:read-file-string (merge-pathnames "test-runner-remote.lisp" (or *compile-file-truename* *load-truename*)))
  "The helper's source, evaluated in the other Lisp before running tests.")

(defstruct (source-test (:conc-name source-test-))
  "A test found in the source. NAME is as written; KEY is how results name
it (TEST-KEY); PARENT is a parachute parent's key."
  name key framework package parent line
  (file nil)                            ; set by whoever read the file
  (status nil)                          ; nil (not run), :passed, :failed, :skipped
  (messages '()))

(defun test-key (name)
  "How results name the test written NAME: a symbol's name in upper case
\(without its package), or a string's text."
  (let ((name (string-trim " " name)))
    (cond ((zerop (length name)) "")
          ((char= (char name 0) #\") (string-trim "\"" name))
          ((char= (char name 0) #\|) (string-trim "|" name))
          (t (let ((colon (position #\: name :from-end t)))
               (string-upcase (if colon (subseq name (1+ colon)) name)))))))

(defun fiveam-source-p (text)
  (or (search "fiveam" text :test #'char-equal)
      (search "5am" text :test #'char-equal)
      (search "(def-suite" text :test #'char-equal)))

(defun source-tests (text)
  "The tests TEXT (Lisp source) defines, in order: parachute's (define-test
name …), nested ones too, and FiveAM's (test name …) where the file uses FiveAM."
  (let ((tests '())
        (package nil)
        (fiveam (fiveam-source-p text))
        (*newline-positions* (cons text (newline-positions text))))
    (labels ((option (elements key)
               (loop for (k v) on elements
                     when (and v (eq (node-kind k) :atom) (string-equal key (node-text k)))
                       return v))
             (add (name-node framework parent)
               (let ((name (if (eq (node-kind name-node) :list)
                               (let ((first (first (form-elements name-node text))))
                                 (and first (node-text first)))
                               (node-text name-node))))
                 (when name
                   (let ((test (make-source-test :name name :key (test-key name) :framework framework
                                                 :package package :parent parent
                                                 :line (line-at-position text (node-start name-node)))))
                     (push test tests)
                     test))))
             (walk (nodes depth parent)
               (dolist (node nodes)
                 (when (eq (node-kind node) :list)
                   (let* ((elements (form-elements node text))
                          (head (operator-name (first elements)))
                          (head-text (and (first elements) (eq (node-kind (first elements)) :atom)
                                          (string-downcase (node-text (first elements))))))
                     (cond ((null head))
                           ((and (string= head "in-package") (second elements) (eq (node-kind (second elements)) :atom))
                            (setf package (outline-package-name (node-text (second elements)))))
                           ((and (string= head "define-test") (second elements))
                            (let* ((explicit (option (cddr elements) ":parent"))
                                   (test (add (second elements) :parachute
                                              (cond ((and explicit (eq (node-kind explicit) :atom))
                                                     (test-key (node-text explicit)))
                                                    (t parent)))))
                              ;; Tests defined inside it are its children.
                              (when (and test (< depth 6))
                                (walk (cddr elements) (1+ depth) (source-test-key test)))))
                           ((and (string= head "test") (second elements)
                                 (or fiveam (member head-text '("5am:test" "fiveam:test" "it.bese.fiveam:test") :test #'string=)))
                            (add (second elements) :fiveam nil))
                           ((and (< depth 4) (member head *definition-containers* :test #'string=))
                            (walk (rest elements) (1+ depth) parent))))))))
      (walk (read-form-tree text) 0 nil))
    (nreverse tests)))

(defun parse-test-results (reply)
  "From the helper's RUN: (key status messages), STATUS a keyword."
  (loop for (key status messages) in reply
        when (stringp key)
          collect (list key
                        (cond ((equal status "passed") :passed)
                              ((equal status "skipped") :skipped)
                              (t :failed))
                        (remove-if-not #'stringp messages))))

(defun apply-test-results (tests results package)
  "Set the status and messages of TESTS (source-tests) from RESULTS (as
from PARSE-TEST-RESULTS) of tests run in PACKAGE. The tests updated."
  (loop for (key status messages) in results
        for test = (find-if (lambda (test) (and (equal (source-test-key test) key)
                                                (equal (source-test-package test) package)))
                            tests)
        when test
          do (setf (source-test-status test) status
                   (source-test-messages test) messages)
          and collect test))

(defun test-roots (tests)
  "TESTS whose parent isn't among them: running these runs the rest."
  (remove-if (lambda (test)
               (and (source-test-parent test)
                    (find-if (lambda (other) (and (equal (source-test-key other) (source-test-parent test))
                                                  (equal (source-test-package other) (source-test-package test))))
                             tests)))
             tests))

(defun test-run-groups (tests)
  "TESTS grouped for the helper's RUN: ((framework package names…) …)."
  (let ((groups '()))
    (dolist (test tests)
      (let* ((framework (string-downcase (symbol-name (source-test-framework test))))
             (group (find-if (lambda (g) (and (equal (first g) framework) (equal (second g) (source-test-package test))))
                             groups)))
        (if group
            (setf (cddr group) (append (cddr group) (list (source-test-name test))))
            (push (list framework (source-test-package test) (source-test-name test)) groups))))
    (nreverse groups)))
