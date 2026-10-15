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
  (end-line nil)                        ; the form's last line (Racket: to place failures)
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

;;; Clojure: clojure.test's deftest

(defun sx-line (text position)
  (count #\Newline text :end (min position (length text))))

(defun sx-head (text element)
  "The operator of the list ELEMENT, without its namespace, or nil."
  (when (char= #\( (char text (car element)))
    (let ((first (first (sx-list-elements text (car element)))))
      (when first
        (let ((head (sx-text text first)))
          (subseq head (1+ (or (position #\/ head :from-end t) -1))))))))

(defun sx-top-elements (text)
  (let ((i 0) (n (length text)) (elements '()))
    (loop
      (setf i (sx-skip-blank text i))
      (when (>= i n) (return (nreverse elements)))
      (let ((end (sx-element-end text i)))
        (push (cons i end) elements)
        (setf i (max end (1+ i)))))))

(defun clojure-source-tests (text)
  "The deftests in TEXT (Clojure source), each in the namespace of the (ns …) before it."
  (let ((tests '()) (namespace nil))
    (dolist (element (sx-top-elements text))
      (let ((head (sx-head text element)))
        (when head
          (let* ((parts (sx-list-elements text (car element)))
                 (name (find-if-not (lambda (e) (char= #\^ (char text (car e)))) (rest parts))))
            (cond ((and (member head '("ns" "in-ns") :test #'string=) name)
                   (setf namespace (string-left-trim "'" (sx-text text name))))
                  ((and (string= head "deftest") name)
                   (let ((n (sx-text text name)))
                     (push (make-source-test :name n :key n :framework :clojure :package namespace
                                             :line (sx-line text (car name)) :end-line (sx-line text (cdr element)))
                           tests))))))))
    (nreverse tests)))

;;; Racket: rackunit's test-case and test-suite, and checks in (module+ test)

(defun racket-check-p (text element)
  (let ((head (sx-head text element)))
    (and head (> (length head) 5) (string= "check" head :end2 5))))

(defun racket-source-tests (text)
  "The test-cases and test-suites in TEXT (Racket source), and a (module+
test) block with checks of its own as one test, \"module+ test\"."
  (let ((tests '()))
    (labels ((add (name element parent)
               (let ((test (make-source-test :name name :key name :framework :rackunit :parent parent
                                             :line (sx-line text (car element)) :end-line (sx-line text (cdr element)))))
                 (push test tests)
                 test))
             (walk (elements depth parent)
               (dolist (element elements)
                 (let ((head (sx-head text element)))
                   (when head
                     (let ((parts (sx-list-elements text (car element))))
                       (cond ((and (member head '("test-case" "test-suite") :test #'string=) (second parts)
                                   (char= #\" (char text (car (second parts)))))
                              (let ((test (add (string-trim "\"" (sx-text text (second parts))) element parent)))
                                (when (< depth 8) (walk (cddr parts) (1+ depth) (source-test-key test)))))
                             ((and (member head '("module+" "module*") :test #'string=) (second parts)
                                   (string= "test" (sx-text text (second parts))))
                              (let ((block (and (some (lambda (e) (racket-check-p text e)) (cddr parts))
                                                (add "module+ test" element parent))))
                                (walk (cddr parts) (1+ depth) (if block (source-test-key block) parent))))
                             ((< depth 8) (walk (rest parts) (1+ depth) parent)))))))))
      (walk (sx-top-elements text) 0 nil))
    (nreverse tests)))

(defun mode-source-tests (text mode)
  "The tests in TEXT, a file in MODE."
  (case mode
    (lisp-mode (source-tests text))
    (clojure-mode (clojure-source-tests text))
    (racket-mode (racket-source-tests text))))

(defun parse-raco-test-output (output)
  "The failures and errors raco test reported in OUTPUT: (name line message),
NAME the test-case's (or nil), LINE from its location (from 0, or nil)."
  (let ((failures '()))
    (dolist (block (ppcre:split "(?m)^-{20}\\s*$" output))
      (let* ((lines (remove "" (mapcar (lambda (l) (string-right-trim '(#\Return) l))
                                        (uiop:split-string block :separator '(#\Newline)))
                            :test #'string=))
             (marker (position-if (lambda (l) (member (string-trim " " l) '("FAILURE" "ERROR") :test #'string=)) lines)))
        (when marker
          (let* ((name (and (plusp marker) (string-trim " " (nth (1- marker) lines))))
                 (location (find-if (lambda (l) (eql 0 (search "location:" l))) lines))
                 (line (and location
                            (multiple-value-bind (match groups) (ppcre:scan-to-strings ":(\\d+):\\d+\\s*$" location)
                              (and match (1- (parse-integer (aref groups 0))))))))
            (push (list (and name (not (eql 0 (search "raco test" name))) name)
                        line
                        (format nil "~{~a~^~%~}" (subseq lines marker)))
                  failures)))))
    (nreverse failures)))

(defun apply-raco-results (tests failures exit-code output)
  "Set the status of TESTS (one file's rackunit tests) from what raco test
said: FAILURES (from PARSE-RACO-TEST-OUTPUT), its EXIT-CODE and OUTPUT."
  (let ((messages (make-hash-table :test 'eq)))
    (dolist (failure failures)
      (destructuring-bind (name line message) failure
        (let ((test (or (and name (find name tests :key #'source-test-key :test #'equal))
                        (and line
                             (first (sort (remove-if-not (lambda (test) (<= (source-test-line test) line
                                                                            (or (source-test-end-line test) line)))
                                                         tests)
                                          #'< :key (lambda (test) (- (or (source-test-end-line test) 0)
                                                                     (source-test-line test))))))
                        (first tests))))
          (when test (push message (gethash test messages))))))
    (dolist (test tests)
      (let ((own (gethash test messages)))
        (cond (own (setf (source-test-status test) :failed
                         (source-test-messages test) (reverse own)))
              ((and (not (eql exit-code 0)) (null failures))
               ;; It didn't run: a compile error, say.
               (setf (source-test-status test) :failed
                     (source-test-messages test)
                     (list (let ((text (string-trim '(#\Space #\Newline) output)))
                             (if (> (length text) 1500) (subseq text (- (length text) 1500)) text)))))
              (t (setf (source-test-status test) :passed
                       (source-test-messages test) '())))))
    tests))
