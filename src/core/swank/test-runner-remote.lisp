;;;; test-runner-remote.lisp — the test explorer's helper, for the other Lisp
;;;;
;;;; Not compiled into Cadre: test-runner.lisp sends this text to the Lisp,
;;;; which evaluates it. RUN runs tests with parachute or FiveAM, without the
;;;; debugger, and answers with plain data. Neither framework need be loaded
;;;; when this is read: their symbols are looked up when called.

(defpackage #:cadre-test-runner
  (:use #:cl)
  (:export #:run))

(in-package #:cadre-test-runner)

(defun sym (name package)
  (or (and (find-package package) (find-symbol name package))
      (error "~a isn't loaded (no ~a::~a)" package package name)))

(defun call (name package &rest args)
  (apply (sym name package) args))

(defun test-key (name)
  "How Cadre names a test: a symbol's name, or a string as it is."
  (if (symbolp name) (symbol-name name) (princ-to-string name)))

(defun clip (text)
  (let ((text (string-trim '(#\Space #\Newline) (princ-to-string text))))
    (if (> (length text) 2000) (concatenate 'string (subseq text 0 2000) "…") text)))

(defun pkg (name)
  "The package NAME names, however it is written; CL-USER for none."
  (if (or (null name) (string= name ""))
      (find-package :common-lisp-user)
      (or (find-package name) (find-package (string-upcase name))
          (error "No package ~a: is the test system loaded?" name))))

(defun read-name (text package)
  "The test name TEXT (as written in the source) read in PACKAGE."
  (let ((*package* (pkg package))
        (*read-eval* nil))
    (read-from-string text)))

;;; Parachute

(defun parachute-results (report)
  "(key status messages) for each test the parachute REPORT ran."
  (let ((out '())
        (seen (make-hash-table :test 'eq)) ; a child's result is under its parent and the report too
        (test-result (sym "TEST-RESULT" "PARACHUTE")))
    (labels ((failures (result)
               ;; The failed checks under RESULT, not inside child tests.
               (loop for r across (call "RESULTS" "PARACHUTE" result)
                     when (and (eq (call "STATUS" "PARACHUTE" r) :failed) (not (typep r test-result)))
                       collect (clip (call "FORMAT-RESULT" "PARACHUTE" r :extensive))))
             (walk (result)
               (when (and (typep result test-result) (not (gethash result seen)))
                 (setf (gethash result seen) t)
                 (let ((test (call "EXPRESSION" "PARACHUTE" result)))
                   (push (list (test-key (call "NAME" "PARACHUTE" test))
                               (string-downcase (symbol-name (call "STATUS" "PARACHUTE" result)))
                               (let ((messages (failures result))
                                     (description (call "DESCRIPTION" "PARACHUTE" result)))
                                 (if (and description (eq (call "STATUS" "PARACHUTE" result) :failed))
                                     (cons (clip description) messages)
                                     messages)))
                         out)))
               (when (typep result (sym "PARENT-RESULT" "PARACHUTE"))
                 (loop for r across (call "RESULTS" "PARACHUTE" result) do (walk r)))))
      (walk report))
    (nreverse out)))

(defun run-parachute (package names)
  (loop for text in names
        append (handler-case
                   (let* ((name (read-name text package))
                          (test (or (call "FIND-TEST" "PARACHUTE" name (pkg package))
                                    (error "No test ~a in ~a: is it loaded?" text package))))
                     (parachute-results (call "TEST" "PARACHUTE" test :report (sym "QUIET" "PARACHUTE"))))
                 (error (e)
                   (list (list (test-key (ignore-errors (read-name text package))) "failed" (list (clip e))))))))

;;; FiveAM

(defun fiveam-results (name)
  "(key status messages) for each test FiveAM runs for NAME, a test or suite."
  (let ((table (make-hash-table :test 'equal))
        (order '()))
    (progv (list (sym "*ON-ERROR*" "IT.BESE.FIVEAM") (sym "*ON-FAILURE*" "IT.BESE.FIVEAM")
                 (sym "*TEST-DRIBBLE*" "IT.BESE.FIVEAM") (sym "*PRINT-NAMES*" "IT.BESE.FIVEAM"))
        (list nil nil (make-broadcast-stream) nil)
      (dolist (r (call "RUN" "IT.BESE.FIVEAM" name))
        (let* ((key (test-key (call "NAME" "IT.BESE.FIVEAM" (call "TEST-CASE" "IT.BESE.FIVEAM" r))))
               (entry (or (gethash key table)
                          (progn (push key order)
                                 (setf (gethash key table) (list "passed" '())))))
               (kind (symbol-name (type-of r))))
          (cond ((string= kind "TEST-PASSED"))
                ((string= kind "TEST-SKIPPED")
                 (when (string= (first entry) "passed") (setf (first entry) "skipped")))
                (t (setf (first entry) "failed")
                   (push (clip (call "REASON" "IT.BESE.FIVEAM" r)) (second entry)))))))
    (loop for key in (nreverse order)
          collect (destructuring-bind (status messages) (gethash key table)
                    (list key status (reverse messages))))))

(defun run-fiveam (package names)
  (loop for text in names
        append (handler-case
                   (let ((name (read-name text package)))
                     (unless (call "GET-TEST" "IT.BESE.FIVEAM" name)
                       (error "No test ~a in ~a: is it loaded?" text package))
                     (fiveam-results name))
                 (error (e)
                   (list (list (test-key (ignore-errors (read-name text package))) "failed" (list (clip e))))))))

(defun run (framework package names)
  "Run the tests NAMES (as written in the source) of PACKAGE with FRAMEWORK
\(\"parachute\" or \"fiveam\"): (key status messages) for each test run,
children included; STATUS passed, failed or skipped."
  (if (string-equal framework "fiveam")
      (run-fiveam package names)
      (run-parachute package names)))
