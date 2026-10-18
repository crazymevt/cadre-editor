;;;; Rename's use of the running Lisp, and its matches (project-search.lisp).
(in-package #:cadre-ui-tests)

(define-test rename)

(defpackage #:cadre-rename-test-a (:use #:cl) (:export #:widget))
(defpackage #:cadre-rename-test-b (:use #:cl #:cadre-rename-test-a))
(intern "WIDGET" '#:cadre-rename-test-a)

(define-test symbol-homes-from-the-image :parent rename
  ;; The source the image evaluates gives each package's symbol's home:
  ;; B uses A, so its WIDGET is A's; CL-USER would intern its own; a missing
  ;; package gives nil.
  (let* ((packages '("CADRE-RENAME-TEST-A" "CADRE-RENAME-TEST-B" "COMMON-LISP-USER" "NO-SUCH-PACKAGE"))
         (source (ui::symbol-homes-source "widget" packages))
         (value (let ((*package* (find-package :cl-user))) (eval (read-from-string source)))))
    (is equal '("CADRE-RENAME-TEST-A" "CADRE-RENAME-TEST-A" "COMMON-LISP-USER" nil) value)
    ;; As the image's reply comes back: (output printed-value).
    (let ((table (ui::parse-symbol-homes (list "" (prin1-to-string value)) packages)))
      (is equal "CADRE-RENAME-TEST-A" (gethash "CADRE-RENAME-TEST-B" table))
      (false (gethash "NO-SUCH-PACKAGE" table)))
    ;; A reply that isn't the list asked for is refused.
    (false (ui::parse-symbol-homes (list "" "(1 2)") packages))
    (false (ui::parse-symbol-homes (list "" "#.(error \"x\")") packages)))
  (is equal "Widget" (ui::symbol-name-for-lookup "|Widget|"))
  (is equal "WIDGET" (ui::symbol-name-for-lookup "widget")))

(define-test classified-rename-matches :parent rename
  (let* ((text (format nil "(in-package #:cadre-rename-test-b)~%(defun f () (widget) (let ((widget 1)) widget))"))
         (contexts (c:symbol-contexts text "widget"))
         (resolve (lambda (p) (and (string= p "CADRE-RENAME-TEST-B") "CADRE-RENAME-TEST-A"))))
    ;; Columns are within the line; locals and the symbol are told apart.
    (is equal '((1 13 19 :same nil) (1 28 34 :local "local binding") (1 39 45 :local "local binding"))
        (ui::classified-matches contexts "CADRE-RENAME-TEST-A" resolve))
    (true (ui::rename-checked-kind-p :same))
    (true (ui::rename-checked-kind-p :export))
    (false (ui::rename-checked-kind-p :unknown))
    (false (ui::rename-checked-kind-p :local))))

(define-test offset-line-columns :parent rename
  (let ((starts (coerce (c::line-starts (format nil "ab~%cde~%f")) 'vector)))
    (is equal '(0 1) (multiple-value-list (ui::offset-line-column starts 1)))
    (is equal '(1 2) (multiple-value-list (ui::offset-line-column starts 5)))
    (is equal '(2 0) (multiple-value-list (ui::offset-line-column starts 7)))))
