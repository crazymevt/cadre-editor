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

(define-test rename-rejects-changed-context :parent rename
  ;; The name can still occupy the same columns after becoming quoted data.
  ;; Exercise application itself, for both project and local rename previews.
  (dolist (mode '(:symbol :lexical))
    (let* ((path (merge-pathnames (format nil "cadre-rename-~a.lisp" (c:make-uuid)) (uiop:temporary-directory)))
           (current "(widget)") (written nil)
           (ui::*project-search*
             (ui::make-project-search :mode mode :symbol "widget"
                                      :entry :pattern :replace-entry :replacement
                                      :results-data (list (list path '((0 1 7)))))))
      (setf (gethash path (ui::ps-rename-contexts ui::*project-search*))
            (ui::rename-contexts current '((0 1 7))))
      (with-assistant-functions
          ((gtk:editable-get-text (lambda (entry) (if (eq entry :pattern) "widget" "gadget")))
           (ui::checked-matches #'identity)
           (ui::find-file-buffer (lambda (file) (declare (ignore file)) nil))
           (ui::read-text-file (lambda (file) (declare (ignore file)) current))
           (ui::write-text-file (lambda (file text) (declare (ignore file)) (setf written text))))
        (is equal '(1 1 0) (multiple-value-list (ui::apply-project-replace)))
        (is equal "(gadget)" written)
        ;; A different top-level form may change without invalidating this place.
        (setf current (format nil "(widget)~%(unrelated 1)") written nil)
        (is equal '(1 1 0) (multiple-value-list (ui::apply-project-replace)))
        (is equal (format nil "(gadget)~%(unrelated 1)") written)
        (setf current "'widget " written nil)
        (is equal '(0 0 1) (multiple-value-list (ui::apply-project-replace)))
        (false written)))))

(define-test rename-contexts-track-scope-and-package :parent rename
  (let* ((source (format nil "(in-package #:one)~%(defun f () (widget))~%(defun g () (widget))"))
         (first '(1 13 19)) (second '(2 13 19))
         (before (ui::rename-contexts source (list first second)))
         (changed (ui::rename-contexts
                   (format nil "(in-package #:one)~%(defun f () (widget))~%(defun g () '(widget))")
                   (list first second))))
    (true (gethash first before))
    (true (gethash second before))
    (is equal (gethash first before) (gethash first changed))
    (false (equal (gethash second before) (gethash second changed)))
    (false (equal (gethash first before)
                  (gethash first (ui::rename-contexts
                                  (format nil "(in-package #:two)~%(defun f () (widget))") (list first)))))))

(define-test opening-symbol-search-resets-package :parent rename
  (let ((ui::*project-search* (ui::make-project-search :target-package "STALE")))
    (with-assistant-functions
        ((ui::show-sidebar-page (lambda (&rest args) (declare (ignore args))))
         (gtk:button-set-label (lambda (&rest args) (declare (ignore args))))
         (gtk:editable-set-text (lambda (&rest args) (declare (ignore args))))
         (ui::show-replace-row (lambda (&rest args) (declare (ignore args))))
         (ui::run-project-search (lambda ()))
         (gtk:widget-grab-focus (lambda (&rest args) (declare (ignore args))))
         (gtk:editable-select-region (lambda (&rest args) (declare (ignore args)))))
      (ui::open-project-search :text "widget" :mode :symbol :symbol "widget" :target-package "CURRENT")
      (is string= "CURRENT" (ui::ps-target-package ui::*project-search*))
      (ui::open-project-search :text "other" :mode :symbol :symbol "other")
      (false (ui::ps-target-package ui::*project-search*)))))

(define-test rename-keeps-old-name-for-missing-files :parent rename
  (dolist (mode '(:symbol :lexical))
    (let* ((path (merge-pathnames (format nil "cadre-missing-~a.lisp" (c:make-uuid))
                                  (uiop:temporary-directory)))
           (entry-text "widget")
           (status nil)
           (ui::*project-search*
             (ui::make-project-search :mode mode :symbol "widget"
                                      :entry :pattern :replace-entry :replacement
                                      :results-data (list (list path '((0 1 7) (0 8 14)))))))
      (setf (gethash path (ui::ps-rename-contexts ui::*project-search*))
            (ui::rename-contexts "(widget widget)" '((0 1 7) (0 8 14))))
      (with-assistant-functions
          ((gtk:editable-get-text (lambda (entry) (if (eq entry :pattern) entry-text "gadget")))
           (gtk:editable-set-text (lambda (entry text) (declare (ignore entry)) (setf entry-text text)))
           (ui::checked-matches #'identity)
           (ui::find-file-buffer (lambda (file) (declare (ignore file)) nil))
           (ui::write-text-file (lambda (&rest args) (declare (ignore args)) (error "Must not write a missing file")))
           (ui::message (lambda (control &rest args) (setf status (apply #'format nil control args))))
           (ui::run-project-search (lambda ())))
        (is equal '(0 0 2) (multiple-value-list (ui::apply-project-replace)))
        (ui::finish-project-replace ui::*project-search* t "gadget")
        (is string= "widget" (ui::ps-symbol ui::*project-search*))
        (is string= "widget" entry-text)
        (true (search "2 places left alone" status))
        (false (probe-file path))))))

(define-test rename-keeps-old-name-after-skips :parent rename
  ;; Skipped places stay in the preview, so it refreshes on the old name.
  (loop for (skipped expected) in '((0 "gadget") (1 "widget"))
        do (let ((ui::*project-search* (ui::make-project-search :mode :symbol :symbol "widget" :entry :pattern))
                 (entry-text "widget"))
             (with-assistant-functions
                 ((ui::apply-project-replace (lambda () (values 1 1 skipped)))
                  (ui::message (lambda (&rest args) (declare (ignore args))))
                  (ui::run-project-search (lambda ()))
                  (gtk:editable-set-text (lambda (entry text) (declare (ignore entry)) (setf entry-text text))))
               (ui::finish-project-replace ui::*project-search* t "gadget")
               (is string= expected (ui::ps-symbol ui::*project-search*))
               (is string= expected entry-text)))))
