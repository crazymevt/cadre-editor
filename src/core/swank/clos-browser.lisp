;;;; Requests for a CLOS browser backed by the connected image.
(in-package #:cadre)

(defparameter +clos-browser-helper+
  #.(uiop:read-file-string (merge-pathnames "clos-browser-remote.lisp" (or *compile-file-truename* *load-truename*))))

(defun clos-browser-call (operation &rest arguments)
  (apply #'swank-call (format nil "cadre-clos-browser:~a" operation) arguments))

(defun clos-browser-entries (catalog &key (filter "") kind)
  "Filter the catalog without asking the Lisp again."
  (remove-if-not (lambda (entry)
                   (and (or (null kind) (eq kind (getf entry :kind)))
                        (search filter (getf entry :name) :test #'char-equal)))
                 (getf catalog :entries)))
