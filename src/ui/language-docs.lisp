;;;; language-docs.lisp — Racket's and Clojure's docs in the HyperSpec tab
;;;;
;;;; Racket: Build Docs and Clojure: Build Docs make a docset on this
;;;; machine (core/language-docs.lisp); Clojure: Build Docs with ClojureDocs
;;;; Examples downloads ClojureDocs' export into it too. Look Up in Docs
;;;; shows the entry for the name at the cursor in the code's language: the
;;;; HyperSpec for Common Lisp, else the Racket or Clojure docset.

(in-package #:cadre-ui)

(defun reload-docsets ()
  "Find the docsets again, the next time one is wanted (a new one was made)."
  (setf *docsets* :unread))

(defun build-docs-in-background (what build)
  "Call BUILD (which returns the number of entries) in another thread, and say how it went."
  (message "Building the ~a docs…" what)
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let ((count (funcall build)))
           (deliver-to-gui (lambda ()
                             (reload-docsets)
                             (message "The ~a docs are ready: ~:d entries. Look Up in Docs shows them." what count))))
       (error (e)
         (let ((text (princ-to-string e)))
           (deliver-to-gui (lambda () (message "Building the ~a docs failed: ~a" what text)))))))
   :name (format nil "~(~a~) docs" what)))

(define-command racket-build-docs ()
  "Make the Racket docs for Look Up in Docs: every documented export's
signature and contracts, from the installed Racket, with a link to its page."
  (:title "Racket: Build Docs")
  (build-docs-in-background "Racket" #'build-racket-docset))

(defun build-clojure-docs (&key clojuredocs)
  (with-connection (connection :language :clojure)
    (let ((version (connection-implementation connection)))
      (message "Reading the docstrings of the loaded namespaces…")
      (request #'request-clojure-docs-vars connection
               :on-ok (lambda (vars)
                        (build-docs-in-background
                         "Clojure"
                         (lambda ()
                           (write-clojure-docset vars :version version
                                                      :clojuredocs (and clojuredocs (download-clojuredocs))))))))))

(define-command clojure-build-docs ()
  "Make the Clojure docs for Look Up in Docs: the arglists and docstrings of
every public var in the namespaces the connected Clojure has loaded."
  (:title "Clojure: Build Docs")
  (build-clojure-docs))

(define-command clojure-build-docs-with-clojuredocs ()
  "Clojure: Build Docs, with ClojureDocs' community examples, see-alsos and
notes, downloaded from clojuredocs.org into the docs on this machine."
  (:title "Clojure: Build Docs with ClojureDocs Examples")
  (build-clojure-docs :clojuredocs t))

;;; Looking up

(defparameter *language-docsets* '((:racket "racket" "Racket") (:clojure "clojure" "Clojure"))
  "Language → its docset's id, and the title its Build Docs command has.")

(defun preferred-records (pairs language view)
  "The (docset . record)s among PAIRS to show first: in Clojure the file's
namespace's, then clojure.core's; in Racket racket/base's, then racket's,
then the other racket/… libraries'."
  (flet ((in (test) (remove-if-not (lambda (p) (let ((package (docset:record-package (cdr p))))
                                                 (and package (funcall test package))))
                                   pairs)))
    (or (loop for test in (case language
                            (:clojure (list (lambda (p) (equal p (view-package view)))
                                            (lambda (p) (equal p "clojure.core"))))
                            (:racket (list (lambda (p) (equal p "racket/base"))
                                           (lambda (p) (equal p "racket"))
                                           (lambda (p) (uiop:string-prefix-p "racket/" p)))))
              for found = (in test)
              when found return found)
        pairs)))

(define-command look-up-documentation ()
  "Show the docs for the name at the cursor: the HyperSpec (or a library's
docs) in Common Lisp, the docs Racket: Build Docs or Clojure: Build Docs
made in Racket and Clojure."
  (let* ((view (context-view))
         (language (view-language view))
         (known (assoc language *language-docsets*)))
    (if (null known)
        (hyperspec-look-up)
        (destructuring-bind (id title) (rest known)
          (let ((docset (or (find-docset id)
                            (editor-error "No ~a docs yet: run ~a: Build Docs first" title title)))
                (text (or (hyperspec-text-at-cursor view) (editor-error "No name at the cursor"))))
            (let* ((name (let ((slash (position #\/ text :from-end t)))
                           (if (and (eq language :clojure) slash (< 0 slash (1- (length text))))
                               (subseq text (1+ slash))
                               text)))
                   (pairs (preferred-records (mapcar (lambda (r) (cons docset r)) (docset:find-records docset name))
                                             language view)))
              (cond ((null pairs) (hyperspec-search-for name))
                    ((null (rest pairs)) (show-record (first pairs)))
                    (t (open-picker (window-picker *window*)
                                    :items pairs :label #'record-label
                                    :detail (lambda (p) (format nil "~a — ~a" (docset:record-kind-name (cdr p))
                                                                (docset:record-package (cdr p))))
                                    :placeholder (format nil "~a docs for ~a" title name)
                                    :on-choose #'show-record)))))))))
