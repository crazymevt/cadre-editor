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

(defun build-docs-in-background (what build &optional then)
  "Call BUILD (which returns the number of entries) in another thread, and say
how it went; then call THEN (on the GTK thread)."
  (message "Building the ~a docs…" what)
  (sb-thread:make-thread
   (lambda ()
     (handler-case
         (let ((count (funcall build)))
           (deliver-to-gui (lambda ()
                             (reload-docsets)
                             (message "The ~a docs are ready: ~:d entr~:@p. Look Up in Docs shows them." what count)
                             (when then (funcall then)))))
       (error (e)
         (let ((text (princ-to-string e)))
           (deliver-to-gui (lambda ()
                             (message "Building the ~a docs failed: ~a" what text)
                             (when then (funcall then))))))))
   :name (format nil "~(~a~) docs" what)))

(define-command racket-build-docs ()
  "Make the Racket docs for Look Up in Docs: every documented export's
signature and contracts, from the installed Racket, with a link to its page."
  (:title "Racket: Build Docs")
  (build-docs-in-background "Racket" #'build-racket-docset))

(define-command clojure-build-docs ()
  "Make the Clojure docs for Look Up in Docs: the arglists and docstrings of
Clojure's standard namespaces, from a Clojure started for it (not a
project's REPL, so no project's code is in them)."
  (:title "Clojure: Build Docs")
  (build-docs-in-background "Clojure" #'build-clojure-docset))

(define-command clojure-build-docs-with-clojuredocs ()
  "Clojure: Build Docs, with ClojureDocs' community examples, see-alsos and
notes, downloaded from clojuredocs.org into the docs on this machine."
  (:title "Clojure: Build Docs with ClojureDocs Examples")
  (build-docs-in-background "Clojure" (lambda () (build-clojure-docset :clojuredocs t))))

;;; A project's own docs

(defun project-title (root)
  (car (last (pathname-directory (uiop:ensure-directory-pathname root)))))

(define-command build-project-docs ()
  "Make the current project's own docs, kept apart from the language's: a
Racket package's exports (read from its source), or a Clojure project's
namespaces (through its REPL). Look Up in Docs searches them first."
  (let* ((root (project-root-or-error))
         (title (project-title root))
         (kind (clojure-project-kind-here root)))
    (cond
      (kind
       (let ((namespaces (remove-if-not (lambda (e) (search "/src/" (namestring (cdr e))))
                                        (clojure-project-namespaces root))))
         (unless namespaces (editor-error "~a has no namespaces under src/" title))
         (with-connection (connection :language :clojure)
           (message "Reading ~a's namespaces…" title)
           (request #'request-clojure-docs-vars connection :code (clojure-project-docs-code (mapcar #'car namespaces))
                    :on-ok (lambda (vars)
                             (build-docs-in-background
                              title (lambda () (write-clojure-project-docset root vars :title title))))))))
      ((racket-package-modules root)
       (build-docs-in-background title (lambda () (build-racket-project-docset root :title title))))
      (t (editor-error "~a isn't a Racket package or Clojure project" title)))))

;;; Looking up

(defparameter *language-docsets* '((:racket "racket" "Racket") (:clojure "clojure" "Clojure"))
  "Language → its docset's id, and the title its Build Docs command has.")

(defun project-docset-here ()
  "The current project's own docset, if Build Project Docs made one."
  (let ((root (current-project-root)))
    (and root (find-docset (project-docset-id root)))))

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
          (let ((docset (find-docset id))
                (project (project-docset-here))
                (text (or (hyperspec-text-at-cursor view) (editor-error "No name at the cursor"))))
            (unless (or docset project)
              (editor-error "No ~a docs yet: run ~a: Build Docs (Settings → ~a) first" title title title))
            (let* ((name (let ((slash (position #\/ text :from-end t)))
                           (if (and (eq language :clojure) slash (< 0 slash (1- (length text))))
                               (subseq text (1+ slash))
                               text)))
                   ;; The project's own entries first, then the language's.
                   (pairs (or (and project (mapcar (lambda (r) (cons project r)) (docset:find-records project name)))
                              (and docset
                                   (preferred-records (mapcar (lambda (r) (cons docset r)) (docset:find-records docset name))
                                                      language view)))))
              (cond ((null pairs) (hyperspec-search-for name))
                    ((null (rest pairs)) (show-record (first pairs)))
                    (t (open-picker (window-picker *window*)
                                    :items pairs :label #'record-label
                                    :detail (lambda (p) (format nil "~a — ~a" (docset:record-kind-name (cdr p))
                                                                (docset:record-package (cdr p))))
                                    :placeholder (format nil "~a docs for ~a" title name)
                                    :on-choose #'show-record)))))))))

;;; Settings → Racket and Clojure: when the docs were built, and rebuilding them

(defun docset-status (id)
  "What the docset ID is, and when it was made, as a line of text."
  (let* ((directory (language-docset-directory id))
         (manifest (probe-file (merge-pathnames "manifest.sexp" directory)))
         (docset (and manifest (ignore-errors (docset:open-docset directory)))))
    (if (null docset)
        "Not built yet"
        (multiple-value-bind (s m h day month year) (decode-universal-time (file-write-date manifest))
          (declare (ignore s))
          (format nil "~:d entries~@[ from ~a~]~:[~;, with ClojureDocs' examples~] · built ~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d"
                  (getf (docset::docset-manifest docset) :entry-count)
                  (let ((v (getf (docset::docset-manifest docset) :version))) (and (plusp (length v)) v))
                  (search "ClojureDocs" (or (getf (docset::docset-manifest docset) :subtitle) ""))
                  year month day h m)))))

(defun docs-settings-group (category)
  "The Documentation group for the Racket or Clojure page of Settings."
  (let* ((clojure (string= category "Clojure"))
         (id (if clojure "clojure" "racket"))
         (row (make-instance 'adw:action-row :title (format nil "~a docs" category) :subtitle (docset-status id)
                                             :subtitle-lines 2))
         (group (make-instance 'adw:preferences-group
                               :title "Documentation"
                               :description (if clojure
                                                "Look Up in Docs shows these: Clojure's standard namespaces, made on this machine by a Clojure started for it. A project's own docs are made from its Project page."
                                                "Look Up in Docs shows these: every documented name in your installed Racket, made on this machine. A package's own docs are made from its Project page."))))
    (flet ((rebuild-button (label tooltip build)
             (let ((button (make-instance 'gtk:button :label label :valign :center :tooltip-text tooltip)))
               (gobject:connect button :clicked
                                (lambda (b)
                                  (gtk:widget-set-sensitive b nil)
                                  (adw:action-row-set-subtitle row "Building…")
                                  (build-docs-in-background category build
                                                            (lambda ()
                                                              (gtk:widget-set-sensitive b t)
                                                              (adw:action-row-set-subtitle row (docset-status id))))))
               (adw:action-row-add-suffix row button))))
      (if clojure
          (progn
            (rebuild-button "Rebuild" "Make the Clojure docs again" #'build-clojure-docset)
            (rebuild-button "With ClojureDocs" "Make them with ClojureDocs' examples, downloaded from clojuredocs.org"
                            (lambda () (build-clojure-docset :clojuredocs t))))
          (rebuild-button "Rebuild" "Make the Racket docs again (after installing or updating packages)"
                          #'build-racket-docset)))
    (adw:preferences-group-add group row)
    group))
