;;;; language-docs.lisp — docsets for Racket and Clojure, made on this machine
;;;;
;;;; The HyperSpec tab shows docsets (the docset library). Racket's and
;;;; Clojure's documentation isn't shipped with Cadre: these functions make a
;;;; docset from what the user has, in ~/.local/share/docsets/ where the tab
;;;; finds it.
;;;;   racket/   every documented export's signature and contracts (the
;;;;             docs' blue boxes), from the installed Racket, by
;;;;             vendor/cadre-racket/docs.rkt, with a link to its full page
;;;;   clojure/  the arglists and docstrings of every public var of the
;;;;             namespaces loaded in the connected Clojure; with ClojureDocs'
;;;;             export, which the user downloads, its examples, see-alsos
;;;;             and notes too

(in-package #:cadre)

(defun language-docset-directory (id)
  (merge-pathnames (make-pathname :directory (list :relative id)) (docset:user-docset-directory)))

(defun unique-ids (entries)
  "ENTRIES with each id made unique (a second \"x\" becomes \"x~2\")."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (entry entries entries)
      (let* ((id (second entry))
             (n (incf (gethash id seen 0))))
        (when (> n 1)
          (setf (second entry) (format nil "~a~~~d" id n)))))))

(defun collapse-spaces (string)
  (string-trim " " (ppcre:regex-replace-all "\\s+" string " ")))

;;; Racket

(defun racket-kind (kind)
  (cond ((string= kind "procedure") (values :function "Procedure"))
        ((member kind '("syntax" "syntax class") :test #'string=) (values :macro (string-capitalize kind)))
        ((member kind '("value" "parameter") :test #'string=) (values :variable (string-capitalize kind)))
        ((member kind '("struct" "class" "interface") :test #'string=) (values :class (string-capitalize kind)))
        ((string= kind "") (values :other "Binding"))
        (t (values :other (string-capitalize kind)))))

(defun racket-docset-entry (name libraries kind lines url)
  "A docset entry for a documented Racket export, from docs.rkt's line."
  (multiple-value-bind (kind kind-name) (racket-kind kind)
    (list :entry (format nil "~a~@[@~a~]" name (first libraries))
          :names (list name) :kind kind :kind-name kind-name :package (first libraries)
          :summary (if lines (collapse-spaces (first lines)) "")
          :upstream url
          :sections (append
                     (and lines (list (list "Signature" (list :code-block "lisp" (format nil "~{~a~^~%~}" lines)))))
                     (and libraries (list (list "Provided by" (list :p (format nil "~{~a~^, ~}" libraries)))))
                     (and url (list (list "Documentation"
                                          (list :p (list :link url "The full entry in the installed Racket documentation")))))))))

(defun racket-docset-entries (output)
  "The docset entries for OUTPUT, the lines docs.rkt prints."
  (unique-ids
   (loop for line in (uiop:split-string output :separator '(#\Newline))
         for data = (and (plusp (length line)) (ignore-errors (read-sexp line)))
         when (and (consp data) (stringp (first data)))
           collect (destructuring-bind (name libraries kind lines url) data
                     (racket-docset-entry name (remove-if-not #'stringp libraries) (or kind "")
                                          (remove-if-not #'stringp lines) (and (stringp url) url))))))

(defun racket-version-string ()
  (string-trim '(#\Newline #\Space)
               (uiop:run-program (list *racket-command* "-e" "(display (version))") :output :string)))

(defun build-racket-docset (&key (directory (language-docset-directory "racket")))
  "Make the Racket docset in DIRECTORY from the installed Racket's documentation.
Returns the number of entries. Takes some seconds."
  (let* ((output (uiop:run-program (list *racket-command*
                                         (uiop:native-namestring
                                          (merge-pathnames "docs.rkt" (uiop:pathname-directory-pathname
                                                                       (racket-backend-file)))))
                                   :output :string :error-output :string))
         (entries (racket-docset-entries output))
         (version (racket-version-string)))
    (when (null entries) (editor-error "Racket's documentation listed nothing (is the full Racket installed?)"))
    (docset:write-docset directory
                         (list :id "racket" :title "Racket" :subtitle "Signatures from the installed Racket documentation"
                               :version version :packages '()
                               :source "The documentation installed with Racket"
                               :attribution (format nil "Made by Cadre from Racket ~a's documentation on this machine." version)
                               :license "As Racket's documentation (Apache 2.0 or MIT)"
                               :toc '())
                         entries)
    (length entries)))

;;; Clojure

(defparameter *clojure-docs-code*
  "(do
    (doseq [n '[clojure.string clojure.set clojure.walk clojure.edn clojure.java.io clojure.pprint
                clojure.test clojure.zip clojure.data clojure.repl clojure.math clojure.datafy]]
      (try (require n) (catch Throwable _ nil)))
    (binding [*print-length* nil *print-level* nil]
     (pr-str
      (for [ns (sort-by str (remove #(re-find #\"^(cider|orchard|nrepl|mranderson|refactor-nrepl|clj-reload|compliment|haystack|logjam|puget|fipp|suitable|cljfmt)\\.\" (str (ns-name %)))
                                    (all-ns)))
            [sym v] (sort-by (comp str key) (ns-publics ns))
            :let [m (meta v)]]
        (list (str (ns-name ns)) (str sym)
              (cond (:macro m) \"macro\"
                    (fn? (try @v (catch Throwable _ nil))) \"function\"
                    :else \"var\")
              (map str (:arglists m))
              (or (:doc m) \"\"))))))"
  "Clojure that loads Clojure's standard namespaces, then lists every public
var of the loaded ones as (namespace name kind (arglist …) docstring),
printed. The REPL's tooling (cider-nrepl, nREPL and the libraries they bring) is left out.")

(defun clojure-kind (kind)
  (cond ((string= kind "macro") (values :macro "Macro"))
        ((string= kind "function") (values :function "Function"))
        (t (values :variable "Var"))))

(defun clojuredocs-index (export)
  "ClojureDocs' EXPORT (parsed JSON) as a table: \"ns/name\" → the var's object."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (var (jlist (jget export "vars")) table)
      (setf (gethash (format nil "~a/~a" (jget var "ns") (jget var "name")) table) var))))

(defun clojuredocs-sections (var ids)
  "Sections for ClojureDocs' VAR: its examples, see-alsos (linked when IDS,
the docset's ids, has them) and notes."
  (let ((examples (remove-if-not #'stringp (mapcar (lambda (e) (jget e "body")) (jlist (jget var "examples")))))
        (see (loop for s in (jlist (jget var "see-alsos"))
                   for ns = (jget s "to-var" "ns") for name = (jget s "to-var" "name")
                   when (and (stringp ns) (stringp name))
                     collect (let ((id (format nil "~a/~a" ns name)))
                               (if (gethash id ids) (list :ref id (format nil "~a/~a" ns name)) (format nil "~a/~a" ns name)))))
        (notes (remove-if-not #'stringp (mapcar (lambda (n) (jget n "body")) (jlist (jget var "notes"))))))
    (append
     (and examples (list (cons "Examples (ClojureDocs)"
                               (mapcar (lambda (body) (list :code-block "lisp" (string-trim '(#\Newline #\Space) body)))
                                       examples))))
     (and see (list (list "See Also (ClojureDocs)" (cons :p (loop for (x . more) on see
                                                                   collect x when more collect ", ")))))
     (and notes (list (cons "Notes (ClojureDocs)" (mapcar (lambda (body) (list :p body)) notes))))
     (let ((href (jget var "href")))
       (and (stringp href)
            (list (list "ClojureDocs" (list :p (list :link (format nil "https://clojuredocs.org~a" href)
                                                     "This var on clojuredocs.org")))))))))

(defun clojure-docset-entries (vars &optional clojuredocs)
  "Docset entries for VARS, (namespace name kind arglists doc) lists, with
CLOJUREDOCS (from clojuredocs-index) merged in when given."
  (let ((ids (make-hash-table :test 'equal)))
    (dolist (v vars) (setf (gethash (format nil "~a/~a" (first v) (second v)) ids) t))
    (unique-ids
     (loop for (ns name kind arglists doc) in vars
           for id = (format nil "~a/~a" ns name)
           collect (multiple-value-bind (kind kind-name) (clojure-kind kind)
                     (let ((arglists (remove-if-not #'stringp arglists))
                           (doc (if (stringp doc) doc ""))
                           (extra (and clojuredocs (gethash id clojuredocs))))
                       (list :entry id :names (list name) :kind kind :kind-name kind-name :package ns
                             :summary (collapse-spaces (subseq doc 0 (or (position #\Newline doc) (length doc))))
                             :sections (append
                                        (and arglists
                                             (list (list "Syntax" (list :code-block "lisp"
                                                                        (format nil "~{(~a~@[ ~a~])~^~%~}"
                                                                                (loop for a in arglists
                                                                                      append (list name (let ((inside (string-trim "[]" a)))
                                                                                                          (and (plusp (length inside)) inside)))))))))
                                        (and (plusp (length doc))
                                             (list (list "Description" (list :code-block nil (string-trim '(#\Newline) doc)))))
                                        (and extra (clojuredocs-sections extra ids))))))))))

(defun clojure-docs-vars (value)
  "The vars from the value *CLOJURE-DOCS-CODE* evaluates to (a printed string)."
  (let ((data (clojure-printed-sexp (clojure-read-string value))))
    (remove-if-not (lambda (v) (and (consp v) (stringp (first v)) (stringp (second v)))) data)))

(defun write-clojure-docset (vars &key clojuredocs version (directory (language-docset-directory "clojure")))
  "Write the Clojure docset for VARS; CLOJUREDOCS is ClojureDocs' parsed export, or nil."
  (let ((entries (clojure-docset-entries vars (and clojuredocs (clojuredocs-index clojuredocs)))))
    (docset:write-docset directory
                         (list :id "clojure" :title "Clojure"
                               :subtitle (if clojuredocs
                                             "Docstrings from the connected Clojure, with ClojureDocs' examples"
                                             "Docstrings from the connected Clojure")
                               :version (or version "") :packages (remove-duplicates (mapcar #'first vars) :test #'string=)
                               :source "The namespaces loaded in the connected Clojure"
                               :attribution (if clojuredocs
                                                "Docstrings from the Clojure libraries' authors. Examples, see-alsos and notes from ClojureDocs (clojuredocs.org), by its contributors, downloaded on this machine and not part of Cadre."
                                                "Docstrings from the Clojure libraries' authors, read from the connected Clojure on this machine.")
                               :license "As each library's"
                               :toc '())
                         entries)
    (length entries)))

(defparameter *clojuredocs-export-url* "https://clojuredocs.org/clojuredocs-export.json"
  "Where ClojureDocs publishes its data export.")

(defun download-clojuredocs ()
  "ClojureDocs' export, downloaded (with curl) and parsed."
  (parse-json (uiop:run-program (list "curl" "-fsSL" "--max-time" "120" *clojuredocs-export-url*)
                                :output :string :error-output :string)))

(defun request-clojure-docs-vars (connection &key on-ok on-abort)
  "Ask CONNECTION's Clojure for its public vars (CLOJURE-DOCS-VARS' lists)."
  (nrepl-eval-request connection *clojure-docs-code* :ns "user" :on-abort on-abort
                      :on-done (lambda (values error-output failed)
                                 (if (or failed (null values))
                                     (when on-abort (funcall on-abort (eval-error-reason error-output)))
                                     (when on-ok (funcall on-ok (clojure-docs-vars (car (last values)))))))))
