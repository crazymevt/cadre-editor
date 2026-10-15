;;;; language-projects.lisp — Racket packages and Clojure projects, for
;;;; the Project page
;;;;
;;;; Read from the project's files, before any Racket or Clojure runs:
;;;;   a Racket package's info.rkt   its collection, version and dependencies
;;;;   a Clojure project's deps.edn, project.clj or bb.edn   its dependencies
;;;;                                 and aliases (Leiningen's profiles)
;;;; and the commands that install what they depend on: raco pkg install
;;;; for the Racket dependencies vendor/cadre-racket/pkgs.rkt says are
;;;; missing; clojure -P, lein deps or bb prepare to download a Clojure
;;;; project's.

(in-package #:cadre)

;;; Racket

(defun read-racket-info (directory)
  "DIRECTORY's info.rkt as a plist: :collection, :version, :description,
:deps and :build-deps (dependency sources, as strings), or nil without one."
  (let ((file (probe-file (merge-pathnames "info.rkt" directory))))
    (when file
      (let* ((text (uiop:read-file-string file))
             (start (if (uiop:string-prefix-p "#lang" text) (or (position #\Newline text) (length text)) 0))
             (definitions '()))
        (loop with i = start
              for (form next) = (multiple-value-list (handler-case (read-sexp text i)
                                                       (error () (values nil nil))))
              while next
              do (setf i next)
                 (when (and (consp form) (remote-symbol= (first form) "define") (remote-symbol-p (second form)))
                   (push (cons (remote-symbol-base-name (second form))
                               (let ((value (third form)))
                                 (if (and (consp value) (eq (first value) 'quote)) (second value) value)))
                         definitions)))
        (flet ((value (name) (cdr (assoc name definitions :test #'string=)))
               (sources (list)
                 ;; "pkg" or ("pkg" #:version "1.2")
                 (loop for dep in (and (listp list) list)
                       for source = (if (consp dep) (first dep) dep)
                       when (stringp source) collect source)))
          (list :collection (let ((c (value "COLLECTION"))) (and (stringp c) c))
                :version (let ((v (value "VERSION"))) (and (stringp v) v))
                :description (let ((d (value "PKG-DESC"))) (and (stringp d) d))
                :deps (sources (value "DEPS"))
                :build-deps (sources (value "BUILD-DEPS"))))))))

(defun racket-missing-dependencies (sources)
  "The SOURCES (dependency names or sources) no installed Racket package provides."
  (when sources
    (let ((output (uiop:run-program (list* *racket-command*
                                           (uiop:native-namestring
                                            (merge-pathnames "pkgs.rkt" (uiop:pathname-directory-pathname (racket-backend-file))))
                                           "missing" sources)
                                    :output :string :error-output nil :ignore-error-status t)))
      (remove "" (uiop:split-string output :separator '(#\Newline)) :test #'string=))))

(defun racket-install-arguments (sources)
  "raco's arguments that install SOURCES and what they need, skipping any installed."
  (list* "pkg" "install" "--auto" "--skip-installed" "--batch" sources))

(defun racket-package-modules (directory)
  "The .rkt files in DIRECTORY and its folders (not info.rkt, compiled/ or
hidden folders), sorted."
  (let ((files '()))
    (labels ((walk (dir)
               (dolist (f (uiop:directory-files dir "*.rkt"))
                 (unless (string= (file-namestring f) "info.rkt") (push f files)))
               (dolist (sub (uiop:subdirectories dir))
                 (let ((name (car (last (pathname-directory sub)))))
                   (unless (or (string= name "compiled") (char= (char name 0) #\.))
                     (walk sub))))))
      (walk (uiop:ensure-directory-pathname directory)))
    (sort files #'string< :key #'namestring)))

;;; Clojure

(defun clojure-coordinate-version (coordinate)
  "The version a deps.edn coordinate names: :mvn/version, a git tag or sha, or a local root."
  (or (edn-get coordinate "mvn/version")
      (edn-get coordinate "git/tag")
      (let ((sha (or (edn-get coordinate "git/sha") (edn-get coordinate "sha"))))
        (and (stringp sha) (subseq sha 0 (min 7 (length sha)))))
      (let ((root (edn-get coordinate "local/root"))) (and root (format nil "local ~a" root)))
      ""))

(defun read-clojure-project-file (directory kind)
  "The project in DIRECTORY, of KIND (:deps, :lein or :babashka), as a plist:
:name, :version, :deps ((library . version) …) and :aliases (names)."
  (let* ((file (merge-pathnames (ecase kind (:deps "deps.edn") (:lein "project.clj") (:babashka "bb.edn")) directory))
         (forms (ignore-errors (read-edn-forms (uiop:read-file-string file))))
         (folder (car (last (pathname-directory (uiop:ensure-directory-pathname directory))))))
    (ecase kind
      ((:deps :babashka)
       (let ((map (first forms)))
         (list :name folder :version nil
               :deps (loop for (lib . coordinate) in (rest (edn-get map "deps"))
                           collect (cons (edn-name lib) (clojure-coordinate-version coordinate)))
               :aliases (loop for (alias . nil) in (rest (edn-get map "aliases"))
                              collect (edn-name alias)))))
      (:lein
       (let* ((form (find-if (lambda (f) (and (consp f) (eq (first f) :list)
                                             (equal (edn-name (second f)) "defproject")))
                             forms))
              (options (cddddr form))
              (option (lambda (key) (getf-edn options key))))
         (list :name (edn-name (third form)) :version (fourth form)
               :deps (loop for dep in (rest (funcall option "dependencies"))
                           when (and (consp dep) (eq (first dep) :vector))
                             collect (cons (edn-name (second dep)) (or (third dep) "")))
               :aliases (loop for (profile . nil) in (rest (funcall option "profiles"))
                              collect (edn-name profile))))))))

(defun getf-edn (options key)
  "The value after the keyword KEY in OPTIONS, a list of EDN values."
  (loop for (k v) on options by #'cddr
        when (and (consp k) (eq (first k) :keyword) (string= (second k) key)) return v))

(defun clojure-fetch-command (kind aliases)
  "The program and arguments that download a KIND of project's dependencies
(with ALIASES, deps.edn's, or Leiningen's profiles) without starting it."
  (ecase kind
    (:deps (append (list *clojure-command* "-P")
                   (and aliases (list (format nil "-M~{:~a~}" aliases)))))
    (:lein (if aliases
               (list *leiningen-command* "with-profile" (format nil "+~{~a~^,+~}" aliases) "deps")
               (list *leiningen-command* "deps")))
    (:babashka (list *babashka-command* "prepare"))))

(defun clojure-deps-signature (project)
  "Something equal (under EQUAL) for two readings of a project when they
depend on the same libraries at the same versions."
  (sort (copy-list (getf project :deps)) #'string< :key #'car))

(defparameter *clojure-sync-deps-code*
  "(if (and (try (require 'clojure.repl.deps) true (catch Throwable _ false))
          (System/getProperty \"clojure.basis\"))
     (binding [*repl* true] ((resolve 'clojure.repl.deps/sync-deps)) :synced)
     :restart)"
  "Clojure that loads the libraries deps.edn has gained into the running
REPL (Clojure 1.12's sync-deps), or says it must restart.")

(defun clojure-project-namespaces (directory)
  "(namespace . file) for each Clojure source file under DIRECTORY's src/,
test/ and dev/ folders, its namespace read from its (ns …) form."
  (let ((found '()))
    (dolist (folder '("src/" "test/" "dev/"))
      (let ((root (merge-pathnames folder directory)))
        (when (uiop:directory-exists-p root)
          (uiop:collect-sub*directories
           root t t
           (lambda (dir)
             (dolist (file (append (uiop:directory-files dir "*.clj") (uiop:directory-files dir "*.cljc")))
               (let ((ns (ignore-errors
                          (multiple-value-bind (match groups)
                              (ppcre:scan-to-strings "\\(ns\\s+(?:\\^\\S+\\s+)?([^\\s()\\[\\]{}]+)"
                                                     (uiop:read-file-string file))
                            (and match (aref groups 0))))))
                 (when ns (push (cons ns file) found)))))))))
    (sort found #'string< :key #'car)))
