;;;; build.lisp — how to build a project's program
;;;;
;;;; A project builds with `make build` if its Makefile has a build target,
;;;; or else, if one of its systems declares a :build-operation (as a new
;;;; application's does), with ASDF's make in a fresh Lisp: building saves
;;;; an image, which ends the Lisp that does it, so it can't be the one
;;;; Cadre talks to.

(in-package #:cadre)

(defun makefile-has-target-p (makefile target)
  "True if MAKEFILE (a pathname) has a rule for TARGET."
  (let ((text (and (probe-file makefile) (read-text-file makefile))))
    (and text
         (ppcre:scan (format nil "(?m)^~a\\s*:(?!=)" (ppcre:quote-meta-chars target)) text)
         t)))

(defun asd-build-pathname (asd)
  "If the system file ASD declares a :build-operation, its :build-pathname
(or t if it has none)."
  (let ((text (read-text-file asd)))
    (when (and text (search ":build-operation" text))
      (multiple-value-bind (match groups) (ppcre:scan-to-strings ":build-pathname\\s+\"([^\"]+)\"" text)
        (if match (aref groups 0) t)))))

(defun asd-entry-point (text)
  "The :entry-point in an .asd file's TEXT (\"pkg:main\"), or nil."
  (multiple-value-bind (match groups) (ppcre:scan-to-strings ":entry-point\\s+\"([^\"]+)\"" text)
    (and match (aref groups 0))))

(defun asd-depends-on-p (text library)
  "True if an .asd file's TEXT depends on LIBRARY (\"name\", :name or #:name)."
  (and (ppcre:scan (format nil "(?i):depends-on\\s*\\([^)]*(\"~a\"|#?:~:*~a[\\s)])" (ppcre:quote-meta-chars library)) text) t))

(defun asd-uses-gtk4-p (text)
  "True if an .asd file's TEXT depends on the gtk4 library."
  (asd-depends-on-p text "gtk4"))

(defun project-app (root)
  "The program in ROOT that Run App starts, as a plist (:system :entry :gtk
:raylib), from the first .asd file with an :entry-point; or nil."
  (loop for asd in (uiop:directory-files (uiop:ensure-directory-pathname root) "*.asd")
        for text = (read-text-file asd)
        for entry = (and text (asd-entry-point text))
        when entry return (list :system (string-downcase (pathname-name asd)) :entry entry
                                :gtk (asd-uses-gtk4-p text)
                                :raylib (asd-depends-on-p text "raylib"))))

(defun project-build-plan (root &key (lisp (first *lisp-command*)))
  "How to build the project in ROOT, as a plist: :program and :arguments to
run there, a :description, and the :output file if known; or nil if it has
nothing to build."
  (let* ((root (uiop:ensure-directory-pathname root))
         (asds (uiop:directory-files root "*.asd"))
         (built (loop for asd in asds
                      for output = (asd-build-pathname asd)
                      when output return (cons (pathname-name asd) output)))
         (output (and built (stringp (cdr built)) (uiop:native-namestring (merge-pathnames (cdr built) root)))))
    (cond ((makefile-has-target-p (merge-pathnames "Makefile" root) "build")
           (list :program "make" :arguments '("build") :description "make build" :output output))
          (built
           (let ((system (car built)))
             (list :program lisp
                   :arguments (list "--non-interactive"
                                    "--eval" "(require :asdf)"
                                    "--eval" (format nil "(push ~s asdf:*central-registry*)" (uiop:native-namestring root))
                                    "--eval" (format nil "(if (find-package :ql) (uiop:symbol-call :ql :quickload ~s) (asdf:load-system ~s))" system system)
                                    "--eval" (format nil "(asdf:make ~s)" system))
                   :description (format nil "(asdf:make ~s) in a new ~a" system lisp)
                   :output output))))))

(defun project-test-plan (root system &key (lisp (first *lisp-command*)))
  "How to run SYSTEM's tests in a new process in ROOT: `make test` if the
Makefile has a test target, else asdf:test-system in a new Lisp. A plist like
PROJECT-BUILD-PLAN's."
  (let ((root (uiop:ensure-directory-pathname root)))
    (if (makefile-has-target-p (merge-pathnames "Makefile" root) "test")
        (list :program "make" :arguments '("test") :description "make test")
        (list :program lisp
              :arguments (list "--non-interactive"
                               "--eval" "(require :asdf)"
                               "--eval" (format nil "(push ~s asdf:*central-registry*)" (uiop:native-namestring root))
                               "--eval" (format nil "(when (find-package :ql) (uiop:symbol-call :ql :quickload ~s))" system)
                               "--eval" (format nil "(asdf:test-system ~s)" system))
              :description (format nil "(asdf:test-system ~s) in a new ~a" system lisp)))))
