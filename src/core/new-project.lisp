;;;; new-project.lisp — the files of a new Lisp project
;;;;
;;;; PROJECT-FILES-FOR describes a project as (relative-path . contents):
;;;;
;;;;   NAME.asd            the system, and NAME/tests with ASDF's test-op
;;;;   src/package.lisp    the package
;;;;   src/main.lisp       the code (and, for an application, MAIN)
;;;;   tests/main.lisp     the tests (Parachute or FiveAM)
;;;;   Makefile            load, test, clean (and build, for an application)
;;;;   README.md, .gitignore, and LICENSE (MIT or BSD 2-Clause)
;;;;
;;;; CREATE-LISP-PROJECT writes them into a new folder.

(in-package #:cadre)

(defun valid-project-name-p (name)
  "True if NAME can name a project: lower-case letters, digits and hyphens,
starting with a letter."
  (and (plusp (length name))
       (lower-case-p (char name 0))
       (every (lambda (c) (or (lower-case-p c) (digit-char-p c) (char= c #\-))) name)
       (char/= (char name (1- (length name))) #\-)))

(defun fill-template (template values)
  "TEMPLATE with each {{key}} replaced by its value in VALUES, a plist of
keywords and strings."
  (with-output-to-string (out)
    (loop with start = 0
          for open = (search "{{" template :start2 start)
          do (if (null open)
                 (progn (write-string template out :start start) (return))
                 (let ((close (search "}}" template :start2 open)))
                   (write-string template out :start start :end open)
                   (let ((key (intern (string-upcase (subseq template (+ open 2) close)) :keyword)))
                     (write-string (or (getf values key) "") out))
                   (setf start (+ close 2)))))))

(defun template-lines (&rest lines)
  (format nil "~{~a~%~}" lines))

;;; The system

(defun asd-template (kind tests)
  (apply #'template-lines
         (append
          '(";;;; {{name}}.asd"
            ""
            "(defsystem \"{{name}}\""
            "  :description \"{{description}}\""
            "  :author \"{{author}}\""
            "  :license \"{{license}}\""
            "  :version \"0.1.0\""
            "  :depends-on ()"
            "  :components ((:module \"src\""
            "                :serial t"
            "                :components ((:file \"package\")"
            "                             (:file \"main\"))))")
          (when (eq kind :application)
            '("  :build-operation \"program-op\""
              "  :build-pathname \"bin/{{name}}\""
              "  :entry-point \"{{name}}:main\""))
          '("  :in-order-to ((test-op (test-op \"{{name}}/tests\"))))"
            ""
            "(defsystem \"{{name}}/tests\""
            "  :description \"Tests for {{name}}.\""
            "  :author \"{{author}}\""
            "  :license \"{{license}}\"")
          (ecase tests
            (:parachute
             '("  :depends-on (\"{{name}}\" \"parachute\")"
               "  :components ((:module \"tests\""
               "                :components ((:file \"main\"))))"
               "  :perform (test-op (op c) (uiop:symbol-call :parachute :test :{{name}}/tests)))"))
            (:fiveam
             '("  :depends-on (\"{{name}}\" \"fiveam\")"
               "  :components ((:module \"tests\""
               "                :components ((:file \"main\"))))"
               "  :perform (test-op (op c) (uiop:symbol-call :fiveam :run! (uiop:find-symbol* :{{name}} :{{name}}/tests))))"))))))

(defun package-template (kind)
  (template-lines ";;;; package.lisp"
                  ""
                  "(defpackage #:{{name}}"
                  "  (:use #:cl)"
                  (if (eq kind :application)
                      "  (:export #:hello #:main))"
                      "  (:export #:hello))")))

(defun main-template (kind)
  (apply #'template-lines
         (append
          '(";;;; main.lisp — {{description}}"
            ""
            "(in-package #:{{name}})"
            ""
            "(defun hello (&optional (who \"World\"))"
            "  \"A greeting for WHO.\""
            "  (format nil \"Hello, ~a!\" who))")
          (when (eq kind :application)
            '(""
              "(defun main ()"
              "  \"The program's entry point (the :entry-point in {{name}}.asd).\""
              "  (write-line (hello (or (first (uiop:command-line-arguments)) \"World\"))))")))))

(defun tests-template (tests)
  (ecase tests
    (:parachute
     (template-lines ";;;; tests/main.lisp"
                     ""
                     "(defpackage #:{{name}}/tests"
                     "  (:use #:cl #:parachute))"
                     ""
                     "(in-package #:{{name}}/tests)"
                     ""
                     "(define-test {{name}})"
                     ""
                     "(define-test hello"
                     "  :parent {{name}}"
                     "  (is string= \"Hello, World!\" ({{name}}:hello))"
                     "  (is string= \"Hello, Lisp!\" ({{name}}:hello \"Lisp\")))"))
    (:fiveam
     (template-lines ";;;; tests/main.lisp"
                     ""
                     "(defpackage #:{{name}}/tests"
                     "  (:use #:cl #:fiveam))"
                     ""
                     "(in-package #:{{name}}/tests)"
                     ""
                     "(def-suite {{name}} :description \"Tests for {{name}}.\")"
                     "(in-suite {{name}})"
                     ""
                     "(test hello"
                     "  (is (string= \"Hello, World!\" ({{name}}:hello)))"
                     "  (is (string= \"Hello, Lisp!\" ({{name}}:hello \"Lisp\"))))"))))

;;; Around it

(defun makefile-template (kind tests)
  (let ((tab (string #\Tab)))
    (apply #'template-lines
           (append
            (list "# Makefile for {{name}}. Needs Quicklisp, loaded from your Lisp's init file."
                  ""
                  "LISP ?= sbcl"
                  "RUN = $(LISP) --non-interactive --eval '(require :asdf)' --eval '(push (truename \".\") asdf:*central-registry*)'"
                  ""
                  (if (eq kind :application) ".PHONY: load test build clean" ".PHONY: load test clean")
                  ""
                  "# Load the system, fetching its dependencies"
                  "load:"
                  (concatenate 'string tab "$(RUN) --eval '(ql:quickload :{{name}})'")
                  ""
                  "# Run the tests; exits 1 if one fails"
                  "test:"
                  (concatenate 'string tab "$(RUN) --eval '(ql:quickload :{{name}}/tests)' \\")
                  (concatenate 'string tab tab
                               (ecase tests
                                 (:parachute "--eval '(uiop:quit (if (eq :passed (parachute:status (parachute:test :{{name}}/tests))) 0 1))'")
                                 (:fiveam "--eval '(uiop:quit (if (fiveam:run! (quote {{name}}/tests::{{name}})) 0 1))'"))))
            (when (eq kind :application)
              (list ""
                    "# Build the program: bin/{{name}}"
                    "build:"
                    (concatenate 'string tab "$(RUN) --eval '(ql:quickload :{{name}})' --eval '(asdf:make :{{name}})'")))
            (list ""
                  "clean:"
                  (concatenate 'string tab "find . -name '*.fasl' -delete")
                  (concatenate 'string tab "rm -rf bin"))))))

(defun readme-template (kind)
  (apply #'template-lines
         (append
          '("# {{name}}"
            ""
            "{{description}}"
            ""
            "## Usage"
            ""
            "```lisp"
            "(ql:quickload :{{name}})"
            "({{name}}:hello \"Lisp\")   ; => \"Hello, Lisp!\""
            "```")
          (when (eq kind :application)
            '(""
              "## Building"
              ""
              "```sh"
              "make build"
              "bin/{{name}} Lisp"
              "```"))
          '(""
            "## Tests"
            ""
            "```sh"
            "make test"
            "```"
            ""
            "Or, in a Lisp: `(asdf:test-system :{{name}})`."))))

(defparameter *gitignore-template*
  (template-lines "*.fasl" "*.dx64fsl" "*.lx64fsl" "*.x86f" "*.abcl" "*~" ".#*" "\\#*#" ".DS_Store" "bin/" "build/"))

(defparameter *license-templates*
  `(("MIT"
     . ,(template-lines
         "MIT License"
         ""
         "Copyright (c) {{year}} {{author}}"
         ""
         "Permission is hereby granted, free of charge, to any person obtaining a copy"
         "of this software and associated documentation files (the \"Software\"), to deal"
         "in the Software without restriction, including without limitation the rights"
         "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell"
         "copies of the Software, and to permit persons to whom the Software is"
         "furnished to do so, subject to the following conditions:"
         ""
         "The above copyright notice and this permission notice shall be included in all"
         "copies or substantial portions of the Software."
         ""
         "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
         "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
         "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
         "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
         "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,"
         "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"
         "SOFTWARE."))
    ("BSD-2-Clause"
     . ,(template-lines
         "BSD 2-Clause License"
         ""
         "Copyright (c) {{year}}, {{author}}"
         ""
         "Redistribution and use in source and binary forms, with or without"
         "modification, are permitted provided that the following conditions are met:"
         ""
         "1. Redistributions of source code must retain the above copyright notice, this"
         "   list of conditions and the following disclaimer."
         ""
         "2. Redistributions in binary form must reproduce the above copyright notice,"
         "   this list of conditions and the following disclaimer in the documentation"
         "   and/or other materials provided with the distribution."
         ""
         "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\""
         "AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE"
         "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE"
         "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE"
         "FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
         "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR"
         "SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER"
         "CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,"
         "OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE"
         "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.")))
  "License name → LICENSE file text.")

(defun project-files-for (name &key (description "") (author "") (license "MIT")
                                    (kind :library) (tests :parachute))
  "The files of a new project NAME, as (relative-path . contents). KIND is
:library or :application; TESTS is :parachute or :fiveam; LICENSE is a key
of *license-templates*, or nil for none."
  (unless (valid-project-name-p name)
    (editor-error "A project name is lower-case letters, digits and hyphens, starting with a letter"))
  (let* ((values (list :name name
                       :description (substitute #\' #\" (if (string= description "") "A Common Lisp project." description))
                       :author (substitute #\' #\" author)
                       :license (or license "Proprietary")
                       :year (princ-to-string (nth-value 5 (decode-universal-time (get-universal-time))))))
         (files (list (cons (format nil "~a.asd" name) (asd-template kind tests))
                      (cons "src/package.lisp" (package-template kind))
                      (cons "src/main.lisp" (main-template kind))
                      (cons "tests/main.lisp" (tests-template tests))
                      (cons "Makefile" (makefile-template kind tests))
                      (cons "README.md" (readme-template kind))
                      (cons ".gitignore" *gitignore-template*))))
    (when license
      (let ((text (cdr (assoc license *license-templates* :test #'string=))))
        (unless text (editor-error "Unknown license: ~a" license))
        (setf files (append files (list (cons "LICENSE" text))))))
    (loop for (path . template) in files
          collect (cons path (fill-template template values)))))

(defun create-lisp-project (parent name &rest options &key &allow-other-keys)
  "Make the folder NAME in PARENT and write the project's files there (see
PROJECT-FILES-FOR for OPTIONS). Returns the folder."
  (let* ((files (apply #'project-files-for name options))
         (directory (merge-pathnames (make-pathname :directory (list :relative name))
                                     (uiop:ensure-directory-pathname parent))))
    (unless (uiop:directory-exists-p parent)
      (editor-error "There is no folder ~a" (uiop:native-namestring parent)))
    (when (and (uiop:directory-exists-p directory)
               (or (uiop:directory-files directory) (uiop:subdirectories directory)))
      (editor-error "~a already exists and isn't empty" (uiop:native-namestring directory)))
    (loop for (path . contents) in files
          do (let ((file (merge-pathnames path directory)))
               (ensure-directories-exist file)
               (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
                 (write-string contents out))))
    directory))
