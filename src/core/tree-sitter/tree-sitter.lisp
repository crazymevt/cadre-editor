;;;; tree-sitter.lisp — other languages, through tree-sitter grammars
;;;;
;;;; libtree-sitter (installed separately, e.g. by Homebrew) parses; each
;;;; language is a grammar, a small C library Cadre builds from the grammar's
;;;; repository at a pinned version, with the grammar's highlight queries.
;;;; Calls go through shim.c, compiled once, which turns the by-value structs
;;;; of tree-sitter's API into integers.
;;;;
;;;; Highlighting runs a language's highlight query over a range and paints
;;;; its captures. An inner node wins over the one around it. On the same
;;;; node, it depends on how the language's queries are written: JavaScript's
;;;; put generic patterns first, so the last pattern wins; JSON's put specific
;;;; ones first, so the first wins (each language's :precedence). The predicates tree-sitter leaves to its clients (#match?,
;;;; #eq?, #any-of? and their negations) are checked here; #is-not? local
;;;; needs scope analysis Cadre doesn't do, and holds.
;;;;
;;;; Positions: tree-sitter counts UTF-8 bytes, Cadre characters. A document
;;;; keeps the text it parsed as octets, and maps between the two when the
;;;; text isn't all ASCII.

(in-package #:cadre)

;;; Where things are

(define-option *tree-sitter-directory* nil (or null string)
  "Where Cadre keeps tree-sitter grammars it builds. Nil means
$XDG_DATA_HOME/cadre/tree-sitter/ (~/.local/share/cadre/tree-sitter/)."
  :category "Languages")

(define-option *tree-sitter-prefix* nil (or null string)
  "Where libtree-sitter is installed (its lib/ and include/). Nil means
Homebrew's tree-sitter, /usr/local or /usr, whichever has it."
  :category "Languages")

(define-option *c-compiler* "cc" string
  "The C compiler Cadre builds tree-sitter grammars with."
  :category "Languages")

(defun tree-sitter-directory ()
  (uiop:ensure-directory-pathname
   (or *tree-sitter-directory*
       (merge-pathnames "cadre/tree-sitter/"
                        (let ((xdg (uiop:getenv "XDG_DATA_HOME")))
                          (if (plusp (length xdg))
                              (uiop:ensure-directory-pathname xdg)
                              (merge-pathnames ".local/share/" (user-homedir-pathname))))))))

(defun shared-library-type () #+darwin "dylib" #-darwin "so")

(defvar *found-prefix* nil)

(defun tree-sitter-prefix ()
  "The directory holding libtree-sitter's lib/ and include/, or nil."
  (or *tree-sitter-prefix*
      *found-prefix*
      (setf *found-prefix*
            (find-if (lambda (dir)
                       (and dir (probe-file (merge-pathnames "include/tree_sitter/api.h" (uiop:ensure-directory-pathname dir)))))
                     (list (ignore-errors (string-trim '(#\Newline #\Space)
                                                       (uiop:run-program '("brew" "--prefix" "tree-sitter")
                                                                         :output :string :ignore-error-status t)))
                           "/opt/homebrew" "/usr/local" "/usr")))))

;;; Languages

(define-major-mode javascript-mode (:title "JavaScript" :extensions ("js" "mjs" "cjs" "jsx"))
  "JavaScript, highlighted by its tree-sitter grammar.")
(define-major-mode typescript-mode (:title "TypeScript" :extensions ("ts" "mts" "cts"))
  "TypeScript, highlighted by its tree-sitter grammar.")
(define-major-mode tsx-mode (:title "TSX" :extensions ("tsx"))
  "TypeScript with JSX, highlighted by its tree-sitter grammar.")
(define-major-mode json-mode (:title "JSON" :extensions ("json" "jsonc"))
  "JSON, highlighted by its tree-sitter grammar.")
(define-major-mode html-mode (:title "HTML" :extensions ("html" "htm" "xhtml"))
  "HTML, highlighted by its tree-sitter grammar (and its scripts and styles by theirs).")
(define-major-mode css-mode (:title "CSS" :extensions ("css"))
  "CSS, highlighted by its tree-sitter grammar.")

(defparameter *tree-sitter-modes*
  '((javascript-mode . "javascript") (typescript-mode . "typescript") (tsx-mode . "tsx") (json-mode . "json")
    (html-mode . "html") (css-mode . "css"))
  "Major mode → its tree-sitter language.")

(defun tree-sitter-language-for-mode (mode)
  (cdr (assoc mode *tree-sitter-modes*)))

(defparameter *tree-sitter-repos*
  '(("javascript" :url "https://github.com/tree-sitter/tree-sitter-javascript" :tag "v0.23.1")
    ("typescript" :url "https://github.com/tree-sitter/tree-sitter-typescript" :tag "v0.23.2")
    ("json" :url "https://github.com/tree-sitter/tree-sitter-json" :tag "v0.24.8")
    ("html" :url "https://github.com/tree-sitter/tree-sitter-html" :tag "v0.23.2")
    ("css" :url "https://github.com/tree-sitter/tree-sitter-css" :tag "v0.23.2"))
  "Grammar repositories, by name, at the versions Cadre builds.")

(defparameter *ecma-outline*
  "(function_declaration name: (identifier) @name) @definition.function
(generator_function_declaration name: (identifier) @name) @definition.function
(class_declaration name: (_) @name) @definition.class
(method_definition name: (_) @name) @definition.method
(variable_declarator name: (identifier) @name value: [(arrow_function) (function_expression)]) @definition.function
")

(defparameter *typescript-outline*
  (concatenate 'string *ecma-outline*
               "(abstract_class_declaration name: (_) @name) @definition.class
(interface_declaration name: (_) @name) @definition.interface
(type_alias_declaration name: (_) @name) @definition.type
(enum_declaration name: (_) @name) @definition.enum
(function_signature name: (_) @name) @definition.function
"))

(defparameter *tree-sitter-languages*
  `((:name "javascript" :title "JavaScript" :repos ("javascript") :source "javascript/src/"
     :symbol "tree_sitter_javascript" :extensions ("js" "mjs" "cjs" "jsx") :comment "//"
     :highlights (("javascript" "queries/highlights.scm") ("javascript" "queries/highlights-jsx.scm")
                  ("javascript" "queries/highlights-params.scm"))
     :outline ,*ecma-outline*)
    (:name "typescript" :title "TypeScript" :repos ("typescript" "javascript") :source "typescript/typescript/src/"
     :symbol "tree_sitter_typescript" :extensions ("ts" "mts" "cts") :comment "//"
     ;; TypeScript's own patterns last, so they win over JavaScript's.
     :highlights (("javascript" "queries/highlights.scm") ("typescript" "queries/highlights.scm"))
     :outline ,*typescript-outline*)
    (:name "tsx" :title "TSX" :repos ("typescript" "javascript") :source "typescript/tsx/src/"
     :symbol "tree_sitter_tsx" :extensions ("tsx") :comment "//"
     :highlights (("javascript" "queries/highlights.scm") ("javascript" "queries/highlights-jsx.scm")
                  ("typescript" "queries/highlights.scm"))
     :outline ,*typescript-outline*)
    (:name "json" :title "JSON" :repos ("json") :source "json/src/"
     :symbol "tree_sitter_json" :extensions ("json" "jsonc") :comment "//"
     :highlights (("json" "queries/highlights.scm")) :precedence :first
     :outline "(document (object (pair key: (string (string_content) @name)) @definition.key))")
    (:name "html" :title "HTML" :repos ("html") :source "html/src/"
     :symbol "tree_sitter_html" :extensions ("html" "htm" "xhtml") :block-comment ("<!--" "-->")
     :highlights (("html" "queries/highlights.scm")) :injections ("html" "queries/injections.scm")
     :outline "(element (start_tag (tag_name) @tag) (text) @name (#match? @tag \"^[hH][1-6]$\")) @definition.heading")
    (:name "css" :title "CSS" :repos ("css") :source "css/src/"
     :symbol "tree_sitter_css" :extensions ("css") :block-comment ("/*" "*/")
     ;; Custom properties (--x) come before plain ones: the first pattern wins.
     :highlights (("css" "queries/highlights.scm")) :precedence :first
     :outline "(rule_set (selectors) @name) @definition.rule
(keyframes_statement (keyframes_name) @name) @definition.keyframes"))
  "The languages Cadre knows: their grammar (repositories, source folder in
the first, the function returning it), file types, :comment (a line comment)
or :block-comment (start and end), highlight query files (and :precedence,
:last unless :first wins on the same node), an :injections query file (code
in other languages inside, as HTML's scripts and styles) and outline query.")

(defun tree-sitter-language-spec (name)
  (find name *tree-sitter-languages* :key (lambda (l) (getf l :name)) :test #'string-equal))

(defun tree-sitter-language-for-file (pathname)
  "The name of the tree-sitter language for files like PATHNAME, or nil."
  (let ((type (and pathname (pathname-type pathname))))
    (and type
         (getf (find-if (lambda (l) (member type (getf l :extensions) :test #'string-equal)) *tree-sitter-languages*)
               :name))))

(defun language-library (name)
  (merge-pathnames (format nil "lib/libtree-sitter-~a.~a" name (shared-library-type)) (tree-sitter-directory)))

(defun language-highlights-file (name)
  (merge-pathnames (format nil "queries/~a/highlights.scm" name) (tree-sitter-directory)))

(defun language-injections-file (name)
  (merge-pathnames (format nil "queries/~a/injections.scm" name) (tree-sitter-directory)))

(defun shim-library ()
  (merge-pathnames (format nil "lib/libcadre-tree-sitter.~a" (shared-library-type)) (tree-sitter-directory)))

(defun tree-sitter-language-installed-p (name)
  (and (probe-file (language-library name)) (probe-file (language-highlights-file name)) t))

;;; Installing

(defun run-or-fail (program arguments &key directory log)
  "Run PROGRAM with ARGUMENTS; LOG gets its command line and output. Signals
an editor-error if it fails."
  (when log (funcall log (format nil "$ ~a~{ ~a~}" program arguments)))
  (multiple-value-bind (output error code)
      (uiop:run-program (cons program arguments) :directory directory :output :string :error-output :string
                                                 :ignore-error-status t)
    (when (and log (plusp (length (string-trim '(#\Newline) (concatenate 'string output error)))))
      (funcall log (string-right-trim '(#\Newline) (concatenate 'string output error))))
    (unless (zerop code)
      (editor-error "~a failed: ~a" program (string-trim '(#\Newline #\Space) (if (plusp (length error)) error output))))
    output))

(defun repo-directory (repo)
  (destructuring-bind (&key tag &allow-other-keys) (cdr (assoc repo *tree-sitter-repos* :test #'string=))
    (merge-pathnames (format nil "src/~a-~a/" repo tag) (tree-sitter-directory))))

(defun ensure-repo (repo &key log)
  "Clone REPO at its pinned version, unless that's done."
  (let ((directory (repo-directory repo)))
    (unless (probe-file (merge-pathnames ".git/" directory))
      (destructuring-bind (&key url tag) (cdr (assoc repo *tree-sitter-repos* :test #'string=))
        (ensure-directories-exist directory)
        (when log (funcall log (format nil "Cloning ~a ~a" url tag)))
        (git-network (merge-pathnames "../" directory) "clone" "--quiet" "--depth" "1" "--branch" tag url
                     (uiop:native-namestring directory))))
    directory))

(defun compile-shared-library (sources output &key include-directories libraries log)
  (ensure-directories-exist output)
  (run-or-fail *c-compiler*
               (append (list "-O2" "-shared" "-fPIC" "-o" (uiop:native-namestring output))
                       (loop for dir in include-directories collect (format nil "-I~a" (uiop:native-namestring dir)))
                       (mapcar #'uiop:native-namestring sources)
                       libraries)
               :log log))

(defun ensure-shim (&key log)
  "Build the shim library, unless it's there."
  (let ((output (shim-library))
        (prefix (or (tree-sitter-prefix)
                    (editor-error "libtree-sitter isn't installed (with Homebrew: brew install tree-sitter)"))))
    (unless (probe-file output)
      (let ((prefix (uiop:ensure-directory-pathname prefix)))
        (compile-shared-library (list (asdf:system-relative-pathname :cadre "src/core/tree-sitter/shim.c"))
                                output
                                :include-directories (list (merge-pathnames "include/" prefix))
                                :libraries (list (format nil "-L~a" (uiop:native-namestring (merge-pathnames "lib/" prefix)))
                                                 "-ltree-sitter"
                                                 (format nil "-Wl,-rpath,~a" (uiop:native-namestring (merge-pathnames "lib/" prefix))))
                                :log log)))
    output))

(defun install-tree-sitter-language (name &key log)
  "Build the grammar of the language NAME and copy its highlight queries:
clone its repositories at their pinned versions, compile. LOG, if given, is
called with each line of progress."
  (let ((spec (or (tree-sitter-language-spec name) (editor-error "Cadre doesn't know the language ~a" name))))
    (ensure-shim :log log)
    (dolist (repo (getf spec :repos)) (ensure-repo repo :log log))
    (let* (;; :source starts with the first repository's name; it is cloned as repo-tag.
           (source (merge-pathnames (subseq (getf spec :source) (1+ (position #\/ (getf spec :source))))
                                    (repo-directory (first (getf spec :repos)))))
           (files (remove nil (list (probe-file (merge-pathnames "parser.c" source))
                                    (probe-file (merge-pathnames "scanner.c" source))))))
      (unless files (editor-error "No parser.c in ~a" (uiop:native-namestring source)))
      (when log (funcall log (format nil "Compiling the ~a grammar" (getf spec :title))))
      (compile-shared-library files (language-library name) :include-directories (list source) :log log))
    (let ((query (with-output-to-string (out)
                   (loop for (repo path) in (getf spec :highlights)
                         for file = (merge-pathnames path (repo-directory repo))
                         when (probe-file file)
                           do (format out "; From ~a/~a~%~a~%" repo path (read-text-file file))))))
      (ensure-directories-exist (language-highlights-file name))
      (with-open-file (out (language-highlights-file name) :direction :output :if-exists :supersede
                                                           :external-format :utf-8)
        (write-string query out)))
    (let ((injections (getf spec :injections)))
      (when injections
        (with-open-file (out (language-injections-file name) :direction :output :if-exists :supersede
                                                             :external-format :utf-8)
          (write-string (read-text-file (merge-pathnames (second injections) (repo-directory (first injections)))) out))))
    (when log (funcall log (format nil "Installed ~a" (getf spec :title))))
    name))

;;; The library

(defvar *tree-sitter-loaded* nil)

(defun load-tree-sitter ()
  "Load libtree-sitter and the shim, once. Signals an editor-error if they're missing."
  (unless *tree-sitter-loaded*
    (let ((prefix (or (tree-sitter-prefix) (editor-error "libtree-sitter isn't installed (with Homebrew: brew install tree-sitter)"))))
      (unless (probe-file (shim-library))
        (editor-error "No tree-sitter grammars are installed yet (M-x install-language-grammar)"))
      (cffi:load-foreign-library (merge-pathnames (format nil "lib/libtree-sitter.~a" (shared-library-type))
                                                  (uiop:ensure-directory-pathname prefix)))
      (cffi:load-foreign-library (shim-library))
      (setf *tree-sitter-loaded* t)))
  t)

(cffi:defcfun ("cts_parser_new" %parser-new) :pointer (language :pointer))
(cffi:defcfun ("cts_parser_delete" %parser-delete) :void (parser :pointer))
(cffi:defcfun ("cts_language_abi" %language-abi) :uint32 (language :pointer))
(cffi:defcfun ("cts_parse" %parse) :pointer (parser :pointer) (old :pointer) (text :pointer) (length :uint32))
(cffi:defcfun ("cts_tree_delete" %tree-delete) :void (tree :pointer))
(cffi:defcfun ("cts_tree_has_error" %tree-has-error) :uint32 (tree :pointer))
(cffi:defcfun ("cts_query_new" %query-new) :pointer
  (language :pointer) (source :pointer) (length :uint32) (error-offset :pointer) (error-type :pointer))
(cffi:defcfun ("cts_query_delete" %query-delete) :void (query :pointer))
(cffi:defcfun ("cts_query_pattern_count" %query-pattern-count) :uint32 (query :pointer))
(cffi:defcfun ("cts_query_capture_count" %query-capture-count) :uint32 (query :pointer))
(cffi:defcfun ("cts_query_capture_name" %query-capture-name) :pointer (query :pointer) (index :uint32) (length :pointer))
(cffi:defcfun ("cts_query_string" %query-string) :pointer (query :pointer) (index :uint32) (length :pointer))
(cffi:defcfun ("cts_query_predicates" %query-predicates) :uint32 (query :pointer) (pattern :uint32) (out :pointer) (max :uint32))
(cffi:defcfun ("cts_matches" %matches) :uint32
  (tree :pointer) (query :pointer) (start :uint32) (end :uint32) (out :pointer) (max :uint32) (truncated :pointer))
(cffi:defcfun ("cts_multiline_nodes" %multiline-nodes) :uint32 (tree :pointer) (out :pointer) (max :uint32))

(defun foreign-utf8 (pointer length)
  (sb-ext:octets-to-string (let ((v (make-array length :element-type '(unsigned-byte 8))))
                             (dotimes (i length v) (setf (aref v i) (cffi:mem-aref pointer :uint8 i))))
                           :external-format :utf-8))

;;; Queries

(defstruct (ts-query (:constructor %make-ts-query))
  pointer captures predicates)          ; capture names; per pattern, a list of predicates

(defun query-error-message (type)
  (case type (1 "syntax error") (2 "unknown node type") (3 "unknown field") (4 "unknown capture")
    (5 "impossible pattern") (6 "bad language") (t "error")))

(defun compile-query (language source)
  "A TS-QUERY from SOURCE for LANGUAGE (a pointer), or signals an
editor-error saying where SOURCE is wrong."
  (let ((octets (sb-ext:string-to-octets source :external-format :utf-8)))
    (cffi:with-foreign-objects ((text :uint8 (max 1 (length octets))) (offset :uint32) (type :uint32))
      (dotimes (i (length octets)) (setf (cffi:mem-aref text :uint8 i) (aref octets i)))
      (let ((pointer (%query-new language text (length octets) offset type)))
        (when (cffi:null-pointer-p pointer)
          (let* ((at (cffi:mem-ref offset :uint32))
                 (char (length (sb-ext:octets-to-string octets :end (min at (length octets)) :external-format :utf-8))))
            (editor-error "Query ~a at line ~d: ~a" (query-error-message (cffi:mem-ref type :uint32))
                          (1+ (count #\Newline source :end (min char (length source))))
                          (string-trim '(#\Space #\Newline)
                                       (subseq source char (min (length source) (+ char 60)))))))
        (let ((query (%make-ts-query :pointer pointer)))
          (setf (ts-query-captures query)
                (coerce (loop for i below (%query-capture-count pointer)
                              collect (cffi:with-foreign-object (length :uint32)
                                        (let ((name (%query-capture-name pointer i length)))
                                          (foreign-utf8 name (cffi:mem-ref length :uint32)))))
                        'vector)
                (ts-query-predicates query) (read-predicates pointer))
          (sb-ext:finalize query (lambda () (%query-delete pointer)) :dont-save t)
          query)))))

(defun read-predicates (pointer)
  "Each pattern's predicates, as lists (operator argument…): a capture
argument is its index, a string argument a string."
  (flet ((string-value (id)
           (cffi:with-foreign-object (length :uint32)
             (foreign-utf8 (%query-string pointer id length) (cffi:mem-ref length :uint32)))))
    (coerce
     (loop for pattern below (%query-pattern-count pointer)
           collect (let ((count (cffi:with-foreign-object (out :uint32 2) (%query-predicates pointer pattern out 0))))
                     (if (zerop count)
                         '()
                         (cffi:with-foreign-object (out :uint32 (* 2 count))
                           (%query-predicates pointer pattern out count)
                           (let ((predicates '()) (current '()))
                             (dotimes (i count)
                               (let ((type (cffi:mem-aref out :uint32 (* 2 i)))
                                     (id (cffi:mem-aref out :uint32 (1+ (* 2 i)))))
                                 (case type
                                   (0 (push (nreverse current) predicates) (setf current '()))
                                   (1 (push (list :capture id) current))
                                   (2 (push (string-value id) current)))))
                             (nreverse predicates))))))
     'vector)))

(defun predicate-holds-p (predicate capture-text)
  "Whether PREDICATE (operator argument…) holds; CAPTURE-TEXT gives a capture's text by index."
  (destructuring-bind (operator &rest arguments) predicate
    (flet ((value (argument) (if (consp argument) (funcall capture-text (second argument)) argument)))
      (let ((operator (if (stringp operator) operator "")))
        (cond ((member operator '("eq?" "not-eq?") :test #'string=)
               (let ((same (equal (value (first arguments)) (value (second arguments)))))
                 (if (string= operator "eq?") same (not same))))
              ((member operator '("match?" "not-match?") :test #'string=)
               (let ((found (and (value (first arguments))
                                 (ignore-errors (ppcre:scan (value (second arguments)) (value (first arguments)))))))
                 (if (string= operator "match?") (and found t) (not found))))
              ((member operator '("any-of?" "not-any-of?") :test #'string=)
               (let ((found (member (value (first arguments)) (mapcar #'value (rest arguments)) :test #'equal)))
                 (if (string= operator "any-of?") (and found t) (not found))))
              ;; is?, is-not?, set!… and anything unknown: no opinion.
              (t t))))))

;;; Languages, loaded

(defstruct (ts-language (:constructor %make-ts-language))
  name pointer highlights outline injections comment block-comment problems (precedence :last))

(defvar *loaded-languages* (make-hash-table :test 'equal))

(defun compile-highlights (language name)
  "The highlight query of NAME: all of it, or, if that doesn't compile, the
parts that do. Returns the query and a list of problems."
  (let ((source (read-text-file (language-highlights-file name))))
    (handler-case (values (compile-query language source) '())
      (editor-error (whole)
        ;; One file at a time (the file markers split them), keeping those that compile.
        (let ((parts (ppcre:split "(?m)^; From " source)) (good '()) (problems (list (editor-error-message whole))))
          (dolist (part parts)
            (let ((part (if (plusp (length part)) (concatenate 'string "; From " part) part)))
              (handler-case (progn (compile-query language part) (push part good))
                (editor-error (e) (push (editor-error-message e) problems)))))
          (values (and good (compile-query language (format nil "~{~a~%~}" (nreverse good))))
                  (nreverse problems)))))))

(defun load-ts-language (name)
  "The language NAME, loaded (once): its grammar and its queries."
  (or (gethash name *loaded-languages*)
      (let ((spec (or (tree-sitter-language-spec name) (editor-error "Cadre doesn't know the language ~a" name))))
        (unless (tree-sitter-language-installed-p name)
          (editor-error "The ~a grammar isn't installed (M-x install-language-grammar)" (getf spec :title)))
        (load-tree-sitter)
        (cffi:load-foreign-library (language-library name))
        (let ((pointer (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer (getf spec :symbol)) () :pointer)))
          (multiple-value-bind (highlights problems) (compile-highlights pointer name)
            (setf (gethash name *loaded-languages*)
                  (%make-ts-language :name name :pointer pointer :highlights highlights
                                     :outline (ignore-errors (compile-query pointer (getf spec :outline)))
                                     :comment (getf spec :comment) :block-comment (getf spec :block-comment)
                                     :injections (and (probe-file (language-injections-file name))
                                                      (ignore-errors (compile-query pointer (read-text-file (language-injections-file name)))))
                                     :problems problems
                                     :precedence (getf spec :precedence :last))))))))

;;; Documents: a text and its tree

(defstruct (ts-document (:constructor %make-ts-document))
  language
  (cell (list nil nil))                 ; (parser tree), freed when the document is collected
  (lock (sb-thread:make-mutex :name "tree-sitter parser"))
  (injected (make-hash-table :test 'equal)) ; (language start-byte text) → document, for this parse
  string octets
  byte-chars                            ; byte offset → char offset, or nil if ASCII
  char-bytes)                           ; char offset → byte offset, or nil if ASCII

(defun ts-document-parser (document) (first (ts-document-cell document)))
(defun ts-document-tree (document) (second (ts-document-cell document)))

(defun make-ts-document (language)
  "A document for LANGUAGE (a TS-LANGUAGE), with nothing parsed yet."
  (let ((parser (%parser-new (ts-language-pointer language))))
    (when (cffi:null-pointer-p parser)
      (editor-error "This libtree-sitter can't use the ~a grammar (its ABI is ~d)" (ts-language-name language)
                    (%language-abi (ts-language-pointer language))))
    (let* ((cell (list parser nil))
           (document (%make-ts-document :language language :cell cell)))
      (sb-ext:finalize document (lambda ()
                                  (when (second cell) (%tree-delete (second cell)))
                                  (%parser-delete (first cell)))
                       :dont-save t)
      document)))

(defun ts-parse-state (document string)
  "Parse STRING with DOCUMENT's parser, without changing what DOCUMENT shows:
returns the new state for TS-INSTALL-STATE. Safe on another thread (one
parse at a time per document)."
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8))
         (n (length octets))
         (byte-chars nil) (char-bytes nil))
    (unless (= n (length string))
      (setf byte-chars (make-array (1+ n) :element-type '(unsigned-byte 32))
            char-bytes (make-array (1+ (length string)) :element-type '(unsigned-byte 32)))
      (let ((byte 0))
        (dotimes (c (length string))
          (let ((width (let ((code (char-code (char string c))))
                         (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))))
            (setf (aref char-bytes c) byte)
            (dotimes (k width) (setf (aref byte-chars (+ byte k)) c))
            (incf byte width))))
      (setf (aref char-bytes (length string)) n (aref byte-chars n) (length string)))
    (let ((tree (sb-thread:with-mutex ((ts-document-lock document))
                  (sb-sys:with-pinned-objects (octets)
                    (%parse (ts-document-parser document) (cffi:null-pointer) (sb-sys:vector-sap octets) n)))))
      (list :tree tree :string string :octets octets :byte-chars byte-chars :char-bytes char-bytes))))

(defun ts-install-state (document state)
  "Make DOCUMENT show STATE (from TS-PARSE-STATE), freeing the tree it replaces."
  (let ((cell (ts-document-cell document)))
    (when (second cell) (%tree-delete (second cell)))
    (setf (second cell) (getf state :tree)
          (ts-document-string document) (getf state :string)
          (ts-document-octets document) (getf state :octets)
          (ts-document-byte-chars document) (getf state :byte-chars)
          (ts-document-char-bytes document) (getf state :char-bytes))
    (clrhash (ts-document-injected document))
    document))

(defun ts-parse (document string)
  "Parse STRING (the whole text) into DOCUMENT."
  (ts-install-state document (ts-parse-state document string)))

(defun byte-char (document byte)
  (let ((map (ts-document-byte-chars document)))
    (if map (aref map (min byte (1- (length map)))) byte)))

(defun char-byte (document char)
  (let ((map (ts-document-char-bytes document)))
    (if map (aref map (min char (1- (length map)))) char)))

(defun ts-has-error-p (document)
  (and (ts-document-tree document) (plusp (%tree-has-error (ts-document-tree document)))))

(defun query-matches (document query start-byte end-byte)
  "The matches of QUERY between the two byte offsets, as lists (pattern
(capture start-byte end-byte)…)."
  (let ((tree (ts-document-tree document)))
    (when tree
      (loop with size = 65536
            do (cffi:with-foreign-objects ((out :uint32 size) (truncated :uint32))
                 (let ((n (%matches tree (ts-query-pointer query) start-byte end-byte out size truncated)))
                   (when (or (zerop (cffi:mem-ref truncated :uint32)) (> size (* 64 1024 1024)))
                     (return
                       (loop with i = 0
                             while (< i n)
                             collect (let ((pattern (cffi:mem-aref out :uint32 i))
                                           (count (cffi:mem-aref out :uint32 (1+ i))))
                                       (incf i 2)
                                       (cons pattern
                                             (loop repeat count
                                                   collect (prog1 (list (cffi:mem-aref out :uint32 i)
                                                                        (cffi:mem-aref out :uint32 (+ i 1))
                                                                        (cffi:mem-aref out :uint32 (+ i 2)))
                                                             (incf i 3))))))))))
               (setf size (* size 4))))))

(defun accepted-matches (document query start-byte end-byte)
  "QUERY-MATCHES whose predicates hold."
  (let ((octets (ts-document-octets document)))
    (remove-if-not
     (lambda (match)
       (let ((predicates (aref (ts-query-predicates query) (first match))))
         (or (null predicates)
             (flet ((capture-text (index)
                      (let ((capture (find index (rest match) :key #'first)))
                        (and capture (sb-ext:octets-to-string octets :start (second capture) :end (third capture)
                                                                     :external-format :utf-8)))))
               (every (lambda (p) (predicate-holds-p p #'capture-text)) predicates)))))
     (query-matches document query start-byte end-byte))))

;;; Highlighting

(defparameter *code-faces* '(:code-function :code-type :code-property)
  "Faces for kinds of names that Lisp doesn't distinguish.")

(defparameter *capture-faces*
  '(("comment" . :comment) ("string.special.key" . :code-property) ("string" . :string)
    ("escape" . :character) ("number" . :number) ("keyword" . :keyword)
    ("constant.builtin" . :builtin) ("constant" . :constant) ("function.builtin" . :builtin)
    ("function" . :code-function) ("constructor" . :code-type) ("type" . :code-type)
    ("variable.builtin" . :builtin) ("property" . :code-property) ("attribute" . :code-property)
    ("tag" . :code-type) ("label" . :code-property) ("module" . :code-type) ("embedded" . nil))
  "Capture name → face. A name without an entry uses its prefix's (function.method uses function's).")

(defun capture-face (name)
  "The face for the capture NAME, or nil."
  (loop for current = name then (subseq current 0 (position #\. current :from-end t))
        do (let ((entry (assoc current *capture-faces* :test #'string=)))
             (when entry (return (cdr entry))))
        while (find #\. current)))

(defun own-highlight-spans (document start-char end-char)
  "The faces DOCUMENT's own highlight query gives its text between the two
char offsets, as sorted, non-overlapping (start end face) in chars."
  (let* ((query (ts-language-highlights (ts-document-language document)))
         (start-byte (char-byte document start-char))
         (end-byte (char-byte document end-char))
         (width (max 0 (- end-byte start-byte))))
    (when (and query (plusp width))
      (let ((captures '())
            (paint (make-array width :initial-element nil)))
        (dolist (match (accepted-matches document query start-byte end-byte))
          (dolist (capture (rest match))
            (destructuring-bind (index s e) capture
              (when (< s e)
                (push (list s e (first match) (capture-face (aref (ts-query-captures query) index))) captures)))))
        ;; Outer nodes first, so inner ones paint over them; on the same node,
        ;; the winning pattern last.
        (let ((last-wins (eq (ts-language-precedence (ts-document-language document)) :last)))
          (setf captures (sort captures (lambda (a b)
                                          (let ((la (- (second a) (first a))) (lb (- (second b) (first b))))
                                            (or (> la lb)
                                                (and (= la lb) (if last-wins
                                                                   (< (third a) (third b))
                                                                   (> (third a) (third b))))))))))
        (dolist (c captures)
          (destructuring-bind (s e pattern face) c
            (declare (ignore pattern))
            (loop for b from (max s start-byte) below (min e end-byte)
                  do (setf (aref paint (- b start-byte)) (or face :none)))))
        ;; Runs of one face, in chars.
        (let ((spans '()) (run-start 0) (run-face (aref paint 0)))
          (flet ((close-run (end)
                   (when (and run-face (not (eq run-face :none)))
                     (push (list (byte-char document (+ start-byte run-start))
                                 (byte-char document (+ start-byte end))
                                 run-face)
                           spans))))
            (loop for i from 1 below width
                  unless (eq (aref paint i) run-face)
                    do (close-run i) (setf run-start i run-face (aref paint i)))
            (close-run width))
          (nreverse spans))))))

;;; Code in other languages inside (injections)

(defun injection-regions (document start-byte end-byte)
  "The regions between the two byte offsets that hold code in another
language, as (start-byte end-byte language-name), from the language's
injections query (@injection.content, with the language from
#set! injection.language or an @injection.language capture)."
  (let ((query (ts-language-injections (ts-document-language document)))
        (octets (ts-document-octets document))
        (regions '()))
    (when query
      (dolist (match (query-matches document query start-byte end-byte))
        (let ((content nil)
              (language (loop for p in (aref (ts-query-predicates query) (first match))
                              when (and (equal (first p) "set!") (equal (second p) "injection.language"))
                                return (third p))))
          (dolist (capture (rest match))
            (let ((name (aref (ts-query-captures query) (first capture))))
              (cond ((string= name "injection.content") (setf content capture))
                    ((string= name "injection.language")
                     (setf language (sb-ext:octets-to-string octets :start (second capture) :end (third capture)
                                                                    :external-format :utf-8))))))
          (when (and content language (< (second content) (third content)))
            (push (list (second content) (third content) (string-downcase language)) regions)))))
    (nreverse regions)))

(defun injected-document (document start end language)
  "A document for the LANGUAGE code between the bytes START and END of DOCUMENT,
parsed once per parse of DOCUMENT; nil if LANGUAGE isn't installed."
  (let* ((text (sb-ext:octets-to-string (ts-document-octets document) :start start :end end :external-format :utf-8))
         (key (list language start text)))
    (multiple-value-bind (cached found) (gethash key (ts-document-injected document))
      (if found
          cached
          (setf (gethash key (ts-document-injected document))
                (and (tree-sitter-language-spec language)
                     (tree-sitter-language-installed-p language)
                     (ignore-errors
                      (let ((inner (make-ts-document (load-ts-language language))))
                        (ts-parse inner text)
                        inner))))))))

(defun ts-highlight-spans (document start-char end-char)
  "The faces of DOCUMENT's text between the two char offsets, as sorted,
non-overlapping (start end face) in chars, code in other languages inside
colored by theirs."
  (let* ((start-byte (char-byte document start-char))
         (end-byte (char-byte document end-char))
         (regions (injection-regions document start-byte end-byte))
         (spans (own-highlight-spans document start-char end-char)))
    (if (null regions)
        spans
        (let ((inside '()))
          (dolist (region regions)
            (destructuring-bind (rs re language) region
              (let ((inner (injected-document document rs re language))
                    (rs-char (byte-char document rs))
                    (re-char (byte-char document re)))
                ;; The region is the inner language's: drop the outer faces there.
                (setf spans (remove-if (lambda (span) (and (< (first span) re-char) (> (second span) rs-char))) spans))
                (when inner
                  (dolist (span (ts-highlight-spans inner (max 0 (- start-char rs-char))
                                                    (- (min end-char re-char) rs-char)))
                    (push (list (+ rs-char (first span)) (+ rs-char (second span)) (third span)) inside))))))
          (sort (append spans inside) #'< :key #'first)))))

;;; Folding and the outline

(defun ts-fold-ranges (document)
  "Fold ranges (first last) from the nodes that span lines: one per line, the longest."
  (let ((tree (ts-document-tree document))
        (longest (make-hash-table)))
    (when tree
      (loop with size = 16384
            do (cffi:with-foreign-object (out :uint32 (* 2 size))
                 (let ((n (%multiline-nodes tree out size)))
                   (when (or (< n size) (> size (* 4 1024 1024)))
                     (dotimes (i (min n size))
                       (let ((first (cffi:mem-aref out :uint32 (* 2 i)))
                             (last (cffi:mem-aref out :uint32 (1+ (* 2 i)))))
                         (setf (gethash first longest) (max last (gethash first longest 0)))))
                     (return))))
               (setf size (* size 4))))
    (sort (loop for first being the hash-keys of longest using (hash-value last) collect (list first last))
          #'< :key #'first)))

(defun ts-outline (document)
  "What DOCUMENT defines, as (name kind line depth), in order: from the
language's outline query, @name inside @definition.KIND; depth is how many
definitions it sits in."
  (let ((query (ts-language-outline (ts-document-language document)))
        (string (ts-document-string document)))
    (when (and query string)
      (let ((items '()))
        (dolist (match (accepted-matches document query 0 (length (ts-document-octets document))))
          (let ((name nil) (definition nil) (kind nil))
            (dolist (capture (rest match))
              (let ((capture-name (aref (ts-query-captures query) (first capture))))
                (cond ((string= capture-name "name") (setf name capture))
                      ((and (> (length capture-name) 11) (string= "definition." capture-name :end2 11))
                       (setf definition capture
                             kind (intern (string-upcase (subseq capture-name 11)) :keyword))))))
            (when (and name definition)
              (push (list (subseq string (byte-char document (second name)) (byte-char document (third name)))
                          kind
                          (byte-char document (second definition))
                          (second definition) (third definition))
                    items))))
        ;; Lines from a table of newlines; depth from a stack of the definitions still open.
        (let ((*newline-positions* (cons string (newline-positions string)))
              (open '()))
          (loop for item in (sort items #'< :key #'fourth)
                collect (destructuring-bind (name kind char start end) item
                          (loop while (and open (<= (first open) start)) do (pop open))
                          (prog1 (list name kind (line-at-position string char) (length open))
                            (push end open)))))))))
