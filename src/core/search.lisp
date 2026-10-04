;;;; search.lisp — finding text and symbols in a project's files
;;;;
;;;; Matches are reported per line as (line start end), lines and columns
;;;; from 0. A whole-word match is bounded by characters that can't be part
;;;; of a Lisp symbol, so foo doesn't match inside foo-bar. Symbol
;;;; occurrences come from the lexer, so strings and comments are skipped,
;;;; and a package-qualified symbol (pkg:foo) counts as foo.

(in-package #:cadre)

(defun directory-link-p (directory)
  "True if DIRECTORY (a directory pathname) is a symbolic link. Comparing
truenames would be wrong where a parent is a link, as /var is on macOS."
  (let ((path (string-right-trim "/" (uiop:native-namestring directory))))
    (ignore-errors (sb-posix:s-islnk (sb-posix:stat-mode (sb-posix:lstat path))))))

(defun project-files (directory &key (hidden-names '(".git")) (hidden-types '()) (max-size 2000000))
  "Every file under DIRECTORY, leaving out HIDDEN-NAMES (files or folders),
files whose type is in HIDDEN-TYPES, and files bigger than MAX-SIZE bytes."
  (let ((files '()))
    (labels ((hidden-p (name) (member name hidden-names :test #'string=))
             (walk (dir)
               (dolist (file (uiop:directory-files dir))
                 (let ((name (file-namestring file)))
                   (unless (or (hidden-p name)
                               (member (pathname-type file) hidden-types :test #'equalp)
                               (let ((size (ignore-errors (with-open-file (in file) (file-length in)))))
                                 (or (null size) (> size max-size))))
                     (push file files))))
               (dolist (sub (uiop:subdirectories dir))
                 (unless (or (hidden-p (car (last (pathname-directory sub))))
                             ;; Don't follow links to directories (they may loop).
                             (directory-link-p sub))
                   (walk sub)))))
      (walk (uiop:ensure-directory-pathname directory)))
    (sort files #'string< :key #'namestring)))

(defun read-text-file (pathname)
  "PATHNAME's contents as a string, or nil if it looks binary or can't be read."
  (let ((octets (ignore-errors
                 (with-open-file (in pathname :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                     (read-sequence v in)
                     v)))))
    (when (and octets (not (find 0 octets :end (min 8000 (length octets)))))
      (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
        (error () (sb-ext:octets-to-string octets :external-format :latin-1))))))

(defun split-text-lines (text)
  (loop with start = 0
        for nl = (position #\Newline text :start start)
        collect (subseq text start (or nl (length text)))
        while nl do (setf start (1+ nl))))

(defun word-boundary-p (line index)
  (or (< index 0) (>= index (length line)) (not (symbol-constituent-p (char line index)))))

(defun regex-scanner (pattern &key case-sensitive)
  "A scanner for the regular expression PATTERN (Perl syntax). Signals an
editor-error if it isn't valid."
  (handler-case (cl-ppcre:create-scanner pattern :case-insensitive-mode (not case-sensitive))
    (cl-ppcre:ppcre-syntax-error (e)
      (error 'editor-error :message (format nil "Bad regular expression: ~a" e)))))

(defun line-matches (line pattern &key case-sensitive whole-word regex)
  "The (start . end) of each match of PATTERN in LINE. With REGEX, PATTERN
is a regular expression or a scanner from REGEX-SCANNER."
  (loop for (start . end) in (if regex
                                 (let ((scanner (if (stringp pattern)
                                                    (regex-scanner pattern :case-sensitive case-sensitive)
                                                    pattern)))
                                   (loop for (s e) on (cl-ppcre:all-matches scanner line) by #'cddr
                                         when (< s e) collect (cons s e)))
                                 (find-all pattern line :case-fold (not case-sensitive)))
        when (or (not whole-word)
                 (and (word-boundary-p line (1- start)) (word-boundary-p line end)))
          collect (cons start end)))

(defun text-matches (text pattern &key case-sensitive whole-word regex)
  "The matches of PATTERN in TEXT, as (line start end)."
  (when (plusp (length pattern))
    (let ((pattern (if regex (regex-scanner pattern :case-sensitive case-sensitive) pattern)))
      (loop for line in (split-text-lines text)
            for n from 0
            append (loop for (start . end) in (line-matches line pattern :case-sensitive case-sensitive
                                                                         :whole-word whole-word :regex regex)
                         collect (list n start end))))))

(defun regex-replacement (pattern match replacement &key case-sensitive)
  "REPLACEMENT for MATCH of the regular expression PATTERN, with \\1 … filled in."
  (cl-ppcre:regex-replace (regex-scanner pattern :case-sensitive case-sensitive) match replacement))

(defun symbol-name-part (token-text)
  "The start of the symbol's name in TOKEN-TEXT, after any package prefix."
  (let ((colon (position #\: token-text :from-end t)))
    (if (and colon (< colon (1- (length token-text)))) (1+ colon) 0)))

(defun symbol-occurrences (text name)
  "Where the symbol NAME (ignoring case and package prefixes) occurs in TEXT,
which is Lisp source: (line start end) of the name, without its prefix."
  (let* ((base (subseq name (symbol-name-part name)))
         (state '(:code 0)))
    (loop for line in (split-text-lines text)
          for n from 0
          append (multiple-value-bind (tokens end) (lex-line line state)
                   (setf state end)
                   (loop for tk in tokens
                         when (eq (token-type tk) :symbol)
                           append (let* ((token-text (subseq line (token-start tk) (token-end tk)))
                                         (offset (symbol-name-part token-text)))
                                    (when (string-equal base token-text :start2 offset)
                                      (list (list n (+ (token-start tk) offset) (token-end tk))))))))))

(defun replace-matches (text matches replacement)
  "TEXT with each match (line start end) replaced by REPLACEMENT."
  (let ((lines (coerce (split-text-lines text) 'vector)))
    (loop for (line start end) in (sort (copy-list matches)
                                        (lambda (a b) (or (> (first a) (first b))
                                                          (and (= (first a) (first b)) (> (second a) (second b))))))
          do (setf (aref lines line)
                   (concatenate 'string (subseq (aref lines line) 0 start) replacement
                                (subseq (aref lines line) end))))
    (format nil "~{~a~^~%~}" (coerce lines 'list))))
