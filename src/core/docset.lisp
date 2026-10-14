;;;; docset.lisp — reading docsets, the documentation the HyperSpec tab shows
;;;;
;;;; A docset is a directory of three files of plain Lisp data: manifest.sexp
;;;; (what it is), index.sexp (one record per entry: names, kind, package,
;;;; summary, and the entry's file position) and entries.sexp (the entries,
;;;; read one at a time when first shown). The format is spec-browser's
;;;; (docs/docset-format.md there); this is its reader, copied here so core
;;;; keeps no dependencies. Cadre ships the Common Lisp docset, made from the
;;;; dpANS3 draft of the standard, in data/docsets/; gtk4 and raylib build
;;;; theirs in their own docs/docset/.

(defpackage #:cadre-docset
  (:use #:cl)
  (:export
   #:docset #:open-docset #:docset-directory #:docset-manifest #:docset-index
   #:docset-id #:docset-title #:docset-packages #:docset-toc
   #:docset-entry #:entry-record
   #:record-id #:record-names #:record-kind #:record-kind-name #:record-package
   #:record-parent #:record-summary
   #:find-docsets #:*library-systems*
   #:find-records #:lookup-symbol #:search-names
   #:entry-id #:entry-names #:entry-kind-name #:entry-sections #:entry-section))

(defpackage #:cadre-docset.data
  (:use)
  (:import-from #:cl #:nil #:t)
  (:documentation "The package docset data is read in: NIL and T are Lisp's, and any
other symbol a docset should not contain lands here, not in a real package."))

(in-package #:cadre-docset)

(defun read-data (stream &optional (eof-error-p t))
  "Reads one form of docset data, with stray symbols kept out of real packages."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package '#:cadre-docset.data)))
      (read stream eof-error-p nil))))

(defun data-file (directory name)
  (merge-pathnames name (uiop:ensure-directory-pathname directory)))

;;; Reading

(defstruct (docset (:constructor %make-docset))
  directory manifest index
  (by-id (make-hash-table :test 'equal))
  (by-name (make-hash-table :test 'equal))   ; lowercase name -> records
  (cache (make-hash-table :test 'equal)))    ; id -> entry, once read

(defun record-id (record) (first record))
(defun record-names (record) (getf (rest record) :names))
(defun record-kind (record) (getf (rest record) :kind))
(defun record-kind-name (record) (getf (rest record) :kind-name))
(defun record-package (record) (getf (rest record) :package))
(defun record-parent (record) (getf (rest record) :parent))
(defun record-summary (record) (getf (rest record) :summary))
(defun record-position (record) (getf (rest record) :position))

(defun docset-id (docset) (getf (docset-manifest docset) :id))
(defun docset-title (docset) (getf (docset-manifest docset) :title))
(defun docset-packages (docset) (getf (docset-manifest docset) :packages))
(defun docset-toc (docset) (getf (docset-manifest docset) :toc))

(defun open-docset (directory)
  "Reads DIRECTORY's manifest and index. Entries are read when asked for."
  (let* ((directory (uiop:ensure-directory-pathname directory))
         (manifest (with-open-file (in (data-file directory "manifest.sexp") :external-format :utf-8)
                     (read-data in)))
         (index (with-open-file (in (data-file directory "index.sexp") :external-format :utf-8)
                  (read-data in)))
         (docset (%make-docset :directory directory :manifest manifest :index index)))
    (unless (eql (getf manifest :format) 1)
      (error "~A is docset format ~S; this reader knows format 1." directory (getf manifest :format)))
    (dolist (record index)
      (setf (gethash (record-id record) (docset-by-id docset)) record)
      (dolist (name (record-names record))
        (push record (gethash (string-downcase name) (docset-by-name docset)))))
    (maphash (lambda (name records) (setf (gethash name (docset-by-name docset)) (nreverse records)))
             (docset-by-name docset))
    docset))

(defun entry-record (docset id)
  (gethash id (docset-by-id docset)))

(defun docset-entry (docset id)
  "The entry with ID, read from disk the first time."
  (or (gethash id (docset-cache docset))
      (let ((record (entry-record docset id)))
        (when record
          (setf (gethash id (docset-cache docset))
                (with-open-file (in (data-file (docset-directory docset) "entries.sexp")
                                    :external-format :utf-8)
                  (file-position in (record-position record))
                  (read-data in)))))))

(defun entry-id (entry) (second entry))
(defun entry-names (entry) (getf (cddr entry) :names))
(defun entry-kind-name (entry) (getf (cddr entry) :kind-name))
(defun entry-sections (entry) (getf (cddr entry) :sections))

(defun entry-section (entry label)
  (cdr (assoc label (entry-sections entry) :test #'string-equal)))

;;; Finding docsets

(defvar *library-systems* '("gtk4" "raylib")
  "ASDF systems whose docs/docset/ FIND-DOCSETS looks for, when ASDF can find them.")

(defun library-docset-directory (system-name)
  (let ((system (ignore-errors (asdf:find-system system-name nil))))
    (when system
      (let ((dir (merge-pathnames "docs/docset/" (asdf:system-source-directory system))))
        (when (probe-file (data-file dir "manifest.sexp")) dir)))))

(defun find-docsets (&key directories (systems *library-systems*))
  "Opens every docset found in DIRECTORIES (each docset in a subdirectory)
and in the docs/docset/ of SYSTEMS."
  (let ((found '()))
    (dolist (dir directories)
      (dolist (sub (directory (merge-pathnames "*/manifest.sexp" (uiop:ensure-directory-pathname dir))))
        (push (make-pathname :name nil :type nil :defaults sub) found)))
    (dolist (system systems)
      (let ((dir (library-docset-directory system)))
        (when dir (push dir found))))
    (loop for dir in (nreverse found)
          for docset = (handler-case (open-docset dir)
                         (error (e) (warn "Skipping docset ~A: ~A" dir e) nil))
          when docset collect docset)))

;;; Looking things up

(defun find-records (docset name)
  "The index records of DOCSET's entries named NAME, in any case."
  (gethash (string-downcase name) (docset-by-name docset)))

(defun lookup-symbol (docsets name package)
  "Records documenting the symbol NAME of the package named PACKAGE (a
string), across DOCSETS, as (docset . record): entries for the symbol
itself first, then entries with no package (glossary terms)."
  (let ((symbols '()) (others '()))
    (dolist (docset docsets)
      (dolist (record (find-records docset name))
        (cond ((null (record-package record)) (push (cons docset record) others))
              ((string-equal package (record-package record)) (push (cons docset record) symbols)))))
    (append (nreverse symbols) (nreverse others))))

(defun search-names (docset query &key (limit 50))
  "Records whose names contain QUERY: names starting with it first, then shorter names first."
  (let ((query (string-downcase query)) (hits '()))
    (dolist (record (docset-index docset))
      (let ((best nil))
        (dolist (name (record-names record))
          (let ((at (search query (string-downcase name))))
            (when at
              (let ((score (+ (if (zerop at) 0 1000) (length name))))
                (when (or (null best) (< score best)) (setf best score))))))
        (when best (push (cons best record) hits))))
    (mapcar #'cdr (subseq (stable-sort hits #'< :key #'car) 0 (min limit (length hits))))))
