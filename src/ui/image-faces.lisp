;;;; image-faces.lisp — highlighting from what the connected Lisp knows
;;;;
;;;; While highlighting, symbols the lexer leaves plain are looked up in a
;;;; cache of what the Lisp says they are (image.lisp in core). Names not in
;;;; the cache are collected and, shortly after, asked about in one request
;;;; per package; when the answer comes, the buffers are highlighted again.
;;;; The cache is emptied whenever the image may have changed: after
;;;; evaluating, compiling or loading anything.

(in-package #:cadre-ui)

(defvar *image-cache* (make-hash-table :test 'equal)
  "(package . name) → the name's class in the image, or :pending.")
(defvar *image-queue* (make-hash-table :test 'equal) "Package → names to ask about.")
(defvar *image-timer* nil)
(defvar *image-generation* 0 "Counts image changes, to drop answers that are out of date.")

(defparameter *image-batch-size* 300)

(defstruct (image-context (:conc-name ic-))
  package
  (locals (make-hash-table)))           ; top-level form start line → local function names

(defun image-context (buffer syntax line)
  "What HIGHLIGHT-LINE needs to add image faces in BUFFER near LINE, or nil."
  (when (and *highlight-from-image* (connected-p))
    (make-image-context :package (or (buffer-local buffer :package)
                                     (buffer-package-name syntax line)
                                     (connection-package *connection*)))))

(defun image-class (package name)
  (gethash (cons package (string-downcase name)) *image-cache*))

(defun image-function-p (package)
  (lambda (name) (eq (image-class package name) :function)))

(defun context-local-names (context syntax line)
  (let ((start (toplevel-form-bounds syntax line 0)))
    (when start
      (multiple-value-bind (names found) (gethash start (ic-locals context))
        (if found
            names
            (setf (gethash start (ic-locals context)) (local-function-names syntax line)))))))

(defun common-lisp-name-p (text)
  "True if TEXT, without a package, names a symbol in COMMON-LISP: the
lexer already knows those."
  (and (not (find #\: text)) (find-symbol (string-upcase text) :common-lisp) t))

(defun image-face (context syntax line token string)
  "The face for TOKEN (a symbol on LINE, whose text is STRING) from what the
image knows, or nil. Unknown names are queued to be asked about."
  (let ((text (subseq string (token-start token) (token-end token)))
        (package (ic-package context)))
    (when (and (not (member (token-subtype token) '(:name :feature)))
               (classifiable-name-p text)
               (not (common-lisp-name-p text)))
      (multiple-value-bind (class found) (gethash (cons package (string-downcase text)) *image-cache*)
        (cond ((not found) (queue-image-name package (string-downcase text)) nil)
              ((eq (token-subtype token) :head)
               (case class
                 (:macro :macro)
                 (:special-operator :builtin)
                 ((:unknown :symbol)
                  (and (surely-called-p syntax line token (image-function-p package)
                                        (context-local-names context syntax line))
                       :undefined-function))))
              (t (case class
                   (:special-variable :special-variable)
                   (:constant :constant))))))))

(defun queue-image-name (package name)
  (setf (gethash (cons package name) *image-cache*) :pending)
  (push name (gethash package *image-queue*))
  (unless *image-timer*
    (setf *image-timer*
          (glib:timeout-add glib:+priority-default-idle+ 150
                            (lambda () (setf *image-timer* nil) (send-image-queries) nil)))))

(defun send-image-queries ()
  (let ((generation *image-generation*)
        (queue (loop for package being the hash-keys of *image-queue* using (hash-value names)
                     collect (cons package names))))
    (clrhash *image-queue*)
    (when (connected-p)
      (loop for (package . names) in queue
            do (loop for rest = names then (nthcdr *image-batch-size* rest)
                     while rest
                     do (let ((batch (subseq rest 0 (min *image-batch-size* (length rest))))
                              (package package))
                          (rex *connection* (swank-call "swank:eval-and-grab-output"
                                                        (image-classify-source package batch))
                               :package "COMMON-LISP-USER"
                               :on-ok (lambda (reply)
                                        (when (= generation *image-generation*)
                                          (loop for name in batch
                                                for class in (parse-image-classes reply (length batch))
                                                do (setf (gethash (cons package name) *image-cache*) class))
                                          (rehighlight-lisp-buffers)))
                               :on-abort (lambda (reason)
                                           (declare (ignore reason))
                                           (dolist (name batch)
                                             (setf (gethash (cons package name) *image-cache*) nil))))))))))

(defun rehighlight-lisp-buffers ()
  (dolist (buffer (buffer-list))
    (let ((syntax (buffer-syntax buffer)))
      (when syntax
        (forget-highlighting syntax)
        (schedule-highlight buffer)))))

(defun image-changed ()
  "The image may have changed (something was evaluated, compiled or loaded):
forget what it said about names, and ask again."
  (incf *image-generation*)
  (clrhash *image-cache*)
  (clrhash *image-queue*)
  (rehighlight-lisp-buffers))
