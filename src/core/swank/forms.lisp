;;;; forms.lisp — from buffer text to Swank requests, and back
;;;;
;;;; Which package a buffer's code is in, the form around the cursor (for
;;;; argument hints), the symbol at the cursor, and Swank's source locations
;;;; and compiler notes.

(in-package #:cadre)

;;; The buffer's package: the last top-level (in-package …) before a line.

(defun package-designator-name (string)
  "The package name a designator written as STRING names: FOO for foo,
:foo, #:foo or \"FOO\"."
  (let ((s (string-trim " " string)))
    (cond ((and (> (length s) 1) (char= (char s 0) #\")) (string-trim "\"" s))
          ((and (> (length s) 2) (string= "#:" s :end2 2)) (string-upcase (subseq s 2)))
          ((and (> (length s) 1) (char= (char s 0) #\:)) (string-upcase (subseq s 1)))
          ((and (> (length s) 1) (char= (char s 0) #\|)) (string-trim "|" s))
          (t (string-upcase s)))))

(defun buffer-package-name (syntax line)
  "The package from the last top-level IN-PACKAGE form before LINE, or nil."
  (loop for l from (min line (1- (syntax-line-count syntax))) downto 0
        for tokens = (line-tokens syntax l)
        do (when (and (>= (length tokens) 3)
                      (eq (token-type (aref tokens 0)) :open)
                      (zerop (token-depth (aref tokens 0)))
                      (eq (token-type (aref tokens 1)) :symbol))
             (let ((string (text-line-string (syntax-text syntax) l)))
               (when (string-equal "in-package"
                                   (symbol-base-name (subseq string (token-start (aref tokens 1))
                                                             (token-end (aref tokens 1)))))
                 (let ((designator (aref tokens 2)))
                   (return (package-designator-name
                            (subseq string (token-start designator) (token-end designator))))))))))

;;; The symbol at the cursor

(defun symbol-at (syntax line column)
  "The text of the symbol at or just before (LINE, COLUMN), or nil."
  (let* ((tokens (line-tokens syntax line))
         (token (find-if (lambda (tk) (and (eq (token-type tk) :symbol)
                                           (<= (token-start tk) column (token-end tk))))
                         tokens)))
    (and token (subseq (text-line-string (syntax-text syntax) line) (token-start token) (token-end token)))))

(defun symbol-prefix-at (syntax line column)
  "The part of the symbol before (LINE, COLUMN), and the column where it
starts; nil if the cursor is not just after a symbol or keyword."
  (let* ((tokens (line-tokens syntax line))
         (token (find-if (lambda (tk) (and (member (token-type tk) '(:symbol :keyword))
                                           (< (token-start tk) column)
                                           (<= column (token-end tk))))
                         tokens)))
    (and token
         (values (subseq (text-line-string (syntax-text syntax) line) (token-start token) column)
                 (token-start token)))))

;;; The form around the cursor, for swank:autodoc
;;;
;;; Autodoc wants the forms around the cursor as nested lists of strings,
;;; with the symbol swank::%cursor-marker% where the cursor is:
;;;   (format t |)  →  ("format" "t" "" swank::%cursor-marker%)

(defparameter +cursor-marker+ (remote-symbol "swank::%cursor-marker%"))

(defun element-text (syntax line column end-line end-column)
  (with-output-to-string (out)
    (loop for l from line to end-line
          for string = (text-line-string (syntax-text syntax) l)
          do (write-string string out :start (if (= l line) column 0)
                                      :end (if (= l end-line) end-column (length string)))
             (unless (= l end-line) (write-char #\Newline out)))))

(defun raw-form-at (syntax line column &key (levels 3))
  "The forms around (LINE, COLUMN), innermost LEVELS deep, as autodoc wants
them; nil outside any list."
  (labels ((elements-before (open-line open-column stop-line stop-column inner)
             ;; The elements of the list at OPEN, up to STOP, as strings;
             ;; INNER (a list) replaces the element containing STOP.
             (multiple-value-bind (ol oi) (open-place syntax open-line open-column)
               (let ((items '()))
                 (dolist (e (list-elements syntax ol oi))
                   (destructuring-bind (l c tk) e
                     (declare (ignore tk))
                     (when (or (> l stop-line) (and (= l stop-line) (>= c stop-column)))
                       (return))
                     (multiple-value-bind (el ec) (forward-sexp-position syntax l c)
                       (cond ((null el) (return))
                             ((or (< el stop-line) (and (= el stop-line) (< ec stop-column)))
                              (push (element-text syntax l c el ec) items))
                             ;; The cursor is in or at the end of this element.
                             (inner (return))
                             (t (push (element-text syntax l c stop-line stop-column) items)
                                (return-from elements-before
                                  (nreverse (cons +cursor-marker+ items))))))))
                 (nreverse (cons +cursor-marker+
                                 (if inner (cons inner items) (cons "" items))))))))
    (multiple-value-bind (ol oc) (up-list-position syntax line column)
      (when ol
        (let ((form (elements-before ol oc line column nil)))
          (loop repeat (1- levels)
                do (multiple-value-bind (pl pc) (up-list-position syntax ol oc)
                     (unless pl (return))
                     (setf form (remove +cursor-marker+ (elements-before pl pc ol oc form)
                                        :count 1 :from-end t)
                           ol pl oc pc)))
          form)))))

;;; Source locations
;;;
;;; (:location (:file "/path") (:position 123) (:snippet "…"))
;;; (:location (:buffer "name") (:offset 1 20) nil)
;;; (:error "message")
;;; Swank positions count characters from 1.

(defun parse-location (location)
  "A plist describing LOCATION: :file, :buffer, :position (from 0),
:line and :column (from 0), :error."
  (cond ((and (consp location) (eq (first location) :error))
         (list :error (second location)))
        ((and (consp location) (eq (first location) :location))
         (let ((result '()))
           (dolist (part (rest location))
             (when (consp part)
               (case (first part)
                 (:file (setf (getf result :file) (second part)))
                 (:buffer (setf (getf result :buffer) (second part)))
                 (:buffer-and-file (setf (getf result :buffer) (second part)
                                         (getf result :file) (third part)))
                 (:position (setf (getf result :position) (max 0 (1- (second part)))))
                 (:offset (setf (getf result :position) (max 0 (+ (second part) (third part) -1))))
                 (:line (setf (getf result :line) (max 0 (1- (second part)))
                              (getf result :column) (max 0 (or (third part) 0))))
                 (:snippet (setf (getf result :snippet) (second part)))
                 (:function-name (setf (getf result :function-name) (second part))))))
           result))
        (t (list :error (format nil "Unknown location: ~s" location)))))

;;; Compiler notes

(defstruct (compiler-note (:constructor make-compiler-note (severity message location)))
  severity message location)

(defun parse-compiler-notes (notes)
  (loop for note in notes
        collect (make-compiler-note (getf note :severity) (getf note :message)
                                    (parse-location (getf note :location)))))

(defun parse-compilation-result (result)
  "From (:compilation-result notes successp duration loadp faslfile): the
notes (as compiler-note structures), whether it succeeded, and the seconds taken."
  (destructuring-bind (tag notes successp duration &rest more) result
    (declare (ignore tag more))
    (values (parse-compiler-notes notes) successp duration)))

(defun severity-rank (severity)
  (case severity
    ((:error :read-error) 0)
    (:warning 1)
    (:redefinition 3)
    (t 2)))
