;;;; text.lisp — the text protocol buffers are built on
;;;;
;;;; A buffer's text is any object implementing these generic functions.
;;;; Positions are character offsets from 0. The GUI implements them for
;;;; gtk:text-buffer; STRING-TEXT below implements them in plain Lisp, for
;;;; tests and for buffers that are never shown.

(in-package #:cadre)

(defgeneric text-length (text)
  (:documentation "The number of characters in TEXT."))

(defgeneric text-string (text &optional start end)
  (:documentation "The characters of TEXT from START (default 0) to END (default the end), as a string."))

(defgeneric text-char (text position)
  (:documentation "The character at POSITION in TEXT."))

(defgeneric text-insert (text position string)
  (:documentation "Insert STRING into TEXT at POSITION."))

(defgeneric text-delete (text start end)
  (:documentation "Delete the characters of TEXT from START to END."))

(defgeneric text-point (text)
  (:documentation "The cursor position in TEXT. Settable with setf."))

(defgeneric (setf text-point) (position text))

(defgeneric text-modified-p (text)
  (:documentation "True if TEXT has changed since it was loaded or saved. Settable with setf."))

(defgeneric (setf text-modified-p) (value text))

(defgeneric text-replace-contents (text string)
  (:documentation "Replace all of TEXT with STRING, as when loading a file: the
change cannot be undone, the cursor moves to the start, and TEXT is marked unmodified."))

(defgeneric text-line-count (text)
  (:documentation "The number of lines in TEXT. Text ending in a newline has an empty last line."))

(defgeneric text-line-string (text line)
  (:documentation "The characters of LINE (from 0) in TEXT, without its newline."))

(defgeneric text-line-position (text line &optional column)
  (:documentation "The position of COLUMN (default 0) on LINE in TEXT."))

(defgeneric text-position-line (text position)
  (:documentation "The line and column of POSITION in TEXT, as two values."))

(defmethod text-char (text position)
  (char (text-string text position (1+ position)) 0))

;;; Line functions in terms of TEXT-STRING, for implementations without
;;; their own. They scan the whole text, so they suit small texts and tests.

(defun line-starts (string)
  (cons 0 (loop for i from 0 below (length string)
                when (char= (char string i) #\Newline) collect (1+ i))))

(defmethod text-line-count (text)
  (length (line-starts (text-string text))))

(defmethod text-line-string (text line)
  (let* ((string (text-string text))
         (start (nth line (line-starts string))))
    (unless start (error "Line ~d is past the end of the text." line))
    (subseq string start (or (position #\Newline string :start start) (length string)))))

(defmethod text-line-position (text line &optional (column 0))
  (let ((start (nth line (line-starts (text-string text)))))
    (unless start (error "Line ~d is past the end of the text." line))
    (+ start column)))

(defmethod text-position-line (text position)
  (let ((starts (line-starts (text-string text))))
    (loop for (start next) on starts
          for line from 0
          when (or (null next) (< position next))
            return (values line (- position start)))))

;;; A plain Lisp implementation

(defclass string-text ()
  ((chars :initform (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)
          :reader string-text-chars)
   (point :initform 0)
   (modified :initform nil)))

(defun make-string-text (&optional (initial-contents ""))
  (let ((text (make-instance 'string-text)))
    (text-replace-contents text initial-contents)
    text))

(defun check-range (text start end)
  (unless (<= 0 start end (text-length text))
    (error "Range ~d–~d is outside the text (length ~d)." start end (text-length text))))

(defmethod text-length ((text string-text))
  (length (string-text-chars text)))

(defmethod text-string ((text string-text) &optional (start 0) end)
  (let ((end (or end (text-length text))))
    (check-range text start end)
    (subseq (string-text-chars text) start end)))

(defmethod text-char ((text string-text) position)
  (check-range text position (1+ position))
  (char (string-text-chars text) position))

(defmethod text-insert ((text string-text) position string)
  (check-range text position position)
  (with-slots (chars point modified) text
    (let ((old-length (length chars))
          (n (length string)))
      (dotimes (i n) (vector-push-extend #\Nul chars))
      (replace chars chars :start1 (+ position n) :start2 position :end2 old-length)
      (replace chars string :start1 position)
      (when (>= point position) (incf point n))
      (setf modified t))))

(defmethod text-delete ((text string-text) start end)
  (check-range text start end)
  (with-slots (chars point modified) text
    (let ((n (- end start)))
      (replace chars chars :start1 start :start2 end)
      (decf (fill-pointer chars) n)
      (cond ((>= point end) (decf point n))
            ((> point start) (setf point start)))
      (setf modified t))))

(defmethod text-point ((text string-text))
  (slot-value text 'point))

(defmethod (setf text-point) (position (text string-text))
  (check-range text position position)
  (setf (slot-value text 'point) position))

(defmethod text-modified-p ((text string-text))
  (slot-value text 'modified))

(defmethod (setf text-modified-p) (value (text string-text))
  (setf (slot-value text 'modified) (and value t)))

(defmethod text-replace-contents ((text string-text) string)
  (with-slots (chars point modified) text
    (setf (fill-pointer chars) 0)
    (loop for c across string do (vector-push-extend c chars))
    (setf point 0 modified nil)))
