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
   (modified :initform nil)
   ;; Where each line starts, made when asked for and dropped on a change.
   (line-starts :initform nil)))

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
      (setf modified t (slot-value text 'line-starts) nil))))

(defmethod text-delete ((text string-text) start end)
  (check-range text start end)
  (with-slots (chars point modified) text
    (let ((n (- end start)))
      (replace chars chars :start1 start :start2 end)
      (decf (fill-pointer chars) n)
      (cond ((>= point end) (decf point n))
            ((> point start) (setf point start)))
      (setf modified t (slot-value text 'line-starts) nil))))

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
    (setf point 0 modified nil (slot-value text 'line-starts) nil)))

;;; Its lines, from a vector of where they start, so reading a text line by
;;; line takes time in proportion to its length.

(defun string-text-line-starts (text)
  (or (slot-value text 'line-starts)
      (setf (slot-value text 'line-starts)
            (coerce (line-starts (string-text-chars text)) 'simple-vector))))

(defun string-text-line-start (text line)
  (let ((starts (string-text-line-starts text)))
    (unless (< -1 line (length starts)) (error "Line ~d is past the end of the text." line))
    (svref starts line)))

(defmethod text-line-count ((text string-text))
  (length (string-text-line-starts text)))

(defmethod text-line-string ((text string-text) line)
  (let* ((chars (string-text-chars text))
         (start (string-text-line-start text line)))
    (subseq chars start (or (position #\Newline chars :start start) (length chars)))))

(defmethod text-line-position ((text string-text) line &optional (column 0))
  (+ (string-text-line-start text line) column))

(defmethod text-position-line ((text string-text) position)
  (let ((starts (string-text-line-starts text))
        (low 0))
    ;; The last line starting at or before POSITION.
    (let ((high (1- (length starts))))
      (loop while (< low high)
            do (let ((middle (ceiling (+ low high) 2)))
                 (if (<= (svref starts middle) position)
                     (setf low middle)
                     (setf high (1- middle))))))
    (values low (- position (svref starts low)))))

(defun indentation-context-line (text line &key (limit 1000))
  "For brace languages: the nearest line before LINE, within LIMIT lines,
that starts at the left edge and opens the block LINE is in, or nil when
LINE itself starts at the left edge. Lines that only close a block (}, ],
), </…>) don't count."
  (flet ((indented-or-blank-p (string)
           (or (zerop (length string)) (member (char string 0) '(#\Space #\Tab))))
         (opener-p (string)
           (and (plusp (length (string-trim '(#\Space #\Tab) string)))
                (not (member (char string 0) '(#\Space #\Tab #\} #\] #\))))
                (not (uiop:string-prefix-p "</" string)))))
    (when (and (< 0 line (text-line-count text))
               (indented-or-blank-p (text-line-string text line)))
      (loop for l from (1- line) downto (max 0 (- line limit))
            for string = (text-line-string text l)
            unless (indented-or-blank-p string)
              do (return (and (opener-p string) l))))))
