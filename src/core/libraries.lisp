;;;; libraries.lisp — a project's libraries: the :depends-on of its .asd,
;;;; and the systems Quicklisp offers
;;;;
;;;; The .asd is edited as text, so its layout and comments stay: a small
;;;; scanner finds each element's start and end (strings, comments, #+
;;;; conditionals and nested lists included). An edit is (start end text):
;;;; replace START to END with TEXT.

(in-package #:cadre)

;;; Scanning s-expressions in text

(defun sx-skip-blank (text i)
  "The position of the first element at or after I: past whitespace and comments."
  (let ((n (length text)))
    (loop
      (cond ((>= i n) (return n))
            ((member (char text i) '(#\Space #\Tab #\Newline #\Return #\Page)) (incf i))
            ((char= (char text i) #\;)
             (setf i (or (position #\Newline text :start i) n)))
            ((and (char= (char text i) #\#) (< (1+ i) n) (char= (char text (1+ i)) #\|))
             (let ((close (search "|#" text :start2 (+ i 2))))
               (setf i (if close (+ close 2) n))))
            (t (return i))))))

(defun sx-atom-end (text i)
  (let ((n (length text)))
    (loop while (and (< i n) (not (member (char text i) '(#\Space #\Tab #\Newline #\Return #\Page #\( #\) #\" #\;))))
          do (incf i (if (char= (char text i) #\\) 2 1)))
    (min i n)))

(defun sx-element-end (text i)
  "The end of the element starting at I."
  (let ((n (length text)))
    (cond ((>= i n) n)
          ((char= (char text i) #\()
           (nth-value 1 (sx-list-elements text i)))
          ((char= (char text i) #\")
           (let ((j (1+ i)))
             (loop while (and (< j n) (char/= (char text j) #\"))
                   do (incf j (if (char= (char text j) #\\) 2 1)))
             (min n (1+ j))))
          ((member (char text i) '(#\' #\` #\,))
           (let ((j (if (and (char= (char text i) #\,) (< (1+ i) n) (char= (char text (1+ i)) #\@)) (+ i 2) (1+ i))))
             (sx-element-end text (sx-skip-blank text j))))
          ((and (char= (char text i) #\#) (< (1+ i) n))
           (case (char text (1+ i))
             ((#\+ #\-)                 ; #+feature element: one element
              (let ((feature-end (sx-element-end text (sx-skip-blank text (+ i 2)))))
                (sx-element-end text (sx-skip-blank text feature-end))))
             (#\' (sx-element-end text (sx-skip-blank text (+ i 2))))
             (#\( (nth-value 1 (sx-list-elements text (1+ i))))
             (#\\ (sx-atom-end text (+ i 3)))
             (t (sx-atom-end text i))))
          (t (sx-atom-end text i)))))

(defun sx-list-elements (text open)
  "The elements of the list whose ( is at OPEN, as (start . end), and the
position after its )."
  (let ((n (length text)) (i (1+ open)) (elements '()))
    (loop
      (setf i (sx-skip-blank text i))
      (cond ((>= i n) (return (values (nreverse elements) n)))
            ((char= (char text i) #\)) (return (values (nreverse elements) (1+ i))))
            (t (let ((end (sx-element-end text i)))
                 (push (cons i end) elements)
                 (setf i (max end (1+ i)))))))))

(defun sx-text (text element)
  (subseq text (car element) (cdr element)))

(defun sx-line-start (text i)
  (let ((nl (position #\Newline text :end i :from-end t)))
    (if nl (1+ nl) 0)))

;;; The .asd's systems and their dependencies

(defun system-designator-name (string)
  "A system name as written (\"foo\", #:foo, :foo, foo) in lower case; nil
for something else, such as (:version \"foo\" \"1.0\"), whose name is found
in it."
  (let ((s (string-trim " " string)))
    (cond ((zerop (length s)) nil)
          ((char= (char s 0) #\") (string-trim "\"" s))
          ((char= (char s 0) #\() nil)
          ((and (> (length s) 1) (string= "#:" s :end2 2)) (string-downcase (subseq s 2)))
          ((char= (char s 0) #\:) (string-downcase (subseq s 1)))
          ((char= (char s 0) #\#) nil)
          (t (string-downcase s)))))

(defun dependency-name (text element)
  "The system an element of :depends-on names: (:version \"x\" …) and
(:feature f \"x\") name x, #+f \"x\" names x."
  (let* ((s (sx-text text element)))
    (cond ((and (> (length s) 1) (char= (char s 0) #\#) (member (char s 1) '(#\+ #\-)))
           (let* ((feature-end (sx-element-end text (sx-skip-blank text (+ (car element) 2))))
                  (start (sx-skip-blank text feature-end)))
             (dependency-name text (cons start (cdr element)))))
          ((char= (char s 0) #\()
           (let ((parts (sx-list-elements text (car element))))
             (cond ((and parts (member (string-downcase (sx-text text (first parts))) '(":version" ":require") :test #'string=)
                         (second parts))
                    (system-designator-name (sx-text text (second parts))))
                   ((and parts (string-equal ":feature" (sx-text text (first parts))) (third parts))
                    (dependency-name text (third parts))))))
          (t (system-designator-name s)))))

(defun asd-systems (text)
  "The systems TEXT (an .asd) defines: (name element), ELEMENT the
(defsystem …) form's range."
  (let ((i 0) (n (length text)) (systems '()))
    (loop
      (setf i (sx-skip-blank text i))
      (when (>= i n) (return (nreverse systems)))
      (let ((end (sx-element-end text i)))
        (when (char= (char text i) #\()
          (let ((parts (sx-list-elements text i)))
            (when (and (second parts)
                       (string-equal "defsystem"
                                     (let ((head (sx-text text (first parts))))
                                       (subseq head (1+ (or (position #\: head :from-end t) -1))))))
              (let ((name (system-designator-name (sx-text text (second parts)))))
                (when name (push (list name (cons i end)) systems))))))
        (setf i (max end (1+ i)))))))

(defun asd-system-form (text system)
  (or (second (find (string-downcase system) (asd-systems text) :key #'first :test #'string=))
      (editor-error "No system ~a in the .asd" system)))

(defun asd-depends-on (text system)
  "SYSTEM's :depends-on in TEXT: the dependencies as (name element), and the
list's range (nil if there is no :depends-on), and the end of what comes
before :depends-on."
  (let* ((form (asd-system-form text system))
         (parts (sx-list-elements text (car form))))
    (loop for (before key value) on (cdr parts)
          when (and value (string-equal ":depends-on" (sx-text text key))
                    (char= #\( (char text (car value))))
            do (return-from asd-depends-on
                 (values (mapcar (lambda (e) (list (dependency-name text e) e))
                                 (sx-list-elements text (car value)))
                         value
                         (cdr before))))
    (values '() nil nil)))

(defun asd-dependency-names (text system)
  (remove nil (mapcar #'first (asd-depends-on text system))))

(defun dependency-text (name style)
  "NAME written as STYLE (an existing dependency's text) writes names."
  (let ((s (or style "\"")))
    (cond ((char= (char s 0) #\") (format nil "~s" name))
          ((and (> (length s) 1) (string= "#:" s :end2 2)) (format nil "#:~a" name))
          ((char= (char s 0) #\:) (format nil ":~a" name))
          ((char= (char s 0) #\() (format nil "~s" name))
          (t name))))

(defun asd-add-dependency (text system name)
  "The edit adding NAME to SYSTEM's :depends-on, as (start end text); nil if
it is there already."
  (let ((name (string-downcase name)))
    (multiple-value-bind (dependencies list) (asd-depends-on text system)
      (cond ((member name dependencies :key #'first :test #'equal) nil)
            (list
             (let* ((elements (mapcar #'second dependencies))
                    (style (find-if #'system-designator-name elements
                                    :key (lambda (e) (sx-text text e))))
                    (new (dependency-text name (and style (sx-text text style)))))
               (if (null elements)
                   (list (1- (cdr list)) (1- (cdr list)) new)
                   (let* ((first (first elements))
                          (last (car (last elements)))
                          (multi-line (find #\Newline text :start (car first) :end (car last))))
                     (list (cdr last) (cdr last)
                           (if multi-line
                               (format nil "~%~a~a" (make-string (- (car first) (sx-line-start text (car first))) :initial-element #\Space) new)
                               (format nil " ~a" new)))))))
            (t
             ;; No :depends-on: one after the name, indented as the next option.
             (let* ((form (asd-system-form text system))
                    (parts (sx-list-elements text (car form)))
                    (name-end (cdr (second parts)))
                    (next (third parts))
                    (new (format nil ":depends-on (~a)" (dependency-text name "\""))))
               (if (and next (find #\Newline text :start name-end :end (car next)))
                   (list name-end name-end
                         (format nil "~%~a~a" (make-string (- (car next) (sx-line-start text (car next))) :initial-element #\Space) new))
                   (list name-end name-end (format nil " ~a" new)))))))))

(defun asd-remove-dependency (text system name)
  "The edit taking NAME out of SYSTEM's :depends-on; nil if it isn't there."
  (multiple-value-bind (dependencies list before) (asd-depends-on text system)
    (let ((entry (find (string-downcase name) dependencies :key #'first :test #'equal)))
      (when (and entry (null (rest dependencies)))
        ;; The only one: the :depends-on goes too.
        (return-from asd-remove-dependency (list before (cdr list) "")))
      (when entry
        (let* ((element (second entry))
               (position (position entry dependencies))
               (previous (and (plusp position) (second (nth (1- position) dependencies)))))
          (if previous
              (list (cdr previous) (cdr element) "")
              (let ((next (second (second dependencies))))
                (list (car element) (if next (car next) (cdr element)) ""))))))))

(defun apply-edit (text edit)
  (destructuring-bind (start end new) edit
    (concatenate 'string (subseq text 0 start) new (subseq text end))))

;;; Quicklisp's systems

(defparameter *quicklisp-systems-form*
  "(let ((ql (find-package \"QL-DIST\")))
  (if ql
      (mapcar (lambda (s)
                (list (funcall (find-symbol \"NAME\" ql) s)
                      (funcall (find-symbol \"PROJECT-NAME\" ql) (funcall (find-symbol \"RELEASE\" ql) s))
                      (if (funcall (find-symbol \"INSTALLEDP\" ql) s) 1 0)))
              (funcall (find-symbol \"PROVIDED-SYSTEMS\" ql) t))
      :no-quicklisp))"
  "Source whose value is every system Quicklisp offers, as (name project
installed), installed 1 or 0; or :NO-QUICKLISP.")

(defstruct (library (:constructor make-library (name project installed)))
  name project installed)

(defun parse-quicklisp-systems (data)
  "LIBRARYs from the value of *QUICKLISP-SYSTEMS-FORM*, or :no-quicklisp."
  (if (listp data)
      (sort (loop for (name project installed) in data
                  when (stringp name)
                    collect (make-library name (if (stringp project) project name) (eql installed 1)))
            #'string< :key #'library-name)
      :no-quicklisp))

(defun search-libraries (libraries query &key (limit 200))
  "The LIBRARIES matching QUERY, best first: the name itself, names starting
with it, names containing it, then projects containing it."
  (let ((q (string-downcase (string-trim " " query))))
    (if (zerop (length q))
        (subseq libraries 0 (min limit (length libraries)))
        (let ((ranked '()))
          (dolist (library libraries)
            (let* ((name (string-downcase (library-name library)))
                   (rank (cond ((string= name q) 0)
                               ((eql 0 (search q name)) 1)
                               ((search q name) 2)
                               ((search q (string-downcase (library-project library))) 3))))
              (when rank (push (cons rank library) ranked))))
          (let ((sorted (stable-sort (nreverse ranked) #'< :key #'car)))
            (mapcar #'cdr (subseq sorted 0 (min limit (length sorted)))))))))
