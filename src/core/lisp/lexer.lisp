;;;; lexer.lisp — Common Lisp source, one line at a time
;;;;
;;;; LEX-LINE turns one line into tokens, given the state at the start of
;;;; the line, and returns the state at its end. A state is a list:
;;;;   (:code depth)            ordinary code, DEPTH parentheses open
;;;;   (:string depth)          inside a string
;;;;   (:block depth nesting)   inside #| |#, NESTING deep
;;;;   (:bar depth)             inside a |symbol|
;;;; States are compared with EQUAL, which is what lets the line cache
;;;; (syntax.lisp) stop re-lexing as soon as a line ends as it did before.

(in-package #:cadre)

(defparameter +initial-state+ '(:code 0))

(defstruct (token (:constructor make-token (type start end depth &optional subtype)))
  "A token on one line. TYPE is one of :open :close :symbol :keyword :number
:character :string :comment :quote :reader-conditional :reader :invalid.
START and END are columns. DEPTH is the parenthesis depth: for :open the
depth before it, for :close the depth after it (so a pair has equal
depths), otherwise the depth around the token. SUBTYPE refines symbols:
:head (first in a list), :name (the name after a def… head) or :feature
(the feature expression after #+ or #-)."
  type start end depth subtype)

(defun state-mode (state) (first state))
(defun state-depth (state) (second state))

(defun whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Return #\Page #\Newline #\No-break_space)))

(defun terminating-char-p (c)
  (or (whitespace-char-p c) (member c '(#\( #\) #\" #\' #\` #\, #\;))))

(defun scan-string (line start)
  "The column after the string body starting at START (just past the
opening quote), and whether the closing quote was found."
  (loop with n = (length line)
        for i from start below n
        do (case (char line i)
             (#\\ (incf i))
             (#\" (return-from scan-string (values (1+ i) t))))
        finally (return (values n nil))))

(defun scan-block-comment (line start nesting)
  "Scan #| |# from START with NESTING open; returns the column after the
comment (or the line's end) and the nesting left (0 when closed)."
  (loop with n = (length line)
        with i = start
        while (< i n)
        do (cond ((and (char= (char line i) #\|) (< (1+ i) n) (char= (char line (1+ i)) #\#))
                  (decf nesting) (incf i 2)
                  (when (zerop nesting) (return-from scan-block-comment (values i 0))))
                 ((and (char= (char line i) #\#) (< (1+ i) n) (char= (char line (1+ i)) #\|))
                  (incf nesting) (incf i 2))
                 (t (incf i)))
        finally (return (values n nesting))))

(defun scan-bar (line start)
  "Scan a |…| part from START (just past the opening bar). Returns the
column after the closing bar, and whether it was found."
  (loop with n = (length line)
        for i from start below n
        do (case (char line i)
             (#\\ (incf i))
             (#\| (return-from scan-bar (values (1+ i) t))))
        finally (return (values n nil))))

(defun scan-atom (line start)
  "The end of the symbol or number starting at START, and whether it ended
inside an unclosed |…|."
  (loop with n = (length line)
        with i = start
        while (< i n)
        do (let ((c (char line i)))
             (cond ((char= c #\\) (incf i 2))
                   ((char= c #\|)
                    (multiple-value-bind (end closed) (scan-bar line (1+ i))
                      (unless closed (return-from scan-atom (values n t)))
                      (setf i end)))
                   ((terminating-char-p c) (return-from scan-atom (values i nil)))
                   (t (incf i))))
        finally (return (values (min i n) nil))))

(defun number-string-p (s)
  "True if S reads as a number in base 10: an integer, ratio or float."
  (let ((n (length s)) (i 0))
    (labels ((digits ()
               (let ((start i))
                 (loop while (and (< i n) (digit-char-p (char s i))) do (incf i))
                 (> i start)))
             (sign () (when (and (< i n) (member (char s i) '(#\+ #\-))) (incf i)))
             (end-p () (= i n))
             (exponent ()
               (when (and (< i n) (member (char-downcase (char s i)) '(#\e #\d #\f #\l #\s)))
                 (incf i) (sign) (digits))))
      (sign)
      (let ((int (digits)))
        (cond ((end-p) int)
              ((char= (char s i) #\/) (incf i) (and int (digits) (end-p)))
              ((char= (char s i) #\.)
               (incf i)
               (let ((frac (digits)))
                 (cond ((end-p) (or int frac))
                       (t (and (or int frac) (exponent) (end-p))))))
              (t (and int (exponent) (end-p))))))))

(defun definer-name-p (name)
  "True for a head symbol that defines something named by its first argument."
  (let ((name (string-downcase (subseq name (1+ (or (position #\: name :from-end t) -1))))))
    (and (> (length name) 3) (string= "def" name :end2 3))))

(defun lex-line (line state)
  "Tokens for LINE (a string without its newline), starting in STATE.
Returns the tokens (a list) and the state at the end of the line."
  (let ((tokens '())
        (n (length line))
        (i 0)
        (mode (state-mode state))
        (depth (state-depth state))
        (nesting 0)                     ; #| |# nesting, in :block mode
        (after-open nil)                ; the next atom is a list's head
        (name-next nil)                 ; the next symbol is a definition's name
        (feature-next nil))             ; the next element is a #+/#- feature
    (labels ((emit (type start end &optional subtype)
               (push (make-token type start end depth subtype) tokens))
             (element ()
               ;; The next element of a list has started: return the
               ;; subtype for a symbol here, and clear the flags.
               (prog1 (cond (feature-next :feature) (after-open :head) (name-next :name))
                 (setf after-open nil name-next nil feature-next nil)))
             (char-at (j) (and (< j n) (char line j)))
             (dispatch ()
               ;; At #: reader macros.
               (let ((next (char-at (1+ i))))
                 (cond
                   ((null next) (emit :invalid i n) (setf i n))
                   ((char= next #\|)
                    (multiple-value-bind (end left) (scan-block-comment line (+ i 2) 1)
                      (emit :comment i end)
                      (setf i end)
                      (when (plusp left) (setf mode :block nesting left))))
                   ((char= next #\\)
                    (let* ((first (char-at (+ i 2)))
                           (end (cond ((null first) n)
                                      ((terminating-char-p first)
                                       (+ i 3))
                                      (t (max (+ i 3) (scan-atom line (+ i 2)))))))
                      (element)
                      (emit :character i (min end n))
                      (setf i (min end n))))
                   ((char= next #\') (emit :quote i (+ i 2)) (incf i 2))
                   ((char= next #\.) (emit :quote i (+ i 2)) (incf i 2))
                   ((char= next #\()
                    (element)
                    (emit :open i (+ i 2)) (incf depth) (incf i 2))
                   ((member next '(#\+ #\-))
                    (emit :reader-conditional i (+ i 2)) (incf i 2)
                    (setf feature-next t))
                   ((char= next #\:)
                    (let ((end (scan-atom line (+ i 2))))
                      (element)
                      (emit :symbol i end)
                      (setf i end)))
                   ((member (char-downcase next) '(#\x #\b #\o #\*))
                    (let ((end (scan-atom line (+ i 2))))
                      (element)
                      (emit :number i end)
                      (setf i end)))
                   ((digit-char-p next)
                    (let ((j (1+ i)))
                      (loop while (and (char-at j) (digit-char-p (char-at j))) do (incf j))
                      (let ((c (char-at j)))
                        (cond ((and c (char-equal c #\r))
                               (let ((end (scan-atom line (1+ j))))
                                 (element)
                                 (emit :number i end)
                                 (setf i end)))
                              (t (emit :reader i (min n (1+ j)))
                                 (setf i (min n (1+ j))))))))
                   (t (emit :reader i (+ i 2)) (incf i 2))))))
      ;; Continue whatever the previous line left open.
      (ecase mode
        (:code)
        (:string (multiple-value-bind (end closed) (scan-string line 0)
                   (emit :string 0 end)
                   (setf i end)
                   (when closed (setf mode :code))))
        (:block (multiple-value-bind (end left) (scan-block-comment line 0 (third state))
                  (emit :comment 0 end)
                  (setf i end nesting left)
                  (when (zerop left) (setf mode :code))))
        (:bar (multiple-value-bind (end closed) (scan-bar line 0)
                (if closed
                    (multiple-value-bind (end2 open) (scan-atom line end)
                      (emit :symbol 0 end2)
                      (setf i end2)
                      (unless open (setf mode :code)))
                    (progn (emit :symbol 0 end) (setf i end))))))
      (loop while (and (< i n) (eq mode :code))
            do (let ((c (char line i)))
                 (cond
                   ((whitespace-char-p c) (incf i))
                   ((char= c #\;) (emit :comment i n) (setf i n))
                   ((char= c #\()
                    (element)
                    (emit :open i (1+ i)) (incf depth) (incf i)
                    (setf after-open t))
                   ((char= c #\))
                    (if (zerop depth)
                        (emit :invalid i (1+ i))  ; a close paren with nothing open
                        (progn (decf depth) (emit :close i (1+ i))))
                    (incf i)
                    (setf after-open nil name-next nil))
                   ((char= c #\")
                    (multiple-value-bind (end closed) (scan-string line (1+ i))
                      (element)
                      (emit :string i end)
                      (setf i end)
                      (unless closed (setf mode :string))))
                   ((member c '(#\' #\`)) (emit :quote i (1+ i)) (incf i))
                   ((char= c #\,)
                    (let ((end (if (member (char-at (1+ i)) '(#\@ #\.)) (+ i 2) (1+ i))))
                      (emit :quote i end) (setf i end)))
                   ((char= c #\#) (dispatch))
                   (t
                    (multiple-value-bind (end open) (scan-atom line i)
                      (let ((end (max end (1+ i)))
                            (subtype (element)))
                        (let ((text (subseq line i end)))
                          (cond (open (emit :symbol i end subtype) (setf mode :bar))
                                ((number-string-p text) (emit :number i end))
                                ((char= c #\:) (emit :keyword i end))
                                (t (emit :symbol i end subtype)
                                   (when (and (eq subtype :head) (definer-name-p text))
                                     (setf name-next t)))))
                        (setf i end)))))))
      (values (nreverse tokens)
              (ecase mode
                (:code (list :code depth))
                (:string (list :string depth))
                (:block (list :block depth nesting))
                (:bar (list :bar depth)))))))
