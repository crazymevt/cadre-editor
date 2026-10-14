;;;; lexer.lisp — Lisp source, one line at a time
;;;;
;;;; LEX-LINE turns one line into tokens, given the state at the start of
;;;; the line, and returns the state at its end. It reads Common Lisp unless
;;;; given another dialect (dialect.lisp). A state is a list:
;;;;   (:code depth)            ordinary code, DEPTH brackets open
;;;;   (:string depth)          inside a string
;;;;   (:here depth end)        inside a Racket here string, ended by the line END
;;;;   (:block depth nesting)   inside #| |#, NESTING deep
;;;;   (:bar depth)             inside a |symbol|
;;;; States are compared with EQUAL, which is what lets the line cache
;;;; (syntax.lisp) stop re-lexing as soon as a line ends as it did before.

(in-package #:cadre)

(defparameter +initial-state+ '(:code 0))

(defstruct (token (:constructor make-token (type start end depth &optional subtype)))
  "A token on one line. TYPE is one of :open :close :symbol :keyword :number
:character :string :comment :quote :reader-conditional :reader :invalid,
and in Racket and Clojure also :constant (#t, #f) and :datum-comment (#;
and #_, which comment out the element after them).
START and END are columns. DEPTH is the parenthesis depth: for :open the
depth before it, for :close the depth after it (so a pair has equal
depths), otherwise the depth around the token. SUBTYPE refines symbols:
:head (first in a list), :name (the name after a def… head) or :feature
(the feature expression after #+ or #-). For :open and :close it is the
bracket character: ( [ or {."
  type start end depth subtype)

(defun state-mode (state) (first state))
(defun state-depth (state) (second state))

(defun whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Return #\Page #\Newline #\No-break_space)))

(defun terminating-char-p (c)
  (or (whitespace-char-p c) (member c '(#\( #\) #\" #\' #\` #\, #\;))))

(defun dialect-whitespace-p (c dialect)
  (or (whitespace-char-p c) (and (char= c #\,) (dialect-comma-whitespace dialect))))

(defun dialect-terminating-p (c dialect)
  (or (dialect-whitespace-p c dialect) (member c (dialect-terminating dialect))))

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

(defun scan-atom (line start &optional (dialect *common-lisp-dialect*))
  "The end of the symbol or number starting at START, and whether it ended
inside an unclosed |…|."
  (loop with n = (length line)
        with bars = (dialect-bar-symbols dialect)
        with i = start
        while (< i n)
        do (let ((c (char line i)))
             (cond ((and bars (char= c #\\)) (incf i 2))
                   ((and bars (char= c #\|))
                    (multiple-value-bind (end closed) (scan-bar line (1+ i))
                      (unless closed (return-from scan-atom (values n t)))
                      (setf i end)))
                   ((dialect-terminating-p c dialect) (return-from scan-atom (values i nil)))
                   (t (incf i))))
        finally (return (values (min i n) nil))))

(defun scan-word (line start)
  "The end of the run of letters starting at START."
  (or (position-if-not #'alpha-char-p line :start start) (length line)))

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

(defun clojure-number-p (s)
  "True if S reads as a Clojure number: 1, -2.5e3, 1/2, 42N, 1.5M, 0x1F, 2r1010."
  (let* ((n (length s))
         (body (if (and (> n 1) (member (char s (1- n)) '(#\N #\M))) (subseq s 0 (1- n)) s))
         (unsigned (if (and (plusp (length body)) (member (char body 0) '(#\+ #\-))) (subseq body 1) body))
         (r (position #\r unsigned :test #'char-equal)))
    (flet ((digits-p (string radix)
             (and (plusp (length string)) (every (lambda (c) (digit-char-p c radix)) string))))
      (or (and (number-string-p body)
               (notany (lambda (c) (member (char-downcase c) '(#\d #\f #\l #\s))) body))
          (and (> (length unsigned) 2) (string-equal "0x" unsigned :end2 2)
               (digits-p (subseq unsigned 2) 16))
          (and r (digits-p (subseq unsigned 0 r) 10)
               (let ((radix (parse-integer unsigned :end r)))
                 (and (<= 2 radix 36) (digits-p (subseq unsigned (1+ r)) radix))))))))

(defun dialect-number-p (text dialect)
  (case (dialect-name dialect)
    (:clojure (clojure-number-p text))
    (:racket (or (number-string-p text)
                 (member text '("+inf.0" "-inf.0" "+nan.0" "-nan.0" "+inf.f" "-inf.f" "+nan.f" "-nan.f")
                         :test #'string-equal)))
    (t (number-string-p text))))

(defun definer-name-p (name)
  "True for a Common Lisp head symbol that defines something named by its first argument."
  (lisp-definer-p name))

(defun lex-line (line state &optional (dialect *common-lisp-dialect*))
  "Tokens for LINE (a string without its newline), starting in STATE, read
by DIALECT's rules. Returns the tokens (a list) and the state at the end of
the line."
  (let ((tokens '())
        (n (length line))
        (i 0)
        (kind (dialect-name dialect))
        (mode (state-mode state))
        (depth (state-depth state))
        (nesting 0)                     ; #| |# nesting, in :block mode
        (terminator nil)                ; a here string's last line, in :here mode
        (after-open nil)                ; the next atom is a list's head
        (name-next nil)                 ; the next symbol is a definition's name
        (name-head nil)                 ; the next list head is a definition's name
        (feature-next nil))             ; the next element is a #+/#- feature
    (labels ((emit (type start end &optional subtype)
               (push (make-token type start end depth subtype) tokens))
             (element ()
               ;; The next element of a list has started: return the
               ;; subtype for a symbol here, and clear the flags.
               (prog1 (cond (feature-next :feature) (after-open (if name-head :name :head)) (name-next :name))
                 (setf after-open nil name-next nil feature-next nil name-head nil)))
             (char-at (j) (and (< j n) (char line j)))
             (open-list (start end bracket head)
               ;; An open bracket from START to END (a prefix such as #( or
               ;; #hash( included). HEAD: its first element is an operator.
               (let ((named (and name-next head (dialect-named-head dialect))))
                 (element)
                 (emit :open start end bracket) (incf depth)
                 (setf i end after-open head name-head named)))
             (close-list (bracket)
               (if (zerop depth)
                   (emit :invalid i (1+ i))     ; a close bracket with nothing open
                   (progn (decf depth) (emit :close i (1+ i) bracket)))
               (incf i)
               (setf after-open nil name-next nil name-head nil))
             (string-from (start quote)
               ;; A string whose token starts at START (a prefix such as
               ;; #rx included) and whose opening quote is at QUOTE.
               (multiple-value-bind (end closed) (scan-string line (1+ quote))
                 (element)
                 (emit :string start end)
                 (setf i end)
                 (unless closed (setf mode :string))))
             (character-at (start body)
               ;; A character whose token starts at START, written from BODY on.
               (let* ((first (char-at body))
                      (end (cond ((null first) n)
                                 ((dialect-terminating-p first dialect) (1+ body))
                                 (t (max (1+ body) (scan-atom line body dialect))))))
                 (element)
                 (emit :character start (min end n))
                 (setf i (min end n))))
             (prefix (type end)
               (emit type i end) (setf i end))
             (whole (type start end)
               ;; An element of TYPE from START to END.
               (element) (emit type start end) (setf i end))
             (atom-at (start)
               (multiple-value-bind (end open) (scan-atom line start dialect)
                 (let* ((end (max end (1+ start)))
                        (subtype (element))
                        (text (subseq line start end)))
                   (cond (open (emit :symbol start end subtype) (setf mode :bar))
                         ((dialect-number-p text dialect) (emit :number start end))
                         ((and (char= (char line start) #\:) (dialect-colon-keywords dialect))
                          (emit :keyword start end))
                         (t (emit :symbol start end subtype)
                            (when (and (eq subtype :head) (funcall (dialect-definer dialect) text))
                              (setf name-next t))))
                   (setf i end))))
             (block-comment ()
               (multiple-value-bind (end left) (scan-block-comment line (+ i 2) 1)
                 (emit :comment i end)
                 (setf i end)
                 (when (plusp left) (setf mode :block nesting left))))
             (graph-label ()
               ;; #n= and #n#, or #nR… (a number in base n) in Common Lisp.
               (let ((j (1+ i)))
                 (loop while (and (char-at j) (digit-char-p (char-at j))) do (incf j))
                 (let ((c (char-at j)))
                   (if (and c (char-equal c #\r) (eq kind :common-lisp))
                       (whole :number i (scan-atom line (1+ j) dialect))
                       (prefix :reader (min n (1+ j)))))))
             (dispatch ()
               (let ((next (char-at (1+ i))))
                 (cond ((null next) (emit :invalid i n) (setf i n))
                       ((digit-char-p next) (graph-label))
                       (t (ecase kind
                            (:common-lisp (lisp-dispatch next))
                            (:racket (racket-dispatch next))
                            (:clojure (clojure-dispatch next)))))))
             (lisp-dispatch (next)
               (cond
                 ((char= next #\|) (block-comment))
                 ((char= next #\\) (character-at i (+ i 2)))
                 ((member next '(#\' #\.)) (prefix :quote (+ i 2)))
                 ((char= next #\() (open-list i (+ i 2) #\( nil))
                 ((member next '(#\+ #\-))
                  (prefix :reader-conditional (+ i 2))
                  (setf feature-next t))
                 ((char= next #\:) (whole :symbol i (scan-atom line (+ i 2) dialect)))
                 ((member (char-downcase next) '(#\x #\b #\o #\*))
                  (whole :number i (scan-atom line (+ i 2) dialect)))
                 (t (prefix :reader (+ i 2)))))
             (racket-dispatch (next)
               (cond
                 ((char= next #\|) (block-comment))
                 ((char= next #\;) (prefix :datum-comment (+ i 2)))
                 ((char= next #\\) (character-at i (+ i 2)))
                 ((member next '(#\' #\` #\&)) (prefix :quote (+ i 2)))
                 ((char= next #\,) (prefix :quote (if (eql (char-at (+ i 2)) #\@) (+ i 3) (+ i 2))))
                 ((member next (dialect-opens dialect)) (open-list i (+ i 2) next nil))
                 ((char= next #\") (string-from i (1+ i)))                 ; a byte string
                 ((char= next #\:) (whole :keyword i (scan-atom line (+ i 2) dialect)))
                 ((and (char= next #\<) (eql (char-at (+ i 2)) #\<))      ; a here string
                  (emit :string i n)
                  (setf terminator (subseq line (+ i 3)) mode :here i n))
                 ((char= next #\!)
                  (if (member (char-at (+ i 2)) '(#\/ #\Space nil))
                      (prefix :comment n)                                ; #! /usr/bin/env racket
                      (prefix :reader (scan-atom line (+ i 2) dialect))))  ; #!racket, like #lang
                 ((alpha-char-p next)
                  (let* ((word-end (scan-word line (1+ i)))
                         (word (string-downcase (subseq line (1+ i) word-end)))
                         (after (char-at word-end)))
                    (cond
                      ((and (member word '("rx" "px") :test #'string=)
                            (or (eql after #\") (and (eql after #\#) (eql (char-at (1+ word-end)) #\"))))
                       (string-from i (position #\" line :start word-end)))
                      ((and after (member after (dialect-opens dialect)))    ; #hash( #s( #fl(
                       (open-list i (1+ word-end) after nil))
                      ((string= word "lang") (prefix :reader word-end))
                      ((and (member word '("t" "f" "true" "false") :test #'string=)
                            (or (null after) (dialect-terminating-p after dialect)))
                       (whole :constant i word-end))
                      ((member (char word 0) '(#\x #\b #\o #\d #\e #\i))
                       (whole :number i (scan-atom line (1+ i) dialect)))
                      (t (prefix :reader word-end)))))
                 (t (prefix :reader (+ i 2)))))
             (clojure-dispatch (next)
               (cond
                 ((char= next #\_) (prefix :datum-comment (+ i 2)))
                 ((member next '(#\' #\^)) (prefix :quote (+ i 2)))
                 ((char= next #\{) (open-list i (+ i 2) #\{ nil))      ; a set
                 ((char= next #\() (open-list i (+ i 2) #\( t))        ; an anonymous function
                 ((char= next #\") (string-from i (1+ i)))              ; a regex
                 ((char= next #\?)                                      ; #?( and #?@(
                  (let ((k (if (eql (char-at (+ i 2)) #\@) (+ i 3) (+ i 2))))
                    (if (eql (char-at k) #\()
                        (open-list i (1+ k) #\( nil)
                        (prefix :reader k))))
                 ((char= next #\:)                                      ; #:ns{…} and #::{…}
                  (let ((end (scan-atom line (1+ i) dialect)))
                    (if (eql (char-at end) #\{)
                        (open-list i (1+ end) #\{ nil)
                        (prefix :reader (max end (+ i 2))))))
                 ((char= next #\#) (whole :number i (scan-atom line (+ i 2) dialect)))  ; ##Inf
                 ((char= next #\!) (prefix :comment n))
                 ((alpha-char-p next) (prefix :reader (scan-atom line (1+ i) dialect)))  ; #inst, #my/tag
                 (t (prefix :reader (+ i 2))))))
      ;; Continue whatever the previous line left open.
      (ecase mode
        (:code)
        (:string (multiple-value-bind (end closed) (scan-string line 0)
                   (emit :string 0 end)
                   (setf i end)
                   (when closed (setf mode :code))))
        (:here (emit :string 0 n)
               (setf i n terminator (third state))
               (when (string= line terminator) (setf mode :code)))
        (:block (multiple-value-bind (end left) (scan-block-comment line 0 (third state))
                  (emit :comment 0 end)
                  (setf i end nesting left)
                  (when (zerop left) (setf mode :code))))
        (:bar (multiple-value-bind (end closed) (scan-bar line 0)
                (if closed
                    (multiple-value-bind (end2 open) (scan-atom line end dialect)
                      (emit :symbol 0 end2)
                      (setf i end2)
                      (unless open (setf mode :code)))
                    (progn (emit :symbol 0 end) (setf i end))))))
      (loop while (and (< i n) (eq mode :code))
            do (let ((c (char line i)))
                 (cond
                   ((dialect-whitespace-p c dialect) (incf i))
                   ((char= c #\;) (emit :comment i n) (setf i n))
                   ((member c (dialect-opens dialect)) (open-list i (1+ i) c (char= c #\()))
                   ((find c (dialect-opens dialect) :key #'closing-bracket) (close-list c))
                   ((char= c #\") (string-from i i))
                   ((member c '(#\' #\`)) (prefix :quote (1+ i)))
                   ((and (eq kind :clojure) (char= c #\~))
                    (prefix :quote (if (eql (char-at (1+ i)) #\@) (+ i 2) (1+ i))))
                   ((and (eq kind :clojure) (member c '(#\@ #\^))) (prefix :quote (1+ i)))
                   ((and (eq kind :clojure) (char= c #\\)) (character-at i (1+ i)))
                   ((char= c #\,)
                    (prefix :quote (if (member (char-at (1+ i)) '(#\@ #\.)) (+ i 2) (1+ i))))
                   ((char= c #\#) (dispatch))
                   (t (atom-at i)))))
      (values (nreverse tokens)
              (ecase mode
                (:code (list :code depth))
                (:string (list :string depth))
                (:here (list :here depth terminator))
                (:block (list :block depth nesting))
                (:bar (list :bar depth)))))))
