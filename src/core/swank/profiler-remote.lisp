;;;; profiler-remote.lisp — the profiler's helper, for the other Lisp
;;;;
;;;; Not compiled into Cadre: profiler.lisp sends this text to the Lisp Cadre
;;;; is connected to, which evaluates it (SBCL only, through sb-sprof). REPORT
;;;; reads sb-sprof's samples (each a stack of frames, outermost first) and
;;;; answers with plain data: names as text, counts as numbers.

(require :sb-sprof)

(defpackage #:cadre-profiler
  (:use #:cl)
  (:export #:run #:start #:stop #:profiling-p #:report))

(in-package #:cadre-profiler)

(defvar *trim* nil
  "True when the samples are of RUN's thunk: only stacks with CALL-THUNK
count, and only the frames below it.")

(declaim (notinline call-thunk))
(defun call-thunk (thunk)
  "Call THUNK; its frame marks where the profiled form's frames start."
  (multiple-value-prog1 (funcall thunk)))

(defun run (thunk &key (mode :cpu) (interval 0.001))
  "Call THUNK with the profiler on; its values."
  (sb-sprof:reset)
  (setf *trim* t)
  (sb-sprof:with-profiling (:mode mode :sample-interval interval :max-samples 200000 :threads :all :report nil)
    (call-thunk thunk)))

(defun start (&key (mode :cpu) (interval 0.005))
  "Profile every thread until STOP."
  (sb-sprof:stop-profiling)
  (sb-sprof:reset)
  (setf *trim* nil)
  (sb-sprof:start-profiling :mode mode :sample-interval interval :max-samples 200000 :threads :all)
  t)

(defun stop ()
  (sb-sprof:stop-profiling)
  t)

(defun internal (name package)
  "The symbol NAME in PACKAGE, found when called: internal names, read in
this file, would be interned in a locked package where they don't exist."
  (find-symbol name package))

(defun profiling-p ()
  (let ((flag (internal "*PROFILING*" "SB-SPROF")))
    (and flag (boundp flag) (symbol-value flag) t)))

(defun head-is (name &rest heads)
  (and (consp name) (symbolp (first name)) (member (symbol-name (first name)) heads :test #'string=)))

(defun clean-name (name)
  "NAME, a debug name, without the compiler's wrappings."
  (cond ((head-is name "XEP" "TL-XEP" "&MORE-PROCESSOR" "&OPTIONAL-PROCESSOR" "VARARGS-ENTRY")
         (clean-name (second name)))
        ((head-is name "FAST-METHOD") (cons 'method (rest name)))
        ((member (and (consp name) (first name)) '(flet labels lambda))
         (let ((in (member :in name)))
           (if (and in (stringp (second in)))
               (ldiff name in)
               name)))
        (t name)))

(defun frame-name (info)
  (cond ((null info) "unknown")
        ((stringp info) info)
        ((typep info (internal "DEBUG-FUN" "SB-DI")) (clean-name (sb-di:debug-fun-name info)))
        (t (princ-to-string (type-of info)))))

(defun definition-name (name)
  "The name of the definition NAME's code is in: FOO for FOO, (SETF FOO),
(METHOD FOO …) and (LAMBDA … :IN FOO); nil if there is none."
  (cond ((and name (symbolp name)) name)
        ((not (consp name)) nil)
        ((eq (first name) 'setf) name)
        ((eq (first name) 'method) (second name))
        ((member (first name) '(flet labels lambda))
         (let ((in (member :in name)))
           (and in (definition-name (clean-name (second in))))))
        (t nil)))

(defun name-text (name)
  (if (stringp name)
      name
      (let ((*package* (find-package :common-lisp-user)) (*print-pretty* nil) (*print-readably* nil)
            (*print-length* 8) (*print-level* 4))
        (let ((text (prin1-to-string name)))
          ;; (LAMBDA NIL …) as written: (LAMBDA () …)
          (if (and (consp name) (member (first name) '(lambda flet labels)) (eql 0 (search "(LAMBDA NIL" text)))
              (concatenate 'string "(LAMBDA ()" (subseq text 11))
              text)))))

(defun report (&key (limit 1500) (min-fraction 1/2000))
  "The samples, as (:samples n :mode m :running p :functions … :tree …).
Each function is (name definition self total callers callees), callers and
callees as ((name . count) …); TREE is (name count child …), its children
those of at least MIN-FRACTION of the samples."
  (let ((texts (make-hash-table :test 'equal)) ; name → its text, printed once
        (stats (make-hash-table :test 'equal)) ; text → #(self total callers callees definition)
        (tree (list "all" 0))
        (samples 0))
    (flet ((text (name)
             (or (gethash name texts)
                 (setf (gethash name texts) (name-text name))))
           (stat (text name)
             (or (gethash text stats)
                 (setf (gethash text stats)
                       (vector 0 0 (make-hash-table :test 'equal) (make-hash-table :test 'equal)
                               (let ((d (definition-name name))) (and d (name-text d))))))))
      (when (symbol-value (internal "*SAMPLES*" "SB-SPROF"))
       ;; Each thread's samples are gathered when a report is made.
       (unless (profiling-p)
         (let ((progress (internal "*SHOW-PROGRESS*" "SB-SPROF")))
           (progv (and progress (list progress)) '(nil)
             (ignore-errors (sb-sprof:report :type nil)))))
       (sb-sprof:map-traces
       (lambda (thread trace)
         (declare (ignore thread))
         (let ((names '()))
           (funcall (internal "MAP-TRACE-PC-LOCS" "SB-SPROF") (lambda (info pc) (declare (ignore pc)) (push (frame-name info) names)) trace)
           (setf names (nreverse names))
           (when *trim*
             (let ((at (position 'call-thunk names :from-end t)))
               (setf names (and at (nthcdr (1+ at) names)))
               ;; and the lambda around the form
               (when (and (consp (first names)) (eq (first (first names)) 'lambda)
                          (null (definition-name (first names))))
                 (pop names))))
           (when names
             (incf samples)
             (incf (second tree))
             (let ((node tree) (seen '()) (pairs '()) (caller nil))
               (dolist (name names)
                 (let* ((text (text name))
                        (s (stat text name)))
                   (unless (member text seen :test #'string=)
                     (push text seen)
                     (incf (aref s 1)))
                   (when (and caller (not (member (cons caller text) pairs :test #'equal)))
                     (push (cons caller text) pairs)
                     (incf (gethash caller (aref s 2) 0))
                     (incf (gethash text (aref (gethash caller stats) 3) 0)))
                   (setf caller text)
                   (let ((child (assoc text (cddr node) :test #'string=)))
                     (unless child
                       (setf child (list text 0))
                       (setf (cddr node) (cons child (cddr node))))
                     (incf (second child))
                     (setf node child))))
               (incf (aref (gethash caller stats) 0))))))
       (symbol-value (internal "*SAMPLES*" "SB-SPROF")))))
    (labels ((pairs (table)
               (let ((list '()))
                 (maphash (lambda (k v) (push (cons k v) list)) table)
                 (sort list #'> :key #'cdr)))
             (prune (node)
               (list* (first node) (second node)
                      (loop for child in (sort (copy-list (cddr node)) #'> :key #'second)
                            when (>= (second child) (* min-fraction samples))
                              collect (prune child)))))
      (let ((functions '()))
        (maphash (lambda (text s)
                   (push (list text (aref s 4) (aref s 0) (aref s 1) (pairs (aref s 2)) (pairs (aref s 3)))
                         functions))
                 stats)
        (setf functions (sort functions (lambda (a b) (or (> (fourth a) (fourth b))
                                                          (and (= (fourth a) (fourth b)) (> (third a) (third b)))))))
        (list :samples samples
              :mode (let ((samples (symbol-value (internal "*SAMPLES*" "SB-SPROF")))
                          (mode (internal "SAMPLES-MODE" "SB-SPROF")))
                      (string-downcase (or (and samples mode (fboundp mode) (ignore-errors (funcall mode samples))) :cpu)))
              :running (profiling-p)
              :functions (if (> (length functions) limit) (subseq functions 0 limit) functions)
              :tree (prune tree))))))
