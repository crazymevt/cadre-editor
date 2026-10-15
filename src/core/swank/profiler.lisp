;;;; profiler.lisp — sb-sprof's samples, as the Profiler page shows them
;;;;
;;;; The other Lisp profiles with sb-sprof, through a small helper Cadre
;;;; sends it (profiler-remote.lisp, kept as text in +PROFILER-HELPER+). Its
;;;; report is plain data: each function with the samples it was running in
;;;; itself (self) and anywhere on the stack (total), its callers and
;;;; callees; and the tree of stacks, for the flame graph.

(in-package #:cadre)

(defparameter +profiler-helper+
  #.(uiop:read-file-string (merge-pathnames "profiler-remote.lisp" (or *compile-file-truename* *load-truename*)))
  "The helper's source, evaluated in the other Lisp before profiling.")

(defstruct (profile-function (:conc-name profile-function-))
  "A function seen in the samples. DEFINITION is the name of the definition
its code is in (for a lambda, the function around it), or nil."
  (name "" :type string) definition (self 0) (total 0) (callers '()) (callees '()))

(defstruct (profile (:conc-name profile-))
  (samples 0) (mode "cpu") (running nil)
  (functions '())                       ; profile-functions, most total first
  (tree nil))                           ; (name count child …)

(defun parse-profile-report (reply)
  "A PROFILE from the helper's REPORT."
  (flet ((pairs (list) (loop for (name . count) in list collect (cons name count))))
    (make-profile :samples (or (getf reply :samples) 0)
                  :mode (or (getf reply :mode) "cpu")
                  :running (and (getf reply :running) t)
                  :functions (loop for (name definition self total callers callees) in (getf reply :functions)
                                   collect (make-profile-function :name name :definition definition
                                                                  :self self :total total
                                                                  :callers (pairs callers) :callees (pairs callees)))
                  :tree (getf reply :tree))))

(defun profile-percent (count samples)
  "COUNT of SAMPLES as a percentage, as text."
  (if (plusp samples)
      (format nil "~,1f%" (/ (* 100.0 count) samples))
      "–"))

(defun profile-mode-title (mode)
  (cond ((equal mode "alloc") "allocation")
        ((equal mode "time") "wall time")
        (t "CPU")))

(defun sort-profile-functions (functions key &key filter)
  "FUNCTIONS sorted by KEY (:self, :total or :name), most first (names A to
Z); with FILTER, only those whose name contains it."
  (let ((list (if (and filter (plusp (length filter)))
                  (remove-if-not (lambda (f) (search filter (profile-function-name f) :test #'char-equal)) functions)
                  (copy-list functions))))
    (ecase key
      (:self (stable-sort list #'> :key #'profile-function-self))
      (:total (stable-sort list #'> :key #'profile-function-total))
      (:name (stable-sort list #'string-lessp :key #'profile-function-name)))))

(defun find-profile-function (profile name)
  (find name (profile-functions profile) :key #'profile-function-name :test #'string=))

;;; The flame graph: the tree of stacks, callers above the calls they made

(defun flame-node-at (tree path)
  "The node of TREE that PATH (names below the root) leads to, or nil."
  (let ((node tree))
    (dolist (name path node)
      (setf node (assoc name (cddr node) :test #'string=))
      (unless node (return nil)))))

(defun flame-boxes (tree &key path (max-depth 200))
  "The flame graph's boxes, as (node depth x width), X and WIDTH fractions of
the whole width. With PATH, zoomed into that node: it and the nodes above it
take the whole width."
  (let ((boxes '())
        (node tree)
        (depth 0))
    ;; The way down to the zoomed node, full width
    (dolist (name path)
      (push (list node depth 0.0 1.0) boxes)
      (incf depth)
      (setf node (or (assoc name (cddr node) :test #'string=) (return))))
    (labels ((lay (node depth x width)
               (push (list node depth x width) boxes)
               (when (and (< depth max-depth) (plusp (second node)))
                 (let ((x x))
                   (dolist (child (cddr node))
                     (let ((w (* width (/ (second child) (second node)))))
                       (lay child (1+ depth) x w)
                       (incf x w)))))))
      (lay node depth 0.0 1.0))
    (nreverse boxes)))

(defun flame-path-to (tree target)
  "The path to TARGET, a node of TREE (compared with EQ), or nil."
  (labels ((walk (node path)
             (if (eq node target)
                 (return-from flame-path-to (reverse path))
                 (dolist (child (cddr node))
                   (walk child (cons (first child) path))))))
    (walk tree '())
    nil))

(defun flame-depth (boxes)
  (1+ (reduce #'max boxes :key #'second :initial-value 0)))
