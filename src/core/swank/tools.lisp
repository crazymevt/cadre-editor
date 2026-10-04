;;;; tools.lisp — the replies of Swank's inspector and cross-referencer,
;;;; turned into plain data for the GUI
;;;;
;;;; The inspector describes an object as a title and a list of parts:
;;;; strings, values (which can be inspected in turn), actions (which run
;;;; something in the Lisp) and labels. The cross-referencer answers with
;;;; names and source locations, grouped by kind.

(in-package #:cadre)

;;; The inspector

(defstruct (inspection (:conc-name inspection-))
  "An object as the inspector shows it."
  (title "" :type string)
  (parts '())                           ; segments, see INSPECTOR-SEGMENTS
  (next 0)                              ; where the next range of parts starts
  (more nil))                           ; true if there are parts not yet fetched

(defun inspector-segments (parts)
  "Swank's inspector PARTS as segments: (:text string), (:value string index),
(:action string index) or (:label string)."
  (loop for part in parts
        collect (cond ((stringp part) (list :text part))
                      ((and (consp part) (eq (first part) :value))
                       (list :value (string (second part)) (third part)))
                      ((and (consp part) (eq (first part) :action))
                       (list :action (string (second part)) (third part)))
                      ((and (consp part) (eq (first part) :label))
                       (list :label (string (second part))))
                      (t (list :text (princ-to-string part))))))

(defun parse-inspector-range (content)
  "From the inspector's (parts length start end): the segments, where the
next range starts, and whether there is more."
  (destructuring-bind (parts length start end) content
    (declare (ignore start))
    (values (inspector-segments parts) length (> length end))))

(defun parse-inspection (reply)
  "An INSPECTION from a reply of swank:init-inspector and friends, or nil
for a reply of nil (no object, as from inspector-pop at the first one)."
  (when (consp reply)
    (multiple-value-bind (parts next more) (parse-inspector-range (getf reply :content))
      (make-inspection :title (or (getf reply :title) "") :parts parts :next next :more more))))

;;; Cross-references

(defparameter *xref-kinds*
  '((:calls "Calls" "who calls")
    (:calls-who "Called by it" "calls who")
    (:references "References" "who references")
    (:binds "Binds" "who binds")
    (:sets "Sets" "who sets")
    (:macroexpands "Expands" "who macroexpands")
    (:specializes "Methods" "who specializes")
    (:callers "Callers" "callers")
    (:callees "Callees" "callees"))
  "Each kind of cross-reference: its keyword, a heading and a phrase.")

(defun xref-kind-heading (kind)
  (or (second (assoc kind *xref-kinds*)) (string-capitalize (string kind))))

(defstruct (xref (:constructor make-xref (kind name location)))
  "A place that refers to a name: its KIND (:calls…), the NAME of the
definition there, and its LOCATION as from PARSE-LOCATION."
  kind name location)

(defun parse-xrefs (kind reply)
  "XREFs from a reply of swank:xref (for KIND), or :not-implemented."
  (if (eq reply :not-implemented)
      :not-implemented
      (loop for (name location) in reply
            collect (make-xref kind (princ-to-string name) (parse-location location)))))

(defun parse-xrefs-groups (reply)
  "XREFs from a reply of swank:xrefs: ((kind (name location)…)…)."
  (loop for (kind . entries) in reply
        append (parse-xrefs kind entries)))

;;; The debugger

(defstruct (frame (:constructor make-frame (number description restartable)))
  "A backtrace frame: its NUMBER, how it prints, and whether it can be restarted."
  number description restartable)

(defun parse-frames (frames)
  "FRAMEs from swank:backtrace's ((number description [plist])…)."
  (loop for (number description . more) in frames
        collect (make-frame number description (and (getf (first more) :restartable) t))))

(defun parse-frame-locals (reply)
  "From swank:frame-locals-and-catch-tags: the locals as (name value) pairs,
and the catch tags."
  (destructuring-bind (locals tags) reply
    (values (loop for local in locals
                  collect (list (getf local :name) (getf local :value)))
            tags)))

(defun stepper-condition-p (condition)
  "True if CONDITION, the (text type …) of a :debug event, is the stepper
stopping at a form rather than an error."
  (and (consp condition) (stringp (second condition))
       (search "STEP-FORM-CONDITION" (string-upcase (second condition)))
       t))

;;; The trace dialog (swank-trace-dialog)

(defstruct (trace-call (:conc-name trace-call-))
  "One call of a traced function: its ID, its PARENT's id (or nil), the
function's NAME, its ARGS and RESULTS (as printed), and its STATE:
:returned, :running (not returned yet) or :unwound (left by a non-local exit)."
  id parent name (args '()) (results '()) (state :returned) (children '()))

(defun trace-spec-name (spec)
  "A traced function's spec, as read from the Lisp, as text: symbols without
their package, so (SETF CL-USER::FOO) is \"(setf foo)\"."
  (labels ((walk (x)
             (cond ((remote-symbol-p x) (string-downcase (remote-symbol-base-name x)))
                   ((consp x) (format nil "(~{~a~^ ~})" (mapcar #'walk x)))
                   ((stringp x) x)
                   (t (string-downcase (princ-to-string x))))))
    (walk spec)))

(defun parse-trace-call (entry)
  "A TRACE-CALL from the trace dialog's (id parent spec ((i text)…) ((i text)…))."
  (destructuring-bind (id parent spec args results &rest more) entry
    (declare (ignore more))
    (let* ((results (mapcar #'second results))
           (state (cond ((and (= 1 (length results)) (search "STILL-INSIDE" (string-upcase (first results)))) :running)
                        ((and (= 1 (length results)) (search "EXITED-NON-LOCALLY" (string-upcase (first results)))) :unwound)
                        (t :returned))))
      (make-trace-call :id id :parent parent :name (trace-spec-name spec)
                       :args (mapcar #'second args)
                       :results (if (eq state :returned) results '())
                       :state state))))

(defstruct (trace-tree (:conc-name trace-tree-))
  "The calls fetched from the trace dialog so far, by id, and the top-level ones."
  (calls (make-hash-table))
  (roots '())                           ; newest first
  (key 0))                              ; for report-partial-tree: a new key starts over

(defun trace-tree-add (tree entries)
  "Add the trace dialog's ENTRIES to TREE: new calls, and calls fetched
earlier that have since returned. Returns the number of new calls and
the number of calls updated."
  (let ((new 0) (updated 0))
    (dolist (entry entries (values new updated))
      (let* ((call (parse-trace-call entry))
             (old (gethash (trace-call-id call) (trace-tree-calls tree))))
        (cond (old
               (unless (and (eq (trace-call-state old) (trace-call-state call))
                            (equal (trace-call-results old) (trace-call-results call)))
                 (incf updated))
               (setf (trace-call-results old) (trace-call-results call)
                     (trace-call-state old) (trace-call-state call)))
              (t
               (incf new)
               (setf (gethash (trace-call-id call) (trace-tree-calls tree)) call)
               (let ((parent (and (trace-call-parent call)
                                  (gethash (trace-call-parent call) (trace-tree-calls tree)))))
                 (if parent
                     (push call (trace-call-children parent))
                     (push call (trace-tree-roots tree))))))))))

(defun trace-tree-count (tree)
  (hash-table-count (trace-tree-calls tree)))

(defun trace-tree-lines (tree &key collapsed (limit most-positive-fixnum))
  "TREE's calls in order, each as (call depth): oldest first, children under
their parent, skipping the children of calls in COLLAPSED (a hash table of
ids). At most LIMIT lines."
  (let ((lines '()) (count 0))
    (labels ((walk (calls depth)
               (dolist (call (reverse calls))
                 (when (>= count limit) (return-from trace-tree-lines (nreverse lines)))
                 (push (list call depth) lines)
                 (incf count)
                 (unless (and collapsed (gethash (trace-call-id call) collapsed))
                   (walk (trace-call-children call) (1+ depth))))))
      (walk (trace-tree-roots tree) 0))
    (nreverse lines)))

(defun trace-call-text (call)
  "How a call shows: (name arg…) ⇒ results."
  (format nil "(~a~{ ~a~})~a" (trace-call-name call) (trace-call-args call)
          (ecase (trace-call-state call)
            (:running " …")
            (:unwound " ⇏ exited non-locally")
            (:returned (format nil " ⇒ ~:[nothing~;~:*~{~a~^, ~}~]" (trace-call-results call))))))
