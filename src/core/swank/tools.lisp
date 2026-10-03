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
