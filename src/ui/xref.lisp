;;;; xref.lisp — cross-references: who calls, references, binds or sets a name
;;;;
;;;; Results go to the References page, grouped by kind, each with the
;;;; definition's name and where it is. Clicking one goes there.

(in-package #:cadre-ui)

(defvar *references-list* nil "The References page's list box.")
(defvar *references-title* nil)
(defvar *reference-rows* (make-hash-table :test 'eq) "List box row → xref.")

(defun make-references-widget ()
  (setf *references-list* (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar"))
        *references-title* (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :css-classes '("heading")
                                                     :label "No references yet"))
  (gobject:connect *references-list* :row-activated
                   (lambda (box row)
                     (declare (ignore box))
                     (let ((xref (gethash row *reference-rows*)))
                       (when xref
                         (handler-case (goto-location (xref-location xref))
                           (editor-error (e) (message "~a" (editor-error-message e))))))))
  (gtk:build
    (gtk:box :orientation :vertical
      (gtk:box :margin-start 10 :margin-end 6 :margin-top 4 :margin-bottom 4
        *references-title*)
      (gtk:separator)
      (gtk:scrolled-window :vexpand t :child *references-list*))))

;;; Lines for positions, so a result can say file:line

(defun location-line (location)
  "The line (from 0) LOCATION points at, or nil."
  (or (getf location :line)
      (let ((position (getf location :position)))
        (when position
          (let ((buffer (or (and (getf location :buffer) (find-buffer (getf location :buffer)))
                            (and (getf location :file) (find-file-buffer (getf location :file))))))
            (cond (buffer
                   (values (text-position-line (buffer-text buffer) (min position (text-length (buffer-text buffer))))))
                  ((getf location :file)
                   (ignore-errors
                    (with-open-file (in (getf location :file) :external-format :utf-8)
                      (let ((count 0))
                        (dotimes (i position count)
                          (let ((c (read-char in nil)))
                            (unless c (return count))
                            (when (char= c #\Newline) (incf count))))))))))))))

(defun location-place-string (location)
  (cond ((getf location :error) (getf location :error))
        (t (format nil "~a~@[:~d~]"
                   (cond ((getf location :file) (file-namestring (getf location :file)))
                         ((getf location :buffer))
                         (t ""))
                   (let ((line (location-line location))) (and line (1+ line)))))))

(defun show-references (title xrefs)
  "Show XREFS on the References page under TITLE."
  (gtk:list-box-remove-all *references-list*)
  (clrhash *reference-rows*)
  (gtk:label-set-text *references-title* title)
  (if (null xrefs)
      (gtk:list-box-append *references-list*
                           (make-instance 'gtk:label :label "None found" :xalign 0.0 :margin-start 6
                                                     :css-classes '("dim-label")))
      (let ((kind nil))
        (dolist (xref xrefs)
          (unless (eq kind (xref-kind xref))
            (setf kind (xref-kind xref))
            (let ((heading (make-instance 'gtk:list-box-row :activatable nil :selectable nil
                                                            :child (make-instance 'gtk:label :label (xref-kind-heading kind)
                                                                                             :xalign 0.0 :margin-top 4
                                                                                             :css-classes '("caption-heading" "dim-label")))))
              (gtk:list-box-append *references-list* heading)))
          (let ((row (make-instance 'gtk:list-box-row
                                    :child (gtk:build
                                             (gtk:box :spacing 12 :margin-start 8
                                               (gtk:label :label (substitute #\Space #\Newline (xref-name xref))
                                                          :xalign 0.0 :hexpand t :ellipsize :end
                                                          :css-classes '("monospace"))
                                               (gtk:label :label (location-place-string (xref-location xref))
                                                          :css-classes '("dim-label")))))))
            (setf (gethash row *reference-rows*) xref)
            (gtk:list-box-append *references-list* row)))))
  (panel-set-title (window-panel *window*) "references"
                   (if xrefs (format nil "References (~d)" (length xrefs)) "References"))
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "references"))

;;; Asking

(defun call-with-symbol-name (prompt function)
  "Call FUNCTION with the symbol at the cursor and its package; if there is
none, ask for a name."
  (let* ((view (current-view))
         (syntax (and view (buffer-syntax (view-buffer view))))
         (name (and syntax (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c))))
         (package (if view (view-package view) (and (connected-p) (connection-package *connection*)))))
    (if name
        (funcall function name package)
        (open-picker (window-picker *window*)
                     :placeholder prompt
                     :on-choose (lambda (text)
                                  (unless (string= (string-trim " " text) "")
                                    (funcall function (string-trim " " text) package)))))))

(defun xref-command (kinds)
  "Find KINDS of cross-references to the symbol at the cursor."
  (call-with-symbol-name
   "Find references to"
   (lambda (name package)
     (if (not (connected-p))
         ;; No Lisp to ask: search the project's source for the symbol instead.
         (progn (setf (ps-symbol *project-search*) name)
                (open-project-search :text name :mode :symbol)
                (message "No Lisp running: showing where ~a appears in the project's files" name))
     (with-connection (connection)
       (rex connection
            (if (rest kinds)
                (swank-call "swank:xrefs" kinds name)
                (swank-call "swank:xref" (first kinds) name))
            :package package
            :on-ok (lambda (reply)
                     (let ((xrefs (if (rest kinds) (parse-xrefs-groups reply) (parse-xrefs (first kinds) reply))))
                       (if (eq xrefs :not-implemented)
                           (message "This Lisp cannot find ~a" (third (assoc (first kinds) *xref-kinds*)))
                           (show-references (if (rest kinds)
                                                (format nil "References to ~a" name)
                                                (format nil "~@(~a~) ~a" (third (assoc (first kinds) *xref-kinds*)) name))
                                            xrefs))))))))))

(define-command find-references ()
  "Find everything that calls, references, binds, sets, expands or specializes the symbol at the cursor."
  (xref-command '(:calls :references :binds :sets :macroexpands :specializes)))

(define-command who-calls ()
  "Find the functions that call the function at the cursor."
  (xref-command '(:calls)))

(define-command who-references ()
  "Find the code that refers to the global variable at the cursor."
  (xref-command '(:references)))

(define-command who-binds ()
  "Find the code that binds the global variable at the cursor."
  (xref-command '(:binds)))

(define-command who-sets ()
  "Find the code that sets the global variable at the cursor."
  (xref-command '(:sets)))

(define-command who-macroexpands ()
  "Find the code that uses the macro at the cursor."
  (xref-command '(:macroexpands)))

(define-command who-specializes ()
  "Find the methods specialized on the class at the cursor."
  (xref-command '(:specializes)))

(define-command list-callers ()
  "List the functions that call the function at the cursor (by searching the image)."
  (xref-command '(:callers)))

(define-command list-callees ()
  "List the functions the function at the cursor calls."
  (xref-command '(:callees)))
