;;;; stepper.lisp — stepping through evaluation, one form at a time
;;;;
;;;; Step Expression evaluates a form under CL:STEP (on a definition, it
;;;; first compiles it for stepping and asks for a call to step). The Lisp
;;;; stops before each function call; the Debugger page becomes the Stepper,
;;;; showing the call and its arguments, and the call is highlighted in its
;;;; source. Step Into goes into the call, Step Over makes it whole, Step Out
;;;; finishes the current function, and Resume runs on.
;;;;
;;;; Only code compiled for stepping stops: CL:STEP compiles the stepped form
;;;; that way itself, and Compile for Debugging (or a (debug 3) policy) does
;;;; it for definitions, so their calls can be stepped into.

(in-package #:cadre-ui)

(defun stepping-level-p (level)
  "True if LEVEL is the stepper stopped at a form, not an error."
  (and level (stepper-condition-p (dl-condition level))))

(defun render-stepping (level)
  (destructuring-bind (text &rest more) (dl-condition level)
    (declare (ignore more))
    (gtk:box-append *debugger-box* (label "Stepping" :css-classes '("title-4")))
    (gtk:box-append *debugger-box* (label (string-trim '(#\Space #\Newline) text)
                                          :wrap t :selectable t :css-classes '("monospace")))
    (gtk:box-append *debugger-box*
                    (gtk:build
                      (gtk:box :spacing 6 :margin-top 4
                        (gtk:button :label "Step _Into" :use-underline t :css-classes '("suggested-action")
                                    :tooltip-text "Go into this call, stopping at the calls it makes (s)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'step-into)))
                        (gtk:button :label "Step _Over" :use-underline t
                                    :tooltip-text "Make this call whole and stop at the next one (x)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'step-over)))
                        (gtk:button :label "Step O_ut" :use-underline t
                                    :tooltip-text "Finish the current function and stop after it returns (o)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'step-out)))
                        (gtk:button :label "_Resume" :use-underline t
                                    :tooltip-text "Stop stepping and run on (c)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'stop-stepping)))
                        (gtk:button :label "_Abort" :use-underline t :css-classes '("destructive-action")
                                    :tooltip-text "Abandon the evaluation (a)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'debugger-abort))))))
    (gtk:box-append *debugger-box*
                    (label "The stepper stops before function calls, in code compiled for stepping. To step into a function, compile it with Compile for Debugging first."
                           :wrap t :css-classes '("dim-label" "caption")))
    (render-backtrace level)))

;;; Moving

(defun stepping-level ()
  (let ((level (first *debug-levels*)))
    (unless (stepping-level-p level) (editor-error "Not stepping: use Step Expression to start"))
    level))

(defun step-command (name)
  (let ((level (stepping-level)))
    (frame-rex level (swank-call name 0) :on-abort (lambda (reason) (declare (ignore reason))))))

(define-command step-into ()
  "While stepping, go into the call, stopping at the calls it makes. In the
debugger after a break, continue and stop at the next call compiled for stepping."
  (let ((level (first *debug-levels*)))
    (cond ((stepping-level-p level) (step-command "swank:sldb-step"))
          ((and level (restart-position level "CONTINUE"))
           (frame-rex level (swank-call "swank:sldb-step" 0) :on-abort (lambda (reason) (declare (ignore reason)))))
          (t (editor-error "Not stepping: use Step Expression to start")))))

(define-command step-over ()
  "While stepping, make the call whole and stop at the next one."
  (step-command "swank:sldb-next"))

(define-command step-out ()
  "While stepping, finish the current function and stop after it returns."
  (step-command "swank:sldb-out"))

(define-command stop-stepping ()
  "Stop stepping and let the evaluation run on."
  (let* ((level (stepping-level))
         (n (or (restart-position level "STEP-CONTINUE") (restart-position level "CONTINUE")
                (editor-error "No restart to resume with"))))
    (invoke-restart-number n)))

;;; The form being stepped, highlighted in its source

(defvar *stepped-form* nil "(buffer start-mark end-mark) of the highlighted form, or nil.")

(defun clear-stepped-form ()
  (when *stepped-form*
    (destructuring-bind (buffer start end) *stepped-form*
      (let ((gtk-buffer (buffer-text buffer)))
        (unless (gtk:text-mark-get-deleted start)
          (gtk:text-buffer-remove-tag-by-name gtk-buffer "cadre-stepper-current"
                                              (gtk:text-buffer-get-iter-at-mark gtk-buffer start)
                                              (gtk:text-buffer-get-iter-at-mark gtk-buffer end))
          (gtk:text-buffer-delete-mark gtk-buffer start)
          (gtk:text-buffer-delete-mark gtk-buffer end))))
    (setf *stepped-form* nil)))

(defun highlight-form-at-cursor (view)
  "Highlight the form starting at VIEW's cursor as the one being stepped."
  (clear-stepped-form)
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (syntax (buffer-syntax buffer)))
    (when syntax
      (multiple-value-bind (line column) (cursor-line-column view)
        (multiple-value-bind (el ec) (forward-sexp-position syntax line column)
          (when el
            (let ((start (line-iter gtk-buffer line column))
                  (end (line-iter gtk-buffer el ec)))
              (ensure-face-tag gtk-buffer "cadre-stepper-current" :stepper-current)
              (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-stepper-current" start end)
              (setf *stepped-form*
                    (list buffer
                          (gtk:text-buffer-create-mark gtk-buffer nil start t)
                          (gtk:text-buffer-create-mark gtk-buffer nil end nil))))))))))

(defun show-stepped-form (level)
  "Show where LEVEL, the stepper stopped at a form, is in the source, then
give the Stepper the keyboard again."
  (frame-rex level (swank-call "swank::frame-source-location" 0)
             :on-abort (lambda (reason) (declare (ignore reason)) (focus-debugger))
             :on-ok (lambda (reply)
                      (let ((location (parse-location reply)))
                        (if (getf location :error)
                            (progn (clear-stepped-form) (focus-debugger))
                            (handler-case
                                (goto-location location
                                               :then (lambda (view)
                                                       (when (member level *debug-levels*)
                                                         (highlight-form-at-cursor view)
                                                         (focus-debugger))))
                              (editor-error () (focus-debugger))))))))

;;; Starting

(defun step-form-text (text package)
  "Evaluate TEXT, a form, under the stepper."
  (with-connection (connection)
    (rex connection (swank-call "swank:interactive-eval" (format nil "(cl:step ~a)" text) 3 120) :package package
         :on-ok (lambda (result) (show-result result) (image-changed))
         :on-abort (lambda (reason) (declare (ignore reason)) (message "Stepping abandoned")))))

(defun definition-name-at (view syntax line column)
  "The name the definition at (LINE, COLUMN) in VIEW defines, as written, or nil."
  (multiple-value-bind (sl sc) (toplevel-form-bounds syntax line column)
    (when sl
      (multiple-value-bind (hl hc) (forward-sexp-position syntax sl (1+ sc))
        (when hl
          (multiple-value-bind (el ec) (forward-sexp-position syntax hl hc)
            (when el
              (let ((text (string-trim '(#\Space #\Tab #\Newline) (view-region-text view hl hc el ec))))
                (and (plusp (length text)) text)))))))))

(define-command step-expression ()
  "Step through an evaluation: the selection, or the top-level form at the
cursor. On a definition, compile it for stepping and ask for a call to step.
While already stepping, step into the next form."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (package (view-package view)))
    (cond ((stepping-level-p (first *debug-levels*)) (step-into))
          ((gtk:text-buffer-get-has-selection gtk-buffer)
           (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
             (declare (ignore has))
             (step-form-text (gtk:text-buffer-get-text gtk-buffer start end t) package)))
          (t
           (multiple-value-bind (line column) (cursor-line-column view)
             (let ((syntax (current-syntax)))
               (multiple-value-bind (sl sc el ec) (toplevel-form-bounds syntax line column)
                 (unless sl (editor-error "Not in a top-level form"))
                 (if (definition-form-p syntax line column)
                     (let ((name (definition-name-at view syntax line column)))
                       (compile-toplevel-form
                        :policy (list (cons (remote-symbol "cl:debug") 3))
                        :what "Compiled for stepping"
                        :then (lambda (ok)
                                (when ok
                                  (open-picker (window-picker *window*)
                                               :placeholder "Step which call?"
                                               :text (if (and name (not (find #\( name))) (format nil "(~a " name) "")
                                               :on-choose (lambda (text)
                                                            (unless (string= (string-trim " " text) "")
                                                              (step-form-text text package))))))))
                     (step-form-text (view-region-text view sl sc el ec) package)))))))))

(define-command compile-defun-for-debugging ()
  "Compile the top-level form around the cursor with full debugging
information (debug 3): its locals show in the debugger, and the stepper can
step into it."
  (:modes lisp-mode)
  (compile-toplevel-form :policy (list (cons (remote-symbol "cl:debug") 3))
                         :what "Compiled for debugging"))
