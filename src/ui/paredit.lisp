;;;; paredit.lisp — structural editing commands, and paredit mode
;;;;
;;;; The commands that reshape lists (slurp, barf, raise, splice, wrap,
;;;; split, join) work in every Lisp buffer. Paredit mode, a minor mode,
;;;; also keeps parentheses balanced as you type: ( inserts a pair, ) moves
;;;; past the end of the list, DEL and C-d step over parentheses rather than
;;;; delete half a pair, and C-k kills only whole expressions. The edits
;;;; themselves are computed in core (lisp/paredit.lisp).

(in-package #:cadre-ui)

(define-minor-mode paredit-mode (:title "Paredit")
  "Keep parentheses balanced while typing.")

(define-option *paredit* nil boolean
  "Turn on paredit mode in Lisp buffers and REPLs.")

(defparameter *paredit-major-modes* '(lisp-mode repl-mode editor-repl-mode))

(defun maybe-enable-paredit (buffer)
  (when (and *paredit* (member (buffer-major-mode buffer) *paredit-major-modes*))
    (set-minor-mode buffer 'paredit-mode t)))

(add-hook '*buffer-created-hook* 'maybe-enable-paredit)

(define-command paredit-mode ()
  "Turn paredit mode on or off, in every Lisp buffer and REPL."
  (setf *paredit* (not *paredit*)
        (setting :paredit) *paredit*)
  (dolist (buffer (buffer-list))
    (when (member (buffer-major-mode buffer) *paredit-major-modes*)
      (set-minor-mode buffer 'paredit-mode *paredit*)))
  (when *window* (update-status *window*))
  (message "Paredit mode ~:[off~;on~]" *paredit*))

(defun edits-editable-p (gtk-buffer edits)
  "True unless one of EDITS would change read-only text (the REPL's prompt and output)."
  (every (lambda (edit)
           (destructuring-bind (start end string) edit
             (declare (ignore string))
             (if (< start end)
                 (loop for i from start below end
                       always (gtk:text-iter-editable (iter-at gtk-buffer i) t))
                 (gtk:text-iter-can-insert (iter-at gtk-buffer start) t))))
         edits))

(defun run-structural-edit (function &key (reindent t) args)
  "Make the edits FUNCTION (a core paredit function) computes at the cursor,
as one undo step. With REINDENT, re-indent the lines the edits span."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (syntax (current-syntax)))
    (multiple-value-bind (line column) (text-position-line gtk-buffer (point-offset view))
      (multiple-value-bind (edits point stick delta) (apply function syntax line column args)
        (unless (edits-editable-p gtk-buffer edits) (editor-error "Text is read-only"))
        (with-user-action (gtk-buffer)
          (apply-edits gtk-buffer edits point (or stick :before) (or delta 0))
          (when (and reindent edits)
            (let* ((first (reduce #'min edits :key #'first))
                   (last (map-offset (reduce #'max edits :key #'second) edits :after))
                   (first-line (text-position-line gtk-buffer (min first (text-length gtk-buffer))))
                   (last-line (text-position-line gtk-buffer (min last (text-length gtk-buffer)))))
              (when (> last-line first-line)
                (indent-lines view (1+ first-line) last-line)))))
        (scroll-to-cursor view)))))

(defmacro define-structural-command (name documentation function &rest options)
  `(define-command ,name ()
     ,documentation
     (:modes lisp-mode repl-mode editor-repl-mode)
     ,@options
     (run-structural-edit ',function)))

(define-structural-command slurp-forward
  "Pull the expression after the list into it: (a b|) c → (a b| c)."
  paredit-slurp-forward (:repeat t))
(define-structural-command barf-forward
  "Push the list's last expression out of it: (a b| c) → (a b|) c."
  paredit-barf-forward (:repeat t))
(define-structural-command slurp-backward
  "Pull the expression before the list into it: a (b| c) → (a b| c)."
  paredit-slurp-backward (:repeat t))
(define-structural-command barf-backward
  "Push the list's first expression out of it: (a b| c) → a (b| c)."
  paredit-barf-backward (:repeat t))
(define-structural-command raise-sexp
  "Replace the list around the cursor with the expression at the cursor: (a |b c) → |b."
  paredit-raise)
(define-structural-command splice-sexp
  "Remove the parentheses around the cursor: (a (b| c) d) → (a b| c d)."
  paredit-splice)
(define-structural-command splice-sexp-killing-backward
  "Remove the parentheses around the cursor and what comes before it: (a |b) → |b."
  paredit-splice-killing-backward)
(define-structural-command splice-sexp-killing-forward
  "Remove the parentheses around the cursor and what comes after it: (a| b) → a|."
  paredit-splice-killing-forward)
(define-structural-command split-sexp
  "Split the list (or string) at the cursor in two: (a| b) → (a)| (b)."
  paredit-split)
(define-structural-command join-sexps
  "Join the lists (or strings) before and after the cursor: (a) |(b) → (a |b)."
  paredit-join)

(define-command wrap-round ()
  "Wrap the next expression, or the selection, in parentheses: |a b → (|a) b."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (let ((view (current-view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
      (setf (buffer-local (view-buffer view) :mark-active) nil)
      (run-structural-edit 'paredit-wrap
                           :args (and has (list :region (cons (gtk:text-iter-get-offset start)
                                                              (gtk:text-iter-get-offset end))))))))

;;; Typing, in paredit mode

(define-command paredit-open-round ()
  "Insert a pair of parentheses (in code), or wrap the selection in them."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (if (gtk:text-buffer-get-has-selection (view-gtk-buffer (current-view)))
      (call-command 'wrap-round)
      (run-structural-edit 'paredit-open :reindent nil)))

(define-command paredit-close-round ()
  "Move past the end of the list, deleting the spaces before its close paren."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (run-structural-edit 'paredit-close :reindent nil))

(define-command paredit-doublequote ()
  "Insert a pair of quotes; inside a string, move past its end or insert \\\"."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (run-structural-edit 'paredit-quote :reindent nil))

(defun delete-selection-if-any (view)
  (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
    (when has
      (delete-text-between (view-gtk-buffer view) (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end))
      t)))

(define-command paredit-backward-delete ()
  "Delete the character before the cursor, stepping over parentheses and quotes instead of unbalancing them."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (:repeat t)
  (unless (delete-selection-if-any (current-view))
    (run-structural-edit 'paredit-delete-before :reindent nil)))

(define-command paredit-forward-delete ()
  "Delete the character after the cursor, stepping over parentheses and quotes instead of unbalancing them."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (:repeat t)
  (unless (delete-selection-if-any (current-view))
    (run-structural-edit 'paredit-delete-after :reindent nil)))
