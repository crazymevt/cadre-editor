;;;; drive.lisp — drive a Cadre window from a script: steps run one after
;;;; another on GTK's main loop, with checks, key presses and screenshots.
;;;; Loaded by smoke.lisp and gallery.lisp after their own IN-PACKAGE, with
;;;; *OUT* (where screenshots go) and *ROOT* (the project) defined, and a
;;;; FINISH function the last step calls.

(defvar *results* '())
(defvar *steps* '())

(defun check (name ok &optional detail)
  (push (list name (and ok t) detail) *results*)
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok name detail))

(defvar *paintable* nil)

(defun screenshot (name &optional widget)
  "Render the window (or WIDGET, such as a popover) to build/smoke/NAME.png."
  (let* ((window (or widget (cadre-ui::window-gtk-window *window*)))
         (w (gtk:widget-get-width window))
         (h (gtk:widget-get-height window))
         (paintable (if widget
                        (gtk:widget-paintable-new window)
                        (or *paintable* (setf *paintable* (gtk:widget-paintable-new window)))))
         (snapshot (gtk:snapshot-new)))
    (gdk:paintable-snapshot paintable snapshot (float w 1d0) (float h 1d0))
    (let* ((node (gtk:snapshot-to-node snapshot))
           (renderer (gtk:native-get-renderer window))
           (texture (gsk:renderer-render-texture renderer node nil))
           (path (merge-pathnames (format nil "~a.png" name) *out*)))
      (ensure-directories-exist path)
      (gdk:texture-save-to-png texture (namestring path))
      path)))

(defun press (keys)
  "Type KEYS (such as \"C-s\") through Cadre's key handling."
  (dolist (key (parse-keys keys))
    (let* ((mods (cadre-ui::key-modifiers key))
           (name (subseq key (* 2 (length mods))))
           (keyval (if (and (= 1 (length name)) (not (alphanumericp (char name 0))))
                       (gdk:unicode-to-keyval (char-code (char name 0))) ; punctuation: - = + …
                       (gdk:keyval-from-name (cond ((string= name "TAB") "Tab")
                                                   ((string= name "RET") "Return")
                                                   ((string= name "ESC") "Escape")
                                                   (t name)))))
           (state (loop for m in mods
                        collect (ecase m (#\C :control-mask) (#\M :alt-mask)
                                  (#\s :super-mask) (#\S :shift-mask)))))
      (cadre-ui::handle-key *window* keyval state))))

(defun has-face-p (line column face)
  "True if the current buffer's text at (LINE, COLUMN) has FACE's tag."
  (let* ((gtk-buffer (buffer-text (current-buffer)))
         (tag (cadre-ui::face-tag gtk-buffer face)))
    (gtk:text-iter-has-tag (cadre-ui::line-iter gtk-buffer line column) tag)))

(defun cursor ()
  (multiple-value-list (cadre-ui::cursor-line-column (current-view))))

(defun set-cursor (line column)
  (cadre-ui::goto-line-column (current-view) line column :extend nil))

(defun line-text (line)
  (text-line-string (buffer-text (current-buffer)) line))

(defun picker () (cadre-ui::window-picker-object *window*))

(defmacro timed (&body body)
  `(let ((start (get-internal-real-time)))
     ,@body
     (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))

(defun insert-at-cursor (string)
  (gtk:text-buffer-insert-at-cursor (buffer-text (current-buffer)) string -1))

(defun cadre-ensure-all ()
  (let ((syntax (cadre-ui::buffer-syntax (current-buffer))))
    (ensure-lexed syntax (1- (syntax-line-count syntax)))))

(defun show-buffer-named (name)
  (cadre-ui::show-buffer *window* (find-buffer name)))

(defun repl-text ()
  (buffer-string (cadre-ui::repl-buffer cadre-ui::*repl*)))

(defun tab-titles ()
  (mapcar #'adw:tab-page-get-title (cadre-ui::window-pages *window*)))

(defun find-widgets (root predicate)
  "Every widget under ROOT (inclusive) satisfying PREDICATE."
  (let ((found '()))
    (labels ((walk (w)
               (when (funcall predicate w) (push w found))
               (loop for c = (gtk:widget-get-first-child w) then (gtk:widget-get-next-sibling c)
                     while c do (walk c))))
      (walk root))
    (nreverse found)))

(defun label-texts (root)
  (mapcar #'gtk:label-get-text (find-widgets root (lambda (w) (typep w 'gtk:label)))))

(defun panel-page-title (name)
  (let ((stack (cadre-ui::panel-stack (cadre-ui::window-panel *window*))))
    (gtk:stack-page-get-title (gtk:stack-get-page stack (gtk:stack-get-child-by-name stack name)))))

;;; Sections: `make smoke ONLY=run-app` runs that section, and the ones it
;;; needs, instead of everything (`make smoke-list` names them). Each
;;; section starts with (section "name" :needs (...)); its steps are tagged
;;; with it.

(defvar *sections* '() "(name . needs), in order.")
(defvar *current-section* nil)

(defmacro section (name &key needs)
  `(progn (setf *current-section* ,name)
          (setf *sections* (append *sections* (list (cons ,name ',needs))))))

(defun push-step (step)
  (push (cons *current-section* step) *steps*))

(defun selected-sections (only)
  "The sections to run for ONLY (names separated by commas or spaces), with
everything they need, in file order; all of them if ONLY is empty."
  (let ((names (remove "" (uiop:split-string only :separator ", ") :test #'string=))
        (chosen '()))
    (labels ((add (name)
               (let ((entry (or (assoc name *sections* :test #'string=)
                                (progn (format t "~&No smoke section ~s. Sections: ~{~a~^ ~}~%"
                                               name (mapcar #'car *sections*))
                                       (uiop:quit 2)))))
                 (unless (member name chosen :test #'string=)
                   (push name chosen)
                   (mapc #'add (cdr entry))))))
      (if names
          (progn (mapc #'add names)
                 (remove-if-not (lambda (s) (member s chosen :test #'string=)) (mapcar #'car *sections*)))
          (mapcar #'car *sections*)))))

(defmacro then (delay &body body)
  `(push-step (cons ,delay (lambda () ,@body))))

(defmacro then-when ((condition &key (timeout 30)) &body body)
  "A step that runs once CONDITION is true (checked every 100 ms), or after TIMEOUT seconds."
  `(push-step (list :wait (lambda () ,condition) ,timeout (lambda () ,@body)
                   ,(format nil "~a: ~a" *current-section*
                            (let ((s (let ((*print-case* :downcase)) (prin1-to-string condition))))
                              (if (> (length s) 70) (concatenate 'string (subseq s 0 70) "…") s))))))

(defun open-files (&rest files)
  "Steps that open FILES (relative to *root*) unless they are open, and show
the first: so a section can run without the ones before it."
  (then 50
    (dolist (file files)
      (unless (find-buffer (file-namestring file))
        (open-file-path (merge-pathnames file *root*)))))
  (then-when ((every (lambda (file) (find-buffer (file-namestring file))) files) :timeout 10)
    (show-buffer-named (file-namestring (first files)))))

(defvar *quicklisp-loaded* nil)

(defun load-quicklisp ()
  "Steps that load Quicklisp into the Lisp, as a usual init file would (the
smoke test starts it without one), and wait for it."
  (then 50
    (with-connection (connection)
      (cadre-ui::rex connection (swank-call "swank:interactive-eval"
                                            "(load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))" 3 120)
                     :on-ok (lambda (v) (declare (ignore v)) (setf *quicklisp-loaded* t)))))
  (then-when (*quicklisp-loaded* :timeout 120)))

(defun use-root-project ()
  "A step that opens *root* again if an earlier section opened another folder
\(new-project, gtk-app): sections whose files are under *root* need it to be
the project."
  (then 50
    (unless (equal (namestring (truename (cadre-ui::window-project *window*))) (namestring (truename *root*)))
      (cadre-ui::set-window-project *window* *root*))))

(defun connect-lisp ()
  "Steps that start a Lisp unless one is connected, and wait for it."
  (then 50
    (unless (or (cadre-ui::connected-p) cadre-ui::*connecting*) (call-command 'lisp)))
  (then-when ((cadre-ui::connected-p) :timeout 120)))

(defun run-step (fn)
  (handler-case (funcall fn)
    (error (e) (check "step ran without error" nil (princ-to-string e)))))

(defun run-steps (steps)
  (if (null steps)
      (finish)
      (let ((step (first steps)))
        (if (eq (car step) :wait)
            (destructuring-bind (condition timeout fn where) (rest step)
              (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
                (glib:timeout-add glib:+priority-default+ 100
                                  (lambda ()
                                    (cond ((or (ignore-errors (funcall condition))
                                               (when (> (get-internal-real-time) deadline)
                                                 ;; Going on anyway: a wait that always times
                                                 ;; out only slows the run, so say so.
                                                 (format t "~&(waited the full ~d s, as ~a never held)~%"
                                                         timeout where)
                                                 t))
                                           (run-step fn)
                                           (run-steps (rest steps))
                                           nil)
                                          (t t))))))
            (glib:timeout-add glib:+priority-default+ (car step)
                              (lambda ()
                                (run-step (cdr step))
                                (run-steps (rest steps))
                                nil))))))
