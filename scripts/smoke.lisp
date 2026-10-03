;;;; smoke.lisp — drive a real Cadre window through the M0 features:
;;;;   make smoke
;;;; Uses a throwaway project and config directory, checks each step, saves
;;;; screenshots to build/smoke/, prints a report, and exits 0 if all passed.

(push (truename ".") asdf:*central-registry*)
(push (truename "../gtk4/") asdf:*central-registry*)
(ql:quickload :cadre :silent t)

(defpackage #:cadre-smoke (:use #:cl #:cadre #:cadre-ui))
(in-package #:cadre-smoke)

(defvar *out* (merge-pathnames "build/smoke/" (truename ".")))
(defvar *root* (merge-pathnames (format nil "cadre-smoke-~d/" (get-universal-time))
                                (uiop:temporary-directory)))
(defvar *results* '())
(defvar *steps* '())

(defun check (name ok &optional detail)
  (push (list name (and ok t) detail) *results*)
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok name detail))

(defvar *paintable* nil)

(defun screenshot (name)
  "Render the window to build/smoke/NAME.png."
  (let* ((window (cadre-ui::window-gtk-window *window*))
         (w (gtk:widget-get-width window))
         (h (gtk:widget-get-height window))
         (paintable (or *paintable* (setf *paintable* (gtk:widget-paintable-new window))))
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
           (keyval (gdk:keyval-from-name (cond ((string= name "TAB") "Tab")
                                               ((string= name "RET") "Return")
                                               ((string= name "ESC") "Escape")
                                               (t name))))
           (state (loop for m in mods
                        collect (ecase m (#\C :control-mask) (#\M :alt-mask)
                                  (#\s :super-mask) (#\S :shift-mask)))))
      (cadre-ui::handle-key *window* keyval state))))

(defun tab-titles ()
  (mapcar #'adw:tab-page-get-title (cadre-ui::window-pages *window*)))

(defmacro then (delay &body body)
  `(push (cons ,delay (lambda () ,@body)) *steps*))

(defun finish ()
  (let ((failed (count nil *results* :key #'second)))
    (format t "~&~%~d checks, ~d failed. Screenshots in ~a~%" (length *results*) failed *out*)
    (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore)
    (uiop:quit (if (zerop failed) 0 1))))

(defun run-steps (steps)
  (if (null steps)
      (finish)
      (destructuring-bind ((delay . fn) . rest) steps
        (glib:timeout-add glib:+priority-default+ delay
                          (lambda ()
                            (handler-case (funcall fn)
                              (error (e) (check "step ran without error" nil (princ-to-string e))))
                            (run-steps rest)
                            nil)))))

;;; A throwaway project
(ensure-directories-exist (merge-pathnames "src/" *root*))
(with-open-file (o (merge-pathnames "src/hello.lisp" *root*) :direction :output)
  (format o "(defun hello (name)~%  (format t \"Hello, ~~a!~~%\" name))~%"))
(with-open-file (o (merge-pathnames "notes.txt" *root*) :direction :output)
  (format o "Some notes.~%"))
(with-open-file (o (merge-pathnames "cache.fasl" *root*) :direction :output)
  (format o "hidden"))

;;; The steps
(then 1500
  (check "window is the frontend" (eq *frontend* *window*))
  (check "project is open" (equal (truename (cadre-ui::window-project *window*)) (truename *root*)))
  (check "no tabs at start" (null (tab-titles)))
  (check "starts in the horizontal layout"
         (eq :vertical (gtk:orientable-get-orientation (cadre-ui::window-main-paned *window*))))
  (screenshot "01-start")
  (open-file-path (merge-pathnames "src/hello.lisp" *root*))
  (open-file-path (merge-pathnames "notes.txt" *root*)))

(then 1000
  (check "two tabs open" (equal '("hello.lisp" "notes.txt") (tab-titles)) (tab-titles))
  (check "opening an open file reuses its tab"
         (progn (open-file-path (merge-pathnames "notes.txt" *root*))
                (= 2 (length (tab-titles)))))
  (press "C-S-TAB")
  (check "previous-tab selects hello.lisp"
         (string= "hello.lisp" (buffer-name (view-buffer (current-view)))))
  (check "hello.lisp is in Lisp mode" (eq 'lisp-mode (buffer-major-mode (current-buffer))))
  (let ((buffer (current-buffer)))
    (buffer-insert buffer (format nil ";;; Edited by the smoke test~%") 0)
    (check "editing marks the buffer modified" (buffer-modified-p buffer))
    (check "the tab shows unsaved changes" (string= "hello.lisp ●" (first (tab-titles))) (tab-titles)))
  )

(then 500
  (screenshot "02-tabs")                ; after GTK has drawn the step before
  (press "C-s"))                        ; save through the keymap

(then 1000
  (let ((buffer (current-buffer)))
    (check "C-s saved the buffer" (not (buffer-modified-p buffer)))
    (check "the file on disk has the edit"
           (search ";;; Edited by the smoke test"
                   (uiop:read-file-string (merge-pathnames "src/hello.lisp" *root*))))
    (check "the tab no longer shows changes" (string= "hello.lisp" (first (tab-titles))))))

(then 300
  (press "C-k C-l"))                    ; toggle-layout, a two-key sequence

(then 800
  (check "C-k C-l switched to the vertical layout"
         (eq :horizontal (gtk:orientable-get-orientation (cadre-ui::window-main-paned *window*))))
  (check "the panel is beside the editor, wide"
         (> (- (gtk:widget-get-width (cadre-ui::window-main-paned *window*))
               (gtk:paned-get-position (cadre-ui::window-main-paned *window*)))
            300))
  )

(then 300
  (screenshot "03-vertical")
  (press "C-k C-l"))

(then 800
  (check "and back to horizontal"
         (eq :vertical (gtk:orientable-get-orientation (cadre-ui::window-main-paned *window*))))
  (press "C-b")
  (check "C-b hides the sidebar" (not (gtk:widget-get-visible (cadre-ui::window-sidebar *window*))))
  (press "C-b")
  (check "C-b shows it again" (gtk:widget-get-visible (cadre-ui::window-sidebar *window*)))
  (press "C-k C-q")
  (check "an undefined sequence is reported"
         (search "C-k C-q is undefined"
                 (gtk:label-get-text (cadre-ui::window-status-message *window*)))))

(then 300
  (press "C-w"))                        ; close hello.lisp (saved, so no question)

(then 800
  (check "C-w closed the tab" (equal '("notes.txt") (tab-titles)) (tab-titles))
  (check "closing the last view killed the buffer"
         (null (find "hello.lisp" (buffer-list) :key #'buffer-name :test #'string=)))
  (press "C-n"))

(then 800
  (check "C-n opened an untitled buffer"
         (equal '("notes.txt" "untitled") (tab-titles)) (tab-titles))
  (setf cadre-ui::*keybinding-profile* :emacs)
  (let ((buffer (current-buffer)))
    (buffer-insert buffer "abc" 0)
    (setf (buffer-point buffer) 0)
    (press "C-f C-f")
    (check "Emacs C-f moves forward" (= 2 (buffer-point buffer)) (buffer-point buffer))
    (press "C-e")
    (check "Emacs C-e moves to the end of the line" (= 3 (buffer-point buffer)))
    (setf (buffer-modified-p buffer) nil))
  (setf cadre-ui::*keybinding-profile* :standard))

(then 300
  (screenshot "04-end"))

(setf *steps* (reverse *steps*))

;;; Run, with a fresh config directory so first-run questions are skipped.
(let ((config (merge-pathnames (format nil "cadre-smoke-config-~d/" (get-universal-time))
                                (uiop:temporary-directory))))
  (ensure-directories-exist (merge-pathnames "cadre/" config))
  (with-open-file (o (merge-pathnames "cadre/settings.sexp" config) :direction :output)
    (prin1 '(:keybinding-profile :standard) o))
  (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config) 1))

(glib:timeout-add glib:+priority-default+ 100 (lambda () (run-steps *steps*) nil))
(cadre-ui:main :project *root* :init-file nil :quit-after 60)
(format t "~&Timed out.~%")
(uiop:quit 1)
