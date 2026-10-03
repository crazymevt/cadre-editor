;;;; settings-page.lisp — the settings page: every option, searchable
;;;;
;;;; One page per option category, built from the options' types
;;;; (OPTION-KIND): switches, number fields, choices and text fields, and a
;;;; Lisp field for anything else. Changes apply at once where they can
;;;; (*OPTION-APPLIERS*), are saved to settings.sexp, and a reset button
;;;; puts an option back to its default.

(in-package #:cadre-ui)

(defun apply-layout-option () (apply-layout *window*))
(defun apply-paredit-option ()
  (dolist (buffer (buffer-list))
    (when (member (buffer-major-mode buffer) *paredit-major-modes*)
      (set-minor-mode buffer 'paredit-mode *paredit*)))
  (update-status *window*))
(defun apply-keybinding-option () (when *keybinding-profile* (set-keybinding-profile *keybinding-profile*)))
(defun refresh-explorer ()
  (when (window-project *window*) (set-window-project *window* (window-project *window*))))

(defparameter *option-appliers*
  '((*editor-font* . install-font-css)
    (*layout* . apply-layout-option)
    (*color-scheme* . apply-theme) (*light-theme* . apply-theme) (*dark-theme* . apply-theme)
    (*keybinding-profile* . apply-keybinding-option)
    (*paredit* . apply-paredit-option)
    (*highlight-from-image* . image-changed)
    (*explorer-hidden-names* . refresh-explorer) (*explorer-hidden-types* . refresh-explorer))
  "Option name → a function that puts a new value into effect. Other options
take effect the next time they are used.")

(defparameter *restart-options* '(*auto-vertical-min-width*)
  "Options that take effect when Cadre next starts.")

(defparameter *category-icons*
  '(("Appearance" . "cadre-appearance-symbolic") ("Editing" . "cadre-edit-symbolic")
    ("Keyboard" . "cadre-keyboard-symbolic") ("Lisp" . "cadre-system-symbolic")
    ("Claude" . "cadre-claude-symbolic") ("Explorer" . "folder-symbolic")
    ("Session" . "cadre-session-symbolic") ("Other" . "cadre-other-symbolic")))

(defparameter *category-order* '("Appearance" "Editing" "Keyboard" "Lisp" "Claude" "Explorer" "Session" "Other"))

(defun option-changed (option)
  "OPTION was set from the page: save it and put it into effect."
  (let ((name (option-name option)))
    (save-option name)
    (let ((applier (cdr (assoc name *option-appliers*))))
      (when applier
        (handler-case (funcall applier)
          (error (e) (message "Could not apply ~a: ~a" (option-title option) e)))))
    (message "~a: ~a~:[~; (after a restart)~]" (option-title option) (write-option-value option)
             (member name *restart-options*))))

(defun option-choices (option)
  "The values OPTION can take, as (value . label), or nil for free text."
  (case (option-name option)
    (*light-theme* (mapcar (lambda (th) (cons (theme-name th) (theme-title th))) (theme-choices-for nil)))
    (*dark-theme* (mapcar (lambda (th) (cons (theme-name th) (theme-title th))) (theme-choices-for t)))
    (*claude-model* (mapcar (lambda (m) (cons m m))
                            (remove-duplicates (cons *claude-model* (copy-list *claude-models*)) :test #'equal)))
    (t (let ((kind (option-kind option)))
         (when (and (consp kind) (eq (first kind) :choice))
           (mapcar (lambda (v) (cons v (if v (string-capitalize (princ-to-string v)) "Not set")))
                   (second kind)))))))

(defun option-subtitle (option)
  (let ((doc (or (option-documentation option) "")))
    (format nil "~a~:[~;~%Takes effect after a restart.~]"
            (substitute #\Space #\Newline doc) (member (option-name option) *restart-options*))))

(defun markup-escape (string)
  (with-output-to-string (out)
    (loop for c across string
          do (case c (#\< (write-string "&lt;" out)) (#\> (write-string "&gt;" out))
               (#\& (write-string "&amp;" out)) (t (write-char c out))))))

(defun make-option-row (option)
  "A row editing OPTION, and a function that shows its current value again."
  (let* ((title (option-title option))
         (subtitle (markup-escape (option-subtitle option)))
         (choices (option-choices option))
         (kind (option-kind option))
         (updating nil))
    (macrolet ((quietly (&body body)
                 ;; Showing the value must not count as the user changing it.
                 `(progn (setf updating t) (unwind-protect (progn ,@body) (setf updating nil)))))
     (flet ((guarded (fn) (lambda (&rest args) (declare (ignore args)) (unless updating (funcall fn)))))
      (cond
        ((eq kind :boolean)
         (let ((row (make-instance 'adw:switch-row :title title :subtitle subtitle)))
           (gobject:connect row "notify::active"
                            (guarded (lambda ()
                                       (set-option-value option (adw:switch-row-get-active row))
                                       (option-changed option))))
           (values row (lambda () (quietly (adw:switch-row-set-active row (and (option-value option) t)))))))
        (choices
         (let ((row (make-instance 'adw:combo-row :title title :subtitle subtitle
                                                  :model (gtk:string-list-new (mapcar #'cdr choices)))))
           (gobject:connect row "notify::selected"
                            (guarded (lambda ()
                                       (let ((choice (nth (adw:combo-row-get-selected row) choices)))
                                         (when choice
                                           (set-option-value option (car choice))
                                           (option-changed option))))))
           (values row (lambda ()
                         (let ((i (position (option-value option) choices :key #'car :test #'equal)))
                           (when i (quietly (adw:combo-row-set-selected row i))))))))
        ((and (consp kind) (eq (first kind) :integer))
         (let ((row (adw:spin-row-new-with-range (float (or (second kind) 0) 1d0)
                                                 (float (or (third kind) 1000000) 1d0) 1d0)))
           (adw:preferences-row-set-title row title)
           (adw:action-row-set-subtitle row subtitle)
           (gobject:connect row "notify::value"
                            (guarded (lambda ()
                                       (set-option-value option (round (adw:spin-row-get-value row)))
                                       (option-changed option))))
           (values row (lambda () (quietly (adw:spin-row-set-value row (float (option-value option) 1d0)))))))
        (t
         (let ((row (make-instance 'adw:entry-row :title (format nil "~a~:[~; (Lisp)~]" title (eq kind :lisp))
                                                  :show-apply-button t :tooltip-text (option-subtitle option))))
           (gobject:connect row :apply
                            (lambda (r)
                              (declare (ignore r))
                              (handler-case
                                  (progn (set-option-value option (read-option-value option (gtk:editable-get-text row)))
                                         (gtk:widget-remove-css-class row "error")
                                         (option-changed option))
                                (editor-error (e)
                                  (gtk:widget-add-css-class row "error")
                                  (message "~a" (editor-error-message e))))))
           (values row (lambda () (quietly
                                    (gtk:editable-set-text row (write-option-value option))
                                    (gtk:widget-remove-css-class row "error")))))))))))

(defun add-reset-button (row option refresh)
  (let ((button (make-instance 'gtk:button :icon-name "cadre-undo-symbolic" :valign :center
                                           :tooltip-text "Back to the default" :css-classes '("flat"))))
    (gobject:connect button :clicked
                     (lambda (b) (declare (ignore b))
                       (set-option-value option (option-default option))
                       (forget-option (option-name option))
                       (funcall refresh)
                       (let ((applier (cdr (assoc (option-name option) *option-appliers*))))
                         (when applier (funcall applier)))
                       (message "~a is back to its default" (option-title option))))
    (if (typep row 'adw:entry-row)
        (adw:entry-row-add-suffix row button)
        (adw:action-row-add-suffix row button))))

(defun make-files-group ()
  (gtk:build
    (adw:preferences-group :title "Files"
                           :description "Options set in init.lisp apply at startup; choices made here are applied after it."
      (adw:button-row :title "Open init.lisp" :start-icon-name "cadre-edit-symbolic"
                      :on-activated (lambda (r) (declare (ignore r)) (call-command 'open-init-file)))
      (adw:button-row :title "Choose a color theme…" :start-icon-name "cadre-appearance-symbolic"
                      :on-activated (lambda (r) (declare (ignore r)) (call-command 'choose-theme))))))

(defvar *settings-dialog* nil)

(defun make-settings-dialog ()
  (let ((dialog (make-instance 'adw:preferences-dialog :title "Settings" :search-enabled t
                                                       :content-width 720 :content-height 640))
        (by-category (make-hash-table :test 'equal)))
    (dolist (option (list-options))
      (push option (gethash (or (option-category option) "Other") by-category)))
    (dolist (category (append *category-order*
                              (sort (set-difference (loop for k being the hash-keys of by-category collect k)
                                                    *category-order* :test #'equal)
                                    #'string<)))
      (let ((options (gethash category by-category)))
        (when (or options (string= category "Other"))
          (let ((page (make-instance 'adw:preferences-page :title category
                                                           :icon-name (or (cdr (assoc category *category-icons* :test #'equal))
                                                                          "cadre-other-symbolic")))
                (group (make-instance 'adw:preferences-group)))
            (dolist (option (sort (copy-list options) #'string< :key #'option-title))
              (multiple-value-bind (row refresh) (make-option-row option)
                (funcall refresh)
                (add-reset-button row option refresh)
                (adw:preferences-group-add group row)))
            (adw:preferences-page-add page group)
            (when (string= category "Other") (adw:preferences-page-add page (make-files-group)))
            (adw:preferences-dialog-add dialog page)))))
    dialog))

(define-command settings ()
  "Show the settings: every option, by category, searchable."
  ;; Built afresh each time, so it shows options defined since (in init.lisp, say).
  (setf *settings-dialog* (make-settings-dialog))
  (adw:dialog-present *settings-dialog* (window-gtk-window *window*)))

(define-command customize ()
  "Show the settings (as M-x customize does in Emacs)."
  (call-command 'settings))

(define-command open-init-file ()
  "Open your init file, ~/.config/cadre/init.lisp, creating it if needed."
  (let ((file (init-file)))
    (unless (probe-file file)
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output)
        (format out ";;; init.lisp — loaded at startup in the CADRE-USER package.~%;;; For example:~%;;;   (setf *editor-font* \"Iosevka 13pt\")~%;;;   (bind-key cadre-ui::*emacs-global-keymap* \"C-c t\" 'cadre-ui::choose-theme)~%")))
    (when *settings-dialog* (adw:dialog-close *settings-dialog*))
    (open-file-path file)))
