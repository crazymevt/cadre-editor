;;;; commands.lisp — the M0 commands: files, tabs, layout, and basic editing

(in-package #:cadre-ui)

(defun call-command (name)
  "Run the command NAME as the user would: report editor errors as
messages, and other errors as messages with details in the Output panel."
  (handler-case (run-command name)
    (editor-error (e) (message "~a" (editor-error-message e)))
    (error (e)
      (message "Error in ~(~a~): ~a" name e)
      nil)))

(defun current-view ()
  "The view with the keyboard focus (a tab or the REPL), else the selected
tab's. Signals an editor-error if there is none."
  (or (and *window* (focused-view *window*))
      (and *window* (selected-view *window*))
      (editor-error "No file is open.")))

(defun current-tab-view ()
  "The view in the selected tab. Signals an editor-error if there is none."
  (or (and *window* (selected-view *window*))
      (editor-error "No file is open.")))

;;; Reading and writing files

(defun decode-file-contents (octets)
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (sb-ext:octets-to-string octets :external-format :latin-1))))

(defun open-file-path (pathname &key then)
  "Open the file at PATHNAME in a tab, or select its tab if it is open.
THEN, if given, is called with the view once the file is showing."
  (let ((existing (find-file-buffer pathname)))
    (if existing
        (let ((view (show-buffer *window* existing)))
          (when then (funcall then view)))
        (let ((file (gio:file-new-for-path (uiop:native-namestring pathname))))
          (gio:async (gio:file-load-contents-async file)
                     (lambda (ok contents etag)
                       (declare (ignore ok etag))
                       ;; It may have been opened while this was loading.
                       (let ((buffer (or (find-file-buffer pathname)
                                         (make-buffer :file pathname
                                                      :text (make-gtk-text
                                                             (decode-file-contents
                                                              (coerce contents '(vector (unsigned-byte 8)))))))))
                         (let ((view (show-buffer *window* buffer)))
                           (when then (funcall then view)))))
                     :error (lambda (e)
                              (message "Could not open ~a: ~a" (uiop:native-namestring pathname)
                                       (glib:glib-error-message e))))))))

(defun write-buffer (buffer pathname continuation)
  "Write BUFFER to PATHNAME, then call CONTINUATION with t, or nil on failure."
  (run-hook '*before-save-hook* buffer)
  (let ((octets (sb-ext:string-to-octets (buffer-string buffer) :external-format :utf-8))
        (file (gio:file-new-for-path (uiop:native-namestring pathname))))
    (gio:async (gio:file-replace-contents-async file octets nil nil '(:none))
               (lambda (ok etag)
                 (declare (ignore ok etag))
                 (let ((renamed (not (equal (buffer-file buffer) pathname))))
                   (setf (buffer-file buffer) pathname
                         (buffer-modified-p buffer) nil)
                   (when renamed
                     (setf (buffer-name buffer) (cadre::unique-buffer-name
                                                 (file-namestring pathname))
                           (buffer-major-mode buffer) (major-mode-for-file pathname))
                     (attach-syntax buffer)
                     (update-tab-titles *window* buffer)
                     (update-status *window*)))
                 (run-hook '*after-save-hook* buffer)
                 (message "Saved ~a" (uiop:native-namestring pathname))
                 (funcall continuation t))
               :error (lambda (e)
                        (message "Could not save ~a: ~a" (uiop:native-namestring pathname)
                                 (glib:glib-error-message e))
                        (funcall continuation nil)))))

(defun save-buffer-to-chosen-file (win buffer continuation)
  (let ((dialog (gtk:file-dialog-new)))
    (gtk:file-dialog-set-initial-name dialog (if (buffer-file buffer)
                                                 (file-namestring (buffer-file buffer))
                                                 "untitled.lisp"))
    (when (window-project win)
      (gtk:file-dialog-set-initial-folder
       dialog (gio:file-new-for-path (uiop:native-namestring (window-project win)))))
    (gio:async (gtk:file-dialog-save dialog (window-gtk-window win))
               (lambda (file)
                 (write-buffer buffer (pathname (gio:file-get-path file)) continuation))
               :error (lambda (e)
                        (declare (ignore e))
                        (funcall continuation nil)))))

(defun save-one (win buffer continuation)
  (if (buffer-file buffer)
      (write-buffer buffer (buffer-file buffer) continuation)
      (progn (show-buffer win buffer)
             (save-buffer-to-chosen-file win buffer continuation))))

(defun save-buffers (win buffers continuation)
  "Save BUFFERS one after another; call CONTINUATION with t if all were saved."
  (if (null buffers)
      (funcall continuation t)
      (save-one win (first buffers)
                (lambda (ok)
                  (if ok
                      (save-buffers win (rest buffers) continuation)
                      (funcall continuation nil))))))

(defun open-project (directory)
  "Show DIRECTORY in the explorer and remember it for next time."
  (set-window-project *window* directory)
  (setf (setting :last-project) (uiop:native-namestring (window-project *window*))))

;;; Files

(define-command new-file ()
  "Open a new, empty buffer in a tab."
  (show-buffer *window* (make-buffer :name "untitled" :text (make-gtk-text))))

(define-command open-file ()
  "Choose a file and open it in a tab."
  (let ((dialog (gtk:file-dialog-new)))
    (when (window-project *window*)
      (gtk:file-dialog-set-initial-folder
       dialog (gio:file-new-for-path (uiop:native-namestring (window-project *window*)))))
    (gio:async (gtk:file-dialog-open dialog (window-gtk-window *window*))
               (lambda (file) (open-file-path (pathname (gio:file-get-path file))))
               :error (lambda (e) (declare (ignore e))))))

(define-command open-folder ()
  "Choose a folder and show it in the explorer."
  (gio:async (gtk:file-dialog-select-folder (gtk:file-dialog-new) (window-gtk-window *window*))
             (lambda (file) (open-project (pathname (gio:file-get-path file))))
             :error (lambda (e) (declare (ignore e)))))

(define-command save-buffer ()
  "Save the current buffer to its file, asking for a name if it has none."
  (let ((buffer (view-buffer (current-tab-view))))
    (if (or (buffer-modified-p buffer) (null (buffer-file buffer)))
        (save-one *window* buffer (lambda (ok) (declare (ignore ok))))
        (message "No changes to save"))))

(define-command save-buffer-as ()
  "Save the current buffer to a file you choose."
  (save-buffer-to-chosen-file *window* (view-buffer (current-tab-view))
                              (lambda (ok) (declare (ignore ok)))))

(define-command save-all ()
  "Save every buffer with unsaved changes."
  (let ((modified (remove-if-not #'buffer-modified-p (buffer-list))))
    (if modified
        (save-buffers *window* modified (lambda (ok) (declare (ignore ok))))
        (message "No changes to save"))))

;;; Tabs

(define-command close-tab ()
  "Close the selected tab, asking about unsaved changes."
  (let ((view (current-tab-view)))
    (adw:tab-view-close-page (window-tab-view *window*) (view-page *window* view))))

(define-command next-tab ()
  "Select the tab to the right, wrapping around."
  (let ((tabs (window-tab-view *window*)))
    (unless (adw:tab-view-select-next-page tabs)
      (when (window-pages *window*)
        (adw:tab-view-set-selected-page tabs (first (window-pages *window*)))))
    (when (selected-view *window*) (focus-view (selected-view *window*)))))

(define-command previous-tab ()
  "Select the tab to the left, wrapping around."
  (let ((tabs (window-tab-view *window*)))
    (unless (adw:tab-view-select-previous-page tabs)
      (when (window-pages *window*)
        (adw:tab-view-set-selected-page tabs (car (last (window-pages *window*))))))
    (when (selected-view *window*) (focus-view (selected-view *window*)))))

;;; The window

(define-command toggle-sidebar ()
  "Show or hide the sidebar."
  (set-sidebar-visible *window* (not (gtk:widget-get-visible (window-sidebar *window*)))))

(define-command show-explorer ()
  "Show the explorer in the sidebar."
  (show-sidebar-page *window* "explorer" :toggle nil))

(define-command toggle-panel ()
  "Show or hide the panel."
  (set-panel-visible *window* (not (panel-visible-p *window*))))

(define-command toggle-layout ()
  "Switch between the panel below the editor and the panel beside it."
  (let ((new (if (eq (window-layout *window*) :vertical) :horizontal :vertical)))
    (setf *layout* new
          (setting :layout) new)
    (unless (panel-visible-p *window*) (set-panel-visible *window* t))
    (apply-layout *window*)
    (message "~:[Horizontal~;Vertical~] layout" (eq new :vertical))))

(define-command use-automatic-layout ()
  "Choose the layout by window width: vertical for wide windows."
  (setf *layout* :auto
        (setting :layout) :auto)
  (apply-layout *window*)
  (message "Automatic layout: vertical when the window is at least ~d pixels wide"
           *auto-vertical-min-width*))

(define-command show-output ()
  "Show the Output page of the panel."
  (set-panel-visible *window* t)
  (gtk:stack-set-visible-child-name (panel-stack (window-panel *window*)) "output"))

(define-command use-standard-keys ()
  "Use the standard (VS Code style) keyboard shortcuts."
  (set-keybinding-profile :standard))

(define-command use-emacs-keys ()
  "Use Emacs keyboard shortcuts."
  (set-keybinding-profile :emacs))

(define-command keyboard-quit ()
  "Cancel: close the find bar or a picker, and clear the selection."
  (when (window-picker-object *window*)
    (close-picker (window-picker-object *window*)))
  (when (find-bar-open-p (window-find-bar *window*))
    (find-close (window-find-bar *window*)))
  (let ((view (and *window* (selected-view *window*))))
    (when view
      (setf (buffer-local (view-buffer view) :mark-active) nil)
      (let ((buffer (view-gtk-buffer view)))
        (gtk:text-buffer-place-cursor buffer (cursor-iter buffer)))))
  (message "Quit"))

(define-command quit ()
  "Close the window, asking about unsaved changes."
  (gtk:window-close (window-gtk-window *window*)))

;;; Editing
;;;
;;; GtkTextView's own keyboard handling does most editing. These commands
;;; exist for keys that GtkTextView does not bind, mostly in the Emacs
;;; profile. Movement goes through GtkTextView's :move-cursor signal, so it
;;; behaves as the arrow keys do.

(defun move (step count)
  (let ((view (current-view)))
    (gobject:emit (view-text-view view) :move-cursor step count
                  (and (buffer-local (view-buffer view) :mark-active) t))
    (scroll-to-cursor view)))

(define-command forward-char () "Move forward one character." (move :logical-positions 1))
(define-command backward-char () "Move back one character." (move :logical-positions -1))
(define-command next-line () "Move down one line." (move :display-lines 1))
(define-command previous-line () "Move up one line." (move :display-lines -1))
(define-command forward-word () "Move forward one word." (move :words 1))
(define-command backward-word () "Move back one word." (move :words -1))
(define-command beginning-of-line () "Move to the start of the line." (move :paragraph-ends -1))
(define-command end-of-line () "Move to the end of the line." (move :paragraph-ends 1))
(define-command scroll-down-page () "Move down one screen." (move :pages 1))
(define-command scroll-up-page () "Move up one screen." (move :pages -1))
(define-command beginning-of-buffer () "Move to the start of the buffer." (move :buffer-ends -1))
(define-command end-of-buffer () "Move to the end of the buffer." (move :buffer-ends 1))

(define-command set-mark ()
  "Start selecting: movement extends the selection until Quit (C-g)."
  (let ((buffer (view-buffer (current-view))))
    (setf (buffer-local buffer :mark-active) t)
    (message "Mark set")))

(define-command delete-char ()
  "Delete the character after the cursor."
  (gobject:emit (view-text-view (current-view)) :delete-from-cursor :chars 1))

(define-command kill-line ()
  "Cut from the cursor to the end of the line, or the line break if at the end."
  (let* ((view (current-view))
         (buffer (view-gtk-buffer view))
         (start (cursor-iter buffer))
         (end (cursor-iter buffer)))
    (if (gtk:text-iter-ends-line end)
        (gtk:text-iter-forward-char end)
        (gtk:text-iter-forward-to-line-end end))
    (gtk:text-buffer-select-range buffer start end)
    (gobject:emit (view-text-view view) :cut-clipboard)))

(define-command cut ()
  "Cut the selection to the clipboard."
  (setf (buffer-local (view-buffer (current-view)) :mark-active) nil)
  (gobject:emit (view-text-view (current-view)) :cut-clipboard))

(define-command copy ()
  "Copy the selection to the clipboard."
  (let ((view (current-view)))
    (setf (buffer-local (view-buffer view) :mark-active) nil)
    (gobject:emit (view-text-view view) :copy-clipboard)
    (let ((buffer (view-gtk-buffer view)))
      (gtk:text-buffer-place-cursor buffer (cursor-iter buffer)))))

(define-command paste ()
  "Paste from the clipboard."
  (gobject:emit (view-text-view (current-view)) :paste-clipboard))

(define-command undo ()
  "Undo the last change."
  (let ((buffer (view-gtk-buffer (current-view))))
    (if (gtk:text-buffer-get-can-undo buffer)
        (gtk:text-buffer-undo buffer)
        (message "Nothing to undo"))))

(define-command redo ()
  "Redo the last undone change."
  (let ((buffer (view-gtk-buffer (current-view))))
    (if (gtk:text-buffer-get-can-redo buffer)
        (gtk:text-buffer-redo buffer)
        (message "Nothing to redo"))))
