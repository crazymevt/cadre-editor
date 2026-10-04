;;;; terminal.lisp — the Terminal page: shells, and Claude Code, in the panel
;;;;
;;;; Each terminal is a VteTerminal (GNOME's terminal widget, libvte for GTK
;;;; 4), loaded when first needed and called through CFFI: the gtk4 bindings
;;;; have no Vte namespace, and Cadre needs only a few dozen calls. VTE's own
;;;; spawning crashes the child on macOS, so the child is started with
;;;; g_spawn_async, with vte_pty_child_setup (a C function) attaching it to
;;;; the pty. The core (terminal.lisp) decides the command, the environment,
;;;; and which keys a terminal gets; HANDLE-KEY asks TERMINAL-HANDLE-KEY first
;;;; when a terminal has the focus.

(in-package #:cadre-ui)

(define-option *terminal-scrollback* 10000 integer
  "Lines each terminal keeps above the screen."
  :category "Terminal")

(define-option *terminal-option-as-meta* t boolean
  "On macOS, Option with a key sends Meta (Esc, then the key), as shells and
Emacs expect, rather than the character Option types (∫ for Option+B)."
  :category "Terminal")

(define-option *terminal-editor-keys* nil list
  "Keys that act as Cadre's commands even when a terminal has the focus
(other keys go to the terminal; on macOS, ⌘ keys are always Cadre's). Nil
means the keybinding profile's: Ctrl+`, Ctrl+~, Ctrl+Shift+P and F1, and in the Emacs
profile also C-x and M-x."
  :category "Terminal")

;;; libvte

(defparameter *vte-library-names*
  '(#+darwin "libvte-2.91-gtk4.0.dylib" #-darwin "libvte-2.91-gtk4.so.0"))

(defvar *vte-loaded* :unknown)
(defvar *vte-symbols* (make-hash-table :test 'equal))

(defun vte-available-p ()
  "Load libvte (for GTK 4) if it is installed; true if it is."
  (when (eq *vte-loaded* :unknown)
    (setf *vte-loaded*
          (loop for name in *vte-library-names*
                thereis (loop for directory in (append gtk4.runtime:*library-directories* (list nil))
                              thereis (ignore-errors
                                       (cffi:load-foreign-library
                                        (if directory (namestring (merge-pathnames name directory)) name))
                                       t)))))
  *vte-loaded*)

(defun vte-symbol (name)
  (or (gethash name *vte-symbols*)
      (setf (gethash name *vte-symbols*)
            (or (cffi:foreign-symbol-pointer name)
                (error "libvte has no ~a" name)))))

(defmacro vte (name return-type &rest arguments)
  "Call the libvte (or GLib) function NAME: arguments as type, value pairs."
  `(gtk4.runtime:with-gtk-float-traps
     (cffi:foreign-funcall-pointer (vte-symbol ,name) () ,@arguments ,return-type)))

(defun vte-missing-message ()
  (format nil "The Terminal needs VTE, GNOME's terminal widget~:[ (libvte for GTK 4, from your distribution)~; (brew install vte3)~]"
          (macos-p)))

(defun take-gerror (place)
  "The message of the GError at PLACE (a GError**), freeing it; nil if none."
  (let ((error (cffi:mem-ref place :pointer)))
    (unless (cffi:null-pointer-p error)
      (prog1 (cffi:mem-ref error :string 8) ; GError: domain, code, message
        (vte "g_error_free" :void :pointer error)))))

(defun foreign-strv (strings)
  "A NULL-terminated array of new C strings; free it with FREE-STRV."
  (let ((array (cffi:foreign-alloc :pointer :count (1+ (length strings)))))
    (loop for s in strings for i from 0
          do (setf (cffi:mem-aref array :pointer i) (cffi:foreign-string-alloc s :encoding :utf-8)))
    (setf (cffi:mem-aref array :pointer (length strings)) (cffi:null-pointer))
    array))

(defun free-strv (array)
  (loop for i from 0
        for p = (cffi:mem-aref array :pointer i)
        until (cffi:null-pointer-p p)
        do (cffi:foreign-string-free p))
  (cffi:foreign-free array))

(defun take-c-string (pointer)
  "The string at POINTER, freed with g_free; nil for NULL."
  (unless (cffi:null-pointer-p pointer)
    (prog1 (cffi:foreign-string-to-lisp pointer :encoding :utf-8)
      (vte "g_free" :void :pointer pointer))))

;;; Terminals

(defstruct (term (:conc-name term-))
  widget pointer pid command title directory (exited nil) button)

(defvar *terminals* '() "The open terminals, oldest first.")
(defvar *current-terminal* nil)
(defvar *terminal-stack* nil "The page's stack: a child per terminal, and \"empty\".")
(defvar *terminal-tabs* nil "The box of the terminals' tab buttons.")
(defvar *terminal-autostart* t
  "Whether opening the Terminal tab with no terminal starts a shell; off
while a command that starts its own terminal shows the page.")

(defparameter *terminal-palettes*
  '((:dark "#000000" "#cd3131" "#0dbc79" "#e5e510" "#2472c8" "#bc3fbc" "#11a8cd" "#e5e5e5"
     "#666666" "#f14c4c" "#23d18b" "#f5f543" "#3b8eea" "#d670d6" "#29b8db" "#ffffff")
    (:light "#000000" "#cd3131" "#00bc00" "#949800" "#0451a5" "#bc05bc" "#0598bc" "#555555"
     "#666666" "#cd3131" "#14ce14" "#b5ba00" "#0451a5" "#bc05bc" "#0598bc" "#a5a5a5"))
  "The 16 ANSI colors, for dark and light themes.")

(defun parse-hex-color (string)
  "STRING, #rrggbb or #rrggbbaa, as four floats from 0 to 1."
  (let ((hex (string-left-trim "#" string)))
    (flet ((part (i) (/ (parse-integer hex :start i :end (+ i 2) :radix 16) 255.0)))
      (list (part 0) (part 2) (part 4) (if (>= (length hex) 8) (part 6) 1.0)))))

(defun set-rgba (pointer index color)
  (loop for value in (parse-hex-color color) for j from 0
        do (setf (cffi:mem-aref pointer :float (+ (* 4 index) j)) value)))

(defun terminal-colors ()
  "The current theme's foreground, background (transparent, showing the
panel, when the theme leaves the editor's to GTK) and palette."
  (let* ((theme (current-theme))
         (dark (adw:dark-p))
         (background (loop for th = theme then (theme-parent th) while th thereis (theme-background th)))
         (foreground (loop for th = theme then (theme-parent th) while th thereis (theme-foreground th))))
    (values (or foreground (if dark "#e6e6e6" "#2e2e2e"))
            (or background "#00000000")
            (cdr (assoc (if dark :dark :light) *terminal-palettes*)))))

(defun style-terminal (term)
  (multiple-value-bind (foreground background palette) (terminal-colors)
    (cffi:with-foreign-objects ((fg :float 4) (bg :float 4) (colors :float (* 4 16)))
      (set-rgba fg 0 foreground)
      (set-rgba bg 0 background)
      (loop for color in palette for i from 0 do (set-rgba colors i color))
      (vte "vte_terminal_set_colors" :void :pointer (term-pointer term) :pointer fg :pointer bg
                                           :pointer colors :size 16)))
  (vte "vte_terminal_set_scrollback_lines" :void :pointer (term-pointer term) :long *terminal-scrollback*))

(defun restyle-terminals ()
  "Give every terminal the current theme's colors (after the theme changes)."
  (dolist (term *terminals*)
    (style-terminal term)))

(defparameter *terminal-link-patterns*
  '(;; URLs
    "(?:https?|file)://[^\\s<>\"'`]*[^\\s<>\"'`.,;:!?)\\]]"
    ;; Files, perhaps with :line and :column, as compilers print them
    "(?:~|\\.{1,2})?/?(?:[\\w@+-][\\w.@+-]*/)*[\\w@+-][\\w.@+-]*\\.[A-Za-z][A-Za-z0-9]*(?::\\d+){0,2}")
  "What ⌘-click (Ctrl+click) opens in a terminal, as PCRE2 patterns.")

(defun add-terminal-links (term)
  (cffi:with-foreign-object (error :pointer)
    (dolist (pattern *terminal-link-patterns*)
      (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
      (let ((regex (vte "vte_regex_new_for_match" :pointer :string pattern :ssize -1
                                                           :uint32 (logior #x00080000 #x00000400) ; UTF, MULTILINE
                                                           :pointer error)))
        (if (cffi:null-pointer-p regex)
            (message "Terminal link pattern: ~a" (take-gerror error))
            (let ((tag (vte "vte_terminal_match_add_regex" :int :pointer (term-pointer term) :pointer regex :uint32 0)))
              (vte "vte_terminal_match_set_cursor_name" :void :pointer (term-pointer term) :int tag :string "pointer")
              (vte "vte_regex_unref" :void :pointer regex)))))))

(defun terminal-context-menu ()
  (let ((menu (gio:menu-new))
        (edit (gio:menu-new))
        (terminal (gio:menu-new)))
    (flet ((item (section label command)
             (gio:menu-append section label (format nil "app.command('~(~a~)')" command))))
      (item edit "Copy" 'terminal-copy)
      (item edit "Paste" 'terminal-paste)
      (item edit "Select All" 'terminal-select-all)
      (item terminal "Clear" 'clear-terminal)
      (item terminal "New Terminal" 'new-terminal)
      (item terminal "Kill Terminal" 'kill-terminal))
    (gio:menu-append-section menu nil edit)
    (gio:menu-append-section menu nil terminal)
    menu))

(defun spawn-in-terminal (term)
  "Start TERM's command on a new pty in TERM. Returns the pid, or signals an editor-error."
  (cffi:with-foreign-objects ((error :pointer) (pid :int))
    (setf (cffi:mem-ref error :pointer) (cffi:null-pointer))
    (let ((pty (vte "vte_pty_new_sync" :pointer :int 0 :pointer (cffi:null-pointer) :pointer error)))
      (when (cffi:null-pointer-p pty)
        (editor-error "Couldn't make a terminal: ~a" (take-gerror error)))
      (vte "vte_terminal_set_pty" :void :pointer (term-pointer term) :pointer pty)
      (let ((argv (foreign-strv (term-command term)))
            (envp (foreign-strv (terminal-environment (sb-ext:posix-environ)))))
        (unwind-protect
             (let ((ok (vte "g_spawn_async" :int
                            :string (uiop:native-namestring (term-directory term))
                            :pointer argv :pointer envp
                            :int (logior 2 4) ; G_SPAWN_DO_NOT_REAP_CHILD, G_SPAWN_SEARCH_PATH
                            :pointer (vte-symbol "vte_pty_child_setup") :pointer pty
                            :pointer pid :pointer error)))
               (when (zerop ok)
                 (vte "g_object_unref" :void :pointer pty)
                 (editor-error "Couldn't start ~a: ~a" (first (term-command term)) (take-gerror error))))
          (free-strv argv)
          (free-strv envp)))
      ;; The terminal holds the pty now.
      (vte "g_object_unref" :void :pointer pty)
      (vte "vte_terminal_watch_child" :void :pointer (term-pointer term) :int (cffi:mem-ref pid :int))
      (cffi:mem-ref pid :int))))

(defun default-terminal-directory ()
  (or (window-project *window*)
      (let* ((view (and *window* (selected-view *window*)))
             (file (and view (buffer-file (view-buffer view)))))
        (and file (uiop:pathname-directory-pathname file)))
      (user-homedir-pathname)))

(defun start-terminal (&key (command (terminal-shell-command)) (directory (default-terminal-directory))
                            title (focus t))
  "Open a new terminal running COMMAND (an argument list) in DIRECTORY, and show it."
  (unless (vte-available-p)
    (let ((*terminal-autostart* nil)) (show-terminal-page))
    (editor-error "~a" (vte-missing-message)))
  (let* ((pointer (vte "vte_terminal_new" :pointer))
         (widget (gtk4.runtime:wrap-object pointer))
         (term (make-term :widget widget :pointer pointer :command command
                          :title (or title (terminal-title-for command))
                          :directory (uiop:ensure-directory-pathname directory))))
    (gtk:widget-add-css-class widget "cadre-terminal")
    (gtk:widget-set-hexpand widget t)
    (gtk:widget-set-vexpand widget t)
    (vte "vte_terminal_set_audible_bell" :void :pointer pointer :int 0)
    (vte "vte_terminal_set_mouse_autohide" :void :pointer pointer :int 1)
    (vte "vte_terminal_set_allow_hyperlink" :void :pointer pointer :int 1)
    (vte "vte_terminal_set_scroll_on_keystroke" :void :pointer pointer :int 1)
    (vte "vte_terminal_set_scroll_on_output" :void :pointer pointer :int 0)
    (vte "vte_terminal_set_context_menu_model" :void :pointer pointer
         :pointer (gtk4.runtime:object-pointer (terminal-context-menu)))
    (style-terminal term)
    (add-terminal-links term)
    (gobject:connect widget "child-exited" (lambda (w status) (declare (ignore w)) (terminal-exited term status)))
    (gobject:connect widget "termprop-changed"
                     (lambda (w name) (declare (ignore w))
                       (when (equal name "xterm.title") (terminal-title-changed term))))
    (setup-terminal-clicks term)
    (setf (term-pid term) (spawn-in-terminal term))
    (setf *terminals* (append *terminals* (list term)))
    (gtk:stack-add-named *terminal-stack* widget (format nil "term-~d" (term-pid term)))
    (add-terminal-tab term)
    (select-terminal term :focus focus)
    term))

(defun live-terminals () (remove-if #'term-exited *terminals*))

(defun terminal-title-changed (term)
  (let ((title (vte "vte_terminal_get_window_title" :string :pointer (term-pointer term))))
    (when (plusp (length title))
      (setf (term-title term) title)
      (update-terminal-tab term))))

(defun wait-status-text (status)
  "A wait status as words: the exit code, or the signal that ended it."
  (if (zerop (logand status #x7f))
      (format nil "exited with code ~d" (ash status -8))
      (format nil "ended by signal ~d" (logand status #x7f))))

(defun terminal-exited (term status)
  "TERM's process ended: close it if it ended well, else keep it, saying how it ended."
  (unless (term-exited term)
    (setf (term-exited term) t)
    (if (zerop status)
        (remove-terminal term)
        (let ((text (format nil "~c[0m~c~%[Process ~a]~c~%" #\Esc #\Return (wait-status-text status) #\Return)))
          (cffi:with-foreign-string ((bytes length) text :encoding :utf-8 :null-terminated-p nil)
            (vte "vte_terminal_feed" :void :pointer (term-pointer term) :pointer bytes :ssize length))
          (update-terminal-tab term)
          (message "~a ~a" (term-title term) (wait-status-text status))))))

(defun remove-terminal (term)
  (setf *terminals* (remove term *terminals*))
  (gtk:stack-remove *terminal-stack* (term-widget term))
  (gtk:box-remove *terminal-tabs* (term-button term))
  (when (eq term *current-terminal*)
    (setf *current-terminal* nil)
    (let ((next (car (last *terminals*))))
      (if next
          (select-terminal next :focus (terminal-page-visible-p))
          (gtk:stack-set-visible-child-name *terminal-stack* "empty")))))

(defun kill-term (term)
  "End TERM's process (with SIGHUP, as closing a terminal window does) and close it."
  (unless (term-exited term)
    (setf (term-exited term) t)
    (ignore-errors (sb-posix:kill (term-pid term) sb-posix:sighup)))
  (remove-terminal term))

;;; The page

(defun terminal-tab-label (term)
  (format nil "~a~:[~; (ended)~]" (term-title term) (term-exited term)))

(defun add-terminal-tab (term)
  (let* ((button (make-instance 'gtk:toggle-button :label (terminal-tab-label term)
                                                   :css-classes '("flat" "cadre-terminal-tab")
                                                   :tooltip-text (format nil "~{~a~^ ~}~%in ~a~%Middle-click to kill"
                                                                         (term-command term)
                                                                         (uiop:native-namestring (term-directory term)))))
         (middle (gtk:gesture-click-new)))
    (let ((other (find-if #'term-button (remove term *terminals*))))
      (when other (gtk:toggle-button-set-group button (term-button other))))
    (gobject:connect button :toggled
                     (lambda (b) (when (and (gtk:toggle-button-get-active b) (not (eq term *current-terminal*)))
                                   (select-terminal term :focus t))))
    (gtk:gesture-single-set-button middle 2)
    (gobject:connect middle :pressed (lambda (g n x y) (declare (ignore g n x y)) (kill-term term)))
    (gtk:widget-add-controller button middle)
    (setf (term-button term) button)
    (gtk:box-append *terminal-tabs* button)))

(defun update-terminal-tab (term)
  (when (term-button term)
    (gtk:button-set-label (term-button term) (terminal-tab-label term))))

(defun select-terminal (term &key focus)
  (setf *current-terminal* term)
  (gtk:stack-set-visible-child *terminal-stack* (term-widget term))
  (unless (gtk:toggle-button-get-active (term-button term))
    (gtk:toggle-button-set-active (term-button term) t))
  (when focus (gtk:widget-grab-focus (term-widget term))))

(defun make-terminal-widget ()
  "The Terminal page: the terminals' tabs and buttons above, the current terminal below."
  (let ((stack (make-instance 'gtk:stack :vexpand t :hexpand t))
        (tabs (make-instance 'gtk:box :spacing 2)))
    (setf *terminal-stack* stack
          *terminal-tabs* tabs)
    (gtk:stack-add-named stack
                         (let ((page (placeholder-page "utilities-terminal-symbolic" "No Terminal"
                                                       "A shell in the project's folder")))
                           (adw:status-page-set-child page (let ((button (command-button "list-add-symbolic" "New Terminal" 'new-terminal
                                                                                          :label "New Terminal")))
                                                             (gtk:widget-set-halign button :center)
                                                             (gtk:widget-add-css-class button "pill")
                                                             button))
                           page)
                         "empty")
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :spacing 4 :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2
          (make-instance 'gtk:scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :never
                                              :hexpand t :child tabs)
          (flat-command-button "list-add-symbolic" "New Terminal" 'new-terminal)
          (flat-command-button "cadre-claude-symbolic" "Claude Code in a Terminal" 'claude-code-in-terminal)
          (flat-command-button "user-trash-symbolic" "Kill Terminal" 'kill-terminal))
        stack))))

(defun flat-command-button (icon tooltip command)
  (let ((button (command-button icon tooltip command)))
    (gtk:widget-add-css-class button "flat")
    button))

(defun terminal-page-visible-p ()
  (and *window* (string= (panel-visible-name (window-panel *window*)) "terminal")))

(defun show-terminal-page ()
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "terminal")
  (unless (vte-available-p)
    (let ((page (gtk:stack-get-child-by-name *terminal-stack* "empty")))
      (adw:status-page-set-title page "VTE isn't installed")
      (adw:status-page-set-description page (vte-missing-message))
      (adw:status-page-set-child page nil))))

(defun terminal-page-shown ()
  "The Terminal tab was opened: start a shell if there's none, and focus the terminal."
  (cond ((not (vte-available-p)) (show-terminal-page))
        ((live-terminals) (when *current-terminal* (gtk:widget-grab-focus (term-widget *current-terminal*))))
        ((and (null *terminals*) *terminal-autostart*) (handler-case (start-terminal)
                              (editor-error (e) (message "~a" e))))))

;;; Keys and clicks

(defun focused-terminal (win)
  "The terminal that has the keyboard focus, or nil."
  (let ((focus (gtk:root-get-focus (window-gtk-window win))))
    (and focus (find focus *terminals* :key #'term-widget))))

(defun terminal-editor-keys ()
  (or *terminal-editor-keys*
      (cdr (assoc (or *keybinding-profile* :standard) *terminal-default-editor-keys*))))

(defun terminal-handle-key (term keyval state keycode)
  "Act on a key typed in TERM. Returns :editor when it is a command for
Cadre, t when it was handled here, nil to let the terminal have it."
  (let* ((mods (modifier-list state))
         (meta (and (macos-p) *terminal-option-as-meta* (member :alt-mask mods)
                    (not (member :control-mask mods)) (not (member :super-mask mods))))
         (keyval (or (and meta keycode (base-keyval keycode state)) keyval))
         (key (event-key keyval state))
         (action (and key (terminal-key-action key :macos (macos-p) :editor-keys (terminal-editor-keys)
                                                   :option-as-meta meta))))
    (cond ((null action) nil)
          ((eq action :terminal) nil)
          ((eq action :editor) :editor)
          ((eq action :copy) (terminal-copy-text term) t)
          ((eq action :paste) (terminal-paste-text term) t)
          ((eq action :select-all) (vte "vte_terminal_select_all" :void :pointer (term-pointer term)) t)
          ((eq action :clear) (clear-term term) t)
          ((and (consp action) (eq (first action) :send)) (feed-terminal term (second action)) t))))

(defun feed-terminal (term string)
  "Write STRING to TERM's process, as if typed."
  (cffi:with-foreign-string ((bytes length) string :encoding :utf-8 :null-terminated-p nil)
    (vte "vte_terminal_feed_child" :void :pointer (term-pointer term) :pointer bytes :ssize length)))

(defun terminal-copy-text (term)
  (when (/= 0 (vte "vte_terminal_get_has_selection" :int :pointer (term-pointer term)))
    (vte "vte_terminal_copy_clipboard_format" :void :pointer (term-pointer term) :int 1))) ; VTE_FORMAT_TEXT

(defun terminal-paste-text (term)
  (vte "vte_terminal_paste_clipboard" :void :pointer (term-pointer term)))

(defun clear-term (term)
  "Forget TERM's scrollback and screen, and have the program draw itself again (Ctrl+L)."
  (vte "vte_terminal_reset" :void :pointer (term-pointer term) :int 1 :int 1)
  (feed-terminal term (string (code-char 12))))

(defun terminal-text (term)
  "What TERM shows, as text."
  (take-c-string (vte "vte_terminal_get_text_format" :pointer :pointer (term-pointer term) :int 1)))

(defun terminal-current-directory (term)
  "The folder TERM's shell says it is in (if it reports it, with OSC 7)."
  (let ((uri (vte "vte_terminal_get_current_directory_uri" :string :pointer (term-pointer term))))
    (and uri (uiop:string-prefix-p "file://" uri)
         (let ((path (subseq uri (position #\/ uri :start 7))))
           (uiop:ensure-directory-pathname (cl-ppcre:regex-replace-all "%20" path " "))))))

(defun open-terminal-link (term text)
  "Open TEXT, clicked in TERM: a URL in the browser, a file reference in a tab."
  (if (search "://" text)
      (gio:async (gtk:uri-launcher-launch (gtk:uri-launcher-new text) (window-gtk-window *window*))
                 (lambda (ok) (declare (ignore ok)))
                 :error (lambda (e) (message "Couldn't open ~a: ~a" text e)))
      (multiple-value-bind (file line column) (parse-file-reference text)
        (let ((path (and file (resolve-file-reference
                               file (list (terminal-current-directory term) (term-directory term)
                                          (window-project *window*))))))
          (if path
              (goto-location (list :file (uiop:native-namestring path)
                                   :line (and line (max 0 (1- line)))
                                   :column (and column (max 0 (1- column)))))
              (message "No file ~a" (or file text)))))))

(defun setup-terminal-clicks (term)
  "⌘-click (Ctrl+click elsewhere) on a link or file reference opens it."
  (let ((click (gtk:gesture-click-new)))
    (gtk:gesture-single-set-button click 1)
    (gtk:event-controller-set-propagation-phase click :capture)
    (gobject:connect click :pressed
                     (lambda (gesture n x y)
                       (declare (ignore n))
                       (let ((mods (modifier-list (gtk:event-controller-get-current-event-state gesture))))
                         (when (member (if (macos-p) :super-mask :control-mask) mods)
                           (let ((text (or (take-c-string (vte "vte_terminal_check_hyperlink_at" :pointer
                                                               :pointer (term-pointer term) :double x :double y))
                                           (cffi:with-foreign-object (tag :int)
                                             (take-c-string (vte "vte_terminal_check_match_at" :pointer
                                                                 :pointer (term-pointer term) :double x :double y
                                                                 :pointer tag))))))
                             (when text
                               (gtk:gesture-set-state gesture :claimed)
                               (open-terminal-link term text)))))))
    (gtk:widget-add-controller (term-widget term) click)))

;;; Commands

(defun current-terminal ()
  (let ((term *current-terminal*))
    (or (and term (not (term-exited term)) term)
        (editor-error "No terminal"))))

(define-command show-terminal ()
  "Show the Terminal page and put the cursor in the terminal, starting a shell if there is none."
  (show-terminal-page)
  (terminal-page-shown))

(define-command new-terminal ()
  "Open a new terminal: your shell, in the project's folder."
  (let ((*terminal-autostart* nil)) (show-terminal-page))
  (start-terminal))

(define-command kill-terminal ()
  "End the current terminal's program and close it."
  (let ((term (or *current-terminal* (editor-error "No terminal"))))
    (kill-term term)))

(define-command clear-terminal ()
  "Clear the current terminal and its scrollback."
  (clear-term (current-terminal)))

(define-command terminal-copy ()
  "Copy the current terminal's selection."
  (terminal-copy-text (current-terminal)))

(define-command terminal-paste ()
  "Paste into the current terminal."
  (terminal-paste-text (current-terminal)))

(define-command terminal-select-all ()
  "Select all of the current terminal's text."
  (vte "vte_terminal_select_all" :void :pointer (term-pointer (current-terminal))))

(define-command claude-code-in-terminal ()
  "Run Claude Code itself (its own interface) in a terminal, in the project's
folder, with Cadre's editor and Lisp tools available to it."
  (let ((program (or (find-claude-program)
                     (editor-error "Claude Code isn't installed: see https://claude.com/claude-code, or set *claude-program*"))))
    (ensure-mcp-server)
    (let ((*terminal-autostart* nil)) (show-terminal-page))
    (start-terminal :command (list program "--mcp-config" (uiop:native-namestring *mcp-config*))
                    :title "Claude Code")))

(define-command send-to-terminal ()
  "Run the selection, or the current line, in the current terminal (starting
one if needed), as if typed there."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (text (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
                 (if has
                     (gtk:text-buffer-get-text gtk-buffer start end nil)
                     (current-line-text view))))
         (term (or (first (member *current-terminal* (live-terminals)))
                   (car (last (live-terminals)))
                   (progn (let ((*terminal-autostart* nil)) (show-terminal-page))
                          (start-terminal :focus nil)))))
    (show-terminal-page)
    (select-terminal term)
    (feed-terminal term (format nil "~a~c" (string-right-trim '(#\Newline) text) #\Return))))
