;;;; threads.lisp — the Threads page: the Lisp's threads
;;;;
;;;; Lists the other Lisp's threads (a server's, a GTK app's, background
;;;; work), refreshed every two seconds while the page shows. Debug
;;;; interrupts a thread and opens the debugger in it: its backtrace, its
;;;; locals, and Continue to let it go on. Kill ends it. The threads Swank
;;;; runs for itself are hidden unless asked for.

(in-package #:cadre-ui)

(defstruct (threads-page (:conc-name thp-))
  widget list title show-swank
  (threads '())
  (fetching nil)
  (timer nil))

(defvar *threads-page* nil)

(defparameter *threads-refresh-ms* 2000)

(defun make-threads-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (title (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t :css-classes '("heading")
                                          :label "Threads"))
         (show-swank (make-instance 'gtk:check-button :label "Swank's threads"
                                                      :tooltip-text "Also show the threads Swank runs for itself"))
         (widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:box :spacing 4 :margin-start 10 :margin-end 6 :margin-top 4 :margin-bottom 4
                       title
                       show-swank
                       (gtk:button :icon-name "view-refresh-symbolic" :css-classes '("flat")
                                   :tooltip-text "Refresh"
                                   :on-clicked (lambda (b) (declare (ignore b)) (refresh-threads))))
                     (gtk:separator)
                     (gtk:scrolled-window :vexpand t :child list)))))
    (setf *threads-page* (make-threads-page :widget widget :list list :title title :show-swank show-swank))
    (gobject:connect show-swank :toggled (lambda (b) (declare (ignore b)) (fill-threads-list *threads-page*)))
    (fill-threads-list *threads-page*)
    widget))

(defun thread-row (thread)
  (let* ((box (make-instance 'gtk:box :spacing 8 :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2))
         (row (make-instance 'gtk:list-box-row :child box :activatable nil)))
    (gtk:box-append box (label (lisp-thread-name thread) :hexpand t :ellipsize :end :css-classes '("monospace")))
    (gtk:box-append box (label (format nil "~a~@[ · ~a~]" (lisp-thread-id thread)
                                       (let ((s (lisp-thread-status thread))) (and (plusp (length s)) s)))
                               :css-classes '("dim-label" "numeric")))
    (let ((debug (make-instance 'gtk:button :label "Debug" :css-classes '("flat")
                                            :tooltip-text "Interrupt the thread and open the debugger in it (Continue lets it go on)"))
          (kill (make-instance 'gtk:button :label "Kill" :css-classes '("flat" "destructive-action")
                                           :tooltip-text "End the thread")))
      (gobject:connect debug :clicked (lambda (b) (declare (ignore b)) (debug-thread thread)))
      (gobject:connect kill :clicked (lambda (b) (declare (ignore b)) (confirm-kill-thread thread)))
      (gtk:box-append box debug)
      (gtk:box-append box kill))
    row))

(defun shown-threads (page)
  (if (gtk:check-button-get-active (thp-show-swank page))
      (thp-threads page)
      (remove-if #'swank-thread-p (thp-threads page))))

(defun fill-threads-list (page)
  (let ((list (thp-list page))
        (shown (shown-threads page)))
    (gtk:list-box-remove-all list)
    (gtk:label-set-text (thp-title page)
                        (cond ((not (connected-p)) "Threads")
                              (t (format nil "~d thread~:p~[~:;, ~:*~d of Swank's hidden~]"
                                         (length shown) (- (length (thp-threads page)) (length shown))))))
    (if (and (null shown) (not (connected-p)))
        (gtk:list-box-append list (label "Start a Lisp to see its threads" :margin-start 8 :css-classes '("dim-label")))
        (dolist (thread shown)
          (gtk:list-box-append list (thread-row thread))))))

;;; Asking the Lisp

(defun refresh-threads ()
  (let ((page *threads-page*))
    (when (and page (connected-p) (not (thp-fetching page)))
      (setf (thp-fetching page) t)
      (rex *connection* (swank-call "swank:list-threads")
           :on-ok (lambda (reply)
                    (setf (thp-fetching page) nil)
                    (let ((threads (parse-thread-list reply)))
                      ;; Redraw only when something changed, so a click isn't lost.
                      (unless (equalp threads (thp-threads page))
                        (setf (thp-threads page) threads)
                        (fill-threads-list page))))
           :on-abort (lambda (reason)
                       (setf (thp-fetching page) nil)
                       (message "Could not list the threads: ~a" reason))))))

(defun threads-page-showing-p ()
  (and *window* *threads-page*
       (equal (panel-visible-name (window-panel *window*)) "threads")
       (gtk:widget-get-mapped (thp-widget *threads-page*))))

(defun start-threads-polling ()
  (let ((page *threads-page*))
    (unless (thp-timer page)
      (setf (thp-timer page)
            (glib:timeout-add glib:+priority-default+ *threads-refresh-ms*
                              (lambda ()
                                (cond ((and (threads-page-showing-p) (connected-p))
                                       (refresh-threads)
                                       t)
                                      (t (setf (thp-timer page) nil) nil))))))))

(defun threads-page-shown ()
  "The Threads tab was chosen."
  (refresh-threads)
  (start-threads-polling))

(defun threads-disconnected ()
  (when *threads-page*
    (setf (thp-threads *threads-page*) '() (thp-fetching *threads-page*) nil)
    (fill-threads-list *threads-page*)))

(defun debug-thread (thread)
  (with-connection (connection)
    (rex connection (swank-call "swank:debug-nth-thread" (lisp-thread-index thread))
         :on-ok (lambda (reply) (declare (ignore reply))
                  (message "Interrupted ~a" (lisp-thread-name thread))))))

(defun kill-thread (thread)
  (with-connection (connection)
    (rex connection (swank-call "swank:kill-nth-thread" (lisp-thread-index thread))
         :on-ok (lambda (reply) (declare (ignore reply))
                  (message "Killed ~a" (lisp-thread-name thread))
                  (refresh-threads)))))

(defun confirm-kill-thread (thread)
  (let ((dialog (adw:alert-dialog-new (format nil "Kill ~a?" (lisp-thread-name thread))
                                      (if (swank-thread-p thread)
                                          "Swank needs this thread: Cadre may lose its connection to the Lisp."
                                          "The thread ends where it is, without finishing its work."))))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "kill" "_Kill")
    (adw:alert-dialog-set-response-appearance dialog "kill" :destructive)
    (adw:alert-dialog-set-default-response dialog "cancel")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (when (string= response "kill") (kill-thread thread))))))

(define-command show-threads ()
  "Show the Threads page: the Lisp's threads, to debug or kill one."
  (panel-show (window-panel *window*) "threads")
  (set-panel-visible *window* t)
  (threads-page-shown))
