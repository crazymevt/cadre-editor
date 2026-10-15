;;;; api.lisp — the API page and request tabs: a front end to curl
;;;;
;;;; The API page (the activity bar's ⇄) lists the project's .http files
;;;; and the requests in them, and the History of requests sent but not
;;;; saved. Above them: the environment ({{variables}} come from the
;;;; project's http-client.env.json) and buttons to edit the environments
;;;; and the secrets, which are kept outside the project (in the config
;;;; directory's secrets/ folder), so git never sees them.
;;;;
;;;; A request opens in a tab: method, URL and Send; its headers, body and
;;;; options; and below, the response (status, time, size, body — JSON laid
;;;; out, and highlighted as JSON, XML or HTML — and headers). Save (Ctrl+S) asks which .http file in the project
;;;; it goes in (with no folder open, your own requests.http), and offers to
;;;; move anything that looks like a secret (an Authorization header, a
;;;; password field) into the secrets, leaving a {{variable}} in its place.
;;;; Copy as curl and Import curl command go to and from the command line.
;;;; In a .http file itself, Ctrl+Enter sends the request at the cursor.
;;;;
;;;; The core (http/) reads and writes .http files and runs curl.

(in-package #:cadre-ui)

(define-major-mode api-request-mode (:title "Request")
  "An API request, in a tab.")

(defstruct (api-page (:conc-name ap-))
  (file nil)                            ; the .http file it's saved in, or nil
  (saved-name nil)                      ; its name there, to find it again
  (name "")
  (options '())
  buffer method url headers-box params-box captures-box body-view send-button status response-stack
  (syncing nil)                         ; true while the URL and the Params rows update each other
  response-view                         ; an editor view of the response body, highlighted by its type
  response-headers pretty-button copy-body-button run response loading
  (auth '())                            ; the Auth tab's widgets, a plist
  (body-mode :text)                     ; :text, :form or :multipart
  body-mode-dropdown body-stack form-box multipart-box
  (multipart-rows '()))                 ; row → (name-entry value-entry file-toggle)

;;; Where things are kept

(defun api-project () (and *window* (window-project *window*)))

(defun api-home () (merge-pathnames "api/" (config-directory)))

(defun api-environments-file ()
  (let ((project (api-project)))
    (if project (project-http-environments-file project) (merge-pathnames "http-client.env.json" (api-home)))))

(defun api-secrets-file ()
  (let ((project (api-project)))
    (merge-pathnames (format nil "secrets/~a" (if project (http-secrets-file-name project) "global.json"))
                     (config-directory))))

(defun api-history-file () (merge-pathnames "history.http" (api-home)))

(defun api-default-file ()
  (let ((project (api-project)))
    (if project
        (merge-pathnames "api/requests.http" (uiop:ensure-directory-pathname project))
        (merge-pathnames "requests.http" (api-home)))))

(defvar *api-environment* nil "The environment chosen on the API page, or nil.")

(defvar *api-captured* '()
  "Values captured from responses, (name . value) each: {{variables}} until
Cadre quits (or the folder changes). Never written anywhere.")

(defun api-environments () (read-http-environments (api-environments-file)))

(defun api-lookup (file-variables)
  (http-variable-lookup :file-variables file-variables
                        :captured *api-captured*
                        :environment (http-environment-variables (api-environments) *api-environment*)
                        :secrets (http-environment-variables (read-http-environments (api-secrets-file))
                                                             *api-environment*)))

(defun api-http-files ()
  "The .http files in the project (or your own requests.http), sorted."
  (let ((project (api-project)) (files '()) (count 0))
    (if (null project)
        (let ((own (merge-pathnames "requests.http" (api-home)))) (and (probe-file own) (list own)))
        (labels ((walk (dir depth)
                   (dolist (file (uiop:directory-files dir))
                     (when (and (member (pathname-type file) '("http" "rest") :test #'equalp) (< count 500))
                       (incf count)
                       (push file files)))
                   (when (< depth 8)
                     (dolist (sub (uiop:subdirectories dir))
                       (let ((name (car (last (pathname-directory sub)))))
                         (unless (or (member name *explorer-hidden-names* :test #'string=)
                                     (member name '("node_modules" "target" "build" "dist" ".cadre") :test #'string=)
                                     (directory-link-p sub))
                           (walk sub (1+ depth))))))))
          (walk (uiop:ensure-directory-pathname project) 0)
          (sort files #'string< :key #'namestring)))))

(defun api-file-label (file)
  (let ((project (api-project)))
    (if project (file-in-project-name file project) (file-namestring file))))

(defun read-http-requests (file)
  (if (probe-file file) (parse-http-file (uiop:read-file-string file)) (values '() '())))

;;; History: requests sent, newest first, kept as a .http file

(defvar *api-history* :unread)
(defparameter *api-history-size* 50)

(defun api-history ()
  (when (eq *api-history* :unread)
    (setf *api-history* (ignore-errors (read-http-requests (api-history-file)))))
  *api-history*)

(defun remember-api-request (request)
  (let ((copy (copy-http-request request)))
    (setf (http-request-start copy) nil (http-request-end copy) nil)
    (setf *api-history*
          (let ((rest (remove-if (lambda (r) (and (string= (http-request-method r) (http-request-method copy))
                                                  (string= (http-request-url r) (http-request-url copy))
                                                  (string= (http-request-body r) (http-request-body copy))))
                                 (api-history))))
            (subseq (cons copy rest) 0 (min *api-history-size* (1+ (length rest))))))
    (ignore-errors
     (ensure-directories-exist (api-history-file))
     (with-open-file (out (api-history-file) :direction :output :if-exists :supersede :external-format :utf-8)
       (format out "# Requests Cadre sent, newest first (kept here, not in any project)~%~%")
       (dolist (r *api-history*) (write-string (render-http-request r) out))))
    (refresh-api-page)))

;;; The page

(defvar *api-list* nil)
(defvar *api-environment-dropdown* nil)
(defvar *api-status* nil)
(defvar *api-dropdown-updating* nil)

(defun make-api-widget ()
  (setf *api-list* (boxed-list)
        *api-status* (make-instance 'gtk:label :wrap t :xalign 0.0 :margin-start 12 :margin-end 12
                                               :margin-top 8 :css-classes '("dim-label"))
        *api-environment-dropdown* (gtk:drop-down-new-from-strings '("No environment")))
  (gobject:connect *api-environment-dropdown* "notify::selected"
                   (lambda (d pspec)
                     (declare (ignore pspec))
                     (unless *api-dropdown-updating*
                       (let ((i (gtk:drop-down-get-selected d)))
                         (setf *api-environment* (and (plusp i) (nth (1- i) (http-environment-names (api-environments))))
                               (setting :api-environment) *api-environment*)))))
  (gtk:widget-set-tooltip-text *api-environment-dropdown* "The environment {{variables}} come from")
  (gtk:widget-set-hexpand *api-environment-dropdown* t)
  (gtk:build
    (gtk:box :orientation :vertical
      (gtk:box :margin-start 12 :margin-end 6 :margin-top 4
        (gtk:label :label "API" :xalign 0.0 :hexpand t :css-classes '("caption-heading" "dim-label"))
        (gtk:button :icon-name "view-refresh-symbolic" :tooltip-text "Refresh" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (refresh-api-page)))
        (gtk:button :icon-name "edit-paste-symbolic" :tooltip-text "Import a curl command…" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'api-import-curl)))
        (gtk:button :icon-name "list-add-symbolic" :tooltip-text "New request" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'api-new-request))))
      (gtk:box :spacing 4 :margin-start 8 :margin-end 6 :margin-top 4
        *api-environment-dropdown*
        (gtk:button :icon-name "cadre-edit-symbolic" :css-classes '("flat")
                    :tooltip-text "Edit the environments (http-client.env.json, in the project)"
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'api-edit-environments)))
        (gtk:button :icon-name "dialog-password-symbolic" :css-classes '("flat")
                    :tooltip-text "Edit the secrets (kept outside the project, never committed)"
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'api-edit-secrets))))
      (gtk:scrolled-window :vexpand t :hscrollbar-policy :never
        (gtk:box :orientation :vertical
          *api-status*
          *api-list*)))))

(defun request-row (request on-open &optional menu-items)
  (let ((row (one-line-row :title (http-request-title request) :activatable t
                           :subtitle (format nil "~a ~a" (http-request-method request) (http-request-url request)))))
    (gobject:connect row :activated (guarded on-open))
    (when menu-items (adw:action-row-add-suffix row (actions-menu-button "More" menu-items)))
    row))

(defun refresh-api-environments ()
  (when *api-environment-dropdown*
    (let* ((names (http-environment-names (api-environments)))
           (*api-dropdown-updating* t))
      (unless (member *api-environment* names :test #'equal)
        (setf *api-environment* (find (setting :api-environment) names :test #'equal)))
      (gtk:drop-down-set-model *api-environment-dropdown* (gtk:string-list-new (cons "No environment" names)))
      (gtk:drop-down-set-selected *api-environment-dropdown*
                                  (let ((i (position *api-environment* names :test #'equal))) (if i (1+ i) 0))))))

(defun refresh-api-page ()
  (when *api-list*
    (refresh-api-environments)
    (gtk:list-box-remove-all *api-list*)
    (let ((files (api-http-files)))
      (dolist (file files)
        (let* ((file file)
               (requests (handler-case (read-http-requests file) (error () '())))
               (row (make-instance 'adw:expander-row :title (api-file-label file) :title-lines 1
                                                     :subtitle (format nil "~d request~:p" (length requests))
                                                     :expanded t)))
          (adw:expander-row-add-suffix row (actions-menu-button "More"
                                                                (list (cons "Open the File" (lambda () (open-file-path file))))))
          (dolist (request requests)
            (let ((request request))
              (adw:expander-row-add-row
               row (request-row request (lambda () (open-api-request request :file file))
                                (list (cons "Copy as curl" (lambda () (copy-request-as-curl request :file file)))
                                      (cons "Delete…" (lambda () (delete-api-request file request))))))))
          (gtk:list-box-append *api-list* row)))
      (when *api-captured*
        (let ((row (make-instance 'adw:expander-row :title "Captured" :title-lines 1
                                                    :subtitle (format nil "~d value~:p from responses" (length *api-captured*)))))
          (adw:expander-row-add-suffix row (flat-button "Clear" "Forget the captured values"
                                                        (lambda () (setf *api-captured* '()) (refresh-api-page))))
          (loop for (name . value) in *api-captured*
                do (adw:expander-row-add-row
                    row (one-line-row :title (format nil "{{~a}}" name)
                                      :subtitle (if (> (length value) 12) (format nil "~a…" (subseq value 0 12)) value))))
          (gtk:list-box-append *api-list* row)))
      (let ((history (api-history)))
        (when history
          (let ((row (make-instance 'adw:expander-row :title "History" :title-lines 1
                                                      :subtitle (format nil "~d sent, not saved" (length history)))))
            (dolist (request history)
              (let ((request request))
                (adw:expander-row-add-row row (request-row request (lambda () (open-api-request request))))))
            (gtk:list-box-append *api-list* row))))
      (gtk:label-set-text *api-status*
                          (if files
                              ""
                              (format nil "No .http files ~:[(open a folder to keep them with a project)~;in the project yet~]. + makes a request; saving it makes one."
                                      (api-project))))
      (gtk:widget-set-visible *api-status* (null files)))))

(define-command show-api ()
  "Show the API page: the project's .http requests, the environment, and the history."
  (show-sidebar-page *window* "api" :toggle nil))

(defun api-project-changed ()
  (setf *api-captured* '())
  (refresh-api-page))

(add-hook '*after-save-hook*
          (lambda (buffer)
            (let ((file (buffer-file buffer)))
              (when (and file (or (member (pathname-type file) '("http" "rest") :test #'equalp)
                                  (equal (file-namestring file) "http-client.env.json")))
                (refresh-api-page)))))

;;; Environments and secrets, edited as JSON

(defparameter *environments-template*
  "{
  \"$shared\": {
  },
  \"dev\": {
    \"baseUrl\": \"http://localhost:8080\"
  }
}
")

(define-command api-edit-environments ()
  "Edit the API environments: http-client.env.json in the project. Secrets go in API Edit Secrets instead."
  (let ((file (api-environments-file)))
    (unless (probe-file file)
      (ensure-directories-exist file)
      (with-open-file (out file :direction :output :external-format :utf-8) (write-string *environments-template* out)))
    (open-file-path file)))

(define-command api-edit-secrets ()
  "Edit the API secrets for this project: variables like the environments', kept in the config directory, outside the project."
  (let ((file (api-secrets-file)))
    (unless (probe-file file)
      (write-http-environments file (mapcar (lambda (name) (list name)) (cons "$shared" (http-environment-names (api-environments))))
                               :private t))
    (open-file-path file)
    (message "Secrets for this project, kept in ~a: never in the project" (uiop:native-namestring file))))

;;; A request in a tab

(defun api-request-buffer (file name)
  (and file name
       (find-if (lambda (b) (let ((page (buffer-local b :api-page)))
                              (and page (ap-file page) (equal (namestring (ap-file page)) (namestring file))
                                   (equal (ap-saved-name page) name))))
                (buffer-list))))

(defun open-api-request (request &key file send)
  "Show REQUEST in a tab (of FILE, where it's saved, if any); with SEND, send it."
  (let ((buffer (api-request-buffer file (and file (http-request-name request)))))
    (unless buffer
      (let ((page (make-api-page :file (and file (pathname file)) :saved-name (and file (http-request-name request))
                                 :name (http-request-name request) :options (http-request-options request))))
        (setf buffer (make-buffer :name (api-tab-name request) :text (make-gtk-text) :major-mode 'api-request-mode)
              (ap-buffer page) buffer
              (buffer-local buffer :api-page) page
              (buffer-local buffer :tab-widget) (lambda (view) (declare (ignore view)) (make-api-request-widget page request))
              (buffer-local buffer :save-function) (lambda (continuation) (save-api-request page continuation)))
        (unless file (setf (buffer-modified-p buffer) (plusp (length (http-request-url request)))))))
    (show-buffer *window* buffer)
    (when send (send-api-request (buffer-local buffer :api-page)))
    buffer))

(defun api-tab-name (request)
  (let ((title (if (plusp (length (http-request-name request)))
                   (http-request-name request)
                   (let ((url (http-request-url request)))
                     (if (plusp (length url)) (format nil "~a ~a" (http-request-method request) url) "New request")))))
    (if (> (length title) 40) (concatenate 'string (subseq title 0 39) "…") title)))

(define-command api-new-request ()
  "Open a new API request in a tab."
  (open-api-request (make-http-request :url "")))

(defun api-changed (page)
  (unless (ap-loading page)
    (setf (buffer-modified-p (ap-buffer page)) t)))

(defun pair-row (box name value &key on-change (what "header") (placeholders '("Name" "Value")))
  "A row of BOX: entries for a name and a value, and a button removing it.
ON-CHANGE is called after either changes, or the row goes."
  (let* ((row (make-instance 'gtk:box :spacing 4))
         (name-entry (make-instance 'gtk:entry :placeholder-text (first placeholders) :text name :width-chars 18))
         (value-entry (make-instance 'gtk:entry :placeholder-text (second placeholders) :text value :hexpand t))
         (remove (make-instance 'gtk:button :icon-name "list-remove-symbolic" :css-classes '("flat")
                                            :tooltip-text (format nil "Remove the ~a" what))))
    (dolist (e (list name-entry value-entry))
      (gobject:connect e :changed (lambda (e) (declare (ignore e)) (funcall on-change))))
    (gobject:connect remove :clicked (lambda (b) (declare (ignore b))
                                       (gtk:box-remove box row)
                                       (funcall on-change)))
    (gtk:box-append row name-entry)
    (gtk:box-append row value-entry)
    (gtk:box-append row remove)
    (gtk:box-append box row)
    row))

(defun box-rows (box)
  (loop for child = (gtk:widget-get-first-child box) then (gtk:widget-get-next-sibling child)
        while child collect child))

(defun box-pairs (box)
  "The (name . value) of BOX's rows that have a name."
  (loop for row in (box-rows box)
        for name-entry = (gtk:widget-get-first-child row)
        for name = (string-trim " " (gtk:editable-get-text name-entry))
        when (plusp (length name))
          collect (cons name (gtk:editable-get-text (gtk:widget-get-next-sibling name-entry)))))

(defun add-header-row (page &optional (name "") (value ""))
  (pair-row (ap-headers-box page) name value :on-change (lambda () (api-changed page))))

(defun add-capture-row (page &optional (name "") (path ""))
  (pair-row (ap-captures-box page) name path :what "capture" :on-change (lambda () (api-changed page))
                                              :placeholders '("Variable" "$.path, header Name, or status")))

;;; Auth: a form over the headers. Use puts the secret in this project's
;;; secrets and the header, with a {{variable}}, in the request.

(defparameter *auth-types* '((nil . "None") (:bearer . "Bearer token") (:basic . "Basic") (:api-key . "API key")))

(defun set-page-header (page name value)
  "Set header NAME in PAGE's Headers rows (adding a row if there's none)."
  (let ((row (find-if (lambda (row) (string-equal (string-trim " " (gtk:editable-get-text (gtk:widget-get-first-child row))) name))
                      (box-rows (ap-headers-box page)))))
    (if row
        (gtk:editable-set-text (gtk:widget-get-next-sibling (gtk:widget-get-first-child row)) value)
        (add-header-row page name value))))

(defun remove-auth-headers (page)
  (dolist (row (box-rows (ap-headers-box page)))
    (let ((name (string-trim " " (gtk:editable-get-text (gtk:widget-get-first-child row)))))
      (when (or (string-equal name "Authorization") (member name *api-key-headers* :test #'string-equal))
        (gtk:box-remove (ap-headers-box page) row)))))

(defun make-auth-widget (page request)
  (multiple-value-bind (type variable header) (http-request-auth request)
    (let* ((group (make-instance 'adw:preferences-group :margin-start 8 :margin-end 8 :margin-top 8))
           (kind (make-instance 'adw:combo-row :title "Type" :model (gtk:string-list-new (mapcar #'cdr *auth-types*))))
           (header-row (make-instance 'adw:entry-row :title "Header" :text (if (eq type :api-key) header "X-API-Key")))
           (variable-row (make-instance 'adw:entry-row :title "Variable ({{name}} in the request)"
                                                       :text (or variable "")))
           (user (make-instance 'adw:entry-row :title "Username"))
           (secret (make-instance 'adw:password-entry-row :title "Token"))
           (note (make-instance 'gtk:label :xalign 0.0 :wrap t :margin-start 12 :margin-end 12 :margin-top 8
                                           :css-classes '("dim-label" "caption")))
           (use (make-instance 'gtk:button :label "Use" :css-classes '("suggested-action") :halign :start
                                           :margin-start 12 :margin-top 8
                                           :tooltip-text "Keep the secret outside the project and set the request's header")))
      (labels ((chosen () (car (nth (adw:combo-row-get-selected kind) *auth-types*)))
               (default-variable (type) (case type (:basic "basic_auth") (:api-key "api_key") (:bearer "token")))
               (type-changed ()
                 ;; A new type: its usual variable, unless one was typed in.
                 (let ((now (gtk:editable-get-text variable-row)))
                   (when (and (chosen) (or (string= now "") (member now '("basic_auth" "api_key" "token") :test #'string=)))
                     (gtk:editable-set-text variable-row (default-variable (chosen)))))
                 (update))
               (update ()
                 (let ((type (chosen)))
                   (gtk:widget-set-visible header-row (eq type :api-key))
                   (gtk:widget-set-visible user (eq type :basic))
                   (dolist (row (list variable-row secret)) (gtk:widget-set-visible row (and type t)))
                   (adw:preferences-row-set-title secret (case type (:basic "Password") (:api-key "Key") (t "Token")))
                   (gtk:label-set-text note
                                       (if type
                                           (format nil "Use keeps what you type here in this project's secrets~@[ (~a environment)~], outside the project, and sends ~a. Leave it empty to keep the value already stored."
                                                   *api-environment*
                                                   (let ((h (auth-header type (let ((v (gtk:editable-get-text variable-row)))
                                                                                (if (string= v "") "…" v))
                                                                         :header (gtk:editable-get-text header-row))))
                                                     (format nil "~a: ~a" (car h) (cdr h))))
                                           "No authentication: Use takes the Authorization header out.")))))
        (adw:combo-row-set-selected kind (or (position type *auth-types* :key #'car) 0))
        (dolist (row (list kind header-row variable-row user secret)) (adw:preferences-group-add group row))
        (gobject:connect kind "notify::selected" (lambda (r p) (declare (ignore r p)) (type-changed)))
        (dolist (row (list variable-row header-row)) (gobject:connect row :changed (lambda (e) (declare (ignore e)) (update))))
        (gobject:connect use :clicked (lambda (b) (declare (ignore b)) (use-auth page)))
        (setf (ap-auth page) (list :kind kind :header header-row :variable variable-row :user user :secret secret
                                   :note note :chosen #'chosen))
        (update)
        (gtk:build
          (gtk:scrolled-window :hscrollbar-policy :never
            (gtk:box :orientation :vertical
              group note use)))))))

(defun use-auth (page)
  "Do what the Auth tab says: keep its secret, set the header."
  (let* ((auth (ap-auth page))
         (type (funcall (getf auth :chosen)))
         (variable (string-trim " {}" (gtk:editable-get-text (getf auth :variable))))
         (secret (gtk:editable-get-text (getf auth :secret)))
         (user (gtk:editable-get-text (getf auth :user)))
         (value (case type
                  (:basic (and (or (plusp (length user)) (plusp (length secret)))
                               (base64-string (format nil "~a:~a" user secret))))
                  (t (and (plusp (length secret)) secret)))))
    (cond ((null type)
           (remove-auth-headers page)
           (api-changed page)
           (message "The request sends no authentication"))
          ((string= variable "") (message "Name the variable the secret is kept in"))
          (t
           (when value
             (store-api-secrets (list (list :variable variable :value value)) :quietly t)
             (gtk:editable-set-text (getf auth :secret) ""))
           (remove-auth-headers page)
           (let ((header (auth-header type variable :header (string-trim " " (gtk:editable-get-text (getf auth :header))))))
             (set-page-header page (car header) (cdr header))
             (api-changed page)
             (message "~@[Kept {{~a}} in the secrets; ~]the request sends ~a: ~a"
                      (and value variable) (car header) (cdr header)))))))

;;; The body: text, a form's fields, or multipart parts (text or files)

(defparameter *body-modes* '((:text . "Text") (:form . "Form") (:multipart . "Multipart")))
(defparameter *multipart-boundary* "CadreBoundary")

(defun page-boundary (page)
  (or (multipart-boundary (make-http-request :headers (box-pairs (ap-headers-box page)))) *multipart-boundary*))

(defun multipart-parts (page)
  (loop for row in (box-rows (ap-multipart-box page))
        for (name-entry value-entry file) = (cdr (assoc row (ap-multipart-rows page)))
        for name = (and name-entry (string-trim " " (gtk:editable-get-text name-entry)))
        when (plusp (length name))
          collect (if (gtk:toggle-button-get-active file)
                      (list :name name :file (gtk:editable-get-text value-entry))
                      (list :name name :value (gtk:editable-get-text value-entry)))))

(defun page-body (page)
  "The body PAGE's Body tab describes, as the .http file has it."
  (ecase (ap-body-mode page)
    (:text (string-trim '(#\Newline #\Space #\Tab #\Return) (text-string (gtk:text-view-get-buffer (ap-body-view page)))))
    (:form (pairs-form-body (box-pairs (ap-form-box page))))
    (:multipart (let ((parts (multipart-parts page)))
                  (if parts (render-multipart-body parts (page-boundary page)) "")))))

(defun add-form-row (page &optional (name "") (value ""))
  (pair-row (ap-form-box page) name value :what "field" :on-change (lambda () (api-changed page))))

(defun add-multipart-row (page &key (name "") (value "") file)
  "A row of the Multipart body: a name, and a value or (with File on) a file's path."
  (let* ((box (ap-multipart-box page))
         (row (make-instance 'gtk:box :spacing 4))
         (name-entry (make-instance 'gtk:entry :placeholder-text "Name" :text name :width-chars 14))
         (value-entry (make-instance 'gtk:entry :placeholder-text (if file "File" "Value") :text value :hexpand t))
         (toggle (make-instance 'gtk:toggle-button :label "File" :active (and file t) :css-classes '("flat")
                                                   :tooltip-text "Send a file (its path) instead of the text"))
         (choose (make-instance 'gtk:button :label "Choose…" :css-classes '("flat")))
         (remove (make-instance 'gtk:button :icon-name "list-remove-symbolic" :css-classes '("flat")
                                            :tooltip-text "Remove the part")))
    (dolist (e (list name-entry value-entry))
      (gobject:connect e :changed (lambda (e) (declare (ignore e)) (api-changed page))))
    (gobject:connect toggle :toggled (lambda (b)
                                       (gtk:entry-set-placeholder-text value-entry (if (gtk:toggle-button-get-active b) "File" "Value"))
                                       (api-changed page)))
    (gobject:connect choose :clicked
                     (lambda (b) (declare (ignore b))
                       (gio:async (gtk:file-dialog-open (gtk:file-dialog-new) (window-gtk-window *window*))
                                  (lambda (f)
                                    (gtk:editable-set-text value-entry (gio:file-get-path f))
                                    (gtk:toggle-button-set-active toggle t)
                                    (when (string= (string-trim " " (gtk:editable-get-text name-entry)) "")
                                      (gtk:editable-set-text name-entry "file")))
                                  :error (lambda (e) (declare (ignore e))))))
    (gobject:connect remove :clicked (lambda (b) (declare (ignore b))
                                       (gtk:box-remove box row)
                                       (setf (ap-multipart-rows page) (remove row (ap-multipart-rows page) :key #'car))
                                       (api-changed page)))
    (dolist (w (list name-entry value-entry toggle choose remove)) (gtk:box-append row w))
    (gtk:box-append box row)
    (push (list row name-entry value-entry toggle) (ap-multipart-rows page))
    row))

(defun show-body (page body mode)
  "Fill the Body tab's MODE (:text, :form or :multipart) from BODY."
  (ecase mode
    (:text (gtk:text-buffer-set-text (gtk:text-view-get-buffer (ap-body-view page)) body -1))
    (:form (dolist (row (box-rows (ap-form-box page))) (gtk:box-remove (ap-form-box page) row))
     (loop for (name . value) in (form-body-pairs body) do (add-form-row page name value)))
    (:multipart (dolist (row (box-rows (ap-multipart-box page))) (gtk:box-remove (ap-multipart-box page) row))
     (setf (ap-multipart-rows page) '())
     (dolist (part (parse-multipart-body body (page-boundary page)))
       (add-multipart-row page :name (getf part :name) :value (or (getf part :file) (getf part :value) "")
                               :file (getf part :file))))))

(defun request-body-mode (request)
  (let ((type (http-request-content-type request)))
    (cond ((search "multipart/form-data" type) :multipart)
          ((search "application/x-www-form-urlencoded" type) :form)
          (t :text))))

(defun set-body-mode (page mode)
  "Show the body as MODE, carrying what it holds over, and set the Content-Type to match."
  (let ((old (ap-body-mode page)))
    (unless (eq old mode)
      (let ((body (page-body page)))
        (setf (ap-body-mode page) mode)
        (case mode
          (:form (set-page-header page "Content-Type" "application/x-www-form-urlencoded")
           (show-body page (if (eq old :text) body "") :form))
          (:multipart (set-page-header page "Content-Type" (format nil "multipart/form-data; boundary=~a" *multipart-boundary*))
           (show-body page (if (eq old :text) "" body) :multipart)
           (unless (box-rows (ap-multipart-box page)) (add-multipart-row page)))
          (:text (show-body page body :text)))
        (gtk:stack-set-visible-child-name (ap-body-stack page) (string-downcase mode))
        (api-changed page)))))

(defun make-body-widget (page request body-view)
  (let* ((mode (request-body-mode request))
         (dropdown (gtk:drop-down-new-from-strings (mapcar #'cdr *body-modes*)))
         (stack (make-instance 'gtk:stack :vexpand t))
         (form (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 6))
         (multipart (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 6)))
    (setf (ap-body-mode-dropdown page) dropdown (ap-body-stack page) stack
          (ap-form-box page) form (ap-multipart-box page) multipart (ap-body-mode page) mode)
    (gtk:stack-add-named stack
                         (gtk:build
                           (gtk:box :orientation :vertical
                             (gtk:scrolled-window :vexpand t body-view)
                             (gtk:box :margin-start 8 :margin-bottom 4 :spacing 6
                               (flat-button "Format JSON" "Lay the body out as JSON"
                                            (lambda () (let ((b (gtk:text-view-get-buffer body-view)))
                                                         (handler-case (progn (parse-json (text-string b))
                                                                              (gtk:text-buffer-set-text b (format-json (text-string b)) -1))
                                                           (error () (message "The body isn't JSON"))))))
                               (gtk:label :xalign 0.0 :css-classes '("dim-label" "caption")
                                          :label "Set a Content-Type header to match (application/json, …); < ./file sends a file"))))
                         "text")
    (gtk:stack-add-named stack
                         (gtk:build
                           (gtk:scrolled-window :hscrollbar-policy :never
                             (gtk:box :orientation :vertical
                               form
                               (gtk:box :margin-start 8 :margin-top 4
                                 (flat-button "Add Field" "Add a form field"
                                              (lambda () (gtk:widget-grab-focus (add-form-row page))))))))
                         "form")
    (gtk:stack-add-named stack
                         (gtk:build
                           (gtk:scrolled-window :hscrollbar-policy :never
                             (gtk:box :orientation :vertical
                               multipart
                               (gtk:box :margin-start 8 :margin-top 4 :spacing 6
                                 (flat-button "Add Part" "Add a text or file part"
                                              (lambda () (add-multipart-row page)))
                                 (gtk:label :xalign 0.0 :css-classes '("dim-label" "caption")
                                            :label "Files are sent as they are when the request goes, read from where they are")))))
                         "multipart")
    (unless (eq mode :text) (show-body page (http-request-body request) mode))
    (gtk:drop-down-set-selected dropdown (position mode *body-modes* :key #'car))
    (gtk:stack-set-visible-child-name stack (string-downcase mode))
    (gobject:connect dropdown "notify::selected"
                     (lambda (d p) (declare (ignore p))
                       (set-body-mode page (car (nth (gtk:drop-down-get-selected d) *body-modes*)))))
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :margin-start 8 :margin-top 6 :spacing 6
          (gtk:label :label "Body" :css-classes '("dim-label"))
          dropdown)
        stack))))

;;; Params: the URL's query, as rows. Each follows the other.

(defun add-param-row (page &optional (name "") (value ""))
  (pair-row (ap-params-box page) name value :what "parameter" :on-change (lambda () (params-changed page))))

(defun params-changed (page)
  "A Params row changed: write the query into the URL."
  (unless (ap-syncing page)
    (setf (ap-syncing page) t)
    (unwind-protect
         (let ((url (ap-url page)))
           (gtk:editable-set-text url (url-with-query-parameters (gtk:editable-get-text url) (box-pairs (ap-params-box page)))))
      (setf (ap-syncing page) nil))))

(defun show-url-params (page)
  "The URL changed: show its query in the Params rows."
  (unless (ap-syncing page)
    (setf (ap-syncing page) t)
    (unwind-protect
         (let ((box (ap-params-box page)))
           (dolist (row (box-rows box)) (gtk:box-remove box row))
           (loop for (name . value) in (url-query-parameters (gtk:editable-get-text (ap-url page)))
                 do (add-param-row page name value)))
      (setf (ap-syncing page) nil))))

(defun page-request (page)
  "The request PAGE's fields describe."
  (make-http-request
   :name (ap-name page)
   :method (nth (gtk:drop-down-get-selected (ap-method page)) *http-methods*)
   :url (string-trim " " (gtk:editable-get-text (ap-url page)))
   :headers (box-pairs (ap-headers-box page))
   :body (page-body page)
   :options (let ((captures (box-pairs (ap-captures-box page)))
                  (options (copy-list (ap-options page))))
              (remf options :captures)
              (if captures (list* :captures captures options) options))))

(defun option-switch (page title subtitle option &key invert)
  (let ((row (make-instance 'adw:switch-row :title title :subtitle subtitle
                                            :active (let ((on (getf (ap-options page) option))) (if invert (not on) (and on t))))))
    (gobject:connect row "notify::active"
                     (lambda (r pspec)
                       (declare (ignore pspec))
                       (let ((on (adw:switch-row-get-active r)))
                         (setf (getf (ap-options page) option) (if invert (not on) on))
                         (api-changed page))))
    row))

(defun make-api-request-widget (page request)
  (setf (ap-loading page) t)
  (let* ((method (gtk:drop-down-new-from-strings *http-methods*))
         (url (make-instance 'gtk:entry :hexpand t :text (http-request-url request)
                                        :placeholder-text "https://… or {{baseUrl}}/path"))
         (send (make-instance 'gtk:button :label "Send" :css-classes '("suggested-action")
                                          :tooltip-text "Send the request (Enter in the URL)"))
         (headers (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 6))
         (params (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 6))
         (captures (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 4))
         (body (make-instance 'gtk:text-view :monospace t :wrap-mode :word-char :top-margin 6 :left-margin 8
                                             :right-margin 8 :bottom-margin 6 :vexpand t))
         (request-stack (make-instance 'gtk:stack :vexpand t))
         (options (make-instance 'adw:preferences-group :margin-start 8 :margin-end 8 :margin-top 8))
         (timeout (gtk:spin-button-new-with-range 1d0 3600d0 1d0))
         (status (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end :selectable t
                                           :label "Send it to see the response"
                                           :css-classes '("dim-label")))
         (response-stack (make-instance 'gtk:stack :vexpand t))
         (response-view (make-response-view))
         (pretty (make-instance 'gtk:toggle-button :label "Pretty" :active t :css-classes '("flat")
                                                   :tooltip-text "Lay JSON and XML out (off: as it came)"))
         (response-headers (make-instance 'gtk:text-view :monospace t :editable nil :wrap-mode :word-char
                                                         :top-margin 6 :left-margin 8 :right-margin 8))
         (copy-body (flat-button "Copy" "Copy the response body"
                                 (lambda () (let ((r (ap-response page)))
                                              (when r (copy-to-clipboard (http-response-body r)) (message "Copied the response"))))))
         (save-body (flat-button "Save…" "Save the response body to a file" (lambda () (save-response-body page))))
         (timeout-row (make-instance 'adw:action-row :title "Timeout" :subtitle "Seconds before giving up")))
    (setf (ap-method page) method (ap-url page) url (ap-send-button page) send (ap-headers-box page) headers
          (ap-params-box page) params (ap-captures-box page) captures
          (ap-body-view page) body (ap-status page) status (ap-response-stack page) response-stack
          (ap-response-view page) response-view (ap-response-headers page) response-headers
          (ap-pretty-button page) pretty
          (ap-copy-body-button page) copy-body)
    (gtk:drop-down-set-selected method (or (position (http-request-method request) *http-methods* :test #'string-equal) 0))
    (loop for (name . value) in (http-request-headers request) do (add-header-row page name value))
    (show-url-params page)
    (loop for (name . path) in (getf (ap-options page) :captures) do (add-capture-row page name path))
    (when (eq (request-body-mode request) :text)
      (gtk:text-buffer-set-text (gtk:text-view-get-buffer body) (http-request-body request) -1))
    (gtk:spin-button-set-value timeout (coerce (or (getf (ap-options page) :timeout) 60) 'double-float))
    (gtk:widget-set-valign timeout :center)
    (adw:action-row-add-suffix timeout-row timeout)
    (adw:preferences-group-add options (option-switch page "Follow redirects" "Go where a 3xx response points"
                                                      :no-redirect :invert t))
    (adw:preferences-group-add options (option-switch page "Skip TLS certificate checks"
                                                      "For local servers with self-signed certificates" :insecure))
    (adw:preferences-group-add options timeout-row)
    (gobject:connect timeout "value-changed"
                     (lambda (s) (let ((v (gtk:spin-button-get-value-as-int s)))
                                   (setf (getf (ap-options page) :timeout) (if (= v 60) nil v))
                                   (api-changed page))))
    (gobject:connect method "notify::selected" (lambda (d p) (declare (ignore d p)) (api-changed page)))
    (gobject:connect url :changed (lambda (e) (declare (ignore e)) (show-url-params page) (api-changed page)))
    (gobject:connect url :activate (lambda (e) (declare (ignore e)) (send-or-cancel page)))
    (gobject:connect (gtk:text-view-get-buffer body) :changed (lambda (b) (declare (ignore b)) (api-changed page)))
    (gobject:connect send :clicked (lambda (b) (declare (ignore b)) (send-or-cancel page)))
    (gtk:stack-add-titled request-stack
                          (gtk:build
                            (gtk:scrolled-window :hscrollbar-policy :never
                              (gtk:box :orientation :vertical
                                params
                                (gtk:box :margin-start 8 :margin-top 4 :spacing 6
                                  (flat-button "Add Parameter" "Add a query parameter to the URL"
                                               (lambda () (gtk:widget-grab-focus (add-param-row page))))
                                  (gtk:label :xalign 0.0 :css-classes '("dim-label" "caption")
                                             :label "The URL's query: editing one edits the other")))))
                          "params" "Params")
    (gtk:stack-add-titled request-stack
                          (gtk:build
                            (gtk:scrolled-window :hscrollbar-policy :never
                              (gtk:box :orientation :vertical
                                headers
                                (gtk:box :margin-start 8 :margin-top 4
                                  (flat-button "Add Header" "Add a header" (lambda () (add-header-row page) (api-changed page)))))))
                          "headers" "Headers")
    (gtk:stack-add-titled request-stack (make-body-widget page request body) "body" "Body")
    (gtk:stack-add-titled request-stack (make-auth-widget page request) "auth" "Auth")
    (gtk:stack-add-titled request-stack
                          (gtk:build
                            (gtk:scrolled-window :hscrollbar-policy :never
                              (gtk:box :orientation :vertical
                                options
                                (gtk:label :label "CAPTURE FROM THE RESPONSE" :xalign 0.0 :margin-start 12 :margin-top 14
                                           :css-classes '("caption-heading" "dim-label"))
                                (gtk:label :xalign 0.0 :margin-start 12 :wrap t :css-classes '("dim-label" "caption")
                                           :label "Values for {{variables}} in the requests sent after this one, such as a login's token. Kept until Cadre quits, never written to a file.")
                                captures
                                (gtk:box :margin-start 8 :margin-top 4 :margin-bottom 8
                                  (flat-button "Add Capture" "Capture a value from the response"
                                               (lambda () (gtk:widget-grab-focus (add-capture-row page))))))))
                          "options" "Options")
    (gtk:stack-set-visible-child-name request-stack
                                      (cond ((url-query-parameters (http-request-url request)) "params")
                                            ((plusp (length (http-request-body request))) "body")
                                            (t "headers")))
    (gtk:stack-add-titled response-stack (view-widget response-view) "body" "Body")
    (gobject:connect pretty :toggled (lambda (b) (declare (ignore b))
                                       (when (ap-response page) (show-response-body page (ap-response page)))))
    (gtk:stack-add-titled response-stack (make-instance 'gtk:scrolled-window :child response-headers :vexpand t) "headers" "Headers")
    (gtk:widget-set-visible copy-body nil)
    (gtk:widget-set-visible save-body nil)
    (gtk:widget-set-visible pretty nil)
    (setf (ap-run page) nil (ap-loading page) nil)
    (let ((paned (make-instance 'gtk:paned :orientation :vertical :vexpand t :position 260
                                           :shrink-start-child nil :shrink-end-child nil)))
      (gtk:paned-set-start-child paned
                                 (gtk:build
                                   (gtk:box :orientation :vertical
                                     (gtk:stack-switcher :stack request-stack :halign :start :margin-start 8 :margin-top 4)
                                     request-stack)))
      (gtk:paned-set-end-child paned
                               (gtk:build
                                 (gtk:box :orientation :vertical
                                   (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 4 :margin-bottom 2
                                     (gtk:stack-switcher :stack response-stack)
                                     status pretty copy-body save-body)
                                   (gtk:separator)
                                   response-stack)))
      (gobject:connect copy-body "notify::visible"
                       (lambda (b p) (declare (ignore p))
                         (gtk:widget-set-visible save-body (gtk:widget-get-visible b))
                         (gtk:widget-set-visible pretty (gtk:widget-get-visible b))))
      (gtk:build
        (gtk:box :orientation :vertical
          (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 6 :margin-bottom 4
            method url send
            (gtk:separator :orientation :vertical)
            (icon-button "edit-copy-symbolic" "Copy as a curl command line"
                         (lambda () (copy-request-as-curl (page-request page) :file (ap-file page))))
            (icon-button "cadre-save-symbolic" "Save the request in a .http file (Ctrl+S)"
                         (lambda () (save-api-request page (lambda (ok) (declare (ignore ok)))))))
          (gtk:separator)
          paned)))))

;;; Sending

(defun page-file-variables (page)
  (and (ap-file page) (nth-value 1 (read-http-requests (ap-file page)))))

(defun resolved-request (request &key file)
  "REQUEST with its variables' values from FILE, the environment and the secrets; signals if any has none."
  (multiple-value-bind (resolved missing)
      (resolve-http-request request (api-lookup (and file (nth-value 1 (read-http-requests file)))))
    (when missing
      (editor-error "No value for ~{{{~a}}~^, ~}: add ~:[it~;them~] to the environment~@[ (~a)~] or the secrets"
                    missing (rest missing) *api-environment*))
    resolved))

(defun page-directory (page)
  "Where files the body names (< ./file) are: beside its .http file, else the project's folder."
  (or (and (ap-file page) (uiop:pathname-directory-pathname (ap-file page))) (api-project)))

(defun send-or-cancel (page)
  (handler-case (if (ap-run page) (cancel-api-request page) (send-api-request page))
    (editor-error (e) (show-api-status page (editor-error-message e) "error"))))

(defun show-api-status (page text &optional class)
  (let ((label (ap-status page)))
    (gtk:label-set-text label text)
    (dolist (c '("dim-label" "success" "warning" "error")) (gtk:widget-remove-css-class label c))
    (gtk:widget-add-css-class label (or class "dim-label"))))

(defun send-api-request (page)
  (let* ((request (page-request page))
         (resolved (progn (when (string= (http-request-url request) "") (editor-error "Type the URL first"))
                          (resolved-request request :file (ap-file page))))
         (run (start-curl resolved :directory (page-directory page))))
    (setf (ap-run page) run)
    (gtk:button-set-label (ap-send-button page) "Cancel")
    (gtk:widget-remove-css-class (ap-send-button page) "suggested-action")
    (show-api-status page (format nil "Sending ~a ~a…" (http-request-method resolved) (http-request-url resolved)))
    (unless (ap-file page) (remember-api-request request))
    (sb-thread:make-thread
     (lambda ()
       (let ((response (handler-case (finish-curl run)
                         (error (e) (make-http-response :error (princ-to-string e))))))
         (glib:call-in-main-thread
          (lambda ()
            (when (eq (ap-run page) run)
              (setf (ap-run page) nil)
              (show-api-response page response))))))
     :name "cadre curl")))

(defun cancel-api-request (page)
  (let ((run (ap-run page)))
    (when run
      (setf (ap-run page) nil)
      (cancel-curl run)
      (reset-send-button page)
      (show-api-status page "Cancelled" "warning"))))

(defun reset-send-button (page)
  (gtk:button-set-label (ap-send-button page) "Send")
  (gtk:widget-add-css-class (ap-send-button page) "suggested-action"))

(defun byte-size-text (n)
  (cond ((< n 1024) (format nil "~d B" n))
        ((< n (* 1024 1024)) (format nil "~,1f KB" (/ n 1024)))
        (t (format nil "~,1f MB" (/ n 1024 1024)))))

(defun show-api-response (page response)
  (setf (ap-response page) response)
  (reset-send-button page)
  (cond ((http-response-error response)
         (show-api-status page (http-response-error response) "error")
         (set-response-text (ap-response-view page) "" 'fundamental-mode)
         (gtk:text-buffer-set-text (gtk:text-view-get-buffer (ap-response-headers page)) "" -1)
         (gtk:widget-set-visible (ap-copy-body-button page) nil))
        (t
         (let ((status (http-response-status response)))
           (show-api-status page (format nil "~d ~a · ~:d ms · ~a" status (http-response-reason response)
                                         (http-response-milliseconds response) (byte-size-text (http-response-size response)))
                            (cond ((< status 300) "success") ((< status 400) "warning") (t "error"))))
         (show-response-body page response)
         (gtk:text-buffer-set-text (gtk:text-view-get-buffer (ap-response-headers page))
                                   (format nil "~{~a~%~}" (mapcar (lambda (h) (format nil "~a: ~a" (car h) (cdr h)))
                                                                 (http-response-headers response)))
                                   -1)
         (gtk:widget-set-visible (ap-copy-body-button page) t)
         (capture-from-response page response))))

(defun capture-from-response (page response)
  "Keep the values the request's captures take from RESPONSE, and say so."
  (let ((captures (getf (http-request-options (page-request page)) :captures)))
    (when captures
      (multiple-value-bind (found missing) (capture-response-values response captures)
        (loop for (name . value) in found
              do (setf *api-captured* (cons (cons name value) (remove name *api-captured* :key #'car :test #'string=))))
        (show-api-status page (format nil "~a~@[ · captured ~{{{~a}}~^, ~}~]~@[ · not found: ~{~a~^, ~}~]"
                                      (gtk:label-get-text (ap-status page)) (mapcar #'car found) missing)
                         (if missing "warning" (if (< (http-response-status response) 300) "success" "warning")))
        (when found (refresh-api-page))))))

(defun make-response-view ()
  "A read-only editor view for response bodies, highlighted like a file of their type."
  (let* ((buffer (make-buffer :name "*Response*" :text (make-gtk-text) :major-mode 'fundamental-mode))
         (view (make-editor-view buffer)))
    (gtk:text-view-set-editable (view-text-view view) nil)
    (gtk:text-view-set-wrap-mode (view-text-view view) :word-char)
    (attach-syntax buffer)
    (setf (buffer-local buffer :embedded-views) (list view))
    (gobject:connect (gtk:scrolled-window-get-vadjustment (view-widget view)) :value-changed
                     (lambda (a) (declare (ignore a)) (schedule-highlight buffer)))
    view))

(defun set-response-text (view text mode)
  (let ((buffer (view-buffer view)))
    (unless (eq (buffer-major-mode buffer) mode)
      (setf (buffer-major-mode buffer) mode)
      (attach-syntax buffer))
    (text-replace-contents (buffer-text buffer) text)
    (gtk:text-buffer-place-cursor (buffer-text buffer) (gtk:text-buffer-get-start-iter (buffer-text buffer)))
    (gtk:text-buffer-set-modified (buffer-text buffer) nil)
    (schedule-highlight buffer)))

(defun show-response-body (page response)
  (set-response-text (ap-response-view page)
                     (response-body-text response :pretty (gtk:toggle-button-get-active (ap-pretty-button page)))
                     (response-body-mode response)))

(add-hook '*buffer-killed-hook*
          (lambda (buffer)
            (let ((page (buffer-local buffer :api-page)))
              (when (and page (ap-response-view page))
                (let ((response-buffer (view-buffer (ap-response-view page))))
                  (when (member response-buffer (buffer-list)) (kill-buffer response-buffer)))))))

(defun save-response-body (page)
  (let ((response (ap-response page))
        (chooser (gtk:file-dialog-new)))
    (when response
      (gtk:file-dialog-set-initial-name chooser (if (search "json" (or (http-response-content-type response) ""))
                                                    "response.json" "response.txt"))
      (gio:async (gtk:file-dialog-save chooser (window-gtk-window *window*))
                 (lambda (file)
                   (with-open-file (out (gio:file-get-path file) :direction :output :if-exists :supersede
                                                                 :external-format :utf-8)
                     (write-string (http-response-body response) out))
                   (message "Saved the response to ~a" (gio:file-get-path file)))
                 :error (lambda (e) (declare (ignore e)))))))

(define-command api-send-request-at-cursor ()
  "Send the request at the cursor in this .http file, showing it in a tab."
  (:modes http-mode)
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (offset (gtk:text-iter-get-offset (cursor-iter (view-gtk-buffer view))))
         (request (find-if (lambda (r) (<= (http-request-start r) offset (1- (max (1+ (http-request-start r)) (http-request-end r)))))
                           (parse-http-file (buffer-string buffer)))))
    (unless request (editor-error "No request at the cursor"))
    (unless (buffer-file buffer) (editor-error "Save the file first"))
    (when (buffer-modified-p buffer)
      (editor-error "Save the file first: the request is sent as saved"))
    (open-api-request request :file (buffer-file buffer) :send t)))

;;; Saving

(defvar *api-save-dialog* nil "The open Save Request dialog's parts (for the smoke test).")
(defvar *api-secrets-dialog* nil "The open secrets question (for the smoke test).")

(defun save-api-request (page continuation)
  "Save PAGE's request in its .http file, asking which one the first time."
  (if (ap-file page)
      (check-secrets-then-save page (ap-file page) (ap-name page) continuation)
      (save-request-dialog page continuation)))

(defun save-request-dialog (page continuation)
  (let* ((files (api-http-files))
         (default (api-default-file))
         (choices (append (mapcar #'api-file-label files)
                          (unless (member (namestring default) (mapcar #'namestring files) :test #'string=)
                            (list (format nil "~a (new)" (api-file-label default))))))
         (paths (append files (unless (member (namestring default) (mapcar #'namestring files) :test #'string=)
                                (list default))))
         (dialog (make-instance 'adw:dialog :title "Save Request" :content-width 480))
         (name (make-instance 'adw:entry-row :title "Name"
                                             :text (if (plusp (length (ap-name page))) (ap-name page)
                                                       (api-tab-name (page-request page)))))
         (file (make-instance 'adw:combo-row :title "File" :model (gtk:string-list-new choices)))
         (note (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label" "caption")
                                         :label (if (api-project)
                                                    "In the project, to commit with it. Secrets are offered a place outside it."
                                                    "No folder is open: it's kept in your own requests.http.")))
         (save (make-instance 'gtk:button :label "_Save" :use-underline t :css-classes '("suggested-action")))
         (cancel (make-instance 'gtk:button :label "_Cancel" :use-underline t))
         (done nil))
    (flet ((finish (ok)
             (unless done (setf done t) (funcall continuation ok)))
           (save-it ()
             (let ((n (string-trim " " (gtk:editable-get-text name))))
               (when (plusp (length n))
                 (setf done t)
                 (adw:dialog-close dialog)
                 (check-secrets-then-save page (nth (adw:combo-row-get-selected file) paths) n continuation)))))
      (gobject:connect save :clicked (lambda (b) (declare (ignore b)) (save-it)))
      (gobject:connect name :entry-activated (lambda (e) (declare (ignore e)) (save-it)))
      (gobject:connect cancel :clicked (lambda (b) (declare (ignore b)) (adw:dialog-close dialog)))
      (gobject:connect dialog :closed (lambda (d) (declare (ignore d)) (finish nil)))
      (let ((group (make-instance 'adw:preferences-group))
            (header (make-instance 'adw:header-bar :show-end-title-buttons nil :show-start-title-buttons nil))
            (toolbar (make-instance 'adw:toolbar-view)))
        (adw:preferences-group-add group name)
        (adw:preferences-group-add group file)
        (adw:header-bar-pack-start header cancel)
        (adw:header-bar-pack-end header save)
        (adw:toolbar-view-add-top-bar toolbar header)
        (adw:toolbar-view-set-content toolbar (gtk:build (gtk:box :orientation :vertical :spacing 12 :margin-start 18
                                                                  :margin-end 18 :margin-top 12 :margin-bottom 18
                                                           group note)))
        (adw:dialog-set-child dialog toolbar))
      (setf *api-save-dialog* (list :dialog dialog :name name :file file :paths paths :save-it #'save-it))
      (adw:dialog-present dialog (window-gtk-window *window*))
      (gtk:widget-grab-focus name))))

(defun check-secrets-then-save (page file name continuation)
  "Save PAGE's request as NAME in FILE, first offering to move what looks like a secret out of it."
  (let* ((request (page-request page))
         (secrets (find-literal-secrets request)))
    (setf (http-request-name request) name)
    (if (null secrets)
        (funcall continuation (write-api-request page file request))
        (let ((dialog (adw:alert-dialog-new
                       "Move secrets out of the file?"
                       (format nil "This looks like a secret, which would be committed with the project:~%~%~{~a~%~}~%Move ~:[it~;them~] to this project's secrets~@[ (~a environment)~], outside the project, and leave {{variables}} in the request?"
                               (mapcar (lambda (s)
                                         (let ((value (getf s :value)))
                                           (format nil "• ~a ~a: ~a… → {{~a}}" (string-downcase (getf s :where)) (getf s :key)
                                                   (subseq value 0 (min 4 (length value))) (getf s :variable))))
                                       secrets)
                               (rest secrets) *api-environment*))))
          (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
          (adw:alert-dialog-add-response dialog "keep" "_Keep in File")
          (adw:alert-dialog-add-response dialog "move" "_Move to Secrets")
          (adw:alert-dialog-set-response-appearance dialog "keep" :destructive)
          (adw:alert-dialog-set-response-appearance dialog "move" :suggested)
          (adw:alert-dialog-set-default-response dialog "move")
          (adw:alert-dialog-set-close-response dialog "cancel")
          (setf *api-secrets-dialog* (list :dialog dialog
                                           :answer (lambda (response)
                                                     (secrets-answered page file request secrets response continuation))))
          (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                     (lambda (response) (secrets-answered page file request secrets response continuation)))))))

(defun secrets-answered (page file request secrets response continuation)
  "Save REQUEST as the secrets question's RESPONSE says: \"move\" them out, \"keep\" them, or not at all."
  (cond ((string= response "move")
         (store-api-secrets secrets)
         (let ((moved (move-secrets-out request secrets)))
           (load-request-fields page moved)
           (funcall continuation (write-api-request page file moved))))
        ((string= response "keep") (funcall continuation (write-api-request page file request)))
        (t (funcall continuation nil))))

(defun store-api-secrets (secrets &key quietly)
  "Put SECRETS (from FIND-LITERAL-SECRETS: plists with :variable and :value) in this project's secrets file, in the current environment."
  (let ((file (api-secrets-file))
        (env (or *api-environment* "$shared")))
    (write-http-environments file
                             (reduce (lambda (envs s) (set-http-environment-variable envs env (getf s :variable) (getf s :value)))
                                     secrets :initial-value (read-http-environments file))
                             :private t)
    (unless quietly
      (message "Moved ~d secret~:p to ~a" (length secrets) (uiop:native-namestring file)))))

(defun load-request-fields (page request)
  "Show REQUEST's URL, headers and body in PAGE's fields."
  (setf (ap-loading page) t)
  (unwind-protect
       (progn
         (gtk:editable-set-text (ap-url page) (http-request-url request))
         (dolist (row (box-rows (ap-headers-box page))) (gtk:box-remove (ap-headers-box page) row))
         (loop for (name . value) in (http-request-headers request) do (add-header-row page name value))
         (show-body page (http-request-body request) (ap-body-mode page)))
    (setf (ap-loading page) nil)))

(defun request-with-relative-files (request directory)
  "REQUEST with the files its body names written relative to DIRECTORY (./photo.png) when they're under it."
  (let ((copy (copy-http-request request))
        (boundary (multipart-boundary request))
        (file (body-file-reference (http-request-body request))))
    (cond (boundary
           (setf (http-request-body copy)
                 (render-multipart-body (mapcar (lambda (part)
                                                  (if (getf part :file)
                                                      (let ((p (copy-list part)))
                                                        (setf (getf p :file) (relative-body-path (getf part :file) directory))
                                                        p)
                                                      part))
                                                (parse-multipart-body (http-request-body request) boundary))
                                        boundary)))
          (file (setf (http-request-body copy) (format nil "< ~a" (relative-body-path file directory)))))
    copy))

(defun write-api-request (page file request)
  "Write REQUEST into FILE (replacing the one PAGE was saved as there). Returns t."
  (handler-case
      (let* ((request (request-with-relative-files request (uiop:pathname-directory-pathname file)))
             (text (if (probe-file file) (uiop:read-file-string file) ""))
             (old (and (equal (and (ap-file page) (namestring (ap-file page))) (namestring file))
                       (find (ap-saved-name page) (parse-http-file text) :key #'http-request-name :test #'equal)))
             (same-name (find (http-request-name request) (parse-http-file text) :key #'http-request-name :test #'equal))
             (new-text (cond (old (replace-http-request text old request))
                             (same-name (replace-http-request text same-name request))
                             (t (append-http-request text request)))))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
          (write-string new-text out))
        (setf (ap-file page) (pathname file)
              (ap-saved-name page) (http-request-name request)
              (ap-name page) (http-request-name request)
              (buffer-name (ap-buffer page)) (api-tab-name request)
              (buffer-modified-p (ap-buffer page)) nil)
        (update-tab-titles *window* (ap-buffer page))
        (let ((open (find-file-buffer file)))
          (when (and open (not (buffer-modified-p open)))
            (ignore-errors (reload-buffer-text open new-text))))
        (refresh-api-page)
        (message "Saved ~a in ~a" (http-request-name request) (api-file-label file))
        t)
    (error (e) (message "Couldn't save the request: ~a" e) nil)))

(defun delete-api-request (file request)
  (let ((dialog (adw:alert-dialog-new (format nil "Delete “~a”?" (http-request-title request))
                                      (format nil "It goes from ~a." (api-file-label file)))))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "delete" "_Delete")
    (adw:alert-dialog-set-response-appearance dialog "delete" :destructive)
    (adw:alert-dialog-set-default-response dialog "cancel")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (when (string= response "delete")
                   (let* ((text (uiop:read-file-string file))
                          (found (find (http-request-name request) (parse-http-file text)
                                       :key #'http-request-name :test #'equal)))
                     (when found
                       (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
                         (write-string (remove-http-request text found) out))
                       (refresh-api-page)
                       (message "Deleted ~a" (http-request-title request)))))))))

;;; curl command lines

(defun copy-request-as-curl (request &key file)
  "Copy a curl command for REQUEST, with its variables' values put in when they all have one."
  (let ((resolved (handler-case (resolved-request request :file file) (editor-error () nil))))
    (copy-to-clipboard (http-request-curl-command (or resolved request)
                                                  :directory (or (and file (uiop:pathname-directory-pathname file)) (api-project))))
    (message "Copied the curl command~:[ (with its {{variables}}: some have no value)~;~]" resolved)))

(define-command api-import-curl ()
  "Make a request from a curl command line (as browsers' Copy as cURL gives)."
  (let* ((dialog (make-instance 'adw:dialog :title "Import curl Command" :content-width 560 :content-height 320))
         (text (make-instance 'gtk:text-view :monospace t :wrap-mode :word-char :top-margin 8 :left-margin 8 :right-margin 8))
         (import (make-instance 'gtk:button :label "_Import" :use-underline t :css-classes '("suggested-action")))
         (cancel (make-instance 'gtk:button :label "_Cancel" :use-underline t))
         (header (make-instance 'adw:header-bar :show-end-title-buttons nil :show-start-title-buttons nil))
         (toolbar (make-instance 'adw:toolbar-view)))
    (gobject:connect cancel :clicked (lambda (b) (declare (ignore b)) (adw:dialog-close dialog)))
    (gobject:connect import :clicked
                     (lambda (b) (declare (ignore b))
                       (let ((request (parse-curl-command (text-string (gtk:text-view-get-buffer text)))))
                         (if (null request)
                             (message "That isn't a curl command with a URL")
                             (progn (adw:dialog-close dialog)
                                    (open-api-request request))))))
    (adw:header-bar-pack-start header cancel)
    (adw:header-bar-pack-end header import)
    (adw:toolbar-view-add-top-bar toolbar header)
    (adw:toolbar-view-set-content toolbar (make-instance 'gtk:scrolled-window :child text :vexpand t))
    (adw:dialog-set-child dialog toolbar)
    (adw:dialog-present dialog (window-gtk-window *window*))
    (gtk:widget-grab-focus text)))
