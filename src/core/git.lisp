;;;; git.lisp — what Git says about a project, through the git command
;;;;
;;;; Everything runs `git` (*GIT-PROGRAM*) and reads its output: the
;;;; repository a file is in, the branch, the status of each changed file,
;;;; a file's text at HEAD, and the changed lines between two texts (from
;;;; `git diff --no-index -U0`, so a buffer's unsaved text can be compared
;;;; with HEAD). Staging, unstaging, discarding and committing are here
;;;; too. Nothing here touches GTK; the callers decide which thread.

(in-package #:cadre)

(defvar *git-program* "git" "The git command.")

(defun git (directory &rest arguments)
  "Run git in DIRECTORY with ARGUMENTS. Returns its output and exit code."
  (multiple-value-bind (output error code)
      (uiop:run-program (list* *git-program* "-C" (uiop:native-namestring directory) arguments)
                        :output :string :error-output :string :ignore-error-status t
                        :external-format :utf-8)
    (values output code error)))

(defun git-ok (directory &rest arguments)
  "Run git; return its output, or signal an editor-error with git's message."
  (multiple-value-bind (output code error) (apply #'git directory arguments)
    (unless (zerop code)
      (error 'editor-error :message (string-trim '(#\Space #\Newline)
                                                 (if (plusp (length error)) error output))))
    output))

(defun git-available-p ()
  (ignore-errors (zerop (nth-value 1 (git (user-homedir-pathname) "--version")))))

(defun git-toplevel (path)
  "The root of the Git work tree containing PATH (a file or folder), or nil."
  (let ((directory (if (uiop:directory-pathname-p path) path (uiop:pathname-directory-pathname path))))
    (when (uiop:directory-exists-p directory)
      (multiple-value-bind (output code) (ignore-errors (git directory "rev-parse" "--show-toplevel"))
        (and code (zerop code)
             (uiop:ensure-directory-pathname (string-trim '(#\Newline #\Return) output)))))))

(defun git-relative-path (root path)
  "PATH relative to ROOT, with forward slashes, as git names it."
  (let ((root (namestring (or (ignore-errors (truename root)) root)))
        (path (namestring (or (ignore-errors (truename path)) path))))
    (if (and (> (length path) (length root)) (string= root path :end2 (length root)))
        (subseq path (length root))
        path)))

(defun git-branch (root)
  "The current branch's name (or a short commit id when detached), or nil."
  (multiple-value-bind (output code) (git root "symbolic-ref" "--short" "-q" "HEAD")
    (if (zerop code)
        (string-trim '(#\Newline) output)
        (multiple-value-bind (output code) (git root "rev-parse" "--short" "HEAD")
          (and (zerop code) (format nil "(~a)" (string-trim '(#\Newline) output)))))))

(defun git-head-id (root)
  (multiple-value-bind (output code) (git root "rev-parse" "-q" "--verify" "HEAD")
    (and (zerop code) (string-trim '(#\Newline) output))))

(defun git-head-text (root relative)
  "The text of RELATIVE (a path from ROOT) at HEAD, or nil if HEAD doesn't have it."
  (multiple-value-bind (output code) (git root "show" (format nil "HEAD:~a" relative))
    (and (zerop code) output)))

;;; Status

(defun parse-git-status (output)
  "Entries from `git status --porcelain=v1 -z`: (index worktree path original),
INDEX and WORKTREE being characters such as #\\M, #\\A, #\\D, #\\? or #\\Space."
  (let ((fields (uiop:split-string output :separator (string (code-char 0))))
        (entries '()))
    (loop while fields
          do (let ((field (pop fields)))
               (when (>= (length field) 4)
                 (let ((x (char field 0)) (y (char field 1)) (path (subseq field 3)))
                   ;; A rename or copy is followed by the original name.
                   (push (list x y path (and (member x '(#\R #\C)) (pop fields))) entries)))))
    (nreverse entries)))

(defun git-status (root)
  "The changed files under ROOT, as from PARSE-GIT-STATUS."
  (parse-git-status (git-ok root "status" "--porcelain=v1" "-z" "--untracked-files=all")))

(defun git-status-kind (index worktree)
  "One word for a status pair, for colors: :conflict, :untracked, :added, :deleted, :renamed or :modified."
  (cond ((or (char= index #\U) (char= worktree #\U) (and (char= index #\A) (char= worktree #\A))
             (and (char= index #\D) (char= worktree #\D)))
         :conflict)
        ((char= index #\?) :untracked)
        ((or (char= worktree #\D) (char= index #\D)) :deleted)
        ((char= index #\A) :added)
        ((char= index #\R) :renamed)
        (t :modified)))

;;; Changed lines

(defun parse-hunk-header (line)
  "(old-start old-count new-start new-count) from \"@@ -a,b +c,d @@\"."
  (flet ((range (string)
           (let ((comma (position #\, string)))
             (if comma
                 (list (parse-integer string :end comma) (parse-integer string :start (1+ comma)))
                 (list (parse-integer string) 1)))))
    (let* ((minus (position #\- line))
           (plus (position #\+ line :start minus))
           (old (range (subseq line (1+ minus) (position #\Space line :start minus))))
           (new (range (subseq line (1+ plus) (position #\Space line :start plus)))))
      (append old new))))

(defun parse-unified-hunks (output)
  "The hunks of a unified diff with no context, as (old-start old-count new-start new-count)."
  (loop for line in (split-text-lines output)
        when (and (> (length line) 3) (string= "@@ " line :end2 3))
          collect (parse-hunk-header line)))

(defun line-changes (old-text new-text)
  "How NEW-TEXT's lines differ from OLD-TEXT's, as hunks (old-start old-count
new-start new-count), lines counted from 1 as diff does."
  (let ((old (uiop:with-temporary-file (:stream s :pathname p :keep t :type "old" :external-format :utf-8) (write-string old-text s) p))
        (new (uiop:with-temporary-file (:stream s :pathname p :keep t :type "new" :external-format :utf-8) (write-string new-text s) p)))
    (unwind-protect
         (parse-unified-hunks
          (git (uiop:pathname-directory-pathname old) "diff" "--no-index" "--no-color" "--no-ext-diff" "-U0"
               "--" (uiop:native-namestring old) (uiop:native-namestring new)))
      (ignore-errors (delete-file old))
      (ignore-errors (delete-file new)))))

(defun hunk-kind (hunk)
  (destructuring-bind (old-start old-count new-start new-count) hunk
    (declare (ignore old-start new-start))
    (cond ((zerop old-count) :added)
          ((zerop new-count) :deleted)
          (t :modified))))

;;; Changing things

(defun git-stage (root paths) (apply #'git-ok root "add" "--" paths))

(defun git-unstage (root paths)
  "Take PATHS out of the index (back to HEAD, or untracked in a repository with no commits)."
  (if (git-head-id root)
      (apply #'git-ok root "restore" "--staged" "--" paths)
      (apply #'git-ok root "rm" "--cached" "-q" "--" paths)))

(defun git-discard (root paths)
  "Put tracked PATHS back as they are in the index, losing their changes."
  (apply #'git-ok root "restore" "--" paths))

(defun git-commit (root message &key amend)
  "Commit what's staged with MESSAGE. Returns git's output."
  (let ((file (uiop:with-temporary-file (:stream s :pathname p :keep t :type "msg" :external-format :utf-8) (write-string message s) p)))
    (unwind-protect
         (apply #'git-ok root "commit" "-F" (uiop:native-namestring file) (and amend (list "--amend")))
      (ignore-errors (delete-file file)))))

(defun git-diff-text (root relative &key staged head)
  "The unified diff of RELATIVE: the working file against the index; with
STAGED the index against HEAD; with HEAD the working file against HEAD."
  (values (apply #'git root "diff" "--no-color" "--no-ext-diff"
                 (append (cond (staged (list "--cached")) (head (list "HEAD")))
                         (list "--" relative)))))

;;; Branches

(defun git-branches (root)
  "The branches, as plists (:name :current :remote :upstream), local ones first."
  (let ((output (git-ok root "for-each-ref" "--format=%(refname)%09%(refname:short)%09%(HEAD)%09%(upstream:short)"
                        "refs/heads" "refs/remotes")))
    (loop for line in (split-text-lines output)
          for fields = (uiop:split-string line :separator (string #\Tab))
          when (and (= (length fields) 4)
                    ;; origin/HEAD is an alias, not a branch.
                    (not (search "/HEAD" (first fields) :from-end t :start2 (max 0 (- (length (first fields)) 5)))))
            collect (destructuring-bind (ref name head upstream) fields
                      (list :name name :current (string= head "*")
                            :remote (and (>= (length ref) 13) (string= "refs/remotes/" ref :end2 13))
                            :upstream (and (plusp (length upstream)) upstream))))))

(defun git-switch (root name &key create track)
  "Switch to branch NAME; with CREATE, make it first (from HEAD); with TRACK,
make a local branch following the remote branch NAME (\"origin/x\")."
  (cond (create (git-ok root "switch" "-c" name))
        (track (git-ok root "switch" "--track" name))
        (t (git-ok root "switch" name))))

(defun git-delete-branch (root name &key force)
  (git-ok root "branch" (if force "-D" "-d") name))

(defun git-remotes (root)
  (split-text-lines (string-trim '(#\Newline) (git-ok root "remote"))))

(defun git-upstream (root)
  "The current branch's upstream (\"origin/main\"), or nil."
  (multiple-value-bind (output code) (git root "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{upstream}")
    (and (zerop code) (string-trim '(#\Newline) output))))

(defun git-ahead-behind (root)
  "How many commits the current branch is ahead of and behind its upstream,
or nil if it has none."
  (multiple-value-bind (output code) (git root "rev-list" "--left-right" "--count" "@{upstream}...HEAD")
    (when (zerop code)
      (destructuring-bind (behind ahead) (mapcar #'parse-integer (uiop:split-string (string-trim '(#\Newline) output)
                                                                                      :separator '(#\Tab #\Space)))
        (values ahead behind)))))

;;; Talking to remotes, without ever waiting for a password

(defparameter *git-network-environment*
  '("GIT_TERMINAL_PROMPT=0"
    "GIT_SSH_COMMAND=ssh -o BatchMode=yes -o ConnectTimeout=20")
  "Added to git's environment for fetch, pull and push: it fails instead of
asking for a password (a credential helper or ssh-agent must supply it).")

(defun git-network (root &rest arguments)
  "Run a git command that talks to a remote. Returns git's output, or signals
an editor-error with its message."
  (let* ((output (make-string-output-stream))
         (error-output (make-string-output-stream))
         (process (sb-ext:run-program *git-program* (list* "-C" (uiop:native-namestring root) arguments)
                                      :search t :input nil :output output :error error-output :wait t
                                      :environment (append *git-network-environment* (sb-ext:posix-environ))))
         (code (sb-ext:process-exit-code process))
         (out (get-output-stream-string output))
         (err (get-output-stream-string error-output)))
    (unless (zerop code)
      (error 'editor-error :message (string-trim '(#\Space #\Newline)
                                                 (if (plusp (length err)) err out))))
    (concatenate 'string out err)))

(defun git-fetch (root) (git-network root "fetch" "--prune"))

(defun git-pull (root &key (mode :ff-only))
  "Pull the current branch's upstream: MODE :ff-only (only if it fast-forwards),
:merge or :rebase."
  (git-network root "pull" (ecase mode (:ff-only "--ff-only") (:merge "--no-rebase") (:rebase "--rebase"))))

(defun git-push (root &key set-upstream)
  "Push the current branch. With SET-UPSTREAM (a remote name), push it there
and make that its upstream."
  (if set-upstream
      (git-network root "push" "-u" set-upstream "HEAD")
      (git-network root "push")))

;;; History

(defun git-log (root &key path (count 200) (skip 0))
  "Commits, newest first, as plists (:hash :short :author :time :subject),
TIME being universal time. With PATH, only commits that changed it (following renames)."
  (let ((output (apply #'git root "log" (format nil "--format=%H~a%h~a%an~a%at~a%s~a"
                                                 #\Us #\Us #\Us #\Us #\Rs)
                       (format nil "-n~d" count) (format nil "--skip=~d" skip)
                       (and path (list "--follow" "--" path)))))
    (loop for record in (uiop:split-string output :separator (string #\Rs))
          for fields = (uiop:split-string (string-trim '(#\Newline) record) :separator (string #\Us))
          when (= (length fields) 5)
            collect (destructuring-bind (hash short author time subject) fields
                      (list :hash hash :short short :author author
                            :time (+ (parse-integer time) (encode-universal-time 0 0 0 1 1 1970 0))
                            :subject subject)))))

(defun git-show-text (root hash &key path)
  "The commit HASH: its message and diff (only PATH's, with PATH)."
  (values (apply #'git root "show" "--no-color" "--no-ext-diff" "--format=fuller" hash
                 (and path (list "--" path)))))

(defun relative-time (time &optional (now (get-universal-time)))
  "TIME (universal time) as \"just now\", \"5 minutes ago\", \"3 days ago\" …"
  (let ((seconds (- now time)))
    (flet ((ago (n unit) (format nil "~d ~a~p ago" n unit n)))
      (cond ((< seconds 60) "just now")
            ((< seconds 3600) (ago (floor seconds 60) "minute"))
            ((< seconds 86400) (ago (floor seconds 3600) "hour"))
            ((< seconds (* 86400 30)) (ago (floor seconds 86400) "day"))
            ((< seconds (* 86400 365)) (ago (floor seconds (* 86400 30)) "month"))
            (t (ago (floor seconds (* 86400 365)) "year"))))))

;;; Blame

(defun parse-blame-porcelain (output)
  "Per line, a plist (:hash :short :author :time :summary) from `git blame --porcelain`."
  (let ((commits (make-hash-table :test 'equal))
        (lines '())
        (current nil))
    (dolist (line (split-text-lines output))
      (cond ((and (>= (length line) 41) (every (lambda (c) (digit-char-p c 16)) (subseq line 0 40))
                  (char= (char line 40) #\Space))
             (let ((hash (subseq line 0 40)))
               (setf current (or (gethash hash commits)
                                 (setf (gethash hash commits)
                                       (list :hash hash :short (subseq hash 0 7)
                                             :uncommitted (every (lambda (c) (char= c #\0)) hash)))))))
            ((and current (> (length line) 7) (string= "author " line :end2 7))
             (setf (getf current :author) (subseq line 7)))
            ((and current (> (length line) 12) (string= "author-time " line :end2 12))
             (setf (getf current :time) (+ (parse-integer line :start 12) (encode-universal-time 0 0 0 1 1 1970 0))))
            ((and current (> (length line) 8) (string= "summary " line :end2 8))
             (setf (getf current :summary) (subseq line 8)))
            ((and current (plusp (length line)) (char= (char line 0) #\Tab))
             ;; The line itself: the commit's details are complete by now.
             (setf (gethash (getf current :hash) commits) current)
             (push current lines))))
    (coerce (nreverse lines) 'vector)))

(defun git-blame (root relative &optional contents)
  "Who last changed each line of RELATIVE, as from PARSE-BLAME-PORCELAIN.
With CONTENTS (the text being edited), blame that instead of the saved
file; lines not committed yet have :uncommitted t."
  (if contents
      (let ((file (uiop:with-temporary-file (:stream s :pathname p :keep t :type "blame" :external-format :utf-8)
                    (write-string contents s) p)))
        (unwind-protect
             (parse-blame-porcelain (git-ok root "blame" "--porcelain" "--contents" (uiop:native-namestring file)
                                            "--" relative))
          (ignore-errors (delete-file file))))
      (parse-blame-porcelain (git-ok root "blame" "--porcelain" "--" relative))))

;;; Stashes

(defun git-stashes (root)
  "The stashes, newest first, as plists (:ref \"stash@{0}\" :subject)."
  (loop for line in (split-text-lines (git root "stash" "list" (format nil "--format=%gd~a%s" #\Us)))
        for fields = (uiop:split-string line :separator (string #\Us))
        when (= (length fields) 2)
          collect (list :ref (first fields) :subject (second fields))))

(defun git-stash-push (root &key message (untracked t))
  "Put the uncommitted changes (and new files, with UNTRACKED) in a stash."
  (apply #'git-ok root "stash" "push" (append (and untracked (list "--include-untracked"))
                                               (and message (plusp (length message)) (list "-m" message)))))

(defun git-stash-apply (root ref) (git-ok root "stash" "apply" ref))
(defun git-stash-pop (root ref) (git-ok root "stash" "pop" ref))
(defun git-stash-drop (root ref) (git-ok root "stash" "drop" ref))

;;; Staging one change at a time

(defun diff-file-header (diff)
  "The lines of DIFF (a unified diff of one file) before its first hunk."
  (loop for line in (split-text-lines diff)
        until (and (> (length line) 2) (string= "@@" line :end2 2))
        collect line))

(defun diff-hunks (diff)
  "DIFF's hunks, each (header-line body-lines old-start old-count new-start new-count)."
  (let ((hunks '()) (current nil))
    (dolist (line (split-text-lines diff))
      (cond ((and (> (length line) 2) (string= "@@" line :end2 2))
             (when current (push current hunks))
             (setf current (list line '())))
            ((and current (plusp (length line)) (member (char line 0) '(#\Space #\+ #\- #\\)))
             (push line (second current)))))
    (when current (push current hunks))
    (mapcar (lambda (h) (append (list (first h) (reverse (second h))) (parse-hunk-header (first h))))
            (nreverse hunks))))

(defun hunk-patch (header hunk)
  "A patch of one HUNK of a file, with the file's diff HEADER lines."
  (format nil "~{~a~%~}~a~%~{~a~%~}" header (first hunk) (second hunk)))

(defun git-apply-patch (root patch &key cached reverse zero-context)
  "Apply PATCH (text) to the index (CACHED) or the working tree."
  (let ((file (uiop:with-temporary-file (:stream s :pathname p :keep t :type "patch" :external-format :utf-8)
                (write-string patch s) p)))
    (unwind-protect
         (apply #'git-ok root "apply" (append (and cached (list "--cached")) (and reverse (list "--reverse"))
                                              (and zero-context (list "--unidiff-zero"))
                                              (list "--whitespace=nowarn" (uiop:native-namestring file))))
      (ignore-errors (delete-file file)))))

(defun hunk-new-range (hunk)
  "The lines (from 1) a hunk covers in the new text, as first and last; a
deletion covers the line before it."
  (destructuring-bind (old-start old-count new-start new-count) (cddr hunk)
    (declare (ignore old-start old-count))
    (if (zerop new-count) (values new-start new-start) (values new-start (+ new-start new-count -1)))))

(defun git-stage-lines (root relative first last)
  "Stage the saved changes of RELATIVE that touch lines FIRST to LAST (from 1).
Returns how many changes were staged."
  (let* ((diff (git root "diff" "--no-color" "--no-ext-diff" "-U0" "--" relative))
         (header (diff-file-header diff))
         (hunks (remove-if-not (lambda (h) (multiple-value-bind (a b) (hunk-new-range h) (and (<= a last) (>= b first))))
                               (diff-hunks diff))))
    (dolist (h hunks)
      (git-apply-patch root (hunk-patch header h) :cached t :zero-context t))
    (length hunks)))

;;; Merge conflicts

(defun find-conflicts (text)
  "The conflict regions in TEXT, as plists of lines (from 0): :start (the
<<<<<<< line), :base (a ||||||| line, or nil), :middle (=======), :end (>>>>>>>)."
  (let ((conflicts '()) (start nil) (base nil) (middle nil))
    (loop for line in (split-text-lines text)
          for n from 0
          do (flet ((marker-p (prefix) (and (>= (length line) 7) (string= prefix line :end2 7)
                                            (or (= (length line) 7) (char= (char line 7) #\Space)))))
               (cond ((marker-p "<<<<<<<") (setf start n base nil middle nil))
                     ((and start (not middle) (marker-p "|||||||")) (setf base n))
                     ((and start (not middle) (string= line "=======")) (setf middle n))
                     ((and start middle (marker-p ">>>>>>>"))
                      (push (list :start start :base base :middle middle :end n) conflicts)
                      (setf start nil base nil middle nil)))))
    (nreverse conflicts)))

(defun conflict-resolution (lines conflict choice)
  "The lines that replace CONFLICT (in LINES, a vector) for CHOICE: :ours,
:theirs or :both."
  (destructuring-bind (&key start base middle end) conflict
    (let ((ours (loop for i from (1+ start) below (or base middle) collect (aref lines i)))
          (theirs (loop for i from (1+ middle) below end collect (aref lines i))))
      (ecase choice
        (:ours ours)
        (:theirs theirs)
        (:both (append ours theirs))))))

(defun git-directory (root)
  (let ((dir (string-trim '(#\Newline) (git-ok root "rev-parse" "--git-dir"))))
    (uiop:ensure-directory-pathname (merge-pathnames dir root))))

(defun git-operation (root)
  "What's in progress in ROOT: :merge, :rebase, :cherry-pick, :revert, or nil."
  (let ((dir (ignore-errors (git-directory root))))
    (when dir
      (cond ((or (uiop:directory-exists-p (merge-pathnames "rebase-merge/" dir))
                 (uiop:directory-exists-p (merge-pathnames "rebase-apply/" dir)))
             :rebase)
            ((probe-file (merge-pathnames "MERGE_HEAD" dir)) :merge)
            ((probe-file (merge-pathnames "CHERRY_PICK_HEAD" dir)) :cherry-pick)
            ((probe-file (merge-pathnames "REVERT_HEAD" dir)) :revert)))))

(defun git-merge-message (root)
  "The message git prepared for the merge commit, or nil."
  (let ((file (merge-pathnames "MERGE_MSG" (git-directory root))))
    (and (probe-file file)
         (format nil "~{~a~^~%~}" (remove-if (lambda (l) (and (plusp (length l)) (char= (char l 0) #\#)))
                                            (split-text-lines (uiop:read-file-string file)))))))

(defun git-merge (root branch)
  "Merge BRANCH into the current branch. Returns git's output; a merge with
conflicts signals an editor-error whose message says so."
  (git-ok root "merge" "--no-edit" branch))

(defun git-abort (root operation)
  (ecase operation
    (:merge (git-ok root "merge" "--abort"))
    (:rebase (git-ok root "rebase" "--abort"))
    (:cherry-pick (git-ok root "cherry-pick" "--abort"))
    (:revert (git-ok root "revert" "--abort"))))

(defun git-continue (root operation)
  "Go on with OPERATION once its conflicts are resolved and staged (without
opening an editor for messages)."
  (let ((*git-network-environment* (list "GIT_EDITOR=true")))
    (ecase operation
      (:merge (git-network root "commit" "--no-edit"))
      (:rebase (git-network root "rebase" "--continue"))
      (:cherry-pick (git-network root "cherry-pick" "--continue"))
      (:revert (git-network root "revert" "--continue")))))
