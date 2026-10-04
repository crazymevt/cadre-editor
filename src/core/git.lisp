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
