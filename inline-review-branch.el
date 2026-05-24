;;; inline-review-branch.el --- Branch checkout and restore for inline-review -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Branch checkout, original-branch save/restore, and worktree stash
;; management for inline-review.
;;
;; Public API (called from inline-review.el):
;;   `inline-review--review-in-progress-p'
;;   `inline-review--checkout-branch-for-review'
;;   `inline-review--save-original-branch'
;;   `inline-review--load-original-branch'
;;   `inline-review--pop-stash'

;;; Code:

(require 'cl-lib)
(require 'inline-review-backend)

;; Forward declaration — the authoritative defvar is in inline-review.el.
(defvar inline-review-mode)

;;;; ─── Owner / Repo String ────────────────────────────────────────────────────

(defun inline-review--owner-repo-string ()
  "Return a sanitized `owner_repo' string from `inline-review--project-info'.
GitHub uses owner/repo from the project-info alist.
GitLab/Gongfeng use the URL-decoded `project-id' with slashes replaced.
Non-alphanumeric characters (outside `._-') are replaced with underscores
so the result is safe for a git branch name."
  (let ((owner (alist-get 'owner inline-review--project-info))
        (repo (alist-get 'repo inline-review--project-info))
        (project-id (alist-get 'project-id inline-review--project-info)))
    (let ((s
           (cond
            ((and owner repo)
             (format "%s_%s" owner repo))
            (project-id
             (let ((decoded (url-unhex-string project-id)))
               (replace-regexp-in-string "/" "_" decoded)))
            (t "unknown"))))
      (replace-regexp-in-string "[^A-Za-z0-9._-]" "_" s))))

;;;; ─── Original Branch Save / Restore ─────────────────────────────────────────

(defun inline-review--load-original-branch ()
  "Return the persisted original branch for the current repo, or nil."
  (when-let ((root (inline-review--git-root)))
    (let ((file
           (expand-file-name "inline-review-original-branch"
                             (expand-file-name ".git" root))))
      (when (file-readable-p file)
        (string-trim
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))))))

(defun inline-review--save-original-branch ()
  "Save the current git branch to `.git/inline-review-original-branch'.
Only writes the file if it does not already exist, so the true original
branch is preserved across repeated `review-url' calls in the same
session."
  (when-let ((root (inline-review--git-root)))
    (let ((file
           (expand-file-name "inline-review-original-branch"
                             (expand-file-name ".git" root))))
      (unless (file-exists-p file)
        (let ((current
               (string-trim
                (shell-command-to-string
                 "git rev-parse --abbrev-ref HEAD 2>/dev/null"))))
          (when (and (not (string-empty-p current))
                     (not (string= current "HEAD")))
            (write-region current nil file nil 'silent)))))))

;;;; ─── Worktree Stash ─────────────────────────────────────────────────────────

(defun inline-review--stash-worktree ()
  "Stash the current worktree if dirty and record the stash SHA.
Only tracked changes (staged or unstaged modifications) trigger a stash;
untracked files are deliberately ignored so that a workspace containing
only new/untracked files is not considered dirty.
Returns t if a stash was created, nil if the worktree was already clean.
Signals an error if the stash command fails."
  (let* ((default-directory
          (or (inline-review--git-root) default-directory))
         (raw (shell-command-to-string "git status --porcelain 2>/dev/null"))
         ;; Exclude untracked ("?? ") and ignored ("!! ") lines; only tracked
         ;; changes (modifications, deletions, renames, copies) are relevant.
         (tracked-lines
          (cl-remove-if (lambda (l) (string-match-p "^[?!][?!] " l))
                        (split-string raw "\n" t))))
    (when tracked-lines
      (let ((errbuf (get-buffer-create " *crm-stash-err*")))
        (with-current-buffer errbuf (erase-buffer))
        (let ((rc (call-process "git" nil (list errbuf t) nil
                                "stash" "push" "-m"
                                "inline-review auto-stash")))
          (if (and (integerp rc) (zerop rc))
              (let ((sha (string-trim
                          (shell-command-to-string
                           "git rev-parse stash@{0} 2>/dev/null"))))
                (if (string-empty-p sha)
                    (user-error
                     "inline-review: stash push succeeded \
but could not resolve stash@{0}")
                  (inline-review--record-stash sha)
                  (message "inline-review: stashed local changes (%s)"
                           (substring sha 0 (min 8 (length sha))))
                  t))
            (let ((err (with-current-buffer errbuf (buffer-string))))
              (user-error
               "inline-review: git stash failed%s"
               (if (string-empty-p err)
                   ""
                 (format " \u2014 %s" (string-trim err)))))))))))

(defun inline-review--record-stash (sha)
  "Record SHA as the stash commit created for this review session.
SHA is the full commit hash returned by `git rev-parse stash@{0}' immediately
after the stash push, and is used by `inline-review--pop-stash' to
locate the exact stash entry even if other stashes are pushed in between."
  (when-let ((root (inline-review--git-root)))
    (let ((file (expand-file-name "inline-review-stash"
                                  (expand-file-name ".git" root))))
      (write-region sha nil file nil 'silent))))

(defun inline-review--find-stash-ref (sha)
  "Return the stash ref (e.g. \"stash@{2}\") whose commit hash equals SHA.
Returns nil if no entry in the current stash list matches."
  (let* ((list-buf (generate-new-buffer " *crm-stash-list*"))
         (rc (call-process "git" nil list-buf nil
                           "stash" "list" "--format=%H %gd"))
         (output (with-current-buffer list-buf
                   (prog1 (buffer-string) (kill-buffer list-buf)))))
    (when (and (integerp rc) (zerop rc))
      (let ((match
             (cl-find-if (lambda (line) (string-prefix-p sha line))
                         (split-string output "\n" t))))
        (when match
          ;; Line format: "<full-sha> stash@{N}"
          (cadr (split-string match " " t)))))))

(defun inline-review--pop-stash ()
  "Pop the auto-stash if one was recorded for this review session.
Reads the stash commit SHA saved by `inline-review--record-stash',
locates that exact entry in the stash list (so intervening stashes pushed
by the user do not get accidentally applied), and pops it by ref.  If the
SHA is no longer in the stash list the sentinel is removed with a warning."
  (when-let ((root (inline-review--git-root)))
    (let ((file (expand-file-name "inline-review-stash"
                                  (expand-file-name ".git" root))))
      (when (file-exists-p file)
        (let* ((saved-sha (string-trim
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))
               (default-directory root))
          (if (string-empty-p saved-sha)
              ;; Legacy empty sentinel (pre-SHA scheme): remove and skip.
              (progn
                (delete-file file)
                (message "inline-review: legacy stash sentinel \
(no SHA recorded); skipping restore to avoid popping wrong stash"))
            (let ((stash-ref (inline-review--find-stash-ref saved-sha)))
              (if (null stash-ref)
                  (progn
                    (delete-file file)
                    (message
                     "inline-review: recorded stash %s not found \
in stash list; skipping restore"
                     (substring saved-sha 0 (min 8 (length saved-sha)))))
                (let ((errbuf (get-buffer-create " *crm-stash-err*")))
                  (with-current-buffer errbuf (erase-buffer))
                  (let ((rc (call-process "git" nil (list errbuf t) nil
                                          "stash" "pop" stash-ref)))
                    (if (and (integerp rc) (zerop rc))
                        (progn
                          (delete-file file)
                          (message
                           "inline-review: restored stashed \
changes (%s)" stash-ref))
                      (let ((err (with-current-buffer errbuf (buffer-string))))
                        (message
                         "inline-review: git stash pop %s failed%s"
                         stash-ref
                         (if (string-empty-p err)
                             ""
                           (format " \u2014 %s" (string-trim err))))))))))))))))

;;;; ─── Remote Sync ────────────────────────────────────────────────────────────

(defun inline-review--pull-current-branch ()
  "Pull the current branch from its upstream if one is configured.
Runs `git pull --ff-only'; silently skips when no upstream is set.
Returns t if a pull was performed, nil otherwise."
  (let* ((default-directory
          (or (inline-review--git-root) default-directory))
         (upstream
          (string-trim
           (shell-command-to-string
            "git rev-parse --abbrev-ref @{u} 2>/dev/null"))))
    (when (and (not (string-empty-p upstream))
               (not (string-match-p "^fatal" upstream)))
      (let ((errbuf (get-buffer-create " *crm-pull-err*")))
        (with-current-buffer errbuf (erase-buffer))
        (let ((rc (call-process "git" nil (list errbuf t) nil
                                "pull" "--ff-only")))
          (if (and (integerp rc) (zerop rc))
              (progn
                (message
                 "inline-review: pulled latest changes from %s"
                 upstream)
                t)
            (let ((err (with-current-buffer errbuf (buffer-string))))
              (message
               "inline-review: git pull --ff-only failed%s \
(proceeding with local version)"
               (if (string-empty-p err)
                   ""
                 (format " \u2014 %s" (string-trim err))))
              nil))))))  )

;;;; ─── Reentrancy Guard ───────────────────────────────────────────────────────

(defun inline-review--review-in-progress-p ()
  "Return non-nil if a review session is currently active for the current project.
Checks the current git root only, so reviews in other projects are unaffected.
Checks for:
- a fully-prepared review recorded in `inline-review--review-active-cache'
- a saved original-branch file (crash-recovery: survives an Emacs restart)"
  (let ((root (inline-review--git-root)))
    (or
     ;; Check the in-memory cache keyed by git root (nil when outside a repo).
     (gethash root inline-review--review-active-cache)
     ;; Saved original branch on disk (crash-recovery)
     (inline-review--load-original-branch))))

;;;; ─── Auto Checkout via Forge Refs ───────────────────────────────────────────

(defconst inline-review--mr-ref-formats
  '((github   . "pull/%d/head")
    (gitlab   . "merge-requests/%d/head")
    (gongfeng . "merge-requests/%d/head")
    (codeberg . "pull/%d/head"))
  "Backend → server-side ref pattern for the MR/PR head commit.
GitHub and Codeberg/Forgejo publish `refs/pull/<id>/head'; GitLab and
Gongfeng publish `refs/merge-requests/<iid>/head'.  These refs can be
fetched directly via git, so the source branch can be checked out without
any extra backend API call.")

(defun inline-review--auto-checkout-source-branch ()
  "Fetch and checkout the source branch of the current MR/PR via git refs.

Uses the well-known ref published by the forge for the MR/PR head — see
`inline-review--mr-ref-formats' — so no backend API call is made.
The ref is fetched from `origin' into FETCH_HEAD; a local branch named
`<owner_repo>_<iid>' is then created or reset to that commit and
checked out.  The current buffer is reverted on success.

Returns t on success, nil on failure or unsupported backend.  Callers
should fall back to a manual flow when nil is returned."
  (let* ((backend inline-review--current-backend)
         (iid inline-review--mr-iid)
         (fmt (alist-get backend inline-review--mr-ref-formats)))
    (when (and backend iid fmt)
      (let* ((default-directory
              (or (inline-review--git-root) default-directory))
             (ref (format fmt iid))
             (local (format "%s_%d" (inline-review--owner-repo-string)
                            iid))
             (errbuf (get-buffer-create " *crm-fetch-err*")))
        (with-current-buffer errbuf (erase-buffer))
        ;; Step 1: fetch the head ref into FETCH_HEAD.
        (let ((fetch-rc
               (call-process "git" nil (list errbuf t) nil
                             "fetch" "origin" ref)))
          (when (and (integerp fetch-rc) (zerop fetch-rc))
            ;; Step 2: stash dirty worktree so checkout cannot fail.
            (inline-review--stash-worktree)
            ;; Step 3: create or reset the local branch from FETCH_HEAD
            ;; and check it out.  `-B' is safe even when already on the
            ;; target branch (it updates the branch ref and working tree).
            (with-current-buffer errbuf (erase-buffer))
            (let ((co-rc
                   (call-process "git" nil (list errbuf t) nil
                                 "checkout" "-B" local "FETCH_HEAD")))
              (when (and (integerp co-rc) (zerop co-rc))
                (message
                 "inline-review: checked out source branch %s"
                 local)
                (inline-review--pull-current-branch)
                (when (and buffer-file-name
                           (file-readable-p buffer-file-name))
                  (revert-buffer t t))
                t))))))))

(defun inline-review--checkout-branch-for-review ()
  "Checkout the source branch for the current MR/PR review.

First saves the current git branch so it can be restored later by
`inline-review-finish-review'.

Then tries `inline-review--auto-checkout-source-branch', which
fetches the well-known forge ref and checks it out — no backend API
call.  On success, no prompt is shown.

If automatic checkout is unsupported or fails (origin unreachable, ref
missing, etc.), falls back to listing local and remote-tracking
branches and prompting the user to pick one.  If a remote-tracking ref
is selected and plain checkout fails, a local tracking branch is
created automatically and named `<owner_repo>_<remote_branch>'.  Reverts
the current buffer after a successful checkout.  Skips silently when
the user accepts the empty default."
  (inline-review--save-original-branch)
  (unless (inline-review--auto-checkout-source-branch)
    (let* ((root (or (inline-review--git-root) default-directory))
           (default-directory root)
           (local-branches
            (split-string
             (shell-command-to-string
              "git branch '--format=%(refname:short)' 2>/dev/null")
             "\n" t))
           (remote-branches
            (split-string
             (shell-command-to-string
              "git branch -r '--format=%(refname:short)' 2>/dev/null")
             "\n" t))
           (all-branches
            (delete-dups (append local-branches remote-branches)))
           (branch
            (if (and (boundp 'inline-review--mr-source-branch)
                     inline-review--mr-source-branch)
                inline-review--mr-source-branch
              (completing-read
               "Checkout branch for review (RET to skip): "
               all-branches
               nil nil nil nil ""))))
      (unless (string-empty-p branch)
        ;; Stash dirty worktree so checkout cannot fail.
        (inline-review--stash-worktree)
        ;; Try plain checkout first (handles local branches and already-fetched
        ;; remote-tracking refs like "origin/foo" via DWIM).
        (let* ((errbuf (get-buffer-create " *crm-checkout-err*"))
               (result
                (progn
                  (with-current-buffer errbuf
                    (erase-buffer))
                  ;; DESTINATION=(errbuf t): stdout→errbuf, stderr merged in.
                  ;; The stderr slot must be nil/t/filename, NOT a buffer object.
                  (call-process "git"
                                nil
                                (list errbuf t)
                                nil
                                "checkout"
                                branch))))
          (when (not (and (integerp result) (zerop result)))
            ;; Plain checkout failed.  If the name looks like a remote-tracking
            ;; ref (e.g. "origin/feature"), create a local tracking branch
            ;; named <owner_repo>_<remote_branch>.
            (let* ((local-name
                    (when (string-match "^[^/]+/\\(.+\\)$" branch)
                      (match-string 1 branch)))
                   (review-name
                    (when local-name
                      (format "%s_%s"
                              (inline-review--owner-repo-string)
                              local-name)))
                   (retry-result
                    (when review-name
                      (with-current-buffer errbuf
                        (erase-buffer))
                      (call-process "git"
                                    nil
                                    (list errbuf t)
                                    nil
                                    "checkout"
                                    "-b"
                                    review-name
                                    "--track"
                                    branch))))
              (if (and (integerp retry-result) (zerop retry-result))
                  (setq branch review-name)
                (let ((err
                       (with-current-buffer errbuf
                         (buffer-string))))
                  (user-error
                   "inline-review: git checkout %s failed%s"
                   branch
                   (if (string-empty-p err)
                       ""
                     (format " \u2014 %s" (string-trim err)))))))
            (message "inline-review: checked out branch %s" branch)
            ;; Pull to sync with remote before rendering overlays.
            (inline-review--pull-current-branch)
            ;; Revert the buffer so its content matches the newly-checked-out
            ;; file; the diff's new-file line numbers reference this version.
            (when (and buffer-file-name
                       (file-readable-p buffer-file-name))
              (revert-buffer t t))))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-branch)

;;; inline-review-branch.el ends here
