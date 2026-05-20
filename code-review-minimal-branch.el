;;; code-review-minimal-branch.el --- Branch checkout and restore for code-review-minimal -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Branch checkout, original-branch save/restore, and worktree stash
;; management for code-review-minimal.
;;
;; Public API (called from code-review-minimal.el):
;;   `code-review-minimal--review-in-progress-p'
;;   `code-review-minimal--checkout-branch-for-review'
;;   `code-review-minimal--save-original-branch'
;;   `code-review-minimal--load-original-branch'
;;   `code-review-minimal--pop-stash'

;;; Code:

(require 'cl-lib)
(require 'code-review-minimal-backend)

;; Forward declaration — the authoritative defvar is in code-review-minimal.el.
(defvar code-review-minimal-mode)

;;;; ─── Owner / Repo String ────────────────────────────────────────────────────

(defun code-review-minimal--owner-repo-string ()
  "Return a sanitized `owner_repo' string from `code-review-minimal--project-info'.
GitHub uses owner/repo from the project-info alist.
GitLab/Gongfeng use the URL-decoded `project-id' with slashes replaced.
Non-alphanumeric characters (outside `._-') are replaced with underscores
so the result is safe for a git branch name."
  (let ((owner (alist-get 'owner code-review-minimal--project-info))
        (repo (alist-get 'repo code-review-minimal--project-info))
        (project-id (alist-get 'project-id code-review-minimal--project-info)))
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

(defun code-review-minimal--load-original-branch ()
  "Return the persisted original branch for the current repo, or nil."
  (when-let ((root (code-review-minimal--git-root)))
    (let ((file
           (expand-file-name "code-review-minimal-original-branch"
                             (expand-file-name ".git" root))))
      (when (file-readable-p file)
        (string-trim
         (with-temp-buffer
           (insert-file-contents file)
           (buffer-string)))))))

(defun code-review-minimal--save-original-branch ()
  "Save the current git branch to `.git/code-review-minimal-original-branch'.
Only writes the file if it does not already exist, so the true original
branch is preserved across repeated `review-url' calls in the same
session."
  (when-let ((root (code-review-minimal--git-root)))
    (let ((file
           (expand-file-name "code-review-minimal-original-branch"
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

(defun code-review-minimal--stash-worktree ()
  "Stash the current worktree if dirty and record the stash marker.
Returns t if a stash was created, nil if the worktree was already clean.
Signals an error if the stash command fails."
  (let ((default-directory
         (or (code-review-minimal--git-root) default-directory))
        (status
         (string-trim
          (shell-command-to-string
           "git status --porcelain 2>/dev/null"))))
    (when (not (string-empty-p status))
      (let ((errbuf (get-buffer-create " *crm-stash-err*")))
        (with-current-buffer errbuf (erase-buffer))
        (let ((rc (call-process "git" nil (list errbuf t) nil
                                "stash" "push" "-m"
                                "code-review-minimal auto-stash")))
          (if (and (integerp rc) (zerop rc))
              (progn
                (code-review-minimal--record-stash)
                (message
                 "code-review-minimal: stashed local changes")
                t)
            (let ((err (with-current-buffer errbuf (buffer-string))))
              (user-error
               "code-review-minimal: git stash failed%s"
               (if (string-empty-p err)
                   ""
                 (format " — %s" (string-trim err)))))))))))

(defun code-review-minimal--record-stash ()
  "Record that a stash was created for this review session."
  (when-let ((root (code-review-minimal--git-root)))
    (let ((file (expand-file-name "code-review-minimal-stash"
                                  (expand-file-name ".git" root))))
      (write-region "" nil file nil 'silent))))

(defun code-review-minimal--pop-stash ()
  "Pop the auto-stash if one was recorded for this review session."
  (when-let ((root (code-review-minimal--git-root)))
    (let ((file (expand-file-name "code-review-minimal-stash"
                                  (expand-file-name ".git" root))))
      (when (file-exists-p file)
        (let ((default-directory root)
              (errbuf (get-buffer-create " *crm-stash-err*")))
          (with-current-buffer errbuf (erase-buffer))
          (let ((rc (call-process "git" nil (list errbuf t) nil
                                  "stash" "pop")))
            (if (and (integerp rc) (zerop rc))
                (progn
                  (delete-file file)
                  (message
                   "code-review-minimal: restored stashed changes"))
              (let ((err (with-current-buffer errbuf (buffer-string))))
                (message
                 "code-review-minimal: git stash pop failed%s"
                 (if (string-empty-p err)
                     ""
                   (format " — %s" (string-trim err))))))))))))

;;;; ─── Reentrancy Guard ───────────────────────────────────────────────────────

(defun code-review-minimal--review-in-progress-p ()
  "Return non-nil if a review session is currently active for the current project.
Checks the current git root only, so reviews in other projects are unaffected.
Checks for:
- a fully-prepared review recorded in `code-review-minimal--review-active-cache'
- a saved original-branch file (crash-recovery: survives an Emacs restart)"
  (let ((root (code-review-minimal--git-root)))
    (or
     ;; Check the in-memory cache keyed by git root (nil when outside a repo).
     (gethash root code-review-minimal--review-active-cache)
     ;; Saved original branch on disk (crash-recovery)
     (code-review-minimal--load-original-branch))))

;;;; ─── Auto Checkout via Forge Refs ───────────────────────────────────────────

(defconst code-review-minimal--mr-ref-formats
  '((github   . "pull/%d/head")
    (gitlab   . "merge-requests/%d/head")
    (gongfeng . "merge-requests/%d/head")
    (codeberg . "pull/%d/head"))
  "Backend → server-side ref pattern for the MR/PR head commit.
GitHub and Codeberg/Forgejo publish `refs/pull/<id>/head'; GitLab and
Gongfeng publish `refs/merge-requests/<iid>/head'.  These refs can be
fetched directly via git, so the source branch can be checked out without
any extra backend API call.")

(defun code-review-minimal--auto-checkout-source-branch ()
  "Fetch and checkout the source branch of the current MR/PR via git refs.

Uses the well-known ref published by the forge for the MR/PR head — see
`code-review-minimal--mr-ref-formats' — so no backend API call is made.
The ref is fetched from `origin' into FETCH_HEAD; a local branch named
`<owner_repo>_<iid>' is then created or reset to that commit and
checked out.  The current buffer is reverted on success.

Returns t on success, nil on failure or unsupported backend.  Callers
should fall back to a manual flow when nil is returned."
  (let* ((backend code-review-minimal--current-backend)
         (iid code-review-minimal--mr-iid)
         (fmt (alist-get backend code-review-minimal--mr-ref-formats)))
    (when (and backend iid fmt)
      (let* ((default-directory
              (or (code-review-minimal--git-root) default-directory))
             (ref (format fmt iid))
             (local (format "%s_%d" (code-review-minimal--owner-repo-string)
                            iid))
             (errbuf (get-buffer-create " *crm-fetch-err*")))
        (with-current-buffer errbuf (erase-buffer))
        ;; Step 1: fetch the head ref into FETCH_HEAD.
        (let ((fetch-rc
               (call-process "git" nil (list errbuf t) nil
                             "fetch" "origin" ref)))
          (when (and (integerp fetch-rc) (zerop fetch-rc))
            ;; Step 2: stash dirty worktree so checkout cannot fail.
            (code-review-minimal--stash-worktree)
            ;; Step 3: create or reset the local branch from FETCH_HEAD
            ;; and check it out.  `-B' is safe even when already on the
            ;; target branch (it updates the branch ref and working tree).
            (with-current-buffer errbuf (erase-buffer))
            (let ((co-rc
                   (call-process "git" nil (list errbuf t) nil
                                 "checkout" "-B" local "FETCH_HEAD")))
              (when (and (integerp co-rc) (zerop co-rc))
                (message
                 "code-review-minimal: checked out source branch %s"
                 local)
                (when (and buffer-file-name
                           (file-readable-p buffer-file-name))
                  (revert-buffer t t))
                t))))))))

(defun code-review-minimal--checkout-branch-for-review ()
  "Checkout the source branch for the current MR/PR review.

First saves the current git branch so it can be restored later by
`code-review-minimal-finish-review'.

Then tries `code-review-minimal--auto-checkout-source-branch', which
fetches the well-known forge ref and checks it out — no backend API
call.  On success, no prompt is shown.

If automatic checkout is unsupported or fails (origin unreachable, ref
missing, etc.), falls back to listing local and remote-tracking
branches and prompting the user to pick one.  If a remote-tracking ref
is selected and plain checkout fails, a local tracking branch is
created automatically and named `<owner_repo>_<remote_branch>'.  Reverts
the current buffer after a successful checkout.  Skips silently when
the user accepts the empty default."
  (code-review-minimal--save-original-branch)
  (unless (code-review-minimal--auto-checkout-source-branch)
    (let* ((root (or (code-review-minimal--git-root) default-directory))
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
            (if (and (boundp 'code-review-minimal--mr-source-branch)
                     code-review-minimal--mr-source-branch)
                code-review-minimal--mr-source-branch
              (completing-read
               "Checkout branch for review (RET to skip): "
               all-branches
               nil nil nil nil ""))))
      (unless (string-empty-p branch)
        ;; Stash dirty worktree so checkout cannot fail.
        (code-review-minimal--stash-worktree)
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
                              (code-review-minimal--owner-repo-string)
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
                   "code-review-minimal: git checkout %s failed%s"
                   branch
                   (if (string-empty-p err)
                       ""
                     (format " — %s" (string-trim err)))))))
            (message "code-review-minimal: checked out branch %s" branch)
            ;; Revert the buffer so its content matches the newly-checked-out
            ;; file; the diff's new-file line numbers reference this version.
            (when (and buffer-file-name
                       (file-readable-p buffer-file-name))
              (revert-buffer t t))))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'code-review-minimal-branch)

;;; code-review-minimal-branch.el ends here
