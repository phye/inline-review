;;; inline-review.el --- Minimal Code Review with overlay for GitHub/GitLab/Gongfeng -*- lexical-binding: t; -*-

;; Author: phye
;; Version: 0.2.0
;; Keywords: tools, vc, review
;; Package-Requires: ((emacs "27.1") (ghub "3.6"))

;;; Commentary:
;;
;; inline-review is a lightweight minor mode for performing code review
;; directly inside Emacs against GitHub Pull Requests, GitLab Merge Requests,
;; and Gongfeng (工蜂) MRs.
;;
;; Quick start:
;;   1. Add an entry to ~/.authinfo (or ~/.authinfo.gpg) for each forge you use:
;;        machine api.github.com  login ^crm password <github-token>
;;        machine gitlab.com      login ^crm password <gitlab-token>
;;        machine git.woa.com      login ^crm password <gongfeng-token>
;;        machine code.tencent.com login ^crm password <gongfeng-token>
;;      The `^crm' login distinguishes these entries from tokens used by other
;;      Emacs forge tools (e.g. Magit/ghub use `^').
;;      The host is taken from `inline-review-*-api-url', so GitHub
;;      Enterprise and self-hosted GitLab instances work automatically by
;;      setting the appropriate base-URL custom variable.
;;
;;      For multiple accounts on the same host, set the git config first:
;;        git config --global <backend>.user yourname
;;      then use `yourname^crm' as the login in ~/.authinfo.
;;
;;   2. Start a review session from a PR/MR web URL:
;;        M-x inline-review-review-url
;;      The backend (github/gitlab/gongfeng) is auto-detected from the URL host
;;      and the git remote.  Inline comment overlays are rendered immediately.
;;
;;   3. To post a new comment, select a region and run:
;;        M-x inline-review-add-comment
;;      An overlay input area opens beneath the selection.
;;      Type your comment and press C-c C-c to submit, or C-c C-k to cancel.
;;
;; Supported backends:
;;   - github    : github.com and GitHub Enterprise
;;                 HTTP: ghub (Authorization: Bearer <token>)
;;   - gitlab    : gitlab.com and self-hosted GitLab (API v4)
;;                 HTTP: ghub with :forge 'gitlab (PRIVATE-TOKEN header)
;;   - gongfeng  : git.woa.com / code.tencent.com — Tencent's Gongfeng (工蜂) (API v3)
;;                 HTTP: url-retrieve with explicit PRIVATE-TOKEN header
;;                 (Gongfeng's API v3 is not wire-compatible with GitLab v4)
;;
;; Sub-files:
;;   inline-review-custom.el   — all defgroup/defcustom/defface declarations
;;   inline-review-backend.el  — registry, auth, cache, buffer-local state
;;   inline-review-branch.el   — branch checkout, original-branch save/restore, stash
;;   inline-review-diff.el     — diff patch parsing and hunk overlay rendering
;;   inline-review-comment.el  — comment overlays, input, commands, and CRUD dispatch
;;   inline-review-github.el   — GitHub backend
;;   inline-review-gitlab.el   — GitLab backend
;;   inline-review-gongfeng.el — Gongfeng backend
;;
;; License: MIT

;;; Code:

(require 'cl-lib)
(require 'inline-review-custom)
(require 'inline-review-backend)
(require 'inline-review-branch)
(require 'inline-review-diff)
(require 'inline-review-comment)
(require 'inline-review-github)
(require 'inline-review-gitlab)
(require 'inline-review-gongfeng)
(require 'inline-review-codeberg)

;; Forward declarations for functions defined in sibling files.
;; These are evaluated at compile time via `require' above; the
;; declare-forms simply document the cross-file boundary.
(declare-function inline-review--review-in-progress-p
                  "inline-review-branch")
(declare-function inline-review--checkout-branch-for-review
                  "inline-review-branch")
(declare-function inline-review--load-original-branch
                  "inline-review-branch")
(declare-function inline-review--pop-stash
                  "inline-review-branch")
(declare-function inline-review--render-comment-threads
                  "inline-review-comment")
(declare-function inline-review--clear-overlays
                  "inline-review-comment")
(declare-function inline-review--cancel-comment
                  "inline-review-comment")
(declare-function inline-review--clear-hunk-overlays
                  "inline-review-diff")
(declare-function inline-review--diff-cache-key
                  "inline-review-diff")
(declare-function inline-review--fetch-diff-then
                  "inline-review-diff")

;;
;; These functions are the only orchestrators that trigger rendering
;; and re-fetch logic.  Backend functions receive callbacks / on-success
;; thunks and must not touch overlays or trigger re-fetches themselves.

(defun inline-review--refresh-overlays ()
  "Fetch diff and comments via the current backend and render overlays.
Diff hunk overlays are rendered first; comment thread overlays are rendered
after the diff fetch completes (or immediately if diff is disabled/unavailable).
The backend `:fetch-diff' callback receives change plists; `:fetch' receives
thread plists.  Both are filtered to the current file."
  (let ((rel-path (inline-review--relative-file-path))
        (buf (current-buffer))
        (backend inline-review--current-backend)
        (iid inline-review--mr-iid)
        (proj inline-review--project-info))
    (let ((fetch-comments
           (lambda ()
             (funcall
              (inline-review--backend-prop backend :fetch)
              (lambda (threads)
                (inline-review--render-comment-threads
                 buf rel-path threads))))))
      (if (and inline-review-highlight-hunks
               (inline-review--backend-prop
                backend
                :fetch-diff))
          (inline-review--fetch-diff-then
           backend buf iid proj rel-path fetch-comments)
        (funcall fetch-comments)))))

;;;; ─── Public Commands ───────────────────────────────────────────────────────

;;;###autoload
(defun inline-review-review-url (url)
  "Start a code review session for the MR/PR at URL.
URL must be a full web URL of a pull/merge request, e.g.:
  https://git.woa.com/adp/proto-unified/-/merge_requests/856
  https://github.com/owner/repo/pull/42
  https://gitlab.com/ns/project/-/merge_requests/7

Automatically detects the backend (github/gitlab/gongfeng), project, and
MR IID from the URL, then enables `inline-review-mode' and fetches
inline comments for the current buffer."
  (interactive (list (read-string "MR/PR URL: ")))
  (when (inline-review--review-in-progress-p)
    (user-error
     "inline-review: a review is already in progress.  \
Call `inline-review-finish-review' first"))
  (let ((parsed (inline-review--parse-mr-url url)))
    (unless (and parsed (plist-get parsed :iid))
      (user-error
       "inline-review: expected a full MR/PR URL, got: %S"
       url))
    (let ((iid (plist-get parsed :iid))
          (backend (plist-get parsed :backend))
          (projinfo (plist-get parsed :project-info)))
      ;; Install state before enabling the mode so mode-activation sees it.
      (setq
       inline-review--mr-iid iid
       inline-review--mr-id nil
       inline-review--mr-source-branch nil
       inline-review--mr-target-branch nil
       inline-review--project-info projinfo)
      (when backend
        (setq inline-review--current-backend backend)
        (inline-review--save-backend backend))
      (inline-review--save-iid iid)
      ;; Invalidate diff cache for this MR so we always start fresh
      (remhash
       (inline-review--diff-cache-key
        (or backend inline-review--current-backend)
        iid
        projinfo)
       inline-review--diff-cache)
      (inline-review--ensure-backend)
      (inline-review--assert-token
       inline-review--current-backend)
      (message "inline-review: reviewing !%d on %s [%s]"
               iid
               (or (alist-get 'project-id projinfo)
                   (format "%s/%s"
                           (alist-get 'owner projinfo)
                           (alist-get 'repo projinfo)))
               inline-review--current-backend)
      (let* ((initial-buf (current-buffer))
             (proceed
              (lambda ()
                (with-current-buffer initial-buf
                  ;; Capture branch names NOW, before checkout may call
                  ;; revert-buffer → kill-all-local-variables and wipe them.
                  (let ((src inline-review--mr-source-branch)
                        (tgt inline-review--mr-target-branch))
                    (inline-review--checkout-branch-for-review)
                    ;; revert-buffer (called during checkout) runs
                    ;; kill-all-local-variables, which resets all defvar-local state
                    ;; to nil.  Re-apply the values captured by this closure so that
                    ;; mode activation succeeds even when the current buffer is not
                    ;; among the files changed by the MR.
                    (setq inline-review--mr-iid iid
                          inline-review--project-info projinfo)
                    (when backend
                      (setq inline-review--current-backend backend))
                    (when src
                      (setq inline-review--mr-source-branch src))
                    (when tgt
                      (setq inline-review--mr-target-branch tgt))
                    ;; Enable mode (which refreshes overlays) or just refresh if already on
                    (if (bound-and-true-p inline-review-mode)
                        (inline-review--refresh-overlays)
                      (inline-review-mode 1))
                    ;; Mark this project as fully prepared so navigation commands
                    ;; (next-hunk, previous-hunk, next-thread, previous-thread)
                    ;; know a live review exists here.  Other projects are unaffected.
                    (when-let ((root (inline-review--git-root)))
                      (puthash root iid
                               inline-review--review-active-cache))))))
             (resolve-branches-fn
              (inline-review--backend-prop
               inline-review--current-backend :resolve-branches)))
        ;; Call :resolve-branches first so that branch names are populated in
        ;; buffer-local state before the checkout prompt is shown.  All
        ;; built-in backends supply this hook.  Custom backends that omit it
        ;; fall through to checkout directly.
        (if resolve-branches-fn
            (funcall resolve-branches-fn
                     (lambda (source target)
                       (with-current-buffer initial-buf
                         (when source
                           (setq inline-review--mr-source-branch source))
                         (when target
                           (setq inline-review--mr-target-branch target))
                         (message "[inline-review] source-branch=%s target-branch=%s"
                                  inline-review--mr-source-branch
                                  inline-review--mr-target-branch))
                       (funcall proceed)))
          (funcall proceed))))))

;;;###autoload
(defun inline-review-finish-review ()
  "Finish the review session for the current project and clean up its state.

Only the current project (git root) is affected; reviews in other projects
continue uninterrupted.  Disables `inline-review-mode' in every buffer
belonging to this project, restores the original branch, clears the diff cache
entry for this MR, removes the per-project in-memory caches, and deletes the
per-repo cache files under .git/."
  (interactive)
  (let* ((root (inline-review--git-root))
         ;; Capture MR identity from buffer-local state before mode teardown
         ;; resets it.  Fall back to iid-cache in case we're called from a
         ;; buffer that never had mode active.
         (current-backend inline-review--current-backend)
         (current-iid    (or inline-review--mr-iid
                             (and root
                                  (gethash root
                                           inline-review--iid-cache))))
         (current-proj   inline-review--project-info))
    ;; 0. Disable mode in all live buffers that belong to this project so
    ;; overlays are removed before the working tree changes underneath them.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p inline-review-mode)
                   root
                   buffer-file-name
                   (string-prefix-p root
                                    (expand-file-name buffer-file-name)))
          (inline-review-mode -1))))
    ;; 1. Restore the original branch if one was saved.
    (when-let ((original (inline-review--load-original-branch)))
      (let ((errbuf (get-buffer-create " *crm-finish-err*")))
        (with-current-buffer errbuf (erase-buffer))
        (let ((rc (call-process "git" nil (list errbuf t) nil
                                "checkout" original)))
          (if (and (integerp rc) (zerop rc))
              (progn
                (message
                 "inline-review: restored original branch %s" original)
                (inline-review--pop-stash)
                ;; Only revert buffers belonging to this project.
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (when (and buffer-file-name
                               root
                               (string-prefix-p root
                                                (expand-file-name
                                                 buffer-file-name))
                               (file-readable-p buffer-file-name))
                      (revert-buffer t t)))))
            (let ((err (with-current-buffer errbuf (buffer-string))))
              (message
               "inline-review: failed to restore branch %s%s"
               original
               (if (string-empty-p err)
                   ""
                 (format " — %s" (string-trim err)))))))))
    ;; 2. Remove the diff-cache entry for this MR.
    (when (and current-backend current-iid)
      (remhash
       (inline-review--diff-cache-key
        current-backend current-iid current-proj)
       inline-review--diff-cache))
    ;; 3. Remove this project's entries from the in-memory caches.
    ;; `remhash' with any key (including nil) is safe — it only removes that
    ;; specific entry and leaves the rest intact.  No root guard needed here.
    (remhash root inline-review--iid-cache)
    (remhash root inline-review--backend-cache)
    (remhash root inline-review--review-active-cache)
    ;; 4. Remove the per-repo cache files so IID/backend/original-branch
    ;; are not reused next time.
    (when root
      (dolist (fname
               '("inline-review-iid"
                 "inline-review-backend"
                 "inline-review-original-branch"
                 "inline-review-stash"))
        (let ((file
               (expand-file-name fname
                                 (expand-file-name ".git" root))))
          (when (file-exists-p file)
            (delete-file file)))))
    (message
     "inline-review: review session finished and all state cleared.")))

;;;###autoload
(defun inline-review-refresh ()
  "Re-fetch comments (and diff, if enabled) and refresh overlays.
Use this to update the display after external changes (e.g. a colleague
posted a new comment)."
  (interactive)
  (unless (bound-and-true-p inline-review-mode)
    (user-error
     "inline-review: please enable `inline-review-mode' first"))
  (unless inline-review--mr-iid
    (user-error "inline-review: no MR IID set"))
  (inline-review--assert-token
   inline-review--current-backend)
  (message "inline-review: refreshing...")
  (inline-review--refresh-overlays))

(defun inline-review--overview-open-file (button)
  "Open the file associated with BUTTON in the overview buffer."
  (find-file (button-get button 'inline-review-file)))

;;;###autoload
(defun inline-review-overview ()
  "Pop up a read-only buffer with `git diff --stat' between source and target branch.
Fetches the latest refs from origin each time and diffs against remote-tracking
branches (origin/source vs origin/target) to ensure the stat reflects the most
up-to-date remote state rather than potentially stale local branches."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review — run `inline-review-review-url' first"))
  (let ((source inline-review--mr-source-branch)
        (target inline-review--mr-target-branch)
        (root   (inline-review--git-root)))
    ;; Fallback: if the current buffer doesn't have branch names (e.g. the
    ;; user called `overview' from a file that was never opened via
    ;; `--goto-hunk'), scan all live buffers where inline-review-mode
    ;; is active and borrow the names from the first one that has them.
    (unless (and source target)
      (dolist (buf (buffer-list))
        (when (and (not (and source target))
                   (buffer-live-p buf))
          (with-current-buffer buf
            (when (bound-and-true-p inline-review-mode)
              (when (and (not source) inline-review--mr-source-branch)
                (setq source inline-review--mr-source-branch))
              (when (and (not target) inline-review--mr-target-branch)
                (setq target inline-review--mr-target-branch)))))))
    (unless source
      (user-error
       "inline-review: source branch not known yet \
(wait for branch resolution to complete)"))
    (unless target
      (user-error
       "inline-review: target branch not known yet \
(wait for branch resolution to complete)"))
    (unless root
      (user-error "inline-review: not inside a git repository"))
    ;; Use remote-tracking refs so the diff reflects the latest remote state
    ;; rather than potentially outdated local branches.
    (let* ((default-directory root)
           (remote-source
            (if (string-prefix-p "origin/" source)
                source
              (concat "origin/" source)))
           (remote-target
            (if (string-prefix-p "origin/" target)
                target
              (concat "origin/" target))))
      ;; Fetch the two branches from origin to ensure we have up-to-date refs.
      (call-process "git" nil nil nil
                    "fetch" "origin" source target)
      (let* ((outbuf (get-buffer-create "*inline-review-overview*")))
        (with-current-buffer outbuf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (let ((rc (call-process "git" nil (list outbuf t) nil
                                    "diff" "--stat"
                                    remote-target remote-source)))
              (if (and (integerp rc) (zerop rc))
                  (progn
                    (goto-char (point-min))
                    (while (re-search-forward
                            "^ \\([^|\n]+?\\) *|" nil t)
                      (let* ((file-str (string-trim (match-string 1)))
                             (file (cond
                                    ((string-match
                                      "\\`\\(.+\\) => \\(.+\\)\\'"
                                      file-str)
                                     (match-string 2 file-str))
                                    (t file-str)))
                             (abs-file (expand-file-name file root))
                             (beg (match-beginning 1))
                             (end (match-end 1)))
                        (make-text-button
                         beg end
                         'inline-review-file abs-file
                         'action
                         'inline-review--overview-open-file
                         'follow-link t
                         'help-echo "Click to open file"
                         'face 'link)))
                    (goto-char (point-min))
                    (view-mode 1))
                (let ((err (string-trim (buffer-string))))
                  (erase-buffer)
                  (insert
                   (format "git diff --stat %s %s failed%s\n"
                           remote-target remote-source
                           (if (string-empty-p err)
                               ""
                             (format ": %s" err))))
                  (goto-char (point-min))
                  (view-mode 1))))))
        (pop-to-buffer outbuf)))))

;;;###autoload
(defun inline-review-set-backend-for-repo (backend)
  "Set and persist the backend for the current repository.
Use this to override auto-detection."
  (interactive (list
                (intern
                 (completing-read
                  "Backend: " '("github" "gitlab" "gongfeng")))))
  (setq inline-review--current-backend backend)
  (inline-review--save-backend backend)
  (message "inline-review: backend set to %s (persisted)."
           backend))

;;;; ─── Minor Mode ────────────────────────────────────────────────────────────

(defvar inline-review-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key
     m (kbd "C-c C-d") #'inline-review-view-removed-lines)
    m)
  "Keymap for `inline-review-mode'.")

;;;###autoload
(define-minor-mode inline-review-mode
  "Minor mode for reviewing code inline using overlays.

Typically you start a session with:
  M-x inline-review-review-url

which accepts a full MR/PR URL, auto-detects the backend, and enables
this mode.  The mode can also be toggled directly; if no MR is cached it
will prompt for a URL via `inline-review-review-url'.

Commands:
  `inline-review-review-url'        - start review from a URL (main entry point)
  `inline-review-add-comment'       - add comment for selected region
  `inline-review-edit-comment'      - edit comment at point
  `inline-review-reply-comment'     - reply to comment thread at point
  `inline-review-delete-comment'    - delete comment at point
  `inline-review-refresh'           - re-fetch comments
  `inline-review-overview'          - show git diff --stat for this MR/PR
  `inline-review-next-thread'        - go to next comment thread (cross-file)
  `inline-review-previous-thread'    - go to previous comment thread (cross-file)
  `inline-review-next-hunk'          - go to next diff hunk (cross-file)
  `inline-review-previous-hunk'      - go to previous diff hunk (cross-file)
  `inline-review-first-hunk'         - go to first diff hunk in project
  `inline-review-last-hunk'          - go to last diff hunk in project
  `inline-review-view-removed-lines' - view full removed block at point
  `inline-review-resolve-comment'   - resolve comment at point
  `inline-review-toggle-hide-resolved' - toggle visibility of resolved threads
  `inline-review-set-backend-for-repo' - change backend for this repo
  `inline-review-finish-review'     - end session and clear all state/cache"
  :lighter " IR"
  :keymap
  inline-review-mode-map
  (if inline-review-mode
      (progn
        ;; Determine backend
        (inline-review--ensure-backend)
        (message "inline-review: using %s backend"
                 inline-review--current-backend)
        ;; Get token for backend
        (inline-review--assert-token
         inline-review--current-backend)
        ;; Get MR IID — if not already set via review-url, ask for a URL now
        (unless inline-review--mr-iid
          (let ((cached (inline-review--load-cached-iid)))
            (if cached
                (progn
                  (setq inline-review--mr-iid cached)
                  (message
                   "inline-review: using cached MR IID !%d"
                   cached))
              (call-interactively #'inline-review-review-url)
              ;; review-url already refreshes overlays and handles the rest; bail out
              (setq inline-review-mode nil)
              (cl-return-from nil))))
        ;; Set left margin for diff fringe indicators
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (let ((margins (window-margins win)))
            (set-window-margins win 2 (cdr margins))))
        ;; Refresh diff and comment overlays
        (inline-review--refresh-overlays))
    ;; Disable
    (inline-review--clear-overlays)
    (inline-review--clear-hunk-overlays)
    (when inline-review--input-overlay
      (inline-review--cancel-comment))
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (let ((margins (window-margins win)))
        (set-window-margins win 0 (cdr margins))))
    (setq
     inline-review--mr-iid nil
     inline-review--mr-id nil
     inline-review--mr-source-branch nil
     inline-review--mr-target-branch nil
     inline-review--project-info nil
     inline-review--current-backend nil)))

(provide 'inline-review)

;;; inline-review.el ends here
