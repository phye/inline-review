;;; code-review-minimal.el --- Minimal Code Review with overlay for GitHub/GitLab/Gongfeng -*- lexical-binding: t; -*-

;; Author: phye
;; Version: 0.2.0
;; Keywords: tools, vc, review
;; Package-Requires: ((emacs "27.1") (ghub "3.6"))

;;; Commentary:
;;
;; code-review-minimal is a lightweight minor mode for performing code review
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
;;      The host is taken from `code-review-minimal-*-api-url', so GitHub
;;      Enterprise and self-hosted GitLab instances work automatically by
;;      setting the appropriate base-URL custom variable.
;;
;;      For multiple accounts on the same host, set the git config first:
;;        git config --global <backend>.user yourname
;;      then use `yourname^crm' as the login in ~/.authinfo.
;;
;;   2. Start a review session from a PR/MR web URL:
;;        M-x code-review-minimal-review-url
;;      The backend (github/gitlab/gongfeng) is auto-detected from the URL host
;;      and the git remote.  Inline comment overlays are rendered immediately.
;;
;;   3. To post a new comment, select a region and run:
;;        M-x code-review-minimal-add-comment
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
;;   code-review-minimal-custom.el   — all defgroup/defcustom/defface declarations
;;   code-review-minimal-backend.el  — registry, auth, cache, buffer-local state
;;   code-review-minimal-branch.el   — branch checkout, original-branch save/restore, stash
;;   code-review-minimal-diff.el     — diff patch parsing and hunk overlay rendering
;;   code-review-minimal-comment.el  — comment overlays, input, commands, and CRUD dispatch
;;   code-review-minimal-github.el   — GitHub backend
;;   code-review-minimal-gitlab.el   — GitLab backend
;;   code-review-minimal-gongfeng.el — Gongfeng backend
;;
;; License: MIT

;;; Code:

(require 'cl-lib)
(require 'code-review-minimal-custom)
(require 'code-review-minimal-backend)
(require 'code-review-minimal-branch)
(require 'code-review-minimal-diff)
(require 'code-review-minimal-comment)
(require 'code-review-minimal-github)
(require 'code-review-minimal-gitlab)
(require 'code-review-minimal-gongfeng)
(require 'code-review-minimal-codeberg)

;; Forward declarations for functions defined in sibling files.
;; These are evaluated at compile time via `require' above; the
;; declare-forms simply document the cross-file boundary.
(declare-function code-review-minimal--review-in-progress-p
                  "code-review-minimal-branch")
(declare-function code-review-minimal--checkout-branch-for-review
                  "code-review-minimal-branch")
(declare-function code-review-minimal--load-original-branch
                  "code-review-minimal-branch")
(declare-function code-review-minimal--pop-stash
                  "code-review-minimal-branch")
(declare-function code-review-minimal--render-comment-threads
                  "code-review-minimal-comment")
(declare-function code-review-minimal--clear-overlays
                  "code-review-minimal-comment")
(declare-function code-review-minimal--cancel-comment
                  "code-review-minimal-comment")
(declare-function code-review-minimal--clear-hunk-overlays
                  "code-review-minimal-diff")
(declare-function code-review-minimal--diff-cache-key
                  "code-review-minimal-diff")
(declare-function code-review-minimal--fetch-diff-then
                  "code-review-minimal-diff")

;;
;; These functions are the only orchestrators that trigger rendering
;; and re-fetch logic.  Backend functions receive callbacks / on-success
;; thunks and must not touch overlays or trigger re-fetches themselves.

(defun code-review-minimal--refresh-overlays ()
  "Fetch diff and comments via the current backend and render overlays.
Diff hunk overlays are rendered first; comment thread overlays are rendered
after the diff fetch completes (or immediately if diff is disabled/unavailable).
The backend `:fetch-diff' callback receives change plists; `:fetch' receives
thread plists.  Both are filtered to the current file."
  (let ((rel-path (code-review-minimal--relative-file-path))
        (buf (current-buffer))
        (backend code-review-minimal--current-backend)
        (iid code-review-minimal--mr-iid)
        (proj code-review-minimal--project-info))
    (let ((fetch-comments
           (lambda ()
             (funcall
              (code-review-minimal--backend-prop backend :fetch)
              (lambda (threads)
                (code-review-minimal--render-comment-threads
                 buf rel-path threads))))))
      (if (and code-review-minimal-highlight-hunks
               (code-review-minimal--backend-prop
                backend
                :fetch-diff))
          (code-review-minimal--fetch-diff-then
           backend buf iid proj rel-path fetch-comments)
        (funcall fetch-comments)))))

;;;; ─── Public Commands ───────────────────────────────────────────────────────

;;;###autoload
(defun code-review-minimal-review-url (url)
  "Start a code review session for the MR/PR at URL.
URL must be a full web URL of a pull/merge request, e.g.:
  https://git.woa.com/adp/proto-unified/-/merge_requests/856
  https://github.com/owner/repo/pull/42
  https://gitlab.com/ns/project/-/merge_requests/7

Automatically detects the backend (github/gitlab/gongfeng), project, and
MR IID from the URL, then enables `code-review-minimal-mode' and fetches
inline comments for the current buffer."
  (interactive (list (read-string "MR/PR URL: ")))
  (when (code-review-minimal--review-in-progress-p)
    (user-error
     "code-review-minimal: a review is already in progress.  \
Call `code-review-minimal-finish-review' first"))
  (let ((parsed (code-review-minimal--parse-mr-url url)))
    (unless (and parsed (plist-get parsed :iid))
      (user-error
       "code-review-minimal: expected a full MR/PR URL, got: %S"
       url))
    (let ((iid (plist-get parsed :iid))
          (backend (plist-get parsed :backend))
          (projinfo (plist-get parsed :project-info)))
      ;; Install state before enabling the mode so mode-activation sees it.
      (setq
       code-review-minimal--mr-iid iid
       code-review-minimal--mr-id nil
       code-review-minimal--mr-source-branch nil
       code-review-minimal--mr-target-branch nil
       code-review-minimal--project-info projinfo)
      (when backend
        (setq code-review-minimal--current-backend backend)
        (code-review-minimal--save-backend backend))
      (code-review-minimal--save-iid iid)
      ;; Invalidate diff cache for this MR so we always start fresh
      (remhash
       (code-review-minimal--diff-cache-key
        (or backend code-review-minimal--current-backend)
        iid
        projinfo)
       code-review-minimal--diff-cache)
      (code-review-minimal--ensure-backend)
      (code-review-minimal--assert-token
       code-review-minimal--current-backend)
      (message "code-review-minimal: reviewing !%d on %s [%s]"
               iid
               (or (alist-get 'project-id projinfo)
                   (format "%s/%s"
                           (alist-get 'owner projinfo)
                           (alist-get 'repo projinfo)))
               code-review-minimal--current-backend)
      (let ((proceed
             (lambda ()
               (code-review-minimal--checkout-branch-for-review)
               ;; revert-buffer (called during checkout) runs
               ;; kill-all-local-variables, which resets all defvar-local state
               ;; to nil.  Re-apply the values captured by this closure so that
               ;; mode activation succeeds even when the current buffer is not
               ;; among the files changed by the MR.
               (setq code-review-minimal--mr-iid iid
                     code-review-minimal--project-info projinfo)
               (when backend
                 (setq code-review-minimal--current-backend backend))
               ;; Enable mode (which refreshes overlays) or just refresh if already on
               (if (bound-and-true-p code-review-minimal-mode)
                   (code-review-minimal--refresh-overlays)
                 (code-review-minimal-mode 1))))
            (resolve-branches-fn
             (code-review-minimal--backend-prop
              code-review-minimal--current-backend :resolve-branches)))
        ;; Call :resolve-branches first so that branch names are populated in
        ;; buffer-local state before the checkout prompt is shown.  All
        ;; built-in backends supply this hook.  Custom backends that omit it
        ;; fall through to checkout directly.
        (if resolve-branches-fn
            (funcall resolve-branches-fn
                     (lambda (source target)
                       (when source
                         (setq code-review-minimal--mr-source-branch source))
                       (when target
                         (setq code-review-minimal--mr-target-branch target))
                       (message "[code-review-minimal] source-branch=%s target-branch=%s"
                                code-review-minimal--mr-source-branch
                                code-review-minimal--mr-target-branch)
                       (funcall proceed)))
          (funcall proceed))))))

;;;###autoload
(defun code-review-minimal-finish-review ()
  "Finish the current review session and clean up all state.

Disables `code-review-minimal-mode' in every buffer that has it active,
clears the diff cache, the IID/backend in-memory caches, and removes the
per-repo cache files (.git/code-review-minimal-iid and
.git/code-review-minimal-backend) so the next session starts fresh."
  (interactive)
  (let ((root (code-review-minimal--git-root)))
    ;; 0. Disable mode in all live buffers first so overlays are removed
    ;; before the working tree changes underneath them.
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (bound-and-true-p code-review-minimal-mode)
          (code-review-minimal-mode -1))))
    ;; 1. Restore the original branch if one was saved.
    (when-let ((original (code-review-minimal--load-original-branch)))
      (let ((errbuf (get-buffer-create " *crm-finish-err*")))
        (with-current-buffer errbuf (erase-buffer))
        (let ((rc (call-process "git" nil (list errbuf t) nil
                                "checkout" original)))
          (if (and (integerp rc) (zerop rc))
              (progn
                (message
                 "code-review-minimal: restored original branch %s" original)
                (code-review-minimal--pop-stash)
                (dolist (buf (buffer-list))
                  (with-current-buffer buf
                    (when (and buffer-file-name
                               (file-readable-p buffer-file-name))
                      (revert-buffer t t)))))
            (let ((err (with-current-buffer errbuf (buffer-string))))
              (message
               "code-review-minimal: failed to restore branch %s%s"
               original
               (if (string-empty-p err)
                   ""
                 (format " — %s" (string-trim err)))))))))
    ;; 2. Clear the global diff cache entirely.
    (clrhash code-review-minimal--diff-cache)
    ;; 3. Clear the in-memory IID and backend caches.
    (clrhash code-review-minimal--iid-cache)
    (clrhash code-review-minimal--backend-cache)
    ;; 4. Remove the per-repo cache files so IID/backend/original-branch
    ;; are not reused next time.
    (when root
      (dolist (fname
               '("code-review-minimal-iid"
                 "code-review-minimal-backend"
                 "code-review-minimal-original-branch"
                 "code-review-minimal-stash"))
        (let ((file
               (expand-file-name fname
                                 (expand-file-name ".git" root))))
          (when (file-exists-p file)
            (delete-file file)))))
    (message
     "code-review-minimal: review session finished and all state cleared.")))

;;;###autoload
(defun code-review-minimal-refresh ()
  "Re-fetch comments (and diff, if enabled) and refresh overlays.
Use this to update the display after external changes (e.g. a colleague
posted a new comment)."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error
     "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (unless code-review-minimal--mr-iid
    (user-error "code-review-minimal: no MR IID set"))
  (code-review-minimal--assert-token
   code-review-minimal--current-backend)
  (message "code-review-minimal: refreshing...")
  (code-review-minimal--refresh-overlays))

;;;###autoload
(defun code-review-minimal-set-backend-for-repo (backend)
  "Set and persist the backend for the current repository.
Use this to override auto-detection."
  (interactive (list
                (intern
                 (completing-read
                  "Backend: " '("github" "gitlab" "gongfeng")))))
  (setq code-review-minimal--current-backend backend)
  (code-review-minimal--save-backend backend)
  (message "code-review-minimal: backend set to %s (persisted)."
           backend))

;;;; ─── Minor Mode ────────────────────────────────────────────────────────────

(defvar code-review-minimal-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key
     m (kbd "C-c C-d") #'code-review-minimal-view-removed-lines)
    m)
  "Keymap for `code-review-minimal-mode'.")

;;;###autoload
(define-minor-mode code-review-minimal-mode
  "Minor mode for reviewing code inline using overlays.

Typically you start a session with:
  M-x code-review-minimal-review-url

which accepts a full MR/PR URL, auto-detects the backend, and enables
this mode.  The mode can also be toggled directly; if no MR is cached it
will prompt for a URL via `code-review-minimal-review-url'.

Commands:
  `code-review-minimal-review-url'        - start review from a URL (main entry point)
  `code-review-minimal-add-comment'       - add comment for selected region
  `code-review-minimal-edit-comment'      - edit comment at point
  `code-review-minimal-reply-comment'     - reply to comment thread at point
  `code-review-minimal-delete-comment'    - delete comment at point
  `code-review-minimal-refresh'           - re-fetch comments
  `code-review-minimal-next-thread'        - go to next comment thread (cross-file)
  `code-review-minimal-previous-thread'    - go to previous comment thread (cross-file)
  `code-review-minimal-next-hunk'          - go to next diff hunk (cross-file)
  `code-review-minimal-previous-hunk'      - go to previous diff hunk (cross-file)
  `code-review-minimal-view-removed-lines' - view full removed block at point
  `code-review-minimal-resolve-comment'   - resolve comment at point
  `code-review-minimal-toggle-hide-resolved' - toggle visibility of resolved threads
  `code-review-minimal-set-backend-for-repo' - change backend for this repo
  `code-review-minimal-finish-review'     - end session and clear all state/cache"
  :lighter " CR"
  :keymap
  code-review-minimal-mode-map
  (if code-review-minimal-mode
      (progn
        ;; Determine backend
        (code-review-minimal--ensure-backend)
        (message "code-review-minimal: using %s backend"
                 code-review-minimal--current-backend)
        ;; Get token for backend
        (code-review-minimal--assert-token
         code-review-minimal--current-backend)
        ;; Get MR IID — if not already set via review-url, ask for a URL now
        (unless code-review-minimal--mr-iid
          (let ((cached (code-review-minimal--load-cached-iid)))
            (if cached
                (progn
                  (setq code-review-minimal--mr-iid cached)
                  (message
                   "code-review-minimal: using cached MR IID !%d"
                   cached))
              (call-interactively #'code-review-minimal-review-url)
              ;; review-url already refreshes overlays and handles the rest; bail out
              (setq code-review-minimal-mode nil)
              (cl-return-from nil))))
        ;; Set left margin for diff fringe indicators
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (let ((margins (window-margins win)))
            (set-window-margins win 2 (cdr margins))))
        ;; Refresh diff and comment overlays
        (code-review-minimal--refresh-overlays))
    ;; Disable
    (code-review-minimal--clear-overlays)
    (code-review-minimal--clear-hunk-overlays)
    (when code-review-minimal--input-overlay
      (code-review-minimal--cancel-comment))
    (dolist (win (get-buffer-window-list (current-buffer) nil t))
      (let ((margins (window-margins win)))
        (set-window-margins win 0 (cdr margins))))
    (setq
     code-review-minimal--mr-iid nil
     code-review-minimal--mr-id nil
     code-review-minimal--mr-source-branch nil
     code-review-minimal--mr-target-branch nil
     code-review-minimal--project-info nil
     code-review-minimal--current-backend nil)))

(provide 'code-review-minimal)

;;; code-review-minimal.el ends here
