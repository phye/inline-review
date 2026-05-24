;;; inline-review-backend.el --- Backend registry, config, auth, and per-repo cache -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; This file is the shared foundation required by all backend files and by
;; inline-review.el.  It provides:
;;
;;   Customization variables — see inline-review-custom.el
;;   Backend registry (`inline-review-backend-registry',
;;                     `inline-review-register-backend',
;;                     `inline-review--backend-prop')
;;   Backend detection & selection
;;     `inline-review--detect-backend'
;;     `inline-review--ensure-backend'
;;   Token management
;;     `inline-review--git-config'
;;     `inline-review--authinfo-token'
;;     `inline-review--backend-host'
;;     `inline-review--get-token'
;;     `inline-review--assert-token'
;;   Remote & URL parsing
;;     `inline-review--git-remote-url'
;;     `inline-review--parse-mr-url'
;;   Per-repo cache
;;     `inline-review--iid-cache'
;;     `inline-review--backend-cache'
;;     `inline-review--diff-cache'
;;     `inline-review--git-root'
;;     `inline-review--cache-file'
;;     `inline-review--load-cached-iid' / `--save-iid'
;;     `inline-review--load-cached-backend' / `--save-backend'
;;   Buffer-local state variables
;;     `inline-review--mr-iid'
;;     `inline-review--mr-id'
;;     `inline-review--mr-source-branch'
;;     `inline-review--mr-target-branch'
;;     `inline-review--project-info'
;;     `inline-review--current-backend'
;;   Utility helpers shared with diff and comment layers
;;     `inline-review--relative-file-path'
;;     `inline-review--line-number-at'

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'auth-source)
(require 'inline-review-custom)

;;;; ─── Backend Registry ───────────────────────────────────────────────────────
;;
;; THE single extension point for backends.
;;
;; Each entry: (BACKEND-SYMBOL PLIST) where PLIST contains:
;;
;;   :api-url-var  Symbol of the `defcustom' holding the API base URL.
;;   :remote-re     Regexp matched against the git remote URL for auto-detection.
;;
;;   :fetch   Function (callback)
;;     Fetch all MR/PR threads asynchronously.  On completion call:
;;       (funcall callback THREADS)
;;     where THREADS is a list of plists with keys:
;;       :path     — file path string (relative to git root)
;;       :line     — 1-based integer line number
;;       :thread   — list of note alists, each with keys `author', `body',
;;                   `created_at'
;;       :resolved — t (resolved), `:json-false' (open), or nil (unknown)
;;       :note-id  — integer id of the root note
;;
;;   :fetch-diff  Function (callback)  [optional]
;;     Fetch the diff.  On completion call (funcall callback CHANGES)
;;     where CHANGES is a list of plists with keys:
;;       :old-path, :new-path, :patch (unified diff string, may be nil)
;;
;;   :resolve-branches  Function (callback)  [optional]
;;     Fetch only the MR source and target branch names, then call:
;;       (funcall callback SOURCE-BRANCH TARGET-BRANCH)
;;     where both args are strings (or nil if unavailable).
;;     All built-in backends supply this hook.  GitHub and Codeberg
;;     implement it via a lightweight GET to the PR endpoint (head.ref /
;;     base.ref); GitLab and Gongfeng reuse the resolve-mr-id GET which
;;     already returns branch names as a side-effect.  Omitting this key
;;     is allowed for custom backends that always resolve branches from
;;     git refs alone.
;;
;;   :post    Function (beg end body on-success)
;;   :update  Function (note-id body on-success)
;;   :resolve Function (note-id note-body on-success)
;;   :reply   Function (note-id body on-success)
;;   :delete  Function (note-id on-success)

(defvar inline-review-backend-registry nil
  "Alist mapping backend symbols to their configuration and function table.
Each entry: (BACKEND-SYMBOL . PLIST).  See `inline-review-register-backend'
for the list of recognised plist keys.

Note: the value is always reset on package load so that reloading the file
picks up any additions (e.g. new :fetch-diff handlers).  User-added backends
via `inline-review-register-backend' belong in init.el, which runs after
package load.")

;; Use setq (not the defvar initialiser) so that every reload refreshes the
;; built-in entries.  defvar only runs its initialiser when the variable is
;; void, meaning changes to this table would be invisible until Emacs restarts.
(setq inline-review-backend-registry
      '((gongfeng
         :api-url-var inline-review-gongfeng-api-url
         :remote-re "git\\.woa\\.com\\|code\\.tencent\\.com"
         :fetch inline-review--gongfeng-fetch-comments
         :resolve-branches inline-review--gongfeng-resolve-branches
         :fetch-diff inline-review--gongfeng-fetch-diff
         :post inline-review--gongfeng-post-comment
         :update inline-review--gongfeng-update-comment
         :resolve inline-review--gongfeng-resolve-comment
         :reply inline-review--gongfeng-reply-comment
         :delete inline-review--gongfeng-delete-comment)
        (github
         :api-url-var inline-review-github-api-url
         :remote-re "github"
         :fetch inline-review--github-fetch-comments
         :resolve-branches inline-review--github-resolve-branches
         :fetch-diff inline-review--github-fetch-diff
         :post inline-review--github-post-comment
         :update inline-review--github-update-comment
         :resolve inline-review--github-resolve-comment
         :reply inline-review--github-reply-comment
         :delete inline-review--github-delete-comment)
        (gitlab
         :api-url-var inline-review-gitlab-api-url
         :remote-re "gitlab"
         :fetch inline-review--gitlab-fetch-comments
         :resolve-branches inline-review--gitlab-resolve-branches
         :fetch-diff inline-review--gitlab-fetch-diff
         :post inline-review--gitlab-post-comment
         :update inline-review--gitlab-update-comment
         :resolve inline-review--gitlab-resolve-comment
         :reply inline-review--gitlab-reply-comment
         :delete inline-review--gitlab-delete-comment)
        (codeberg
         :api-url-var inline-review-codeberg-api-url
         :remote-re "codeberg"
         :fetch inline-review--codeberg-fetch-comments
         :resolve-branches inline-review--codeberg-resolve-branches
         :fetch-diff inline-review--codeberg-fetch-diff
         :post inline-review--codeberg-post-comment
         :update inline-review--codeberg-update-comment
         :resolve inline-review--codeberg-resolve-comment
         :reply inline-review--codeberg-reply-comment
         :delete inline-review--codeberg-delete-comment)))

(defun inline-review-register-backend (backend &rest plist)
  "Register BACKEND with its configuration PLIST in the backend registry.
BACKEND is a symbol (e.g. `myfoo').  PLIST must supply:

  :api-url-var  — symbol of the defcustom holding the API base URL
  :remote-re     — regexp for auto-detecting this backend from a remote URL
  :fetch         — function (callback) fetching threads and passing them to callback
  :resolve-branches — optional function (callback) fetching only source and target
                      branch names: calls (callback SOURCE TARGET), both strings or nil.
                      All built-in backends supply this.  Omit for custom backends
                      that always resolve branches via git refs alone.
  :post          — function (beg end body on-success) posting a new comment
  :update        — function (note-id body on-success) updating a comment
  :resolve       — function (note-id note-body on-success) resolving a thread

New entries are prepended so they take precedence over built-in ones for
:remote-re matching.  If a backend with the same symbol already exists it
is replaced."
  (setq inline-review-backend-registry
        (cons
         (cons backend plist)
         (assq-delete-all
          backend inline-review-backend-registry))))

(defun inline-review--backend-prop (backend prop)
  "Return PROP for BACKEND from `inline-review-backend-registry'.
Signals an error if BACKEND is not registered."
  (let ((entry (assq backend inline-review-backend-registry)))
    (unless entry
      (error "inline-review: unknown backend `%s'" backend))
    (plist-get (cdr entry) prop)))

;;;; ─── Detection & Selection ─────────────────────────────────────────────────

(defun inline-review--detect-backend (remote-url)
  "Auto-detect backend symbol from REMOTE-URL, or nil if unrecognised."
  (car
   (cl-find-if
    (lambda (entry)
      (string-match-p (plist-get (cdr entry) :remote-re) remote-url))
    inline-review-backend-registry)))

(defun inline-review--ensure-backend ()
  "Determine and return the backend to use.
Uses `inline-review-backend' if set, otherwise auto-detects from remote URL.
Caches the result per repository."
  (unless inline-review--current-backend
    (let ((cached (inline-review--load-cached-backend)))
      (if cached
          (setq inline-review--current-backend cached)
        (let* ((remote (inline-review--git-remote-url))
               (detected (inline-review--detect-backend remote))
               (backend (or inline-review-backend detected)))
          (if backend
              (progn
                (setq inline-review--current-backend backend)
                (inline-review--save-backend backend)
                (message
                 "inline-review: auto-detected %s backend from remote"
                 backend))
            (user-error
             "inline-review: Cannot detect backend from remote: %s. \
Please set `inline-review-backend'"
             remote))))))
  inline-review--current-backend)

;;;; ─── Token Management ───────────────────────────────────────────────────────

(defun inline-review--git-config (key)
  "Return the value of git config KEY, or nil if unset."
  (let ((val
         (string-trim
          (shell-command-to-string
           (format "git config --global %s 2>/dev/null" key)))))
    (and (not (string-empty-p val)) val)))

(defun inline-review--authinfo-token (host backend)
  "Look up a token for HOST in authinfo/netrc via `auth-source'.
Returns the secret string, or nil if not found.

Searches in order:
  1. login ^crm                   — dedicated inline-review entry
  2. login <git-config-user>^crm  — per-user entry (git config BACKEND.user)
  3. any login on HOST            — fallback"
  (let* ((git-user
          (inline-review--git-config
           (format "%s.user" (symbol-name backend))))
         (found
          (or (car
               (auth-source-search :host host :user "\\^crm" :max 1))
              (and git-user
                   (car
                    (auth-source-search
                     :host host
                     :user
                     (concat git-user "\\^crm")
                     :max 1)))
              (car (auth-source-search :host host :max 1)))))
    (when found
      (let ((secret (plist-get found :secret)))
        (if (functionp secret)
            (funcall secret)
          secret)))))

(defun inline-review--backend-host (backend)
  "Return the hostname for BACKEND, derived from its base-URL defcustom."
  (replace-regexp-in-string
   "^https?://\\([^/]+\\).*" "\\1"
   (symbol-value
    (inline-review--backend-prop backend :api-url-var))))

(defun inline-review--get-token (backend)
  "Get the authentication token for BACKEND from authinfo/netrc."
  (inline-review--authinfo-token
   (inline-review--backend-host backend) backend))

(defun inline-review--assert-token (backend)
  "Signal an error if no token is found in authinfo for BACKEND."
  (unless (let ((tok (inline-review--get-token backend)))
            (and (stringp tok) (not (string-empty-p tok))))
    (user-error
     "inline-review: No token found for %s.  \
Add an entry to ~/.authinfo (or ~/.authinfo.gpg), e.g.:\n  machine %s login ^crm password <token>"
     backend (inline-review--backend-host backend))))

;;;; ─── Remote & URL Parsing ───────────────────────────────────────────────────

(defun inline-review--git-remote-url ()
  "Return the URL of the `origin' remote."
  (let ((default-directory
         (or (locate-dominating-file
              (or buffer-file-name default-directory) ".git")
             default-directory)))
    (string-trim
     (shell-command-to-string
      "git remote get-url origin 2>/dev/null"))))

(defun inline-review--parse-mr-url (input)
  "Parse a MR/PR URL or bare integer INPUT.
Returns a plist with :iid and optionally :backend and :project-info, or nil.

Supported URL formats:
  GitHub:   https://github.com/OWNER/REPO/pull/IID
  GitLab:   https://gitlab.com/NS/PROJECT/-/merge_requests/IID
  Gongfeng: https://git.woa.com/NS/PROJECT/-/merge_requests/IID"
  (when (and input (not (string-empty-p (string-trim input))))
    (let ((s (string-trim input)))
      (cond
       ;; GitHub: https://HOST/OWNER/REPO/pull[s]/IID
       ((string-match
         "https?://\\([^/]*github[^/]*\\)/\\([^/]+\\)/\\([^/]+\\)/pulls?/\\([0-9]+\\)"
         s)
        (list
         :iid (string-to-number (match-string 4 s))
         :backend 'github
         :project-info
         `((owner . ,(match-string 2 s))
           (repo . ,(match-string 3 s)))))
       ;; Codeberg: https://HOST/OWNER/REPO/pulls/IID
       ((string-match
         "https?://\\([^/]*codeberg[^/]*\\)/\\([^/]+\\)/\\([^/]+\\)/pulls/\\([0-9]+\\)"
         s)
        (list
         :iid (string-to-number (match-string 4 s))
         :backend 'codeberg
         :project-info
         `((owner . ,(match-string 2 s))
           (repo . ,(match-string 3 s)))))
       ;; GitLab / Gongfeng: https://HOST/NS/.../REPO/-/merge_requests/IID
       ((string-match
         "https?://\\([^/]+\\)/\\(.*\\)/-/merge_requests/\\([0-9]+\\)"
         s)
        (let* ((host (match-string 1 s))
               (path (match-string 2 s))
               (iid (string-to-number (match-string 3 s)))
               (backend (inline-review--detect-backend host)))
          (list
           :iid iid
           :backend backend
           :project-info `((project-id . ,(url-hexify-string path))))))
       ;; Bare integer fallback
       ((string-match "\\`[0-9]+\\'" s)
        (list :iid (string-to-number s)))))))

;;;; ─── Per-repo Cache ─────────────────────────────────────────────────────────

(defvar inline-review--iid-cache (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → MR IID (integer).")

(defvar inline-review--backend-cache
  (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → backend symbol.")

(defvar inline-review--diff-cache (make-hash-table :test 'equal)
  "In-memory cache mapping MR key → list of change plists.
The key is produced by `inline-review--diff-cache-key'.")

(defvar inline-review--review-active-cache
  (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → active MR IID (integer).
Set only after `inline-review-review-url' finishes all preparation
steps (branch checkout + mode activation).  Cleared per-project by
`inline-review-finish-review'.  Multiple projects may have
simultaneous active entries.")


(defun inline-review--git-root ()
  "Return the absolute path to the git root for the current buffer, or nil."
  (when-let ((root
              (locate-dominating-file
               (or buffer-file-name default-directory) ".git")))
    (expand-file-name root)))

(defun inline-review--cache-file (filename)
  "Return the path to a per-repo cache file in .git/ directory."
  (when-let ((root (inline-review--git-root)))
    (expand-file-name filename (expand-file-name ".git" root))))

(defun inline-review--load-cached-iid ()
  "Return the persisted MR IID for the current repo, or nil."
  (let ((root (inline-review--git-root)))
    (or (and root (gethash root inline-review--iid-cache))
        (when-let ((file
                    (inline-review--cache-file
                     "inline-review-iid")))
          (when (file-readable-p file)
            (let* ((raw
                    (with-temp-buffer
                      (insert-file-contents file)
                      (string-trim (buffer-string))))
                   (iid (string-to-number raw)))
              (when (and (integerp iid) (> iid 0))
                (when root
                  (puthash root iid inline-review--iid-cache))
                iid)))))))

(defun inline-review--save-iid (iid)
  "Persist IID for the current repo."
  (when-let ((root (inline-review--git-root)))
    (puthash root iid inline-review--iid-cache))
  (when-let ((file
              (inline-review--cache-file
               "inline-review-iid")))
    (write-region (number-to-string iid) nil file nil 'silent)))

(defun inline-review--load-cached-backend ()
  "Return the persisted backend for the current repo, or nil."
  (let ((root (inline-review--git-root)))
    (or (and root (gethash root inline-review--backend-cache))
        (when-let ((file
                    (inline-review--cache-file
                     "inline-review-backend")))
          (when (file-readable-p file)
            (let ((backend
                   (with-temp-buffer
                     (insert-file-contents file)
                     (string-trim (buffer-string)))))
              (when (> (length backend) 0)
                (when root
                  (puthash
                   root backend inline-review--backend-cache))
                (intern backend))))))))

(defun inline-review--save-backend (backend)
  "Persist BACKEND for the current repo."
  (when-let ((root (inline-review--git-root)))
    (puthash root backend inline-review--backend-cache))
  (when-let ((file
              (inline-review--cache-file
               "inline-review-backend")))
    (write-region (symbol-name backend) nil file nil 'silent)))

;;;; ─── Buffer-local State ─────────────────────────────────────────────────────

(defvar-local inline-review--mr-iid nil
  "MR IID (per-project integer id) currently being reviewed.")

(defvar-local inline-review--mr-id nil
  "MR global integer id resolved from `inline-review--mr-iid'.")

(defvar-local inline-review--mr-source-branch nil
  "Source branch name for the MR currently being reviewed, or nil if unknown.")

(defvar-local inline-review--mr-target-branch nil
  "Target/base branch name for the MR currently being reviewed, or nil if unknown.")

(defvar-local inline-review--project-info nil
  "Project info alist with backend-specific keys.
GitHub: ((owner . \"user\") (repo . \"project\"))
GitLab/Gongfeng: ((project-id . \"namespace%2Fproject\"))")

(defvar-local inline-review--current-backend nil
  "The backend symbol currently in use (github, gitlab, gongfeng).")

;;;; ─── Shared Utility Helpers ─────────────────────────────────────────────────

(defun inline-review--relative-file-path ()
  "Return the path of the current buffer's file relative to git root."
  (when buffer-file-name
    (let* ((root (locate-dominating-file buffer-file-name ".git")))
      (if root
          (file-relative-name buffer-file-name
                              (expand-file-name root))
        (file-name-nondirectory buffer-file-name)))))

(defun inline-review--line-number-at (pos)
  "Return 1-based line number for POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-backend)

;;; inline-review-backend.el ends here
