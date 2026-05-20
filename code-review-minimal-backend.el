;;; code-review-minimal-backend.el --- Backend registry, config, auth, and per-repo cache -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; This file is the shared foundation required by all backend files and by
;; code-review-minimal.el.  It provides:
;;
;;   Customization variables — see code-review-minimal-custom.el
;;   Backend registry (`code-review-minimal-backend-registry',
;;                     `code-review-minimal-register-backend',
;;                     `code-review-minimal--backend-prop')
;;   Backend detection & selection
;;     `code-review-minimal--detect-backend'
;;     `code-review-minimal--ensure-backend'
;;   Token management
;;     `code-review-minimal--git-config'
;;     `code-review-minimal--authinfo-token'
;;     `code-review-minimal--backend-host'
;;     `code-review-minimal--get-token'
;;     `code-review-minimal--assert-token'
;;   Remote & URL parsing
;;     `code-review-minimal--git-remote-url'
;;     `code-review-minimal--parse-mr-url'
;;   Per-repo cache
;;     `code-review-minimal--iid-cache'
;;     `code-review-minimal--backend-cache'
;;     `code-review-minimal--diff-cache'
;;     `code-review-minimal--git-root'
;;     `code-review-minimal--cache-file'
;;     `code-review-minimal--load-cached-iid' / `--save-iid'
;;     `code-review-minimal--load-cached-backend' / `--save-backend'
;;   Buffer-local state variables
;;     `code-review-minimal--mr-iid'
;;     `code-review-minimal--mr-id'
;;     `code-review-minimal--mr-source-branch'
;;     `code-review-minimal--mr-target-branch'
;;     `code-review-minimal--project-info'
;;     `code-review-minimal--current-backend'
;;   Utility helpers shared with diff and comment layers
;;     `code-review-minimal--relative-file-path'
;;     `code-review-minimal--line-number-at'

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'auth-source)
(require 'code-review-minimal-custom)

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

(defvar code-review-minimal-backend-registry nil
  "Alist mapping backend symbols to their configuration and function table.
Each entry: (BACKEND-SYMBOL . PLIST).  See `code-review-minimal-register-backend'
for the list of recognised plist keys.

Note: the value is always reset on package load so that reloading the file
picks up any additions (e.g. new :fetch-diff handlers).  User-added backends
via `code-review-minimal-register-backend' belong in init.el, which runs after
package load.")

;; Use setq (not the defvar initialiser) so that every reload refreshes the
;; built-in entries.  defvar only runs its initialiser when the variable is
;; void, meaning changes to this table would be invisible until Emacs restarts.
(setq code-review-minimal-backend-registry
      '((gongfeng
         :api-url-var code-review-minimal-gongfeng-api-url
         :remote-re "git\\.woa\\.com\\|code\\.tencent\\.com"
         :fetch code-review-minimal--gongfeng-fetch-comments
         :resolve-branches code-review-minimal--gongfeng-resolve-branches
         :fetch-diff code-review-minimal--gongfeng-fetch-diff
         :post code-review-minimal--gongfeng-post-comment
         :update code-review-minimal--gongfeng-update-comment
         :resolve code-review-minimal--gongfeng-resolve-comment
         :reply code-review-minimal--gongfeng-reply-comment
         :delete code-review-minimal--gongfeng-delete-comment)
        (github
         :api-url-var code-review-minimal-github-api-url
         :remote-re "github"
         :fetch code-review-minimal--github-fetch-comments
         :resolve-branches code-review-minimal--github-resolve-branches
         :fetch-diff code-review-minimal--github-fetch-diff
         :post code-review-minimal--github-post-comment
         :update code-review-minimal--github-update-comment
         :resolve code-review-minimal--github-resolve-comment
         :reply code-review-minimal--github-reply-comment
         :delete code-review-minimal--github-delete-comment)
        (gitlab
         :api-url-var code-review-minimal-gitlab-api-url
         :remote-re "gitlab"
         :fetch code-review-minimal--gitlab-fetch-comments
         :resolve-branches code-review-minimal--gitlab-resolve-branches
         :fetch-diff code-review-minimal--gitlab-fetch-diff
         :post code-review-minimal--gitlab-post-comment
         :update code-review-minimal--gitlab-update-comment
         :resolve code-review-minimal--gitlab-resolve-comment
         :reply code-review-minimal--gitlab-reply-comment
         :delete code-review-minimal--gitlab-delete-comment)
        (codeberg
         :api-url-var code-review-minimal-codeberg-api-url
         :remote-re "codeberg"
         :fetch code-review-minimal--codeberg-fetch-comments
         :resolve-branches code-review-minimal--codeberg-resolve-branches
         :fetch-diff code-review-minimal--codeberg-fetch-diff
         :post code-review-minimal--codeberg-post-comment
         :update code-review-minimal--codeberg-update-comment
         :resolve code-review-minimal--codeberg-resolve-comment
         :reply code-review-minimal--codeberg-reply-comment
         :delete code-review-minimal--codeberg-delete-comment)))

(defun code-review-minimal-register-backend (backend &rest plist)
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
  (setq code-review-minimal-backend-registry
        (cons
         (cons backend plist)
         (assq-delete-all
          backend code-review-minimal-backend-registry))))

(defun code-review-minimal--backend-prop (backend prop)
  "Return PROP for BACKEND from `code-review-minimal-backend-registry'.
Signals an error if BACKEND is not registered."
  (let ((entry (assq backend code-review-minimal-backend-registry)))
    (unless entry
      (error "code-review-minimal: unknown backend `%s'" backend))
    (plist-get (cdr entry) prop)))

;;;; ─── Detection & Selection ─────────────────────────────────────────────────

(defun code-review-minimal--detect-backend (remote-url)
  "Auto-detect backend symbol from REMOTE-URL, or nil if unrecognised."
  (car
   (cl-find-if
    (lambda (entry)
      (string-match-p (plist-get (cdr entry) :remote-re) remote-url))
    code-review-minimal-backend-registry)))

(defun code-review-minimal--ensure-backend ()
  "Determine and return the backend to use.
Uses `code-review-minimal-backend' if set, otherwise auto-detects from remote URL.
Caches the result per repository."
  (unless code-review-minimal--current-backend
    (let ((cached (code-review-minimal--load-cached-backend)))
      (if cached
          (setq code-review-minimal--current-backend cached)
        (let* ((remote (code-review-minimal--git-remote-url))
               (detected (code-review-minimal--detect-backend remote))
               (backend (or code-review-minimal-backend detected)))
          (if backend
              (progn
                (setq code-review-minimal--current-backend backend)
                (code-review-minimal--save-backend backend)
                (message
                 "code-review-minimal: auto-detected %s backend from remote"
                 backend))
            (user-error
             "code-review-minimal: Cannot detect backend from remote: %s. \
Please set `code-review-minimal-backend'"
             remote))))))
  code-review-minimal--current-backend)

;;;; ─── Token Management ───────────────────────────────────────────────────────

(defun code-review-minimal--git-config (key)
  "Return the value of git config KEY, or nil if unset."
  (let ((val
         (string-trim
          (shell-command-to-string
           (format "git config --global %s 2>/dev/null" key)))))
    (and (not (string-empty-p val)) val)))

(defun code-review-minimal--authinfo-token (host backend)
  "Look up a token for HOST in authinfo/netrc via `auth-source'.
Returns the secret string, or nil if not found.

Searches in order:
  1. login ^crm                   — dedicated code-review-minimal entry
  2. login <git-config-user>^crm  — per-user entry (git config BACKEND.user)
  3. any login on HOST            — fallback"
  (let* ((git-user
          (code-review-minimal--git-config
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

(defun code-review-minimal--backend-host (backend)
  "Return the hostname for BACKEND, derived from its base-URL defcustom."
  (replace-regexp-in-string
   "^https?://\\([^/]+\\).*" "\\1"
   (symbol-value
    (code-review-minimal--backend-prop backend :api-url-var))))

(defun code-review-minimal--get-token (backend)
  "Get the authentication token for BACKEND from authinfo/netrc."
  (code-review-minimal--authinfo-token
   (code-review-minimal--backend-host backend) backend))

(defun code-review-minimal--assert-token (backend)
  "Signal an error if no token is found in authinfo for BACKEND."
  (unless (let ((tok (code-review-minimal--get-token backend)))
            (and (stringp tok) (not (string-empty-p tok))))
    (user-error
     "code-review-minimal: No token found for %s.  \
Add an entry to ~/.authinfo (or ~/.authinfo.gpg), e.g.:\n  machine %s login ^crm password <token>"
     backend (code-review-minimal--backend-host backend))))

;;;; ─── Remote & URL Parsing ───────────────────────────────────────────────────

(defun code-review-minimal--git-remote-url ()
  "Return the URL of the `origin' remote."
  (let ((default-directory
         (or (locate-dominating-file
              (or buffer-file-name default-directory) ".git")
             default-directory)))
    (string-trim
     (shell-command-to-string
      "git remote get-url origin 2>/dev/null"))))

(defun code-review-minimal--parse-mr-url (input)
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
               (backend (code-review-minimal--detect-backend host)))
          (list
           :iid iid
           :backend backend
           :project-info `((project-id . ,(url-hexify-string path))))))
       ;; Bare integer fallback
       ((string-match "\\`[0-9]+\\'" s)
        (list :iid (string-to-number s)))))))

;;;; ─── Per-repo Cache ─────────────────────────────────────────────────────────

(defvar code-review-minimal--iid-cache (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → MR IID (integer).")

(defvar code-review-minimal--backend-cache
  (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → backend symbol.")

(defvar code-review-minimal--diff-cache (make-hash-table :test 'equal)
  "In-memory cache mapping MR key → list of change plists.
The key is produced by `code-review-minimal--diff-cache-key'.")

(defvar code-review-minimal--review-active-cache
  (make-hash-table :test 'equal)
  "In-memory cache mapping git-root (string) → active MR IID (integer).
Set only after `code-review-minimal-review-url' finishes all preparation
steps (branch checkout + mode activation).  Cleared per-project by
`code-review-minimal-finish-review'.  Multiple projects may have
simultaneous active entries.")


(defun code-review-minimal--git-root ()
  "Return the absolute path to the git root for the current buffer, or nil."
  (when-let ((root
              (locate-dominating-file
               (or buffer-file-name default-directory) ".git")))
    (expand-file-name root)))

(defun code-review-minimal--cache-file (filename)
  "Return the path to a per-repo cache file in .git/ directory."
  (when-let ((root (code-review-minimal--git-root)))
    (expand-file-name filename (expand-file-name ".git" root))))

(defun code-review-minimal--load-cached-iid ()
  "Return the persisted MR IID for the current repo, or nil."
  (let ((root (code-review-minimal--git-root)))
    (or (and root (gethash root code-review-minimal--iid-cache))
        (when-let ((file
                    (code-review-minimal--cache-file
                     "code-review-minimal-iid")))
          (when (file-readable-p file)
            (let* ((raw
                    (with-temp-buffer
                      (insert-file-contents file)
                      (string-trim (buffer-string))))
                   (iid (string-to-number raw)))
              (when (and (integerp iid) (> iid 0))
                (when root
                  (puthash root iid code-review-minimal--iid-cache))
                iid)))))))

(defun code-review-minimal--save-iid (iid)
  "Persist IID for the current repo."
  (when-let ((root (code-review-minimal--git-root)))
    (puthash root iid code-review-minimal--iid-cache))
  (when-let ((file
              (code-review-minimal--cache-file
               "code-review-minimal-iid")))
    (write-region (number-to-string iid) nil file nil 'silent)))

(defun code-review-minimal--load-cached-backend ()
  "Return the persisted backend for the current repo, or nil."
  (let ((root (code-review-minimal--git-root)))
    (or (and root (gethash root code-review-minimal--backend-cache))
        (when-let ((file
                    (code-review-minimal--cache-file
                     "code-review-minimal-backend")))
          (when (file-readable-p file)
            (let ((backend
                   (with-temp-buffer
                     (insert-file-contents file)
                     (string-trim (buffer-string)))))
              (when (> (length backend) 0)
                (when root
                  (puthash
                   root backend code-review-minimal--backend-cache))
                (intern backend))))))))

(defun code-review-minimal--save-backend (backend)
  "Persist BACKEND for the current repo."
  (when-let ((root (code-review-minimal--git-root)))
    (puthash root backend code-review-minimal--backend-cache))
  (when-let ((file
              (code-review-minimal--cache-file
               "code-review-minimal-backend")))
    (write-region (symbol-name backend) nil file nil 'silent)))

;;;; ─── Buffer-local State ─────────────────────────────────────────────────────

(defvar-local code-review-minimal--mr-iid nil
  "MR IID (per-project integer id) currently being reviewed.")

(defvar-local code-review-minimal--mr-id nil
  "MR global integer id resolved from `code-review-minimal--mr-iid'.")

(defvar-local code-review-minimal--mr-source-branch nil
  "Source branch name for the MR currently being reviewed, or nil if unknown.")

(defvar-local code-review-minimal--mr-target-branch nil
  "Target/base branch name for the MR currently being reviewed, or nil if unknown.")

(defvar-local code-review-minimal--project-info nil
  "Project info alist with backend-specific keys.
GitHub: ((owner . \"user\") (repo . \"project\"))
GitLab/Gongfeng: ((project-id . \"namespace%2Fproject\"))")

(defvar-local code-review-minimal--current-backend nil
  "The backend symbol currently in use (github, gitlab, gongfeng).")

;;;; ─── Shared Utility Helpers ─────────────────────────────────────────────────

(defun code-review-minimal--relative-file-path ()
  "Return the path of the current buffer's file relative to git root."
  (when buffer-file-name
    (let* ((root (locate-dominating-file buffer-file-name ".git")))
      (if root
          (file-relative-name buffer-file-name
                              (expand-file-name root))
        (file-name-nondirectory buffer-file-name)))))

(defun code-review-minimal--line-number-at (pos)
  "Return 1-based line number for POS."
  (save-excursion
    (goto-char pos)
    (line-number-at-pos)))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'code-review-minimal-backend)

;;; code-review-minimal-backend.el ends here
