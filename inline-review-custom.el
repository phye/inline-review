;;; inline-review-custom.el --- Customization variables and faces -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review, faces

;;; Commentary:
;;
;; All `defgroup', `defcustom', and `defface' declarations for
;; inline-review.  Every other file in the package requires this file,
;; so that M-x customize-group shows every option and face in one place.
;;
;; Customization variables:
;;   `inline-review-backend'                    — backend override (nil = auto)
;;   `inline-review-github-api-url'             — GitHub API base URL
;;   `inline-review-gitlab-api-url'             — GitLab API base URL
;;   `inline-review-gongfeng-api-url'           — Gongfeng API base URL
;;   `inline-review-codeberg-api-url'           — Codeberg API base URL
;;   `inline-review-hide-resolved'              — suppress resolved threads
;;   `inline-review-highlight-hunks'            — enable diff hunk overlays
;;   `inline-review-inline-removed-lines-limit' — truncation threshold
;;
;; Faces:
;;   Comment overlays:
;;     `inline-review-comment-face'       — unresolved comment body
;;     `inline-review-resolved-body-face' — resolved comment body
;;     `inline-review-input-face'         — comment-input overlay
;;   Inline status / header:
;;     `inline-review-header-face'        — author/date header line
;;     `inline-review-resolved-face'      — ✓resolved status indicator
;;     `inline-review-unresolved-face'    — ○open status indicator
;;   Hunk highlighting (background-only — syntax faces show through):
;;     `inline-review-hunk-added-face'    — added lines
;;     `inline-review-hunk-removed-face'  — removed lines (inline)
;;     `inline-review-hunk-region-face'   — overall hunk region background
;;
;; Color palette
;; ─────────────
;; Dark theme — muted blues/greens on dark backgrounds, high enough contrast
;;   for readability without being jarring next to source code:
;;   • comment bg  #1e2a3a  (deep navy)    fg  #9ec8f0  (sky blue)
;;   • resolved bg #1a2e1a  (deep green)   fg  #7ec87e  (sage green)
;;   • input bg    #1e2e1e  (deep green)   fg  #98e898  (light green)
;;   • header fg   #6ab0e8  (cornflower)
;;   • resolved fg #5ec45e  (medium green)
;;   • open fg     #6ab0e8  (cornflower)
;;
;; Light theme — soft tinted backgrounds, dark foregrounds for legibility:
;;   • comment bg  #edf4ff  (light blue tint) fg  #1a3a6e  (dark navy)
;;   • resolved bg #edfaed  (light green)     fg  #1a4a1a  (dark green)
;;   • input bg    #f0fff0  (mint)             fg  #1a4a1a  (dark green)
;;   • header fg   #1a4080  (dark blue)
;;   • resolved fg #1a6a1a  (dark green)
;;   • open fg     #1a4080  (dark blue)

;;; Code:

;;;; ─── Group ──────────────────────────────────────────────────────────────────

(defgroup inline-review nil
  "Code-review overlays for GitHub/GitLab/Gongfeng/Codeberg Pull/Merge Requests."
  :group 'tools
  :prefix "inline-review-")

;;;; ─── Backend ────────────────────────────────────────────────────────────────

(defcustom inline-review-backend nil
  "Backend override for code review.
If nil (the default), the backend is auto-detected from the git remote URL
and the result is cached per repository.  Only set this when auto-detection
fails or gives the wrong result for a particular repository.

Valid values: nil (auto), or any backend symbol registered in
`inline-review-backend-registry' (e.g. `github', `gitlab', `gongfeng', `codeberg').

This variable is intended to be set per-repository via a .dir-locals.el file:

  ((nil . ((inline-review-backend . gongfeng))))

It is declared safe for directory-local use so Emacs will not prompt for
confirmation when the value is a registered backend symbol."
  :type
  '(choice
    (const :tag "Auto-detect" nil)
    (const :tag "GitHub" github)
    (const :tag "GitLab" gitlab)
    (const :tag "Gongfeng (工蜂)" gongfeng)
    (const :tag "Codeberg" codeberg))
  :group 'inline-review)

;; Derived from the registry at runtime so new backends are automatically valid.
;; `inline-review-backend-registry' is defined in inline-review-backend.el,
;; which loads after this file; the lambda is only called at dir-local validation
;; time (not at load time), so the forward reference is safe.
(put
 'inline-review-backend 'safe-local-variable
 (lambda (v)
   (or (null v)
       (and (boundp 'inline-review-backend-registry)
            (assq v inline-review-backend-registry)))))

;;;; ─── API URLs ───────────────────────────────────────────────────────────────

(defcustom inline-review-github-api-url "https://api.github.com"
  "Base URL for GitHub API.
For GitHub Enterprise, use: https://your-github-enterprise.com/api/v3"
  :type 'string
  :group 'inline-review)

(defcustom inline-review-gitlab-api-url
  "https://gitlab.com/api/v4"
  "Base URL for GitLab API.
For self-hosted GitLab, use: https://your-gitlab.com/api/v4"
  :type 'string
  :group 'inline-review)

(defcustom inline-review-gongfeng-api-url
  "https://git.woa.com/api/v3"
  "Base URL for Gongfeng API."
  :type 'string
  :group 'inline-review)

(defcustom inline-review-codeberg-api-url
  "https://codeberg.org/api/v1"
  "Base URL for Codeberg (Gitea) API.
For self-hosted Gitea instances, use: https://your-gitea.com/api/v1"
  :type 'string
  :group 'inline-review)

;;;; ─── Display ────────────────────────────────────────────────────────────────

(defcustom inline-review-hide-resolved nil
  "When non-nil, do not render overlays for resolved comment threads."
  :type 'boolean
  :group 'inline-review)

(defcustom inline-review-highlight-hunks t
  "When non-nil, highlight diff hunks with overlays in review buffers.
Added lines are shown with a green tint, removed lines are displayed
inline in red, and the overall hunk region gets a subtle background."
  :type 'boolean
  :group 'inline-review)

(defcustom inline-review-inline-removed-lines-limit 5
  "Maximum number of removed lines to display inline.
When a deleted block exceeds this count, the remaining lines are hidden
behind a truncation indicator; use `inline-review-view-removed-lines'
to view the full block in a popup buffer."
  :type 'integer
  :group 'inline-review)

;;;; ─── Comment overlay faces ─────────────────────────────────────────────────

(defface inline-review-comment-face
  '((((background dark))
     :background "#1e2a3a"
     :foreground "#9ec8f0"
     :box (:line-width 1 :color "#3a5a7a")
     :extend t)
    (t
     :background "#edf4ff"
     :foreground "#1a3a6e"
     :box (:line-width 1 :color "#7aaad8")
     :extend t))
  "Face for the body of an unresolved CR comment overlay."
  :group 'inline-review)

(defface inline-review-resolved-body-face
  '((((background dark))
     :background "#1a2e1a"
     :foreground "#7ec87e"
     :box (:line-width 1 :color "#2e5e2e")
     :extend t)
    (t
     :background "#edfaed"
     :foreground "#1a4a1a"
     :box (:line-width 1 :color "#6ab86a")
     :extend t))
  "Face for the body of a resolved CR comment overlay."
  :group 'inline-review)

(defface inline-review-input-face
  '((((background dark))
     :background "#1e2e1e"
     :foreground "#98e898"
     :box (:line-width 1 :color "#366836")
     :extend t)
    (t
     :background "#f0fff0"
     :foreground "#1a4a1a"
     :box (:line-width 1 :color "#6ab86a")
     :extend t))
  "Face for the comment-input overlay."
  :group 'inline-review)

;;;; ─── Inline status / header faces ─────────────────────────────────────────

(defface inline-review-header-face
  '((((background dark)) :foreground "#6ab0e8" :weight bold)
    (t :foreground "#1a4080" :weight bold))
  "Face for the header line (author, date) inside a comment overlay."
  :group 'inline-review)

(defface inline-review-resolved-face
  '((((background dark)) :foreground "#5ec45e" :weight bold)
    (t :foreground "#1a6a1a" :weight bold))
  "Face for the ✓resolved status indicator in a comment overlay."
  :group 'inline-review)

(defface inline-review-unresolved-face
  '((((background dark)) :foreground "#6ab0e8" :weight bold)
    (t :foreground "#1a4080" :weight bold))
  "Face for the ○open status indicator in a comment overlay."
  :group 'inline-review)

(defface inline-review-outdated-face
  '((((background dark)) :foreground "#e8a06a" :weight bold)
    (t :foreground "#80401a" :weight bold))
  "Face for the ⚠outdated status indicator in a comment overlay."
  :group 'inline-review)

;;;; ─── Hunk highlighting faces ───────────────────────────────────────────────

(defface inline-review-hunk-added-face
  '((((background dark)) :background "#1a3a1a" :extend t)
    (t :background "#edfaed" :extend t))
  "Face for lines added in the current MR/PR diff.
Only a background tint is applied so that syntax-highlighting foreground
colours on the underlying buffer text are not overridden."
  :group 'inline-review)

(defface inline-review-hunk-removed-face
  '((((background dark)) :background "#8b1a1a" :extend t)
    (t :background "#ffcccc" :extend t))
  "Face for removed lines shown inline in the diff overlay.
Only a background tint is applied so that syntax-highlighting foreground
colours on the underlying buffer text are not overridden."
  :group 'inline-review)

(defface inline-review-hunk-region-face
  '((((background dark)) :background "#1e2530" :extend t)
    (t :background "#f0f4f8" :extend t))
  "Face for the overall hunk region background."
  :group 'inline-review)

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-custom)

;;; inline-review-custom.el ends here
