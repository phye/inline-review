;;; inline-review-overview.el --- Tree overview for inline-review MR/PR -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; The `*inline-review-overview*' buffer shows a folding tree of files
;; changed in the current MR/PR.  The file list is derived from the
;; same cached backend diff that drives `inline-review-next-hunk' /
;; `inline-review-previous-hunk', so overview entries and hunk
;; navigation always see an identical file set — the one the forge's
;; MR UI shows and the one against whose line numbers inline comments
;; are anchored.  Directory nodes are togglable via RET; file nodes
;; RET-open the file in another window (or the current window with a
;; prefix argument).  `n'/`p' navigate between tree entries; `o' opens
;; the file link on the current line in another window.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)

(declare-function inline-review--git-root                "inline-review-backend" ())
(declare-function inline-review--backend-prop            "inline-review-backend" (backend prop))
(declare-function inline-review--diff-cache-key          "inline-review-diff"    (backend iid project-info))
(declare-function inline-review--review-in-progress-p    "inline-review"         ())
(declare-function inline-review-mode                     "inline-review"         (&optional arg))
(declare-function inline-review-refresh                  "inline-review"         ())

(defvar inline-review--current-backend)
(defvar inline-review--mr-iid)
(defvar inline-review--project-info)
(defvar inline-review--diff-cache)

;;;; ─── State ─────────────────────────────────────────────────────────────────

(defvar-local inline-review--overview-tree nil
  "Hash-table representing the file tree for the current MR.
Directory nodes are nested hash-tables; file leaves are plists
with `:file' (relative path) and `:stat' (visualisation string).")

(defvar-local inline-review--overview-folded nil
  "Hash-table of currently folded directory paths (\"a/b\" -> t).")

(defvar-local inline-review--overview-summary nil
  "Trailing summary line (\"N files changed, ...\"), or nil.")

(defvar-local inline-review--overview-root nil
  "Absolute project root corresponding to the tree entries.")

(defvar-local inline-review--overview-origin-buffer nil
  "Buffer from which the overview was invoked.
Used by `inline-review-overview-refresh' and
`inline-review-overview-regenerate' to run commands in the context of
an `inline-review-mode' buffer that carries the MR state the overview
needs.  When this buffer is dead, the wrapper commands fall back to any
live `inline-review-mode' buffer under `inline-review--overview-root'.")

;;;; ─── Faces ─────────────────────────────────────────────────────────────────

(defface inline-review-overview-directory
  '((t :inherit dired-directory))
  "Face for directory entries in the inline-review overview tree."
  :group 'inline-review)

;;;; ─── Tree Data ─────────────────────────────────────────────────────────────

(defun inline-review--overview-tree-insert (root segments leaf)
  "Insert LEAF at path SEGMENTS (list of strings) into tree ROOT.
ROOT is a hash-table representing a directory; child directories are
hash-tables and file leaves are plists (`:file' `:stat')."
  (if (null (cdr segments))
      (puthash (car segments) leaf root)
    (let ((sub (gethash (car segments) root)))
      (unless (hash-table-p sub)
        (setq sub (make-hash-table :test 'equal))
        (puthash (car segments) sub root))
      (inline-review--overview-tree-insert sub (cdr segments) leaf))))

(defun inline-review--overview-patch-counts (patch)
  "Return (ADDED . REMOVED) line counts for PATCH string.
Lines beginning with a single `+' or `-' count toward additions and
removals; the diff header lines `+++' and `---' and the hunk header
`@@' lines do not."
  (let ((added 0)
        (removed 0))
    (when patch
      (dolist (line (split-string patch "\n"))
        (cond
         ((string-prefix-p "+++" line))
         ((string-prefix-p "---" line))
         ((string-prefix-p "@@" line))
         ((string-prefix-p "+" line) (cl-incf added))
         ((string-prefix-p "-" line) (cl-incf removed)))))
    (cons added removed)))

(defun inline-review--overview-stat-string (added removed)
  "Format a git-stat-like \"<total> <bar>\" string from ADDED and REMOVED.
The bar is scaled so its total length never exceeds 20 characters,
matching git's default `--stat-graph-width'."
  (let* ((total (+ added removed))
         (max-bar 20))
    (cond
     ((zerop total) "0")
     ((<= total max-bar)
      (format "%d %s%s"
              total
              (make-string added ?+)
              (make-string removed ?-)))
     (t
      (let* ((scale (/ (float max-bar) total))
             (p (max (if (zerop added) 0 1) (round (* added scale))))
             (m (max (if (zerop removed) 0 1) (- max-bar p))))
        (format "%d %s%s"
                total
                (make-string p ?+)
                (make-string m ?-)))))))

(defun inline-review--overview-entry-from-change (change)
  "Return (REL-PATH . STAT-STR) for CHANGE plist, or nil to skip.
Mirrors the filter used by `inline-review--all-hunk-positions' so the
overview tree lists exactly the files hunk navigation visits — pure
deletions (both paths resolve to \"/dev/null\" on the new side) are
excluded."
  (let* ((new (plist-get change :new-path))
         (old (plist-get change :old-path))
         (rel (or new old)))
    (when (and rel (not (string= rel "/dev/null")))
      (let* ((counts (inline-review--overview-patch-counts
                      (plist-get change :patch)))
             (stat (inline-review--overview-stat-string
                    (car counts) (cdr counts))))
        (cons rel stat)))))

(defun inline-review--overview-summary-line (entries changes)
  "Return the \"N files changed, …\" summary for ENTRIES/CHANGES, or nil."
  (let ((files (length entries))
        (added 0)
        (removed 0))
    (dolist (c changes)
      (let ((counts (inline-review--overview-patch-counts
                     (plist-get c :patch))))
        (cl-incf added (car counts))
        (cl-incf removed (cdr counts))))
    (when (> files 0)
      (format "%d file%s changed, %d insertion%s(+), %d deletion%s(-)"
              files   (if (= files 1) "" "s")
              added   (if (= added 1) "" "s")
              removed (if (= removed 1) "" "s")))))

(defun inline-review--overview-build-tree (entries)
  "Return a hash-table tree built from ENTRIES (list of (REL . STAT))."
  (let ((tree (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let ((segs (split-string (car e) "/" t)))
        (when segs
          (inline-review--overview-tree-insert
           tree segs
           (list :file (car e) :stat (cdr e))))))
    tree))

;;;; ─── Rendering ─────────────────────────────────────────────────────────────

(defun inline-review--overview-render (node prefix parent-path)
  "Render NODE (hash-table) at indentation PREFIX.
PARENT-PATH is the slash-joined path of the parent directory (\"\" at
root).  Directories sort first, then files; only file leaves get the
stat suffix; directories carry a fold indicator and a togglable button."
  (let* ((keys (sort (hash-table-keys node)
                     (lambda (a b)
                       (let ((va (gethash a node))
                             (vb (gethash b node)))
                         (cond
                          ((and (hash-table-p va)
                                (not (hash-table-p vb)))
                           t)
                          ((and (not (hash-table-p va))
                                (hash-table-p vb))
                           nil)
                          (t (string< a b)))))))
         (last-idx (1- (length keys))))
    (cl-loop
     for k in keys
     for i from 0
     for is-last = (= i last-idx)
     for val = (gethash k node)
     do
     (let* ((branch (if is-last "└── " "├── "))
            (line-prefix (concat prefix branch))
            (child-prefix (concat prefix (if is-last "    " "│   "))))
       (cond
        ((hash-table-p val)
         (let* ((path (if (string-empty-p parent-path)
                          k
                        (concat parent-path "/" k)))
                (folded (gethash path
                                 inline-review--overview-folded))
                (indicator (if folded "▸ " "▾ ")))
           (insert line-prefix indicator)
           (let ((name-beg (point)))
             (insert k "/")
             (make-text-button
              name-beg (point)
              'inline-review-dir path
              'action 'inline-review--overview-toggle-dir
              'follow-link t
              'help-echo "RET to toggle folding"
              'face 'inline-review-overview-directory))
           (insert "\n")
           (unless folded
             (inline-review--overview-render val child-prefix path))))
        (t
         (let* ((rel  (plist-get val :file))
                (abs  (expand-file-name
                       rel inline-review--overview-root))
                (stat (plist-get val :stat)))
           (insert line-prefix)
           (let ((name-beg (point)))
             (insert k)
             (make-text-button
              name-beg (point)
              'inline-review-file abs
              'action 'inline-review--overview-open-file
              'follow-link t
              'help-echo "RET to open (C-u for current window); o for other window"
              'face 'link))
           (when (and stat (not (string-empty-p stat)))
             (insert " | " stat))
           (insert "\n"))))))))

(defun inline-review--overview-refresh ()
  "Re-render the tree in the current overview buffer.
Preserves point on the same directory (if the caller was on a directory
button) so RET-toggle keeps the cursor on the toggled line."
  (let* ((pinned-path (let ((btn (button-at (point))))
                        (and btn (button-get btn 'inline-review-dir))))
         (inhibit-read-only t))
    (erase-buffer)
    (inline-review--overview-render
     inline-review--overview-tree "" "")
    (when inline-review--overview-summary
      (insert "\n" inline-review--overview-summary "\n"))
    (goto-char (point-min))
    (when pinned-path
      (let ((btn (next-button (point-min) t)))
        (while (and btn
                    (not (equal (button-get btn 'inline-review-dir)
                                pinned-path)))
          (setq btn (next-button (button-end btn))))
        (when btn
          (goto-char (button-start btn)))))))

;;;; ─── Button Actions ────────────────────────────────────────────────────────

(defun inline-review--overview-toggle-dir (button)
  "Toggle fold state for the directory represented by BUTTON."
  (let ((path (button-get button 'inline-review-dir)))
    (if (gethash path inline-review--overview-folded)
        (remhash path inline-review--overview-folded)
      (puthash path t inline-review--overview-folded))
    (inline-review--overview-refresh)))

(defun inline-review--overview-open-file (button)
  "Open the file associated with BUTTON in the overview buffer.
By default open the file in another window; with a prefix argument
\\[universal-argument] open it in the current window.  Enables
`inline-review-mode' in the newly opened buffer so overlays are
fetched immediately — mode activation picks up MR state (project-info,
branch names, mr-id) from `.git/inline-review-mr-state', so a fresh
buffer for a newly-introduced file inherits the same cache key the
original `review-url' fetch used."
  (let ((file (button-get button 'inline-review-file)))
    (if current-prefix-arg
        (find-file file)
      (find-file-other-window file))
    (unless (bound-and-true-p inline-review-mode)
      (inline-review-mode 1))))

(defun inline-review--overview-line-button ()
  "Return the button on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (let ((eol (line-end-position))
          (btn (button-at (point))))
      (unless btn
        (setq btn (next-button (point) t))
        (when (and btn (> (button-start btn) eol))
          (setq btn nil)))
      btn)))

(defun inline-review-overview-open-file-other-window ()
  "Open the file link on the current line in another window.
No-op with a message if the current line is a directory entry."
  (interactive)
  (let ((btn (inline-review--overview-line-button)))
    (unless btn
      (user-error "inline-review: no entry on this line"))
    (unless (button-get btn 'inline-review-file)
      (user-error "inline-review: current line is a directory"))
    (let ((current-prefix-arg nil))
      (button-activate btn))))

;;;; ─── Navigation ────────────────────────────────────────────────────────────

(defun inline-review-overview-next-entry ()
  "Move point to the next tree entry (directory or file button)."
  (interactive)
  (condition-case nil
      (forward-button 1 nil nil)
    (error (user-error "inline-review: no next entry"))))

(defun inline-review-overview-previous-entry ()
  "Move point to the previous tree entry (directory or file button)."
  (interactive)
  (condition-case nil
      (backward-button 1 nil nil)
    (error (user-error "inline-review: no previous entry"))))

;;;; ─── Refresh / Regenerate ──────────────────────────────────────────────────

(defun inline-review--overview-source-buffer ()
  "Return a live `inline-review-mode' buffer that carries this MR's state.
Prefers `inline-review--overview-origin-buffer' when still alive, otherwise
falls back to any `inline-review-mode' buffer under
`inline-review--overview-root'.  Signals a `user-error' when none exists."
  (let ((origin inline-review--overview-origin-buffer)
        (root   inline-review--overview-root)
        (found  nil))
    (cond
     ((and origin (buffer-live-p origin)
           (with-current-buffer origin
             (bound-and-true-p inline-review-mode)))
      origin)
     (root
      (dolist (buf (buffer-list))
        (unless found
          (with-current-buffer buf
            (when (and (bound-and-true-p inline-review-mode)
                       buffer-file-name
                       (string-prefix-p
                        root (expand-file-name buffer-file-name)))
              (setq found buf)))))
      (or found
          (user-error
           "inline-review: no active review buffer found for %s" root)))
     (t
      (user-error "inline-review: overview has no project root")))))

(defun inline-review-overview-regenerate ()
  "Regenerate the overview tree from the current diff cache.
Runs `inline-review-overview' in the originating review buffer so that
the backend / IID / project-info it needs are in scope."
  (interactive)
  (let ((src (inline-review--overview-source-buffer)))
    (with-current-buffer src
      (inline-review-overview))))

(defun inline-review-overview-refresh ()
  "Invalidate the diff cache for this MR and regenerate the overview.
Runs `inline-review-refresh' in the originating review buffer (which
clears the cached diff and re-fetches overlays), then re-renders the
overview tree from the fresh diff."
  (interactive)
  (let ((src (inline-review--overview-source-buffer)))
    (with-current-buffer src
      (inline-review-refresh)
      (inline-review-overview))))

;;;; ─── Mode ──────────────────────────────────────────────────────────────────

(defvar inline-review-overview-mode-map (make-sparse-keymap)
  "Keymap for `inline-review-overview-mode'.
Populated from `inline-review-overview-key-bindings'; customize that
variable via \\[customize-option] rather than editing this map directly
so that changes made through Customize are honoured.")

(defun inline-review--overview-apply-key-bindings (map bindings)
  "Reset MAP and (re)install BINDINGS on it.
BINDINGS is an alist of (KEY . COMMAND) where KEY is a `kbd' string."
  (setcdr map nil)
  (dolist (b bindings)
    (define-key map (kbd (car b)) (cdr b))))

(defcustom inline-review-overview-key-bindings
  '(("q"     . quit-window)
    ("n"     . inline-review-overview-next-entry)
    ("p"     . inline-review-overview-previous-entry)
    ("o"     . inline-review-overview-open-file-other-window)
    ("r"     . inline-review-overview-refresh)
    ("g"     . inline-review-overview-regenerate)
    ("<RET>" . inline-review-overview-open-file-other-window))
  "Key bindings for `inline-review-overview-mode'.
Each entry is (KEY . COMMAND), where KEY is a string in `kbd' notation
and COMMAND is the command symbol to invoke.  Setting this via
\\[customize-option] rebuilds `inline-review-overview-mode-map' so
existing overview buffers pick up the new bindings on their next
command lookup."
  :type '(alist :key-type (string :tag "Key (kbd notation)")
                :value-type (function :tag "Command"))
  :group 'inline-review
  :set (lambda (sym val)
         (set-default sym val)
         (when (boundp 'inline-review-overview-mode-map)
           (inline-review--overview-apply-key-bindings
            inline-review-overview-mode-map val))))

(inline-review--overview-apply-key-bindings
 inline-review-overview-mode-map
 inline-review-overview-key-bindings)

(define-derived-mode inline-review-overview-mode special-mode
  "IR-Overview"
  "Major mode for the `*inline-review-overview*' buffer.
Displays a folding tree of files changed in the current MR/PR.
\\<inline-review-overview-mode-map>
\\[inline-review-overview-next-entry] / \\[inline-review-overview-previous-entry]
move between entries; RET toggles a directory or opens a file
(other window by default, current window with a prefix arg); \
\\[inline-review-overview-open-file-other-window]
always opens the file on the current line in another window.
\\[inline-review-overview-refresh] re-fetches the diff and regenerates
the overview; \\[inline-review-overview-regenerate] regenerates it
from the already-cached diff."
  :group 'inline-review)

;; Evil users: force `emacs' state so our local keymap wins over
;; `evil-normal-state-map' bindings for `n', `p', `o', RET etc.  This is
;; a no-op when evil is not loaded.
(with-eval-after-load 'evil
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'inline-review-overview-mode 'emacs)))

;;;; ─── Entry Point ───────────────────────────────────────────────────────────

;;;###autoload
(defun inline-review-overview ()
  "Pop up a folding tree of files changed in the current MR/PR.
The file list is derived from the backend's cached version-selection
diff — the same source `inline-review-next-hunk' walks — so overview
entries and hunk navigation always see an identical set of files, and
every file shown is one against whose line numbers inline comments can
be anchored.  If the diff hasn't been fetched yet, this triggers the
backend's `:fetch-diff' once and renders when it returns.  Use
`inline-review-refresh' to invalidate the cache and re-fetch."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review — run `inline-review-review-url' first"))
  (let ((backend inline-review--current-backend)
        (iid     inline-review--mr-iid)
        (proj    inline-review--project-info)
        (root    (inline-review--git-root))
        (origin  (current-buffer)))
    (unless backend
      (user-error "inline-review: no backend set for this buffer"))
    (unless iid
      (user-error "inline-review: no MR IID set for this buffer"))
    (unless root
      (user-error "inline-review: not inside a git repository"))
    (let* ((key (inline-review--diff-cache-key backend iid proj))
           (cached (gethash key inline-review--diff-cache)))
      (if cached
          (inline-review--overview-render-changes cached root origin)
        (message "inline-review: fetching diff for overview...")
        (funcall (inline-review--backend-prop backend :fetch-diff)
                 (lambda (changes)
                   (puthash key changes inline-review--diff-cache)
                   (inline-review--overview-render-changes
                    changes root origin)))))))

(defun inline-review--overview-render-changes (changes root &optional origin)
  "Render the overview tree for CHANGES anchored at project ROOT.
ORIGIN, when non-nil, is the `inline-review-mode' buffer the overview
was invoked from; it is stored so that `r' / `g' bindings can run
refresh and regeneration in a buffer that carries the MR state."
  (let* ((entries (delq nil
                        (mapcar #'inline-review--overview-entry-from-change
                                changes)))
         (summary (inline-review--overview-summary-line entries changes))
         (tree    (inline-review--overview-build-tree entries))
         (outbuf  (get-buffer-create "*inline-review-overview*")))
    (with-current-buffer outbuf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Enable the mode *before* populating buffer-local state —
        ;; `define-derived-mode' runs `kill-all-local-variables'.
        (inline-review-overview-mode)
        (setq inline-review--overview-tree    tree
              inline-review--overview-folded  (make-hash-table :test 'equal)
              inline-review--overview-summary summary
              inline-review--overview-root    root
              inline-review--overview-origin-buffer
              (and (buffer-live-p origin) origin))
        (if entries
            (inline-review--overview-refresh)
          (insert "inline-review: no files changed in this MR\n")
          (goto-char (point-min)))))
    (pop-to-buffer-same-window outbuf)))

(provide 'inline-review-overview)
;;; inline-review-overview.el ends here
