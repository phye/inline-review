;;; inline-review-overview.el --- Tree overview for inline-review MR/PR -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; The `*inline-review-overview*' buffer shows a folding tree of files
;; changed in the current MR/PR (from `git diff --stat').  Directory
;; nodes are togglable via RET; file nodes RET-open the file in another
;; window (or the current window with a prefix argument).  `n'/`p'
;; navigate between tree entries; `o' opens the file link on the
;; current line in another window.

;;; Code:

(require 'cl-lib)
(require 'button)
(require 'subr-x)

(declare-function inline-review--git-root                "inline-review-backend" ())
(declare-function inline-review--review-in-progress-p    "inline-review"         ())
(declare-function inline-review-mode                     "inline-review"         (&optional arg))

(defvar inline-review--mr-source-branch)
(defvar inline-review--mr-target-branch)

;;;; ─── State ─────────────────────────────────────────────────────────────────

(defvar-local inline-review--overview-tree nil
  "Hash-table representing the parsed `git diff --stat' tree.
Directory nodes are nested hash-tables; file leaves are plists
with `:file' (relative path) and `:stat' (visualisation string).")

(defvar-local inline-review--overview-folded nil
  "Hash-table of currently folded directory paths (\"a/b\" -> t).")

(defvar-local inline-review--overview-summary nil
  "Trailing summary line (\"N files changed, ...\"), or nil.")

(defvar-local inline-review--overview-root nil
  "Absolute project root corresponding to the tree entries.")

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

(defun inline-review--overview-parse-stat ()
  "Parse `git diff --stat' output already in the current buffer.
Returns a plist (:entries ENTRIES :summary SUMMARY) where ENTRIES is a
list of (REL-PATH . STAT-STR) preserving git's order and SUMMARY is the
trailing summary line, or nil."
  (let ((entries nil)
        (summary nil))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position))))
        (cond
         ((string-match
           "\\` *\\([^|\n]+?\\) *|\\(.*\\)\\'" line)
          (let ((file (match-string 1 line))
                (stat (string-trim (match-string 2 line))))
            (when (string-match
                   "\\`\\(.+\\) => \\(.+\\)\\'" file)
              (setq file (match-string 2 file)))
            (push (cons file stat) entries)))
         ((string-match
           "\\` *[0-9]+ files? changed" line)
          (setq summary (string-trim line)))))
      (forward-line 1))
    (list :entries (nreverse entries) :summary summary)))

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
fetched immediately."
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
  (let ((next (next-button (point))))
    (if next
        (goto-char (button-start next))
      (user-error "inline-review: no next entry"))))

(defun inline-review-overview-previous-entry ()
  "Move point to the previous tree entry (directory or file button)."
  (interactive)
  (let ((prev (previous-button (point))))
    (if prev
        (goto-char (button-start prev))
      (user-error "inline-review: no previous entry"))))

;;;; ─── Mode ──────────────────────────────────────────────────────────────────

(defvar inline-review-overview-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "q") #'quit-window)
    (define-key m (kbd "n") #'inline-review-overview-next-entry)
    (define-key m (kbd "p") #'inline-review-overview-previous-entry)
    (define-key m (kbd "o") #'inline-review-overview-open-file-other-window)
    m)
  "Keymap for `inline-review-overview-mode'.")

(define-derived-mode inline-review-overview-mode special-mode
  "IR-Overview"
  "Major mode for the `*inline-review-overview*' buffer.
Displays a folding tree of files changed in the current MR/PR.
\\<inline-review-overview-mode-map>
\\[inline-review-overview-next-entry] / \\[inline-review-overview-previous-entry]
move between entries; RET toggles a directory or opens a file
(other window by default, current window with a prefix arg); \
\\[inline-review-overview-open-file-other-window]
always opens the file on the current line in another window."
  :group 'inline-review)

;;;; ─── Entry Point ───────────────────────────────────────────────────────────

;;;###autoload
(defun inline-review-overview ()
  "Pop up a folding tree of files changed in the current MR/PR.
Fetches the latest refs from origin each time and diffs against
remote-tracking branches (origin/source vs origin/target) so the stat
reflects the most up-to-date remote state rather than potentially stale
local branches."
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
    (let* ((default-directory root)
           (remote-source
            (if (string-prefix-p "origin/" source)
                source
              (concat "origin/" source)))
           (remote-target
            (if (string-prefix-p "origin/" target)
                target
              (concat "origin/" target))))
      (call-process "git" nil nil nil "fetch" "origin" source target)
      (let ((outbuf (get-buffer-create "*inline-review-overview*")))
        (with-current-buffer outbuf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (let ((rc (call-process "git" nil (list outbuf t) nil
                                    "diff" "--stat"
                                    remote-target remote-source)))
              (if (and (integerp rc) (zerop rc))
                  (let* ((parsed (inline-review--overview-parse-stat))
                         (entries (plist-get parsed :entries))
                         (summary (plist-get parsed :summary)))
                    (setq inline-review--overview-tree
                          (inline-review--overview-build-tree entries)
                          inline-review--overview-folded
                          (make-hash-table :test 'equal)
                          inline-review--overview-summary summary
                          inline-review--overview-root root)
                    (inline-review-overview-mode)
                    (inline-review--overview-refresh))
                (let ((err (string-trim (buffer-string))))
                  (erase-buffer)
                  (insert
                   (format "git diff --stat %s %s failed%s\n"
                           remote-target remote-source
                           (if (string-empty-p err)
                               ""
                             (format ": %s" err))))
                  (goto-char (point-min))
                  (inline-review-overview-mode))))))
        (pop-to-buffer-same-window outbuf)))))

(provide 'inline-review-overview)
;;; inline-review-overview.el ends here
