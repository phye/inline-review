;;; inline-review-diff.el --- Diff hunk parsing and overlay rendering -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Unified diff patch parsing and hunk highlight overlay rendering for
;; inline-review.
;;
;; Public API:
;;   `inline-review--clear-hunk-overlays'  — remove all hunk overlays
;;   `inline-review--find-patch-for-file'  — look up patch in change list
;;   `inline-review--insert-hunk-overlays' — parse patch and render overlays
;;   `inline-review-view-removed-lines'     — popup with full removed block
;;   `inline-review-next-hunk'              — go to next diff hunk
;;   `inline-review-previous-hunk'          — go to previous diff hunk
;;   `inline-review-first-hunk'             — go to first diff hunk in project
;;   `inline-review-last-hunk'              — go to last diff hunk in project
;;
;; Faces are defined in inline-review-custom.el:
;;   `inline-review-hunk-added-face'
;;   `inline-review-hunk-removed-face'
;;   `inline-review-hunk-region-face'

;;; Code:

(require 'cl-lib)
(require 'inline-review-custom)
(require 'inline-review-backend)
(require 'inline-review-branch)
(require 'inline-review-branch)

;; Forward declaration — authoritative definition is in inline-review.el.
(defvar inline-review-mode)

(defvar-local inline-review--hunk-overlays nil
  "List of hunk highlight overlays managed by `inline-review-mode'.")

;;;; ─── Hunk Overlay Cleanup ───────────────────────────────────────────────────

(defun inline-review--clear-hunk-overlays ()
  "Remove all hunk highlight overlays."
  (mapc #'delete-overlay inline-review--hunk-overlays)
  (setq inline-review--hunk-overlays nil))

;;;; ─── Diff File Lookup ───────────────────────────────────────────────────────

(defun inline-review--find-patch-for-file (changes rel-path)
  "Find the patch string for REL-PATH in CHANGES list.
Each element of CHANGES is a plist with :old-path, :new-path, and :patch."
  (let ((result
         (cl-loop
          for c in changes when
          (or (string= (plist-get c :new-path) rel-path)
              (string= (plist-get c :old-path) rel-path))
          return (plist-get c :patch))))
    result))

;;;; ─── Diff Parsing ───────────────────────────────────────────────────────────

(defun inline-review--format-removed (lines)
  "Format removed LINES as a display string.
LINES is a list in push-order (most-recent first); the result is
returned in original source order."
  (mapconcat #'identity (nreverse lines) "\n"))

(defun inline-review--truncate-removed-lines (lines)
  "Return LINES truncated to `inline-review-inline-removed-lines-limit'.
When truncated, a footer indicator is appended; pressing `C-c C-d' on it
opens a popup with the full removed block."
  (let ((max-lines inline-review-inline-removed-lines-limit))
    (if (> (length lines) max-lines)
        (append
         (cl-subseq lines 0 max-lines)
         (list
          (propertize
           (format "── … %d more lines … ──"
                   (- (length lines) max-lines))
           'help-echo "Press C-c C-d to view full removed lines")))
      lines)))

(defun inline-review--parse-patch (patch)
  "Parse unified diff PATCH string for a single file.
Return a list of hunk plists:
  (:new-start N :new-count M :added-lines (LINE-NUMS) :removed-segments ((ANCHOR . TEXT) ...))
ANCHOR is the new-file line number after which the removed lines should appear;
0 means before the first line of the hunk."
  (let ((hunks nil)
        (lines (split-string patch "\n")))
    (while lines
      (if (string-match
           "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? [+]\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
           (car lines))
          (let* ((new-start
                  (string-to-number (match-string 3 (car lines))))
                 (new-line new-start)
                 (added-lines nil)
                 (removed-segments nil)
                 (current-removed nil)
                 (last-new-line nil))
            (setq lines (cdr lines))
            (while (and lines
                        (not (string-match "^@@ " (car lines)))
                        (not
                         (string-match "^diff --git" (car lines))))
              (let ((line (car lines)))
                (cond
                 ;; Removed line
                 ((string-prefix-p "-" line)
                  (push (substring line 1) current-removed))
                 ;; Added line
                 ((string-prefix-p "+" line)
                  (when current-removed
                    (push (cons
                           (or last-new-line (1- new-start))
                           (inline-review--format-removed
                            current-removed))
                          removed-segments)
                    (setq current-removed nil))
                  (push new-line added-lines)
                  (setq last-new-line new-line)
                  (cl-incf new-line))
                 ;; Context line or no-newline marker
                 ((or (string-prefix-p " " line)
                      (string-prefix-p "\\" line))
                  (when current-removed
                    (push (cons
                           (or last-new-line (1- new-start))
                           (inline-review--format-removed
                            current-removed))
                          removed-segments)
                    (setq current-removed nil))
                  (setq last-new-line new-line)
                  (cl-incf new-line))
                 ;; Skip anything else (e.g. empty lines in patch)
                 (t
                  nil)))
              (setq lines (cdr lines)))
            ;; Flush remaining removed lines at end of hunk
            (when current-removed
              (push (cons
                     (or last-new-line (1- new-start))
                     (inline-review--format-removed
                      current-removed))
                    removed-segments))
            (push (list
                   :new-start new-start
                   :new-count (- new-line new-start)
                   :added-lines (nreverse added-lines)
                   :removed-segments (nreverse removed-segments))
                  hunks))
        (setq lines (cdr lines))))
    (nreverse hunks)))

;;;; ─── Hunk Overlay Rendering ─────────────────────────────────────────────────

(defun inline-review--insert-hunk-overlays (patch)
  "Parse PATCH and insert hunk highlight overlays into the current buffer."
  (let ((hunks (inline-review--parse-patch patch))
        (buf-lines (line-number-at-pos (point-max))))
    (dolist (hunk hunks)
      (let* ((new-start (plist-get hunk :new-start))
             (new-count (plist-get hunk :new-count))
             (added-lines (plist-get hunk :added-lines))
             (removed-segments (plist-get hunk :removed-segments))
             (end-line (+ new-start new-count -1)))
        ;; Region overlay
        (when (and (>= new-start 1) (<= new-start buf-lines))
          (let* ((beg-pos
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- new-start))
                    (point)))
                 (end-pos
                  (save-excursion
                    (goto-char (point-min))
                    (forward-line (1- (min end-line buf-lines)))
                    (line-end-position)))
                 (ov (make-overlay beg-pos end-pos)))
            (overlay-put
             ov 'face 'inline-review-hunk-region-face)
            (overlay-put ov 'inline-review-hunk t)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'priority -10)
            (push ov inline-review--hunk-overlays)))
        ;; Added line overlays
        (dolist (line added-lines)
          (when (and (>= line 1) (<= line buf-lines))
            (let* ((beg
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (point)))
                   (end
                    (save-excursion
                      (goto-char (point-min))
                      (forward-line (1- line))
                      (line-end-position)))
                   (ov (make-overlay beg end)))
              (overlay-put
               ov 'before-string
               (propertize " "
                           'display
                           '((margin left-margin) "+")
                           'face
                           'inline-review-hunk-added-face))
              (overlay-put
               ov 'face 'inline-review-hunk-added-face)
              (overlay-put ov 'inline-review-hunk t)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority -10)
              (push ov inline-review--hunk-overlays))))
        ;; Removed line overlays (shown inline via before/after-string)
        (dolist (seg removed-segments)
          (let ((anchor (car seg))
                (text (cdr seg)))
            (cond
             ;; Before first line of buffer
             ((= anchor 0)
              (when (>= buf-lines 1)
                (let* ((pos
                        (save-excursion
                          (goto-char (point-min))
                          (point)))
                       (ov (make-overlay pos pos nil t nil))
                       (lines
                        (inline-review--truncate-removed-lines
                         (split-string text "\n")))
                       (marked-text
                        (mapconcat
                         (lambda (l)
                           (concat
                            (propertize
                             " "
                             'display
                             '((margin left-margin) "-")
                             'face
                             'inline-review-hunk-removed-face)
                            l))
                         lines
                         "\n")))
                  (overlay-put
                   ov 'before-string
                   (propertize
                    (concat marked-text "\n")
                    'face 'inline-review-hunk-removed-face))
                  (overlay-put ov 'inline-review-hunk t)
                  (overlay-put
                   ov 'inline-review-removed-text text)
                  (overlay-put
                   ov 'inline-review-removed-anchor anchor)
                  (overlay-put ov 'priority -10)
                  (push ov inline-review--hunk-overlays))))
             ;; After anchor line
             ((and (>= anchor 1) (<= anchor buf-lines))
              (let* ((pos
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- anchor))
                        (line-end-position)))
                     (ov (make-overlay pos pos nil t nil))
                     (lines
                      (inline-review--truncate-removed-lines
                       (split-string text "\n")))
                     (marked-text
                      (mapconcat
                       (lambda (l)
                         (concat
                          (propertize
                           " "
                           'display
                           '((margin left-margin) "-")
                           'face
                           'inline-review-hunk-removed-face)
                          l))
                       lines
                       "\n")))
                (overlay-put
                 ov 'after-string
                 (propertize
                  (concat "\n" marked-text)
                  'face 'inline-review-hunk-removed-face))
                (overlay-put ov 'inline-review-hunk t)
                (overlay-put
                 ov 'inline-review-removed-text text)
                (overlay-put
                 ov 'inline-review-removed-anchor anchor)
                (overlay-put ov 'priority -10)
                (push ov inline-review--hunk-overlays))))))))))

;;;; ─── View Removed Lines ─────────────────────────────────────────────────────

(defun inline-review--removed-overlay-at-point ()
  "Find the removed-line overlay whose anchor is closest to the current line."
  (let ((best nil)
        (best-dist nil))
    (dolist (ov inline-review--hunk-overlays)
      (when (overlay-get ov 'inline-review-removed-text)
        (let ((anchor
               (overlay-get ov 'inline-review-removed-anchor)))
          (when anchor
            (let ((dist
                   (abs (- (line-number-at-pos (point)) anchor))))
              (when (or (null best-dist) (< dist best-dist))
                (setq best ov)
                (setq best-dist dist)))))))
    best))

(defun inline-review-view-removed-lines ()
  "Pop up a buffer with the full removed lines for the deleted block near point."
  (interactive)
  (let ((ov (inline-review--removed-overlay-at-point)))
    (if (not ov)
        (message "No deleted block near point")
      (let ((full-text
             (overlay-get ov 'inline-review-removed-text))
            (src-mode
             (with-current-buffer (overlay-buffer ov)
               major-mode)))
        (with-current-buffer (get-buffer-create
                              "*code-review-removed-lines*")
          (erase-buffer)
          (insert full-text)
          (funcall src-mode)
          (goto-char (point-min))
          (view-mode))
        (pop-to-buffer "*code-review-removed-lines*")))))

;;;; ─── Hunk Navigation Helpers ────────────────────────────────────────────────

(defun inline-review--all-hunk-positions ()
  "Return a sorted list of (ABS-PATH . LINE) for every hunk in the current MR diff.
ABS-PATH is the absolute path to the new-side file; LINE is the hunk's new-start line.
Returns nil when no diff data is cached yet."
  (let*
      ((backend inline-review--current-backend)
       (iid inline-review--mr-iid)
       (proj inline-review--project-info)
       (root (inline-review--git-root))
       ;; Primary: look up by exact cache key
       (changes
        (when (and backend iid proj)
          (gethash
           (inline-review--diff-cache-key
            backend iid proj)
           inline-review--diff-cache)))
       ;; Fallback: scan every cached entry and pick the first whose files
       ;; resolve under the current git root.  This handles the case where
       ;; buffer-local vars are stale or nil (e.g. after navigating to a new
       ;; file that hasn't fully inherited MR state yet).
       (changes-fallback
        (unless changes
          (when root
            (let ((found nil))
              (maphash
               (lambda (_k v)
                 (unless found
                   (let* ((first (car v))
                          (rel
                           (or (plist-get first :new-path)
                               (plist-get first :old-path))))
                     (when (and rel
                                (file-exists-p
                                 (expand-file-name rel root)))
                       (setq found v)))))
               inline-review--diff-cache)
              found))))
       (changes (or changes changes-fallback))
       (result nil))
    (if (not (and changes root))
        (progn
          (when (called-interactively-p 'any)
            (message
             "inline-review: diff not yet cached — run `inline-review-review-url' first"))
          nil)
      (dolist (c changes)
        (let* ((rel
                (or (plist-get c :new-path) (plist-get c :old-path)))
               (abs (expand-file-name rel root))
               (patch (plist-get c :patch)))
          (when (and rel patch (not (string= abs "/dev/null")))
            (dolist (hunk (inline-review--parse-patch patch))
              (push (cons abs (plist-get hunk :new-start)) result)))))
      (sort result
            (lambda (a b)
              (or (string< (car a) (car b))
                  (and (string= (car a) (car b))
                       (< (cdr a) (cdr b)))))))))

(defun inline-review--current-hunk-key ()
  "Return a (ABS-PATH . LINE) key representing the current position.
LINE is the current line number; ABS-PATH is the current buffer's absolute path."
  (cons (or buffer-file-name default-directory) (line-number-at-pos)))

(defun inline-review--goto-hunk (abs-path line)
  "Visit ABS-PATH (opening it if needed) and move point to LINE.
Ensures `inline-review-mode' is active in the target buffer.
MR state (backend, iid, project-info, source/target branch) is propagated
from the calling buffer into the target buffer so that hunk navigation
and overview continue to work there."
  ;; Capture MR state from the calling buffer before any buffer switch.
  (let ((src-backend       inline-review--current-backend)
        (src-iid           inline-review--mr-iid)
        (src-mr-id         inline-review--mr-id)
        (src-proj          inline-review--project-info)
        (src-source-branch inline-review--mr-source-branch)
        (src-target-branch inline-review--mr-target-branch))
    (unless (and buffer-file-name
                 (string=
                  (expand-file-name buffer-file-name) abs-path))
      (find-file abs-path))
    ;; Propagate MR state into the new buffer if it is not already set.
    (when (and src-backend (not inline-review--current-backend))
      (setq inline-review--current-backend src-backend))
    (when (and src-iid (not inline-review--mr-iid))
      (setq inline-review--mr-iid src-iid))
    (when (and src-mr-id (not inline-review--mr-id))
      (setq inline-review--mr-id src-mr-id))
    (when (and src-proj (not inline-review--project-info))
      (setq inline-review--project-info src-proj))
    (when (and src-source-branch (not inline-review--mr-source-branch))
      (setq inline-review--mr-source-branch src-source-branch))
    (when (and src-target-branch (not inline-review--mr-target-branch))
      (setq inline-review--mr-target-branch src-target-branch))
    (unless (bound-and-true-p inline-review-mode)
      (inline-review-mode 1))
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun inline-review-next-hunk ()
  "Move point to the next diff hunk within the current project.
Stops at the last hunk with a message rather than wrapping to the first."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let* ((all (inline-review--all-hunk-positions))
         (cur (inline-review--current-hunk-key))
         (next (cl-find-if
                (lambda (entry)
                  (or (string< (car cur) (car entry))
                      (and (string= (car cur) (car entry))
                           (< (cdr cur) (cdr entry)))))
                all)))
    (if next
        (inline-review--goto-hunk (car next) (cdr next))
      (message "inline-review: no more hunks in this project"))))

;;;###autoload
(defun inline-review-previous-hunk ()
  "Move point to the previous diff hunk within the current project.
Stops at the first hunk with a message rather than wrapping to the last."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let* ((all (inline-review--all-hunk-positions))
         (cur (inline-review--current-hunk-key))
         (prev (cl-find-if
                (lambda (entry)
                  (or (string< (car entry) (car cur))
                      (and (string= (car entry) (car cur))
                           (< (cdr entry) (cdr cur)))))
                (reverse all))))
    (if prev
        (inline-review--goto-hunk (car prev) (cdr prev))
      (message "inline-review: no more hunks in this project"))))

;;;###autoload
(defun inline-review-first-hunk ()
  "Move point to the first diff hunk within the current project."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let ((all (inline-review--all-hunk-positions)))
    (if all
        (let ((first (car all)))
          (inline-review--goto-hunk (car first) (cdr first)))
      (message "inline-review: no hunks in this project"))))

;;;###autoload
(defun inline-review-last-hunk ()
  "Move point to the last diff hunk within the current project."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let ((all (inline-review--all-hunk-positions)))
    (if all
        (let ((last (car (last all))))
          (inline-review--goto-hunk (car last) (cdr last)))
      (message "inline-review: no hunks in this project"))))

;;;; ─── Diff Cache ─────────────────────────────────────────────────────────────

(defun inline-review--diff-cache-key (backend iid project-info)
  "Return a cache key for the diff of BACKEND IID PROJECT-INFO."
  (list backend iid project-info))

(defun inline-review--fetch-diff-then
    (backend buf iid project-info rel-path on-done)
  "Fetch or reuse cached diff for BACKEND IID; render hunk overlays in BUF for REL-PATH.
After hunk overlays are in place, call ON-DONE (a zero-argument function) to
trigger the next rendering step (typically fetching comment threads)."
  (let* ((key
          (inline-review--diff-cache-key
           backend iid project-info))
         (cached (gethash key inline-review--diff-cache)))
    (if cached
        (with-current-buffer buf
          (inline-review--clear-hunk-overlays)
          (let ((patch
                 (inline-review--find-patch-for-file
                  cached rel-path)))
            (if patch
                (inline-review--insert-hunk-overlays patch)
              (message
               "inline-review: file not changed in this MR — \
use `inline-review-next-hunk' to navigate to changed files")))
          (funcall on-done))
      (funcall (inline-review--backend-prop backend :fetch-diff)
               (lambda (changes)
                 (puthash key changes inline-review--diff-cache)
                 (with-current-buffer buf
                   (inline-review--clear-hunk-overlays)
                   (let ((patch
                          (inline-review--find-patch-for-file
                           changes rel-path)))
                     (if patch
                         (inline-review--insert-hunk-overlays
                          patch)
                       (message
                        "inline-review: file not changed in this MR — \
use `inline-review-next-hunk' to navigate to changed files")))
                   (funcall on-done)))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-diff)

;;; inline-review-diff.el ends here
