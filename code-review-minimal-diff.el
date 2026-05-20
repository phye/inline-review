;;; code-review-minimal-diff.el --- Diff hunk parsing and overlay rendering -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Unified diff patch parsing and hunk highlight overlay rendering for
;; code-review-minimal.
;;
;; Public API:
;;   `code-review-minimal--clear-hunk-overlays'  — remove all hunk overlays
;;   `code-review-minimal--find-patch-for-file'  — look up patch in change list
;;   `code-review-minimal--insert-hunk-overlays' — parse patch and render overlays
;;   `code-review-minimal-view-removed-lines'     — popup with full removed block
;;   `code-review-minimal-next-hunk'              — go to next diff hunk
;;   `code-review-minimal-previous-hunk'          — go to previous diff hunk
;;
;; Faces are defined in code-review-minimal-custom.el:
;;   `code-review-minimal-hunk-added-face'
;;   `code-review-minimal-hunk-removed-face'
;;   `code-review-minimal-hunk-region-face'

;;; Code:

(require 'cl-lib)
(require 'code-review-minimal-custom)
(require 'code-review-minimal-backend)
(require 'code-review-minimal-branch)
(require 'code-review-minimal-branch)

;; Forward declaration — authoritative definition is in code-review-minimal.el.
(defvar code-review-minimal-mode)

(defvar-local code-review-minimal--hunk-overlays nil
  "List of hunk highlight overlays managed by `code-review-minimal-mode'.")

;;;; ─── Hunk Overlay Cleanup ───────────────────────────────────────────────────

(defun code-review-minimal--clear-hunk-overlays ()
  "Remove all hunk highlight overlays."
  (mapc #'delete-overlay code-review-minimal--hunk-overlays)
  (setq code-review-minimal--hunk-overlays nil))

;;;; ─── Diff File Lookup ───────────────────────────────────────────────────────

(defun code-review-minimal--find-patch-for-file (changes rel-path)
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

(defun code-review-minimal--format-removed (lines)
  "Format removed LINES as a display string.
LINES is a list in push-order (most-recent first); the result is
returned in original source order."
  (mapconcat #'identity (nreverse lines) "\n"))

(defun code-review-minimal--truncate-removed-lines (lines)
  "Return LINES truncated to `code-review-minimal-inline-removed-lines-limit'.
When truncated, a footer indicator is appended; pressing `C-c C-d' on it
opens a popup with the full removed block."
  (let ((max-lines code-review-minimal-inline-removed-lines-limit))
    (if (> (length lines) max-lines)
        (append
         (cl-subseq lines 0 max-lines)
         (list
          (propertize
           (format "── … %d more lines … ──"
                   (- (length lines) max-lines))
           'help-echo "Press C-c C-d to view full removed lines")))
      lines)))

(defun code-review-minimal--parse-patch (patch)
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
                           (code-review-minimal--format-removed
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
                           (code-review-minimal--format-removed
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
                     (code-review-minimal--format-removed
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

(defun code-review-minimal--insert-hunk-overlays (patch)
  "Parse PATCH and insert hunk highlight overlays into the current buffer."
  (let ((hunks (code-review-minimal--parse-patch patch))
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
             ov 'face 'code-review-minimal-hunk-region-face)
            (overlay-put ov 'code-review-minimal-hunk t)
            (overlay-put ov 'evaporate t)
            (overlay-put ov 'priority -10)
            (push ov code-review-minimal--hunk-overlays)))
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
                           'code-review-minimal-hunk-added-face))
              (overlay-put
               ov 'face 'code-review-minimal-hunk-added-face)
              (overlay-put ov 'code-review-minimal-hunk t)
              (overlay-put ov 'evaporate t)
              (overlay-put ov 'priority -10)
              (push ov code-review-minimal--hunk-overlays))))
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
                        (code-review-minimal--truncate-removed-lines
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
                             'code-review-minimal-hunk-removed-face)
                            l))
                         lines
                         "\n")))
                  (overlay-put
                   ov 'before-string
                   (propertize
                    (concat marked-text "\n")
                    'face 'code-review-minimal-hunk-removed-face))
                  (overlay-put ov 'code-review-minimal-hunk t)
                  (overlay-put
                   ov 'code-review-minimal-removed-text text)
                  (overlay-put
                   ov 'code-review-minimal-removed-anchor anchor)
                  (overlay-put ov 'priority -10)
                  (push ov code-review-minimal--hunk-overlays))))
             ;; After anchor line
             ((and (>= anchor 1) (<= anchor buf-lines))
              (let* ((pos
                      (save-excursion
                        (goto-char (point-min))
                        (forward-line (1- anchor))
                        (line-end-position)))
                     (ov (make-overlay pos pos nil t nil))
                     (lines
                      (code-review-minimal--truncate-removed-lines
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
                           'code-review-minimal-hunk-removed-face)
                          l))
                       lines
                       "\n")))
                (overlay-put
                 ov 'after-string
                 (propertize
                  (concat "\n" marked-text)
                  'face 'code-review-minimal-hunk-removed-face))
                (overlay-put ov 'code-review-minimal-hunk t)
                (overlay-put
                 ov 'code-review-minimal-removed-text text)
                (overlay-put
                 ov 'code-review-minimal-removed-anchor anchor)
                (overlay-put ov 'priority -10)
                (push ov code-review-minimal--hunk-overlays))))))))))

;;;; ─── View Removed Lines ─────────────────────────────────────────────────────

(defun code-review-minimal--removed-overlay-at-point ()
  "Find the removed-line overlay whose anchor is closest to the current line."
  (let ((best nil)
        (best-dist nil))
    (dolist (ov code-review-minimal--hunk-overlays)
      (when (overlay-get ov 'code-review-minimal-removed-text)
        (let ((anchor
               (overlay-get ov 'code-review-minimal-removed-anchor)))
          (when anchor
            (let ((dist
                   (abs (- (line-number-at-pos (point)) anchor))))
              (when (or (null best-dist) (< dist best-dist))
                (setq best ov)
                (setq best-dist dist)))))))
    best))

(defun code-review-minimal-view-removed-lines ()
  "Pop up a buffer with the full removed lines for the deleted block near point."
  (interactive)
  (let ((ov (code-review-minimal--removed-overlay-at-point)))
    (if (not ov)
        (message "No deleted block near point")
      (let ((full-text
             (overlay-get ov 'code-review-minimal-removed-text))
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

(defun code-review-minimal--all-hunk-positions ()
  "Return a sorted list of (ABS-PATH . LINE) for every hunk in the current MR diff.
ABS-PATH is the absolute path to the new-side file; LINE is the hunk's new-start line.
Returns nil when no diff data is cached yet."
  (let*
      ((backend code-review-minimal--current-backend)
       (iid code-review-minimal--mr-iid)
       (proj code-review-minimal--project-info)
       (root (code-review-minimal--git-root))
       ;; Primary: look up by exact cache key
       (changes
        (when (and backend iid proj)
          (gethash
           (code-review-minimal--diff-cache-key
            backend iid proj)
           code-review-minimal--diff-cache)))
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
               code-review-minimal--diff-cache)
              found))))
       (changes (or changes changes-fallback))
       (result nil))
    (if (not (and changes root))
        (progn
          (when (called-interactively-p 'any)
            (message
             "code-review-minimal: diff not yet cached — run `code-review-minimal-review-url' first"))
          nil)
      (dolist (c changes)
        (let* ((rel
                (or (plist-get c :new-path) (plist-get c :old-path)))
               (abs (expand-file-name rel root))
               (patch (plist-get c :patch)))
          (when (and rel patch)
            (dolist (hunk (code-review-minimal--parse-patch patch))
              (push (cons abs (plist-get hunk :new-start)) result)))))
      (sort result
            (lambda (a b)
              (or (string< (car a) (car b))
                  (and (string= (car a) (car b))
                       (< (cdr a) (cdr b)))))))))

(defun code-review-minimal--current-hunk-key ()
  "Return a (ABS-PATH . LINE) key representing the current position.
LINE is the current line number; ABS-PATH is the current buffer's absolute path."
  (cons (or buffer-file-name default-directory) (line-number-at-pos)))

(defun code-review-minimal--goto-hunk (abs-path line)
  "Visit ABS-PATH (opening it if needed) and move point to LINE.
Ensures `code-review-minimal-mode' is active in the target buffer.
MR state (backend, iid, project-info) is propagated from the calling buffer
into the target buffer so that hunk navigation continues to work there."
  ;; Capture MR state from the calling buffer before any buffer switch.
  (let ((src-backend code-review-minimal--current-backend)
        (src-iid code-review-minimal--mr-iid)
        (src-mr-id code-review-minimal--mr-id)
        (src-proj code-review-minimal--project-info))
    (unless (and buffer-file-name
                 (string=
                  (expand-file-name buffer-file-name) abs-path))
      (find-file abs-path))
    ;; Propagate MR state into the new buffer if it is not already set.
    (when (and src-backend (not code-review-minimal--current-backend))
      (setq code-review-minimal--current-backend src-backend))
    (when (and src-iid (not code-review-minimal--mr-iid))
      (setq code-review-minimal--mr-iid src-iid))
    (when (and src-mr-id (not code-review-minimal--mr-id))
      (setq code-review-minimal--mr-id src-mr-id))
    (when (and src-proj (not code-review-minimal--project-info))
      (setq code-review-minimal--project-info src-proj))
    (unless (bound-and-true-p code-review-minimal-mode)
      (code-review-minimal-mode 1))
    (goto-char (point-min))
    (forward-line (1- line))))

;;;###autoload
(defun code-review-minimal-next-hunk ()
  "Move point to the next diff hunk, opening other files in the MR if needed.
Wraps around to the first hunk after the last one."
  (interactive)
  (unless (code-review-minimal--review-in-progress-p)
    (user-error
     "code-review-minimal: no active review for this repository — run `code-review-minimal-review-url' first"))
  (let* ((all (code-review-minimal--all-hunk-positions))
     code-review-minimal--current-backend
     code-review-minimal--mr-iid
     (hash-table-count code-review-minimal--diff-cache)
     (length all)
     cur)
    (let ((next
           (or (cl-find-if
                (lambda (entry)
                  (or (string< (car cur) (car entry))
                      (and (string= (car cur) (car entry))
                           (< (cdr cur) (cdr entry)))))
                all)
               ;; wrap around to first hunk
               (car all))))
      (if next
          (code-review-minimal--goto-hunk (car next) (cdr next))
        (user-error
         "code-review-minimal: diff not cached yet — run `code-review-minimal-review-url' first")))))

;;;###autoload
(defun code-review-minimal-previous-hunk ()
  "Move point to the previous diff hunk, opening other files in the MR if needed.
Wraps around to the last hunk before the first one."
  (interactive)
  (unless (code-review-minimal--review-in-progress-p)
    (user-error
     "code-review-minimal: no active review for this repository — run `code-review-minimal-review-url' first"))
  (let* ((all (code-review-minimal--all-hunk-positions))
         (cur (code-review-minimal--current-hunk-key))
         (prev
          (or (cl-find-if
               (lambda (entry)
                 (or (string< (car entry) (car cur))
                     (and (string= (car entry) (car cur))
                          (< (cdr entry) (cdr cur)))))
               (reverse all))
              ;; wrap around to last hunk
              (car (last all)))))
    (if prev
        (code-review-minimal--goto-hunk (car prev) (cdr prev))
      (user-error
       "code-review-minimal: diff not cached yet — run `code-review-minimal-review-url' first"))))

;;;; ─── Diff Cache ─────────────────────────────────────────────────────────────

(defun code-review-minimal--diff-cache-key (backend iid project-info)
  "Return a cache key for the diff of BACKEND IID PROJECT-INFO."
  (list backend iid project-info))

(defun code-review-minimal--fetch-diff-then
    (backend buf iid project-info rel-path on-done)
  "Fetch or reuse cached diff for BACKEND IID; render hunk overlays in BUF for REL-PATH.
After hunk overlays are in place, call ON-DONE (a zero-argument function) to
trigger the next rendering step (typically fetching comment threads)."
  (let* ((key
          (code-review-minimal--diff-cache-key
           backend iid project-info))
         (cached (gethash key code-review-minimal--diff-cache)))
    (if cached
        (with-current-buffer buf
          (code-review-minimal--clear-hunk-overlays)
          (let ((patch
                 (code-review-minimal--find-patch-for-file
                  cached rel-path)))
            (if patch
                (code-review-minimal--insert-hunk-overlays patch)
              (message
               "code-review-minimal: file not changed in this MR — \
use `code-review-minimal-next-hunk' to navigate to changed files")))
          (funcall on-done))
      (funcall (code-review-minimal--backend-prop backend :fetch-diff)
               (lambda (changes)
                 (puthash key changes code-review-minimal--diff-cache)
                 (with-current-buffer buf
                   (code-review-minimal--clear-hunk-overlays)
                   (let ((patch
                          (code-review-minimal--find-patch-for-file
                           changes rel-path)))
                     (if patch
                         (code-review-minimal--insert-hunk-overlays
                          patch)
                       (message
                        "code-review-minimal: file not changed in this MR — \
use `code-review-minimal-next-hunk' to navigate to changed files")))
                   (funcall on-done)))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'code-review-minimal-diff)

;;; code-review-minimal-diff.el ends here
