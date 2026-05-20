;;; code-review-minimal-comment.el --- Comment overlays, input, and commands -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Comment overlay rendering, input overlay, overlay navigation helpers,
;; comment CRUD dispatch, and public comment commands for code-review-minimal.
;;
;; Public API:
;;   Buffer-local state:
;;     `code-review-minimal--overlays'
;;     `code-review-minimal--input-overlay'
;;     `code-review-minimal--input-prompt-end'
;;   Overlay management:
;;     `code-review-minimal--clear-overlays'
;;     `code-review-minimal--insert-discussion-overlay'
;;   Input overlay:
;;     `code-review-minimal--open-input-overlay'
;;     `code-review-minimal--close-input-overlay'
;;     `code-review-minimal--cancel-comment'
;;     `code-review-minimal--submit-comment'
;;     `code-review-minimal-input-mode'
;;   Navigation:
;;     `code-review-minimal--overlay-at-point'
;;     `code-review-minimal--sorted-overlay-positions'
;;   Thread rendering:
;;     `code-review-minimal--render-comment-threads'
;;   Public commands:
;;     `code-review-minimal-add-comment'
;;     `code-review-minimal-edit-comment'
;;     `code-review-minimal-resolve-comment'
;;     `code-review-minimal-reply-comment'
;;     `code-review-minimal-delete-comment'
;;     `code-review-minimal-next-thread'
;;     `code-review-minimal-previous-thread'
;;     `code-review-minimal-toggle-hide-resolved'
;;   Backend dispatch:
;;     `code-review-minimal--post-comment'
;;     `code-review-minimal--update-comment'
;;     `code-review-minimal--resolve-comment'
;;     `code-review-minimal--reply-comment'
;;     `code-review-minimal--delete-comment'
;;
;; Faces are defined in code-review-minimal-custom.el.

;;; Code:

(require 'cl-lib)
(require 'code-review-minimal-custom)
(require 'code-review-minimal-backend)
(require 'code-review-minimal-branch)

;; Forward declarations — authoritative definitions are in sibling files.
(declare-function code-review-minimal--refresh-overlays
                  "code-review-minimal")
(declare-function code-review-minimal--goto-hunk
                  "code-review-minimal-diff")
(declare-function code-review-minimal-mode
                  "code-review-minimal")

;;;; ─── Buffer-local State ─────────────────────────────────────────────────────

(defvar-local code-review-minimal--overlays nil
  "List of comment overlays created by `code-review-minimal-mode'.")

(defvar-local code-review-minimal--input-overlay nil
  "The currently active comment-input overlay, if any.")

(defvar-local code-review-minimal--input-prompt-end nil
  "Marker pointing to the end of the prompt in the input buffer.")

;;;; ─── Overlay Management ─────────────────────────────────────────────────────

(defun code-review-minimal--clear-overlays ()
  "Remove all comment overlays."
  (mapc #'delete-overlay code-review-minimal--overlays)
  (setq code-review-minimal--overlays nil))

;;;; ─── Overlay Rendering ─────────────────────────────────────────────────────

(defun code-review-minimal--render-note
    (note &optional is-first resolved outdated)
  "Render NOTE alist into propertized string."
  (let* ((author-obj (alist-get 'author note))
         (author
          (if author-obj
              (or (alist-get 'name author-obj)
                  (alist-get 'username author-obj)
                  "unknown")
            "unknown"))
         (created-at (alist-get 'created_at note))
         (body (or (alist-get 'body note) ""))
         (is-resolved (eq resolved t))
         (status-str
          (cond
           ((not is-first)
            "")
           (outdated
            (propertize " ⚠outdated"
                        'face
                        'code-review-minimal-outdated-face))
           (is-resolved
            (propertize " ✓resolved"
                        'face
                        'code-review-minimal-resolved-face))
           ((eq resolved :json-false)
            (propertize " ○open"
                        'face
                        'code-review-minimal-unresolved-face))
           (t
            "")))
         (header
          (concat
           (propertize (format "  💬 %s%s"
                               author
                               (if created-at
                                   (format "  [%s]" created-at)
                                 ""))
                       'face 'code-review-minimal-header-face)
           status-str))
         (body-face
          (if is-resolved
              'code-review-minimal-resolved-body-face
            'code-review-minimal-comment-face))
         (body-lines
          (mapconcat
           (lambda (l) (concat "  │ " l)) (split-string body "\n")
           "\n")))
    (concat
     header "\n" (propertize body-lines 'face body-face) "\n")))

(defun code-review-minimal--insert-discussion-overlay
    (line notes resolved first-note-id &optional outdated)
  "Insert a comment-thread overlay anchored after LINE.
LINE is the 1-based line number in the current buffer at which to anchor
the overlay.  NOTES is the list of note alists belonging to this thread.
RESOLVED is the resolved state of the thread (t, `:json-false', or nil).
FIRST-NOTE-ID is the numeric ID of the thread's root note, stored on the
overlay so that replies and edits can target the correct thread.
OUTDATED is non-nil when the comment's original line no longer exists
in the current diff."
  (unless line
    (cl-return-from code-review-minimal--insert-discussion-overlay))
  (let* ((pos (code-review-minimal--line-end-pos line))
         (ov (make-overlay pos pos nil t nil))
         (first-body (alist-get 'body (car notes)))
         (separator
          (propertize "  ├────────────────\n"
                      'face
                      'code-review-minimal-header-face))
         (text
          (propertize (concat
                       "\n"
                       (mapconcat
                        (lambda (note-and-idx)
                          (code-review-minimal--render-note
                           (car note-and-idx)
                           (= (cdr note-and-idx) 0) resolved outdated))
                        (cl-loop
                         for
                         n
                         in
                         notes
                         for
                         i
                         from
                         0
                         collect
                         (cons n i))
                        separator))
                      'cursor 0)))
    (overlay-put ov 'after-string text)
    (overlay-put ov 'code-review-minimal t)
    (overlay-put ov 'code-review-minimal-note-id first-note-id)
    (overlay-put ov 'code-review-minimal-body first-body)
    (overlay-put ov 'code-review-minimal-resolved resolved)
    (overlay-put ov 'priority 10)
    (push ov code-review-minimal--overlays)))

;;;; ─── Input Overlay ─────────────────────────────────────────────────────────

(defvar code-review-minimal--input-map
  (let ((m (make-sparse-keymap)))
    (define-key
     m (kbd "C-c C-c") #'code-review-minimal--submit-comment)
    (define-key
     m (kbd "C-c C-k") #'code-review-minimal--cancel-comment)
    m)
  "Keymap for comment input.")

(defun code-review-minimal--open-input-overlay
    (beg end &optional edit-note-id initial-body reply-note-id)
  "Open inline input overlay below region BEG..END.
If EDIT-NOTE-ID is non-nil, edit existing note with INITIAL-BODY.
If REPLY-NOTE-ID is non-nil, the submission will post a reply to that thread."
  (when code-review-minimal--input-overlay
    (code-review-minimal--close-input-overlay))
  (let* ((end-pos
          (save-excursion
            (goto-char end)
            (line-end-position)))
         (ov (make-overlay end-pos end-pos nil t nil))
         (ibuf (generate-new-buffer "*code-review-minimal-input*"))
         (editing edit-note-id)
         (replying reply-note-id)
         (prompt
          (propertize (concat
                       (cond
                        (editing
                         "\n  ┌─ Edit CR comment ")
                        (replying
                         "\n  ┌─ Reply to CR comment ")
                        (t
                         "\n  ┌─ New CR comment "))
                       (propertize "(C-c C-c submit, C-c C-k cancel)"
                                   'face
                                   '(:weight normal :slant italic))
                       "\n  │ ")
                      'face
                      'code-review-minimal-input-face
                      'read-only
                      t
                      'rear-nonsticky
                      t)))
    (overlay-put ov 'code-review-minimal-input t)
    (overlay-put ov 'code-review-minimal-region-beg beg)
    (overlay-put ov 'code-review-minimal-region-end end)
    (overlay-put ov 'code-review-minimal-input-buffer ibuf)
    (when editing
      (overlay-put ov 'code-review-minimal-edit-note-id edit-note-id))
    (when replying
      (overlay-put
       ov 'code-review-minimal-reply-note-id reply-note-id))
    (setq code-review-minimal--input-overlay ov)
    (with-current-buffer ibuf
      (code-review-minimal-input-mode)
      (insert prompt)
      (setq-local code-review-minimal--input-overlay ov)
      (setq-local code-review-minimal--input-prompt-end
                  (point-marker))
      (when (and editing initial-body)
        (insert initial-body)))
    (let ((win
           (display-buffer ibuf
                           '(display-buffer-below-selected
                             (window-height . 6)))))
      (when win
        (select-window win)))
    (message
     "Type your comment, then C-c C-c to submit or C-c C-k to cancel.")))

(define-derived-mode
 code-review-minimal-input-mode
 text-mode
 "CR-Input"
 "Transient mode for entering a code review comment."
 (set-buffer-file-coding-system 'utf-8)
 (use-local-map code-review-minimal--input-map)
 (when (fboundp 'evil-emacs-state)
   (evil-emacs-state)))

(defun code-review-minimal--get-input-text ()
  "Extract user text from input buffer."
  (when code-review-minimal--input-overlay
    (let ((ibuf
           (overlay-get
            code-review-minimal--input-overlay
            'code-review-minimal-input-buffer)))
      (when (buffer-live-p ibuf)
        (with-current-buffer ibuf
          (string-trim
           (buffer-substring-no-properties
            code-review-minimal--input-prompt-end (point-max))))))))

(defun code-review-minimal--close-input-overlay ()
  "Close input overlay and clean up."
  (when code-review-minimal--input-overlay
    (let* ((ov code-review-minimal--input-overlay)
           (ibuf (overlay-get ov 'code-review-minimal-input-buffer))
           (src-buf (overlay-buffer ov)))
      (delete-overlay ov)
      (setq code-review-minimal--input-overlay nil)
      (when (and src-buf (buffer-live-p src-buf))
        (with-current-buffer src-buf
          (setq code-review-minimal--input-overlay nil)))
      (when (buffer-live-p ibuf)
        (let ((win (get-buffer-window ibuf)))
          (when win
            (delete-window win)))
        (kill-buffer ibuf)))))

(defun code-review-minimal--cancel-comment ()
  "Cancel comment input."
  (interactive)
  (code-review-minimal--close-input-overlay)
  (message "code-review-minimal: comment cancelled."))

(defun code-review-minimal--submit-comment ()
  "Submit comment to API."
  (interactive)
  (let ((body (code-review-minimal--get-input-text)))
    (if (or (null body) (string-empty-p body))
        (message
         "code-review-minimal: empty comment, not submitting.")
      (let* ((ov code-review-minimal--input-overlay)
             (src-buf (overlay-buffer ov))
             (beg (overlay-get ov 'code-review-minimal-region-beg))
             (end (overlay-get ov 'code-review-minimal-region-end))
             (edit-note-id
              (overlay-get ov 'code-review-minimal-edit-note-id)))
        (with-current-buffer src-buf
          (let ((reply-note-id
                 (overlay-get ov 'code-review-minimal-reply-note-id)))
            (cond
             (edit-note-id
              (code-review-minimal--update-comment edit-note-id body))
             (reply-note-id
              (code-review-minimal--reply-comment reply-note-id body))
             (t
              (code-review-minimal--post-comment beg end body))))
          (deactivate-mark)))))
  (code-review-minimal--close-input-overlay))

;;;; ─── Navigation Helpers ─────────────────────────────────────────────────────

(defun code-review-minimal--overlay-at-point ()
  "Return comment overlay at point."
  (let ((found nil))
    (dolist (ov
             (overlays-in
              (line-beginning-position) (1+ (line-end-position))))
      (when (and (overlay-get ov 'code-review-minimal)
                 (overlay-get ov 'code-review-minimal-note-id))
        (setq found ov)))
    found))

(defun code-review-minimal--sorted-overlay-positions ()
  "Return list of overlay start positions sorted ascending."
  (sort (mapcar #'overlay-start code-review-minimal--overlays) #'<))

;;;; ─── Thread Rendering ───────────────────────────────────────────────────────

(defun code-review-minimal--render-comment-threads
    (buf rel-path threads)
  "Render comment overlay threads in BUF for REL-PATH from THREADS list."
  (with-current-buffer buf
    (code-review-minimal--clear-overlays)
    (if (null threads)
        (message "code-review-minimal: no comments found")
      (let ((count 0))
        (dolist (th threads)
          (when (and rel-path
                     (string= (plist-get th :path) rel-path)
                     (plist-get th :line)
                     (not
                      (and code-review-minimal-hide-resolved
                           (eq (plist-get th :resolved) t))))
            (code-review-minimal--insert-discussion-overlay
             (plist-get th :line)
             (plist-get th :thread)
             (plist-get th :resolved)
             (plist-get th :note-id)
             (plist-get th :outdated))
            (cl-incf count)))
        (message
         "code-review-minimal: %d thread(s) in this file, %d total."
         count (length threads))))))

;;;; ─── Thread Navigation Helpers ──────────────────────────────────────────────

(defun code-review-minimal--all-thread-positions ()
  "Return a sorted list of (ABS-PATH . LINE) for every comment thread overlay
in the current project (git root).
Scans all live buffers with `code-review-minimal-mode' active whose
`buffer-file-name' is under the current git root, so thread navigation
never crosses project boundaries.
Returns nil when no comment overlays are found."
  (let ((result nil)
        (root (code-review-minimal--git-root)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p code-review-minimal-mode)
                   code-review-minimal--overlays
                   buffer-file-name
                   ;; Only include buffers that belong to the current project.
                   (or (null root)
                       (string-prefix-p root
                                        (expand-file-name buffer-file-name))))
          (dolist (ov code-review-minimal--overlays)
            (when (and (overlay-buffer ov)
                       (overlay-get ov 'code-review-minimal))
              (let* ((pos (overlay-start ov))
                     (line (line-number-at-pos pos))
                     (abs (expand-file-name buffer-file-name)))
                (push (cons abs line) result)))))))
    ;; Deduplicate and sort.
    (delete-dups
     (sort result
           (lambda (a b)
             (or (string< (car a) (car b))
                 (and (string= (car a) (car b))
                      (< (cdr a) (cdr b)))))))))

(defun code-review-minimal--current-thread-key ()
  "Return a (ABS-PATH . LINE) key representing the current position.
LINE is the current line number; ABS-PATH is the current buffer's absolute path."
  (cons (expand-file-name (or buffer-file-name default-directory))
        (line-number-at-pos)))

;;;; ─── Position Helpers ───────────────────────────────────────────────────────

(defun code-review-minimal--line-end-pos (line)
  "Return buffer position at end of LINE (1-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (line-end-position)))

;;;; ─── Public Commands ────────────────────────────────────────────────────────

;;;###autoload
(defun code-review-minimal-add-comment (beg end)
  "Add a code review comment for selected region BEG..END."
  (interactive "r")
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error
     "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (unless code-review-minimal--mr-iid
    (user-error "code-review-minimal: no MR IID set"))
  (code-review-minimal--assert-token
   code-review-minimal--current-backend)
  (code-review-minimal--open-input-overlay beg end))

;;;###autoload
(defun code-review-minimal-edit-comment ()
  "Edit the code review comment at point."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error
     "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (let ((ov (code-review-minimal--overlay-at-point)))
    (unless ov
      (user-error
       "code-review-minimal: no comment overlay on this line"))
    (let ((note-id (overlay-get ov 'code-review-minimal-note-id))
          (note-body (overlay-get ov 'code-review-minimal-body))
          (line (line-beginning-position)))
      (code-review-minimal--open-input-overlay
       line line note-id note-body))))

;;;###autoload
(defun code-review-minimal-resolve-comment ()
  "Mark the comment at point as resolved."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error
     "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (let ((ov (code-review-minimal--overlay-at-point)))
    (unless ov
      (user-error
       "code-review-minimal: no comment overlay on this line"))
    (let ((already (overlay-get ov 'code-review-minimal-resolved)))
      (when (eq already t)
        (user-error
         "code-review-minimal: comment is already resolved"))
      (code-review-minimal--assert-token
       code-review-minimal--current-backend)
      (code-review-minimal--resolve-comment ov))))

;;;###autoload
(defun code-review-minimal-reply-comment ()
  "Reply to the code review comment thread at point."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error
     "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (let ((ov (code-review-minimal--overlay-at-point)))
    (unless ov
      (user-error
       "code-review-minimal: no comment overlay on this line"))
    (code-review-minimal--assert-token
     code-review-minimal--current-backend)
    (let ((note-id (overlay-get ov 'code-review-minimal-note-id))
          (line (line-beginning-position)))
      (code-review-minimal--open-input-overlay
       line line nil nil note-id))))

;;;###autoload
(defun code-review-minimal-delete-comment ()
  "Delete the code review comment at point."
  (interactive)
  (unless (bound-and-true-p code-review-minimal-mode)
    (user-error
     "code-review-minimal: please enable `code-review-minimal-mode' first"))
  (let ((ov (code-review-minimal--overlay-at-point)))
    (unless ov
      (user-error
       "code-review-minimal: no comment overlay on this line"))
    (code-review-minimal--assert-token
     code-review-minimal--current-backend)
    (let ((note-id (overlay-get ov 'code-review-minimal-note-id)))
      (when (yes-or-no-p (format "Delete comment %s? " note-id))
        (code-review-minimal--delete-comment note-id)))))

;;;###autoload
(defun code-review-minimal-toggle-hide-resolved ()
  "Toggle hiding of resolved comment threads and refresh overlays."
  (interactive)
  (setq code-review-minimal-hide-resolved (not code-review-minimal-hide-resolved))
  (message "code-review-minimal: %s resolved threads"
           (if code-review-minimal-hide-resolved "hiding" "showing"))
  (when (and code-review-minimal-mode code-review-minimal--mr-iid)
    (code-review-minimal--refresh-overlays)))

;;;###autoload
(defun code-review-minimal-next-thread ()
  "Move point to the next comment thread within the current project.
Stops at the last thread with a message rather than wrapping to the first."
  (interactive)
  (unless (code-review-minimal--review-in-progress-p)
    (user-error
     "code-review-minimal: no active review for this repository — run `code-review-minimal-review-url' first"))
  (let* ((all (code-review-minimal--all-thread-positions))
         (cur (code-review-minimal--current-thread-key))
         (next (cl-find-if
                (lambda (entry)
                  (or (string< (car cur) (car entry))
                      (and (string= (car cur) (car entry))
                           (< (cdr cur) (cdr entry)))))
                all)))
    (if next
        (code-review-minimal--goto-hunk (car next) (cdr next))
      (message "code-review-minimal: no more comment threads in this project"))))

;;;###autoload
(defun code-review-minimal-previous-thread ()
  "Move point to the previous comment thread within the current project.
Stops at the first thread with a message rather than wrapping to the last."
  (interactive)
  (unless (code-review-minimal--review-in-progress-p)
    (user-error
     "code-review-minimal: no active review for this repository — run `code-review-minimal-review-url' first"))
  (let* ((all (code-review-minimal--all-thread-positions))
         (cur (code-review-minimal--current-thread-key))
         (prev (cl-find-if
                (lambda (entry)
                  (or (string< (car entry) (car cur))
                      (and (string= (car entry) (car cur))
                           (< (cdr entry) (cdr cur)))))
                (reverse all))))
    (if prev
        (code-review-minimal--goto-hunk (car prev) (cdr prev))
      (message "code-review-minimal: no more comment threads in this project"))))

;;;; ─── Backend Dispatch ───────────────────────────────────────────────────────

(defun code-review-minimal--post-comment (beg end body)
  "Post a new comment via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (code-review-minimal--backend-prop
              code-review-minimal--current-backend
              :post)
             beg end body
             (lambda ()
               (with-current-buffer buf
                 (code-review-minimal--refresh-overlays))))))

(defun code-review-minimal--update-comment (note-id body)
  "Update an existing comment via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (code-review-minimal--backend-prop
              code-review-minimal--current-backend
              :update)
             note-id body
             (lambda ()
               (with-current-buffer buf
                 (code-review-minimal--refresh-overlays))))))

(defun code-review-minimal--resolve-comment (ov)
  "Resolve a comment via the current backend, then refresh overlays."
  (let ((buf (current-buffer))
        (note-id (overlay-get ov 'code-review-minimal-note-id))
        (note-body (overlay-get ov 'code-review-minimal-body)))
    (funcall (code-review-minimal--backend-prop
              code-review-minimal--current-backend
              :resolve)
             note-id note-body
             (lambda ()
               (with-current-buffer buf
                 (code-review-minimal--refresh-overlays))))))

(defun code-review-minimal--reply-comment (note-id body)
  "Post a reply to the thread rooted at NOTE-ID via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (code-review-minimal--backend-prop
              code-review-minimal--current-backend
              :reply)
             note-id body
             (lambda ()
               (with-current-buffer buf
                 (code-review-minimal--refresh-overlays))))))

(defun code-review-minimal--delete-comment (note-id)
  "Delete the comment NOTE-ID via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (code-review-minimal--backend-prop
              code-review-minimal--current-backend
              :delete)
             note-id
             (lambda ()
               (with-current-buffer buf
                 (code-review-minimal--refresh-overlays))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'code-review-minimal-comment)

;;; code-review-minimal-comment.el ends here
