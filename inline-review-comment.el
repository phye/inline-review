;;; inline-review-comment.el --- Comment overlays, input, and commands -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Comment overlay rendering, input overlay, overlay navigation helpers,
;; comment CRUD dispatch, and public comment commands for inline-review.
;;
;; Public API:
;;   Buffer-local state:
;;     `inline-review--overlays'
;;     `inline-review--input-overlay'
;;     `inline-review--input-prompt-end'
;;   Overlay management:
;;     `inline-review--clear-overlays'
;;     `inline-review--insert-discussion-overlay'
;;   Input overlay:
;;     `inline-review--open-input-overlay'
;;     `inline-review--close-input-overlay'
;;     `inline-review--cancel-comment'
;;     `inline-review--submit-comment'
;;     `inline-review-input-mode'
;;   Navigation:
;;     `inline-review--overlay-at-point'
;;     `inline-review--sorted-overlay-positions'
;;   Thread rendering:
;;     `inline-review--render-comment-threads'
;;   Public commands:
;;     `inline-review-add-comment'
;;     `inline-review-edit-comment'
;;     `inline-review-resolve-comment'
;;     `inline-review-reply-comment'
;;     `inline-review-delete-comment'
;;     `inline-review-next-thread'
;;     `inline-review-previous-thread'
;;     `inline-review-first-thread'
;;     `inline-review-last-thread'
;;     `inline-review-toggle-hide-resolved'
;;   Backend dispatch:
;;     `inline-review--post-comment'
;;     `inline-review--update-comment'
;;     `inline-review--resolve-comment'
;;     `inline-review--reply-comment'
;;     `inline-review--delete-comment'
;;
;; Faces are defined in inline-review-custom.el.

;;; Code:

(require 'cl-lib)
(require 'inline-review-custom)
(require 'inline-review-backend)
(require 'inline-review-branch)

;; Forward declarations — authoritative definitions are in sibling files.
(declare-function inline-review--refresh-overlays
                  "inline-review")
(declare-function inline-review--goto-hunk
                  "inline-review-diff")
(declare-function inline-review-mode
                  "inline-review")

;;;; ─── Buffer-local State ─────────────────────────────────────────────────────

(defvar-local inline-review--overlays nil
  "List of comment overlays created by `inline-review-mode'.")

(defvar-local inline-review--input-overlay nil
  "The currently active comment-input overlay, if any.")

(defvar-local inline-review--input-prompt-end nil
  "Marker pointing to the end of the prompt in the input buffer.")

;;;; ─── Overlay Management ─────────────────────────────────────────────────────

(defun inline-review--clear-overlays ()
  "Remove all comment overlays."
  (mapc #'delete-overlay inline-review--overlays)
  (setq inline-review--overlays nil))

;;;; ─── Overlay Rendering ─────────────────────────────────────────────────────

(defun inline-review--render-note
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
                        'inline-review-outdated-face))
           (is-resolved
            (propertize " ✓resolved"
                        'face
                        'inline-review-resolved-face))
           ((eq resolved :json-false)
            (propertize " ○open"
                        'face
                        'inline-review-unresolved-face))
           (t
            "")))
         (header
          (concat
           (propertize (format "  💬 %s%s"
                               author
                               (if created-at
                                   (format "  [%s]" created-at)
                                 ""))
                       'face 'inline-review-header-face)
           status-str))
         (body-face
          (if is-resolved
              'inline-review-resolved-body-face
            'inline-review-comment-face))
         (body-lines
          (mapconcat
           (lambda (l) (concat "  │ " l)) (split-string body "\n")
           "\n")))
    (concat
     header "\n" (propertize body-lines 'face body-face) "\n")))

(defun inline-review--insert-discussion-overlay
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
    (cl-return-from inline-review--insert-discussion-overlay))
  (let* ((pos (inline-review--line-end-pos line))
         (ov (make-overlay pos pos nil t nil))
         (first-body (alist-get 'body (car notes)))
         (separator
          (propertize "  ├────────────────\n"
                      'face
                      'inline-review-header-face))
         (text
          (propertize (concat
                       "\n"
                       (mapconcat
                        (lambda (note-and-idx)
                          (inline-review--render-note
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
    (overlay-put ov 'inline-review t)
    (overlay-put ov 'inline-review-note-id first-note-id)
    (overlay-put ov 'inline-review-body first-body)
    (overlay-put ov 'inline-review-resolved resolved)
    (overlay-put ov 'priority 10)
    (push ov inline-review--overlays)))

;;;; ─── Input Overlay ─────────────────────────────────────────────────────────

(defvar inline-review--input-map
  (let ((m (make-sparse-keymap)))
    (define-key
     m (kbd "C-c C-c") #'inline-review--submit-comment)
    (define-key
     m (kbd "C-c C-k") #'inline-review--cancel-comment)
    m)
  "Keymap for comment input.")

(defun inline-review--open-input-overlay
    (beg end &optional edit-note-id initial-body reply-note-id)
  "Open inline input overlay below region BEG..END.
If EDIT-NOTE-ID is non-nil, edit existing note with INITIAL-BODY.
If REPLY-NOTE-ID is non-nil, the submission will post a reply to that thread."
  (when inline-review--input-overlay
    (inline-review--close-input-overlay))
  (let* ((end-pos
          (save-excursion
            (goto-char end)
            (line-end-position)))
         (ov (make-overlay end-pos end-pos nil t nil))
         (ibuf (generate-new-buffer "*inline-review-input*"))
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
                      'inline-review-input-face
                      'read-only
                      t
                      'rear-nonsticky
                      t)))
    (overlay-put ov 'inline-review-input t)
    (overlay-put ov 'inline-review-region-beg beg)
    (overlay-put ov 'inline-review-region-end end)
    (overlay-put ov 'inline-review-input-buffer ibuf)
    (when editing
      (overlay-put ov 'inline-review-edit-note-id edit-note-id))
    (when replying
      (overlay-put
       ov 'inline-review-reply-note-id reply-note-id))
    (setq inline-review--input-overlay ov)
    (with-current-buffer ibuf
      (inline-review-input-mode)
      (insert prompt)
      (setq-local inline-review--input-overlay ov)
      (setq-local inline-review--input-prompt-end
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
 inline-review-input-mode
 text-mode
 "IR-Input"
 "Transient mode for entering a code review comment."
 (set-buffer-file-coding-system 'utf-8)
 (use-local-map inline-review--input-map)
 (when (fboundp 'evil-emacs-state)
   (evil-emacs-state)))

(defun inline-review--get-input-text ()
  "Extract user text from input buffer."
  (when inline-review--input-overlay
    (let ((ibuf
           (overlay-get
            inline-review--input-overlay
            'inline-review-input-buffer)))
      (when (buffer-live-p ibuf)
        (with-current-buffer ibuf
          (string-trim
           (buffer-substring-no-properties
            inline-review--input-prompt-end (point-max))))))))

(defun inline-review--close-input-overlay ()
  "Close input overlay and clean up."
  (when inline-review--input-overlay
    (let* ((ov inline-review--input-overlay)
           (ibuf (overlay-get ov 'inline-review-input-buffer))
           (src-buf (overlay-buffer ov)))
      (delete-overlay ov)
      (setq inline-review--input-overlay nil)
      (when (and src-buf (buffer-live-p src-buf))
        (with-current-buffer src-buf
          (setq inline-review--input-overlay nil)))
      (when (buffer-live-p ibuf)
        (let ((win (get-buffer-window ibuf)))
          (when win
            (delete-window win)))
        (kill-buffer ibuf)))))

(defun inline-review--cancel-comment ()
  "Cancel comment input."
  (interactive)
  (inline-review--close-input-overlay)
  (message "inline-review: comment cancelled."))

(defun inline-review--submit-comment ()
  "Submit comment to API."
  (interactive)
  (let ((body (inline-review--get-input-text)))
    (if (or (null body) (string-empty-p body))
        (message
         "inline-review: empty comment, not submitting.")
      (let* ((ov inline-review--input-overlay)
             (src-buf (overlay-buffer ov))
             (beg (overlay-get ov 'inline-review-region-beg))
             (end (overlay-get ov 'inline-review-region-end))
             (edit-note-id
              (overlay-get ov 'inline-review-edit-note-id)))
        (with-current-buffer src-buf
          (let ((reply-note-id
                 (overlay-get ov 'inline-review-reply-note-id)))
            (cond
             (edit-note-id
              (inline-review--update-comment edit-note-id body))
             (reply-note-id
              (inline-review--reply-comment reply-note-id body))
             (t
              (inline-review--post-comment beg end body))))
          (deactivate-mark)))))
  (inline-review--close-input-overlay))

;;;; ─── Navigation Helpers ─────────────────────────────────────────────────────

(defun inline-review--overlay-at-point ()
  "Return comment overlay at point."
  (let ((found nil))
    (dolist (ov
             (overlays-in
              (line-beginning-position) (1+ (line-end-position))))
      (when (and (overlay-get ov 'inline-review)
                 (overlay-get ov 'inline-review-note-id))
        (setq found ov)))
    found))

(defun inline-review--sorted-overlay-positions ()
  "Return list of overlay start positions sorted ascending."
  (sort (mapcar #'overlay-start inline-review--overlays) #'<))

;;;; ─── Thread Rendering ───────────────────────────────────────────────────────

(defun inline-review--render-comment-threads
    (buf rel-path threads)
  "Render comment overlay threads in BUF for REL-PATH from THREADS list."
  (with-current-buffer buf
    (inline-review--clear-overlays)
    (if (null threads)
        (message "inline-review: no comments found")
      (let ((count 0))
        (dolist (th threads)
          (when (and rel-path
                     (string= (plist-get th :path) rel-path)
                     (plist-get th :line)
                     (not
                      (and inline-review-hide-resolved
                           (eq (plist-get th :resolved) t))))
            (inline-review--insert-discussion-overlay
             (plist-get th :line)
             (plist-get th :thread)
             (plist-get th :resolved)
             (plist-get th :note-id)
             (plist-get th :outdated))
            (cl-incf count)))
        (message
         "inline-review: %d thread(s) in this file, %d total."
         count (length threads))))))

;;;; ─── Thread Navigation Helpers ──────────────────────────────────────────────

(defun inline-review--all-thread-positions ()
  "Return a sorted list of (ABS-PATH . LINE) for every comment thread overlay
in the current project (git root).
Scans all live buffers with `inline-review-mode' active whose
`buffer-file-name' is under the current git root, so thread navigation
never crosses project boundaries.
Returns nil when no comment overlays are found."
  (let ((result nil)
        (root (inline-review--git-root)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (bound-and-true-p inline-review-mode)
                   inline-review--overlays
                   buffer-file-name
                   ;; Only include buffers that belong to the current project.
                   (or (null root)
                       (string-prefix-p root
                                        (expand-file-name buffer-file-name))))
          (dolist (ov inline-review--overlays)
            (when (and (overlay-buffer ov)
                       (overlay-get ov 'inline-review))
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

(defun inline-review--current-thread-key ()
  "Return a (ABS-PATH . LINE) key representing the current position.
LINE is the current line number; ABS-PATH is the current buffer's absolute path."
  (cons (expand-file-name (or buffer-file-name default-directory))
        (line-number-at-pos)))

;;;; ─── Position Helpers ───────────────────────────────────────────────────────

(defun inline-review--line-end-pos (line)
  "Return buffer position at end of LINE (1-based)."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (line-end-position)))

;;;; ─── Public Commands ────────────────────────────────────────────────────────

;;;###autoload
(defun inline-review-add-comment (beg end)
  "Add a code review comment for selected region BEG..END."
  (interactive "r")
  (unless (bound-and-true-p inline-review-mode)
    (user-error
     "inline-review: please enable `inline-review-mode' first"))
  (unless inline-review--mr-iid
    (user-error "inline-review: no MR IID set"))
  (inline-review--assert-token
   inline-review--current-backend)
  (inline-review--open-input-overlay beg end))

;;;###autoload
(defun inline-review-edit-comment ()
  "Edit the code review comment at point."
  (interactive)
  (unless (bound-and-true-p inline-review-mode)
    (user-error
     "inline-review: please enable `inline-review-mode' first"))
  (let ((ov (inline-review--overlay-at-point)))
    (unless ov
      (user-error
       "inline-review: no comment overlay on this line"))
    (let ((note-id (overlay-get ov 'inline-review-note-id))
          (note-body (overlay-get ov 'inline-review-body))
          (line (line-beginning-position)))
      (inline-review--open-input-overlay
       line line note-id note-body))))

;;;###autoload
(defun inline-review-resolve-comment ()
  "Mark the comment at point as resolved."
  (interactive)
  (unless (bound-and-true-p inline-review-mode)
    (user-error
     "inline-review: please enable `inline-review-mode' first"))
  (let ((ov (inline-review--overlay-at-point)))
    (unless ov
      (user-error
       "inline-review: no comment overlay on this line"))
    (let ((already (overlay-get ov 'inline-review-resolved)))
      (when (eq already t)
        (user-error
         "inline-review: comment is already resolved"))
      (inline-review--assert-token
       inline-review--current-backend)
      (inline-review--resolve-comment ov))))

;;;###autoload
(defun inline-review-reply-comment ()
  "Reply to the code review comment thread at point."
  (interactive)
  (unless (bound-and-true-p inline-review-mode)
    (user-error
     "inline-review: please enable `inline-review-mode' first"))
  (let ((ov (inline-review--overlay-at-point)))
    (unless ov
      (user-error
       "inline-review: no comment overlay on this line"))
    (inline-review--assert-token
     inline-review--current-backend)
    (let ((note-id (overlay-get ov 'inline-review-note-id))
          (line (line-beginning-position)))
      (inline-review--open-input-overlay
       line line nil nil note-id))))

;;;###autoload
(defun inline-review-delete-comment ()
  "Delete the code review comment at point."
  (interactive)
  (unless (bound-and-true-p inline-review-mode)
    (user-error
     "inline-review: please enable `inline-review-mode' first"))
  (let ((ov (inline-review--overlay-at-point)))
    (unless ov
      (user-error
       "inline-review: no comment overlay on this line"))
    (inline-review--assert-token
     inline-review--current-backend)
    (let ((note-id (overlay-get ov 'inline-review-note-id)))
      (when (yes-or-no-p (format "Delete comment %s? " note-id))
        (inline-review--delete-comment note-id)))))

;;;###autoload
(defun inline-review-toggle-hide-resolved ()
  "Toggle hiding of resolved comment threads and refresh overlays."
  (interactive)
  (setq inline-review-hide-resolved (not inline-review-hide-resolved))
  (message "inline-review: %s resolved threads"
           (if inline-review-hide-resolved "hiding" "showing"))
  (when (and inline-review-mode inline-review--mr-iid)
    (inline-review--refresh-overlays)))

;;;###autoload
(defun inline-review-next-thread ()
  "Move point to the next comment thread within the current project.
Stops at the last thread with a message rather than wrapping to the first."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let* ((all (inline-review--all-thread-positions))
         (cur (inline-review--current-thread-key))
         (next (cl-find-if
                (lambda (entry)
                  (or (string< (car cur) (car entry))
                      (and (string= (car cur) (car entry))
                           (< (cdr cur) (cdr entry)))))
                all)))
    (if next
        (inline-review--goto-hunk (car next) (cdr next))
      (message "inline-review: no more comment threads in this project"))))

;;;###autoload
(defun inline-review-previous-thread ()
  "Move point to the previous comment thread within the current project.
Stops at the first thread with a message rather than wrapping to the last."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let* ((all (inline-review--all-thread-positions))
         (cur (inline-review--current-thread-key))
         (prev (cl-find-if
                (lambda (entry)
                  (or (string< (car entry) (car cur))
                      (and (string= (car entry) (car cur))
                           (< (cdr entry) (cdr cur)))))
                (reverse all))))
    (if prev
        (inline-review--goto-hunk (car prev) (cdr prev))
      (message "inline-review: no more comment threads in this project"))))

;;;###autoload
(defun inline-review-first-thread ()
  "Move point to the first comment thread within the current project."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let ((all (inline-review--all-thread-positions)))
    (if all
        (let ((first (car all)))
          (inline-review--goto-hunk (car first) (cdr first)))
      (message "inline-review: no comment threads in this project"))))

;;;###autoload
(defun inline-review-last-thread ()
  "Move point to the last comment thread within the current project."
  (interactive)
  (unless (inline-review--review-in-progress-p)
    (user-error
     "inline-review: no active review for this repository — run `inline-review-review-url' first"))
  (let ((all (inline-review--all-thread-positions)))
    (if all
        (let ((last (car (last all))))
          (inline-review--goto-hunk (car last) (cdr last)))
      (message "inline-review: no comment threads in this project"))))

;;;; ─── Backend Dispatch ───────────────────────────────────────────────────────

(defun inline-review--post-comment (beg end body)
  "Post a new comment via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (inline-review--backend-prop
              inline-review--current-backend
              :post)
             beg end body
             (lambda ()
               (with-current-buffer buf
                 (inline-review--refresh-overlays))))))

(defun inline-review--update-comment (note-id body)
  "Update an existing comment via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (inline-review--backend-prop
              inline-review--current-backend
              :update)
             note-id body
             (lambda ()
               (with-current-buffer buf
                 (inline-review--refresh-overlays))))))

(defun inline-review--resolve-comment (ov)
  "Resolve a comment via the current backend, then refresh overlays."
  (let ((buf (current-buffer))
        (note-id (overlay-get ov 'inline-review-note-id))
        (note-body (overlay-get ov 'inline-review-body)))
    (funcall (inline-review--backend-prop
              inline-review--current-backend
              :resolve)
             note-id note-body
             (lambda ()
               (with-current-buffer buf
                 (inline-review--refresh-overlays))))))

(defun inline-review--reply-comment (note-id body)
  "Post a reply to the thread rooted at NOTE-ID via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (inline-review--backend-prop
              inline-review--current-backend
              :reply)
             note-id body
             (lambda ()
               (with-current-buffer buf
                 (inline-review--refresh-overlays))))))

(defun inline-review--delete-comment (note-id)
  "Delete the comment NOTE-ID via the current backend, then refresh overlays."
  (let ((buf (current-buffer)))
    (funcall (inline-review--backend-prop
              inline-review--current-backend
              :delete)
             note-id
             (lambda ()
               (with-current-buffer buf
                 (inline-review--refresh-overlays))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-comment)

;;; inline-review-comment.el ends here
