;;; inline-review-gongfeng.el --- Gongfeng backend for inline-review -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Gongfeng backend for inline-review.
;; Handles MR comment fetching, posting, updating, and resolving against
;; Gongfeng (工蜂), Tencent's code hosting platform, accessible at
;; git.woa.com (internal) and code.tencent.com (external).
;;
;; Authentication:
;;   Tokens are read exclusively from authinfo/netrc.  Add an entry to
;;   ~/.authinfo (or ~/.authinfo.gpg):
;;     machine git.woa.com      login ^crm password <token>
;;     machine code.tencent.com login ^crm password <token>
;;   The host is derived from `inline-review-gongfeng-api-url'.
;;
;; HTTP layer: Emacs built-in `url-retrieve' with a PRIVATE-TOKEN request
;; header.  Gongfeng runs a customised GitLab REST API v3 that is NOT
;; wire-compatible with the GitLab API v4 used by the gitlab backend, so
;; ghub is deliberately avoided here.
;;
;; API endpoints used (base: https://git.woa.com/api/v3, configurable via
;; `inline-review-gongfeng-api-url'):
;;   Resolve MR id : GET    /projects/:encoded_path/merge_request/iid/:iid → .id
;;   List notes    : GET    /projects/:encoded_path/merge_requests/:id/notes
;;   Create note   : POST   /projects/:encoded_path/merge_requests/:id/notes
;;   Update note   : PUT    /projects/:encoded_path/merge_requests/:id/notes/:note_id
;;   Resolve note  : PUT    /projects/:encoded_path/merge_requests/:id/notes/:note_id
;;   Reply note    : POST   /projects/:encoded_path/merge_requests/:id/notes/:note_id/replies
;;   Delete note   : DELETE /projects/:encoded_path/merge_requests/:id/notes/:note_id
;;   Diff changes  : GET    /projects/:encoded_path/merge_request/:id/changes
;;
;; Backend contract:
;;   :fetch  (callback)                — calls (callback THREADS)
;;   :post   (beg end body on-success) — calls (on-success) on success
;;   :update (note-id body on-success) — calls (on-success) on success
;;   :resolve (ov on-success)          — calls (on-success) on success

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'subr-x)
(require 'inline-review-backend)

;;;; ─── Gongfeng Remote Parsing ────────────────────────────────────────────────

(defun inline-review--parse-gongfeng-project-path (remote-url)
  "Extract namespace/project from REMOTE-URL (ssh or https)."
  (when remote-url
    (cond
     ;; SSH: git@git.woa.com:namespace/project.git
     ((string-match "git@[^:]+:\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS: https://git.woa.com/namespace/project.git
     ((string-match "https?://[^/]+/\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS without .git suffix
     ((string-match "https?://[^/]+/\\(.*[^/]\\)/*$" remote-url)
      (match-string 1 remote-url)))))

;;;; ─── Gongfeng HTTP Layer (url-retrieve, not ghub) ──────────────────────────

(defun inline-review--gongfeng-api-url (&rest path-segments)
  "Build a full Gongfeng API URL by joining PATH-SEGMENTS onto the base URL."
  (concat
   inline-review-gongfeng-api-url
   "/"
   (mapconcat #'identity path-segments "/")))

(defun inline-review--gongfeng-http-status ()
  "Return the integer HTTP status from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun inline-review--gongfeng-response-body ()
  "Return the response body string from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*$" nil t)
        (decode-coding-string
         (buffer-substring (point) (point-max)) 'utf-8)
      "")))

(defun inline-review--gongfeng-parse-response ()
  "Parse JSON body from the current url-retrieve buffer."
  (let ((body (inline-review--gongfeng-response-body)))
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string body))
      (error
       (message
        "inline-review[gongfeng]: JSON parse error: %S\nbody: %s"
        err body)
       nil))))

(defun inline-review--gongfeng-http-request
    (method url &optional payload callback)
  "Perform async HTTP METHOD request to Gongfeng URL via url-retrieve.
PAYLOAD is an alist JSON-encoded as the request body.
CALLBACK receives the parsed JSON response (or nil on error).
The request is aborted after 30 seconds to prevent Emacs from hanging on a dead or slow server."
  (let*
      ((token
        (encode-coding-string
         (or (inline-review--get-token 'gongfeng) "") 'utf-8))
       (url-request-method method)
       (url-request-extra-headers
        `(("PRIVATE-TOKEN" . ,token)
          ("Content-Type" . "application/json; charset=utf-8")))
       (url-request-data
        (when payload
          (let*
              ((body (json-encode payload))
               ;; encode-coding-string produces a unibyte UTF-8 string,
               ;; which url-http-create-request requires (Emacs Bug#23750).
               (encoded (encode-coding-string body 'utf-8)))
            encoded)))
       ;; Disable keep-alives so stale connections don't silently block.
       (url-http-attempt-keepalives nil)
       ;; timer is declared here and set below so that the url-retrieve
       ;; callback (which references it) closes over the variable correctly.
       (watchdog-timer nil)
       (buf
        (url-retrieve
         url
         (lambda (status)
           (when watchdog-timer
             (cancel-timer watchdog-timer))
           (let* ((http-status
                   (inline-review--gongfeng-http-status))
                  (err (plist-get status :error))
                  (body
                   (inline-review--gongfeng-response-body)))
             (cond
              (err
               (message
                "inline-review[gongfeng]: HTTP error %S (URL: %s)\n  body: %s"
                err url (substring body 0 (min 400 (length body)))))
              ((and http-status (>= http-status 400))
               (message
                "inline-review[gongfeng]: HTTP %d for %s\n  body: %s"
                http-status
                url
                (substring body 0 (min 400 (length body)))))
              (t
               (when callback
                 (funcall
                  callback
                  (inline-review--gongfeng-parse-response)))))))
         nil t)))
    ;; Set up a watchdog timer that kills the retrieval buffer if the
    ;; request hasn't completed within the configured timeout.
    (when buf
      (setq
       watchdog-timer
       (run-with-timer
        30 nil
        (lambda ()
          (when (buffer-live-p buf)
            (message
             "inline-review[gongfeng]: request timed out after %ds — %s"
             30 url)
            (kill-buffer buf))))))
    ;; Return buf so callers can cancel if needed (normally unused).
    buf))

;;;; ─── Gongfeng Backend Functions ────────────────────────────────────────────

(defun inline-review--gongfeng-resolve-branches (callback)
  "Fetch MR source and target branch names, then call CALLBACK with them.
Calls (funcall CALLBACK SOURCE-BRANCH TARGET-BRANCH), both strings or nil.
Makes the same single lightweight MR-metadata GET used by resolve-mr-id.
Caches the MR global id as a side-effect so subsequent resolve-mr-id
calls skip the network round-trip.  Does not touch buffer-local branch
variables — that is the caller's responsibility."
  (let* ((project-id (inline-review--gongfeng-ensure-project-id))
         (iid inline-review--mr-iid)
         (url
          (inline-review--gongfeng-api-url
           "projects" project-id "merge_request" "iid"
           (number-to-string iid)))
         (buf (current-buffer)))
    (inline-review--gongfeng-http-request
     "GET" url nil
     (lambda (mr)
       (let ((mr-id (and mr (alist-get 'id mr))))
         (when (numberp mr-id)
           (with-current-buffer buf
             (setq inline-review--mr-id mr-id))))
       (funcall callback
                (and mr (alist-get 'source_branch mr))
                (and mr (alist-get 'target_branch mr)))))))

(defun inline-review--gongfeng-ensure-project-id ()
  "Set project ID from remote for Gongfeng backend."
  (unless (alist-get 'project-id inline-review--project-info)
    (let* ((remote (inline-review--git-remote-url))
           (path
            (inline-review--parse-gongfeng-project-path
             remote)))
      (if path
          (progn
            (message "inline-review: detected project %s" path)
            (setq inline-review--project-info
                  `((project-id . ,(url-hexify-string path)))))
        (let ((manual
               (read-string "Project path (e.g. team/project): ")))
          (setq inline-review--project-info
                `((project-id . ,(url-hexify-string manual))))))))
  (alist-get 'project-id inline-review--project-info))

(defun inline-review--gongfeng-resolve-mr-id (callback)
  "Resolve MR global id for the current IID and call CALLBACK with it."
  (if inline-review--mr-id
      (progn
        (message
         "[crm-gongfeng] resolve-mr-id: reusing cached mr-id=%d"
         inline-review--mr-id)
        (funcall callback inline-review--mr-id))
    (let* ((project-id
            (inline-review--gongfeng-ensure-project-id))
           (iid inline-review--mr-iid)
           (url
            (inline-review--gongfeng-api-url
             "projects"
             project-id
             "merge_request"
             "iid"
             (number-to-string iid)))
           (buf (current-buffer)))
      (message "[crm-gongfeng] resolve-mr-id: fetching iid=%d url=%s"
               iid
               url)
      (inline-review--gongfeng-http-request
       "GET" url
       nil
       (lambda (mr)
         (message "[crm-gongfeng] resolve-mr-id: response keys=%S"
                  (and mr (mapcar #'car mr)))
         (let ((mr-id
                (and mr (alist-get 'id mr))))
           (if (not
                (numberp mr-id))
               (message
                "[crm-gongfeng] resolve-mr-id: failed — id=%S full response: %S"
                mr-id mr)
             (message
              "[crm-gongfeng] resolve-mr-id: resolved iid=%d → mr-id=%d"
              iid mr-id)
             (with-current-buffer buf
               (setq inline-review--mr-id mr-id))
             (funcall callback mr-id))))))))

(defun inline-review--gongfeng-fetch-comments (callback)
  "Fetch MR notes and call CALLBACK with a list of thread plists (Gongfeng)."
  (let* ((project-id
          (inline-review--gongfeng-ensure-project-id))
         (mr-iid inline-review--mr-iid))
    (message "inline-review: fetching comments for MR !%d ..."
             mr-iid)
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (concat
               (inline-review--gongfeng-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes")
               "?per_page=100")))
         (inline-review--gongfeng-http-request
          "GET" url
          nil
          (lambda (notes)
            (funcall callback
                     (inline-review--gongfeng-normalize-notes
                      notes)))))))))

(defun inline-review--gongfeng-fetch-diff (callback)
  "Fetch MR changes and call CALLBACK with a list of change plists (Gongfeng).
Each plist has :old-path, :new-path, and :patch (unified diff string)."
  (let* ((project-id
          (inline-review--gongfeng-ensure-project-id))
         (mr-iid inline-review--mr-iid))
    (message "inline-review: fetching diff for MR !%d ..."
             mr-iid)
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (inline-review--gongfeng-api-url
               "projects"
               project-id
               "merge_request"
               (number-to-string mr-id)
               "changes")))
         (message "[crm-gongfeng] fetch-diff: GET %s" url)
         (inline-review--gongfeng-http-request
          "GET" url
          nil
          (lambda (resp)
            (message
             "[crm-gongfeng] fetch-diff: response type=%s changes-count=%s"
             (type-of resp) (length (alist-get 'files resp)))
            (funcall callback
                     (mapcar
                      (lambda (c)
                        (list
                         :old-path (alist-get 'old_path c)
                         :new-path (alist-get 'new_path c)
                         :patch (alist-get 'diff c)))
                      (or (alist-get 'files resp) '()))))))))))


(defun inline-review--gongfeng-normalize-notes (notes)
  "Convert Gongfeng NOTES list into the standard thread plist format."
  (let ((by-id (make-hash-table))
        (children (make-hash-table))
        (roots nil)
        (result nil))
    (dolist (n (or notes '()))
      (let ((id (alist-get 'id n))
            (pid (alist-get 'parent_id n)))
        (puthash id n by-id)
        (if pid
            (puthash
             pid (append (gethash pid children) (list n)) children)
          (push n roots))))
    (dolist (root (nreverse roots))
      (let* ((file-path (alist-get 'file_path root))
             (note-pos (alist-get 'note_position root))
             (latest-pos
              (and note-pos (alist-get 'latest_position note-pos)))
             (line-num
              (and latest-pos
                   (or (alist-get 'right_line_num latest-pos)
                       (alist-get 'left_line_num latest-pos))))
             (resolve-state (alist-get 'resolve_state root))
             (resolved
              (cond
               ((eql resolve-state 2)
                t)
               ((eql resolve-state 1)
                :json-false)
               (t
                nil)))
             (outdated
              (or (alist-get 'outdated root)
                  (and note-pos (null latest-pos))))
             (root-id (alist-get 'id root))
             (thread
              (cons
               root
               (sort (copy-sequence
                      (or (gethash root-id children) '()))
                     (lambda (a b)
                       (string<
                        (or (alist-get 'created_at a) "")
                        (or (alist-get 'created_at b) "")))))))
        (when (and (integerp line-num) file-path)
          (push (list
                 :path file-path
                 :line line-num
                 :thread thread
                 :resolved resolved
                 :outdated outdated
                 :note-id root-id)
                result))))
    (nreverse result)))

(defun inline-review--gongfeng-post-comment
    (_beg end body on-success)
  "Post comment on line at END with BODY (Gongfeng), then call ON-SUCCESS."
  (let* ((project-id
          (inline-review--gongfeng-ensure-project-id))
         (rel-path (inline-review--relative-file-path))
         (end-line (inline-review--line-number-at end)))
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (inline-review--gongfeng-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes"))
              (payload
               `((body . ,body)
                 (path . ,rel-path)
                 (line . ,(number-to-string end-line))
                 (line_type . "new"))))
         (inline-review--gongfeng-http-request
          "POST" url
          payload
          (lambda (resp)
            (if (and resp
                     (alist-get 'id resp))
                (progn
                  (message
                   "inline-review: comment posted (id=%s)"
                   (alist-get 'id resp))
                  (funcall on-success))
              (message
               "inline-review: failed to post comment")))))))))

(defun inline-review--gongfeng-update-comment
    (note-id body on-success)
  "Update NOTE-ID with BODY (Gongfeng), then call ON-SUCCESS."
  (let* ((project-id
          (inline-review--gongfeng-ensure-project-id)))
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (inline-review--gongfeng-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes"
                (number-to-string note-id)))
              (payload `((body . ,body))))
         (inline-review--gongfeng-http-request
          "PUT" url
          payload
          (lambda (resp)
            (if (and resp
                     (alist-get 'id resp))
                (progn
                  (message "inline-review: note %d updated"
                           note-id)
                  (funcall on-success))
              (message "inline-review: failed to update note %d"
                       note-id)))))))))

(defun inline-review--gongfeng-resolve-comment
    (note-id note-body on-success)
  "Resolve comment NOTE-ID with NOTE-BODY (Gongfeng), then call ON-SUCCESS."
  (let ((project-id
         (inline-review--gongfeng-ensure-project-id)))
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (inline-review--gongfeng-api-url
               "projects"
               project-id
               "merge_requests"
               (number-to-string mr-id)
               "notes"
               (number-to-string note-id))))
         (inline-review--gongfeng-http-request
          "PUT" url
          `((body . ,note-body) (resolve_state . 2))
          (lambda (resp)
            (if (and resp
                     (alist-get 'id resp))
                (progn
                  (message "inline-review: note %d resolved"
                           note-id)
                  (funcall on-success))
              (message
               "inline-review: failed to resolve note %d"
               note-id)))))))))

(defun inline-review--gongfeng-reply-comment
    (note-id body on-success)
  "Post a reply to the root note NOTE-ID with BODY (Gongfeng), then call ON-SUCCESS.

Uses POST /projects/:id/merge_requests/:mr_id/notes/:note_id/replies.
NOTE-ID must be the root note of a thread; the API does not support
replying to replies."
  (let ((project-id
         (inline-review--gongfeng-ensure-project-id)))
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (inline-review--gongfeng-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes"
                (number-to-string note-id)
                "replies"))
              (payload
               `((body . ,body) (notify_enabled . :json-false))))
         (inline-review--gongfeng-http-request
          "POST" url
          payload
          (lambda (resp)
            (if (and resp
                     (alist-get 'id resp))
                (progn
                  (message "inline-review: reply posted (id=%s)"
                           (alist-get 'id resp))
                  (funcall on-success))
              (message
               "inline-review: failed to post reply")))))))))

(defun inline-review--gongfeng-delete-comment
    (note-id on-success)
  "Delete note NOTE-ID (Gongfeng), then call ON-SUCCESS."
  (let ((project-id
         (inline-review--gongfeng-ensure-project-id)))
    (inline-review--gongfeng-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (inline-review--gongfeng-api-url
               "projects"
               project-id
               "merge_requests"
               (number-to-string mr-id)
               "notes"
               (number-to-string note-id))))
         (inline-review--gongfeng-http-request
          "DELETE" url
          nil
          (lambda (_resp)
            (message "inline-review: note %d deleted" note-id)
            (funcall on-success))))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-gongfeng)

;;; inline-review-gongfeng.el ends here
