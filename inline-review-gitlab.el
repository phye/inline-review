;;; inline-review-gitlab.el --- GitLab backend for inline-review -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; GitLab backend for inline-review.
;; Handles MR comment fetching, posting, updating, and resolving via the
;; GitLab REST API v4.
;;
;; Authentication:
;;   Tokens are read exclusively from authinfo/netrc.  Add an entry to
;;   ~/.authinfo (or ~/.authinfo.gpg):
;;     machine gitlab.com login ^crm password <token>
;;   For self-hosted instances, use the host from `inline-review-gitlab-api-url'.
;;
;; HTTP layer: ghub (`ghub-request' with :forge 'gitlab), PRIVATE-TOKEN header.
;;
;; This backend is for standard GitLab instances (API v4) only.  For Gongfeng
;; (Tencent's internal GitLab at git.woa.com, which runs a custom API v3
;; not wire-compatible with v4), see inline-review-gongfeng.el.
;;
;; Backend contract:
;;   :fetch  (callback)                — calls (callback THREADS)
;;   :post   (beg end body on-success) — calls (on-success) on success
;;   :update (note-id body on-success) — calls (on-success) on success
;;   :resolve (ov on-success)          — calls (on-success) on success

;;; Code:

(require 'ghub)
(require 'inline-review-backend)

;;;; ─── GitLab Remote Parsing ─────────────────────────────────────────────────

(defun inline-review--parse-gitlab-project-path (remote-url)
  "Extract namespace/project from REMOTE-URL (ssh or https)."
  (when remote-url
    (cond
     ;; SSH: git@host:namespace/project.git
     ((string-match "git@[^:]+:\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS: https://host/namespace/project.git
     ((string-match "https?://[^/]+/\\(.*\\)\\.git$" remote-url)
      (match-string 1 remote-url))
     ;; HTTPS without .git suffix
     ((string-match "https?://[^/]+/\\(.*[^/]\\)/*$" remote-url)
      (match-string 1 remote-url)))))

;;;; ─── GitLab HTTP Layer ──────────────────────────────────────────────────────

(defun inline-review--gitlab-api-url (&rest path-segments)
  "Build a full GitLab API URL by joining PATH-SEGMENTS onto the base URL."
  (concat
   inline-review-gitlab-api-url
   "/"
   (mapconcat #'identity path-segments "/")))

(defun inline-review--gitlab-http-request
    (method url &optional payload callback)
  "Perform async HTTP METHOD request to GitLab URL via ghub.
PAYLOAD is an alist sent as JSON body.  CALLBACK receives parsed JSON."
  (inline-review--assert-token 'gitlab)
  (let* ((token (inline-review--get-token 'gitlab))
         (host
          (replace-regexp-in-string
           "^https?://" "" inline-review-gitlab-api-url))
         (resource
          (substring url (length inline-review-gitlab-api-url)))
         (wrapped-callback
          (when callback
            (lambda (result _headers _status _req)
              (funcall callback result)))))
    (ghub-request
     method resource nil
     :auth token
     :host host
     :forge 'gitlab
     :payload payload
     :callback wrapped-callback
     :errorback
     (lambda (err _headers _status _req)
       (message "inline-review[gitlab]: HTTP error for %s: %S"
                url err)))))

;;;; ─── GitLab Backend Functions ──────────────────────────────────────────────

(defun inline-review--gitlab-resolve-branches (callback)
  "Fetch MR source and target branch names, then call CALLBACK with them.
Calls (funcall CALLBACK SOURCE-BRANCH TARGET-BRANCH), both strings or nil.
Makes the same single lightweight MR-metadata GET used by resolve-mr-id.
Caches the MR global id as a side-effect so subsequent resolve-mr-id
calls skip the network round-trip.  Does not touch buffer-local branch
variables — that is the caller's responsibility."
  (let* ((project-id (inline-review--gitlab-ensure-project-id))
         (iid inline-review--mr-iid)
         (url
          (inline-review--gitlab-api-url
           "projects" project-id "merge_request" "iid"
           (number-to-string iid)))
         (buf (current-buffer)))
    (inline-review--gitlab-http-request
     "GET" url nil
     (lambda (mr)
       (let ((mr-id (and mr (alist-get 'id mr))))
         (when (numberp mr-id)
           (with-current-buffer buf
             (setq inline-review--mr-id mr-id))))
       (funcall callback
                (and mr (alist-get 'source_branch mr))
                (and mr (alist-get 'target_branch mr)))))))

(defun inline-review--gitlab-ensure-project-id ()
  "Set project ID from remote for GitLab backend."
  (unless (alist-get 'project-id inline-review--project-info)
    (let* ((remote (inline-review--git-remote-url))
           (path
            (inline-review--parse-gitlab-project-path remote)))
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

(defun inline-review--gitlab-resolve-mr-id (callback)
  "Resolve MR global id for the current IID and call CALLBACK with it."
  (if inline-review--mr-id
      (funcall callback inline-review--mr-id)
    (let* ((project-id
            (inline-review--gitlab-ensure-project-id))
           (iid inline-review--mr-iid)
           (url
            (inline-review--gitlab-api-url
             "projects"
             project-id
             "merge_request"
             "iid"
             (number-to-string iid)))
           (buf (current-buffer)))
      (message "inline-review: resolving MR id for IID %d ..."
               iid)
      (inline-review--gitlab-http-request
       "GET" url
       nil
       (lambda (mr)
         (let ((mr-id
                (and mr (alist-get 'id mr))))
           (if (not
                (numberp mr-id))
               (message
                "inline-review: failed to resolve MR id")
             (with-current-buffer buf
               (setq inline-review--mr-id mr-id))
             (funcall callback mr-id))))))))

(defun inline-review--gitlab-fetch-comments (callback)
  "Fetch MR notes and call CALLBACK with a list of thread plists (GitLab)."
  (let* ((project-id (inline-review--gitlab-ensure-project-id))
         (mr-iid inline-review--mr-iid))
    (message "inline-review: fetching comments for MR !%d ..."
             mr-iid)
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (concat
               (inline-review--gitlab-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes")
               "?per_page=100")))
         (inline-review--gitlab-http-request
          "GET" url
          nil
          (lambda (notes)
            (funcall callback
                     (inline-review--gitlab-normalize-notes
                      notes)))))))))

(defun inline-review--gitlab-fetch-diff (callback)
  "Fetch MR changes and call CALLBACK with a list of change plists (GitLab).
Each plist has :old-path, :new-path, and :patch (unified diff string)."
  (let* ((project-id (inline-review--gitlab-ensure-project-id))
         (mr-iid inline-review--mr-iid))
    (message "inline-review: fetching diff for MR !%d ..."
             mr-iid)
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (inline-review--gitlab-api-url
               "projects"
               project-id
               "merge_requests"
               (number-to-string mr-id)
               "changes")))
         (inline-review--gitlab-http-request
          "GET" url
          nil
          (lambda (resp)
            (funcall callback
                     (mapcar
                      (lambda (c)
                        (list
                         :old-path (alist-get 'old_path c)
                         :new-path (alist-get 'new_path c)
                         :patch (alist-get 'diff c)))
                      (or (alist-get 'changes resp) '()))))))))))

(defun inline-review--gitlab-normalize-notes (notes)
  "Convert GitLab NOTES list into the standard thread plist format."
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

(defun inline-review--gitlab-post-comment
    (_beg end body on-success)
  "Post comment on line at END with BODY (GitLab), then call ON-SUCCESS."
  (let* ((project-id (inline-review--gitlab-ensure-project-id))
         (rel-path (inline-review--relative-file-path))
         (end-line (inline-review--line-number-at end)))
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (inline-review--gitlab-api-url
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
         (inline-review--gitlab-http-request
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

(defun inline-review--gitlab-update-comment
    (note-id body on-success)
  "Update NOTE-ID with BODY (GitLab), then call ON-SUCCESS."
  (let* ((project-id (inline-review--gitlab-ensure-project-id)))
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (inline-review--gitlab-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes"
                (number-to-string note-id)))
              (payload `((body . ,body))))
         (inline-review--gitlab-http-request
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

(defun inline-review--gitlab-resolve-comment
    (note-id note-body on-success)
  "Resolve comment NOTE-ID with NOTE-BODY (GitLab), then call ON-SUCCESS."
  (let* ((project-id (inline-review--gitlab-ensure-project-id)))
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (inline-review--gitlab-api-url
               "projects"
               project-id
               "merge_requests"
               (number-to-string mr-id)
               "notes"
               (number-to-string note-id))))
         (inline-review--gitlab-http-request
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

(defun inline-review--gitlab-reply-comment
    (note-id body on-success)
  "Post a reply to the thread rooted at NOTE-ID with BODY (GitLab), then call ON-SUCCESS."
  (let* ((project-id (inline-review--gitlab-ensure-project-id)))
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let* ((url
               (inline-review--gitlab-api-url
                "projects"
                project-id
                "merge_requests"
                (number-to-string mr-id)
                "notes"))
              (payload `((body . ,body) (parent_id . ,note-id))))
         (inline-review--gitlab-http-request
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

(defun inline-review--gitlab-delete-comment (note-id on-success)
  "Delete note NOTE-ID (GitLab), then call ON-SUCCESS."
  (let* ((project-id (inline-review--gitlab-ensure-project-id)))
    (inline-review--gitlab-resolve-mr-id
     (lambda (mr-id)
       (let ((url
              (inline-review--gitlab-api-url
               "projects"
               project-id
               "merge_requests"
               (number-to-string mr-id)
               "notes"
               (number-to-string note-id))))
         (inline-review--gitlab-http-request
          "DELETE" url
          nil
          (lambda (_resp)
            (message "inline-review: note %d deleted" note-id)
            (funcall on-success))))))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-gitlab)

;;; inline-review-gitlab.el ends here
