;;; inline-review-github.el --- GitHub backend for inline-review -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; GitHub backend for inline-review.
;; Handles PR review-comment fetching, posting, and updating via the
;; GitHub REST API v3.
;;
;; Authentication:
;;   Tokens are read exclusively from authinfo/netrc.  Add an entry to
;;   ~/.authinfo (or ~/.authinfo.gpg):
;;     machine api.github.com login ^crm password <token>
;;   For GitHub Enterprise, use the host from `inline-review-github-api-url'.
;;
;; HTTP layer: ghub (`ghub-request'), Authorization: Bearer header.
;;
;; Backend contract:
;;   :fetch  (callback)            — calls (callback THREADS) where THREADS is a
;;                                   list of plists; see `inline-review-register-backend'.
;;   :post   (beg end body on-success) — calls (on-success) on success.
;;   :update (note-id body on-success) — calls (on-success) on success.
;;   :resolve (ov on-success)          — GitHub has no resolve API; shows a
;;                                       message and does NOT call on-success.

;;; Code:

(require 'ghub)
(require 'inline-review-backend)

;;;; ─── GitHub Remote Parsing ─────────────────────────────────────────────────

(defun inline-review--parse-github-repo (remote-url)
  "Parse GitHub REMOTE-URL to get (owner . repo)."
  (when remote-url
    (cond
     ;; SSH: git@github.com:owner/repo.git
     ((string-match
       "git@github\\.com:\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; HTTPS: https://github.com/owner/repo.git
     ((string-match
       "https?://github\\.com/\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; Without .git suffix
     ((string-match
       "https?://github\\.com/\\([^/]+\\)/\\([^/]+\\)/?$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; GitHub Enterprise SSH
     ((string-match
       "git@[^:]+:\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; GitHub Enterprise HTTPS
     ((string-match
       "https?://[^/]+/\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons
       (match-string 1 remote-url) (match-string 2 remote-url))))))

;;;; ─── GitHub HTTP Layer ──────────────────────────────────────────────────────

(defun inline-review--github-api-url (&rest path-segments)
  "Build a full GitHub API URL by joining PATH-SEGMENTS onto the base URL."
  (concat
   inline-review-github-api-url
   "/"
   (mapconcat #'identity path-segments "/")))

(defun inline-review--github-http-request
    (method url &optional payload callback)
  "Perform async HTTP METHOD request to GitHub URL via ghub.
PAYLOAD is an alist sent as JSON body.  CALLBACK receives parsed JSON."
  (inline-review--assert-token 'github)
  (let* ((token (inline-review--get-token 'github))
         (host
          (replace-regexp-in-string
           "^https?://" "" inline-review-github-api-url))
         (resource
          (substring url (length inline-review-github-api-url)))
         (wrapped-callback
          (when callback
            (lambda (result _headers _status _req)
              (funcall callback result)))))
    (ghub-request
     method resource nil
     :auth token
     :host host
     :payload payload
     :callback wrapped-callback
     :errorback
     (lambda (err _headers _status _req)
       (message "inline-review[github]: HTTP error for %s: %S"
                url err)))))

;;;; ─── GitHub Backend Functions ──────────────────────────────────────────────

(defun inline-review--github-ensure-project-info ()
  "Set project info from remote for GitHub backend."
  (unless (alist-get 'owner inline-review--project-info)
    (let* ((remote (inline-review--git-remote-url))
           (parsed (inline-review--parse-github-repo remote)))
      (if parsed
          (progn
            (message "inline-review: detected repo %s/%s"
                     (car parsed)
                     (cdr parsed))
            (setq inline-review--project-info
                  `((owner . ,(car parsed)) (repo . ,(cdr parsed)))))
        (let ((owner (read-string "GitHub owner/organization: "))
              (repo (read-string "GitHub repository name: ")))
          (setq inline-review--project-info
                `((owner . ,owner) (repo . ,repo))))))))

(defun inline-review--github-resolve-branches (callback)
  "Fetch PR source and target branch names, then call CALLBACK with them.
Calls (funcall CALLBACK SOURCE-BRANCH TARGET-BRANCH), both strings or nil.
Makes a single lightweight GET to the PR endpoint to retrieve head.ref
and base.ref.  Does not touch buffer-local branch variables — that is
the caller's responsibility."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid)
         (url
          (inline-review--github-api-url
           "repos" owner repo "pulls" (number-to-string pr-number))))
    (inline-review--github-http-request
     "GET" url nil
     (lambda (pr)
       (funcall callback
                (and pr (alist-get 'ref (alist-get 'head pr)))
                (and pr (alist-get 'ref (alist-get 'base pr))))))))

(defun inline-review--github-fetch-comments (callback)
  "Fetch PR comments and call CALLBACK with a list of thread plists (GitHub)."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid))
    (message "inline-review: fetching comments for PR #%d ..."
             pr-number)
    (let ((url
           (inline-review--github-api-url
            "repos"
            owner
            repo
            "pulls"
            (number-to-string pr-number)
            "comments")))
      (inline-review--github-http-request
       "GET" url
       nil
       (lambda (comments)
         (funcall callback
                  (inline-review--github-normalize-comments
                   comments)))))))

(defun inline-review--github-fetch-diff (callback)
  "Fetch PR changed files and call CALLBACK with a list of change plists (GitHub).
Each plist has :old-path, :new-path, and :patch (unified diff string)."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid))
    (message "inline-review: fetching diff for PR #%d ..."
             pr-number)
    (let ((url
           (inline-review--github-api-url
            "repos"
            owner
            repo
            "pulls"
            (number-to-string pr-number)
            "files")))
      (inline-review--github-http-request
       "GET" url
       nil
       (lambda (files)
         (funcall callback
                  (mapcar
                   (lambda (f)
                     (list
                      :old-path
                      (or (alist-get 'previous_filename f)
                          (alist-get 'filename f))
                      :new-path (alist-get 'filename f)
                      :patch (alist-get 'patch f)))
                   (or files '()))))))))

(defun inline-review--github-normalize-comments (comments)
  "Convert GitHub COMMENTS list into the standard thread plist format."
  (mapcar
   (lambda (c)
     (let* ((path (alist-get 'path c))
            (line
             (or (alist-get 'line c) (alist-get 'original_line c)))
            (body (alist-get 'body c))
            (id (alist-get 'id c))
            (user (alist-get 'login (alist-get 'user c)))
            (created (alist-get 'created_at c))
            (outdated
             (and (null (alist-get 'line c))
                  (alist-get 'original_line c)))
            (note
             `((author . ((name . ,user)))
               (body . ,body)
               (created_at . ,created))))
       (list
        :path path
        :line line
        :thread (list note)
        :resolved nil
        :outdated outdated
        :note-id id)))
   (or comments '())))

(defun inline-review--github-post-comment
    (_beg end body on-success)
  "Post review comment on line at END with BODY (GitHub), then call ON-SUCCESS.
GitHub requires the PR head commit SHA for review comments."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid)
         (rel-path (inline-review--relative-file-path))
         (line (inline-review--line-number-at end)))
    ;; First fetch the PR head commit SHA
    (let ((pr-url
           (inline-review--github-api-url
            "repos" owner repo "pulls" (number-to-string pr-number))))
      (inline-review--github-http-request
       "GET" pr-url
       nil
       (lambda (pr-data)
         (let ((head-sha
                (alist-get 'sha (alist-get 'head pr-data))))
           (if (not head-sha)
               (message
                "inline-review: failed to get PR head commit")
             (let ((url
                    (inline-review--github-api-url
                     "repos"
                     owner
                     repo
                     "pulls"
                     (number-to-string pr-number)
                     "comments"))
                   (payload
                    `((body . ,body)
                      (path . ,rel-path)
                      (line . ,line)
                      (side . "RIGHT")
                      (commit_id . ,head-sha))))
               (inline-review--github-http-request
                "POST"
                url
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
                     "inline-review: failed to post comment"))))))))))))

(defun inline-review--github-update-comment
    (note-id body on-success)
  "Update NOTE-ID with BODY (GitHub), then call ON-SUCCESS."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (url
          (inline-review--github-api-url
           "repos"
           owner
           repo
           "pulls"
           "comments"
           (number-to-string note-id))))
    (inline-review--github-http-request
     "PATCH" url
     `((body . ,body))
     (lambda (resp)
       (if (and resp
                (alist-get 'id resp))
           (progn
             (message "inline-review: comment %d updated"
                      note-id)
             (funcall on-success))
         (message "inline-review: failed to update comment %d"
                  note-id))))))

(defun inline-review--github-resolve-comment
    (_note-id _note-body _on-success)
  "No-op resolve for GitHub (NOTE-ID, NOTE-BODY, ON-SUCCESS are unused).
GitHub's REST API does not expose an endpoint for resolving individual review
comments; resolution must be performed through the web interface.  This
function exists only to satisfy the backend contract."
  (message
   "inline-review: GitHub review comments are resolved via the web interface"))

(defun inline-review--github-reply-comment
    (note-id body on-success)
  "Post a reply to the review comment NOTE-ID with BODY (GitHub), then call ON-SUCCESS."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid)
         (url
          (inline-review--github-api-url
           "repos"
           owner
           repo
           "pulls"
           (number-to-string pr-number)
           "comments"
           (number-to-string note-id)
           "replies")))
    (inline-review--github-http-request
     "POST" url
     `((body . ,body))
     (lambda (resp)
       (if (and resp
                (alist-get 'id resp))
           (progn
             (message "inline-review: reply posted (id=%s)"
                      (alist-get 'id resp))
             (funcall on-success))
         (message "inline-review: failed to post reply"))))))

(defun inline-review--github-delete-comment (note-id on-success)
  "Delete review comment NOTE-ID (GitHub), then call ON-SUCCESS."
  (inline-review--github-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (url
          (inline-review--github-api-url
           "repos"
           owner
           repo
           "pulls"
           "comments"
           (number-to-string note-id))))
    (inline-review--github-http-request
     "DELETE" url
     nil
     (lambda (_resp)
       (message "inline-review: comment %d deleted" note-id)
       (funcall on-success)))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-github)

;;; inline-review-github.el ends here
