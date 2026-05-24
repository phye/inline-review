;;; inline-review-codeberg.el --- Codeberg backend for inline-review -*- lexical-binding: t; -*-

;; Author: phye
;; Keywords: tools, vc, review

;;; Commentary:
;;
;; Codeberg backend for inline-review.
;; Handles PR comment fetching, posting, updating, and deleting via the
;; Gitea REST API v1 (Codeberg runs Forgejo/Gitea).
;;
;; Authentication:
;;   Tokens are read exclusively from authinfo/netrc.  Add an entry to
;;   ~/.authinfo (or ~/.authinfo.gpg):
;;     machine codeberg.org login ^crm password <token>
;;   For self-hosted Gitea instances, use the host from
;;   `inline-review-codeberg-api-url'.
;;
;; HTTP layer: Emacs built-in `url-retrieve' with an Authorization: token
;; header.  The Gitea API v1 is wire-compatible with GitHub for review
;; comments, but we use url-retrieve directly to avoid any ghub assumptions.
;;
;; Backend contract:
;;   :fetch  (callback)                — calls (callback THREADS)
;;   :post   (beg end body on-success) — calls (on-success) on success
;;   :update (note-id body on-success) — calls (on-success) on success
;;   :resolve (ov on-success)          — no-op (Gitea has no resolve API)

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'cl-lib)
(require 'subr-x)
(require 'inline-review-backend)

;;;; ─── Codeberg Remote Parsing ────────────────────────────────────────────────

(defun inline-review--parse-codeberg-repo (remote-url)
  "Parse Codeberg/Gitea REMOTE-URL to get (owner . repo)."
  (when remote-url
    (cond
     ;; SSH: git@codeberg.org:owner/repo.git
     ((string-match
       "git@codeberg\\.org:\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; HTTPS: https://codeberg.org/owner/repo.git
     ((string-match
       "https?://codeberg\\.org/\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; Without .git suffix
     ((string-match
       "https?://codeberg\\.org/\\([^/]+\\)/\\([^/]+\\)/?$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; Self-hosted Gitea SSH
     ((string-match
       "git@[^:]+:\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url)))
     ;; Self-hosted Gitea HTTPS
     ((string-match
       "https?://[^/]+/\\([^/]+\\)/\\(.*\\)\\.git$" remote-url)
      (cons (match-string 1 remote-url) (match-string 2 remote-url))))))

;;;; ─── Codeberg HTTP Layer ────────────────────────────────────────────────────

(defun inline-review--codeberg-api-url (&rest path-segments)
  "Build a full Codeberg API URL by joining PATH-SEGMENTS onto the base URL."
  (concat
   inline-review-codeberg-api-url
   "/"
   (mapconcat #'identity path-segments "/")))

(defun inline-review--codeberg-http-status ()
  "Return the integer HTTP status from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
      (string-to-number (match-string 1)))))

(defun inline-review--codeberg-response-body ()
  "Return the response body string from the current url-retrieve buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^\\s-*$" nil t)
        (decode-coding-string
         (buffer-substring (point) (point-max)) 'utf-8)
      "")))

(defun inline-review--codeberg-parse-response ()
  "Parse JSON body from the current url-retrieve buffer."
  (let ((body (inline-review--codeberg-response-body)))
    (condition-case err
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string body))
      (error
       (message
        "inline-review[codeberg]: JSON parse error: %S\nbody: %s"
        err body)
       nil))))

(defun inline-review--codeberg-http-request
    (method url &optional payload callback)
  "Perform async HTTP METHOD request to Codeberg URL via url-retrieve.
PAYLOAD is an alist JSON-encoded as the request body.
CALLBACK receives the parsed JSON response (or nil on error).
The request is aborted after 30 seconds."
  (let* ((token
          (encode-coding-string
           (or (inline-review--get-token 'codeberg) "") 'utf-8))
         (url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(concat "token " token))
            ("Content-Type" . "application/json; charset=utf-8")))
         (url-request-data
          (when payload
            (let* ((body (json-encode payload))
                   (encoded (encode-coding-string body 'utf-8)))
              encoded)))
         (url-http-attempt-keepalives nil)
         (watchdog-timer nil)
         (buf
          (url-retrieve
           url
           (lambda (status)
             (when watchdog-timer
               (cancel-timer watchdog-timer))
             (let* ((http-status
                     (inline-review--codeberg-http-status))
                    (err (plist-get status :error))
                    (body (inline-review--codeberg-response-body)))
               (cond
                (err
                 (message
                  "inline-review[codeberg]: HTTP error %S (URL: %s)\n  body: %s"
                  err url (substring body 0 (min 400 (length body)))))
                ((and http-status (>= http-status 400))
                 (message
                  "inline-review[codeberg]: HTTP %d for %s\n  body: %s"
                  http-status url
                  (substring body 0 (min 400 (length body)))))
                (t
                 (when callback
                   (funcall
                    callback
                    (inline-review--codeberg-parse-response)))))))
           nil t)))
    (when buf
      (setq watchdog-timer
            (run-with-timer
             30 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (message
                  "inline-review[codeberg]: request timed out after %ds — %s"
                  30 url)
                 (kill-buffer buf))))))
    buf))

;;;; ─── Codeberg Backend Functions ─────────────────────────────────────────────

(defun inline-review--codeberg-ensure-project-info ()
  "Set project info from remote for Codeberg backend."
  (unless (alist-get 'owner inline-review--project-info)
    (let* ((remote (inline-review--git-remote-url))
           (parsed (inline-review--parse-codeberg-repo remote)))
      (if parsed
          (progn
            (message "inline-review: detected repo %s/%s"
                     (car parsed) (cdr parsed))
            (setq inline-review--project-info
                  `((owner . ,(car parsed)) (repo . ,(cdr parsed)))))
        (let ((owner (read-string "Codeberg owner/organization: "))
              (repo (read-string "Codeberg repository name: ")))
          (setq inline-review--project-info
                `((owner . ,owner) (repo . ,repo))))))))

(defun inline-review--codeberg-resolve-branches (callback)
  "Fetch PR source and target branch names, then call CALLBACK with them.
Calls (funcall CALLBACK SOURCE-BRANCH TARGET-BRANCH), both strings or nil.
Makes a single lightweight GET to the PR endpoint to retrieve head.ref
and base.ref.  Does not touch buffer-local branch variables — that is
the caller's responsibility."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid)
         (url
          (inline-review--codeberg-api-url
           "repos" owner repo "pulls" (number-to-string pr-number))))
    (inline-review--codeberg-http-request
     "GET" url nil
     (lambda (pr)
       (funcall callback
                (and pr (alist-get 'ref (alist-get 'head pr)))
                (and pr (alist-get 'ref (alist-get 'base pr))))))))

(defun inline-review--codeberg-fetch-comments (callback)
  "Fetch PR comments and call CALLBACK with a list of thread plists (Codeberg)."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid))
    (message "inline-review: fetching comments for PR #%d ..."
             pr-number)
    (let ((url
           (inline-review--codeberg-api-url
            "repos" owner repo "pulls"
            (number-to-string pr-number) "comments")))
      (inline-review--codeberg-http-request
       "GET" url nil
       (lambda (comments)
         (funcall callback
                  (inline-review--codeberg-normalize-comments
                   comments)))))))

(defun inline-review--codeberg-fetch-diff (callback)
  "Fetch PR changed files and call CALLBACK with a list of change plists (Codeberg).
Each plist has :old-path, :new-path, and :patch (unified diff string)."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid))
    (message "inline-review: fetching diff for PR #%d ..."
             pr-number)
    (let ((url
           (inline-review--codeberg-api-url
            "repos" owner repo "pulls"
            (number-to-string pr-number) "files")))
      (inline-review--codeberg-http-request
       "GET" url nil
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

(defun inline-review--codeberg-normalize-comments (comments)
  "Convert Codeberg COMMENTS list into the standard thread plist format."
  (mapcar
   (lambda (c)
     (let* ((path (alist-get 'path c))
            (line (or (alist-get 'line c)
                      (alist-get 'original_line c)))
            (body (alist-get 'body c))
            (id (alist-get 'id c))
            (user (alist-get 'login (alist-get 'user c)))
            (created (alist-get 'created_at c))
            (outdated (and (null (alist-get 'line c))
                           (alist-get 'original_line c)))
            (note `((author . ((name . ,user)))
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

(defun inline-review--codeberg-post-comment
    (_beg end body on-success)
  "Post review comment on line at END with BODY (Codeberg), then call ON-SUCCESS.
Codeberg requires the PR head commit SHA for review comments."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid)
         (rel-path (inline-review--relative-file-path))
         (line (inline-review--line-number-at end)))
    ;; First fetch the PR head commit SHA
    (let ((pr-url
           (inline-review--codeberg-api-url
            "repos" owner repo "pulls" (number-to-string pr-number))))
      (inline-review--codeberg-http-request
       "GET" pr-url nil
       (lambda (pr-data)
         (let ((head-sha (alist-get 'sha (alist-get 'head pr-data))))
           (if (not head-sha)
               (message
                "inline-review: failed to get PR head commit")
             (let ((url
                    (inline-review--codeberg-api-url
                     "repos" owner repo "pulls"
                     (number-to-string pr-number) "comments"))
                   (payload
                    `((body . ,body)
                      (path . ,rel-path)
                      (line . ,line)
                      (side . "RIGHT")
                      (commit_id . ,head-sha))))
               (inline-review--codeberg-http-request
                "POST" url payload
                (lambda (resp)
                  (if (and resp (alist-get 'id resp))
                      (progn
                        (message
                         "inline-review: comment posted (id=%s)"
                         (alist-get 'id resp))
                        (funcall on-success))
                    (message
                     "inline-review: failed to post comment"))))))))))))

(defun inline-review--codeberg-update-comment
    (note-id body on-success)
  "Update NOTE-ID with BODY (Codeberg), then call ON-SUCCESS."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (url
          (inline-review--codeberg-api-url
           "repos" owner repo "pulls" "comments"
           (number-to-string note-id))))
    (inline-review--codeberg-http-request
     "PATCH" url
     `((body . ,body))
     (lambda (resp)
       (if (and resp (alist-get 'id resp))
           (progn
             (message "inline-review: comment %d updated" note-id)
             (funcall on-success))
         (message "inline-review: failed to update comment %d"
                  note-id))))))

(defun inline-review--codeberg-resolve-comment
    (_note-id _note-body _on-success)
  "No-op resolve for Codeberg.
Gitea/Codeberg does not expose an endpoint for resolving individual review
comments via the REST API.  This function exists only to satisfy the backend
contract."
  (message
   "inline-review: Codeberg review comments are resolved via the web interface"))

(defun inline-review--codeberg-reply-comment
    (note-id body on-success)
  "Post a reply to the review comment NOTE-ID with BODY (Codeberg), then call ON-SUCCESS."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (pr-number inline-review--mr-iid)
         (url
          (inline-review--codeberg-api-url
           "repos" owner repo "pulls"
           (number-to-string pr-number) "comments"
           (number-to-string note-id) "replies")))
    (inline-review--codeberg-http-request
     "POST" url
     `((body . ,body))
     (lambda (resp)
       (if (and resp (alist-get 'id resp))
           (progn
             (message "inline-review: reply posted (id=%s)"
                      (alist-get 'id resp))
             (funcall on-success))
         (message "inline-review: failed to post reply"))))))

(defun inline-review--codeberg-delete-comment (note-id on-success)
  "Delete review comment NOTE-ID (Codeberg), then call ON-SUCCESS."
  (inline-review--codeberg-ensure-project-info)
  (let* ((owner (alist-get 'owner inline-review--project-info))
         (repo (alist-get 'repo inline-review--project-info))
         (url
          (inline-review--codeberg-api-url
           "repos" owner repo "pulls" "comments"
           (number-to-string note-id))))
    (inline-review--codeberg-http-request
     "DELETE" url nil
     (lambda (_resp)
       (message "inline-review: comment %d deleted" note-id)
       (funcall on-success)))))

;;;; ─── Provide ────────────────────────────────────────────────────────────────

(provide 'inline-review-codeberg)

;;; inline-review-codeberg.el ends here
