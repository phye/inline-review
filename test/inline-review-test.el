;;; inline-review-test.el --- Tests for inline-review -*- lexical-binding: t; -*-

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; ERT test suite for inline-review.  Run with:
;;   M-x ert RET t RET
;; or from the command line:
;;   emacs -batch --eval "(add-to-list 'load-path \".\")" -l ert -l test/inline-review-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

;; ── Mock external dependencies ──────────────────────────────────────────────

;; ghub is an external dependency not available in vanilla Emacs.
;; Provide a minimal stub so the GitHub backend loads without error.
(defmacro ghub-request (&rest _args)
  "Stub for `ghub-request'.")

(provide 'ghub)

;; url-http is sometimes needed by url-retrieve based backends;
;; ensure it does not block test loading.
(unless (featurep 'url-http)
  (require 'url-http nil t))

;; ── Load the package ────────────────────────────────────────────────────────

(add-to-list 'load-path
             (expand-file-name ".."
                               (file-name-directory (or load-file-name buffer-file-name))))

(require 'inline-review-custom)
(require 'inline-review-backend)
(require 'inline-review-branch)
(require 'inline-review-diff)
(require 'inline-review-comment)
(require 'inline-review-github)
(require 'inline-review-gitlab)
(require 'inline-review-gongfeng)
(require 'inline-review-codeberg)
(require 'inline-review)

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Utility helpers
;;;; ───────────────────────────────────────────────────────────────────────────

(defmacro crm-test--with-backend-state (&rest body)
  "Evaluate BODY with a clean backend registry and cache state."
  `(let ((inline-review-backend-registry
          (copy-tree inline-review-backend-registry))
         (inline-review--diff-cache (make-hash-table :test 'equal))
         (inline-review--iid-cache (make-hash-table :test 'equal))
         (inline-review--backend-cache (make-hash-table :test 'equal)))
     ,@body))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Backend registry
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-backend-prop-github ()
  "Test retrieving a property from the github backend."
  (should (eq (inline-review--backend-prop 'github :api-url-var)
              'inline-review-github-api-url))
  (should (eq (inline-review--backend-prop 'github :fetch)
              'inline-review--github-fetch-comments)))

(ert-deftest crm-backend-prop-gitlab ()
  "Test retrieving a property from the gitlab backend."
  (should (eq (inline-review--backend-prop 'gitlab :api-url-var)
              'inline-review-gitlab-api-url)))

(ert-deftest crm-backend-prop-gongfeng ()
  "Test retrieving a property from the gongfeng backend."
  (should (eq (inline-review--backend-prop 'gongfeng :api-url-var)
              'inline-review-gongfeng-api-url)))

(ert-deftest crm-backend-prop-codeberg ()
  "Test retrieving a property from the codeberg backend."
  (should (eq (inline-review--backend-prop 'codeberg :api-url-var)
              'inline-review-codeberg-api-url))
  (should (eq (inline-review--backend-prop 'codeberg :fetch)
              'inline-review--codeberg-fetch-comments)))

(ert-deftest crm-backend-prop-unknown ()
  "Test that an unknown backend signals an error."
  (should-error (inline-review--backend-prop 'unknown :fetch)))

(ert-deftest crm-register-backend ()
  "Test registering a new backend."
  (crm-test--with-backend-state
   (inline-review-register-backend
    'testbackend
    :api-url-var 'test-api-url
    :remote-re "test\\.com"
    :fetch 'test-fetch)
   (should (eq (inline-review--backend-prop 'testbackend :api-url-var)
               'test-api-url))
   (should (eq (inline-review--backend-prop 'testbackend :fetch)
               'test-fetch))
   ;; Replace existing
   (inline-review-register-backend
    'testbackend
    :api-url-var 'test-api-url-2
    :remote-re "test\\.com"
    :fetch 'test-fetch-2)
   (should (eq (inline-review--backend-prop 'testbackend :api-url-var)
               'test-api-url-2))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Backend detection
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-detect-backend-github-ssh ()
  "Test auto-detection of github from SSH remote URL."
  (should (eq (inline-review--detect-backend "git@github.com:foo/bar.git")
              'github)))

(ert-deftest crm-detect-backend-github-https ()
  "Test auto-detection of github from HTTPS remote URL."
  (should (eq (inline-review--detect-backend "https://github.com/foo/bar.git")
              'github)))

(ert-deftest crm-detect-backend-gitlab ()
  "Test auto-detection of gitlab from remote URL."
  (should (eq (inline-review--detect-backend "https://gitlab.com/ns/project.git")
              'gitlab)))

(ert-deftest crm-detect-backend-gongfeng ()
  "Test auto-detection of gongfeng from remote URL."
  (should (eq (inline-review--detect-backend "https://git.woa.com/ns/project.git")
              'gongfeng))
  (should (eq (inline-review--detect-backend "https://code.tencent.com/ns/project.git")
              'gongfeng)))

(ert-deftest crm-detect-backend-codeberg ()
  "Test auto-detection of codeberg from remote URL."
  (should (eq (inline-review--detect-backend "https://codeberg.org/owner/repo.git")
              'codeberg))
  (should (eq (inline-review--detect-backend "git@codeberg.org:owner/repo.git")
              'codeberg)))

(ert-deftest crm-detect-backend-unknown ()
  "Test that unknown remotes return nil."
  (should (null (inline-review--detect-backend "https://bitbucket.org/foo/bar.git"))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  URL parsing
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-parse-mr-url-github ()
  "Test parsing a GitHub PR URL."
  (let ((result (inline-review--parse-mr-url
                 "https://github.com/owner/repo/pull/42")))
    (should (equal (plist-get result :iid) 42))
    (should (eq (plist-get result :backend) 'github))
    (should (equal (alist-get 'owner (plist-get result :project-info)) "owner"))
    (should (equal (alist-get 'repo (plist-get result :project-info)) "repo"))))

(ert-deftest crm-parse-mr-url-github-pulls ()
  "Test parsing a GitHub PR URL with 'pulls' plural."
  (let ((result (inline-review--parse-mr-url
                 "https://github.com/owner/repo/pulls/7")))
    (should (equal (plist-get result :iid) 7))
    (should (eq (plist-get result :backend) 'github))))

(ert-deftest crm-parse-mr-url-gitlab ()
  "Test parsing a GitLab MR URL."
  (let ((result (inline-review--parse-mr-url
                 "https://gitlab.com/ns/project/-/merge_requests/7")))
    (should (equal (plist-get result :iid) 7))
    (should (eq (plist-get result :backend) 'gitlab))
    (should (equal (alist-get 'project-id (plist-get result :project-info))
                   "ns%2Fproject"))))

(ert-deftest crm-parse-mr-url-gongfeng ()
  "Test parsing a Gongfeng MR URL."
  (let ((result (inline-review--parse-mr-url
                 "https://git.woa.com/ns/project/-/merge_requests/856")))
    (should (equal (plist-get result :iid) 856))
    (should (eq (plist-get result :backend) 'gongfeng))
    (should (equal (alist-get 'project-id (plist-get result :project-info))
                   "ns%2Fproject"))))

(ert-deftest crm-parse-mr-url-codeberg ()
  "Test parsing a Codeberg PR URL."
  (let ((result (inline-review--parse-mr-url
                 "https://codeberg.org/owner/repo/pulls/42")))
    (should (equal (plist-get result :iid) 42))
    (should (eq (plist-get result :backend) 'codeberg))
    (should (equal (alist-get 'owner (plist-get result :project-info)) "owner"))
    (should (equal (alist-get 'repo (plist-get result :project-info)) "repo"))))

(ert-deftest crm-parse-mr-url-bare-integer ()
  "Test parsing a bare integer (IID only)."
  (let ((result (inline-review--parse-mr-url "856")))
    (should (equal (plist-get result :iid) 856))
    (should (null (plist-get result :backend)))))

(ert-deftest crm-parse-mr-url-nil ()
  "Test parsing nil returns nil."
  (should (null (inline-review--parse-mr-url nil))))

(ert-deftest crm-parse-mr-url-empty ()
  "Test parsing an empty string returns nil."
  (should (null (inline-review--parse-mr-url "")))
  (should (null (inline-review--parse-mr-url "   "))))

(ert-deftest crm-parse-mr-url-invalid ()
  "Test parsing an invalid URL returns nil."
  (should (null (inline-review--parse-mr-url "not-a-url"))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  GitHub repo parsing
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-parse-github-repo-ssh ()
  "Test parsing GitHub SSH remote."
  (let ((result (inline-review--parse-github-repo
                 "git@github.com:foo/bar.git")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest crm-parse-github-repo-https ()
  "Test parsing GitHub HTTPS remote."
  (let ((result (inline-review--parse-github-repo
                 "https://github.com/foo/bar.git")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest crm-parse-github-repo-no-git-suffix ()
  "Test parsing GitHub remote without .git suffix."
  (let ((result (inline-review--parse-github-repo
                 "https://github.com/foo/bar")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest crm-parse-github-repo-enterprise-ssh ()
  "Test parsing GitHub Enterprise SSH remote."
  (let ((result (inline-review--parse-github-repo
                 "git@ghe.example.com:org/repo.git")))
    (should (equal (car result) "org"))
    (should (equal (cdr result) "repo"))))

(ert-deftest crm-parse-github-repo-nil ()
  "Test parsing nil GitHub remote."
  (should (null (inline-review--parse-github-repo nil))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Codeberg repo parsing
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-parse-codeberg-repo-ssh ()
  "Test parsing Codeberg SSH remote."
  (let ((result (inline-review--parse-codeberg-repo
                 "git@codeberg.org:foo/bar.git")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest crm-parse-codeberg-repo-https ()
  "Test parsing Codeberg HTTPS remote."
  (let ((result (inline-review--parse-codeberg-repo
                 "https://codeberg.org/foo/bar.git")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest crm-parse-codeberg-repo-no-git-suffix ()
  "Test parsing Codeberg remote without .git suffix."
  (let ((result (inline-review--parse-codeberg-repo
                 "https://codeberg.org/foo/bar")))
    (should (equal (car result) "foo"))
    (should (equal (cdr result) "bar"))))

(ert-deftest crm-parse-codeberg-repo-nil ()
  "Test parsing nil Codeberg remote."
  (should (null (inline-review--parse-codeberg-repo nil))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Diff patch parsing
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-parse-patch-empty ()
  "Test parsing an empty patch."
  (let ((result (inline-review--parse-patch "")))
    (should (null result))))

(ert-deftest crm-parse-patch-simple-add ()
  "Test parsing a simple addition hunk."
  (let* ((patch "@@ -1,3 +1,4 @@
 context
+added line
 more context
 another context")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 1))
    (let ((hunk (car result)))
      (should (= (plist-get hunk :new-start) 1))
      (should (= (plist-get hunk :new-count) 4))
      (should (equal (plist-get hunk :added-lines) '(2)))
      (should (null (plist-get hunk :removed-segments))))))

(ert-deftest crm-parse-patch-simple-remove ()
  "Test parsing a simple removal hunk."
  (let* ((patch "@@ -1,4 +1,3 @@
 context
-removed line
 more context
 another context")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 1))
    (let ((hunk (car result)))
      (should (= (plist-get hunk :new-start) 1))
      (should (= (plist-get hunk :new-count) 3))
      (should (null (plist-get hunk :added-lines)))
      (let ((removed (plist-get hunk :removed-segments)))
        (should (= (length removed) 1))
        (should (= (caar removed) 1))
        (should (equal (cdar removed) "removed line"))))))

(ert-deftest crm-parse-patch-replace ()
  "Test parsing a hunk with removed then added lines."
  (let* ((patch "@@ -2,3 +2,3 @@
 context
-old line
+new line
 more context")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 1))
    (let ((hunk (car result)))
      (should (= (plist-get hunk :new-start) 2))
      (should (= (plist-get hunk :new-count) 3))
      (should (equal (plist-get hunk :added-lines) '(3)))
      (let ((removed (plist-get hunk :removed-segments)))
        (should (= (length removed) 1))
        (should (= (caar removed) 2))
        (should (equal (cdar removed) "old line"))))))

(ert-deftest crm-parse-patch-multi-hunk ()
  "Test parsing multiple hunks."
  (let* ((patch "@@ -1,2 +1,2 @@
 first
-removed1
+added1
@@ -10,3 +10,3 @@
 second
-old
+new
 context")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 2))
    (let ((h1 (car result))
          (h2 (cadr result)))
      (should (= (plist-get h1 :new-start) 1))
      (should (= (plist-get h2 :new-start) 10)))))

(ert-deftest crm-parse-patch-removed-at-start ()
  "Test parsing a hunk with removed lines before any added/context."
  (let* ((patch "@@ -1,3 +1,2 @@
-removed1
-removed2
 context")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 1))
    (let ((hunk (car result)))
      (should (= (plist-get hunk :new-start) 1))
      (let ((removed (plist-get hunk :removed-segments)))
        (should (= (length removed) 1))
        (should (= (caar removed) 0))
        (should (equal (cdar removed) "removed1\nremoved2"))))))

(ert-deftest crm-parse-patch-removed-at-end ()
  "Test parsing a hunk with removed lines at the end."
  (let* ((patch "@@ -1,3 +1,2 @@
 context
-removed1
-removed2")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 1))
    (let ((hunk (car result)))
      (let ((removed (plist-get hunk :removed-segments)))
        (should (= (length removed) 1))
        (should (= (caar removed) 1))
        (should (equal (cdar removed) "removed1\nremoved2"))))))

(ert-deftest crm-parse-patch-no-newline-marker ()
  "Test that \\ No newline at end marker is handled."
  (let* ((patch "@@ -1,2 +1,3 @@
 context
+added
 \\ No newline at end of file")
         (result (inline-review--parse-patch patch)))
    (should (= (length result) 1))
    (let ((hunk (car result)))
      (should (= (plist-get hunk :new-count) 3))
      (should (equal (plist-get hunk :added-lines) '(2))))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Diff file lookup
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-find-patch-for-file ()
  "Test finding a patch by relative path."
  (let ((changes '((:old-path "a.txt" :new-path "a.txt" :patch "@@ -1 +1 @@")
                   (:old-path "b.txt" :new-path "c.txt" :patch "@@ -2 +2 @@"))))
    (should (string-match "@@ -1" (inline-review--find-patch-for-file
                                    changes "a.txt")))
    (should (string-match "@@ -2" (inline-review--find-patch-for-file
                                    changes "b.txt")))
    (should (string-match "@@ -2" (inline-review--find-patch-for-file
                                    changes "c.txt")))
    (should (null (inline-review--find-patch-for-file
                   changes "d.txt")))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Removed lines helpers
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-format-removed ()
  "Test formatting removed lines."
  (let ((result (inline-review--format-removed '("c" "b" "a"))))
    (should (equal result "a\nb\nc"))))

(ert-deftest crm-truncate-removed-lines-within-limit ()
  "Test that lines within limit are not truncated."
  (let* ((inline-review-inline-removed-lines-limit 5)
         (result (inline-review--truncate-removed-lines
                  '("a" "b" "c"))))
    (should (= (length result) 3))
    (should (equal result '("a" "b" "c")))))

(ert-deftest crm-truncate-removed-lines-exceeds-limit ()
  "Test that lines exceeding limit are truncated."
  (let* ((inline-review-inline-removed-lines-limit 2)
         (result (inline-review--truncate-removed-lines
                  '("a" "b" "c" "d" "e"))))
    (should (= (length result) 3))
    (should (equal (nth 0 result) "a"))
    (should (equal (nth 1 result) "b"))
    (should (string-match-p "3 more lines" (nth 2 result)))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Diff cache key
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-diff-cache-key ()
  "Test diff cache key generation."
  (let ((key (inline-review--diff-cache-key 'github 42 '((owner . "foo")))))
    (should (listp key))
    (should (eq (nth 0 key) 'github))
    (should (= (nth 1 key) 42))
    (should (equal (nth 2 key) '((owner . "foo"))))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Note rendering
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-render-note-basic ()
  "Test rendering a basic note."
  (let* ((note '((author . ((name . "Alice")))
                 (body . "Looks good")
                 (created_at . "2024-01-01")))
         (result (inline-review--render-note note t nil nil)))
    (should (string-match-p "Alice" result))
    (should (string-match-p "Looks good" result))
    (should (string-match-p "2024-01-01" result))))

(ert-deftest crm-render-note-resolved ()
  "Test rendering a resolved note shows resolved status."
  (let* ((note '((author . ((name . "Bob")))
                 (body . "Fixed")))
         (result (inline-review--render-note note t t nil)))
    (should (string-match-p "resolved" result))))

(ert-deftest crm-render-note-outdated ()
  "Test rendering an outdated note shows outdated status."
  (let* ((note '((author . ((name . "Carol")))
                 (body . "Old comment")))
         (result (inline-review--render-note note t nil t)))
    (should (string-match-p "outdated" result))))

(ert-deftest crm-render-note-multiline ()
  "Test rendering a multiline note body."
  (let* ((note '((author . ((name . "Dave")))
                 (body . "line1\nline2\nline3")))
         (result (inline-review--render-note note t nil nil)))
    (should (string-match-p "│ line1" result))
    (should (string-match-p "│ line2" result))
    (should (string-match-p "│ line3" result))))

(ert-deftest crm-render-note-not-first ()
  "Test rendering a reply note (not first in thread)."
  (let* ((note '((author . ((name . "Eve")))
                 (body . "Reply")))
         (result (inline-review--render-note note nil nil nil)))
    ;; No status indicator for non-first notes
    (should (not (string-match-p "open" result)))
    (should (not (string-match-p "resolved" result)))))

(ert-deftest crm-render-note-unknown-author ()
  "Test rendering a note with missing author."
  (let* ((note '((body . "No author")))
         (result (inline-review--render-note note t nil nil)))
    (should (string-match-p "unknown" result))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Overlay helpers
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-overlay-at-point-found ()
  "Test finding an overlay at point."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (let ((ov (make-overlay (line-beginning-position 2)
                            (1+ (line-end-position 2)))))
      (overlay-put ov 'inline-review t)
      (overlay-put ov 'inline-review-note-id 42)
      (goto-char (line-beginning-position 2))
      (let ((found (inline-review--overlay-at-point)))
        (should found)
        (should (= (overlay-get found 'inline-review-note-id) 42))))))

(ert-deftest crm-overlay-at-point-not-found ()
  "Test that no overlay is found when there isn't one."
  (with-temp-buffer
    (insert "line1\nline2")
    (goto-char (line-beginning-position 1))
    (should (null (inline-review--overlay-at-point)))))

(ert-deftest crm-sorted-overlay-positions ()
  "Test getting sorted overlay positions."
  (with-temp-buffer
    (insert "line1\nline2\nline3\nline4")
    (let ((inline-review--overlays nil)
          (ov1 (make-overlay (line-beginning-position 1)
                             (line-end-position 1)))
          (ov2 (make-overlay (line-beginning-position 3)
                             (line-end-position 3)))
          (ov3 (make-overlay (line-beginning-position 2)
                             (line-end-position 2))))
      (overlay-put ov1 'inline-review t)
      (overlay-put ov2 'inline-review t)
      (overlay-put ov3 'inline-review t)
      (push ov1 inline-review--overlays)
      (push ov2 inline-review--overlays)
      (push ov3 inline-review--overlays)
      (let ((positions (inline-review--sorted-overlay-positions)))
        (should (= (length positions) 3))
        (should (apply #'<= positions)))
      ;; Clean up
      (inline-review--clear-overlays)
      (should (null inline-review--overlays)))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Comment thread rendering
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-render-comment-threads-empty ()
  "Test rendering with no threads."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((inline-review--overlays nil))
      (inline-review--render-comment-threads
       (current-buffer) "test.el" nil)
      (should (null inline-review--overlays)))))

(ert-deftest crm-render-comment-threads-mismatch-path ()
  "Test that threads for different paths are not rendered."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((inline-review--overlays nil)
          (threads '((:path "other.el" :line 1 :thread (((body . "note"))) :resolved nil :note-id 1))))
      (inline-review--render-comment-threads
       (current-buffer) "test.el" threads)
      (should (null inline-review--overlays)))))

(ert-deftest crm-render-comment-threads-matching-path ()
  "Test that threads for matching paths are rendered."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((inline-review--overlays nil)
          (threads '((:path "test.el" :line 1 :thread (((body . "note"))) :resolved nil :note-id 1))))
      (inline-review--render-comment-threads
       (current-buffer) "test.el" threads)
      (should (= (length inline-review--overlays) 1)))))

(ert-deftest crm-render-comment-threads-hide-resolved ()
  "Test that resolved threads are hidden when configured."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((inline-review--overlays nil)
          (inline-review-hide-resolved t)
          (threads '((:path "test.el" :line 1 :thread (((body . "note"))) :resolved t :note-id 1))))
      (inline-review--render-comment-threads
       (current-buffer) "test.el" threads)
      (should (null inline-review--overlays)))))

(ert-deftest crm-render-comment-threads-show-resolved ()
  "Test that resolved threads are shown when not hidden."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((inline-review--overlays nil)
          (inline-review-hide-resolved nil)
          (threads '((:path "test.el" :line 1 :thread (((body . "note"))) :resolved t :note-id 1))))
      (inline-review--render-comment-threads
       (current-buffer) "test.el" threads)
      (should (= (length inline-review--overlays) 1)))))

(ert-deftest crm-render-comment-threads-outdated ()
  "Test rendering of outdated threads."
  (with-temp-buffer
    (insert "line1\nline2")
    (let ((inline-review--overlays nil)
          (threads '((:path "test.el" :line 1 :thread (((body . "note")))
                      :resolved nil :note-id 1 :outdated t))))
      (inline-review--render-comment-threads
       (current-buffer) "test.el" threads)
      (should (= (length inline-review--overlays) 1))
      (should (overlay-get (car inline-review--overlays)
                           'inline-review-body))
      (inline-review--clear-overlays))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Hunk navigation helpers
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-current-hunk-key ()
  "Test current hunk key generation."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (forward-line 1)
    (let ((key (inline-review--current-hunk-key)))
      (should (consp key))
      (should (equal (car key) default-directory))
      (should (= (cdr key) 2)))))

(ert-deftest crm-all-hunk-positions-no-cache ()
  "Test hunk positions when no diff is cached."
  (with-temp-buffer
    (let ((inline-review--diff-cache (make-hash-table :test 'equal))
          (inline-review--current-backend nil)
          (inline-review--mr-iid nil)
          (inline-review--project-info nil))
      (should (null (inline-review--all-hunk-positions))))))

(ert-deftest crm-all-hunk-positions-with-cache ()
  "Test hunk positions from cached diff data."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (let* ((inline-review--diff-cache (make-hash-table :test 'equal))
           (inline-review--current-backend 'github)
           (inline-review--mr-iid 1)
           (inline-review--project-info '((owner . "foo") (repo . "bar")))
           (key (inline-review--diff-cache-key
                 'github 1 inline-review--project-info))
           (patch "@@ -1,2 +1,3 @@
 line1
+added
 line2")
           (changes `((:old-path "test.txt" :new-path "test.txt" :patch ,patch))))
      (puthash key changes inline-review--diff-cache)
      (let ((result (inline-review--all-hunk-positions)))
        (should result)
        (should (= (length result) 1))
        (should (= (cdar result) 1))))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Utility helpers
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-line-number-at ()
  "Test line-number-at helper."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (should (= (inline-review--line-number-at (point-min)) 1))
    (should (= (inline-review--line-number-at (1- (point-max))) 3))))

(ert-deftest crm-line-end-pos ()
  "Test line-end-pos helper."
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (goto-char (point-min))
    (should (= (inline-review--line-end-pos 1) (line-end-position)))
    (forward-line 1)
    (should (= (inline-review--line-end-pos 2) (line-end-position)))
    (forward-line 1)
    (should (= (inline-review--line-end-pos 3) (line-end-position)))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Finish review cleanup
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-finish-review-clears-state ()
  "Test that finish-review clears per-project state.
`default-directory' is bound to /tmp so `inline-review--git-root'
returns nil — the hash key for a buffer with no git repo."
  (with-temp-buffer
    ;; Isolate from whatever git repo `make test' runs inside.
    (let ((default-directory "/tmp/")
          (inline-review--diff-cache (make-hash-table :test 'equal))
          (inline-review--iid-cache (make-hash-table :test 'equal))
          (inline-review--backend-cache (make-hash-table :test 'equal))
          (inline-review--review-active-cache (make-hash-table :test 'equal)))
      ;; Set buffer-local MR state so finish-review can build the diff-cache key.
      (setq inline-review--current-backend 'github)
      (setq inline-review--mr-iid 42)
      (setq inline-review--project-info nil)
      ;; With default-directory=/tmp/ and no buffer-file-name, git-root returns nil.
      ;; iid/backend/review-active caches use nil key; diff-cache uses (list 'github 42 nil).
      (puthash nil 42 inline-review--iid-cache)
      (puthash nil 'github inline-review--backend-cache)
      (puthash nil 42 inline-review--review-active-cache)
      (puthash (inline-review--diff-cache-key 'github 42 nil)
               'value inline-review--diff-cache)
      ;; Call finish-review
      (inline-review-finish-review)
      ;; Verify per-project entries are cleared
      (should (= (hash-table-count inline-review--diff-cache) 0))
      (should (= (hash-table-count inline-review--iid-cache) 0))
      (should (= (hash-table-count inline-review--backend-cache) 0))
      (should (= (hash-table-count inline-review--review-active-cache) 0)))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Toggle hide-resolved
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-toggle-hide-resolved ()
  "Test toggling hide-resolved flag."
  (let ((inline-review-hide-resolved nil)
        (inline-review-mode nil)
        (inline-review--mr-iid nil))
    (inline-review-toggle-hide-resolved)
    (should (eq inline-review-hide-resolved t))
    (inline-review-toggle-hide-resolved)
    (should (eq inline-review-hide-resolved nil))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Backend host derivation
;;;; ───────────────────────────────────────────────────────────────────────────

(ert-deftest crm-backend-host-github ()
  "Test host extraction for GitHub."
  (let ((inline-review-github-api-url "https://api.github.com"))
    (should (equal (inline-review--backend-host 'github)
                   "api.github.com"))))

(ert-deftest crm-backend-host-gongfeng ()
  "Test host extraction for Gongfeng."
  (let ((inline-review-gongfeng-api-url "https://git.woa.com/api/v3"))
    (should (equal (inline-review--backend-host 'gongfeng)
                   "git.woa.com"))))

(ert-deftest crm-backend-host-enterprise ()
  "Test host extraction for GitHub Enterprise."
  (let ((inline-review-github-api-url "https://ghe.example.com/api/v3"))
    (should (equal (inline-review--backend-host 'github)
                   "ghe.example.com"))))

;;;; ───────────────────────────────────────────────────────────────────────────
;;;;  Thread navigation
;;;; ───────────────────────────────────────────────────────────────────────────

(defun crm-test--line-beg (n)
  "Return position at beginning of line N (1-based) in current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (point)))

(defun crm-test--line-end (n)
  "Return position at end of line N (1-based) in current buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- n))
    (line-end-position)))

(ert-deftest crm-all-thread-positions-empty ()
  "Test thread positions when no overlays exist."
  (should (null (inline-review--all-thread-positions))))

(ert-deftest crm-all-thread-positions-single-buffer ()
  "Test collecting thread positions from a single buffer."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/crm-test-single.el")
    (insert "line1\nline2\nline3")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    (let ((ov (make-overlay (crm-test--line-beg 2)
                            (crm-test--line-end 2))))
      (overlay-put ov 'inline-review t)
      (push ov inline-review--overlays))
    (let ((result (inline-review--all-thread-positions)))
      (should (= (length result) 1))
      (should (= (cdar result) 2))
      (inline-review--clear-overlays))))

(ert-deftest crm-all-thread-positions-multi-buffer ()
  "Test collecting thread positions across multiple buffers."
  (let ((buf1 (generate-new-buffer " *crm-test-1*"))
        (buf2 (generate-new-buffer " *crm-test-2*")))
    (unwind-protect
        (progn
          ;; Buffer 1: overlay on line 1
          (with-current-buffer buf1
            (setq buffer-file-name "/tmp/crm-test-multi-1.el")
            (insert "a\nb\nc")
            (setq inline-review-mode t)
            (setq inline-review--overlays nil)
            (let ((ov (make-overlay (crm-test--line-beg 1)
                                    (crm-test--line-end 1))))
              (overlay-put ov 'inline-review t)
              (push ov inline-review--overlays)))
          ;; Buffer 2: overlay on line 3
          (with-current-buffer buf2
            (setq buffer-file-name "/tmp/crm-test-multi-2.el")
            (insert "x\ny\nz")
            (setq inline-review-mode t)
            (setq inline-review--overlays nil)
            (let ((ov (make-overlay (crm-test--line-beg 3)
                                    (crm-test--line-end 3))))
              (overlay-put ov 'inline-review t)
              (push ov inline-review--overlays)))
          ;; Collect from buf1 (git-root is nil for /tmp → permissive filter)
          (let ((result (with-current-buffer buf1
                          (inline-review--all-thread-positions))))
            (should (= (length result) 2))
            (should (= (cdr (car result)) 1))
            (should (= (cdr (cadr result)) 3))))
      ;; Cleanup
      (with-current-buffer buf1
        (inline-review--clear-overlays)
        (setq inline-review-mode nil))
      (with-current-buffer buf2
        (inline-review--clear-overlays)
        (setq inline-review-mode nil))
      (kill-buffer buf1)
      (kill-buffer buf2))))

(ert-deftest crm-all-thread-positions-dedup ()
  "Test that multiple overlays on the same line are deduplicated."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/crm-test-dedup.el")
    (insert "line1\nline2\nline3")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    ;; Two overlays on the same line
    (dotimes (_ 2)
      (let ((ov (make-overlay (crm-test--line-beg 2)
                              (crm-test--line-end 2))))
        (overlay-put ov 'inline-review t)
        (push ov inline-review--overlays)))
    (let ((result (inline-review--all-thread-positions)))
      (should (= (length result) 1))
      (should (= (cdar result) 2))
      (inline-review--clear-overlays))))

(ert-deftest crm-next-thread ()
  "Test jumping to the next comment thread."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/crm-test-next-thread.el")
    (insert "line1\nline2\nline3")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    ;; git-root is nil for /tmp; seed review-active-cache so the guard passes.
    (let ((inline-review--review-active-cache (make-hash-table :test 'equal)))
      (puthash nil 42 inline-review--review-active-cache)
      (let ((ov (make-overlay (crm-test--line-beg 3)
                              (crm-test--line-end 3))))
        (overlay-put ov 'inline-review t)
        (push ov inline-review--overlays))
      ;; Point on line 1, next thread should be line 3
      (goto-char (point-min))
      (inline-review-next-thread)
      (should (= (line-number-at-pos) 3))
      (inline-review--clear-overlays))))

(ert-deftest crm-previous-thread ()
  "Test jumping to the previous comment thread."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/crm-test-prev-thread.el")
    (insert "line1\nline2\nline3")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    ;; git-root is nil for /tmp; seed review-active-cache so the guard passes.
    (let ((inline-review--review-active-cache (make-hash-table :test 'equal)))
      (puthash nil 42 inline-review--review-active-cache)
      (let ((ov (make-overlay (crm-test--line-beg 1)
                              (crm-test--line-end 1))))
        (overlay-put ov 'inline-review t)
        (push ov inline-review--overlays))
      ;; Point on line 3, previous thread should be line 1
      (goto-char (point-max))
      (inline-review-previous-thread)
      (should (= (line-number-at-pos) 1))
      (inline-review--clear-overlays))))

(ert-deftest crm-next-thread-at-boundary ()
  "Test that next-thread stops at the last thread instead of wrapping."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/crm-test-next-wrap.el")
    (insert "line1\nline2\nline3")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    ;; git-root is nil for /tmp; seed review-active-cache so the guard passes.
    (let ((inline-review--review-active-cache (make-hash-table :test 'equal)))
      (puthash nil 42 inline-review--review-active-cache)
      (let ((ov1 (make-overlay (crm-test--line-beg 1)
                               (crm-test--line-end 1)))
            (ov2 (make-overlay (crm-test--line-beg 3)
                               (crm-test--line-end 3))))
        (overlay-put ov1 'inline-review t)
        (overlay-put ov2 'inline-review t)
        (push ov1 inline-review--overlays)
        (push ov2 inline-review--overlays))
      ;; Point on line 3 (last thread); next-thread should stay at line 3
      (goto-char (crm-test--line-beg 3))
      (inline-review-next-thread)
      (should (= (line-number-at-pos) 3))
      (inline-review--clear-overlays))))

(ert-deftest crm-previous-thread-at-boundary ()
  "Test that previous-thread stops at the first thread instead of wrapping."
  (with-temp-buffer
    (setq buffer-file-name "/tmp/crm-test-prev-wrap.el")
    (insert "line1\nline2\nline3")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    ;; git-root is nil for /tmp; seed review-active-cache so the guard passes.
    (let ((inline-review--review-active-cache (make-hash-table :test 'equal)))
      (puthash nil 42 inline-review--review-active-cache)
      (let ((ov1 (make-overlay (crm-test--line-beg 1)
                               (crm-test--line-end 1)))
            (ov2 (make-overlay (crm-test--line-beg 3)
                               (crm-test--line-end 3))))
        (overlay-put ov1 'inline-review t)
        (overlay-put ov2 'inline-review t)
        (push ov1 inline-review--overlays)
        (push ov2 inline-review--overlays))
      ;; Point on line 1 (first thread); previous-thread should stay at line 1
      (goto-char (crm-test--line-beg 1))
      (inline-review-previous-thread)
      (should (= (line-number-at-pos) 1))
      (inline-review--clear-overlays))))

(ert-deftest crm-next-thread-no-overlays ()
  "Test that next-thread errors when no overlays exist."
  (with-temp-buffer
    (insert "line1\nline2")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    (should-error (inline-review-next-thread))))

(ert-deftest crm-previous-thread-no-overlays ()
  "Test that previous-thread errors when no overlays exist."
  (with-temp-buffer
    (insert "line1\nline2")
    (setq inline-review-mode t)
    (setq inline-review--overlays nil)
    (should-error (inline-review-previous-thread))))

(provide 'inline-review-test)

;;; inline-review-test.el ends here
