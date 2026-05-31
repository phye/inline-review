# inline-review

[TOC]

> **Note:** Most of the code in this repository was generated with
> [Claude Code](https://claude.ai/code).

TODO(phye): add more snapshots

## Introduction

`inline-review` is a lightweight (but extensible) Emacs minor mode focusing on reviewing Merge/Pull Requests directly within Emacs(so that you don't miss any Emacs feature -- be it code navigation, quick movement, irrelated code context .etc), with the help of [emacs overlay](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html).

- By "lightweight", this package was **deliberately** designed to rely on remote forge API only, thus eliminating any local database requirement (one major diff with [forge](https://github.com/magit/forge)). The advantage of this decision is clear: simpler configuration, easier usage, easy to maintain local/remote data sync (all data are stored and fetched remotely).

- By "extensible", it was designed from DAY ONE with the idea that there're many git forges all over the world (and there will be more, if you count in on premises forge vendor), hence it MUST BE EASY to support other unknown forges. Given this design decision, if you need to add other forge, simply refer to `inline-review-backend-registry` doc string for how to add one. When you're ready, just invoke `inline-review-register-backend` to register your own forge in your init.el, with this package unchanged.

- By "focusing on code review", **only the following code-review related features** are **deliberately** chosen to be implemented(another major diff with [forge](https://github.com/magit/forge)):
  - auto forge induction: all the user needs to provide is the MR/PR url, no more info required
  - overview: show LOC distribution of the MR/PR
  - inline diff rendering: added/deleted lines are marked via overlay
  - inline comment rendering: displays comments directly in the correct buffer via overlay
  - inline comment interaction: add/reply/resolve/delete comments on region selected
  - quick movement of diff hunks and comment threads: so that you can jump from one hunk to another, or from one thread to another.
  
## Installation

### Manual (before emacs 30)

```elisp
(add-to-list 'load-path "/path/to/inline-review")
(require 'inline-review)
```

### use-package (after emacs 30)

```elisp
(use-package inline-review
  :vc (:url "https://github.com/phye/inline-review"
            :rev "master"))
```

> **Note:** while the author is an emacs user of more than 10 years, he is quite fresh in developing emacs package -- he has no idea how to make the package in melpa. And when he figured out how, it's expected that this package can be installed from melpa directly.

## Authentication

In order to access forge APIs, you must provide API tokens for `inline-review`. Please refer to the following guides on how to obtain one.

- **GitHub** — see [Managing your personal access tokens](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) (required scope: `repo`)
- **GitLab** — see [Personal access tokens](https://docs.gitlab.com/user/profile/personal_access_tokens/) (required scope: `api`)
- **Codeberg** — see [Generating an Access Token](https://docs.codeberg.org/advanced/access-token/) (required scope: `repository` read/write)
- **Gongfeng** (`git.woa.com` / `code.tencent.com`) — log in to the platform, go to **User Settings** → **Access Tokens**, and generate a token with full access.

### Token scopes required

| Platform | Required scopes |
|----------|-----------------|
| GitHub   | `repo` (private repos) or `public_repo` (public only) |
| GitLab   | `api` |
| Codeberg | `repository` (read/write) |
| Gongfeng | full access |

Tokens are read exclusively from `~/.authinfo` (or `~/.authinfo.gpg`), refer to the official [doc](https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html) for more detail.
No tokens are stored in Emacs custom variables.

### Shared entry (simplest)

Add one entry per forge host (**the API endpoint (such as api.github.com), not the user endpoint(github.com)**), using `login ^crm` to distinguish these entries from tokens used by other Emacs tools (e.g. Magit/ghub use `login ^`): 

Note: you only need to config token for the forge you're using, the following complete list are just examples.

```
machine api.github.com  login ^crm password <github-token>
machine gitlab.com      login ^crm password <gitlab-token>
machine codeberg.org     login ^crm password <codeberg-token>
machine git.woa.com      login ^crm password <gongfeng-token>
machine code.tencent.com login ^crm password <gongfeng-token>
```

For self-hosted instances, use the actual hostname, e.g.:

```
machine gitlab.company.com  login ^crm password <token>
machine github.company.com  login ^crm password <token>
```

### Per-user entry

If you have multiple accounts on the same host, set the git config key
`<backend>.user` globally first:

```bash
git config --global github.user yourname
git config --global gitlab.user yourname
git config --global codeberg.user yourname
git config --global gongfeng.user yourname
```

Then add a per-user entry to `~/.authinfo` using `<username>^crm` as the login:

```
machine git.woa.com  login yourname^crm  password <token>
```

The lookup order for each host is:

1. `login ^crm` — shared entry
2. `login <git-config-user>^crm` — per-user entry
3. any login on the host — last-resort fallback

## Usage

### Quick start

1. Open any file in a git repository that has an open MR/PR.

2. Run:
   ```
   M-x inline-review-review-url
   ```
   Paste the full web URL of the MR/PR, e.g.:
   ```
   https://git.woa.com/team/project/-/merge_requests/856
   https://github.com/owner/repo/pull/42
   https://gitlab.com/ns/project/-/merge_requests/7
   ```
   The backend, project, and IID are parsed from the URL automatically.
   Comments are fetched and displayed immediately as overlays.

3. Either invoke `inline-review-first-hunk` followed by `inline-review-next-hunk` to navigate between hunks and add/reply comments (`inline-review-mode` will be auto enabled on hunk movement).

4. Or if you know which file to review and want to provide your comments, just enable the mode within that file:
   ```
   M-x inline-review-mode
   ```
   The cached IID and backend are reused automatically.

### Commands

| Command | Description |
|---------|-------------|
| `inline-review-review-url` | **Main entry point.** Start a review from a full MR/PR URL |
| `inline-review-finish-review` | End the review session, restore original branch, and clear all state |
| `inline-review-mode` | Toggle the minor mode (uses cached state if available) |
| `inline-review-overview` | Show `git diff --stat` for the current MR/PR in a popup buffer |
| `inline-review-add-comment` | Add a comment on the selected region |
| `inline-review-edit-comment` | Edit the comment overlay at point |
| `inline-review-reply-comment` | Reply to the comment thread at point |
| `inline-review-delete-comment` | Delete the comment at point |
| `inline-review-resolve-comment` | Mark the comment at point as resolved |
| `inline-review-refresh` | Re-fetch and redisplay all comments and diff hunks |
| `inline-review-next-thread` | Jump to the next comment thread (cross-file) |
| `inline-review-previous-thread` | Jump to the previous comment thread (cross-file) |
| `inline-review-next-hunk` | Jump to the next diff hunk (cross-file) |
| `inline-review-previous-hunk` | Jump to the previous diff hunk (cross-file) |
| `inline-review-first-hunk` | Jump to the first diff hunk in the project |
| `inline-review-last-hunk` | Jump to the last diff hunk in the project |
| `inline-review-view-removed-lines` | Pop up a buffer with the full removed block near point |
| `inline-review-toggle-hide-resolved` | Toggle visibility of resolved comment threads |
| `inline-review-set-backend-for-repo` | Override the auto-detected backend for this repo |

### Adding a comment

1. Select a region of code.
2. `M-x inline-review-add-comment`
3. An input area opens below the selection. Type your comment.
4. `C-c C-c` to submit, `C-c C-k` to cancel.

### Editing a comment

1. Move point to a line that has a comment overlay.
2. `M-x inline-review-edit-comment`
3. Edit the text in the input area, then `C-c C-c`.

### Replying to a comment

1. Move point to a line that has a comment overlay.
2. `M-x inline-review-reply-comment`
3. Type your reply in the input area, then `C-c C-c`.

### Deleting a comment

1. Move point to a line that has a comment overlay.
2. `M-x inline-review-delete-comment`
3. Confirm the prompt to delete.

### Resolving a comment

1. Move point to a line that has an unresolved comment overlay.
2. `M-x inline-review-resolve-comment`

> **Note:** GitHub does not expose a REST API for resolving review comments.
> Use the web interface for GitHub repos.

### Review overview

`M-x inline-review-overview` opens a read-only buffer showing `git diff --stat`
between the MR source and target branches. Each file is a clickable button that
opens the file directly.

### Navigating comment threads and diff hunks

All navigation commands work **cross-file** within the current project:

- `M-x inline-review-next-thread` / `M-x inline-review-previous-thread` — jump between comment threads
- `M-x inline-review-next-hunk` / `M-x inline-review-previous-hunk` — jump between diff hunks
- `M-x inline-review-first-hunk` / `M-x inline-review-last-hunk` — jump to the first / last hunk

The target file is opened automatically and `inline-review-mode` is activated if needed.

### Viewing removed lines

When diff hunk highlighting is enabled, deleted lines are shown inline with a `-`
margin indicator. If a block is truncated, press `C-c C-d` (or run
`M-x inline-review-view-removed-lines`) to pop up a buffer with the full removed text.

### Hiding resolved threads

`M-x inline-review-toggle-hide-resolved` switches between showing and hiding
resolved comment threads, then refreshes the overlays.

### Finishing a review

`M-x inline-review-finish-review` ends the session for the current project:
disables `inline-review-mode` in all project buffers, restores the original git
branch, pops any stashed changes, clears caches, and removes per-repo state files.

## Configuration

### Custom API base URLs

For GitHub Enterprise or self-hosted GitLab, override the base URL:

```elisp
(setq inline-review-github-api-url "https://github.company.com/api/v3")
(setq inline-review-gitlab-api-url "https://gitlab.company.com/api/v4")
(setq inline-review-gongfeng-api-url "https://git.company.com/api/v3")
```

### Force a backend

Auto-detection reads the git remote URL. To override for a specific repo, add a
`.dir-locals.el` at the root:

```elisp
((nil . ((inline-review-backend . gongfeng))))
```

Or interactively (persists to `.git/inline-review-backend`):

```
M-x inline-review-set-backend-for-repo
```

### Adding a custom backend

The backend registry is public. You can add support for any additional forge
without modifying or forking this package. Call
`inline-review-register-backend` from your Emacs init after the package
is loaded:

```elisp
(with-eval-after-load 'inline-review

  ;; Optional: declare the base URL as a custom variable
  (defcustom my-forgejo-api-url "https://forgejo.example.com/api/v1"
    "API base URL for my Forgejo instance."
    :type 'string)

  (inline-review-register-backend
    'forgejo
    :api-url-var  'my-forgejo-api-url
    :remote-re     "forgejo\\.example\\.com"
    :fetch         #'my--forgejo-fetch-comments
    :post          #'my--forgejo-post-comment
    :update        #'my--forgejo-update-comment
    :resolve       #'my--forgejo-resolve-comment))
```

The registered backend is automatically available for auto-detection (via
`:remote-re`), token lookup (via `:api-url-var`), and all dispatch calls.
User-registered backends are prepended to the registry and therefore take
precedence over the built-in ones for remote-URL matching.

The four functions must follow the same async conventions as the built-in
backends — see the commentary in any of the `inline-review-*.el` files
for the expected signatures and patterns.

### Faces

All faces have dark- and light-theme variants and are defined in
`inline-review-custom.el` alongside the customization variables:

| Face | Used for |
|------|----------|
| `inline-review-comment-face` | Unresolved comment body |
| `inline-review-resolved-body-face` | Resolved comment body |
| `inline-review-input-face` | Comment input overlay |
| `inline-review-header-face` | Author / date header line |
| `inline-review-resolved-face` | ✓resolved status indicator |
| `inline-review-unresolved-face` | ○open status indicator |