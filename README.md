# inline-review

> **Note:** Most of the code in this repository was generated with
> [Claude Code](https://claude.ai/code).

A lightweight Emacs minor mode for performing code review directly inside Emacs
against GitHub Pull Requests, GitLab Merge Requests, and Gongfeng (工蜂,
git.woa.com / code.tencent.com) MRs.

## Features

- **Multi-platform**: GitHub, GitLab, and Gongfeng (git.woa.com / code.tencent.com)
- **URL-first entry point**: paste a full MR/PR URL — backend, project, and IID
  are all parsed automatically
- **Overlay-based comments**: review comments appear as inline overlays at the
  correct line numbers
- **Thread display**: root comments and replies rendered together, resolved
  threads visually distinguished
- **Commenting**: select a region, type a comment, `C-c C-c` to submit
- **Edit & resolve**: edit or resolve existing comments at point
- **Per-repo caching**: IID and backend are persisted in `.git/` so you don't
  re-enter them every session
- **authinfo-only auth**: tokens are read exclusively from `~/.authinfo` — no
  tokens stored in Emacs variables
  
## Comparison with other packages

| | [github-review](https://github.com/charignon/github-review) | [code-review](https://github.com/wandersoncferreira/code-review) | inline-review |
|---|---|---|---|
| Platforms | GitHub only | GitHub, GitLab, Gitea | GitHub, GitLab, Gongfeng |
| Local database | None | Required (closql/forge) | **None** — remote API only |
| Dependencies | ghub | forge, closql, magit, ... | ghub only |
| Review interface | Dedicated diff buffer | Dedicated diff buffer | **Inline overlays in your buffer** |
| Focus | Full PR review (approve, request changes) | Full review workflow | **Commenting on open files only** |
| Setup | authinfo token | Sync local DB first | Paste a URL and go |
| Custom backends | No | No | **Yes — register from init, no fork needed** |

**inline-review** is intentionally narrow in scope:

- **No local database.** All data is fetched live from the remote API on demand.
  There is nothing to sync, migrate, or corrupt.
- **No forge/magit dependency.** The only external dependency is `ghub` (for
  HTTP). You do not need a full Magit setup to use this package.
- **Works in your buffer.** Comments appear as overlays directly in the file you
  are editing — no context switching to a separate diff view.
- **One job.** Open a file, paste an MR/PR URL, see comments inline, post
  replies. Nothing more.
- **Extensible backends.** The backend registry (`inline-review-backend-registry`)
  is public. Add support for any forge without forking the package — just call
  `inline-review-register-backend` from your init file.

If you need a full review workflow — diff views, approvals, PR creation — use
[github-review](https://github.com/charignon/github-review),
[code-review](https://github.com/wandersoncferreira/code-review), or
[forge](https://github.com/magit/forge) instead.

## Requirements

- Emacs 27.1+
- [ghub](https://github.com/magit/ghub) 3.6+ (for GitHub and GitLab backends)

## Installation

### Manual

```elisp
(add-to-list 'load-path "/path/to/inline-review")
(require 'inline-review)
```

### use-package

```elisp
(use-package inline-review
  :load-path "~/path/to/inline-review")
```

No token variables to set — see Authentication below.

## Authentication

Tokens are read exclusively from `~/.authinfo` (or `~/.authinfo.gpg`).
No tokens are stored in Emacs custom variables.

### Shared entry (simplest)

Add one entry per forge host, using `login ^crm` to distinguish these entries
from tokens used by other Emacs tools (e.g. Magit/ghub use `login ^`):

```
machine api.github.com  login ^crm password <github-token>
machine gitlab.com      login ^crm password <gitlab-token>
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

### Token scopes required

| Platform | Required scopes |
|----------|----------------|
| GitHub   | `repo` (private repos) or `public_repo` (public only) |
| GitLab   | `api` |
| Gongfeng | full access token from git.woa.com → User Settings → Access Tokens |

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

3. To review the same MR in another file of the same repo, just enable the mode:
   ```
   M-x inline-review-mode
   ```
   The cached IID and backend are reused automatically.

### Commands

| Command | Description |
|---------|-------------|
| `inline-review-review-url` | **Main entry point.** Start a review from a full MR/PR URL |
| `inline-review-mode` | Toggle the minor mode (uses cached state if available) |
| `inline-review-add-comment` | Add a comment on the selected region |
| `inline-review-edit-comment` | Edit the comment overlay at point |
| `inline-review-resolve-comment` | Mark the comment at point as resolved |
| `inline-review-refresh` | Re-fetch and redisplay all comments |
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

### Resolving a comment

1. Move point to a line that has an unresolved comment overlay.
2. `M-x inline-review-resolve-comment`

> **Note:** GitHub does not expose a REST API for resolving review comments.
> Use the web interface for GitHub repos.

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

## Supported platforms

| Platform | Domain | API |
|----------|--------|-----|
| GitHub | github.com | REST v3 |
| GitHub Enterprise | custom | REST v3 |
| GitLab | gitlab.com | REST v4 |
| Self-hosted GitLab | custom | REST v4 |
| Gongfeng (工蜂) | git.woa.com, code.tencent.com | REST v3 (custom) |

## Troubleshooting

**"No token found" error**

- Check that your `~/.authinfo` has an entry with the correct hostname and
  `login ^crm`.
- Test interactively:
  ```elisp
  (auth-source-search :host "git.woa.com" :user "^crm" :max 1)
  ```
- If you use a per-user entry, verify `git config --global gongfeng.user` is set.

**Wrong backend detected**

- Use `M-x inline-review-set-backend-for-repo` to persist the correct
  backend for the repository.
- Or set it via `.dir-locals.el` (see Configuration above).

**Comments not showing**

- Confirm the MR/PR URL is correct and the MR is open.
- Check `*Messages*` for API error details.
- Try `M-x inline-review-refresh`.

**API errors for self-hosted instances**

- Ensure `inline-review-*-api-url` matches your instance's API root.

## License

MIT License — Copyright (c) 2024 phye
