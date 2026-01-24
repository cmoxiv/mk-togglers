# mk-togglers Quick Start Guide

Get up and running with mk-togglers in 5 minutes.

## Installation

```elisp
;; Add to your init.el
(add-to-list 'load-path "/path/to/mk-togglers")
(require 'mk-togglers)
```

## The 5-Minute Tutorial

### 1. Toggle a Buffer

```elisp
;; Create a toggler for the scratch buffer
(mk/buffer-toggler my-scratch-toggle "*scratch*")

;; Bind it to a key
(global-set-key (kbd "C-c s") 'my-scratch-toggle)
```

**Usage:**
- First press: Opens `*scratch*`
- Second press: Returns to previous buffer
- Third press: Back to `*scratch*`

### 2. Toggle a File

```elisp
;; Create a toggler for your TODO file
(mk/file-toggler my-todo-toggle "~/org/todo.org")
(global-set-key (kbd "C-c t") 'my-todo-toggle)
```

### 3. Toggle a Directory

```elisp
;; Create a toggler for your projects directory
(mk/dired-toggler my-projects-toggle "~/projects/")
(global-set-key (kbd "C-c p") 'my-projects-toggle)
```

### 4. Toggle a Terminal

```elisp
;; Create a toggler for a terminal in current directory
(mk/term-toggler my-term-toggle default-directory)
(global-set-key (kbd "C-c C-t") 'my-term-toggle)
```

That's it! You now have smart togglers with memory.

## Common Patterns Cheat Sheet

### Change Window Position

```elisp
;; Bottom of screen
(mk/buffer-toggler my-toggle "*scratch*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))

;; Right sidebar
(mk/buffer-toggler my-toggle "*scratch*"
  :display-action '((display-buffer-in-side-window)
                    (side . right)
                    (window-width . 80)))

;; Left sidebar
(mk/buffer-toggler my-toggle "*scratch*"
  :display-action '((display-buffer-in-side-window)
                    (side . left)
                    (window-width . 40)))

;; Full screen
(mk/buffer-toggler my-toggle "*scratch*"
  :display-action '((display-buffer-full-frame)))
```

### Add Actions

```elisp
;; Run code when opening
(mk/file-toggler my-notes-toggle "~/notes.org"
  :after-show (goto-char (point-max)))

;; Run code when closing
(mk/file-toggler my-notes-toggle "~/notes.org"
  :before-hide (save-buffer))

;; Both
(mk/file-toggler my-notes-toggle "~/notes.org"
  :after-show (message "Welcome to notes!")
  :before-hide (progn
                 (save-buffer)
                 (message "Notes saved!")))
```

### Terminal with Commands

```elisp
;; Run command when creating terminal
(mk/term-toggler my-dev-term "~/projects/myapp/"
  :after-make-run "source .venv/bin/activate\n")

;; Run command when showing terminal
(mk/term-toggler my-dev-term "~/projects/myapp/"
  :after-show-run "clear && git status\n")

;; Start a program in terminal
(mk/term-toggler my-ranger-toggle default-directory
  :after-make-run "ranger\n")
```

### Remote Terminal

```elisp
;; SSH to remote server
(mk/term-toggler my-server-toggle "/ssh:user@example.com:/var/www/")
(global-set-key (kbd "C-c s") 'my-server-toggle)
```

## Common Display Actions Reference

| Where | Code |
|-------|------|
| Bottom (15 lines) | `:display-action '((display-buffer-at-bottom) (window-height . 15))` |
| Bottom (30%) | `:display-action '((display-buffer-at-bottom) (window-height . 0.3))` |
| Right sidebar | `:display-action '((display-buffer-in-side-window) (side . right) (window-width . 80))` |
| Left sidebar | `:display-action '((display-buffer-in-side-window) (side . left) (window-width . 40))` |
| Full frame | `:display-action '((display-buffer-full-frame))` |
| Same window | `:display-action '((display-buffer-same-window))` |

## Common Lifecycle Hooks Reference

| When | Hook | Example |
|------|------|---------|
| Before showing existing buffer | `:before-show` | `(revert-buffer t t)` |
| After showing existing buffer | `:after-show` | `(goto-char (point-max))` |
| Before creating new buffer | `:before-make` | `(message "Creating...")` |
| After creating new buffer | `:after-make` | `(insert "# Header\n")` |
| Before hiding buffer | `:before-hide` | `(save-buffer)` |
| After hiding buffer | `:after-hide` | `(message "Goodbye!")` |

## Keybinding Conventions

Suggested keybinding scheme:

```elisp
;; C-c b - Buffer togglers
(global-set-key (kbd "C-c b s") 'my-scratch-toggle)
(global-set-key (kbd "C-c b m") 'my-messages-toggle)

;; C-c f - File togglers
(global-set-key (kbd "C-c f t") 'my-todo-toggle)
(global-set-key (kbd "C-c f n") 'my-notes-toggle)

;; C-c d - Directory togglers
(global-set-key (kbd "C-c d p") 'my-projects-toggle)
(global-set-key (kbd "C-c d h") 'my-home-toggle)

;; C-c t - Terminal togglers
(global-set-key (kbd "C-c t t") 'my-term-toggle)
(global-set-key (kbd "C-c t d") 'my-dev-term-toggle)

;; C-c s - Server togglers
(global-set-key (kbd "C-c s p") 'my-prod-server-toggle)
(global-set-key (kbd "C-c s d") 'my-dev-server-toggle)
```

## Example Configuration

Here's a complete starter configuration:

```elisp
;; Load library
(add-to-list 'load-path "~/.emacs.d/lisp/mk-togglers")
(require 'mk-togglers)

;; Buffer togglers
(mk/buffer-toggler my/scratch-toggle "*scratch*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c b s") 'my/scratch-toggle)

(mk/buffer-toggler my/messages-toggle "*Messages*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 10)))
(global-set-key (kbd "C-c b m") 'my/messages-toggle)

;; File togglers
(mk/file-toggler my/todo-toggle "~/org/todo.org"
  :after-show (org-overview))
(global-set-key (kbd "C-c f t") 'my/todo-toggle)

(mk/file-toggler my/notes-toggle "~/org/notes.org"
  :after-show (goto-char (point-max)))
(global-set-key (kbd "C-c f n") 'my/notes-toggle)

;; Directory togglers
(mk/dired-toggler my/projects-toggle "~/projects/"
  :display-action '((display-buffer-in-side-window)
                    (side . left)
                    (window-width . 40)))
(global-set-key (kbd "C-c d p") 'my/projects-toggle)

;; Terminal togglers
(mk/term-toggler my/term-toggle default-directory
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c C-t") 'my/term-toggle)

;; Development terminal
(mk/term-toggler my/dev-term-toggle "~/projects/myapp/"
  :after-make-run "source .venv/bin/activate\n"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c t d") 'my/dev-term-toggle)
```

## Next Steps

- Read [README.md](README.md) for comprehensive overview
- Check [EXAMPLES.md](EXAMPLES.md) for real-world usage patterns
- Review [API.md](API.md) for complete API documentation
- See [CONTRIBUTING.md](CONTRIBUTING.md) if you want to contribute

## Troubleshooting

**Problem**: Toggle doesn't return to previous buffer

**Solution**: The buffer is remembering where it was first opened from. This is by design - each toggled buffer has its own memory.

---

**Problem**: Terminal appears in wrong location

**Solution**: Check your `:display-action`. Make sure you're using the right display function.

---

**Problem**: Remote terminal doesn't change directory

**Solution**: The terminal toggler automatically sends `cd` commands. If it's not working, check that the TRAMP path is correct and accessible.

---

For more help, see the main [README.md](README.md) or open an issue.
