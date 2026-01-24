# mk-togglers Examples

Comprehensive real-world examples for using mk-togglers in your Emacs workflow.

## Table of Contents

1. [Basic Togglers](#basic-togglers)
2. [Workflow Togglers](#workflow-togglers)
3. [Development Environment Togglers](#development-environment-togglers)
4. [Remote Work Togglers](#remote-work-togglers)
5. [Custom Display Configurations](#custom-display-configurations)
6. [Advanced Patterns](#advanced-patterns)

## Basic Togglers

### Quick Access to Common Buffers

```elisp
;; Toggle scratch buffer
(mk/buffer-toggler my/scratch-toggle "*scratch*")
(global-set-key (kbd "C-c b s") 'my/scratch-toggle)

;; Toggle messages buffer
(mk/buffer-toggler my/messages-toggle "*Messages*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 10)))
(global-set-key (kbd "C-c b m") 'my/messages-toggle)

;; Toggle compilation buffer
(mk/buffer-toggler my/compilation-toggle "*compilation*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c b c") 'my/compilation-toggle)
```

### File Togglers for Quick Notes

```elisp
;; Daily journal
(mk/file-toggler my/journal-toggle "~/org/journal.org"
  :after-show (progn
                (goto-char (point-max))
                (insert "\n\n** " (format-time-string "%Y-%m-%d %H:%M") "\n"))
  :display-action '((display-buffer-full-frame)))
(global-set-key (kbd "C-c j") 'my/journal-toggle)

;; Quick notes file
(mk/file-toggler my/inbox-toggle "~/org/inbox.org"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c i") 'my/inbox-toggle)

;; Personal TODO list
(mk/file-toggler my/todo-toggle "~/org/todo.org"
  :after-show (org-overview)  ; Fold all headings
  :display-action '((display-buffer-in-side-window)
                    (side . right)
                    (window-width . 80)))
(global-set-key (kbd "C-c t") 'my/todo-toggle)
```

## Workflow Togglers

### Project Management Setup

```elisp
;; Toggle project root directory
(mk/dired-toggler my/project-root-toggle "~/projects/myapp/"
  :display-action '((display-buffer-in-side-window)
                    (side . left)
                    (window-width . 40)))
(global-set-key (kbd "C-c p r") 'my/project-root-toggle)

;; Toggle project documentation
(mk/file-toggler my/project-docs-toggle "~/projects/myapp/README.md"
  :display-action '((display-buffer-in-side-window)
                    (side . right)
                    (window-width . 80)))
(global-set-key (kbd "C-c p d") 'my/project-docs-toggle)

;; Toggle project TODO
(mk/file-toggler my/project-todo-toggle "~/projects/myapp/TODO.md")
(global-set-key (kbd "C-c p t") 'my/project-todo-toggle)
```

### Reference Material Togglers

```elisp
;; Toggle Emacs cheatsheet
(mk/file-toggler my/cheatsheet-toggle "~/docs/emacs-cheatsheet.org"
  :display-action '((display-buffer-in-side-window)
                    (side . right)
                    (window-width . 60)))
(global-set-key (kbd "<f1>") 'my/cheatsheet-toggle)

;; Toggle API reference
(mk/file-toggler my/api-ref-toggle "~/docs/api-reference.md"
  :after-show (markdown-toggle-markup-hiding nil)  ; Show markup
  :display-action '((display-buffer-at-bottom)
                    (window-height . 0.4)))
(global-set-key (kbd "<f2>") 'my/api-ref-toggle)
```

## Development Environment Togglers

### Terminal Management

```elisp
;; General purpose terminal
(mk/term-toggler my/term-toggle default-directory
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c C-t") 'my/term-toggle)

;; Development terminal (always in project root)
(mk/term-toggler my/dev-term-toggle "~/projects/myapp/"
  :with-new-term (keymap-local-set "C-c C-t" 'my/dev-term-toggle)
  :after-make-run "source .venv/bin/activate\n"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c t d") 'my/dev-term-toggle)

;; Git terminal
(mk/term-toggler my/git-term-toggle default-directory
  :with-new-term (progn
                   (keymap-local-set "C-c g" 'my/git-term-toggle)
                   (rename-buffer "*git-term*"))
  :after-make-run "clear && git status\n"
  :display-action '((display-buffer-full-frame)))
(global-set-key (kbd "C-c g") 'my/git-term-toggle)
```

### REPL Togglers

```elisp
;; Python REPL toggler
(mk/toggler my/python-repl-toggle "*Python*"
  :find-form (cl-find-if
              (lambda (buf)
                (with-current-buffer buf
                  (eq major-mode 'inferior-python-mode)))
              (buffer-list))
  :make-form (progn
               (run-python)
               (get-buffer "*Python*"))
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c r p") 'my/python-repl-toggle)

;; Eshell toggler
(mk/toggler my/eshell-toggle "*eshell*"
  :hide-if (eq major-mode 'eshell-mode)
  :find-form (cl-find-if
              (lambda (buf)
                (with-current-buffer buf
                  (eq major-mode 'eshell-mode)))
              (buffer-list))
  :make-form (eshell)
  :display-action '((display-buffer-at-bottom)
                    (window-height . 12)))
(global-set-key (kbd "C-c e") 'my/eshell-toggle)
```

### Test Runner Togglers

```elisp
;; Toggle test output buffer
(mk/buffer-toggler my/test-output-toggle "*pytest*"
  :make-form (get-buffer-create "*pytest*")
  :after-show (progn
                (goto-char (point-max))
                (compilation-minor-mode 1))
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c x t") 'my/test-output-toggle)

;; Terminal for running tests
(mk/term-toggler my/test-term-toggle default-directory
  :with-new-term (keymap-local-set "C-c x t" 'my/test-term-toggle)
  :after-make-run "clear\n"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c x r") 'my/test-term-toggle)
```

## Remote Work Togglers

### SSH Server Management

```elisp
;; Production server terminal
(mk/term-toggler my/prod-server-toggle "/ssh:user@prod-server:/var/www/myapp"
  :with-new-term (progn
                   (keymap-local-set "C-c s p" 'my/prod-server-toggle)
                   (rename-buffer "*prod-server*"))
  :after-make-run "cd /var/www/myapp\n"
  :display-action '((display-buffer-full-frame)))
(global-set-key (kbd "C-c s p") 'my/prod-server-toggle)

;; Development server terminal
(mk/term-toggler my/dev-server-toggle "/ssh:user@dev-server:/home/user/projects"
  :with-new-term (rename-buffer "*dev-server*")
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c s d") 'my/dev-server-toggle)

;; Database server terminal
(mk/term-toggler my/db-server-toggle "/ssh:admin@db-server:/var/log"
  :with-new-term (rename-buffer "*db-server*")
  :after-make-run "sudo -i\n"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c s b") 'my/db-server-toggle)
```

### Multi-Server Workflow

```elisp
;; Define server configurations
(defvar my/servers
  '(("prod"    . "/ssh:deploy@prod.example.com:/var/www/app")
    ("staging" . "/ssh:deploy@staging.example.com:/var/www/app")
    ("dev"     . "/ssh:dev@dev.example.com:/home/dev/app")))

;; Create toggler for each server
(dolist (server my/servers)
  (let* ((name (car server))
         (path (cdr server))
         (func-name (intern (format "my/server-%s-toggle" name))))
    (eval
     `(progn
        (mk/term-toggler ,func-name ,path
          :with-new-term (rename-buffer ,(format "*%s-server*" name))
          :after-make-run "cd /var/www/app && git status\n"
          :display-action '((display-buffer-at-bottom)
                            (window-height . 20)))
        (global-set-key (kbd ,(format "C-c s %s" (substring name 0 1)))
                        ',func-name)))))
```

## Custom Display Configurations

### Sidebar Togglers

```elisp
;; Left sidebar for file browser
(mk/dired-toggler my/sidebar-dired-toggle default-directory
  :display-action '((display-buffer-in-side-window)
                    (side . left)
                    (slot . -1)
                    (window-width . 35)
                    (window-parameters . ((no-delete-other-windows . t)))))
(global-set-key (kbd "C-c w l") 'my/sidebar-dired-toggle)

;; Right sidebar for documentation
(mk/file-toggler my/sidebar-docs-toggle "~/docs/notes.org"
  :display-action '((display-buffer-in-side-window)
                    (side . right)
                    (slot . 1)
                    (window-width . 80)
                    (window-parameters . ((no-delete-other-windows . t)))))
(global-set-key (kbd "C-c w r") 'my/sidebar-docs-toggle)
```

### Bottom Panel Togglers

```elisp
;; Small bottom panel for quick terminal
(mk/term-toggler my/bottom-term-toggle default-directory
  :display-action '((display-buffer-at-bottom)
                    (window-height . 12)
                    (window-parameters . ((mode-line-format . none)))))
(global-set-key (kbd "C-`") 'my/bottom-term-toggle)

;; Large bottom panel for test output
(mk/buffer-toggler my/bottom-output-toggle "*compilation*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 0.4)))
(global-set-key (kbd "C-c w b") 'my/bottom-output-toggle)
```

### Fullscreen Togglers

```elisp
;; Fullscreen scratch for focused work
(mk/buffer-toggler my/focus-scratch-toggle "*scratch*"
  :display-action '((display-buffer-full-frame)
                    (dedicated . t))
  :after-show (text-scale-increase 2)
  :before-hide (text-scale-set 0))
(global-set-key (kbd "C-c f s") 'my/focus-scratch-toggle)

;; Fullscreen terminal for intensive terminal work
(mk/term-toggler my/focus-term-toggle default-directory
  :display-action '((display-buffer-full-frame)
                    (dedicated . t)))
(global-set-key (kbd "C-c f t") 'my/focus-term-toggle)
```

## Advanced Patterns

### Context-Aware Terminal Toggler

Creates a terminal based on current file's directory or project root:

```elisp
(defun my/smart-term-directory ()
  "Return appropriate directory for terminal."
  (or (when (buffer-file-name)
        (file-name-directory (buffer-file-name)))
      default-directory))

(mk/term-toggler my/smart-term-toggle (my/smart-term-directory)
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
(global-set-key (kbd "C-c C-t") 'my/smart-term-toggle)
```

### Conditional Display Actions

Different display based on window configuration:

```elisp
(defun my/adaptive-display-action ()
  "Return display action based on current window state."
  (if (> (length (window-list)) 2)
      ;; Multiple windows: use bottom
      '((display-buffer-at-bottom)
        (window-height . 15))
    ;; Single window: use side window
    '((display-buffer-in-side-window)
      (side . right)
      (window-width . 80))))

(mk/buffer-toggler my/adaptive-scratch-toggle "*scratch*"
  :display-action (my/adaptive-display-action))
(global-set-key (kbd "C-c a") 'my/adaptive-scratch-toggle)
```

### Toggler with Auto-Save

Automatically save current buffer before toggling:

```elisp
(mk/file-toggler my/safe-notes-toggle "~/notes.org"
  :before-show (when (buffer-modified-p)
                 (save-buffer)
                 (message "Saved current buffer"))
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c n") 'my/safe-notes-toggle)
```

### Project-Specific Terminal with Activation

Terminal that activates project environment:

```elisp
(defun my/project-activate-commands ()
  "Return activation commands for current project."
  (cond
   ((file-exists-p ".venv/bin/activate")
    "source .venv/bin/activate\n")
   ((file-exists-p "Pipfile")
    "pipenv shell\n")
   ((file-exists-p "package.json")
    "nvm use\n")
   (t "")))

(mk/term-toggler my/project-term-toggle default-directory
  :after-make-run (my/project-activate-commands)
  :after-show-run (my/project-activate-commands)
  :display-action '((display-buffer-at-bottom)
                    (window-height . 18)))
(global-set-key (kbd "C-c p t") 'my/project-term-toggle)
```

### Ranger File Manager Toggle

Terminal running ranger file manager:

```elisp
(mk/term-toggler my/ranger-toggle default-directory
  :with-new-term (keymap-local-set "C-c r" 'my/ranger-toggle)
  :after-make-run "ranger\n"
  :display-action '((display-buffer-full-frame)))
(global-set-key (kbd "C-c r") 'my/ranger-toggle)
```

### Docker Container Terminal

Toggle terminal inside a Docker container:

```elisp
(defvar my/docker-container "myapp-dev")

(mk/term-toggler my/docker-toggle default-directory
  :after-make-run (format "docker exec -it %s bash\n" my/docker-container)
  :with-new-term (progn
                   (rename-buffer (format "*docker-%s*" my/docker-container))
                   (keymap-local-set "C-c d" 'my/docker-toggle))
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))
(global-set-key (kbd "C-c d") 'my/docker-toggle)
```

### Multiple Togglers with Prefix Argument

Use prefix argument to choose toggler behavior:

```elisp
(defun my/multi-term-toggle (arg)
  "Toggle terminal with different behaviors based on ARG."
  (interactive "P")
  (cond
   ((equal arg '(4))   ; C-u
    (my/term-fullscreen-toggle))
   ((equal arg '(16))  ; C-u C-u
    (my/term-remote-toggle))
   (t
    (my/term-toggle))))

(mk/term-toggler my/term-toggle default-directory
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))

(mk/term-toggler my/term-fullscreen-toggle default-directory
  :display-action '((display-buffer-full-frame)))

(mk/term-toggler my/term-remote-toggle "/ssh:server:/home/user"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 20)))

(global-set-key (kbd "C-c C-t") 'my/multi-term-toggle)
```

### State-Preserving Buffer Toggle

Save and restore window configuration:

```elisp
(defvar my/saved-window-config nil)

(mk/buffer-toggler my/stateful-scratch-toggle "*scratch*"
  :before-show (setq my/saved-window-config (current-window-configuration))
  :before-hide (when my/saved-window-config
                 (set-window-configuration my/saved-window-config)
                 (setq my/saved-window-config nil))
  :display-action '((display-buffer-full-frame)))
(global-set-key (kbd "C-c S") 'my/stateful-scratch-toggle)
```

## Tips and Tricks

### Keybinding Conventions

Use consistent keybinding patterns:

```elisp
;; C-c b for buffer togglers
;; C-c f for file togglers
;; C-c d for directory togglers
;; C-c t for terminal togglers
;; C-c s for server togglers
;; C-c w for window/display togglers
```

### Debugging Togglers

Add messages to see what's happening:

```elisp
(mk/term-toggler my/debug-term-toggle default-directory
  :before-show (message "SHOW: Switching to terminal")
  :after-show (message "SHOW: Terminal displayed")
  :before-make (message "MAKE: Creating new terminal")
  :after-make (message "MAKE: Terminal created")
  :before-hide (message "HIDE: Hiding terminal")
  :after-hide (message "HIDE: Terminal hidden"))
```

### Performance Optimization

For large projects, cache expensive computations:

```elisp
(defvar my/cached-project-root nil)

(defun my/get-project-root ()
  "Get project root with caching."
  (or my/cached-project-root
      (setq my/cached-project-root
            (locate-dominating-file default-directory ".git"))))

(mk/dired-toggler my/fast-project-toggle (my/get-project-root))
```
