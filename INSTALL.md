# Installation Guide

This guide covers all methods of installing mk-togglers.

## Table of Contents

- [Quick Install](#quick-install)
- [Package Manager Installation](#package-manager-installation)
  - [straight.el](#straightel)
  - [use-package with straight.el](#use-package-with-straightel)
  - [Quelpa](#quelpa)
  - [Manual with package.el](#manual-with-packageel)
- [Manual Installation](#manual-installation)
- [System-wide Installation](#system-wide-installation)
- [Verifying Installation](#verifying-installation)

## Quick Install

### Using straight.el (Recommended)

```elisp
(use-package mk-togglers
  :straight (mk-togglers :type git :host github :repo "cmoxiv/mk-togglers")
  :config
  ;; Your configuration here
  (mk/buffer-toggler my-scratch-toggle "*scratch*")
  (global-set-key (kbd "C-c s") 'my-scratch-toggle))
```

### Using Quelpa

```elisp
(use-package mk-togglers
  :ensure t
  :quelpa (mk-togglers :fetcher github :repo "cmoxiv/mk-togglers"))
```

## Package Manager Installation

### straight.el

If you use [straight.el](https://github.com/radian-software/straight.el):

```elisp
;; Install
(straight-use-package
 '(mk-togglers :type git :host github :repo "cmoxiv/mk-togglers"))

;; Then require and configure
(require 'mk-togglers)

;; Your togglers
(mk/buffer-toggler my-scratch-toggle "*scratch*")
(global-set-key (kbd "C-c s") 'my-scratch-toggle)
```

### use-package with straight.el

If you use `use-package` with `straight.el`:

```elisp
(use-package mk-togglers
  :straight (mk-togglers :type git :host github :repo "cmoxiv/mk-togglers")
  :demand t
  :config
  ;; Buffer togglers
  (mk/buffer-toggler my/scratch-toggle "*scratch*"
    :display-action '((display-buffer-at-bottom)
                      (window-height . 15)))
  (global-set-key (kbd "C-c b s") 'my/scratch-toggle)

  ;; File togglers
  (mk/file-toggler my/todo-toggle "~/org/todo.org")
  (global-set-key (kbd "C-c f t") 'my/todo-toggle)

  ;; Terminal togglers
  (mk/term-toggler my/term-toggle default-directory)
  (global-set-key (kbd "C-c C-t") 'my/term-toggle))
```

### Quelpa

If you use [Quelpa](https://github.com/quelpa/quelpa):

```elisp
;; Install
(quelpa '(mk-togglers :fetcher github :repo "cmoxiv/mk-togglers"))

;; Then configure
(require 'mk-togglers)
(mk/buffer-toggler my-scratch-toggle "*scratch*")
(global-set-key (kbd "C-c s") 'my-scratch-toggle)
```

With `use-package` and `quelpa-use-package`:

```elisp
(use-package mk-togglers
  :ensure t
  :quelpa (mk-togglers :fetcher github :repo "cmoxiv/mk-togglers")
  :config
  (mk/buffer-toggler my-scratch-toggle "*scratch*")
  (global-set-key (kbd "C-c s") 'my-scratch-toggle))
```

### Manual with package.el

Using Emacs' built-in package manager:

1. Clone the repository:
   ```bash
   cd ~/.emacs.d/
   git clone https://github.com/cmoxiv/mk-togglers.git
   ```

2. Add to your init file:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/mk-togglers")
   (require 'mk-togglers)
   ```

3. Byte-compile for better performance:
   ```elisp
   M-x byte-recompile-directory RET ~/.emacs.d/mk-togglers RET
   ```

## Manual Installation

### Clone from GitHub

```bash
# Clone to a location of your choice
cd ~/projects
git clone https://github.com/cmoxiv/mk-togglers.git

# Or clone to Emacs directory
cd ~/.emacs.d/lisp
git clone https://github.com/cmoxiv/mk-togglers.git
```

### Add to Emacs configuration

Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
;; Add to load path
(add-to-list 'load-path "~/projects/mk-togglers")
;; Or if cloned to Emacs directory:
;; (add-to-list 'load-path "~/.emacs.d/lisp/mk-togglers")

;; Load the library
(require 'mk-togglers)

;; Create your togglers
(mk/buffer-toggler my-scratch-toggle "*scratch*")
(global-set-key (kbd "C-c s") 'my-scratch-toggle)
```

### Byte-compile

For better performance, byte-compile the library:

```bash
cd ~/projects/mk-togglers
make compile
```

Or from Emacs:
```elisp
M-x byte-compile-file RET ~/projects/mk-togglers/mk-togglers.el RET
```

### Keeping Up-to-Date

Pull the latest changes:

```bash
cd ~/projects/mk-togglers
git pull origin main
make compile
```

Or add this function to your config:

```elisp
(defun my/update-mk-togglers ()
  "Update mk-togglers from GitHub."
  (interactive)
  (let ((default-directory "~/projects/mk-togglers"))
    (shell-command "git pull origin main")
    (byte-recompile-directory default-directory 0)
    (message "mk-togglers updated!")))
```

## System-wide Installation

For system-wide installation (requires root/sudo):

```bash
cd mk-togglers
make install
```

This installs to `/usr/local` by default. The Makefile will:
- Install the Info documentation
- Install and byte-compile the Emacs Lisp
- Register with Info directory

For a custom installation prefix:

```bash
make PREFIX=~/.local install
```

Then add to your init file:

```elisp
;; Add to load path (if not using system directories)
(add-to-list 'load-path "~/.local/share/emacs/site-lisp/mk-togglers")
(require 'mk-togglers)

;; Add Info directory
(add-to-list 'Info-directory-list "~/.local/share/info")
```

Access the Info manual:
```
C-h i m mk-togglers RET
```

## Verifying Installation

### Check if loaded correctly

```elisp
;; Should return the file path
(locate-library "mk-togglers")

;; Should show documentation
(describe-function 'mk/toggler)

;; Create a test toggler
(mk/buffer-toggler test-toggle "*scratch*")
(global-set-key (kbd "C-c test") 'test-toggle)

;; Press C-c test to test
```

### Check version

```elisp
;; Check the version in the file
(with-current-buffer (find-library-name "mk-togglers")
  (goto-char (point-min))
  (re-search-forward "Version: \\(.*\\)")
  (message "mk-togglers version: %s" (match-string 1)))
```

### Run a simple test

```elisp
(progn
  ;; Create test toggler
  (mk/buffer-toggler my-test-scratch "*scratch*"
    :display-action '((display-buffer-at-bottom)
                      (window-height . 10)))

  ;; Test it
  (global-set-key (kbd "C-c M-t") 'my-test-scratch)
  (message "Test toggler created! Press C-c M-t to test."))
```

## Troubleshooting Installation

### Package not found

**Problem**: `Cannot open load file: No such file or directory, mk-togglers`

**Solution**: Check the load path:

```elisp
;; List all load paths
(message "%s" load-path)

;; Manually add the correct path
(add-to-list 'load-path "/path/to/mk-togglers")
```

### Byte-compilation warnings

**Problem**: Warnings during byte-compilation

**Solution**: This is normal for lexical-binding variables. The library works correctly despite minor warnings.

### Straight.el can't find repository

**Problem**: `straight.el` can't clone the repository

**Solution**: Check the repository name and your internet connection:

```elisp
;; Try with explicit URL
(straight-use-package
 '(mk-togglers :type git
               :host github
               :repo "cmoxiv/mk-togglers"
               :files ("*.el")))
```

### Info manual not found

**Problem**: `C-h i m mk-togglers RET` says "No such node"

**Solution**: Build and install the Info documentation:

```bash
cd mk-togglers
make info
make install  # or make PREFIX=~/.local install
```

Then add to your init file:

```elisp
(add-to-list 'Info-directory-list "~/.local/share/info")
```

## Uninstalling

### Straight.el

```elisp
;; Remove from config, then rebuild
(straight-remove-package 'mk-togglers)
```

### Quelpa

```elisp
;; Remove from config, then
(quelpa-remove-package 'mk-togglers)
```

### Manual Installation

```bash
# Remove the directory
rm -rf ~/projects/mk-togglers

# Remove from init file
# Delete the (require 'mk-togglers) line
```

### System Installation

```bash
cd mk-togglers
sudo make uninstall  # or make PREFIX=~/.local uninstall
```

## Next Steps

After installation:

1. Read the [Quick Start Guide](QUICKSTART.md)
2. Browse [Examples](EXAMPLES.md) for ideas
3. Check the [API Reference](API.md) for details
4. Access the Info manual: `C-h i m mk-togglers RET`

## Getting Help

- Check [Troubleshooting](#troubleshooting-installation) section
- Read the [Troubleshooting chapter](README.md#troubleshooting) in README
- Open an issue on GitHub
- Check existing issues for solutions
