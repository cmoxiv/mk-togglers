# mk-togglers

A powerful Emacs Lisp macro library for creating intelligent buffer togglers with memory and context awareness.

## Overview

`mk-togglers` provides a set of macros that generate toggle functions for Emacs buffers. Unlike simple show/hide commands, these togglers remember where you came from, intelligently find or create buffers, and provide extensive customization hooks for controlling behavior at every stage of the toggle lifecycle.

Think of it as a smart light switch with memory - press once to show a buffer, press again to hide it and return exactly where you were.

## Features

- **Context Memory**: Each toggled buffer remembers which buffer you were in when you opened it
- **Smart Buffer Management**: Automatically finds existing buffers or creates new ones
- **Multiple Specialized Togglers**: Pre-built macros for buffers, files, directories, and terminals
- **Remote-Aware**: Terminal togglers intelligently handle remote (TRAMP) connections
- **Extensive Customization**: Hook into every phase of the toggle lifecycle (before/after show/hide/make)
- **Flexible Display Control**: Customize how and where buffers appear using Emacs display actions

## Documentation

- **[Quick Start Guide](QUICKSTART.md)** - Get started in 5 minutes
- **[Examples](EXAMPLES.md)** - Comprehensive real-world usage examples
- **[API Reference](API.md)** - Complete technical documentation
- **[Info Manual](mk-togglers.texi)** - Texinfo manual (build with `make info`)
- **[Contributing](CONTRIBUTING.md)** - Guidelines for contributors
- **[Changelog](CHANGELOG.md)** - Version history and changes

### Building the Info Manual

The project includes a comprehensive Texinfo manual that integrates with Emacs' Info system:

```bash
# Build the Info file
make info

# Build PDF manual
make pdf

# Install Info docs and compiled Lisp
make install

# Or install to custom location
make PREFIX=~/.local install
```

After installation, access the manual in Emacs with:
```
C-h i m mk-togglers RET
```

## Installation

**See [INSTALL.md](INSTALL.md) for complete installation instructions.**

### Quick Install with straight.el

```elisp
(use-package mk-togglers
  :straight (mk-togglers :type git :host github :repo "cmoxiv/mk-togglers")
  :config
  (mk/buffer-toggler my-scratch-toggle "*scratch*")
  (global-set-key (kbd "C-c s") 'my-scratch-toggle))
```

### Quick Install with Quelpa

```elisp
(use-package mk-togglers
  :ensure t
  :quelpa (mk-togglers :fetcher github :repo "cmoxiv/mk-togglers"))
```

### Manual Installation from GitHub

```bash
git clone https://github.com/cmoxiv/mk-togglers.git ~/.emacs.d/lisp/mk-togglers
```

Add to your Emacs configuration:
```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/mk-togglers")
(require 'mk-togglers)
```

### System-wide Installation

```bash
git clone https://github.com/cmoxiv/mk-togglers.git
cd mk-togglers
sudo make install
```

For more installation options (package managers, custom locations, troubleshooting), see **[INSTALL.md](INSTALL.md)**.

## Quick Start

### Basic Buffer Toggle

Toggle the `*scratch*` buffer:

```elisp
(mk/buffer-toggler my-scratch-toggle "*scratch*")
(global-set-key (kbd "C-c s") 'my-scratch-toggle)
```

Now press `C-c s`:
- First press: Opens `*scratch*` (remembers you were in `main.py`)
- Second press: Returns to `main.py`
- Third press: Back to `*scratch*`

### File Toggle

Toggle your TODO file:

```elisp
(mk/file-toggler my-todo-toggle "~/org/todo.org")
(global-set-key (kbd "C-c t") 'my-todo-toggle)
```

### Directory Toggle

Toggle a dired buffer for your projects directory:

```elisp
(mk/dired-toggler my-projects-toggle "~/projects/")
(global-set-key (kbd "C-c p") 'my-projects-toggle)
```

### Terminal Toggle

Toggle a terminal in the current directory:

```elisp
(mk/term-toggler my-term-toggle default-directory)
(global-set-key (kbd "C-c C-t") 'my-term-toggle)
```

## Core Concepts

### The Toggle Lifecycle

Every toggle operation goes through these phases:

```
1. HIDE CHECK: Am I looking at the toggle buffer?
   ├─ YES → HIDE it and return to previous buffer
   └─ NO  → Continue to next phase

2. FIND: Does the buffer already exist?
   └─ Found buffer → Continue to SHOW

3. SHOW CHECK: Should I show the existing buffer?
   ├─ YES → SHOW the buffer and store current location
   └─ NO  → Continue to MAKE

4. MAKE CHECK: Should I create a new buffer?
   ├─ YES → MAKE the buffer and store current location
   └─ NO  → Run FALLBACK
```

### Buffer-Local Memory

Each toggled buffer stores a buffer-local variable `_toggler/prev-buffer_` that points to the buffer you were in when you opened it. This creates a breadcrumb trail back to your original context.

## API Reference

### `mk/toggler`

The foundational macro that all other togglers build upon.

```elisp
(mk/toggler NAME TOGGLE-ITEM &rest KEYWORD-ARGS)
```

**Parameters:**
- `NAME`: Symbol name for the generated function
- `TOGGLE-ITEM`: Default value for the buffer/file/directory to toggle
- `KEYWORD-ARGS`: Extensive customization options (see below)

**Key Customization Points:**

#### Hide Phase
- `:hide-if` - Condition to determine if we should hide (default: buffer name matches)
- `:before-hide` - Code to run before hiding
- `:hide-form` - How to perform the hide action
- `:after-hide` - Code to run after successful hide
- `:hide-failed` - Code to run if hide fails
- `:with-hide-let` - Local bindings for hide phase

#### Find Phase
- `:find-form` - How to find an existing buffer (default: `get-buffer`)

#### Show Phase
- `:show-if` - Condition to show existing buffer (default: buffer was found)
- `:before-show` - Code to run before showing
- `:show-form` - How to show the buffer (default: `pop-to-buffer`)
- `:after-show` - Code to run after successful show
- `:show-failed` - Code to run if show fails
- `:with-show-let` - Local bindings for show phase

#### Make Phase
- `:make-if` - Condition to create new buffer (default: buffer not found)
- `:before-make` - Code to run before creating
- `:make-form` - How to create the buffer
- `:after-make` - Code to run after successful creation
- `:make-failed` - Code to run if creation fails
- `:with-make-let` - Local bindings for make phase

#### Fallback Phase
- `:before-fallback` - Code to run before fallback
- `:fallback-form` - What to do if all else fails
- `:after-fallback` - Code to run after fallback
- `:fallback-failed` - Code to run if fallback fails
- `:with-fallback-let` - Local bindings for fallback phase

#### Buffer Hooks
- `:with-buffer` - Code to run in the context of a shown buffer
- `:with-new-buffer` - Code to run in the context of a newly created buffer

#### Display Control
- `:with-toggle-let` - Global bindings for entire toggle operation
- `:show-prms` - Display parameters for showing buffers

### `mk/buffer-toggler`

Simplified macro for toggling named buffers.

```elisp
(mk/buffer-toggler NAME BUFFER-NAME &rest KEYWORD-ARGS)
```

**Parameters:**
- `NAME`: Symbol name for the generated function
- `BUFFER-NAME`: Name of buffer to toggle (e.g., `"*scratch*"`)
- `:make-form` - How to create buffer (default: `switch-to-buffer`)
- `:display-action` - Display action list (default: full-frame, dedicated)
- `:with-toggle-let` - Additional global bindings
- Plus all `:mk/toggler` options

**Example:**
```elisp
(mk/buffer-toggler my-messages-toggle "*Messages*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 0.3)))
```

### `mk/dired-toggler`

Toggle a dired buffer for a specific directory.

```elisp
(mk/dired-toggler NAME DIRECTORY &rest KEYWORD-ARGS)
```

**Parameters:**
- `NAME`: Symbol name for the generated function
- `DIRECTORY`: Directory path to toggle
- `:hide-if` - When to hide (default: in dired-mode in matching directory)
- `:make-form` - How to create (default: `dired`)
- `:display-action` - Display action (default: bottom, dedicated)
- `:with-toggle-let` - Additional global bindings
- Plus all `mk/toggler` options

**Example:**
```elisp
(mk/dired-toggler my-home-toggle "~/"
  :display-action '((display-buffer-in-side-window)
                    (side . left)
                    (window-width . 0.25)))
```

### `mk/file-toggler`

Toggle a specific file.

```elisp
(mk/file-toggler NAME FILE-PATH &rest KEYWORD-ARGS)
```

**Parameters:**
- `NAME`: Symbol name for the generated function
- `FILE-PATH`: Path to file to toggle
- `:hide-if` - When to hide (default: current buffer file matches path)
- `:make-form` - How to create (default: `find-file`)
- `:display-action` - Display action (default: bottom, dedicated)
- `:with-toggle-let` - Additional global bindings
- Plus all `mk/toggler` options

**Example:**
```elisp
(mk/file-toggler my-init-toggle "~/.emacs.d/init.el"
  :after-show (message "Welcome back to your config!")
  :display-action '((display-buffer-full-frame)))
```

### `mk/term-toggler`

Toggle a terminal buffer with intelligent directory and remote host handling.

```elisp
(mk/term-toggler NAME DIRECTORY &rest KEYWORD-ARGS)
```

**Parameters:**
- `NAME`: Symbol name for the generated function
- `DIRECTORY`: Directory for the terminal (use `default-directory` for current)
- `:with-term` - Code to run when switching to existing terminal
- `:with-new-term` - Code to run when creating new terminal
- `:after-make-run` - Shell command to run after creating terminal
- `:before-show-run` - Shell command to run before showing terminal
- `:after-show-run` - Shell command to run after showing terminal
- `:before-hide-run` - Shell command to run before hiding terminal
- `:after-hide-run` - Shell command to run after hiding terminal
- `:display-action` - Display action (default: bottom, dedicated)
- Plus all `mk/toggler` options

**Special Features:**
- Automatically detects remote directories (TRAMP paths)
- Matches terminals by hostname for remote connections
- Runs `cd` to navigate to the correct directory
- Supports running shell commands at various lifecycle points

**Example:**
```elisp
;; Simple terminal toggle
(mk/term-toggler my-term-toggle default-directory)
(global-set-key (kbd "C-c C-t") 'my-term-toggle)

;; Terminal that starts ranger file manager
(mk/term-toggler my-ranger-toggle default-directory
  :after-make-run "ranger; exit\n"
  :display-action '((display-buffer-full-frame)))
```

## Advanced Usage

### Customizing Display Behavior

Control where and how buffers appear:

```elisp
;; Open scratch in a side window
(mk/buffer-toggler my-scratch-toggle "*scratch*"
  :display-action '((display-buffer-in-side-window)
                    (side . right)
                    (window-width . 80)
                    (dedicated . t)))

;; Open file in bottom window with specific height
(mk/file-toggler my-notes-toggle "~/notes.org"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15)))
```

### Adding Lifecycle Hooks

Run code at specific points in the toggle lifecycle:

```elisp
(mk/buffer-toggler my-scratch-toggle "*scratch*"
  :before-show (message "Opening scratch...")
  :after-show (progn
                (goto-char (point-max))
                (message "Ready!"))
  :before-hide (message "Saving position...")
  :after-hide (message "Returned to work"))
```

### Custom Find Logic

Override how buffers are found:

```elisp
(mk/toggler my-python-repl-toggle "*Python*"
  :find-form (cl-find-if
              (lambda (buf)
                (with-current-buffer buf
                  (eq major-mode 'inferior-python-mode)))
              (buffer-list)))
```

### Context-Specific Terminals

Create different terminal togglers for different contexts:

```elisp
;; Local development terminal
(mk/term-toggler my-dev-term "~/projects/myapp/")
(global-set-key (kbd "C-c t d") 'my-dev-term)

;; Remote server terminal
(mk/term-toggler my-server-term "/ssh:user@server:/var/www/")
(global-set-key (kbd "C-c t s") 'my-server-term)

;; Terminal with automatic setup
(mk/term-toggler my-docker-term default-directory
  :after-make-run "docker exec -it mycontainer bash\n"
  :with-new-term (keymap-local-set "C-c C-d" 'my-docker-term))
```

### Multiple Togglers for Same Buffer

Create different togglers with different display behaviors:

```elisp
;; Scratch in sidebar
(mk/buffer-toggler my-scratch-side "*scratch*"
  :display-action '((display-buffer-in-side-window)
                    (side . right)))

;; Scratch fullscreen
(mk/buffer-toggler my-scratch-full "*scratch*"
  :display-action '((display-buffer-full-frame)))

(global-set-key (kbd "C-c s s") 'my-scratch-side)
(global-set-key (kbd "C-c s f") 'my-scratch-full)
```

## How It Works

### Variable Scoping

The macro uses a clever variable scoping system:

- `_buf`: The buffer name/path being toggled
- `_found-buffer`: Result of the find operation
- `_prev-buffer_`: The buffer you were in before toggling
- `_new-buffer`: The newly created buffer (in make phase)
- `_toggler/prev-buffer_`: Buffer-local variable storing the return destination

These variables are available in your customization expressions.

### Remote Terminal Matching

For terminal togglers, remote paths are handled specially:

1. Detects if directory is remote using `file-remote-p`
2. Parses TRAMP path to extract host information
3. Finds terminals by matching hostname (not full path)
4. Automatically SSHs to remote host when creating new terminal
5. Runs `cd` to navigate to the correct remote directory

## Troubleshooting

### Toggle doesn't return to previous buffer

**Problem**: Pressing toggle twice doesn't return you to where you were.

**Solution**: Check if `:hide-if` condition is correct. The hide condition must properly detect when you're viewing the toggled buffer.

### Terminal opens but doesn't change directory

**Problem**: Terminal toggle creates terminal but stays in wrong directory.

**Solution**: Check `:after-show` and `:with-new-buffer` forms. The terminal toggler uses `term-send-raw-string` to send `cd` commands.

### Buffer appears in wrong window

**Problem**: Buffer shows up in unexpected location.

**Solution**: Customize `:display-action`. See Emacs manual for display action options:
- `display-buffer-in-side-window`
- `display-buffer-at-bottom`
- `display-buffer-full-frame`
- `display-buffer-use-some-window`

### Remote terminal connects to wrong host

**Problem**: Terminal toggler SSHs to unexpected host.

**Solution**: The toggler matches by hostname. If you have multiple paths on the same host, it will reuse the same terminal. To force separate terminals, customize `:find-form`.

## Examples

See [EXAMPLES.md](EXAMPLES.md) for comprehensive real-world usage examples.

## Contributing

Contributions are welcome! Please feel free to submit issues, feature requests, or pull requests.

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines on how to contribute.

## License

See [LICENSE](LICENSE) file for details.

## Credits

Created for efficient Emacs buffer management with context awareness and smart toggle behavior.
