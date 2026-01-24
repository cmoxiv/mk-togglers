# mk-togglers API Reference

Complete technical reference for all macros and their parameters.

## Table of Contents

1. [Core Macro: mk/toggler](#mktoggler)
2. [Specialized Macros](#specialized-macros)
3. [Variables and Scoping](#variables-and-scoping)
4. [Display Actions](#display-actions)
5. [Lifecycle Phases](#lifecycle-phases)

## mk/toggler

The foundational macro that powers all toggle functionality.

### Signature

```elisp
(mk/toggler NAME TOGGLE-ITEM &rest KEYWORD-ARGS)
```

### Parameters

#### Required Parameters

- **NAME** (symbol): The name of the function to create
  - Must be a valid Emacs Lisp symbol
  - Will be callable as an interactive function
  - Example: `my-scratch-toggle`

- **TOGGLE-ITEM** (any): The default item to toggle
  - For buffers: buffer name string (e.g., `"*scratch*"`)
  - For files: file path string (e.g., `"~/todo.org"`)
  - For directories: directory path string (e.g., `"~/projects/"`)
  - Can be an expression that evaluates at runtime (e.g., `default-directory`)

### Keyword Arguments

All keyword arguments are optional and have sensible defaults.

#### `:hide-if` (expression)

Condition that determines if the buffer should be hidden.

- **Default**: `(equal _buf (buffer-name))`
- **Type**: Lisp expression evaluated for truthiness
- **Context**: Evaluated in the context of the current buffer
- **Available variables**: `_buf`, `_found-buffer`
- **Returns**: Non-nil to trigger hide phase

**Examples:**
```elisp
;; Hide if buffer name matches
:hide-if (equal _buf (buffer-name))

;; Hide if in specific major mode
:hide-if (derived-mode-p 'term-mode)

;; Hide if in dired viewing specific directory
:hide-if (and (derived-mode-p 'dired-mode)
              (equal (file-truename default-directory)
                     (file-truename _buf)))

;; Hide if current file matches
:hide-if (and (buffer-file-name)
              (equal (file-truename (buffer-file-name))
                     (file-truename _buf)))
```

#### `:before-hide` (expression)

Code to execute before hiding the buffer.

- **Default**: `(ignore)`
- **Type**: Lisp expression
- **Context**: Evaluated in current buffer (the one being hidden)
- **Return value**: Ignored
- **Use cases**: Saving state, cleanup, logging

**Examples:**
```elisp
;; Log hide action
:before-hide (message "Hiding %s" (buffer-name))

;; Save buffer if modified
:before-hide (when (buffer-modified-p) (save-buffer))

;; Store window position
:before-hide (setq my-saved-position (point))
```

#### `:hide-form` (expression)

The action that performs the hiding.

- **Default**: Complex expression that switches to previous buffer or other buffer
- **Type**: Lisp expression
- **Context**: Evaluated with `:with-hide-let` bindings
- **Returns**: Non-nil on success, nil on failure
- **Default behavior**: Tries to switch to `_toggler/prev-buffer_` or `other-buffer`

**Examples:**
```elisp
;; Simple switch to other buffer
:hide-form (switch-to-buffer (other-buffer))

;; Delete window if not last window
:hide-form (unless (one-window-p) (delete-window))

;; Custom hiding logic
:hide-form (progn
             (bury-buffer)
             (switch-to-buffer (other-buffer))
             t)  ; Return t for success
```

#### `:after-hide` (expression)

Code to execute after successfully hiding the buffer.

- **Default**: `(always)` (always returns t)
- **Type**: Lisp expression
- **Context**: Evaluated after hide action
- **Return value**: Ignored

**Examples:**
```elisp
;; Log success
:after-hide (message "Buffer hidden successfully")

;; Trigger other actions
:after-hide (progn
              (save-some-buffers t)
              (message "Returned to %s" (buffer-name)))
```

#### `:hide-failed` (expression)

Code to execute if hiding fails.

- **Default**: `(message "[mk/toggler] Failed to hide buffer!!")`
- **Type**: Lisp expression
- **Context**: Evaluated when `:hide-form` returns nil

**Examples:**
```elisp
;; Custom error message
:hide-failed (error "Could not hide %s" (buffer-name))

;; Fallback action
:hide-failed (progn
               (message "Hide failed, minimizing instead")
               (iconify-frame))
```

#### `:with-hide-let` (binding list)

Local variable bindings for the hide phase.

- **Default**: `()`
- **Type**: List of bindings `((var1 val1) (var2 val2) ...)`
- **Scope**: Available during `:before-hide`, `:hide-form`, `:after-hide`

**Examples:**
```elisp
;; Simple bindings
:with-hide-let ((saved-point (point))
                (saved-window (selected-window)))

;; Computed bindings
:with-hide-let ((prev-buf (other-buffer))
                (time (current-time)))
```

#### `:find-form` (expression)

Expression to find an existing buffer.

- **Default**: `(get-buffer _buf)`
- **Type**: Lisp expression
- **Context**: Evaluated with `:with-toggle-let` bindings
- **Returns**: Buffer object if found, nil otherwise
- **Result stored in**: `_found-buffer`

**Examples:**
```elisp
;; Find by name
:find-form (get-buffer _buf)

;; Find by major mode
:find-form (cl-find-if
            (lambda (buf)
              (with-current-buffer buf
                (eq major-mode 'python-mode)))
            (buffer-list))

;; Find by file path
:find-form (find-buffer-visiting _buf)

;; Complex search
:find-form (cl-find-if
            (lambda (buf)
              (with-current-buffer buf
                (and (derived-mode-p 'term-mode)
                     (equal default-directory _buf))))
            (buffer-list))
```

#### `:show-if` (expression)

Condition to show an existing buffer.

- **Default**: `_found-buffer`
- **Type**: Lisp expression evaluated for truthiness
- **Context**: Evaluated after `:find-form`
- **Available variables**: `_buf`, `_found-buffer`, `_prev-buffer_`

**Examples:**
```elisp
;; Show if found
:show-if _found-buffer

;; Show only if not killed
:show-if (and _found-buffer (buffer-live-p _found-buffer))

;; Conditional show
:show-if (and _found-buffer
              (with-current-buffer _found-buffer
                (not (buffer-modified-p))))
```

#### `:before-show` (expression)

Code to execute before showing an existing buffer.

- **Default**: `(ignore)`
- **Type**: Lisp expression
- **Context**: Evaluated before `:show-form`

**Examples:**
```elisp
;; Refresh buffer
:before-show (with-current-buffer _found-buffer (revert-buffer t t))

;; Log action
:before-show (message "Showing existing %s" _buf)
```

#### `:show-form` (expression)

The action that displays the buffer.

- **Default**: `(pop-to-buffer _found-buffer)`
- **Type**: Lisp expression
- **Context**: Evaluated with `:with-show-let` bindings
- **Returns**: Buffer object on success, nil on failure

**Examples:**
```elisp
;; Simple display
:show-form (pop-to-buffer _found-buffer)

;; Switch without changing window
:show-form (switch-to-buffer _found-buffer)

;; Display with specific action
:show-form (display-buffer _found-buffer '((display-buffer-at-bottom)))
```

#### `:after-show` (expression)

Code to execute after successfully showing the buffer.

- **Default**: `(always)`
- **Type**: Lisp expression
- **Context**: Evaluated in the context of the shown buffer

**Examples:**
```elisp
;; Go to end
:after-show (goto-char (point-max))

;; Refresh and position
:after-show (progn
              (revert-buffer t t)
              (goto-char (point-min)))

;; Setup buffer-local keybindings
:after-show (keymap-local-set "C-c C-q" 'my-toggle-function)
```

#### `:show-failed` (expression)

Code to execute if showing fails.

- **Default**: `(message "[mk/toggler] Failed to show buffer!!")`

#### `:with-show-let` (binding list)

Local variable bindings for the show phase.

- **Default**: `()`
- **Type**: List of bindings

#### `:make-if` (expression)

Condition to create a new buffer.

- **Default**: `(not _found-buffer)`
- **Type**: Lisp expression evaluated for truthiness

**Examples:**
```elisp
;; Make if not found
:make-if (not _found-buffer)

;; Always make new
:make-if t

;; Conditional make
:make-if (and (not _found-buffer)
              (y-or-n-p "Create new buffer? "))
```

#### `:before-make` (expression)

Code to execute before creating a new buffer.

- **Default**: `(ignore)`

**Examples:**
```elisp
;; Create directory if needed
:before-make (let ((dir (file-name-directory _buf)))
               (unless (file-exists-p dir)
                 (make-directory dir t)))

;; Log action
:before-make (message "Creating new buffer: %s" _buf)
```

#### `:make-form` (expression)

The action that creates the buffer.

- **Default**: `(switch-to-buffer _buf)`
- **Type**: Lisp expression
- **Context**: Evaluated with `:with-make-let` bindings
- **Returns**: Buffer object on success, nil on failure
- **Result stored in**: `_new-buffer`

**Examples:**
```elisp
;; Create buffer
:make-form (switch-to-buffer _buf)

;; Open file
:make-form (find-file _buf)

;; Open directory
:make-form (dired _buf)

;; Create and configure
:make-form (let ((buf (get-buffer-create _buf)))
             (with-current-buffer buf
               (org-mode))
             buf)
```

#### `:after-make` (expression)

Code to execute after successfully creating the buffer.

- **Default**: `(always)`
- **Context**: Evaluated in the context of the new buffer

**Examples:**
```elisp
;; Insert template
:after-make (insert "# New File\n\n")

;; Setup mode
:after-make (progn
              (org-mode)
              (auto-fill-mode 1))

;; Set variables
:after-make (setq-local fill-column 100)
```

#### `:make-failed` (expression)

Code to execute if buffer creation fails.

- **Default**: `(message "[mk/toggler] Failed to make buffer!!")`

#### `:with-make-let` (binding list)

Local variable bindings for the make phase.

- **Default**: `()`

#### `:before-fallback` (expression)

Code to execute before fallback action.

- **Default**: `(ignore)`

#### `:fallback-form` (expression)

Action to take if all else fails.

- **Default**: `(ignore)`
- **Type**: Lisp expression
- **Returns**: Non-nil on success

**Examples:**
```elisp
;; Error message
:fallback-form (error "Cannot create or show buffer")

;; Alternative action
:fallback-form (progn
                 (message "Using fallback")
                 (switch-to-buffer "*scratch*"))
```

#### `:after-fallback` (expression)

Code to execute after successful fallback.

- **Default**: `(always)`

#### `:fallback-failed` (expression)

Code to execute if fallback fails.

- **Default**: `(message "[mk/toggler] Fallback failed!!")`

#### `:with-fallback-let` (binding list)

Local variable bindings for the fallback phase.

- **Default**: `()`

#### `:with-toggle-let` (binding list)

Global variable bindings for the entire toggle operation.

- **Default**: `((display-buffer-overriding-action '((display-buffer-use-some-window) (dedicated . t))))`
- **Scope**: Available throughout entire toggle operation

**Examples:**
```elisp
;; Override display action
:with-toggle-let ((display-buffer-overriding-action
                   '((display-buffer-at-bottom)
                     (window-height . 15))))

;; Multiple bindings
:with-toggle-let ((my-var 123)
                  (my-flag t)
                  (display-buffer-overriding-action
                   '((display-buffer-full-frame))))
```

#### `:with-buffer` (expression)

Code to execute in the context of a shown buffer (existing buffer).

- **Default**: `(ignore)`
- **Context**: Evaluated with `with-current-buffer _found-buffer`

#### `:with-new-buffer` (expression)

Code to execute in the context of a newly created buffer.

- **Default**: `(ignore)`
- **Context**: Evaluated with `with-current-buffer _new-buffer`

#### `:show-prms` (list)

Display parameters for showing buffers.

- **Default**: `'((display-buffer-use-some-window) (dedicated . t))`
- **Type**: Display action list
- **Note**: Usually overridden by `:with-toggle-let`

## Specialized Macros

### mk/buffer-toggler

Simplified macro for toggling named buffers.

```elisp
(mk/buffer-toggler NAME BUFFER-NAME &rest KEYWORD-ARGS)
```

#### Additional Keywords

- **`:display-action`**: Display action for the buffer
  - Default: `'((display-buffer-full-frame) (dedicated . t))`

#### Differences from mk/toggler

- Automatically sets `:with-toggle-let` with `display-buffer-overriding-action`
- Default `:make-form` is `(switch-to-buffer _buf)`
- All `mk/toggler` keywords still available

### mk/dired-toggler

Toggle dired buffers for directories.

```elisp
(mk/dired-toggler NAME DIRECTORY &rest KEYWORD-ARGS)
```

#### Additional Keywords

- **`:display-action`**: Display action for dired buffer
  - Default: `'((display-buffer-at-bottom) (dedicated . t))`

#### Differences from mk/toggler

- Default `:hide-if` checks for `dired-mode` and matching directory
- Default `:make-form` is `(dired _buf)`
- Uses `file-truename` for path comparison

### mk/file-toggler

Toggle specific files.

```elisp
(mk/file-toggler NAME FILE-PATH &rest KEYWORD-ARGS)
```

#### Additional Keywords

- **`:display-action`**: Display action for file buffer
  - Default: `'((display-buffer-at-bottom) (dedicated . t))`

#### Differences from mk/toggler

- Default `:hide-if` checks current buffer file against target file
- Default `:make-form` is `(find-file _buf)`
- Uses `file-truename` for path comparison

### mk/term-toggler

Toggle terminal buffers with special handling for remote connections.

```elisp
(mk/term-toggler NAME DIRECTORY &rest KEYWORD-ARGS)
```

#### Additional Keywords

- **`:with-term`**: Code to run in existing terminal
  - Default: `(always)`

- **`:with-new-term`**: Code to run in new terminal
  - Default: `(always)`

- **`:after-make-run`**: Shell command string to run after creating terminal
  - Default: `""`
  - Example: `"cd /var/www && ls\n"`

- **`:before-show-run`**: Shell command to run before showing terminal
  - Default: `""`

- **`:after-show-run`**: Shell command to run after showing terminal
  - Default: `""`

- **`:before-hide-run`**: Shell command to run before hiding terminal
  - Default: `""`

- **`:after-hide-run`**: Shell command to run after hiding terminal
  - Default: `""`

- **`:display-action`**: Display action for terminal
  - Default: `'((display-buffer-at-bottom) (dedicated . t))`

#### Special Behavior

1. **Remote Detection**: Automatically detects TRAMP paths
2. **Host Matching**: Finds terminals by hostname for remote connections
3. **Auto SSH**: Creates SSH connection for remote paths
4. **Auto CD**: Sends `cd` command to navigate to directory
5. **Buffer Naming**: Renames buffer with host information

## Variables and Scoping

### Internal Variables

These variables are available in your customization expressions:

- **`_buf`**: The toggle item (buffer name, file path, or directory)
- **`_found-buffer`**: Result of `:find-form` (buffer or nil)
- **`_prev-buffer_`**: The buffer you were in before toggling
- **`_new-buffer`**: The newly created buffer (in make phase)
- **`_`**: Placeholder for let bindings

### Buffer-Local Variables

- **`_toggler/prev-buffer_`**: Stores the buffer to return to when hiding
  - Set automatically when showing or creating a buffer
  - Used by default `:hide-form` to return to previous location

## Display Actions

Display actions control how and where buffers appear. They follow Emacs' `display-buffer` system.

### Common Display Action Functions

```elisp
;; Full frame
'((display-buffer-full-frame))

;; Bottom of frame
'((display-buffer-at-bottom)
  (window-height . 15))       ; Fixed height
  (window-height . 0.3))      ; Proportional height

;; Side window
'((display-buffer-in-side-window)
  (side . left)               ; left, right, top, bottom
  (window-width . 40))

;; Same window
'((display-buffer-same-window))

;; Use some window
'((display-buffer-use-some-window)
  (dedicated . t))
```

### Display Action Parameters

- **`window-height`**: Number (lines) or float (proportion)
- **`window-width`**: Number (columns) or float (proportion)
- **`side`**: Symbol (left, right, top, bottom)
- **`slot`**: Number for ordering side windows
- **`dedicated`**: Non-nil to mark window as dedicated
- **`window-parameters`**: Alist of window parameters

## Lifecycle Phases

### Complete Execution Flow

```
1. EVALUATE :with-toggle-let bindings
2. EVALUATE :find-form → _found-buffer

3. IF :hide-if is true:
   a. EVALUATE :with-hide-let bindings
   b. EXECUTE :before-hide
   c. EXECUTE :hide-form
   d. IF successful:
      - EXECUTE :after-hide
      - RETURN
   e. ELSE:
      - EXECUTE :hide-failed
      - RETURN nil

4. CAPTURE current buffer → _prev-buffer_

5. IF :show-if is true:
   a. EVALUATE :with-show-let bindings
   b. EXECUTE :before-show
   c. EXECUTE :show-form → result
   d. IF successful:
      - SWITCH to buffer context
      - SET buffer-local _toggler/prev-buffer_ = _prev-buffer_
      - EXECUTE :with-buffer
      - EXECUTE :after-show
      - RETURN result
   e. ELSE:
      - EXECUTE :show-failed
      - RETURN nil

6. IF :make-if is true:
   a. EVALUATE :with-make-let bindings
   b. EXECUTE :before-make
   c. EXECUTE :make-form → _new-buffer
   d. IF successful:
      - SWITCH to buffer context
      - SET buffer-local _toggler/prev-buffer_ = _prev-buffer_
      - EXECUTE :after-make
      - EXECUTE :with-new-buffer
      - RETURN _new-buffer
   e. ELSE:
      - EXECUTE :make-failed
      - RETURN nil

7. FALLBACK:
   a. EVALUATE :with-fallback-let bindings
   b. EXECUTE :before-fallback
   c. EXECUTE :fallback-form
   d. IF successful:
      - EXECUTE :after-fallback
   e. ELSE:
      - EXECUTE :fallback-failed
```

### Phase Dependencies

- **Hide phase**: Only needs `_buf`
- **Show phase**: Needs `_found-buffer` from find
- **Make phase**: Needs `_found-buffer` to be nil (or `:make-if` override)
- **Fallback phase**: Runs if show and make are skipped

## Best Practices

1. **Always test `:hide-if`**: Ensure it correctly identifies when you're viewing the toggled buffer
2. **Use `file-truename`**: For path comparisons to handle symlinks
3. **Return values**: `:hide-form`, `:show-form`, and `:make-form` should return non-nil on success
4. **Buffer context**: Remember that `:with-buffer` and `:with-new-buffer` run in the target buffer
5. **Cleanup**: Use `:before-hide` for any cleanup before switching away
6. **Error handling**: Customize `*-failed` expressions for better error messages
