# Contributing to mk-togglers

Thank you for your interest in contributing to mk-togglers! This document provides guidelines and information for contributors.

## Table of Contents

1. [Code of Conduct](#code-of-conduct)
2. [How Can I Contribute?](#how-can-i-contribute)
3. [Development Setup](#development-setup)
4. [Coding Standards](#coding-standards)
5. [Testing](#testing)
6. [Submitting Changes](#submitting-changes)

## Code of Conduct

This project adheres to a simple principle: be respectful and constructive in all interactions.

- Be welcoming to newcomers
- Be patient with questions
- Focus on what is best for the project
- Show empathy towards other community members

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check existing issues to avoid duplicates. When creating a bug report, include:

- **Clear title**: Descriptive summary of the issue
- **Steps to reproduce**: Minimal example showing the problem
- **Expected behavior**: What you expected to happen
- **Actual behavior**: What actually happened
- **Environment**: Emacs version, OS, relevant configuration
- **Code sample**: Minimal toggler definition that demonstrates the issue

**Example Bug Report:**

```
Title: Terminal toggler doesn't change directory on remote hosts

Steps to reproduce:
1. Create toggler: (mk/term-toggler my-term "/ssh:user@server:/var/www")
2. Call the toggler
3. Check current directory in terminal

Expected: Terminal should be in /var/www
Actual: Terminal stays in home directory

Environment:
- Emacs 29.1
- macOS 14.0
- TRAMP 2.6.2

Code:
(mk/term-toggler my-server-term "/ssh:deploy@example.com:/var/www"
  :display-action '((display-buffer-at-bottom)))
```

### Suggesting Enhancements

Enhancement suggestions are welcome! Please provide:

- **Clear use case**: Describe the problem you're trying to solve
- **Proposed solution**: How you envision the enhancement working
- **Alternatives considered**: Other approaches you've thought about
- **Example usage**: Show how the enhancement would be used

### Contributing Code

We welcome pull requests for:

- Bug fixes
- New features
- Documentation improvements
- Performance improvements
- Test coverage improvements

## Development Setup

### Prerequisites

- Emacs 26.1 or later (for `cl-defmacro` support)
- Git for version control

### Getting Started

1. Fork the repository
2. Clone your fork:
   ```bash
   git clone https://github.com/yourusername/mk-togglers.git
   cd mk-togglers
   ```

3. Create a feature branch:
   ```bash
   git checkout -b feature/my-enhancement
   ```

4. Load the library in Emacs:
   ```elisp
   (load-file "/path/to/mk-togglers/mk-togglers.el")
   ```

## Coding Standards

### Code Style

Follow standard Emacs Lisp conventions:

#### Naming

- Use `mk/` prefix for all public macros and functions
- Use descriptive names: `mk/buffer-toggler` not `mk/bt`
- Private helpers should use `mk//` prefix (double slash)
- Variables should use `mk-` prefix

#### Formatting

- Use 2-space indentation
- Maximum line length: 100 characters (docstrings and comments can be longer if needed)
- One blank line between top-level forms
- Align keyword arguments vertically when practical

**Good:**
```elisp
(mk/buffer-toggler my-scratch-toggle "*scratch*"
  :display-action '((display-buffer-at-bottom)
                    (window-height . 15))
  :after-show (goto-char (point-max)))
```

**Avoid:**
```elisp
(mk/buffer-toggler my-scratch-toggle "*scratch*" :display-action '((display-buffer-at-bottom) (window-height . 15)) :after-show (goto-char (point-max)))
```

#### Documentation

- All public macros and functions must have docstrings
- Use proper docstring format:
  - First line: Complete sentence ending with period
  - Blank line before detailed description
  - Document all parameters
  - Include usage examples for complex functions

**Example:**
```elisp
(cl-defmacro mk/buffer-toggler (name buffer-name &rest args)
  "Create a buffer toggler function named NAME for BUFFER-NAME.

This macro generates an interactive function that toggles the
visibility of a specific buffer. When called, it will either
show the buffer or hide it and return to the previous buffer.

NAME is the symbol name for the generated function.
BUFFER-NAME is the string name of the buffer to toggle.
ARGS are keyword arguments passed to `mk/toggler'.

Example usage:
  (mk/buffer-toggler my-scratch-toggle \"*scratch*\"
    :display-action '((display-buffer-at-bottom)))"
  ...)
```

### Macro Design Principles

1. **Preserve evaluation context**: Be careful about when arguments are evaluated
2. **Avoid name capture**: Use gensym or unique prefixes for internal variables
3. **Provide defaults**: All keyword arguments should have sensible defaults
4. **Fail gracefully**: Handle errors and provide helpful messages
5. **Be composable**: New macros should build on existing ones when possible

### Code Organization

The file structure follows this pattern:

```elisp
;;;;;;;;;;;;;;;;;;
;; SECTION NAME ;;
;;;;;;;;;;;;;;;;;;

;; Code for that section
```

Current sections:
1. Debugging macros (can be removed in production)
2. Toggle macros (core functionality)
3. Specialized togglers (buffer, dired, file, term)

## Testing

### Manual Testing

When making changes, test with:

1. **Basic togglers**: Ensure simple cases work
   ```elisp
   (mk/buffer-toggler test-scratch "*scratch*")
   ```

2. **Edge cases**: Test unusual configurations
   - Empty buffers
   - Killed buffers
   - Invalid paths
   - Remote paths (for term togglers)

3. **Lifecycle hooks**: Verify all phases execute correctly
   ```elisp
   (mk/buffer-toggler test-lifecycle "*test*"
     :before-show (message "before show")
     :after-show (message "after show")
     :before-hide (message "before hide")
     :after-hide (message "after hide"))
   ```

4. **Display actions**: Test various display configurations
   - Side windows
   - Bottom windows
   - Full frame
   - Multiple display actions

### Test Checklist

Before submitting changes, verify:

- [ ] Basic toggle (show/hide) works
- [ ] Creates buffer if it doesn't exist
- [ ] Returns to previous buffer on hide
- [ ] Display action is respected
- [ ] Lifecycle hooks execute in correct order
- [ ] Works with remote paths (for term toggler)
- [ ] Docstrings are complete and accurate
- [ ] No compiler warnings
- [ ] No byte-compile warnings

### Running Byte Compilation

Check for warnings:

```bash
emacs -batch -f batch-byte-compile mk-togglers.el
```

Should produce no warnings.

## Submitting Changes

### Commit Messages

Write clear, descriptive commit messages:

**Format:**
```
Short summary (50 chars or less)

More detailed explanation if needed. Wrap at 72 characters.
Explain the problem this commit solves and how.

- Bullet points are fine
- Use present tense: "Add feature" not "Added feature"
- Reference issues: "Fixes #123"
```

**Good examples:**
```
Add support for custom find functions in mk/toggler

Users can now provide custom :find-form expressions to control
how existing buffers are located. This enables matching buffers
by mode, properties, or other criteria beyond just buffer name.

Fixes #42
```

```
Fix terminal toggler directory navigation for remote hosts

The :after-show-run command now correctly parses TRAMP paths
and extracts the localname for the cd command.

Fixes #15
```

### Pull Request Process

1. **Update documentation**: If your change affects user-facing behavior
   - Update README.md
   - Update API.md for API changes
   - Add examples to EXAMPLES.md if appropriate

2. **Test thoroughly**: Follow the testing checklist above

3. **Create pull request**:
   - Clear title describing the change
   - Reference any related issues
   - Describe what changed and why
   - Include example usage if adding features
   - Note any breaking changes

4. **Respond to feedback**: Be open to suggestions and iterate

### Pull Request Template

```markdown
## Description
Brief description of what this PR does.

## Motivation
Why is this change needed? What problem does it solve?

## Changes
- List of changes made
- Each change on its own line

## Testing
How was this tested?

## Related Issues
Fixes #123
Relates to #456

## Breaking Changes
List any breaking changes, or write "None"

## Examples
If adding a feature, show how to use it
```

## Questions?

If you have questions about contributing:

1. Check existing documentation (README, API, EXAMPLES)
2. Search existing issues
3. Open a new issue with your question

Thank you for contributing to mk-togglers!
