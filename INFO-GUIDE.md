# Info Documentation Guide

This document explains how to build and use the mk-togglers Info manual.

## What is Info?

Info is Emacs' built-in documentation system. It provides a hierarchical, hyperlinked documentation format that integrates seamlessly with Emacs. Once installed, you can access the mk-togglers manual directly from within Emacs.

## Building the Documentation

### Requirements

To build the Info documentation, you need:
- `makeinfo` (part of the texinfo package)
- For PDF output: `texi2pdf` (also from texinfo)

Install on macOS:
```bash
brew install texinfo
```

Install on Ubuntu/Debian:
```bash
sudo apt-get install texinfo
```

Install on Fedora/RHEL:
```bash
sudo dnf install texinfo
```

### Build Commands

```bash
# Build Info file
make info

# Build PDF manual
make pdf

# Build both Info and compile Emacs Lisp
make all

# Just check for compilation warnings
make check
```

## Installing the Documentation

### System-wide Installation

```bash
sudo make install
```

This installs to `/usr/local` by default:
- Info file: `/usr/local/share/info/mk-togglers.info`
- Emacs Lisp: `/usr/local/share/emacs/site-lisp/mk-togglers/`

### User Installation

To install to your home directory:

```bash
make PREFIX=~/.local install
```

Then add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
;; Add to load path
(add-to-list 'load-path "~/.local/share/emacs/site-lisp/mk-togglers")
(require 'mk-togglers)

;; Add Info directory (if not automatically detected)
(add-to-list 'Info-directory-list "~/.local/share/info")
```

### Manual Installation

If you just want to try the Info manual without installing:

```bash
# Build the Info file
make info

# View it in Emacs
emacs -q --eval "(info \"./mk-togglers.info\")"
```

Or from within Emacs:
```
C-u C-h i ./mk-togglers.info RET
```

## Using the Info Manual in Emacs

### Accessing the Manual

Once installed, access the manual in several ways:

**Method 1: From Info directory**
```
C-h i                     ; Open Info
m mk-togglers RET        ; Jump to mk-togglers manual
```

**Method 2: Direct access**
```
C-h i m mk-togglers RET  ; Open mk-togglers manual directly
```

**Method 3: Search**
```
C-h i
i mk/toggler RET         ; Search index for "mk/toggler"
```

### Navigation Keys

Once in the Info manual:

| Key | Action |
|-----|--------|
| `n` | Next node |
| `p` | Previous node |
| `u` | Up to parent node |
| `l` | Go back (last node visited) |
| `t` | Top of manual |
| `SPC` | Scroll down |
| `DEL` | Scroll up |
| `m` | Jump to menu item |
| `f` | Follow cross-reference |
| `i` | Search index |
| `s` | Search text |
| `g` | Go to node by name |
| `q` | Quit Info |
| `?` | Show all Info commands |

### Example Navigation Session

```
C-h i m mk-togglers RET     ; Open manual
m Quick Start RET           ; Jump to Quick Start chapter
n                           ; Next section
p                           ; Previous section
u                           ; Up to chapter level
t                           ; Back to top
i mk/buffer-toggler RET     ; Search index for mk/buffer-toggler
f mk/toggler RET            ; Follow cross-reference
l                           ; Go back
q                           ; Quit
```

## Info Manual Structure

The mk-togglers manual is organized as follows:

```
mk-togglers (Top)
├── Introduction
│   └── Features
├── Installation
│   ├── Manual Installation
│   └── Using use-package
├── Quick Start
│   ├── Toggle a Buffer
│   ├── Toggle a File
│   ├── Toggle a Directory
│   └── Toggle a Terminal
├── Concepts
│   ├── The Toggle Lifecycle
│   ├── Buffer-Local Memory
│   └── Variables in Scope
├── Macro Reference
│   ├── mk/toggler
│   ├── mk/buffer-toggler
│   ├── mk/dired-toggler
│   ├── mk/file-toggler
│   └── mk/term-toggler
├── Examples
│   ├── Basic Togglers
│   ├── Workflow Togglers
│   ├── Development Environment
│   └── Custom Display
├── Troubleshooting
└── Index
```

## Searching the Manual

### Index Search

The fastest way to find information:

```
C-h i m mk-togglers RET
i term RET              ; Search index for "term"
,                       ; Next index entry
,                       ; Next index entry
```

### Text Search

Search for text anywhere in the manual:

```
C-h i m mk-togglers RET
s TRAMP RET            ; Search for "TRAMP"
C-s                    ; Next occurrence (isearch)
```

## Advantages of Info Documentation

1. **Integrated with Emacs**: No need to leave your editor
2. **Fast Navigation**: Hyperlinked cross-references
3. **Indexed**: Quick lookup of functions and concepts
4. **Searchable**: Full-text search capabilities
5. **Hierarchical**: Easy to browse by topic
6. **Offline**: No internet connection required

## Uninstalling

To remove the Info documentation and Emacs Lisp:

```bash
sudo make uninstall
```

Or for user installation:

```bash
make PREFIX=~/.local uninstall
```

## PDF Alternative

If you prefer PDF documentation:

```bash
make pdf
open mk-togglers.pdf    # macOS
xdg-open mk-togglers.pdf  # Linux
```

The PDF contains the same content as the Info manual but in a traditional document format.

## Troubleshooting

### Info file not found

If Emacs can't find the Info file after installation:

```elisp
;; Add to your init file
(add-to-list 'Info-directory-list "/usr/local/share/info")
;; Or for user installation:
(add-to-list 'Info-directory-list "~/.local/share/info")
```

### Manual not in Info directory menu

Rebuild the Info directory:

```bash
cd /usr/local/share/info  # or your installation directory
sudo install-info mk-togglers.info dir
```

### makeinfo not found

Install the texinfo package (see Requirements section above).

## Further Reading

- Emacs Info manual: `C-h i m info RET`
- Texinfo manual: `C-h i m texinfo RET`
- GNU Info documentation: https://www.gnu.org/software/texinfo/
