;;; mk-togglers.el --- Create intelligent buffer togglers with memory -*- lexical-binding: t; -*-

;; Copyright (C) 2025 cMo

;; Author: cMo
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience, buffers, terminals
;; URL: https://github.com/cmoxiv/mk-togglers

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; mk-togglers provides a set of macros for creating intelligent buffer
;; togglers with memory and context awareness.
;;
;; Unlike simple show/hide commands, these togglers remember where you came
;; from, intelligently find or create buffers, and provide extensive
;; customization hooks for controlling behavior at every stage of the toggle
;; lifecycle.
;;
;; Features:
;; - Context Memory: Each toggled buffer remembers which buffer opened it
;; - Smart Buffer Management: Automatically finds or creates buffers
;; - Specialized Togglers: Pre-built macros for buffers, files, directories, terminals
;; - Remote-Aware: Terminal togglers handle remote (TRAMP) connections
;; - Extensive Customization: Hook into every lifecycle phase
;; - Flexible Display: Control how and where buffers appear
;;
;; Quick Start:
;;
;;   ;; Toggle scratch buffer
;;   (mk/buffer-toggler my-scratch-toggle "*scratch*")
;;   (global-set-key (kbd "C-c s") 'my-scratch-toggle)
;;
;;   ;; Toggle a file
;;   (mk/file-toggler my-todo-toggle "~/todo.org")
;;   (global-set-key (kbd "C-c t") 'my-todo-toggle)
;;
;;   ;; Toggle a terminal
;;   (mk/term-toggler my-term-toggle default-directory)
;;   (global-set-key (kbd "C-c C-t") 'my-term-toggle)
;;
;; For more examples and detailed documentation, see:
;; - README.md: Overview and installation
;; - QUICKSTART.md: 5-minute tutorial
;; - EXAMPLES.md: Real-world usage patterns
;; - API.md: Complete technical reference

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS FOR DEBUGGING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro logd_ (str var)
  "Internal debug logging macro.
Print debug message with format \"[STR] VAR=value\".
STR is a tag string, VAR is the variable to log."
  `(message (format "[%s] %s=%s" ,str ',var ,var)))

(defmacro logd (var &optional tag &rest body)
  "Debug log VAR with optional TAG, then execute BODY.
If TAG is provided, it's used as the log prefix, otherwise \"DEBUG\" is used.
Returns the result of BODY.

Example:
  (logd my-variable \"INFO\"
    (do-something))"
  `(progn (logd_ (or ,tag
		     "DEBUG") ,var)
	  ,@body))

(defvar my/tmp-var 123)

(logd 12 "logd")


;;;;;;;;;;;;;;;;;;;
;; TOGGLE MACROS ;;
;;;;;;;;;;;;;;;;;;;

(cl-defmacro mk/toggler (name toggle-item
			 &rest rest
			 &key
			   (hide-if		'(equal _buf (buffer-name)))
			   (before-hide		'(ignore))
			   (hide-form		'(or (and
						      (one-window-p)
						      (switch-to-buffer (or (bound-and-true-p _toggler/prev-buffer_)
									    (other-buffer))))
						  (let ((win (get-buffer-window)))
						    (pop-to-buffer (or (bound-and-true-p _toggler/prev-buffer_)
								       (other-buffer)))
						    (not (delete-window win)))))
			   (after-hide		'(always))
			   (hide-failed		'(message "[mk/toggler] Failed to hide buffer!!"))
			   (show-if		'_found-buffer)
			   (before-show		'(ignore))
			   (show-form		'(pop-to-buffer _found-buffer))
			   (after-show		'(always))
			   (show-failed		'(message "[mk/toggler] Failed to show buffer!!"))
			   (find-form		'(get-buffer _buf))
			   (make-if		'(not _found-buffer))
			   (before-make		'(ignore))
			   (make-form		'(switch-to-buffer _buf))
			   (after-make		'(always))
			   (make-failed		'(message "[mk/toggler] Failed to make buffer!!"))
			   (before-fallback	'(ignore))
			   (fallback-form	'(ignore))
			   (after-fallback	'(always))
			   (fallback-failed	'(message "[mk/toggler] Fallback failed!!"))
			   (with-toggle-let	'((display-buffer-overriding-action '((display-buffer-use-some-window)
										      (dedicated . t)))))
			   (with-hide-let	'())
			   (with-show-let	'())
			   (with-make-let	'())
			   (with-fallback-let	'())
			   (with-buffer		'(ignore))
			   (with-new-buffer	'(ignore))
			   (show-prms		'((display-buffer-use-some-window)
						  (dedicated . t)))
			   &allow-other-keys)
  "Create an intelligent buffer toggler function named NAME for TOGGLE-ITEM.

This is the foundational macro that generates toggle functions with extensive
customization options. The generated function intelligently shows, hides, or
creates buffers while remembering context.

NAME is the symbol name for the generated interactive function.
TOGGLE-ITEM is the default buffer name, file path, or directory to toggle.

The toggle lifecycle has four phases: HIDE, SHOW, MAKE, and FALLBACK.
Each phase has customizable conditions, actions, and hooks.

HIDE PHASE - When viewing the toggled buffer:
  :hide-if         - Condition to determine if hiding should occur.
                     Default: (equal _buf (buffer-name))
  :before-hide     - Code to run before hiding.
  :hide-form       - Expression that performs the hide action.
                     Should return non-nil on success.
                     Default: Switch to previous buffer or other-buffer.
  :after-hide      - Code to run after successful hide.
  :hide-failed     - Code to run if hide fails.
  :with-hide-let   - Local variable bindings for hide phase.

FIND PHASE - Search for existing buffer:
  :find-form       - Expression to find existing buffer.
                     Result stored in _found-buffer.
                     Default: (get-buffer _buf)

SHOW PHASE - Display an existing buffer:
  :show-if         - Condition to show existing buffer.
                     Default: _found-buffer
  :before-show     - Code to run before showing.
  :show-form       - Expression that displays the buffer.
                     Should return the buffer on success.
                     Default: (pop-to-buffer _found-buffer)
  :after-show      - Code to run after successful show.
                     Evaluated in context of shown buffer.
  :show-failed     - Code to run if show fails.
  :with-show-let   - Local variable bindings for show phase.
  :with-buffer     - Code to run in shown buffer context.

MAKE PHASE - Create a new buffer:
  :make-if         - Condition to create new buffer.
                     Default: (not _found-buffer)
  :before-make     - Code to run before creating.
  :make-form       - Expression that creates the buffer.
                     Should return the new buffer on success.
                     Default: (switch-to-buffer _buf)
  :after-make      - Code to run after successful creation.
                     Evaluated in context of new buffer.
  :make-failed     - Code to run if creation fails.
  :with-make-let   - Local variable bindings for make phase.
  :with-new-buffer - Code to run in new buffer context.

FALLBACK PHASE - If show and make are both skipped:
  :before-fallback - Code to run before fallback.
  :fallback-form   - Expression for fallback action.
  :after-fallback  - Code to run after successful fallback.
  :fallback-failed - Code to run if fallback fails.
  :with-fallback-let - Local variable bindings for fallback phase.

GLOBAL SETTINGS:
  :with-toggle-let - Variable bindings for entire toggle operation.
                     Default: Sets display-buffer-overriding-action.
  :show-prms       - Display parameters (mostly overridden by :with-toggle-let).

AVAILABLE VARIABLES in customization expressions:
  _buf             - The toggle item (buffer name/file/directory)
  _found-buffer    - Result of :find-form
  _prev-buffer_    - Buffer you were in before toggling
  _new-buffer      - Newly created buffer (in make phase)
  _toggler/prev-buffer_ - Buffer-local variable storing return destination

CONTEXT MEMORY:
When a buffer is shown or created, the current buffer is stored in the
target buffer's local variable `_toggler/prev-buffer_'. When hiding,
the toggler returns to this saved buffer.

Example:
  (mk/toggler my-scratch-toggle \"*scratch*\"
    :display-action \\='((display-buffer-at-bottom)
                      (window-height . 15))
    :after-show (goto-char (point-max)))

For simpler usage, see `mk/buffer-toggler', `mk/file-toggler',
`mk/dired-toggler', and `mk/term-toggler'.

See API.md for complete documentation of all parameters."
  `(cl-defun ,name (&optional (_buf ,toggle-item))
     (interactive)
     ;; Override vars
     (let* ((_ nil)			               ; override vars
	    (_found-buffer ,find-form)
	    ,@with-toggle-let)
       (if ,hide-if				       ; hide-if
	   (let* ((_ nil) ,@with-hide-let)
	     (if (progn
		   ,before-hide
		   ,hide-form)
		 (progn
		   ,after-hide
		   ;; (logd _toggler/prev-buffer_)
		   ;; (pop-to-buffer _toggler/prev-buffer_)
		   )
	       ,hide-failed
	       nil))
	 
	 (let ((_prev-buffer_ (current-buffer)))
	   (if ,show-if				       ; show-if
	       (let* ((_ nil) ,@with-show-let)
		 (if-let ((_found-buffer (progn
					   ,before-show
					   ,show-form)))
		     (with-current-buffer _found-buffer
		       (setq-local _toggler/prev-buffer_ _prev-buffer_)
		       ,with-buffer
		       ,after-show)
		   ,show-failed
		   nil))
	     (if ,make-if				       ; make-if
		 (let* ((_ nil) ,@with-make-let)
		   (if-let ((_new-buffer (progn
					   ,before-make
					   ,make-form)))
		       (with-current-buffer _new-buffer
			 (setq-local _toggler/prev-buffer_ _prev-buffer_)
			 ,after-make
			 ,with-new-buffer)
		     ,make-failed
		     nil))
	       (let* ((_ nil) ,@with-fallback-let)       ; fallback
		 (if (progn
		       ,before-fallback
		       ,fallback-form)
		     ,after-fallback
		   ,fallback-failed)))))))))




(cl-defmacro mk/buffer-toggler (name buffer-name
				&rest args
				&key
				  (make-form		'(switch-to-buffer _buf))
				  (with-toggle-let	'())
				  (display-action	''((display-buffer-full-frame)
							   (dedicated . t)))
				  &allow-other-keys)
  "Create a buffer toggler function named NAME for BUFFER-NAME.

This is a simplified wrapper around `mk/toggler' specialized for toggling
named buffers (like *scratch*, *Messages*, etc.).

NAME is the symbol name for the generated interactive function.
BUFFER-NAME is the string name of the buffer to toggle.

Keyword arguments:
  :make-form       - How to create the buffer if it doesn't exist.
                     Default: (switch-to-buffer _buf)
  :display-action  - Display action controlling where buffer appears.
                     Default: Full frame, dedicated window.
  :with-toggle-let - Additional variable bindings for toggle operation.

All keyword arguments from `mk/toggler' are also accepted.

The toggler will:
1. If viewing BUFFER-NAME: hide it and return to previous buffer
2. If BUFFER-NAME exists: show it and remember current location
3. If BUFFER-NAME doesn't exist: create it

Display action examples:
  Bottom of frame with 15 lines:
    :display-action \\='((display-buffer-at-bottom)
                      (window-height . 15))

  Right sidebar with 80 columns:
    :display-action \\='((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 80))

  Full frame:
    :display-action \\='((display-buffer-full-frame))

Example:
  (mk/buffer-toggler my-scratch-toggle \"*scratch*\"
    :display-action \\='((display-buffer-at-bottom)
                      (window-height . 15))
    :after-show (goto-char (point-max)))

  (global-set-key (kbd \"C-c s\") \\='my-scratch-toggle)"
  `(mk/toggler ,name ,buffer-name
	       :make-form		,make-form
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))


(cl-defmacro mk/dired-toggler (name directory
			       &rest args
			       &key
				 (hide-if		'(and (derived-mode-p 'dired-mode)
							  (equal
							   (file-truename default-directory)
							   (file-truename _buf))))
				 (make-form		'(dired _buf))
				 (with-toggle-let	'())
				 (display-action	''((display-buffer-at-bottom)
							   (dedicated . t)))
				 &allow-other-keys)
  "Create a dired toggler function named NAME for DIRECTORY.

This is a wrapper around `mk/toggler' specialized for toggling dired buffers
that browse specific directories.

NAME is the symbol name for the generated interactive function.
DIRECTORY is the directory path to toggle.

Keyword arguments:
  :hide-if         - Condition to hide the dired buffer.
                     Default: In dired-mode viewing the target directory.
  :make-form       - How to create the dired buffer.
                     Default: (dired _buf)
  :display-action  - Display action controlling where dired appears.
                     Default: Bottom of frame, dedicated window.
  :with-toggle-let - Additional variable bindings for toggle operation.

All keyword arguments from `mk/toggler' are also accepted.

The toggler uses `file-truename' for directory comparison, so it correctly
handles symbolic links and relative paths.

Example:
  (mk/dired-toggler my-projects-toggle \"~/projects/\"
    :display-action \\='((display-buffer-in-side-window)
                      (side . left)
                      (window-width . 40)))

  (global-set-key (kbd \"C-c p\") \\='my-projects-toggle)"
  `(mk/toggler ,name ,directory
	       :hide-if		,hide-if
	       :make-form	,make-form
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))



(cl-defmacro mk/file-toggler (name file-path
			      &rest args
			      &key
				(hide-if		'(and (buffer-file-name)
							  (equal
							   (file-truename (buffer-file-name))
							   (file-truename _buf))))
				(make-form		'(find-file _buf))
				(with-toggle-let	'())
				(display-action	''((display-buffer-at-bottom)
						   (dedicated . t)))
				&allow-other-keys)
  "Create a file toggler function named NAME for FILE-PATH.

This is a wrapper around `mk/toggler' specialized for toggling specific files.

NAME is the symbol name for the generated interactive function.
FILE-PATH is the path to the file to toggle.

Keyword arguments:
  :hide-if         - Condition to hide the file buffer.
                     Default: Current buffer is visiting the target file.
  :make-form       - How to open the file.
                     Default: (find-file _buf)
  :display-action  - Display action controlling where file appears.
                     Default: Bottom of frame, dedicated window.
  :with-toggle-let - Additional variable bindings for toggle operation.

All keyword arguments from `mk/toggler' are also accepted.

The toggler uses `file-truename' for file comparison, so it correctly
handles symbolic links and relative paths.

Useful for quick access to frequently used files like TODO lists, notes,
configuration files, etc.

Example:
  (mk/file-toggler my-todo-toggle \"~/org/todo.org\"
    :display-action \\='((display-buffer-at-bottom)
                      (window-height . 20))
    :after-show (org-overview))

  (global-set-key (kbd \"C-c t\") \\='my-todo-toggle)"
  `(mk/toggler ,name ,file-path
	       :hide-if		,hide-if
	       :make-form		,make-form
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))


(cl-defmacro mk/term---toggler
    (name directory
     &rest args
     &key
       (hide-if		'(derived-mode-p 'term-mode 'comint-mode))
       (find-form		'(car (remove-if-not
				       (lambda (bf)
					 (with-current-buffer bf
					   (and (derived-mode-p 'term-mode)
						(if (and (file-remote-p (file-truename (or _buf default-directory)))
							 (file-remote-p (file-truename default-directory)))
						    (progn
						      ;; (logd bf)
						      ;; (logd _buf)
						      ;; (logd default-directory)
						      ;; (logd (file-truename (buffer-file-name bf)))
						      ;; (logd (file-truename (buffer-file-name _buf)))
						      ;; (logd (file-truename default-directory))
						      (with-parsed-tramp-file-name (file-truename (or _buf default-directory)) _buf
							(with-parsed-tramp-file-name (file-truename default-directory) _def
							  (equal _buf-host  _def-host))))
						  (equal (file-truename (or _buf
									    default-directory))
							 (file-truename default-directory))))))
				       (buffer-list))))
       (make-form		'(let ((term-args (if (file-remote-p _buf)
						      (progn (logd _buf)
							     `("-c" ,(with-parsed-tramp-file-name (file-truename _buf) tramp
								       (format "ssh %s@%s -p%s -Y" tramp-user tramp-host (or tramp-port 22)))))
						    '()))
				       (term-name default-directory))
				  (let ((term-buffer (apply #'make-term `(,term-name "bash" nil ,@term-args))))
				    (pop-to-buffer term-buffer)
				    (term-mode)
				    (term-char-mode)
				    term-buffer)))
       (with-new-buffer		'(if (file-remote-p default-directory)
				  (progn (logd default-directory)
					 (term-send-raw-string
					  (with-parsed-tramp-file-name default-directory tramp
					    (format "cd %s\n" tramp-localname))))
				  (add-hook 'after-change-functions
				   (defun my/rename-term-to-path (_ _ _)
				     (rename-buffer
				      (format "*%s*"
					      (setq-local _toggler/default-directory_
							  (if (file-remote-p default-directory)
							      (with-parsed-tramp-file-name (file-truename default-directory) tramp
								(format "/%s:%s@%s#%s" tramp-method  tramp-user tramp-host
									(or tramp-port 22) ; tramp-localname
									))
							    (file-truename default-directory)))) t)) nil t)))
       (with-buffer		'(always))
       (with-term		'(always))
       (with-new-term		'(always))
       (after-make		'(always))
       (before-show		'(and
				  (logd default-directory)
				  (always)))
       (after-show		'(term-send-raw-string
				  (and
				   (logd _buf)
				   (format "cd %s\n"
				    (if (file-remote-p _buf)
					(progn (logd _buf)
					       (with-parsed-tramp-file-name _buf tramp
						 (setq-local _toggler/default-directory_
							     (format "/%s:%s@%s#%s" tramp-method  tramp-user tramp-host
								     (or tramp-port 22) ; tramp-localname
								     ))
						 tramp-localname))
				      (setq-local _toggler/default-directory_ default-directory))))))
       (before-hide		'(always))
       (after-hide		'(always))
       (after-make-run	"")
       (before-show-run	"")
       (after-show-run	"")
       (before-hide-run	"")
       (after-hide-run	"")
       (with-toggle-let	'())
       (display-action	''((display-buffer-at-bottom)
			   (dedicated . t)))
       &allow-other-keys)
  "Internal implementation for terminal togglers with remote host support.

This is an internal macro used by `mk/term-toggler'. Users should typically
use `mk/term-toggler' instead of calling this directly.

Creates a terminal toggler function named NAME for DIRECTORY with extensive
support for remote (TRAMP) connections and terminal lifecycle management.

NAME is the symbol name for the generated interactive function.
DIRECTORY is the directory path (local or remote TRAMP path).

Special keyword arguments for terminal control:
  :with-term        - Code to run in context of existing terminal.
  :with-new-term    - Code to run in context of newly created terminal.
  :after-make-run   - Shell command string to run after creating terminal.
  :before-show-run  - Shell command string to run before showing terminal.
  :after-show-run   - Shell command string to run after showing terminal.
  :before-hide-run  - Shell command string to run before hiding terminal.
  :after-hide-run   - Shell command string to run after hiding terminal.

Remote host handling:
- Automatically detects TRAMP paths using `file-remote-p'
- Matches terminals by hostname for remote connections
- Creates SSH connection when making new remote terminals
- Sends `cd' commands to navigate to correct directory
- Renames terminal buffer with host information

The :find-form is customized to intelligently match terminals:
- For remote paths: matches by hostname (all paths on same host use same terminal)
- For local paths: matches by exact directory path

The :make-form handles both local and remote terminal creation:
- Local: creates terminal with bash
- Remote: creates terminal with SSH connection to remote host

See `mk/term-toggler' for the public interface and usage examples."
  `(mk/toggler ,name ,directory
	       :hide-if		,hide-if
	       :find-form		,find-form
	       :make-form		,make-form
	       :with-new-buffer	(progn
				  ,with-new-buffer
				  ,with-new-term)
	       :with-buffer	(progn
				  ,with-buffer
				  ,with-term)
	       :after-make	(progn
				  ;; ,with-new-term
				  (let ((proc (get-buffer-process _new-buffer)))
				    (term-send-string proc ,after-make-run))
				  ,after-make)
	       :before-show	(progn
				  (let ((proc (get-buffer-process _found-buffer)))
				    (term-send-string proc ,before-show-run))
				  ,before-show)
	       :after-show	(progn
				  ;; ,with-term
				  (let ((proc (get-buffer-process _found-buffer)))
				    (term-send-string proc ,after-show-run))
				  ,after-show)
	       :before-hide	(progn ;before-hide
				  (let ((proc (get-buffer-process (current-buffer))))
				    (term-send-string proc ,before-hide-run))
				  ,before-hide)
	       :after-hide	(progn
				  (let ((proc (get-buffer-process _found-buffer)))
				    (term-send-string proc ,after-hide-run))
				  ,after-hide)
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))

(cl-defmacro mk/term-toggler (name directory
			      &rest args
			      &key
				(with-term		'(always))
				(with-new-term		'(always))
				(after-make-run		"")
				(before-show-run	"")
				(after-show-run		"")
				(before-hide-run	"")
				(after-hide-run		"")
				(display-action		''((display-buffer-at-bottom)
							   (dedicated . t)))
				&allow-other-keys)
  "Create a terminal toggler function named NAME for DIRECTORY.

This is a wrapper around `mk/toggler' specialized for toggling terminal
buffers with intelligent support for both local and remote (TRAMP) connections.

NAME is the symbol name for the generated interactive function.
DIRECTORY is the directory path (local or remote). Use `default-directory'
          to create a terminal in the current buffer's directory.

Keyword arguments:
  :with-term        - Code to run in existing terminal context.
  :with-new-term    - Code to run in newly created terminal context.
  :after-make-run   - Shell command string to run after creating terminal.
                      Example: \"source .venv/bin/activate\\n\"
  :before-show-run  - Shell command string to run before showing terminal.
  :after-show-run   - Shell command string to run after showing terminal.
                      Example: \"clear && git status\\n\"
  :before-hide-run  - Shell command string to run before hiding terminal.
  :after-hide-run   - Shell command string to run after hiding terminal.
  :display-action   - Display action controlling where terminal appears.
                      Default: Bottom of frame, dedicated window.

All keyword arguments from `mk/toggler' are also accepted.

REMOTE HOST SUPPORT:
When DIRECTORY is a TRAMP path (e.g., \"/ssh:user@host:/path/\"):
- Automatically creates SSH connection to remote host
- Matches terminals by hostname (not full path)
- Sends `cd' command to navigate to the remote directory
- Multiple paths on same host will reuse the same terminal

LOCAL DIRECTORY SUPPORT:
When DIRECTORY is a local path:
- Creates terminal with bash in the specified directory
- Matches terminals by exact directory path

SHELL COMMANDS:
The *-run keyword arguments accept shell command strings that are sent
to the terminal at various lifecycle points. Always end commands with \\n.

Examples:

  Basic terminal in current directory:
    (mk/term-toggler my-term-toggle default-directory)
    (global-set-key (kbd \"C-c C-t\") \\='my-term-toggle)

  Development terminal with virtual environment:
    (mk/term-toggler my-dev-term \"~/projects/myapp/\"
      :after-make-run \"source .venv/bin/activate\\n\"
      :display-action \\='((display-buffer-at-bottom)
                        (window-height . 20)))

  Remote server terminal:
    (mk/term-toggler my-server-term \"/ssh:user@example.com:/var/www/\"
      :after-make-run \"cd /var/www && ls -la\\n\")

  Terminal running ranger file manager:
    (mk/term-toggler my-ranger-toggle default-directory
      :after-make-run \"ranger\\n\"
      :display-action \\='((display-buffer-full-frame)))

  Git terminal with status on show:
    (mk/term-toggler my-git-term default-directory
      :after-show-run \"clear && git status\\n\")"

  `(mk/term---toggler ,name ,directory
		      :with-term	,with-term      
		      :with-new-term	,with-new-term  	
		      :after-make-run	,after-make-run 	
		      :before-show-run	,before-show-run	
		      :after-show-run	,after-show-run 	
		      :before-hide-run	,before-hide-run	
		      :after-hide-run	,after-hide-run 	
		      :display-action	,display-action
		      ,@args
		      ))



(provide 'mk-togglers)

;;; mk-togglers.el ends here
