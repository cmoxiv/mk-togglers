
(cl-defmacro mk/toggler (name toggle-item
			 &rest rest
			 &key
			   (hide-if		'(equal _buf (buffer-name)))
			   (before-hide		'(ignore))
			   (hide-form		'(or (and (one-window-p)
						      (switch-to-buffer (other-buffer)))
						  (not (delete-window))))
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
		 ,after-hide
	       ,hide-failed
	       nil))
	 (if ,show-if				       ; show-if
	     (let* ((_ nil) ,@with-show-let)
	       (if-let ((_found-buffer (progn
					 ,before-show
					 ,show-form)))
		   (with-current-buffer _found-buffer
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
		       ,after-make
		       ,with-new-buffer)
		   ,make-failed
		   nil))
	     (let* ((_ nil) ,@with-fallback-let)       ; fallback
	       (if (progn
		     ,before-fallback
		     ,fallback-form)
		   ,after-fallback
		 ,fallback-failed))))))))



(when nil
  (define-key global-map (kbd "C-z")
    (mk/toggler tmp/dashboard-toggler "*dashboard*"
      :hide-if		(equal _buf (buffer-name))
      :before-hide	(message "Called before hide!!")
      :hide-form		(switch-to-buffer (other-buffer))
      :after-hide		(message "Called after hide!! %s" `(,x ,y))
      :hide-failed	(message "HIDE FAILED")
      :find-form		(get-buffer _buf)
      :before-show	(message "Called before show!!")
      :show-if		(not (not _found-buffer))
      :show-form		(pop-to-buffer _found-buffer)
      :after-show		(message "Called after show!!")
      :show-failed	(message "SHOW FAILED")
      :before-make	(message "Called before make!!")
      :make-if		(not _found-buffer)
      :make-form		(dashboard-open)
      :after-make		(message "Called after make!! %s" `(,z1 ,z2))
      :make-failed	(message "MAKE FAILED")
      :before-fallback	(message "Called before fallback!!")
      :fallback-form	(message "Toggling fallbacked!!!!")
      :after-fallback	(message "Called after fallback!!")
      :fallback-failed	(message "FALLBACK FAILED")
      :with-new-buffer	(message "BUFFER-NAME: %s" (buffer-name))
      :with-toggle-let	((x 12)
			 (y "abc"))
      :with-make-let	((z1 123)
			 (z2 12.3))
      :show-prms		'((display-buffer-full-frame)
				  (dedicated . t)))))


(cl-defmacro mk/buffer-toggler (name buffer-name
				&rest args
				&key
				  (make-form		'(switch-to-buffer _buf))
				  (display-action	''((display-buffer-full-frame)
							   (dedicated . t)))
				  &allow-other-keys)
  `(mk/toggler ,name ,buffer-name
     :make-form		,make-form
     :with-toggle-let	((display-buffer-overriding-action ,display-action))
     ,@args))

(when nil
  (define-key global-map (kbd "C-z")
    (mk/buffer-toggler my/dashboard "*dashboard*"))
  (define-key global-map (kbd "C-z")
    (mk/buffer-toggler my/scratch "*scratch*")))


(cl-defmacro mk/dired-toggler (name directory
			       &rest args
			       &key
				 (hide-if		'(and (derived-mode-p 'dired-mode)
							  (equal
							   (file-truename default-directory)
							   (file-truename _buf))))
				 (make-form		'(dired _buf))
				 (display-action	''((display-buffer-at-bottom)
							   (dedicated . t)))
				 &allow-other-keys)
  `(mk/toggler ,name ,directory
     :hide-if		,hide-if
     :make-form		,make-form
     :with-toggle-let	((display-buffer-overriding-action ,display-action))
     ,@args))

(when nil
  (define-key global-map (kbd "C-z")
    (mk/dired-toggler my/dired default-directory
      :before-hide (message "before hide"))))


(cl-defmacro mk/file-toggler (name file-path
			      &rest args
			      &key
				(hide-if		'(and (buffer-file-name)
							  (equal
							   (file-truename (buffer-file-name))
							   (file-truename _buf))))
				(make-form		'(find-file _buf))
				(display-action	''((display-buffer-at-bottom)
						   (dedicated . t)))
				&allow-other-keys)
  `(mk/toggler ,name ,file-path
     :hide-if		,hide-if
     :make-form		,make-form
     :with-toggle-let	((display-buffer-overriding-action ,display-action))
     ,@args))

(when nil
  (define-key global-map (kbd "C-z")
    (mk/file-toggler my/scratch-file "~/scratch.org"
      :before-hide (message "before hide!!")
      :after-hide  (message "after hide!!"))))



(cl-defmacro mk/term---toggler (name directory
				&rest args
				&key
				  (hide-if		'(and (derived-mode-p 'term-mode 'comint-mode)
							  (equal
							   (file-truename default-directory)
							   (file-truename _buf))))
				  (find-form		'(car (remove-if-not
							       (lambda (bf)
								 (with-current-buffer bf
								   (and (derived-mode-p 'term-mode) 
									(equal (file-truename (or _buf
												  default-directory))
									       (file-truename default-directory)))))
							       (buffer-list))))
				  (make-form		'(let ((term-args (if (file-remote-p _buf)
									      `("-c" ,(with-parsed-tramp-file-name (file-truename _buf) tramp
											(format "ssh %s@%s -p%s -Y" tramp-user tramp-host (or tramp-port 22))))
									    '()))
							       (term-name default-directory))
							  (let ((term-buffer (apply #'make-term `(,term-name "bash" nil ,@term-args))))
							    (pop-to-buffer term-buffer)
							    (term-mode)
							    (term-char-mode)
							    term-buffer)))
				  (with-new-buffer	'(progn
							  (add-hook 'after-change-functions
							   (defun my/rename-term-to-path (_ _ _)
							     (rename-buffer
							      (format "*%s*"
								      (if (file-remote-p default-directory)
									  (with-parsed-tramp-file-name (file-truename default-directory) tramp
									    (format "/%s:%s@%s#%s:%s" tramp-method  tramp-user tramp-host
										    (or tramp-port 22)
										    tramp-localname))
									(file-truename default-directory))) t)) nil t)))
				  (with-buffer		'(always))
				  (with-term		'(always))
				  (with-new-term	'(always))
				  (after-make		'(always))
				  (before-show		'(always))
				  (after-show		'(always))
				  (before-hide		'(always))
				  (after-hide		'(always))
				  (after-make-run	"")
				  (before-show-run	"")
				  (after-show-run	"")
				  (before-hide-run	"")
				  (after-hide-run	"")
				  (display-action	''((display-buffer-at-bottom)
							   (dedicated . t)))
				  &allow-other-keys)
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
     :with-toggle-let	((display-buffer-overriding-action ,display-action))
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

(when nil
  (define-key global-map (kbd "C-z")
    (mk/term-toggler my/toggle-bash-term default-directory
      :with-new-term	(keymap-local-set "C-z" #'my/ansi-term)))

  (define-key global-map (kbd "C-M-z")
    (mk/term-toggler my/toggle-ranger-term default-directory
      :with-new-term	(keymap-local-set "C-M-z" #'my/ansi-term)
      :after-make-run	"ranger; exit\n")))

(provide 'mk-togglers)
