
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MACROS FOR DEBUGGING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro logd_ (str var)
  `(message (format "[%s] %s=%s" ,str ',var ,var)))

(defmacro logd (var &optional tag &rest body)
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



;; (when nil
;;   (define-key global-map (kbd "C-z")
;;     (mk/toggler tmp/dashboard-toggler "*dashboard*"
;;       :hide-if		(equal _buf (buffer-name))
;;       :before-hide	(message "Called before hide!!")
;;       :hide-form		(switch-to-buffer (other-buffer))
;;       :after-hide		(message "Called after hide!! %s" `(,x ,y))
;;       :hide-failed	(message "HIDE FAILED")
;;       :find-form		(get-buffer _buf)
;;       :before-show	(message "Called before show!!")
;;       :show-if		(not (not _found-buffer))
;;       :show-form		(pop-to-buffer _found-buffer)
;;       :after-show		(message "Called after show!!")
;;       :show-failed	(message "SHOW FAILED")
;;       :before-make	(message "Called before make!!")
;;       :make-if		(not _found-buffer)
;;       :make-form		(dashboard-open)
;;       :after-make		(message "Called after make!! %s" `(,z1 ,z2))
;;       :make-failed	(message "MAKE FAILED")
;;       :before-fallback	(message "Called before fallback!!")
;;       :fallback-form	(message "Toggling fallbacked!!!!")
;;       :after-fallback	(message "Called after fallback!!")
;;       :fallback-failed	(message "FALLBACK FAILED")
;;       :with-new-buffer	(message "BUFFER-NAME: %s" (buffer-name))
;;       :with-toggle-let	((x 12)
;; 			 (y "abc"))
;;       :with-make-let	((z1 123)
;; 			 (z2 12.3))
;;       :show-prms		'((display-buffer-full-frame)
;; 				  (dedicated . t)))))


(cl-defmacro mk/buffer-toggler (name buffer-name
				&rest args
				&key
				  (make-form		'(switch-to-buffer _buf))
				  (with-toggle-let	'())
				  (display-action	''((display-buffer-full-frame)
							   (dedicated . t)))
				  &allow-other-keys)
  `(mk/toggler ,name ,buffer-name
	       :make-form		,make-form
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))

;; (when nil
;;   (define-key global-map (kbd "C-z")
;;     (mk/buffer-toggler my/dashboard "*dashboard*"))
;;   (define-key global-map (kbd "C-z")
;;     (mk/buffer-toggler my/scratch "*scratch*")))


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
  `(mk/toggler ,name ,directory
	       :hide-if		,hide-if
	       :make-form	,make-form
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))

;; (when nil
;;   (define-key global-map (kbd "C-z")
;;     (mk/dired-toggler my/dired default-directory
;;       :before-hide (message "before hide"))))


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
  `(mk/toggler ,name ,file-path
	       :hide-if		,hide-if
	       :make-form		,make-form
	       :with-toggle-let	((display-buffer-overriding-action ,display-action)
				 ,@with-toggle-let)
	       ,@args))

;; (when nil
;;   (define-key global-map (kbd "C-z")
;;     (mk/file-toggler my/scratch-file "~/scratch.org"
;;       :before-hide (message "before hide!!")
;;       :after-hide  (message "after hide!!"))))



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

;; (when nil
;;   (define-key global-map (kbd "C-z")
;;     (mk/term-toggler my/toggle-bash-term default-directory
;;       :with-new-term	(keymap-local-set "C-z" #'my/ansi-term)))
;; 
;;   (define-key global-map (kbd "C-M-z")
;;     (mk/term-toggler my/toggle-ranger-term default-directory
;;       :with-new-term	(keymap-local-set "C-M-z" #'my/ansi-term)
;;       :after-make-run	"ranger; exit\n")))

(provide 'mk-togglers)
