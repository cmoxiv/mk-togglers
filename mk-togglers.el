
(cl-defmacro mk/toggler (symbol buf
			 &key
			   (hide-if		'(equal buf (buffer-name)))
			   (before-hide		'(ignore))
			   (hide-form		'(switch-to-buffer (other-buffer)))
			   (after-hide		'(always))
			   (hide-failed		'(message "[mk/toggler] Failed to hide buffer!!"))
			   (show-if		'found-buffer)
			   (before-show		'(ignore))
			   (show-form		'(pop-to-buffer found-buffer))
			   (after-show		'(always))
			   (show-failed		'(message "[mk/toggler] Failed to show buffer!!"))
			   (find-form		'(get-buffer buf))
			   (make-if		'(not found-buffer))
			   (before-make		'(ignore))
			   (make-form		'(switch-to-buffer buf))
			   (after-make		'(always))
			   (make-failed		'(message "[mk/toggler] Failed to make buffer!!"))
			   (before-fallback	'(ignore))
			   (fallback-form	'(ignore))
			   (after-fallback	'(always))
			   (fallback-failed	'(message "[mk/toggler] Fallback failed!!"))
			   (with-all-let	'())
			   (with-hide-let	'())
			   (with-show-let	'())
			   (with-make-let	'())
			   (with-fallback-let	'())
			   (with-new-buffer	'(ignore))
			   (show-prms	'((display-buffer-use-some-window)
					  (dedicated . t))))
  (declare (indent defun))
  `(cl-defun ,symbol (&optional (_buf ,buf))
     (interactive)
     (let* ((_ nil)
	    (_found-buffer ,find-form)
	    (display-buffer-overriding-action ,show-prms)
	    ,@with-all-let)
       (if ,hide-if
	   (let* ((_ nil) ,@with-hide-let)
	     (if (progn
		   ,before-hide
		   ,hide-form)
		 ,after-hide
	       ,hide-failed
	       nil))
	 (if ,show-if
	     (let* ((_ nil) ,@with-show-let)
	       (if (progn
		     ,before-show
		     ,show-form)
		   ,after-show
		 ,show-failed
		 nil))
	   (if ,make-if
	       (let* ((_ nil) ,@with-make-let)
		 (if-let ((_new-buffer (progn
					 ,before-make
					 ,make-form)))
		     (with-current-buffer _new-buffer
		       ,after-make
		       ,with-new-buffer)
		   ,make-failed
		   nil))
	     (let* ((_ nil) ,@with-fallback-let)
	       (if (progn
		     ,before-fallback
		     ,fallback-form)
		   ,after-fallback
		 ,fallback-failed))))))))



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
    :with-all-let	((x 12)
			 (y "abc"))
    :with-make-let	((z1 123)
			 (z2 12.3))
    :show-prms		'((display-buffer-full-frame)
			  (dedicated . t))))


;; (let ((_ nil))
;;   (message "nothing here!!"))
;; 
;; (with-current-buffer 
;;     )
;; 
;; (with-current-buffer-window )
;; (with-output-to-string (cl-prin1-to-string "asdasdasd"))
