(in-package :clomments)

(defun compile-js (file-name origin js) 
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "//////////~%// This is a generated file. ~%// If you want to edit it, tweak '~a' and re-evaluate it.~%//////////~%~%" origin)
    (format stream js)))

;;;;;;;;;;;;;;; basic shortcuts
(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro fn (&body body) `(lambda () ,@body))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro parse-json (target)
  `(chain j-query (parse-j-s-o-n ,target)))

;;;;;;;;;;;;;;; recaptcha specific
(defpsmacro captcha-create (elem &optional callback)
  `(chain -recaptcha (create ,*public-key* ,elem
			     (create :theme "clean"
				     ,@(when callback `(:callback ,callback))))))

;;;;;;;;;;;;;;; clomment specific
(defpsmacro page-url (page)
  (concatenate 'string *url* page))

(defpsmacro post-to (target-page data-hash on-success) ; data hash declared like (create :k v ...)
  `(chain $ (post (page-url ,target-page)
		  ,data-hash
		  ,on-success)))

(defpsmacro define-comment-fn (target-page)
  `(defun ,(intern (format nil "send-~(~a~)" target-page)) (comment-id)
     (post-to ,(format nil "/~(~a~)" target-page) (create :comment-id comment-id)
	      (lambda (data) 
		(replace-comment ($ (+ ".clomment-" comment-id)) data)))))

;;;;;;;;;;;;;;; compile file
(compile-js "clomments.js" "js.lisp"
	    (ps (doc-ready (post-to "/get-comments" nil #'display-data))
		
		(chain j-query (get-script "http://www.google.com/recaptcha/api/js/recaptcha_ajax.js"))

		(defun display-data (data)
		  (try (let ((parsed (parse-json data)))
			 (alert (@ parsed message))
			 (captcha-create 
			  "recaptcha_div" 
			  (@ -recaptcha focus_response_field)))
		       (:catch (error)
			 ($ "#clomments-add-comment" (hide))
			 ($ "#clomments" (html data)))))
		
		(defun replace-comment (a-comment data)
		  ($ a-comment (replace-with data)))
		
		(defun show-comment-form ()
		  (captcha-create "recaptcha_div")
		  ($ "#clomments-add-comment" (show)))
		
		(defun send-add-comment ()
		  (post-to "/add-comment"
			   (create :body ($ "#clomment-field-body" (val))
				   :author ($ "#clomment-field-author" (val))
				   :site ($ "#clomment-field-site" (val))
				   :recaptcha-challenge (chain -recaptcha (get_challenge))
				   :recaptcha-response (chain -recaptcha (get_response)))
			   #'display-data))
		
		(define-comment-fn report-comment)
		(define-comment-fn like-comment)
		(define-comment-fn dislike-comment)))