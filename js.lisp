(in-package :clomments)

(compile-js "clomments.js" "js.lisp"
	    (ps (doc-ready (post-to "/get-comments" nil #'display-data))
		
		(chain j-query (get-script "http://www.google.com/recaptcha/api/js/recaptcha_ajax.js"))
		(chain j-query (get-script "https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.16/jquery-ui.min.js"))

		(defun comment-form (&optional parent-id)
		  (who-ps-html (:div :id "clomments-add-comment"
				     (:ul :class "form-fields"
					  (:input :type "hidden" :id "clomment-parent-id" :value parent-id)
					  (:li (:span :class "clomments-label" "Name")
					       (:input :class "text-box" :id "clomment-field-author"))
					  (:li (:span :class "clomments-label" "Your Site")
					       (:input :class "text-box" :id "clomment-field-site"))
					  (:li (:span :class "clomments-label" "Comment")
					       (:textarea :id "clomment-field-body"))
					  (:li (:div :id "recaptcha_div"))
					  (:li (:span :class "clomments-label" "")
					       (:input :type "button" :value "Post"
						       :onclick "sendAddComment();"))))))

		(defun display-data (data)
		  (try (let ((parsed (parse-json data)))
			 (alert (@ parsed message))
			 (captcha-create 
			  "recaptcha_div" 
			  (@ -recaptcha focus_response_field)))
		       (:catch (error)
			 ($ "#clomments-add-comment" (remove))
			 ($ "#clomments" (html data)))))

		(defun get-thread (comment-id elem)
		  (post-to "/get-thread" 
			   (create :comment-id comment-id)
			   (lambda (data) ($ elem (parent) (parent) (replace-with data)))))

		(defun show-mod-view ()
		  (post-to "/moderate-page" (create) #'display-data))
		
		(defun replace-comment (comment-id data)
		  (let ((sel (+ ".clomment-" comment-id)))
		    ($ sel (replace-with data))
		    ($ sel (effect :highlight (create :color "#0f0") 500))))

		(defun show-reply-form (parent-id)
		  ($ "#clomments-add-comment" (remove))
		  ($ (+ ".clomment-" parent-id) (after (comment-form parent-id)))
		  (captcha-create "recaptcha_div")
		  ($ "#clomments-add-comment" (show)))
		
		(defun show-comment-form ()
		  ($ "#clomments-add-comment" (remove))
		  ($ "#clomments" (append (comment-form)))
		  (captcha-create "recaptcha_div")
		  ($ "#clomments-add-comment" (show)))
		
		(defun send-add-comment ()
		  (post-to "/add-comment"
			   (create :body ($ "#clomment-field-body" (val))
				   :author ($ "#clomment-field-author" (val))
				   :site ($ "#clomment-field-site" (val))
				   :parent ($ "#clomment-parent-id" (val))
				   :recaptcha-challenge (chain -recaptcha (get_challenge))
				   :recaptcha-response (chain -recaptcha (get_response)))
			   #'display-data))
		
		(define-comment-fn report-comment)
		(define-comment-fn like-comment)
		(define-comment-fn dislike-comment)))