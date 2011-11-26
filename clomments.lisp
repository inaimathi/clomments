(in-package :clomments)

;;;;;;;;;;;;;;; comment handlers
(define-easy-handler (new-comment :uri "/add-comment") (parent body author site recaptcha-challenge recaptcha-response)
  (if (recaptcha-passed-p recaptcha-challenge recaptcha-response (real-remote-addr))
      (with-conn
	(let ((page (get-page (referer))))
	  (unless page (setf page (add-page (referer))))
	  (add-comment (id page) parent body author site)
	  (redirect "/get-comments")))
      (encode-json-to-string 
       `((:error . :recaptcha) 
	 (:message . "You seem to have mistyped the recaptcha, please try again.")))))

(defmacro define-comment-action (name &body body)
  "This is a shortcut macro for the comment handlers. 
   They all 
        - expect a comment-id
        - get the corresponding comment 
        - do something to it 
        - echo the revised version afterwards"
  `(define-easy-handler (,name :uri ,(format nil "/~(~a~)" name)) (comment-id)
     (with-conn
	 (let ((comment (get-comment comment-id)))
	   (when comment
	     ,@body
	     (with-html-output-to-string (*standard-output*) (echo comment)))))))

(define-comment-action approve-comment
  (setf (state comment) :approved))

(define-comment-action hide-comment
  (setf (state comment) :hidden))

(define-comment-action delete-comment
  (delete-instance-records comment))

(define-comment-action report-comment
  (incf (reports comment))
  (update-record-from-slot comment 'reports))

(define-comment-action like-comment
  (incf (likes comment))
  (update-record-from-slot comment 'likes))

(define-comment-action dislike-comment
  (incf (dislikes comment))
  (update-record-from-slot comment 'dislikes))

;;;;;;;;;;;;;;; ui handlers
(define-easy-handler (get-comments :uri "/get-comments") ()
  "Returns all comments from the calling page."
  (with-conn
    (let ((page (get-page (referer))))
      (with-html-output-to-string (*standard-output* nil :indent t)
	(:a :name "clomments-section")
	(echo page)
	(htm (:input :type "button" :onclick "showCommentForm()" :value "Add Comment")
	     (:input :type "button" :onclick "showModView()" :value "Moderate"))))))

(define-easy-handler (get-comment-thread :uri "/get-thread") (comment-id)
  (when comment-id
    (with-conn
      (with-html-output-to-string (*standard-output* nil :indent t)
	(:ul :class "clomment-list"
	     (mapc #'echo-tree (children (get-comment (parse-integer comment-id)))))))))

(define-easy-handler (moderate-page :uri "/moderate-page") ()
  (with-conn
    (let ((page (get-page (referer))))
      (with-html-output-to-string (*standard-output* nil :indent t)
	(loop for (comment) in (comments page)
	      do (echo comment)
	      do (echo-admin comment))))))

;;;;;;;;;; VIEW
;;;;;;;;;;;;;;; main display
(defmethod echo ((page page))
  (with-html 
    (if (and page (comments page))
	(htm (:ul :class "clomment-list top-level"
		  (loop for (comment) in (comments page)
			do (str (echo-tree comment)))))
	(htm (:p "No comments for this page yet.")))))

(defmethod echo-tree ((comment comment) &optional (depth 0))
  (with-html
    (:li (str (echo comment))
	 (cond ((and (children comment) (> *default-comment-depth* depth))
		(htm (:ul :class "clomment-list"
		      (mapc (lambda (c) (str (echo-tree c (+ 1 depth))))
			    (children comment)))))
	       ((children comment) 
		(htm (:ul :class "clomment-list" 
			  (:li (:div :onclick (format nil "getThread(~a, this);" (id comment)) "Continue Thread >")))))))))

(defmethod echo ((comment comment))
  (with-html
    (:div :class (format nil "clomment-comment clomment-~a" (id comment))
	  (:div :class "clomment-header"
		(:span :class "clomment-author"
		       (str (escape-string (author comment))))
		(when (not (blank-p (site comment)))
		  (htm (:span :class "clomment-site"
			      (str (format nil " of ~a" (escape-string (site comment)))))))
		(:span :class "clomment-posted" (str (echo-posted comment))))
	  (:div :class "clomment-body" (str (escape-string (body comment))))
	  (:div :class "clomment-controls"
		(str (- (likes comment) (dislikes comment)))
	  	(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(send-like-comment ,(id comment)))
	  	    (str "Like"))
	  	(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(send-dislike-comment ,(id comment)))
	  	    (str "Dislike"))
	  	(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(send-report-comment ,(id comment)))
	  	    (str "Report"))
		(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(show-reply-form ,(id comment)))
	  	    (str "Reply"))))))

(defmethod echo-admin ((comment comment))
  (with-html
    (:div :class (format nil "clomment-stats clomment-~a" (id comment))
	  (:p "Likes: " (str (likes comment)))
	  (:p "Dislikes: " (str (dislikes comment)))
	  (:p "Reports: " (str (reports comment)))
	  (:a :href (concatenate 'string *url* "")))))

(defmethod echo-posted ((comment comment))
  (wall-time->time-diff (posted comment)))

(defun wall-time->time-diff (wall-time)
  (let ((diff (time-difference wall-time (now))))
    (cond ((= (duration-minute diff) 0) "just now")
	  ((> (duration-day diff) 30) (print-date wall-time :long-day))
	  ((> (duration-day diff) 0) (print-ago diff :day))
	  ((> (duration-hour diff) 0) (print-ago diff :hour))
	  ((> (duration-minute diff) 0) (print-ago diff :minute)))))

(defun print-ago (duration precision)
  (concatenate 'string (format-duration nil duration :precision precision) " ago"))

(defmethod json ((comment comment))
  "Useful for data exports, or if someone wants to generate their own markup."
  (with-html (encode-json-to-string comment)))

;;;;;;;;;;;;;;; test page
(define-easy-handler (test :uri "/test") ()
  (with-conn
    (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
      (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	     (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		    (:link :rel "stylesheet" :type "text/css" :href "/clomments.css")
		    (:script :type "text/javascript":src "https://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js")
		    (:script :type "text/javascript" :src (concatenate 'string *url* "/clomments.js")))
	     (:body
	      (:p "This is the test page for :clomments")
	      (:hr)
	      (:div :id "clomments"))))))