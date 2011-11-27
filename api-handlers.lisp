(in-package :clomments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains handlers meant to be called from other sites.
;; Output is incomplete HTML, or raw JSON which the caller is expected to transform before displaying.

;;;;;;;;;;;;;;; external request handlers
(define-easy-handler (get-comments :uri "/get-comments") ()
  "Responds with all comments stored for the calling page."
  (with-conn
    (let ((page (get-page (referer))))
      (with-html-output-to-string (*standard-output* nil :indent t)
	(:a :name "clomments-section")
	(echo page)
	(htm (:input :type "button" :onclick "showCommentForm()" :value "Add Comment")
	     (:input :type "button" :onclick "showModView()" :value "Moderate"))))))

(define-easy-handler (moderate-page :uri "/moderate-page") ()
  "Responds with the moderation view of comments for the calling page."
  (with-conn
    (let ((page (get-page (referer))))
      (with-html-output-to-string (*standard-output* nil :indent t)
	(loop for (comment) in (comments page)
	      do (echo comment)
	      do (echo-admin comment))))))

(define-easy-handler (get-comment-thread :uri "/get-thread") (comment-id)
  "Responds with a thread tree starting with the [comment-id] comment."
  (when comment-id
    (with-conn
      (with-html-output-to-string (*standard-output* nil :indent t)
	(:ul :class "clomment-list"
	     (mapc #'echo-tree (children (get-comment (parse-integer comment-id)))))))))

;;;;;;;;;;;;;;; comment action handlers
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