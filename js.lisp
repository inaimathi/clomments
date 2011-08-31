(in-package :clomments)

(defun compile-js (file-name origin js) ;; The macro expects the output of ps to be passed (this is for ease of macroexpansion), but keeps it as a &body arg to maintain indenting
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "//////////~%// This is a generated file. ~%// If you want to edit it, tweak '~a' and re-evaluate it.~%//////////~%~%" origin)
    (format stream js)))

(defpsmacro $ (selector &body chains)
  `(chain (j-query ,selector) ,@chains))

(defpsmacro fn (&body body) `(lambda () ,@body))

(defpsmacro doc-ready (&body body) 
  `($ document (ready (fn ,@body))))

(defpsmacro page-url (page)
  (concatenate 'string *url* page))

(defpsmacro post-to (target-page (&rest data) on-success)
  `(chain $ (post (page-url ,target-page)
		  (create ,@data)
		  ,on-success)))

(defpsmacro define-comment-fn (target-page)
  `(defun ,(intern (format nil "send-~(~a~)" target-page)) (comment-id)
     (post-to ,(format nil "/~(~a~)" target-page) (:comment-id comment-id)
	      (lambda (data) 
		(replace-comment ($ (+ ".clomment-" comment-id)) data)))))

(compile-js "clomments.js" "js.lisp"
	    (ps (doc-ready (post-to "/" nil #'display-data))
		
		(defun send-add-comment ()
		  (post-to "/add-comment"
			   (:body ($ "#clomment-field-body" (val))
			    :author ($ "#clomment-field-author" (val))
			    :site ($ "#clomment-field-site" (val)))
			   #'display-data))
		
		(define-comment-fn report-comment)
		(define-comment-fn like-comment)
		(define-comment-fn dislike-comment)
		
		(defun display-data (data)
		  ($ "#clomments" (html data)))
		
		(defun replace-comment (a-comment data)
		  ($ a-comment (replace-with data)))))