(in-package :clomments)

(defun compile-js (file-name origin js) 
  (with-open-file (stream file-name :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format stream "//////////~%// This is a generated file. ~%// If you want to edit this javascript, tweak '~a' and re-evaluate it instead.~%//////////~%~%" origin)
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
		(replace-comment comment-id data)))))