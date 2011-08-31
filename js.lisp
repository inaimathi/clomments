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

(compile-js "clomments.js" "js.lisp"
	    (ps* `(doc-ready (chain $ (get ,(concatenate 'string *url* "/") (create)
					   #'display-data)))
		 
		 `(defun display-data (data)
		    ($ "#clomments" (html data)))
		 
		 `(defun send-add-comment ()
		    (chain $ (post ,(concatenate 'string *url* "/add-comment")
				   (create :body ($ "#clomment-field-body" (val))
					   :author ($ "#clomment-field-site" (val))
					   :site ($ "#clomment-field-author" (val)))
				   #'display-data)))))