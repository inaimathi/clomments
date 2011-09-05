(in-package :clomments)

(defmacro with-conn (&body body)
  `(let* ((new-connection (connect *db-spec* :database-type :mysql :pool t :make-default t))
	  (result (with-default-database (new-connection) ,@body)))
     (disconnect :database new-connection)
     result))

(defmacro with-html (&body body)
  `(with-html-output (*standard-output* nil :indent t)
     ,@body))

(defun now () 
  (clsql-sys:utime->time (get-universal-time)))

(defun blank-p (thing)
  (or (null thing)
      (and (stringp thing) (string= "" thing))))

(defun recaptcha-passed? (challenge response ip &optional (private-key *private-key*))
  (string= "true" 
	   (car (split #\Newline
		       (http-request "http://api-verify.recaptcha.net/verify" 
				     :method :post 
				     :parameters `(("privatekey" . ,private-key)
						   ("remoteip" . ,ip)
						   ("challenge" . ,challenge)
						   ("response" . ,response)))))))