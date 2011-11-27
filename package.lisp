(defpackage :clomments 
    (:use :cl :hunchentoot :cl-who :clsql :parenscript)
  (:import-from :drakma :http-request)
  (:import-from :cl-ppcre :split :scan :scan-to-strings)
  (:import-from :json :encode-json-to-string)
  (:shadow :get-time))
(in-package :clomments)

;;;;;;;;;; User information (change all of these config options to reflect your system)
(defvar *public-key* "[your-recaptcha-public-key-here]")
(defvar *private-key* "[your-recaptcha-private-key-here]")

(defparameter *db-spec* '("localhost" "clomments" "clomments" "password"))
(defparameter *db* (connect *db-spec* :database-type :mysql :pool t :make-default t))

(defparameter *url* "http://localhost:4242")

;;;;;;;;;; Server settings (Optionally change these to your liking)
(defparameter *default-comment-depth* 2)
(defparameter *server-port* 4242)


;;;;;;;;;; Internal configs (Don't change these as part of simple configuration)
(setf *default-caching* nil)

(defvar *web-server* (start (make-instance 'acceptor :port *server-port*)))
(push (create-static-file-dispatcher-and-handler "/clomments.css" (merge-pathnames "clomments.css")) *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/clomments.js" (merge-pathnames "clomments.js")) *dispatch-table*)

(defun new-database ()
  (dolist (c '(page comment site user salt))
    (when (table-exists-p c) (drop-view-from-class c))
    (create-view-from-class c)))

(when (or (string= *public-key* "[your-recaptcha-public-key-here]")
	  (string= *public-key* "[your-recaptcha-private-key-here]"))
  (format t "Make sure to put in your own private and public recaptcha keys. (http://www.google.com/recaptcha)"))