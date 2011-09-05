(defpackage :clomments 
    (:use :cl :hunchentoot :cl-who :clsql :parenscript)
  (:import-from :drakma :http-request)
  (:import-from :cl-ppcre :split)
  (:import-from :json :encode-json-to-string)
  (:shadow :get-time))
(in-package :clomments)

(defvar *public-key* "[your-recaptcha-public-key-here]")
(defvar *private-key* "[your-recaptcha-private-key-here]")

(defparameter *db-spec* '("localhost" "clomments" "clomments" "password"))
(defparameter *db* (connect *db-spec* :database-type :mysql :pool t :make-default t))
(setf *default-caching* nil)

(defparameter *url* "http://localhost:4242")
;; (defparameter *whitelist* nil) ;; reserved for further stuff (should probably store this in DB)
;; (defparameter *blacklist* nil)

(defvar *web-server* (start (make-instance 'acceptor :port 4242)))
(push (create-static-file-dispatcher-and-handler "/clomments.css" (merge-pathnames "clomments.css")) *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/clomments.js" (merge-pathnames "clomments.js")) *dispatch-table*)

(load "js.lisp") ;;oddities happen on the initial load of js.lisp; this regenerates the file