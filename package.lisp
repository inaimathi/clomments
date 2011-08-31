(defpackage :clomments 
    (:use :cl :hunchentoot :cl-who :clsql :parenscript)
  (:import-from :json :encode-json)
  (:shadow :get-time))
(in-package :clomments)

(defparameter *db-spec* '("localhost" "clomments" "clomments" "password"))
(defparameter *db* (connect *db-spec* :database-type :mysql :pool t :make-default t))
(setf *default-caching* nil)

(defparameter *url* "http://localhost:4242")
;; (defparameter *whitelist* nil) ;; reserved for further stuff (should probably store this in DB)
;; (defparameter *blacklist* nil)

(defvar *web-server* (start (make-instance 'acceptor :port 4242)))
(push (create-static-file-dispatcher-and-handler "/clomments.css" (merge-pathnames "clomments.css")) *dispatch-table*)
(push (create-static-file-dispatcher-and-handler "/clomments.js" (merge-pathnames "clomments.js")) *dispatch-table*)