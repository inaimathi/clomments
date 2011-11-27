(in-package :clomments)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains any handlers meant for internal components.
;; For example, texting/documentation/reporting on the clomments server itself.
;; These handlers should display valid, complete HTML, rather than expecting their output to be transformed further.

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