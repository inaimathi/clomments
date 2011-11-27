;;; -*- Mode: Lisp -*-
(defpackage :clomments-system (:use :cl :asdf))
(in-package :clomments-system)

(asdf:defsystem clomments
  :version "0.11"
  :author "leo.zovic@gmail.com"
  :maintainer "leo.zovic@gmail.com"
  :licence "AGPL"
  :description "A first crack at a comment hosting system"
  :depends-on (:hunchentoot :drakma :cl-ppcre :cl-who :clsql :parenscript :cl-ppcre :cl-json)
  
  :components ((:file "package")
	       (:file "util" :depends-on ("package"))
	       (:file "js-macros" :depends-on ("package"))
	       (:file "js" :depends-on ("package" "js-macros"))
	       (:file "sites+pages" :depends-on ("package" "util" "js"))
	       (:file "comments" :depends-on ("package" "util" "js" "sites+pages"))
	       (:file "api-handlers" :depends-on ("package" "util" "js" "sites+pages" "comments"))
	       (:file "internal-handlers" :depends-on ("package" "util" "js" "sites+pages" "comments" "api-handlers"))))