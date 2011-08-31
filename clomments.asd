;;; -*- Mode: Lisp -*-
(defpackage :clomments-system (:use :cl :asdf))
(in-package :clomments-system)

(asdf:defsystem clomments
  :version "0.1"
  :author "leo.zovic@gmail.com"
  :maintainer "leo.zovic@gmail.com"
  :licence "AGPL"
  :description "A first crack at a comment hosting system"
  :depends-on (:hunchentoot :cl-who :clsql :parenscript :cl-ppcre :cl-json)
  
  :components ((:file "package")
	       (:file "js" :depends-on ("package"))
	       (:file "clomments" :depends-on ("package" "js"))))