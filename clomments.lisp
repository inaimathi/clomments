(in-package :clomments)
(file-enable-sql-reader-syntax)

;;;;;;;;;; MODEL
(defun now () (clsql-sys:utime->time (get-universal-time)))

;;;;;;;;;;;;;;; page-related
(def-view-class page ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (url :type string :reader url :initarg :url)
   (posted :type wall-time :reader posted :initarg :posted)
   (comments :accessor comments :db-kind :join
	     :db-info (:join-class comment :home-key id :foreign-key page-id :set t))))

(defun get-page (url) (caar (select 'page :where [= [slot-value 'page 'url] url])))

(defun add-page (url)
  (let* ((page (make-instance 'page :url url :posted (now)))
	 (page-id (update-records-from-instance page)))
    (setf (id page) page-id)
    page))

;;;;;;;;;;;;;;; comment-related
(def-view-class comment ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (reply-to :type integer :reader reply-to :initarg reply-to :initform nil)
   (page-id :type integer :reader page-id :initarg :page-id)
   
   (reports :type integer :accessor reports :initform 0 :db-constraints :not-null)
   (likes :type integer :accessor likes :initform 0 :db-constraints :not-null)
   (dislikes :type integer :accessor dislikes :initform 0 :db-constraints :not-null)
   
   (body :type string :reader body :initarg :body)
   (author :type string :reader author :initarg :author)
   (site :type string :reader site :initarg :site)
   (posted :type wall-time :reader posted :initarg :posted)))

(defun add-comment (page-id reply-to body author site)
  (let* ((comment (make-instance 'comment 
				 :reply-to (when reply-to (parse-integer reply-to)) 
				 :page-id page-id :posted (now)
				 :body body :author author :site site))
	 (comment-id (update-records-from-instance comment)))
    comment))

(defmethod echo ((comment comment))
  (with-html-output (*standard-output* nil :indent t)
    (:div :class "clomment-comment"
	  (:div :class "clomment-header"
		(:span :class "clomment-author"
		       (str (author comment)))
		(when (site comment)
		  (htm (:span :class "clomment-site"
			      (str (format nil " of ~a" (site comment)))))))
	  (:div :class "clomment-body" (str (body comment)))
	  (:div :class "clomment-controls"
		(:a :href (concatenate 'string *url* "/like-comment") (str "Like"))
		(:a :href (concatenate 'string *url* "/dislike-comment") (str "Dislike"))
		(:a :href (concatenate 'string *url* "/report-comment") (str "Report"))
		(:span :class "clomment-posted" (str (format nil " - ~a" (posted comment))))))))

(defmethod json ((comment comment))
  (with-html-output (*standard-output* nil :indent t)
    (encode-json comment)))

;;;;;;;;;;;;;;; comment handlers
(define-easy-handler (report-comment :uri "/report-comment") (comment-id)
  (let ((comment (caar (select 'comment :where [= [slot-value 'comment 'id] comment-id]))))
    (when comment
      (incf (reports comment))
      (update-records-from-slot (reports comment)))))

;; (define-easy-handler (like-comment :uri "/like-comment") (comment-id))
;; (define-easy-handler (dislike--comment :uri "/dislike-comment") (comment-id))

(define-easy-handler (new-comment :uri "/add-comment") (reply-to body author site)
  (let ((page (get-page (referer))))
    (unless page (setf page (add-page (referer))))
    (add-comment (id page) reply-to body author site)
    (redirect "/")))

;;;;;;;;;; VIEW
;;;;;;;;;;;;;;; main display
(defun comment-form ()
  (with-html-output (*standard-output* nil :indent t)
    (:form :id "#clomment-add-comment" :method "post"
	   (:ul :class "form-fields"
		(:input :type "hidden")
		(:li (:span :class "clomments-label" "Name")
		     (:input :class "text-box" :id "clomment-field-author"))
		(:li (:span :class "clomments-label" "Your Site")
		     (:input :class "text-box" :id "clomment-field-site"))
		(:li (:span :class "clomments-label" "Comment")
		     (:textarea :id "clomment-field-body"))
		(:li (:span :class "clomments-label" "Captcha"))
		(:li (:span :class "clomments-label" "")
		     (:input :type "button" :value "Post"
			     :onclick "sendAddComment();"))))))

(define-easy-handler (get-comments :uri "/") (raw)
  (let ((page (get-page (referer))))
    (with-html-output-to-string (*standard-output* nil :indent t)
      (:a :name "clomments-section")
      (if (and page (comments page))
  	  (dolist (comment (comments page))
  	    (str (if raw (json comment) (echo comment))))
	  (htm (:p "No comments for this page yet.")
	       (:p (str (format nil "No comments on ~a" (referer))))))
      (htm (comment-form)))))

;;;;;;;;;;;;;;; test page
(define-easy-handler (test :uri "/test") ()
  (with-html-output-to-string (*standard-output* nil :prologue t :indent t)
    (:html :xmlns "http://www.w3.org/1999/xhtml" :xml\:lang "en" :lang "en"
	   (:head (:meta :http-equiv "Content-Type" :content "text/html;charset=utf-8")
		  (:link :rel "stylesheet" :type "text/css" :href "/clomments.css")
		  (:script :src "https://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js")
		  (:script :src (concatenate 'string *url* "/clomments.js")))
	   (:body
	    (:p "This is the test page for :clomments")
	    (:hr)
	    (:div :id "clomments")))))