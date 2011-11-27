(in-package :clomments)
(file-enable-sql-reader-syntax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Class definitions and methods for pages and sites (this will probably grow substantially as we go)

;;;;;;;;;;;;;;; classes
(def-view-class site ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (url-host :type string :reader url-host :initarg :url-host)
   (pages :accessor pages :db-kind :join
	  :db-info (:join-class page :home-key id :foreign-key page-id :set t))))

(def-view-class page ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (site-id :type integer :accessor site-id :initarg site-id :db-constraints :not-null)
   (url :type string :reader url :initarg :url)
   (posted :type wall-time :reader posted :initarg :posted)))

(defmethod comments ((page page))
  (select 'comment :where [and [null [slot-value 'comment 'parent]]
			       [= [slot-value 'comment 'page-id] (id page)]]))

;;;;;;;;;;;;;;; site-related
(defun get-site (url) (get-site-by-host (url->host url)))

(defun get-site-by-host (host)
  (caar (select 'site :where [= [slot-value 'site 'url-host] host])))

(defun add-site (url)
  (let* ((host (url->host url))
	 (site (make-instance 'site :url-host host))
	 (site-id (update-record-from-instance site)))
    (setf (id site) site-id)
    site))

;;;;;;;;;;;;;;; page-related
(defun get-page (url) 
  (caar (select 'page :where [= [slot-value 'page 'url] url])))

(defun add-page (url)
  "Adds a new page to the database, and returns the comment instance"
  (let* ((page (make-instance 'page :url url :posted (now)))
	 (page-id (update-records-from-instance page)))
    (setf (id page) page-id)
    page))

(defmethod echo ((page page))
  (with-html 
    (if (and page (comments page))
	(htm (:ul :class "clomment-list top-level"
		  (loop for (comment) in (comments page)
			do (str (echo-tree comment)))))
	(htm (:p "No comments for this page yet.")))))