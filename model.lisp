(in-package :clomments)
(file-enable-sql-reader-syntax)

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

(def-view-class comment ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (parent :type integer :reader parent :initarg :parent :initform nil)
   (page-id :type integer :reader page-id :initarg :page-id)
   
   (state :type symbol :accessor state :initarg :state :initform :posted)
   
   (reports :type integer :accessor reports :initform 0 :db-constraints :not-null)
   (likes :type integer :accessor likes :initform 0 :db-constraints :not-null)
   (dislikes :type integer :accessor dislikes :initform 0 :db-constraints :not-null)

   (children :reader children :db-kind :join
	     :db-info (:join-class comment :home-key id :foreign-key parent :set t))
   
   (body :type string :reader body :initarg :body)
   (author :type string :reader author :initarg :author)
   (site :type string :reader site :initarg :site)
   (posted :type wall-time :reader posted :initarg :posted)))

(defun new-database ()
  (dolist (c '(page comment site user salt))
    (when (table-exists-p c) (drop-view-from-class c))
    (create-view-from-class c)))

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

;;;;;;;;;;;;;;; comment-related
(defun get-comment (comment-id) 
  (caar (select 'comment :where [= [slot-value 'comment 'id] comment-id])))

(defun add-comment (page-id parent body author site)
  "Adds a new comment to the database, and returns the comment instance"
  (let* ((comment (make-instance 'comment 
				 :parent (when parent (parse-integer parent :junk-allowed t)) 
				 :page-id page-id :posted (now)
				 :body body :author author :site site))
	 (comment-id (update-records-from-instance comment)))
    comment))