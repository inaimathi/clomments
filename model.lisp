(in-package :clomments)
(file-enable-sql-reader-syntax)

;;;;;;;;;;;;;;; classes
(def-view-class page ()
  ((id :type integer :accessor id :initarg :id :db-constraints (:not-null :auto-increment) :db-kind :key)
   (url :type string :reader url :initarg :url)
   (posted :type wall-time :reader posted :initarg :posted)
   (comments :accessor comments :db-kind :join
	     :db-info (:join-class comment :home-key id :foreign-key page-id :set t))))

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

(defun new-database ()
  (dolist (c '(page comment))
    (when (table-exists-p c) (drop-view-from-class c))
    (create-view-from-class c)))

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

(defun add-comment (page-id reply-to body author site)
  "Adds a new comment to the database, and returns the comment instance"
  (let* ((comment (make-instance 'comment 
				 :reply-to (when reply-to (parse-integer reply-to)) 
				 :page-id page-id :posted (now)
				 :body body :author author :site site))
	 (comment-id (update-records-from-instance comment)))
    comment))