(in-package :clomments)
(file-enable-sql-reader-syntax)

;;;;;;;;;;class definition
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

;;;;;;;;;;basic methods
(defun get-comment (comment-id) 
  "Takes a comment id and returns the corresponding comment instance"
  (caar (select 'comment :where [= [slot-value 'comment 'id] comment-id])))

(defun add-comment (page-id parent body author site)
  "Adds a new comment to the database, and returns the comment instance"
  (let* ((comment (make-instance 'comment 
				 :parent (when parent (parse-integer parent :junk-allowed t)) 
				 :page-id page-id :posted (now)
				 :body body :author author :site site))
	 (comment-id (update-records-from-instance comment)))
    comment))

;;;;;;;;;;view methods
(defmethod echo-tree ((comment comment) &key (max-depth *default-comment-depth*) (depth 0))
  "Takes a comment and echoes it along with children down to [max-depth]"
  (with-html
    (:li (str (echo comment))
	 (cond ((and (children comment) (> max-depth depth))
		(htm (:ul :class "clomment-list"
		      (mapc (lambda (c) (str (echo-tree c :depth (+ 1 depth))))
			    (children comment)))))
	       ((children comment) 
		(htm (:ul :class "clomment-list" 
			  (:li (:div :onclick (format nil "getThread(~a, this);" (id comment)) "Continue Thread >")))))))))

(defmethod echo ((comment comment))
  "Echoes the HTML of a single comment"
  (with-html
    (:div :class (format nil "clomment-comment clomment-~a" (id comment))
	  (:div :class "clomment-header"
		(:span :class "clomment-author"
		       (str (escape-string (author comment))))
		(when (not (blank-p (site comment)))
		  (htm (:span :class "clomment-site"
			      (str (format nil " of ~a" (escape-string (site comment)))))))
		(:span :class "clomment-posted" (str (echo-posted comment))))
	  (:div :class "clomment-body" (str (escape-string (body comment))))
	  (:div :class "clomment-controls"
		(str (- (likes comment) (dislikes comment)))
	  	(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(send-like-comment ,(id comment)))
	  	    (str "Like"))
	  	(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(send-dislike-comment ,(id comment)))
	  	    (str "Dislike"))
	  	(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(send-report-comment ,(id comment)))
	  	    (str "Report"))
		(:a :href "javascript:void(0)" 
	  	    :onclick (ps* `(show-reply-form ,(id comment)))
	  	    (str "Reply"))))))

(defmethod echo-admin ((comment comment))
  "Echoes the administration info for a comment"
  (with-html
    (:div :class (format nil "clomment-stats clomment-~a" (id comment))
	  (:p "Likes: " (str (likes comment)))
	  (:p "Dislikes: " (str (dislikes comment)))
	  (:p "Reports: " (str (reports comment)))
	  (:a :href (concatenate 'string *url* "")))))

;;;;; echoes the "timestamp" portion of a comment
(defmethod echo-posted ((comment comment))
  (wall-time->time-diff (posted comment)))

(defun wall-time->time-diff (wall-time)
  "Takes a wall time and returns the appropriate 'timestamp' label based on how much time has passed"
  (flet ((print-ago (duration precision)
	   (concatenate 'string (format-duration nil duration :precision precision) " ago")))
    (let ((diff (time-difference wall-time (now))))
      (cond ((= (duration-minute diff) 0) "just now")
	    ((> (duration-day diff) 30) (print-date wall-time :long-day))
	    ((> (duration-day diff) 0) (print-ago diff :day))
	    ((> (duration-hour diff) 0) (print-ago diff :hour))
	    ((> (duration-minute diff) 0) (print-ago diff :minute))))))