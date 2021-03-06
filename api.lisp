(defpackage :cl-point-bot.api
  (:use :cl :json :cl-point-bot.connection)
  (:export :user :create-user))

(in-package :cl-point-bot.api)

(defclass user ()
  ((about :accessor about)
   (xmpp :accessor xmpp)
   (name :accessor name)
   (subscribed :accessor subscribed)
   (created :accessor created)
   (gender :accessor gender)
   (birthdate :accessor birthdate)
   (id :accessor id)
   (avatar :accessor avatar)
   (skype :accessor skype)
   (login :accessor login)
   (icq :accessor icq)
   (homepage :accessor homepage)
   (email :accessor email)
   (location :accessor location)))

(defmethod print-object ((obj user) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (slot-value obj 'login))))

(defclass entry ()
  ((author :accessor author)
   (text :accessor text)
   (id :accessor id)
   (created :accessor created)))

(defclass post (entry)
  ((tags :accessor tags)
   (comments--count :accessor comments--count)
   (comments :accessor comments)))

(defmethod print-object ((obj post) out)
  (print-unreadable-object (obj out :type t)
    (format out "#~s ~s:~%~s" (id obj) (login (author obj)) (text obj))))
(defclass comment (entry)
  ((to--comment--id :accessor to--comment--id)))

(defmethod print-object ((obj comment) out)
  (print-unreadable-object (obj out :type t)
    (format out "/~s ~s: ~s"
	    (id obj)
	    (login (author obj))
	    (text obj))))

(defun create-user (line)
  (with-decoder-simple-clos-semantics
    (let* ((*json-symbols-package* :cl-point-bot.api)
	   (x (decode-json-from-string line))
	   (result (make-instance 'user)))
      (with-slots (id name) x
	(setf (id result) id)
	(setf (name result) name))
      result)))

(defun post-get (id)
  "Requst post #ID with comments

Returns (POST (COMMENT COMMMENT COMMENT))"
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-get (concatenate 'string "post/" id)))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (with-slots (post comments) x
	(list
	 (with-slots (tags author text created id comments--count) post
	   (let* ((p (change-class post 'post))
		  (a (change-class (author p) 'user)))
	     (setf (author p) a)
	     p))
	 (mapcar #'(lambda (comment)
		     (let* ((c (change-class comment 'comment))
			    (a (change-class (author c) 'user)))
		       (setf (author c) a)
		       c))
		 (coerce comments 'list))))))

(defun post-new (text &optional (tags nil) (private nil))
  "Send new post with text and tags

Returns id of new post"
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-post "post" (list (cons "text" text)
					 (when tags
					   (cons "tag" tags))
					 (when private
					   (cons "private" "true"))) t))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
	    (with-slots (id) x
	      id))))

(defun post-update (id text &optional (tags nil))
  "Update existing post with text and tags

Returns id of new post"
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-update (concatenate 'string "post/" id)
			      (list (cons "text" text)
				    (when tags
				      (cons "tag" tags)))))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
	    (with-slots (id) x
	      id))))

(defun post-delete (id)
  "Deletes a post with id

Returns boolean success"
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-delete (concatenate 'string "post/" id)))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (if (slot-boundp x 'ok)
	  t
	  nil))))

(defun comment-new (post text)
  "Send reply to post with text

Returns id of new comment"
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-post (concatenate 'string "post/" post)
			    (list (cons "text" text)) t))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
	    (with-slots (comment--id) x
	      comment--id))))

(defun post-recommend (id &optional (text nil))
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-post (concatenate 'string "post/" id "/r")
			    (list (when text
				    (cons "text" text))) t))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (with-slots (id) x
	id))))

(defun post-unrecommend (id)
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-delete (concatenate 'string "post/" id "/r")))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (if (slot-boundp x 'ok)
	  t
	  nil))))

(defun post-subscribe (id)
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-post (concatenate 'string "post/" id "/s") (list) t))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (with-slots (id) x
	id))))

(defun post-unsubscribe (id)
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-delete (concatenate 'string "post/" id "/s")))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (if (slot-boundp x 'ok)
	  t
	  nil))))

(defun post-bookmark (id &optional (text nil))
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-post (concatenate 'string "post/" id "/b")
			    (list (when text
				    (cons "text" text))) t))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (with-slots (id) x
	id))))

(defun post-unbookmark (id)
  (with-decoder-simple-clos-semantics
    (let*  ((line (api-delete (concatenate 'string "post/" id "/b")))
	    (*json-symbols-package* :cl-point-bot.api)
	    (x (decode-json-from-string line)))
      (if (slot-boundp x 'ok)
	  t
	  nil))))
