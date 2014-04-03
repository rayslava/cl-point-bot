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

(defun create-user (line)
  (with-decoder-simple-clos-semantics
    (let* ((*json-symbols-package* :cl-point-bot.api)
	   (x (decode-json-from-string line))
	   (result (make-instance 'user)))
      (with-slots (id name) x
	(setf (id result) id)
	(setf (name result) name))
      result)))
