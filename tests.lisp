(defpackage :cl-point-bot.tests
  (:use :cl-point-bot.api :cl-point-bot.connection
	:fiveam :common-lisp))

(in-package :cl-point-bot.tests)

(def-suite cl-point-bot-suite :description "Common test-suite")

(def-suite connection-suite :description "Service connection tests"
	   :in cl-point-bot-suite)

(in-suite connection-suite)

(test parse-headers
  "Test parsing headers coming from server"
  (let ((headers  '("HTTP/1.1 200 OK"
		    "Server: nginx"
		   "Date: Tue, 20 May 2014 11:10:36 GMT"
		   "Content-Type: text/html"
		   "Transfer-Encoding: chunked"
		   "Connection: close"
		   "Set-Cookie: user=1890d82cf76204d0292660c26e5d8985a7b5c06b; Domain=.point.im; expires=Mon Aug 18 13:10:36 2014; httponly; Path=/"))
	(right-cookie "user=1890d82cf76204d0292660c26e5d8985a7b5c06b; Domain=.point.im; expires=Mon Aug 18 13:10:36 2014; httponly"))
      (is (cl-point-bot.connection::parse-headers headers))
      (is (string= cl-point-bot.connection::*cookie* right-cookie))))

(test parse-errors
  "Check for any HTTP error code"
  (for-all ((header (lambda ()
		      (list (format nil "HTTP/1.1 ~d Message" (funcall (gen-integer :min 400 :max 600)))))))
    (signals cl-point-bot.connection::http-error (cl-point-bot.connection::parse-headers header))))

(test (construct-request
       :depends-on parse-headers)
  "Check HTTP request header construction"
  (let ((line "PUT")
	(headers "headers")
	(result "PUT
Host: point.im
Accept: */*
Cookie: user=1890d82cf76204d0292660c26e5d8985a7b5c06b; Domain=.point.im; expires=Mon Aug 18 13:10:36 2014; httponly
headers
"))
    (is (cl-point-bot.connection::construct-request line headers))
    (is (string= (cl-point-bot.connection::construct-request line headers)
		 result))))

(def-suite api-suite :description "API Functions tests"
	   :in cl-point-bot-suite)

(in-suite api-suite)

(test create-user
  "Test create-user function"
  (let* ((user-line "{\"about\": \"\\u0418\\u043d\\u0436\\u0435\\u043d\\u0435\\u0440-\\u043f\\u0440\\u043e\\u0433\\u0440\\u0430\\u043c\\u043c\\u0438\\u0441\\u0442 \\u043e\\u0442\\u0434\\u0435\\u043b\\u0430 \\u043f\\u0440\\u043e\\u0431\\u043b\\u0435\\u043c \\u043a\\u043e\\u043c\\u043f\\u0438\\u043b\\u044f\\u0446\\u0438\\u0438 \\u0434\\u043b\\u044f \\u043c\\u043e\\u0431\\u0438\\u043b\\u044c\\u043d\\u044b\\u0445 \\u043f\\u043b\\u0430\\u0442\\u0444\\u043e\\u0440\\u043c \\u0443\\u043f\\u0440\\u0430\\u0432\\u043b\\u0435\\u043d\\u0438\\u044f \\u043f\\u0440\\u043e\\u0433\\u0440\\u0430\\u043c\\u043c\\u043d\\u044b\\u043c \\u043e\\u0431\\u0435\\u0441\\u043f\\u0435\\u0447\\u0435\\u043d\\u0438\\u0435\\u043c \\u0434\\u043b\\u044f \\u043c\\u043e\\u0431\\u0438\\u043b\\u044c\\u043d\\u044b\\u0445 \\u043f\\u043b\\u0430\\u0442\\u0444\\u043e\\u0440\\u043c.\", \"xmpp\": \"rayslava@jabber.ru\", \"name\": \"rayslava\", \"subscribed\": false, \"created\": \"2012-10-19T22:00:00+02:00\", \"bl\": false, \"gender\": true, \"wl\": false, \"birthdate\": null, \"id\": 30, \"rec_sub\": false, \"avatar\": \"rayslava.jpg?r=1488\", \"skype\": \"\", \"login\": \"rayslava\", \"icq\": \"\", \"homepage\": \"http://rayslava.com\", \"email\": \"\", \"location\": \"\\u0414\\u0421\"}")
	 (user (create-user user-line)))
    (is (slot-boundp user 'cl-point-bot.api::id))
    (is (= 30 (slot-value user 'cl-point-bot.api::id))
    (is (slot-boundp user 'cl-point-bot.api::name))
    (is (string= "rayslava" (slot-value user 'cl-point-bot.api::name))))))

