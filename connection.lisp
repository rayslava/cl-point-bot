;(declaim (optimize (debug 3) (speed 0) (space 0)))

(defpackage :cl-point-bot.connection
  (:use :cl :cl+ssl :usocket :cl-ppcre :json)
  (:export :api-login :api-logout :api-get :api-post
	   :https-request :open-https-socket :construct-request))

(in-package :cl-point-bot.connection)

(defparameter *point-host* "point.im")

(defparameter *websocket-url* "https://point.im/ws")

(defparameter *port* 443)

(defparameter *socket-charset* :iso-8859-1)

(defvar *auth-token* nil "A token returned after login to API")

(defvar *auth-csrf-token* nil "A csrf token needed for logout")

(defvar *cookie* nil "Current cookie which should be passed to server")

(defparameter *request-success* '(200 101) "HTTP success codes")

(define-condition http-error (error)
  ((code :initarg :code :reader code))
  (:documentation "HTTP protocol error happened."))

(defun handle-http-error (err)
  "HTTP Protocol error handling

TODO: implement handling actually"
  (error (format nil "HTTP error ~d~%" (code err))))

(define-condition auth-error (error)
  ((message :initarg :message :reader message))
  (:documentation "NonAuthorized message received from API"))

(defun handle-auth-error (err)
  "Couldn't authorize

TODO: implement handling actually"
  (error (format nil "Authorization error ~a~%" (message err))))

(defun read-line-crlf (stream &optional eof-error-p)
  (let ((s (make-string-output-stream)))
    (loop
       for empty = t then nil
       for c = (read-char stream eof-error-p nil)
       while (and c (not (eql c #\return)))
       do
	 (unless (eql c #\newline)
	   (write-char c s))
       finally
	 (return
	   (if empty nil (get-output-stream-string s))))))

(defun parse-headers (headers)
  "Get list of http headers and extract all the useful info from there"
  (mapcar #'(lambda (line)
	      (let ((http-scanner (create-scanner "^HTTP/1.1\\s+(\\d+)\\s+.[\\w\\s\\d]+$"))
		    (cookie-scanner (create-scanner "^Set-Cookie:\\s(user=.*);.*$")))
		    (register-groups-bind (http-code)
			(http-scanner line)
		      (if (not (member (parse-integer http-code) *request-success*))
			  (signal 'http-error :code (parse-integer http-code))))
		    (register-groups-bind (cookie)
			(cookie-scanner line)
		      (if cookie
			  (setf *cookie* cookie)))))
	  headers))

(defun construct-request (line &optional headers)
  "Constructs HTTP request body except main line.
headers - string to append to other headers. Must end with newline.

Actually fills up all the secondary headers"
  (format nil "~A~%Host: ~a~%Accept: */*~%~@[Cookie: ~a~%~]~@[Authorization: ~a~%~]~@[~a~]~%"
	  line
	  *point-host*
	  *cookie*
	  *auth-token*
	  headers))

(defun open-https-socket ()
  "Opens and returns an (socket https-socket) pair to point host"
  (let* ((socket
	 (usocket:socket-connect
	  *point-host* *port*
	  :element-type '(unsigned-byte 8)))
	 (https
	  (cl+ssl:make-ssl-client-stream
	   (usocket:socket-stream socket)
	     :unwrap-stream-p t
	     :external-format '(:latin-1 :eol-style :lf))))   ; Can be opened with utf-8
    https))

(defun https-request (request &key (header-parser 'parse-headers) (data-line nil) (headers nil))
  "Send a single https request to server and return list with results
header-parser is function which is called when HTTP headers arrive
data is post request data
headers is line with additional headers"
  (let ((https (open-https-socket)))
    (unwind-protect
	 (progn
	   (format https (construct-request request headers))
	   (when data-line
	       (format https "~a~%" data-line))
	   (force-output https)
	   (let ((data '()))
	     (loop :for line = (read-line-crlf https nil)
		:while line :do
		(progn
		  (push line data)
		  (when (string= (car data) "")
		    (if (not (string= (cadr data) "0"))
			(progn
			  (handler-bind
			      ((http-error #'handle-http-error))
			    (funcall header-parser data))
			  (setf data nil))
			(return (cddr data))))))))
      (close https))))

(defun api-get (endpoint)
  "Sends GET request to an API endpoint. /api/ is prepended.

Example:
 (api-get \"user/username\") will return a JSON string with user info"
  (car (https-request (format nil "GET /api/~a HTTP/1.1" endpoint))))

(defun api-post (endpoint data &optional (csrf nil))
  "Sends POST request to an API endpoint. /api/ is prepended.
data - list of data tuples in format ((param_name . param_value))

Example:
 (api-post \"login\" '((\"login\" . \"user\") (\"password\" . \"megapass\") will post line \"login=user&password=megapass\" to endpoint /api/login."
  (let ((data-line ""))
    (mapcar #'(lambda (l)
		(let ((name (car l))
		      (value (cdr l)))
		  (setf data-line (concatenate 'string data-line name "=" value "&"))))
	    data)
    (setf data-line (remove #\& data-line :from-end t :count 1))
    (car (https-request (format nil "POST /api/~a HTTP/1.1" endpoint)
		   :data-line data-line
		   :headers (format nil "Content-Type: application/x-www-form-urlencoded; charset=utf-8~%Content-Length: ~d~@[~%X-CSRF: ~a~]~%" (length data-line) (when csrf *auth-csrf-token*))))))

(defun api-put (endpoint data)
  "Sends PUT request to API endpoint. /api/ is prepended.
data - list of data tuples in format ((param_name . param_value))
csrf token is set for every put request

Example:
 (api-put \"post/postid\" '((\"text\" . \"updated post\") (\"tag\" . \"new,tag,line\") will post line \"text=\"updated post\"&tag=new,tag,line\" to endpoint /api/post/postid."
  (let ((data-line ""))
    (mapcar #'(lambda (l)
		(let ((name (car l))
		      (value (cdr l)))
		  (setf data-line (concatenate 'string data-line name "=" value "&"))))
	    data)
    (setf data-line (remove #\& data-line :from-end t :count 1))
    (car (https-request (format nil "PUT /api/~a HTTP/1.1" endpoint)
		   :data-line data-line
		   :headers (format nil "Content-Type: application/x-www-form-urlencoded; charset=utf-8~%Content-Length: ~d~%X-CSRF: ~a~%" (length data-line) *auth-csrf-token*)))))

(defun api-delete (endpoint)
  "Sends DELETE request to API endpoint. /api/ is prepended.
csrf token is set for every delete request"
  (car (https-request (format nil "DELETE /api/~a HTTP/1.1" endpoint)
		      :headers (format nil "X-CSRF: ~a~%" *auth-csrf-token*))))

(defun api-login (login password)
  "Logs in into API and sets up *cookie* *auth-token* and *csrf-token*"
  (with-decoder-simple-clos-semantics
    (let* ((*json-symbols-package* 'cl-point-bot.connection)
	   (json (api-post "login" (list (cons "login" login) (cons "password" password))))
	   (response (decode-json-from-string json)))
      (with-slots (token csrf--token error) response
	(handler-bind
	    ((auth-error #'handle-auth-error))
	  (if (slot-boundp response 'error)
	      (signal 'auth-error :message (values error))
	      (cons (setf *auth-token* (values token))
		    (setf *auth-csrf-token* (values csrf--token)))))))))

(defun api-logout ()
  "Cleans out logged in account"
  (with-decoder-simple-clos-semantics
    (let* ((*json-symbols-package* 'cl-point-bot.connection)
	   (json (api-post "logout" (list (cons "csrf_token" *auth-csrf-token*))))
	   (response (decode-json-from-string json)))
      (with-slots (ok) response
	(setf *auth-token* nil)
	(setf *auth-csrf-token* nil)
	(setf *cookie* nil)
	(values ok)))))
