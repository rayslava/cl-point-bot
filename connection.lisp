;(declaim (optimize (debug 3) (speed 0) (space 0)))

(defpackage :cl-point-bot.connection
  (:use :cl :cl+ssl :usocket :cl-ppcre :json)
  (:export :https-request))

(defparameter *point-host* "point.im")

(defparameter *websocket-url* "https://point.im/ws")

(defparameter *port* 443)

(defvar *auth-token* nil "A token returned after login to API")

(defvar *auth-csrf-token* nil "A csrf token needed for logout")

(defvar *cookie* nil "Current cookie which should be passed to server")

(define-condition http-error (error)
  ((code :initarg :code :reader code))
  (:documentation "HTTP protocol error happened."))


(define-condition auth-error (error)
  ()
  (:documentation "NonAuthorized message received from API"))

(defun handle-http-error (err)
  "HTTP Protocol error handling

TODO: implement handling actually"
  (error (format nil "HTTP error ~d~%" (code err))))

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
		    (cookie-scanner (create-scanner "^Set-Cookie:\\s(.*)$")))
		    (register-groups-bind (http-code)
			(http-scanner line)
		      (if (not (= (parse-integer http-code) 200))
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

(defun https-request (request &key (header-parser 'parse-headers) (data nil) (headers nil))
  "Send a single https request to server and return list with results
header-parser is function which is called when HTTP headers arrive"
  (let* ((socket
	 (usocket:socket-connect
	  *point-host* *port*
	  :element-type '(unsigned-byte 8)))
	 (https
	  (cl+ssl:make-ssl-client-stream
	   (usocket:socket-stream socket)
	     :unwrap-stream-p t
	     :external-format '(:iso-8859-1 :eol-style :lf))))
    (unwind-protect
	 (progn
	   (format https (construct-request request headers))
	   (when data
	     (format https data))
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
      (close https)))))

(defun api-get (endpoint)
  "Sends GET request to an API endpoint. /api/ is prepended.

Example:
 (api-get \"user/username\") will return a JSON string with user info"
  (car (https-request (format nil "GET /api/~a HTTP/1.1" endpoint))))

(defun api-post (endpoint data)
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
		   :data data-line
		   :headers (format nil "Content-Length: ~d~%" (length data-line))))))

(defun api-login (login password)
  "Logs in into API and sets up *cookie* *auth-token* and *csrf-token*"
  (with-decoder-simple-clos-semantics
    (let* ((*json-symbols-package* nil)
	   (response (decode-json-from-string (api-post "login" (list (cons "login" login) (cons "password" password))))))
      (with-slots (token csrf--token) response
	(cons (setf *auth-token* (values token))
	      (setf *auth-csrf-token* (values csrf--token)))))))

(defun api-logout ()
  "Cleans out logged in account"
  (with-decoder-simple-clos-semantics
    (let* ((*json-symbols-package* nil)
	   (x (decode-json-from-string (api-post "logout" (list (cons "csrf_token" *auth-csrf-token*))))))
      (with-slots (ok) x
	(setf *auth-token* nil)
	(setf *auth-csrf-token* nil)
	(setf *cookie* nil)
	(values ok)))))
