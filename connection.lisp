;(declaim (optimize (debug 3) (speed 0) (space 0)))

(defpackage :cl-point-bot.connection
  (:use :cl :cl+ssl :usocket :cl-ppcre)
  (:export :https-request))

(defparameter *point-host* "point.im")

(defparameter *websocket-url* "https://point.im/ws")

(defparameter *port* 443)

(defvar *auth-token* nil "A token returned after login to API")

(defvar *auth-csrf-token* nil "A csrf token needed for logout")

(defvar *cookie* nil "Current cookie which should be passed to server")

(define-condition http-error (error)
  ((code :initarg :code :reader code)))

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
	      (let ((http-scanner (create-scanner "^HTTP/1.1\\s+(\\d+)\\s+.[\\w\\s\\d]+$")))
		(register-groups-bind (http-code)
		    (http-scanner line)
		    (if (not (= (parse-integer http-code) 200))
		      (signal 'http-error :code (parse-integer http-code))))))
	      headers))

(defun construct-request (line)
  "Constructs HTTP request body except main line.
Actually fills up all the secondary headers"
  (format nil "~A~%Host: ~a~%~@[Cookie: ~a~%~]~@[Authorization: ~a~%~]~%~%"
	  line
	  *point-host*
	  *cookie*
	  *auth-token*))

(defun https-request (endpoint &key (header-parser 'parse-headers))
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
	   (format https (construct-request (format nil "GET ~a HTTP/1.1" endpoint)))
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
