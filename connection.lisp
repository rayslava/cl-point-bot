(defpackage :cl-point-bot.connection
  (:use :cl :cl+ssl :usocket)
  (:export :https-request))

(defparameter *point-host* "point.im")

(defparameter *websocket-url* "https://point.im/ws")

(defparameter *port* 443)

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
  (print headers))

(declaim (optimize (debug 3) (speed 0) (space 0)))

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
	   (format https "GET ~a HTTP/1.1~%Host: ~a~%~%" endpoint *point-host*)
	   (force-output https)
	   (let ((data '()))
	     (loop :for line = (read-line-crlf https nil)
		:while line :do
		(progn
		  (push line data)
		  (when (string= (car data) "")
		  (if (not (string= (cadr data) "0"))
		      (progn
			(funcall header-parser data)
			(setf data nil))
		      (return (cddr data))))))))
      (close https)))))
