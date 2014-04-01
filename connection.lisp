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

(defun https-request (endpoint)
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
	   (let ((prevline nil))
	     (loop :for line = (read-line-crlf https nil)
		:while line :do
		(progn
		  (setf prevline line)
		  (format t "~a~%" line)
;		  (map 'string #'(lambda (c) (print c)) line)
		  (when (and (string= prevline line)
			     (string= prevline #\0))
		    (return)))))
      (close https)))))
