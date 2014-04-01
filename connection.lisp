(defpackage :cl-point-bot.connection
  (:use :cl :cl+ssl :usocket)
  (:export :init-client))

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

(defun init-client (&key (unwrap-stream-p t))
  (let* ((socket
	 (usocket:socket-connect
	  *point-host* *port*
	  :element-type '(unsigned-byte 8)))
	(callback nil)
	 (https
	  (cl+ssl:make-ssl-client-stream
	   (usocket:socket-stream socket)
	     :unwrap-stream-p t
	     :external-format '(:iso-8859-1 :eol-style :lf))))
    (unwind-protect
	 (progn
	   (format https "GET /api/login HTTP/1.1~%Host: ~a~%~%" *point-host*)
	   (force-output https)
	   (loop :for line = (read-line-crlf https nil)
	      :while line :do
	      (format t "HTTPS> ~a~%" line)))
      (close https))))
