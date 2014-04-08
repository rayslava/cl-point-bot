(defpackage :cl-point-bot.websocket
  (:use :cl :cl-point-bot.connection :sb-bsd-sockets :cl-json)
  (:export))

(in-package :cl-point-bot.websocket)

(defparameter websocket-key "x3JJHMbDL1EzLkh9GBhXDw==" "A base64 encoded random value")

(defvar bsd-socket t "Underneath socket")

(defvar websock-socket t "Socket used for communications")

(defparameter use-mask nil "Using mask for websocket messages")

(defun format-websocket-headers ()
  "Creates a string with additional headers to start websocket communication"
  (format nil "Connection: Upgrade~%Sec-WebSocket-Key: ~a~%Sec-WebSocket-Protocol: chat,superchat~%Sec-WebSocket-Version: 13~%Upgrade: WebSocket~%" websocket-key))

(defun start-websocket ()
  (let* ((socks (open-https-socket))
	 (https (setf websock-socket (cdr socks))))
    (setf bsd-socket (car socks))
    (unwind-protect
	 (progn
	   (format https (construct-request "GET /ws HTTP/1.1" (format-websocket-headers)))
	   (force-output https)
	   (let ((data '()))
	     (loop :for line = (read-line https)
		:while (and line (not (string= (make-array 1 :element-type 'character
							   :initial-contents (list #\return)) line))) :do
		(progn
		  (push line data)
		  (print line)
		  (handler-bind
		      ((http-error #'cl-point-bot.connection::handle-http-error))
		    (funcall #'cl-point-bot.connection::parse-headers data)))))))))


(defun read-data-portion (&optional (count nil))
  "Reads data portion from socket and returns list with bytes
If count is nil then reads all the data from socket else reads count bytes"
  (let ((data (list))
	(read 0))
    (loop for byte = (push (read-byte websock-socket) data)
	 while (if count
		   (< read count)
		   (listen websock-socket)) do
	 (when count
	   (incf read)))
    (print data)
    (reverse data)))

(defun socket-loop ()
  (let ((data (read-data-portion)))
    (send-data (build-ping))
    (parse-ws-packet (make-array (length data) :initial-contents data))
    (socket-loop)))



(defun close-socket ()
  (close websock-socket))

(defun send-data (data)
  (print data)
  (write-sequence data websock-socket))

(defclass ws-header ()
  ((header-size :accessor header-size)
   (fin :accessor fin)
   (mask :accessor mask)
   (opcode :accessor opcode)
   (N0 :accessor N0)
   (N :accessor N)
   (masking-key :accessor masking-key))
  (:documentation "Header of websocket packet"))

(defparameter debug-packet (make-array 20 :element-type '(unsigned-byte 8)))
(defparameter ping-packet (make-array 2 :element-type '(unsigned-byte 8)
				      :initial-contents '(#x8a 0)))
(defparameter text-ping-packet (make-array 6 :element-type '(unsigned-byte 8)
				      :initial-contents '(#x81 #x04 #x70 #x69 #x6e #x67)))

(defun text-message-callback (data)
  "This is called by packet parser when text message received"
  (cl-json::with-decoder-simple-clos-semantics
    (let* ((line (map 'string #'code-char data))
	   (*json-symbols-package* :cl-point-bot.websocket)
	   (message (if (string= line "ping")
		  "{\"a\": \"text ping\"}"
		  (cl-json::decode-json-from-string line))))
      (print line)
      (with-slots (a cut author tags text post_id) message
	(print (values a cut author tags text post_id))))))




(defun parse-ws-packet (data)
  "Parses a vector with packet data and returns an ws-header"
  (print data)
  (when (< (length data) 2)
    (error "Length < 2"))
  (let ((header (make-instance 'ws-header))
	(i 0))
    (setf (fin header)
	  (if (= #x80 (logand #x80 (aref data 0)))
	      T
	      nil))
    (setf (opcode header)
	  (logand (aref data 0) #x0f))
    (setf (mask header)
	  (logand (aref data 1) #x80))
    (setf (N0 header)
	  (logand (aref data 1) #x7f))
    (setf (header-size header)
	  (+ 2
	     (if (= (N0 header) 126) 2 0)
	     (if (= (N0 header) 127) 6 0)
	     (if (not (= (mask header) 0)) 4 0)))
    (when (< (length data) (header-size header))
      (error "Header says message should be longer"))
    (if (< (N0 header) 126)
	(progn
	  (setf (N header) (N0 header))
	  (setf i 2))
	(if (= (N0 header) 126)
	    (progn
	      (setf (N header) 0)
	      (setf (N header)
		    (logior (ash (aref data 2) 8)
			    (aref data 3)))
	      (setf i 4))
	    (when (= (N0 header) 127)
	      (setf (N header) 0)
	      (setf (N header)
		    (logior
		     (ash (aref data 2) 56)
		     (ash (aref data 3) 48)
		     (ash (aref data 4) 40)
		     (ash (aref data 5) 32)
		     (ash (aref data 6) 24)
		     (ash (aref data 7) 16)
		     (ash (aref data 8) 8)
		     (ash (aref data 9) 0)))
	      (setf i 10))))
    
    (if (not (= (mask header) 0))
	(setf (masking-key header)
	      (make-array 4 :element-type '(unsigned-byte 8)
			  :initial-contents
			  (list (aref data i)
				(aref data (+ i 1))
				(aref data (+ i 2))
				(aref data (+ i 3)))))
	(make-array 4  :element-type '(unsigned-byte 8)
		    :initial-contents '(0 0 0 0)))
    (when (< (length data)
	     (+ (header-size header) (N header)))
      (let ((need-bytes (- (+ (header-size header) (N header)) (length data)))
	    (index (length data)))
	(print (format nil "Needed bytes: ~d, index: ~d, data: ~a~%" need-bytes index data))
	(adjust-array data (+ (header-size header) (N header) 1))
	(print (format nil "Needed bytes: ~d, index: ~d, data: ~a~%" need-bytes index data)) 
	(loop for i from 0 to need-bytes do
	     (setf (aref data index) (car (read-data-portion 1)))
	     (print data)
	     (incf index))))

    (case (opcode header)
      ; continuation
      (0 (print "continuation"))
      ; text
      (1 (progn
	   (when (not (= (mask header) 0))
	     (loop :for i :from 0 :to (N header) :do
		(setf (aref data (+ i (header-size header)))
		      (logior (aref data (+ i (header-size header)))
			      (aref (masking-key header) (logand i 3))))))
	   (text-message-callback (subseq data (header-size header) (+ (header-size header) (N header))))))
      ; binary
      (2 (print "binary"))
      ; close
      (8 (close-socket))
      ; ping
      (9 (progn
	   (when (not (= (mask header) 0))
	     (loop :for i :from 0 :to (N header) :do
		(setf (aref data (+ i (header-size header)))
		      (logior (aref data (+ i (header-size header)))
			      (aref (masking-key header) (logand i 3))))))
	   (send-data (subseq data (header-size header) (+ (header-size header) (N header))))))
      ; pong
      (10 (print "pong")))))

(defun generate-message (opcode &optional (payload nil))
  "Builds a websocket message to send to socket"
  (let* ((header-len (+ 2
			(if payload
			    (+
			     (if (>= (length payload) 126)
				 2
				 0)
			     (if (>= (length payload) 65536)
				 6
				 0)
			     (if use-mask
				 4
				 0))
			    0)))
	 (data (make-array header-len :element-type '(unsigned-byte 8) :adjustable t))
	 (masking-key #(#x12 #x34 #x56 #x78)))
    (setf (aref data 0) (logior #x80 opcode))
    (when payload
      (if (< (length payload) 126)
	  (progn
	    (setf (aref data 1) (logior (logand #xff (length payload))
					(if use-mask #x80 #x0)))
	    (when use-mask
	      (loop for i from 0 to 4 do
		   (setf (aref data (+ i 2)) (aref masking-key i)))))
	  (if (< (length payload) 65536)
	      (progn
		(setf (aref data 1) (logior 126
					    (if use-mask #x80 #x0)))
		(setf (aref data 2) (logand (ash (length payload) -8) #xff))
		(setf (aref data 3) (logand (length payload) #xff))
		(when use-mask
		  (loop for i from 0 to 4 do
		       (setf (aref data (+ i 4)) (aref masking-key i)))))
	      (progn
		(setf (aref data 1) (logior 127
					    (if use-mask #x80 #x0)))
		(loop for i from 0 to 7 do
		     (setf (aref data (+ i 2)) (logand (ash (length data) (* i 8 -1)) #xff)))
		(when use-mask
		  (loop for i from 0 to 4 do
		       (setf (aref data (+ i 10)) (aref masking-key i)))))))
      (print (length data))
      (adjust-array  data (+ header-len (length payload)))
      (print (length data))
      (loop for byte in payload
	 for i from header-len to (length data) do
	   (setf (aref data i) byte)))
    data))

(defun build-ping ()
  (generate-message 9))

(defun build-pong ()
  (generate-message 10))

(defun build-text-message (text)
  "Build an array to write from string text"
  (generate-message 1 (loop for c in (coerce text 'list) collecting (char-code c))))
