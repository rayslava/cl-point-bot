(defpackage :cl-point-bot.websocket
  (:use :cl :cl-point-bot.connection)
  (:export))

(in-package :cl-point-bot.websocket)

(defparameter websocket-key "x3JJHMbDL1EzLkh9GBhXDw==" "A base64 encoded random value")

(defvar websock-socket t "Socket used for communications")

(defun format-websocket-headers ()
  "Creates a string with additional headers to start websocket communication"
  (format nil "Connection: Upgrade~%Sec-WebSocket-Key: ~a~%Sec-WebSocket-Protocol: chat,superchat~%Sec-WebSocket-Version: 13~%Upgrade: WebSocket~%" websocket-key))

(defun start-websocket ()
  (https-request "GET /ws HTTP/1.1" :headers (format-websocket-headers) :single-shot websock-socket)
  (print websock-socket))

    struct wsheader_type {
        unsigned header_size;
        bool fin;
        bool mask;
        enum opcode_type {
            CONTINUATION = 0x0,
            TEXT_FRAME = 0x1,
            BINARY_FRAME = 0x2,
            CLOSE = 8,
            PING = 9,
            PONG = 0xa,
        } opcode;
        int N0;
        uint64_t N;
        uint8_t masking_key[4];
    };

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

(defun parse-ws-packet (data)
  "Parses a vector with packet data and returns an ws-header"
  (print data)
  (when (< (length data) 2)
    (error "Too short!"))
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
      (error "Too short!"))
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
      (error "Too short!"))

    header))
