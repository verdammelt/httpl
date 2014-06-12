(require :sb-bsd-sockets)

(defpackage :HTTPL
  (:use :CL :sb-bsd-sockets)
  (:export :server))
(in-package :HTTPL)

(defun read-from-socket (socket)
  (let ((buffer-size (* 10 1024)))
   (multiple-value-bind (buffer length)
       (socket-receive socket nil buffer-size)
     (subseq buffer 0 length))))

(defun respond (socket response)
  (dolist (r (or (and (listp response) response)
		 (list response)))
    (socket-send socket r nil)
    (socket-send socket (coerce '(#\Return #\Linefeed) 'string) nil)))

(defun process-request (socket)
  (unwind-protect
       (progn 
	 (format t  "-> ~A~%" (read-from-socket socket))
	 (respond socket "HTTP/1.1 200 OK"))
    (socket-close socket)))

(defun server (&optional (port 8080))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
	 (progn
	   (socket-bind socket #(127 0 0 1) port)
	   (socket-listen socket 5)
	   (loop (process-request (socket-accept socket))))
      (socket-close socket))))
