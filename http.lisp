(require :sb-bsd-sockets)

(defpackage :HTTPL
  (:use :CL :sb-bsd-sockets))
(in-package :HTTPL)

(defun read-from-socket (socket)
  ;; (let ((response (multiple-value-list (socket-receive socket nil 10))))
  ;;   (format t "~A" response)
  ;;   (first response))
  (apply #'concatenate 'string 
	 (loop for response = (multiple-value-list (socket-receive socket nil 10 :dontwait t))
	    collecting (first response)
	    until (or (not (second response)) (= (second response) 0))))

  ;; (multiple-value-bind (buffer length)
  ;;     (socket-receive socket nil 10)
  ;;   (subseq buffer 0 length))
  )

(defun respond (socket response)
  (let ((payload (concatenate 'string response '(#\Return #\Linefeed))))
    (socket-send socket payload nil)))

(defun server (&optional (port 8080))
  (let ((socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
	 (progn
	   (socket-bind socket #(127 0 0 1) port)
	   (socket-listen socket 10)	; good value for backlog parameter?
	   (loop do (let ((client-socket (socket-accept socket)))
		      (unwind-protect
		       (progn 
			 (format t  "-> ~A~%" (read-from-socket client-socket))
			 (respond client-socket "HTTP/1.1 200 OK")
			 (socket-close client-socket))
		       (socket-close client-socket)))))
	 (socket-close socket))
    ))
