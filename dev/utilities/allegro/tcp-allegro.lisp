;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       TCP Allegro Implementation                       *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Matt Schmill
;;;             Experimental Knowledge Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             Department of Computer Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  03-04-03 File Created.  (schmill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #:metatilities)

;;;; **************************************************************************

(eval-when (compile load eval)
  (require :sock))

;;;; **************************************************************************

(defmethod open-socket-stream (host port)
  (socket:make-socket :remote-host host :remote-port port))

(defmethod close-socket-stream (stream)
  (common-lisp::close stream))

;;;; **************************************************************************

(defmethod start-server (name listen-port connection-handler &rest handler-args)
  (let ((passive-socket (socket:make-socket :connect :passive :local-port listen-port)))
    (mp::process-run-function (format nil "~A Accept" name)
      #'(lambda (socket handler h-args)
	  (unwind-protect 
	      (loop while t do
		    (let ((connection (socket:accept-connection socket)))
		      (apply handler connection h-args)))
	    (close socket)))
      passive-socket connection-handler handler-args)))

#+TEST
(defun handle-connection (stream)
  (format stream "It connected, that's great~%")
  (close stream))

#+TEST
(start-server "Test Server" 10010 'handle-connection)
    
