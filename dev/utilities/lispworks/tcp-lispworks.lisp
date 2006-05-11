;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                      TCP Lispworks Implementation                      *
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
;;;  03-22-02 File Created.  (schmill)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package #:metatilities)

;;;; **************************************************************************

(defmethod open-socket-stream (host port &rest args)
  (apply #'comm:open-tcp-stream host port args))

(defmethod close-socket-stream (socket-stream)
  (common-lisp::close socket-stream))

;;;; **************************************************************************

(defmethod start-server (name listen-port connection-handler &rest handler-args)
  (comm:start-up-server :function #'(lambda (handle)
                                      (let ((stream (make-instance 'comm:socket-stream
                                                                   :socket handle
                                                                   :direction :io
                                                                   :element-type 'base-char)))
                                        (make-thread name #'(lambda () (apply connection-handler stream handler-args)))))
                        :service listen-port))

#+TEST
(defun c-handler (stream) (format stream "I hate you, goodbye.~%") (force-output stream))
#+TEST
(start-server "Assy" 9900 'c-handler)
