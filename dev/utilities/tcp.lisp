;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                         TCP Generic Functions                          *
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

(in-package metatilities)

(export '(open-socket-stream close-socket-stream with-open-socket
                             start-server))

;;;; **************************************************************************
;;;; fill in these functions for your implementation of LISP

;;; **************************************************************************
;;;
;;; client functionality
;;;

(defgeneric open-socket-stream (host port &rest args))
(defgeneric close-socket-stream (stream))

#-OPENMCL 
;; ok so I changed the syntax arbitrarily, but I assume nobody uses this
;; because the old version was bad - Westy
(defmacro with-open-socket ((stream host port-or-service &rest args) &body body)
  `(let ((,stream (open-socket-stream ,host ,port-or-service ,@args)))
     (unwind-protect (progn ,@body)
       (when ,stream
         (close-socket-stream ,stream)))))

;;; **************************************************************************
;;;
;;; server functionality
;;;


(defgeneric start-server (name listen-port connection-handler &rest handler-args)
  (:documentation 
   "the implementation of START-SERVER should start a process listening on
LISTEN-PORT, under the name NAME (where applicable). The server process
should generate a socket stream on incoming connections and hand the stream
to the function CONNECTION-HANDLER. HANDLER-ARGS are passed to the 
CONNECTION-HANDLER when the process is spawned."))


