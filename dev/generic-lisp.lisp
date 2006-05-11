(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; Interface determination
;;; ---------------------------------------------------------------------------

(defvar *default-interface* nil)

;;; ---------------------------------------------------------------------------

(defun default-interface ()
  "Return the current default interface (this is setfable)."
  *default-interface*)

;;; ---------------------------------------------------------------------------

(defun (setf default-interface) (value)
  (setf *default-interface* value))

;;; ---------------------------------------------------------------------------

(defgeneric is-interface-available-p (interface-name)
  (:documentation "Returns true is interface-name is available."))

;;; ---------------------------------------------------------------------------

(defmethod is-interface-available-p ((interface (eql nil)))
  (values nil))

;;; ---------------------------------------------------------------------------

(defun is-default-interface-available-p ()
  (is-interface-available-p *default-interface*))


;;; ---------------------------------------------------------------------------
;;; quitting
;;; ---------------------------------------------------------------------------

(defgeneric quit-lisp* (interface)
  (:documentation "Quits Lisp"))

(defmethod quit-lisp* (interface)
  (declare (ignore interface))
  (print "I would love to quit for you, but I'm not sure how?"))

;;; ---------------------------------------------------------------------------

(defun quit-lisp ()
  (quit-lisp* *default-interface*))


;;; ---------------------------------------------------------------------------
;;; memory management stuff
;;; ---------------------------------------------------------------------------

(defgeneric total-bytes-allocated* (interface)
  (:documentation "")
  (:method (interface)
           (declare (ignore interface))
           (values nil)))

;;; ---------------------------------------------------------------------------

(defun total-bytes-allocated ()
  "Returns the total number of bytes that this Lisp session has allocated."
  (total-bytes-allocated* *default-interface*))

;;; ---------------------------------------------------------------------------
          
(defgeneric gc-time* (interface)
  (:documentation "")
  (:method (interface)
           (declare (ignore interface))
           (values nil)))

;;; ---------------------------------------------------------------------------

(defun gc-time ()
  "Returns the total amount of time that this Lisp session has spent in garbage collection."
  (gc-time* *default-interface*))

;;; ---------------------------------------------------------------------------

(defgeneric collect-garbage* (interface)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defun collect-garbage ()
  "Tell lisp that now is a good time to collect any accumulated garbage."
  (collect-garbage* *default-interface*))



(defmacro make-load-form* (class-name)
  #+(or OPENMCL (not MCL) ANSI-MAKE-LOAD-FORM)
  `(defmethod make-load-form ((self ,class-name) &optional environment)
    (declare (ignore environment))
    (make-load-form-saving-slots self))
  #+(and DIGITOOL (not ANSI-MAKE-LOAD-FORM))
  `(defmethod make-load-form ((self ,class-name))
    (make-load-form-saving-slots self)))



#|
SB-IMPL::SIGCHLD-HANDLER (fbound)
SB-C::PACK-BEFORE-GC-HOOK (fbound)
SB-BIGNUM:BIGNUM-LOGCOUNT (fbound)
SB-BIGNUM:BIGNUM-GCD (fbound)
SB-BIGNUM::BIGNUM-GCD-ORDER-AND-SUBTRACT (fbound)
SB-BIGNUM::MAKE-GCD-BIGNUM-ODD (fbound)
SB-SYS:WITHOUT-GCING (fbound)
SB-UNIX:KBDGCLICK
SB-UNIX:SIGCONT (bound)
SB-UNIX:SIGCHLD (bound)
SB-VM::FAST-*-BIGC/FIXNUM=>FIXNUM
SB-KERNEL:SUB-GC (bound) (fbound)
SB-KERNEL:*ALREADY-MAYBE-GCING*
SB-KERNEL:TWO-ARG-GCD (fbound)
SB-KERNEL:*GC-INHIBIT* (bound)
SB-KERNEL:GC-REINIT
SB-KERNEL::*GC-TRIGGER* (bound)
SB-KERNEL::GC-START-THE-WORLD (fbound)
SB-KERNEL::*GC-MUTEX* (bound)
SB-KERNEL::PRE-GC-DYNAMIC-USAGE
SB-KERNEL::*ALREADY-IN-GC* (bound)
SB-KERNEL::GC-STOP-THE-WORLD (fbound)
GC (fbound)
*GC-NOTIFY-STREAM*
GC-OFF (fbound)
*BEFORE-GC-HOOKS* (bound)
*AFTER-GC-HOOKS* (bound)
*GC-NOTIFY-AFTER*
*GC-NOTIFY-BEFORE*
GC-ON (fbound)
BYTES-CONSED-BETWEEN-GCS (fbound)
*GC-RUN-TIME* (bound)
LOGCOUNT (fbound)
GCD (fbound)
*
|#

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************