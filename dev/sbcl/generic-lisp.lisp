(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; specify that we're using the SBCL interface
;;; ---------------------------------------------------------------------------

(setf (default-interface) :SBCL)

;;; ---------------------------------------------------------------------------

(defmethod is-interface-available-p ((interface (eql :SBCL)))
  (values t))


;;; ---------------------------------------------------------------------------
;;; quitting
;;; ---------------------------------------------------------------------------

(defmethod quit-lisp* ((interface (eql :SBCL)))
  (cl-user::quit))


;;; ---------------------------------------------------------------------------
;;; memory management stuff
;;; ---------------------------------------------------------------------------

(defmethod total-bytes-allocated* ((interface (eql :SBCL)))
  (values (cl-user::get-bytes-consed)))

;;; ---------------------------------------------------------------------------

(defmethod gc-time* ((interface (eql :SBCL)))
  (values cl-user::*gc-run-time*))

;;; ---------------------------------------------------------------------------

(defmethod collect-garbage* ((interface (eql :SBCL)))
  ;;?? Gary King 2005-11-16: not sure what the parameter to this means
  (sb-kernel::collect-garbage 0))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************