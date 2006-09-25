(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; specify that we're using the ALLEGRO interface
;;; ---------------------------------------------------------------------------

(setf (default-interface) :allegro)

;;; ---------------------------------------------------------------------------

(defmethod is-interface-available-p ((interface (eql :allegro)))
  (values t))


;;; ---------------------------------------------------------------------------
;;; quitting
;;; ---------------------------------------------------------------------------

(defmethod quit-lisp* ((interface (eql :allegro)))
  (excl:exit))


;;; ---------------------------------------------------------------------------
;;; memory management stuff
;;; ---------------------------------------------------------------------------

(defmethod collect-garbage* ((interface (eql :allegro)))
  (excl:gc t))

(defmethod total-bytes-allocated* ((interface (eql :allegro)))
  (values (sys::gsgc-totalloc-bytes t)))


#|

efmethod gc-time* ((interface (eql :allegro)))
  (values (gctime)))

|#


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************