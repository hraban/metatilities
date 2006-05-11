(in-package #:metatilities)

(defmethod reset ((object t))
  (values nil))

;;; ---------------------------------------------------------------------------
;;; samep and nearly samep
;;; ---------------------------------------------------------------------------

(declaim (special *samep-tolerance*)) 
(defvar *samep-tolerance* (coerce 1e-5 'double-float)
  "Used by samep to determine how close things need to be to be 'the same'.")

;;; ---------------------------------------------------------------------------

(declaim (special *samep-test*)) 
(defvar *samep-test* #'equal
  "Used by samep to determine if two things are 'the same'. Defaults to #'equal")

;;; ---------------------------------------------------------------------------

(defgeneric samep (thing-1 thing-2)
  (:documentation "Compares two things and returns true if they are the same
in the sense of being interchangable. Implementations use the special variable
*samep-tolerance* to specify how close two things need to be in order to be 
'the same'. See nearly-samep too.")
  (:method (thing-1 thing-2)
           (funcall *samep-test* thing-1 thing-2))
  (:method ((thing-1 integer) (thing-2 integer))
           ;; we specialize on integers so that they don't get compared 
           ;; with nearly-equal-p
           (= thing-1 thing-2))
  (:method ((thing-1 number) (thing-2 number))
           (= thing-1 thing-2))
  (:method ((thing-1 real) (thing-2 real))
           (nearly-equal-p thing-1 thing-2 *samep-tolerance*))
  (:method ((thing-1 string) (thing-2 string))
           (string-equal thing-1 thing-2)))

;;; ---------------------------------------------------------------------------

(defgeneric nearly-samep (thing-1 thing-2 tolerance)
  (:documentation "Compares two things and returns true if they are the same
in the sense of being interchangable. Tolerance indicates how close things need
to be in order to be 'the same'.")
  (:method (thing-1 thing-2 (tolerance number))
           (let ((*samep-tolerance* tolerance))
             (samep thing-1 thing-2))))

