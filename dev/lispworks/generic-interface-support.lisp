#|

Author: Gary Warren King

|#

(in-package #:metatilities)

;;; ---------------------------------------------------------------------------

(defmethod choose-file-question* ((interface (eql :lispworks)) &rest args
                                  &key (prompt "Choose file:"))
  #+WITHOUT-ARGS
  (multiple-value-bind (file success)
      (capi:prompt-for-file prompt)
    (if success
        file
      :cancel))
  #-WITHOUT-ARGS
  (multiple-value-bind (file success)
      (apply 'capi:prompt-for-file prompt (remf args :prompt))
    (if success
        file
      :cancel)))

;;; ---------------------------------------------------------------------------

(defparameter %table-cell-string-stream 
  (make-string-output-stream))

;;; ---------------------------------------------------------------------------

(defun %table-cell-contents-string-new (item print-function)
  (funcall print-function item %table-cell-string-stream)
  (get-output-stream-string %table-cell-string-stream))
  
;;; ---------------------------------------------------------------------------

(defmethod choose-item-question* ((interface (eql :lispworks)) list &rest args 
                                  &key (title "Select item:")
                                  (table-print-function #'princ) &allow-other-keys)
  (declare (ignore args))
  (loop 
    (format t "~&~A:" title)
    (loop for item in list
          for index from 0 do
          (format t "~&~2d. ~2d~%" index 
                  (%table-cell-contents-string-new item table-print-function)))
    (format t "~&Enter a number (-1 to cancel): ")
    (let ((num (parse-integer (princ-to-string (read)) :junk-allowed t)))
      (cond ((not (numberp num))
             (format t "~&Please enter a number.~%"))
            ((minusp num)
             (return :cancel))
            ((>= num (length list))
             (format t "~&Please enter a number between 0 and than ~d.~%" 
                     (1- (length list))))
            (t
             (return (list (nth num list))))))))


;;; ---------------------------------------------------------------------------
;;; inspect-thing
;;; ---------------------------------------------------------------------------

(defmethod inspect-thing* ((interface (eql :lispworks)) thing &rest args)
  (declare (ignore args))
  #+LISPWORKS4
  (lispworks-tools::inspect-object game-state)
  #+LISPWORKS3
  (tools:w-inspect game-state))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
  