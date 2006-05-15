(in-package #:metatilities)
        
(defun linearize-array (array)
  (make-array (array-total-size array) 
              :displaced-to array
              :element-type (array-element-type array)))

(defun copy-array (array)
  (let ((storage (copy-seq (linearize-array array))))
    (make-array (array-dimensions array) :displaced-to storage)))

(defun maparray (array fn)
  (loop for x across (linearize-array array)  do
        (funcall fn x))
  array)

(defun maparray! (array fn)
  (let ((temp (linearize-array array)))
    (loop for i from 0 to (1- (array-total-size temp)) do
          (setf (aref temp i) (funcall fn (aref temp i))))
    array))

(defun array-row (array row)
  "Returns the row'th row of array. Array is assumed to be two dimensional and row 
is assumed to be in range. The returned array shared structure with the array parameter."
  (make-array (array-dimension array 1)
              :displaced-to (linearize-array array)
              :displaced-index-offset (* row (array-dimension array 1))))