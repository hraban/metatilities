(in-package #:ccl)

(defclass image-window (window)
  ((image-dialog-item :initform nil :reader image-dialog-item)
   (margin :initform 5 :initarg :margin :reader margin)
   (filename :initarg :filename :reader filename))
  (:default-initargs
    :color-p t))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((window image-window) &key)
  (setf (slot-value window 'image-dialog-item)
        (make-instance 'image-dialog-item 
          :view-nick-name :image-dialog-item
          :filename (filename window)))
  (add-subviews window (slot-value window 'image-dialog-item)))

;;; ---------------------------------------------------------------------------

(defmethod window-size-parts ((window image-window))
  (when (image-dialog-item window)
    (let ((margin (margin window))
          (window-size (view-size window)))
      (set-view-size (image-dialog-item window)
                     (make-point (- (point-h window-size) (+ margin margin))
                                 (- (point-v window-size) (+ margin margin))))
      (set-view-position (image-dialog-item window) (make-point margin margin)))))

;;; ---------------------------------------------------------------------------

(defun open-image-window (filename &rest args)
  (apply #'make-instance 'image-window :filename filename args))


#+Test
(open-image-window #P"clnuplot:plots;20050524-roc-p-4-4000.pdf")
