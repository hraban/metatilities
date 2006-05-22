#|

Copyright 2001 Experimental Knowledge Systems Lab, University of Massachusetts Amherst
Professor Paul Cohen, Director

Author: Gary Warren King

|#

(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; beeping
;;; ---------------------------------------------------------------------------

(defmethod interface-beep* ((interface (eql :CCL)) &rest args)
  (declare (ignore args))
  (ccl:beep))

;;; ---------------------------------------------------------------------------
;;; yes or no questions
;;; ---------------------------------------------------------------------------

(defmethod y-or-n-question* ((interface (eql :CCL)) message &rest args)
  (apply #'ccl:y-or-n-dialog message args))

;;; ---------------------------------------------------------------------------
;;; make-ui-point
;;; ---------------------------------------------------------------------------

(defmethod make-ui-point* ((interface (eql :CCL)) x y)
  (ccl:make-point x y))

;;; ---------------------------------------------------------------------------
;;; choose-file-question
;;; ---------------------------------------------------------------------------

(defmethod choose-file-question* ((interface (eql :CCL)) &rest args)
  (apply #'ccl:choose-file-dialog args))

;;; ---------------------------------------------------------------------------

(defmethod choose-new-file-question* ((interface (eql :CCL)) &rest args)
  (apply #'ccl:choose-new-file-dialog args))

;;; ---------------------------------------------------------------------------

(defmethod choose-directory-question* ((interface (eql :CCL)) &rest args)
  (apply #'ccl:choose-directory-dialog args))

;;; ---------------------------------------------------------------------------

(defmethod choose-item-question* ((interface (eql :CCL)) list &rest args 
                                  &key (title "Select item:" title-supplied?)
                                  &allow-other-keys)
  (when title-supplied?
    (remf args :title)
    (setf (getf args :window-title) title))
  (apply #'ccl:select-item-from-list list args))

;;; ---------------------------------------------------------------------------

(defmethod prompt-for* ((interface (eql :CCL)) (type (eql 'string))
                        prompt &rest args &key &allow-other-keys)
  (apply #'ccl:get-string-from-user prompt args))

;;; ---------------------------------------------------------------------------

(defmethod choose-item-from-pup* ((interface (eql :CCL)) the-list &rest args &key &allow-other-keys)
  (multiple-value-bind (result index)
                       (apply #'ccl:select-item-from-pup the-list args)
    (if (and (null result) (null index))
      (throw :cancel (values result index))
      (values result index))))

;;; ---------------------------------------------------------------------------

(defmethod put-item-on-clipboard* ((interface (eql :CCL)) thing)
  ;; this puts it on the clipboard
  (ccl:put-scrap :lisp thing))

;;; ---------------------------------------------------------------------------

(defmethod gui-warn* ((interface (eql :CCL)) string &rest args
                      &key (ok-text "OK") 
                      (title "Warning")
                      (size #@(400 200))
                      &allow-other-keys)
  (apply #'message-dialog string 
         :title title :size size :ok-text ok-text 
         :allow-other-keys t args))

#|

;;; ---------------------------------------------------------------------------
;;; progress bar
;;; ---------------------------------------------------------------------------

(unless (fboundp 'ccl:set-progress-bar-setting)
  (defmethod ccl:progress-bar-setting (&rest args)
    (declare (ignore args)))
  (defmethod ccl:set-progress-bar-setting (&rest args)
    (declare (ignore args)))
  (defsetf ccl:progress-bar-setting ccl:set-progress-bar-setting))

;;; ---------------------------------------------------------------------------

(defvar *progress-bar-last-position* nil)
(defvar *progress-bar-last-size* nil)

;;; ---------------------------------------------------------------------------

(defclass* eksl-progress-bar-dialog-item (ccl:progress-bar-dialog-item)
  "This is an attempt to work around the min/max limitations on the MCL progress
bar (Mac OS progress bar?). It's not working. The values all seem correct but the
display isn't updating properly. Maybe it's something obvious but I'm missing it 
today so it's not currently being used. Sigh.

It probably conses like hell too."
  ((smallest-min 0 ir)
   (biggest-max 0 ir)
   (real-min 0 ir)
   (real-max 0 ir)
   (translate-values nil a)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object eksl-progress-bar-dialog-item) &key)
  (setf (values (slot-value object 'smallest-min) 
                (slot-value object 'biggest-max))
        (ccl::mac-progress-bar-min-max most-negative-fixnum most-positive-fixnum)
        
        (slot-value object 'smallest-min) 0
        (slot-value object 'biggest-max) 100
        
        (slot-value object 'real-min) (ccl:progress-bar-min object)
        (slot-value object 'real-max) (ccl:progress-bar-max object)
        (ccl:progress-bar-min object) (smallest-min object)
        (ccl:progress-bar-max object) (biggest-max object)
        (translate-values object) t))

;;; ---------------------------------------------------------------------------

(defmethod ccl:progress-bar-min ((item eksl-progress-bar-dialog-item))
  (if (translate-values item)
    (real-min item)
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod ccl:set-progress-bar-min ((item eksl-progress-bar-dialog-item) value)
  (cond ((translate-values item)
         (setq value (require-type value 'fixnum))
         (setf (slot-value item 'real-min) value)
         (ccl::update-progress-bar-min-max item) 
         (values value))
        (t 
         (call-next-method))))

;;; ---------------------------------------------------------------------------

(defmethod ccl:progress-bar-max ((item eksl-progress-bar-dialog-item))
  (if (translate-values item)
    (real-max item)
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod ccl:set-progress-bar-max ((item eksl-progress-bar-dialog-item) value)
  (cond ((translate-values item)
         (setq value (require-type value 'fixnum))
         (setf (slot-value item 'real-max) value)
         (ccl::update-progress-bar-min-max item) 
         (values value))
        (t 
         (call-next-method))))
            
;;; ---------------------------------------------------------------------------

(defmethod ccl:progress-bar-setting ((item eksl-progress-bar-dialog-item))
  (if (translate-values item)
    (unwind-protect
      (progn
        (setf (translate-values item) nil)
        (truncate (linear-scale (call-next-method)   
                                (smallest-min item) (biggest-max item)
                                (real-min item) (real-max item))))
      (setf (translate-values item) t))
    (call-next-method)))

;;; ---------------------------------------------------------------------------

(defmethod ccl:set-progress-bar-setting ((item eksl-progress-bar-dialog-item) value)
  (setq value (require-type value 'fixnum))
  (let ((translate? (translate-values item)))
    (unwind-protect
      (progn
        (setf (translate-values item) nil)
        (if translate?
          (call-next-method 
           item
           (truncate (linear-scale value
                                   (real-min item) (real-max item)
                                   (smallest-min item) (biggest-max item))))
          (call-next-method)))
      (setf (translate-values item) translate?)))
        
  (values value))

;;; ---------------------------------------------------------------------------

(defmethod make-progress-bar ((interface (eql :CCL)) min max title
                              &key (determinate-p t) (view-position nil)
                              (nested? t))
  (let ((view-size-x 160)
        (view-size-y 20)
        (extra-y 10)
        (offset-view (and nested? (not view-position) 
                          (plusp *progress-bar-count*))))
    (let ((view (apply #'make-instance 'ccl:palette-windoid
                        :view-size (make-point view-size-x view-size-y)
                        :window-type :windoid
                        :close-box-p nil
                        :window-show nil
                        :window-title title
                        (cond (view-position 
                               (list :view-position view-position))
                              ((and offset-view
                                    (first *progress-bar-last-position*))
                               (list :view-position (first *progress-bar-last-position*)))
                              (t
                               (list :auto-position :centermainscreen))))))
      (multiple-value-bind (ff ms) (view-font-codes view)
        (let ((title-width (max 160 (+ 20 (font-codes-string-width title ff ms))))
              (bar (make-instance 
                     #+No 'eksl-progress-bar-dialog-item
                     'progress-bar-dialog-item
                     :view-position #@(10 4)
                     :view-size (make-point (- title-width 20) 12)
                     :min min
                     :max max
                     :determinate-p determinate-p)))
          
          (set-view-size view title-width 20)
          (when offset-view
            (set-view-position view 
                               (+ (point-h (view-position view))
                                  (round (- (point-h *progress-bar-last-size*)
                                            (point-h (view-size view))) 2))
                               (+ (point-v (view-position view))
                                  (* #+Ignore *progress-bar-count* 
                                     (+ extra-y (point-v (view-size view)))))))
          (add-subviews view bar)
          (window-show view)
          (push (view-position view) *progress-bar-last-position*)
          (setf *progress-bar-last-size* (view-size view))
          
          (values view bar))))))
    
;;; ---------------------------------------------------------------------------

(defmethod progress-bar-value ((interface (eql :CCL)) bar)
  (ccl:progress-bar-setting bar))

;;; ---------------------------------------------------------------------------

(defmethod (setf progress-bar-value) (value (interface (eql :CCL)) bar)
  (ccl:set-progress-bar-setting bar value))

;;; ---------------------------------------------------------------------------

(defmethod close-progress-bar ((interface (eql :CCL)) bar)
  (pop *progress-bar-last-position*)
  (ccl:window-close bar))

;;; ---------------------------------------------------------------------------

#+TEST
(with-progress-bar (0 10)
  (loop repeat 5 do (incf (progress)) (spy (progress)) (sleep .1)))

|#

;;; ---------------------------------------------------------------------------
;;; inspect-thing
;;; ---------------------------------------------------------------------------

(defmethod inspect-thing* ((interface (eql :CCL)) thing &rest args)
  (declare (ignore args))
  (let ((it (inspector::find-inspector-pane thing))) 
    (if it (ccl:window-select it) (inspect thing))))

#|
;;; ---------------------------------------------------------------------------
;;; sound-note
;;; ---------------------------------------------------------------------------

(defvar *default-instrument* nil)

(defmethod sound-note* ((interface (eql :CCL)) pitch velocity &rest args)
  (apply #'ccl:play-note pitch velocity (or args *default-instrument*)))

;;; ---------------------------------------------------------------------------

(defmethod stop-notes* ((interface (eql :CCL)))
  (ccl:stop-the-music))

;;; ---------------------------------------------------------------------------

(defmethod select-instrument* ((interface (eql :CCL)) instrument &rest args)
  (declare (ignore args))
  (setf *default-instrument* (list instrument)))
|#


                        
;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
  