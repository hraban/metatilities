(in-package #:metatilities)


#|

These classes and routines should be PLATFORM INDEPENDENT window/view related
things.

|#

;;; ---------------------------------------------------------------------------
;;; view-requiring-cleanup-mixin
;;; ---------------------------------------------------------------------------

(defclass view-requiring-cleanup-mixin ()
  ()
  (:documentation "A mixin for views that require clean-up *before* they 
are destroyed. To use, inherit from this class and write a method for 
clean-up-view progn. Lisp implementations are required to implement the
necessary window close method so that clean-up-view is called."))

;;; ---------------------------------------------------------------------------

(defgeneric clean-up-view (view-requiring-cleanup-mixin)
  (:method-combination progn))


;;; ---------------------------------------------------------------------------
;;; update-dialog-ui
;;; ---------------------------------------------------------------------------

(defgeneric update-dialog-ui (window-or-view)
  (:documentation "A generic place to hang dialog updating code.")
  (:method ((thing t))
           (values)))


;;; ---------------------------------------------------------------------------
;;; help text mixin
;;; ---------------------------------------------------------------------------

(defclass* help-text-mixin ()
  ()
  (:export-p t))

;;; ---------------------------------------------------------------------------

(defmethod help-spec ((view help-text-mixin))
  (values ""))


;;; ---------------------------------------------------------------------------
;;; margins-mixin
;;; ---------------------------------------------------------------------------

(defclass* margins-mixin ()
  ((left-margin 0 ia)
   (top-margin 0 ia)
   (right-margin 0 ia)
   (bottom-margin 0 ia))
  (:export-slots left-margin top-margin right-margin bottom-margin)
  (:export-p t))

;;; ---------------------------------------------------------------------------

(defmethod (setf left-margin) :after (value (view margins-mixin))
  (check-type value fixnum)
  (note-view-settings-changed view))

;;; ---------------------------------------------------------------------------

(defmethod (setf top-margin) :after (value (view margins-mixin))
  (check-type value fixnum)
  (note-view-settings-changed view))

;;; ---------------------------------------------------------------------------

(defmethod (setf right-margin) :after (value (view margins-mixin))
  (check-type value fixnum)
  (note-view-settings-changed view))

;;; ---------------------------------------------------------------------------

(defmethod (setf bottom-margin) :after (value (view margins-mixin))
  (check-type value fixnum)
  (note-view-settings-changed view))

#|

;;; ---------------------------------------------------------------------------
;;; scaled-view-mixin
;;; ---------------------------------------------------------------------------

(defclass* scaled-view-mixin (margins-mixin)
  ((scale 1.0 i :accessor view-scale :initarg :view-scale))
  (:export-p t))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object scaled-view-mixin) &key)
  (setf (view-scale object) (ensure-float (view-scale object))))

;;; ---------------------------------------------------------------------------

(defmethod (setf view-scale) :around ((new-scale number) (view scaled-view-mixin))
  (unless (= new-scale (view-scale view))
    (call-next-method (float new-scale) view) 
    (note-view-settings-changed view)))

;;; ---------------------------------------------------------------------------

(defmethod scale-x ((view scaled-view-mixin))
  (view-scale view))

;;; ---------------------------------------------------------------------------

(defmethod scale-y ((view scaled-view-mixin))
  (view-scale view))

;;; ---------------------------------------------------------------------------

#+Naive
(defmethod view-x/view-y->x/y ((view scaled-view-mixin) view-x view-y)
  (let ((left-margin (left-margin view))
        (top-margin (top-margin view))
        (scale-x (scale-x view))
        (scale-y (scale-y view)))
    (values
     (+ left-margin 
        (round (* view-x scale-x)))
     (+ top-margin
        (round (* view-y scale-y))))))

#-FPC-PPC
;; This version is slightly faster, and conses slightly less, but it should be
;; better!!!
(defmethod view-x/view-y->x/y ((view scaled-view-mixin) view-x view-y)
  (declare (optimize (speed 3) (space 3) (debug 0) (safety 0))
           (double-float view-x view-y)
           (dynamic-extent view-x view-y))
  (let ((left-margin (left-margin view))
        (top-margin (top-margin view))
        (scale-x (scale-x view))
        (scale-y (scale-y view)))
    (declare (type fixnum left-margin top-margin) 
             (double-float scale-x scale-y)
             (dynamic-extent left-margin top-margin scale-x scale-y))
    (values
     (the fixnum (+ left-margin 
                    (round (* view-x scale-x))))
     (the fixnum (+ top-margin
                    (round (* view-y scale-y)))))))

#+FPC-PPC
;; this version conses up only the two values that it returns
;; I wonder if we want a destructive version?
(defmethod view-x/view-y->x/y ((view scaled-view-mixin) view-x view-y)
  (let ((left-margin (left-margin view))
        (top-margin (top-margin view))
        (scale-x (scale-x view))
        (scale-y (scale-y view)))
    (with-temp-double-floats ((lm! 0d0)
                              (tm! 0d0))
      (%set-double! lm! (* view-x scale-x))
      (%set-double! tm! (* view-y scale-y))
      (values (+ left-margin (round lm!))
              (+ top-margin (round tm!))))))

#+TimingTest
(let ((v (make-instance 'scaled-view-mixin :view-scale 0.25))
      (vx 100.0) (vy 252.7) (x 0) (y 0))
  (timeit (:report t)
          (loop repeat 10000 do
                (setf (values x y) (view-x/view-y->x/y v vx vy))))) 

;;; ---------------------------------------------------------------------------

(defmethod view-rect->rect ((view scaled-view-mixin) view-x view-y view-w view-h)
  (let ((left-margin (left-margin view))
        (top-margin (top-margin view))
        (scale-x (scale-x view))
        (scale-y (scale-y view)))
    (declare (fixnum left-margin top-margin)
             (double-float scale-x scale-y)
             (dynamic-extent left-margin top-margin scale-x scale-y))
    (values
     (the fixnum (+ left-margin 
                    (round (* view-x scale-x))))
     (the fixnum (+ top-margin
                    (round (* view-y scale-y))))
     (the fixnum (round (* view-w scale-x)))
     (the fixnum (round (* view-h scale-y))))))

;;; ---------------------------------------------------------------------------

(defmethod distance-x/distance-y->x/y ((view scaled-view-mixin) view-x view-y)
  (values
   (round (* view-x (scale-x view)))
   (round (* view-y (scale-y view)))))

;;; ---------------------------------------------------------------------------
    
(defmethod x/y->view-x/view-y ((view scaled-view-mixin) x y)
  (values (+ (float (/ (- x (left-margin view))
                       (scale-x view))))
          (+ (float (/ (- y (top-margin view))
                       (scale-y view))))))

;;; ---------------------------------------------------------------------------

(defmethod x/y->distance-x/distance-y ((view scaled-view-mixin) x y)
  (values
   (/ x (scale-x view))
   (/ y (scale-y view))))

;;; ---------------------------------------------------------------------------

#+TEST
(defmethod view-x/view-y->x/y-scaled-view-mixin-original (view view-x view-y)
  (values
   (round (+ (left-margin view) (* (- view-x (view-left view)) 
                                   (scale-x view))))
   (round (+ (top-margin view) (* (- view-y (view-top view))
                                  (scale-y view))))))

#+TEST
(let ((view (active-map-view)))
  (ccl::meter-consing (view-x/view-y->x/y-scaled-view-mixin-original view 23.1d0 46.4d0))
  (timeit (:report t)
    (loop repeat 1000 do
          (view-x/view-y->x/y-scaled-view-mixin-original view 23.1d0 46.4d0)))
  (ccl::meter-consing (view-x/view-y->x/y view 23.1d0 46.4d0))
  (timeit (:report t)
    (loop repeat 1000 do
          (view-x/view-y->x/y view 23.1d0 46.4d0))))
|#

;;; ---------------------------------------------------------------------------
;;; make views compatiable with draw-geom stuff
;;; ---------------------------------------------------------------------------

(defmethod view-x/view-y->x/y ((view t) view-x view-y)
    (values (round view-x) (round view-y)))

;;; ---------------------------------------------------------------------------

(defmethod view-scale ((view t))
  (values 1))

;;; ---------------------------------------------------------------------------

(defgeneric adjust-point-for-scaling (view h &optional v)
  (:documentation "Adjusts the point <h, v> to account for any scaling in
the view. Returns an implementation dependent point."))

;;; ---------------------------------------------------------------------------

(defgeneric view-x/view-y->point (view view-x view-y)
  (:documentation "Adjusts the point <view-x, view-y> to account for any scaling in
the view. Returns an implementation dependent point."))


;;; ---------------------------------------------------------------------------
;;; generically useful things? I have _no_ idea
;;; ---------------------------------------------------------------------------

(defgeneric note-view-settings-changed (view)
  (:documentation "Does whatever makes sense to bring a view up-to-date 
when its settings have changed (e.g., it's scale or margins).")
  (:method ((view t))
           (values)))

;;; ---------------------------------------------------------------------------

(defgeneric redraw (viewer-or-window)
  (:documentation "Cause the viewer-or-window to redraw."))


;;; ---------------------------------------------------------------------------
;;; relative-view-position
;;; ---------------------------------------------------------------------------

#+Ignore
(defgeneric relative-view-position (view-from view-to from-position)
  (:documentation "Returns the position of FROM-POSITION in VIEW-FROM in terms
of the scale of VIEW-TO.")
  
  #+MCL
  (:method ((view-1 t) (view-2 t) from-position)
           (bind ((h-pos (float (point-h from-position)))
                  (v-pos (float (point-v from-position)))
                  (v1-size (view-size view-1))
                  (v1-size-h (float (point-h v1-size)))
                  (v1-size-v (float (point-v v1-size)))
                  (v2-size (view-size view-2))
                  (v2-size-h (float (point-h v2-size)))
                  (v2-size-v (float (point-v v2-size))))
             (declare (dynamic-extent h-pos v-pos 
                                      v1-size v1-size-h v1-size-v
                                      v2-size v2-size-h v2-size-v)
                      (double-float h-pos v-pos v1-size-h v1-size-v
                                    v2-size-h v2-size-v))
             (point (floor (* (/ h-pos v1-size-h) v2-size-h))
                    (floor (* (/ v-pos v1-size-v) v2-size-v))))))

;;; ---------------------------------------------------------------------------
;;; dialog-item-value
;;; ---------------------------------------------------------------------------

(defgeneric dialog-item-value (item) 
  (:method ((item t))
           (error "don't know how to find the value of ~A" item)))

;;; ---------------------------------------------------------------------------

(defgeneric (setf dialog-item-value) (value item) 
  (:method (value (item t))
    (declare (ignore value))
    (error "don't know how to find the value of ~A" item)))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************