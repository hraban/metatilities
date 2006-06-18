(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; class defs
;;; ---------------------------------------------------------------------------

(defclass* notification-manager ()
  ((notification-linkages :unbound r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object notification-manager) &key)
  (setf (slot-value object 'notification-linkages)
        (make-container 'associative-container
                        :initial-element-fn 'make-notification-container)))

;;; ---------------------------------------------------------------------------

(defclass* basic-notification ()
  ((filter-fn nil ir)
   (priority 0 ir)
   (who-cares nil ir)))

;;; ---------------------------------------------------------------------------

(defgeneric register (who-cares notification-type &key filter-fn priority)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric unregister (who-cares &rest notification-types)
  (:documentation "")) 

;;; ---------------------------------------------------------------------------

(defgeneric still-cares-about-notification-p (who-cares notification)
  (:documentation "")
  (:method (who-cares notification)
           (declare (ignore who-cares notification))
           (values t)))

;;; ---------------------------------------------------------------------------

(defgeneric post-notification (notification-type &rest args)
  (:documentation "Send all subscribers a notification. If notification-type
is a keyword, then the keyword is sent. If it is a symbol, then an instance
of this type is created using 'args'."))

;;; ---------------------------------------------------------------------------

(defgeneric notify (who-cares notification)
  (:documentation "")
  (:method ((who-cares t) (notification basic-notification))
           (values)))

;;; ---------------------------------------------------------------------------

(defmacro with-registration ((who-cares notification-type) &body body)
  (with-gensyms (who type)
    `(let ((,who ,who-cares)
           (,type ,notification-type))
       (unwind-protect
         (progn
           (register ,who ,type)
           ,@body)
         (unregister ,who ,type)))))


;;; ---------------------------------------------------------------------------
;;; implementation
;;; ---------------------------------------------------------------------------

(defun make-notification-container ()
  (make-container 'priority-queue-heap
                  :not-lessp-predicate #'<
                  :key #'priority))

;;; ---------------------------------------------------------------------------

(defvar *notification-manager*
  (make-instance 'notification-manager))

;;; ---------------------------------------------------------------------------

(defun reset-notification-manager ()
  (setf *notification-manager* (make-instance 'notification-manager)))

;;; ---------------------------------------------------------------------------

(defmethod register (who-cares notification-type &key filter-fn (priority 0))
  (assert (numberp priority))
  #+Ignore
  (when-debugging-format 
   notifications-verbose
   "NOTIFY: ~A registers for ~S~:[~;, priority ~:*~D~]" 
   who-cares notification-type priority)
  (insert-item
   (item-at (notification-linkages *notification-manager*)
            notification-type)
   (make-instance (if (keywordp notification-type) 
                    'basic-notification notification-type)
     :filter-fn filter-fn
     :priority priority
     :who-cares who-cares)))

;;; ---------------------------------------------------------------------------

(defmethod unregister (who-cares &rest notification-types)
  (iterate-elements 
   notification-types
   (lambda (notification-type)
     #+Ignore
     (when-debugging-format 
      notifications-verbose
      "NOTIFY: ~A UNregisters for ~S" who-cares notification-type)
     (setf (item-at (notification-linkages *notification-manager*)
                    notification-type)
           (delete-item-if
            (item-at (notification-linkages *notification-manager*)
                     notification-type)
            (lambda (notification)
              (eq (who-cares notification) who-cares))))))) 

;;; ---------------------------------------------------------------------------

(defmethod unregister-all (who-cares)
  #+Ignore
  (when-debugging-format 
   notifications-verbose
   "NOTIFY: ~A UNregisters for all notifications" who-cares)
  (iterate-key-value
   (notification-linkages *notification-manager*)
   (lambda (notification-type registrars)
     (iterate-container 
      registrars
      (lambda (notification)
        (when (samep (who-cares notification) who-cares)
          (unregister who-cares notification-type))))))) 

;;; ---------------------------------------------------------------------------

(defmethod post-notification (notification-type &rest args)
  (let ((registrars (item-at (notification-linkages *notification-manager*)
                             notification-type))
        (notification nil))
                 
    (iterate-container 
     registrars
     (lambda (registrar) 
       (bind ((filter-fn (filter-fn registrar))
              (who-cares (who-cares registrar)))
         (when (or (not filter-fn)
                   (and filter-fn (apply filter-fn args)))
           ;; lazy notification creation
           (unless notification
             (setf notification 
                   (if (keywordp notification-type)
                     notification-type
                     (apply #'make-instance notification-type args))))
           (when (still-cares-about-notification-p who-cares notification)
             #+Ignore
             (when-debugging-format 
              notifications
              "NOTIFY: tell ~A about ~A" who-cares notification-type)
             (notify who-cares notification))))))))

;;; ---------------------------------------------------------------------------

(defun notifications ()
  (sort (collect-key-value
         (notification-linkages *notification-manager*)
         :filter (lambda (k v)
                   (declare (ignore k v))
                   (values t))
         :transform (lambda (k v)
                      (list k v)))
        #'string-lessp
        :key #'first))

#| Silly example

(register (krill::current-world-state) 'dance-party
          :filter-fn (lambda (&key venue)
                       (not (null venue))))
(unregister (current-world-state) 'dance-party)

(defclass* dance-party (basic-notification)
  ((venue nil ia)))

(defmethod notify ((who-cares basic-world-state)
                   (notification dance-party))
  (format t "~%Oh boy, a dance party at ~A" (venue notification)))

(post-notification 'dance-party :venue "Tom's")
(post-notification 'dance-party)

;;; ---------------------------------------------------------------------------
;;; ---------------------------------------------------------------------------

(defclass* test-notifications-client () 
  ((data nil a)
   (name nil ir)))
(defclass* test-notifications-notification-1 (basic-notification) ())
(defclass* test-notifications-notification-2 (basic-notification) ())

(defparameter *test-notifications-data* nil)

(deftestsuite test-notifications () 
  ((tester-1 (make-instance 'test-notifications-client :name "A") r)
   (tester-2 (make-instance 'test-notifications-client :name "B") r))
  (:setup
   (nilf *test-notifications-data*))
  (:teardown
   (unregister-all tester-1)
   (unregister-all tester-2)))

(defmethod notify ((who-cares test-notifications-client)
                   (notification test-notifications-notification-1))
  (push :a (data who-cares))
  (push :a *test-notifications-data*))

;;; ---------------------------------------------------------------------------

(defmethod notify ((who-cares test-notifications-client)
                   (notification test-notifications-notification-1))
  (push (name who-cares) (data who-cares))
  (push (name who-cares) *test-notifications-data*))

(addtest (test-notifications)
  basic
  (register tester-1 'test-notifications-notification-1)
  (post-notification 'test-notifications-notification-1)
  (ensure (samep (data tester-1) '("A")))
  (ensure (samep *test-notifications-data* '("A"))))

(addtest (test-notifications)
  priorities-1
  (register tester-1 'test-notifications-notification-1 :priority 1)
  (register tester-2 'test-notifications-notification-1 :priority 2)
  (post-notification 'test-notifications-notification-1)
  (ensure (samep (data tester-1) '("A")))
  (ensure (samep (data tester-2) '("B")))
  (ensure (samep *test-notifications-data* '("B" "A"))))

(addtest (test-notifications)
  priorities-1
  (register tester-1 'test-notifications-notification-1 :priority 2)
  (register tester-2 'test-notifications-notification-1 :priority 1)
  (post-notification 'test-notifications-notification-1)
  (ensure (samep (data tester-1) '("A")))
  (ensure (samep (data tester-2) '("B")))
  (ensure (samep *test-notifications-data* '("A" "B"))))
                   
|#

