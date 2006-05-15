(in-package #:metatilities)

(defgeneric include-class-dependencies (class-type
                                        dynamic-class class-list &rest parameters)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric existing-subclass (class-type class-list)
  (:documentation ""))



;;; ---------------------------------------------------------------------------
;;; Support for dynamic classes based on the parameters for instantiation...
;;;
;;; Here is a quick history lesson: we've been doing this for shapes, since
;;; there was a massive amount of potential shape superclasses, and only a
;;; small subset were ever used for any given instance, and this was the 
;;; cleanest and cutest approach...
;;; ---------------------------------------------------------------------------

(defvar *parameter-dynamic-class-table* nil)

;;; ---------------------------------------------------------------------------

(defun type->parameter-table (type)
  (cdr (assoc type *parameter-dynamic-class-table*)))

;;; ---------------------------------------------------------------------------

(defun (setf type->parameter-table) (value type)
  (let ((it (assoc type *parameter-dynamic-class-table*)))
    (if it
      (setf (cdr it) value)
      (setf *parameter-dynamic-class-table*
            (append *parameter-dynamic-class-table* (list (cons type value))))))
  (values value))

;;; ---------------------------------------------------------------------------

(defun parameter->dynamic-class (table parameter)
  (cdr (assoc parameter table)))

;;; ---------------------------------------------------------------------------

(defun (setf parameter->dynamic-class) (value table parameter)
  (let ((it (assoc parameter table)))
    (if it
      (setf (cdr it) value)
      (let ((temp (cdr table))
            (insert (list (cons parameter value))))
        (setf (cdr insert) temp
              (cdr table) insert))))
  (values value))

;;; ---------------------------------------------------------------------------

(defun table&parameter->dynamic-class (class-type parameter)
  (parameter->dynamic-class (type->parameter-table class-type) parameter))

;;; ---------------------------------------------------------------------------

(defun add-parameter->dynamic-class (class-type 
                                     parameter &rest super-classes)
  (let* ((current-table (or (type->parameter-table class-type) 
                            (list (cons :remove :remove))))
         (have-table? (not (eq (caar current-table) :remove))))
    (dolist (super-class (ensure-list super-classes))
      (let ((it (parameter->dynamic-class current-table parameter)))
        (if it
          (pushnew super-class it)
          (setf (parameter->dynamic-class current-table parameter)
                (list super-class)))))
    (unless have-table?
      (setf (type->parameter-table class-type) current-table)))
  
  (values nil))

;;; ---------------------------------------------------------------------------

(defun add-dynamic-class-for-parameters (class-type dynamic-class 
                                                    &rest parameters)
  (dolist (parameter (ensure-list parameters))
    (add-parameter->dynamic-class 
     class-type parameter dynamic-class)))

;;; ---------------------------------------------------------------------------

#+Later
(defun remove-parameter->dynamic-class (class-type parameter dynamic-class)
  (let ((primary-table (containers:item-at *parameter-dynamic-class-table* class-type)))
    (when (and primary-table (containers:item-at primary-table parameter))
      (setf (containers:item-at primary-table parameter)
            (remove dynamic-class (containers:item-at primary-table parameter))))))

;;; ---------------------------------------------------------------------------

(defun empty-add-parameter->dynamic-class (class-type)
  (setf (type->parameter-table class-type) nil))

;;; ---------------------------------------------------------------------------

(defun empty-all-add-parameter->dynamic-class ()
  (setf *parameter-dynamic-class-table* nil))

;;; ---------------------------------------------------------------------------

(defun dynamic-class-information ()
  (loop for (type . data) in *parameter-dynamic-class-table* collect
        (list type
              (loop for (parameter . class) in data collect
                    (list parameter class)))))

;;; ---------------------------------------------------------------------------

#+Ignore
(define-debugging-class determine-dynamic-class ()
  (:export-p t))

;;; ---------------------------------------------------------------------------

(defmethod include-class-dependencies ((class-type (eql nil)) 
                                       dynamic-class class-list &rest parameters)
  (declare (ignore dynamic-class class-list parameters)))

;;; ---------------------------------------------------------------------------

(defmethod existing-subclass ((class-type (eql nil)) (class-list t))
  (values nil))

;;; ---------------------------------------------------------------------------

(defun determine-dynamic-class (class-type dynamic-class &rest parameters)
  (let ((class-list
         (loop for parameter in parameters
               for keyword? = t then (not keyword?)
               when keyword? nconc
               (loop for class in (table&parameter->dynamic-class class-type parameter)
                     when (or (not dynamic-class)
                              (and dynamic-class
                                   (not (subtypep class dynamic-class))))
                     collect class))))
    (setf class-list
          (apply #'include-class-dependencies class-type dynamic-class class-list parameters))
    (when (and dynamic-class (not (some (lambda (class-name)
                                          (subtypep dynamic-class class-name))
                                        class-list)))
      (setf class-list (nconc (list dynamic-class) class-list)))
    
    (setf class-list (delete-duplicates class-list))
    #+Ignore
    (when-debugging-format determine-dynamic-class
                           "DDC: class list ~A" class-list)
    (let ((it nil))
      (cond ((setf it (existing-subclass class-type class-list))
             #+Ignore
             (when-debugging-format determine-dynamic-class
                                    "DSC: Found existing class: ~S" it)
             it)
            (t
             (if (and (length-1-list-p class-list)
                      (find-class (first class-list) nil))
               (first class-list)
               (define-class nil class-list nil)))))))

;;; ---------------------------------------------------------------------------

#+Later
(defun check-dynamic-class-consistency (class-type)
  #+Later
  ;;?? consistency checking
  (assert (every (lambda (class)
                   (find-class class t))
                 super-classes)
          nil
          "The followin classes do not exist: ~A"
          (list->formatted-string 
           (remove-if (lambda (class) (find-class class t)) super-classes)
           ", " "."))
  #+LATER
  (when copyable?
    ;;?? consistency checking
    (assert (every (lambda (class)
                     (subtypep class 'copyable-mixin))
                   super-classes)
            nil
            "Every class in ~A must be a sub-class of copyable-mixin. ~
             The following are not: ~A"
            (list->formatted-string super-classes ", " "")
            (list->formatted-string 
             (remove-if (lambda (class) (subtypep class 'copyable-mixin)) super-classes)
             ", " "."))))

#+Later
(defun build-dynamic-class-documentation (class-type)
  (setf (documentation (form-symbol class-type) 'type)
        (format nil "~%Applicable Initargs:~%"))
  (setf (documentation (form-symbol 'make- class-type) 'function)
        (format nil "~%Applicable Initargs:~%"))
  (setf (documentation (form-symbol 'new- class-type) 'function)
        (format nil "~%Applicable Initargs:~%"))
  ;;?? documentation, needs string-contains-p
  (unless (string-contains-p (documentation (form-symbol class-type) 'type)
                             (format nil "~A" parameter))
    (setf (documentation (form-symbol class-type) 'type) 
          (format nil "~A~%:~A" 
                  (documentation (form-symbol class-type) 'type) parameter))
    (setf (documentation (form-symbol 'make- class-type) 'function) 
          (format nil "~A~%:~A" 
                  (documentation (form-symbol 'make- class-type) 'function) parameter))
    (setf (documentation (form-symbol 'new- class-type) 'function) 
          (format nil "~A~%:~A" 
                  (documentation (form-symbol 'new- class-type) 'function) parameter))))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
