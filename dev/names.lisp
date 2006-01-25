;;; ---------------------------------------------------------------------------
;;; numbered-instances-mixin
;;;
;;; a sort of light-weight named-object-mixin
;;; ---------------------------------------------------------------------------

(defclass* numbered-instances-mixin (copyable-mixin)
  ((object-number :unbound i))
  (:copy-set-slots (object-number (get-next-instance-number (class-name (class-of self)))))
  (:export-slots object-number))

;;; ---------------------------------------------------------------------------

(defmethod object-number ((object numbered-instances-mixin))
  (set-object-number-if-necessary object))

;;; ---------------------------------------------------------------------------

(defun set-object-number-if-necessary (object)
  "Sets a numbered-instances-mixin's object number if it hasn't already been
set. Returns the object number."
  (if (slot-boundp object 'object-number)
    (slot-value object 'object-number)
    (setf (slot-value object 'object-number)
          (get-next-instance-number object))))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object numbered-instances-mixin) &key)
  (set-object-number-if-necessary object))

;;; ---------------------------------------------------------------------------

(defmethod update-instance-for-different-class :after ((previous numbered-instances-mixin) (target numbered-instances-mixin) &key)
  (setf (slot-value target 'object-number)
        (get-next-instance-number (class-name (class-of target)))))

;;; ---------------------------------------------------------------------------

(defgeneric get-next-instance-number (thing)
  (:documentation "")
  (:method ((class-name symbol))
           (prog1
             (get class-name 'object-number 0)
             (setf (get class-name 'object-number)
                   (1+ (get class-name 'object-number 0)))))
  (:method ((object standard-object))
           (get-next-instance-number (class-name-of object))))

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object numbered-instances-mixin) stream)
  (let ((number (object-number object)))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~S" number))))

;;; ---------------------------------------------------------------------------

(defun reset-symbol-numbering ()
  (loop for name in (mapcar #'class-name
                            (subclasses* (find-class 'numbered-instances-mixin))) do
        (reset-symbol-numbering-for-class name)))

;;; ---------------------------------------------------------------------------

(defun reset-symbol-numbering-for-class (class-name)
  (setf (get class-name 'object-number) 0))

;;; ---------------------------------------------------------------------------

(defun numbered-symbols-count ()
  (loop for name in (mapcar #'class-name
                            (subclasses* (find-class 'numbered-instances-mixin))) sum
        (get name 'object-number 0)))

;;; ---------------------------------------------------------------------------

(defun remove-numbered-symbols (&key (verbose? t))
  (let ((grand-total 0))
    (loop for name in (sort 
                       (mapcar #'class-name
                               (subclasses* (find-class 'numbered-instances-mixin)))
                       #'string-lessp) do
          (let ((i 0)
                (total (get name 'object-number 0)))
            (loop while (< i total) do 
                  (unintern (find-symbol (format nil "~A-~D" name i)))
                  (incf i))
            (when (and (plusp i) verbose?)
              (format t "~&~40A: ~A" name i))
            (incf grand-total total)))
    (format t "~&~&~40A: ~A" "Grand Total" grand-total))
  (reset-symbol-numbering))

;;; ---------------------------------------------------------------------------

(defun remove-numbered-symbols* (&key (verbose? t) (gap-size 10))
  (loop for name in (sort 
                     (mapcar #'class-name
                             (subclasses* (find-class 'numbered-instances-mixin)))
                     #'string-lessp) do
        ;; Extra is a bit of hack
        (let ((extra gap-size)
              (i 0))
          (loop while (or (find-symbol (format nil "~A-~D" name i))
                          (plusp extra)) do
                (unless (unintern (find-symbol (format nil "~A-~D" name i)))
                  (decf extra))
                (incf i))
          (when (and (plusp (- i (- gap-size extra)))
                     verbose?)
            (format t "~&~40A: ~A" name (- i (- gap-size extra)))))))


;;; ---------------------------------------------------------------------------
;;; object-with-name
;;;
;;; An object-with-name has a name slot which gets filled in automatically
;;; unless a name is passed in as an initarg.
;;; ---------------------------------------------------------------------------

(defclass* object-with-name (numbered-instances-mixin)
  ((name :type symbol ir))
  (:documentation "Allows each instance to have an name. One is generated
for it if not provided. The name is always a symbol.")
  :copy-slots)

;;; ---------------------------------------------------------------------------

(defmethod print-object ((object object-with-name) stream)
  (let ((name (and (slot-boundp object 'name) (slot-value object 'name))))
    (print-unreadable-object (object stream :type t :identity t)
      (format stream "~:[<unnamed>~;~s~]" name name))))
  
;;; ---------------------------------------------------------------------------

(defmethod make-name ((object object-with-name) &optional new-name)
  "Make a name for yourself if necessary. This version insures name is a symbol."
  (let ((class-name (class-name (class-of object))))
    (macrolet ((form-name-symbol (&rest strings)
                                 `(form-symbol-in-package *package* ,@strings)))
      (cond ((not new-name) (form-name-symbol
			     (string-upcase class-name)
			     "-"
			     (princ-to-string (object-number object))))
	    ((symbolp new-name) new-name)
	    ((stringp new-name) (form-name-symbol new-name))
	    (t (form-name-symbol (princ-to-string new-name)))))))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :around ((object object-with-name) &rest initargs &key name)
  (if name
    (apply #'call-next-method object :name (name->symbol name) initargs)
    (apply #'call-next-method object :name (make-name object name) initargs)))

;;; ---------------------------------------------------------------------------

(defmethod name->symbol ((name symbol))
  name)

;;; ---------------------------------------------------------------------------

(defmethod name->symbol ((name string))
  (form-symbol name))

;;; ---------------------------------------------------------------------------

(defmethod update-instance-for-different-class :after ((previous object-with-name) 
                                                       (target object-with-name) &key)
  ;;?? changing class always gives a new name...
  (setf (slot-value target 'name)
        (make-name target nil)))

;;; ---------------------------------------------------------------------------

(defmethod (setf name) (new-name (object object-with-name))
  (setf (slot-value object 'name)
        (make-name object new-name)))