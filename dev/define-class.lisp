(in-package metatilities)

;;; ---------------------------------------------------------------------------
;;; some class defining functions
;;; ---------------------------------------------------------------------------

(defvar *define-class-form* 'defclass*)

;;; ---------------------------------------------------------------------------

(defun simple-define-class (superclasses 
                            &optional (name (simple-define-class-name superclasses)))
  "Define a class on the fly..."
  (cond ((and (length-1-list-p superclasses)
               (find-class (first superclasses) nil))
         (values (first superclasses)))
        (t
         (let (#+MCL (ccl::*warn-if-redefine* nil)
               #+MCL (ccl::*record-source-file* nil))
           (eval `(progn
                    (when (find-class ',name nil)
                      (setf (find-class ',name) nil))
                    (defclass* ,name ,(ensure-list superclasses) nil))))
         (values name))))

;;; ---------------------------------------------------------------------------

(defun simple-define-class-name (superclasses &optional (package *package*)) 
  (intern (format nil "~{~a~^-AND-~}" superclasses) package))

;;; ---------------------------------------------------------------------------

(defun define-class (class-name superclasses slots &rest class-options)
  "Define a class with all the bells and whistles on the fly... See 
simple-define-class for the simpler version."
  (let (#+MCL (ccl::*warn-if-redefine* nil)
        #+MCL (ccl::*record-source-file* nil))
    (eval `(,*define-class-form* 
            ,(or class-name 
                 (setf class-name
                       (simple-define-class-name (ensure-list superclasses))))
             ,(ensure-list superclasses) 
             (,@(ensure-list slots))
             ,@class-options)))
  (values class-name))

;;; ---------------------------------------------------------------------------

(defun find-existing-subclass (superclass superclasses)
  "Look through all the sub-classes of superclass and see if any of them descend
from every class in superclasses."
  (mopu:map-subclasses
   superclass
   (lambda (subclass)
     (when (every (lambda (superclass)
                    (member superclass (mopu:superclasses subclass)
                            :key (lambda (x) (class-name x))))
                  superclasses)
       (return-from find-existing-subclass (class-name subclass)))))
  (values nil))

;;; ---------------------------------------------------------------------------

(defun find-or-create-class (root classes)
  (or (find-existing-subclass root classes)
      (let ((superclasses (remove-redundant-classes classes)))
        (define-class (simple-define-class-name (remove-redundant-classes superclasses))
          classes nil))))

;;; ---------------------------------------------------------------------------

(defun remove-redundant-classes (classes)
  (loop for class in classes 
        unless (class-redundant-p class classes) collect
        class))

;;; ---------------------------------------------------------------------------

(defun class-redundant-p (class classes)
  (some
   (lambda (other-class)
     (and (not (eq class other-class))
          (subtypep other-class class)))
   classes))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************