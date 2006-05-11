(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; some class defining functions
;;; ---------------------------------------------------------------------------

(defvar *define-class-form* 'defclass*
  "The name of the form used to define a class. Usually, this will be bound to 'defclass* but when we are using GBBOpen, it will probably be bound to define-class or define-class*.")

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

#+Old
;; returns first match
(defun find-existing-subclass (superclass superclasses)
  "Look through all the sub-classes of superclass and see if any of them descend
from every class in superclasses."
  (mopu:map-subclasses
   superclass
   (lambda (subclass)
     (when (every (lambda (superclass)
                    (member superclass (mopu:superclasses subclass :proper? nil)
                            :key (lambda (x) (class-name x))))
                  superclasses)
       (return-from find-existing-subclass (class-name subclass)))))
  (values nil))

(defun find-existing-subclass (superclass superclasses)
  "Look through all the sub-classes of superclass and see if any of them descend
from every class in superclasses."
  (let ((results nil))
    (mopu:map-subclasses
     superclass
     (lambda (subclass)
       (let ((last-position -1))
         (when (every (lambda (superclass)
                        (let ((pos
                               (position 
                                superclass (mopu:superclasses subclass :proper? nil)
                                :key (lambda (x) (class-name x)))))
                          (prog1
                            (and pos (< last-position pos))
                            (setf last-position pos))))
                      superclasses)
           (push (class-name subclass) results)))))
    (values (first results))))

;;; ---------------------------------------------------------------------------

(defun find-or-create-class (root classes)
  "Try to find a class which is a subclass of root and all of the other `classes` as well. If no such class exists, then it will be created and returned."
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