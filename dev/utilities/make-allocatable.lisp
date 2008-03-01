;;;; -*- Mode:Common-Lisp; Package:eksl-utilities; Fonts:(medfnt courierfb hl12i tr12 medfnb cptfonti hl12b); Base:10 -*-
;;;; -**- $Source: /cvs/cl-markdown/cl-markdown/metatilities/dev/utilities/make-allocatable.lisp,v $ -**-
;;;; *-* Last-edit: Friday, November 18, 2005 07:59:44; Edited-By: Gary *-* 
;;;; 
;;;; *-* Software: Macintosh Common Lisp, Version 4.3 *-*
;;;; *-* Lisp: Macintosh Common Lisp, Version 4.3 *-*

;;	Change History (most recent first):
;;  2 2/2/1    Westy Wrapped a 'without-interrupts' around the allocation function to make it thread safe.
;;  (do not edit before this line!!)


;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Memory Management Functions                      *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: Michael L. Greenberg, David L. Westbrook and Scott D. Anderson
;;;             Experimental Knowledge Systems Laboratory
;;;             Paul R. Cohen, Principal Investigator
;;;             David L. Westbrook, Systems Manager
;;;             David M. Hart, Laboratory Manager
;;;             Department of Computer Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;;
;;;  05-01-94 File Created by copying code from utils.  (anderson)
;;;  05-01-94 Removed use of w:without-interrupts for portability and because 
;;;           the use of memory management in MESS will not need to worry about 
;;;           interrupts.  (anderson)
;;;  05-01-94 Extended to allow CLOS instances to be allocatable. The only 
;;;           change was in the definition of make-fn.   (anderson)
;;;  05-01-94 Remodularized alloc-fn so creation is separate from initialization.
;;;           Put the alloc-fn* onto plist of structure name, so that `make' can 
;;;           get an instance given a type name and do its own initialization. 
;;;           (anderson)
;;;  05-01-94 Implemented allocate and free, functions that take the name of a
;;;           type.  This is part of the interface to `make.'  (anderson)
;;;  05-01-94 Modified allocation-status to calculate width of first column.
;;;           (anderson)
;;;  05-01-94 Went out on a limb and specialized the behavior of the CLOS generic
;;;           function `allocate-instance' to return an existing object if it
;;;           exists.  If this works, it means we can use make-instance and
;;;           initargs on types that are made allocatable.  (anderson)
;;;  06-14-94 That limb broke out from under us, because Lispworks doesn't use
;;;           `allocate-instance.'  If the code is to be portable, we'll have to
;;;           use a different method.  (anderson)
;;;  06-10-95 Introduced the `alloc-info' structure.  (anderson)
;;;  06-10-95 Extend `allocate' so that it takes initargs, which required changing
;;;           the allocate-foo function.  Classes now disallow init-forms, but that
;;;           sacrifices almost no power.  (anderson)
;;;  06-10-95 Modified `allocation-status' so that it can be done for just one 
;;;           type.  (anderson)
;;;  06-21-95 Renamed `free' to `deallocate' because of a name clash.
;;;           (anderson)
;;;  07-01-95 Added a `declare ignore' of `initargs' for structures in the
;;;           allocate function.  (anderson)
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; --*--

(in-package :eksl-utilities)

;;; --*--
;;; ***************************************************************************
;;; Objects that can be freed and allocated

(defvar *allocated-types* nil
  "A list of symbols, the names of types that have been made allocatable.
Defined only for `allocation-status.'")

(defstruct (alloc-info (:type list)
			 (:constructor make-alloc-info
				       (count-var activ-var free-len 
                                                  alloc-fn* alloc-fn free-fn)))
  ;; The slots are filled with symbols, which are evalled or funcalled
  count-var
  activ-var
  free-len
  alloc-fn
  alloc-fn*
  free-fn)

;;; ---------------------------------------------------------------------------

(defgeneric reallocate-instance (instance &key)
  (:documentation "Called when reusing an allocatable class instance. The 
default implementation acts like initialize-instance in that initforms are
re-evaluated. The simplest implementation just calls reinitialize-instance."))

;;; ---------------------------------------------------------------------------

(defmacro make-allocatable (type class?
                                 &key
			         internal-pointer
			         creation-form (initial-array-size 100)
			         init-forms
			         init-args
                                 (item-var (gensym "ITEM-"))
                                 (export? nil))
  "Defines functions to make explicit storage management for particular objects
easier.  The required arguments are the name of the type and a boolean, telling
whether it is a class or structure.  For example, suppose your type is a
structure is named `foo,' and it was defined by a simple use of `defstruct,' so
that the creation-form is `make-foo' \(see CLtL, 1st ed., pg.\ 311\).  Class
instances work the same way.  This macro defines the following four things:

[Function] ALLOCATE-FOO &rest init-args           
     called when you want a foo to use.  It either returns one you previously
freed or a newly-consed one.  `Init-args' are the formal parameters to the
allocate function.  They are defined by you, the user of `make-allocatable,' and
are only referenced by `init-forms' which are also defined by you.  The
`init-forms,' which is just a list of pieces of code, may also refer to the
allocated structure, which is bound to the symbol `item-var,' which is also
defined by you.  By default, both are nil, so `allocate-foo' just returns what
`make-foo' returns---an empty object.  Actually, when objects are allocated and
freed, their slots are not cleared, so the object may contain stuff left over
from previous uses.  However, if the `internal-pointer' feature is used, the
`internal-pointer' place will be set to nil.  The following illustrates most of
these features.

      \(defstruct node
         name
         neighbors\)

      \(make-allocatable node nil
        :item-var new-node
        :internal-pointer node-neighbors
        :init-args \(new-name\)
        :init-forms \(\(setf \(node-name new-node\) new-name\)\)\)

      \(defun allocate-a-bunch-of-nodes \(\)
         \(dotimes \(i 100\)
             \(allocate-node \(format nil \"node-~d\" i\)\)\)\)

[Function] FREE-FOO x
     called when you are done using `x,' which should be an object of type
`foo.'

[Macro] WITH-FOO \(foo &rest args\) &body forms
     A macro that executes `forms' with symbol `foo' bound to a temporary
structure of type `foo.' Like an `allocate-foo' followed by `forms' followed by
`free-foo.' `Args' are init-args for `allocate-foo.'

[Function] CLEAR-FOO
     called if you want to clear the memory-management data-structures.  This
can be dangerous; don't use it unless you know what you're doing.

The preceding not only documents the four things defined by `make-allocatable,'
but also, by example, documents the naming scheme.

There are two data structures by which the freed structures can be stored: a
vector or an `internal-pointer' list.  The vector data structure is obvious, but
internal pointer lists may be unfamiliar.  Think back to the days when you coded
in Pascal or C.  If you wanted a list of structures, you didn't use cons cells,
you used a dedicated slot, usually called `next,' or some such.  Well, when
you've freed a structure, chances are there will be a slot that the allocatable
data structures can use as the ``next'' slot.  You tell it what slot to use by
giving the setfable accessor name, such as `foo-bar' if our `foo' structure has
a `bar' slot and the default conc-name.  The internal-pointer list has zero
storage overhead per freed structure, while the vector needs one element per
freed structure, with some additional for expansion.  Therefore, the
internal-pointer list is superior and should be used whenever possible.  \(I
believe it would only be impossible if all slots have type constraints that are
incompatible with pointing to a structure of the same kind.\) Should you decide
to use the vector data structure, the `initial-array-size' argument gives the
initial size of the vector.  The default is 100 elements.  The vector is grown
if it overflows during freeing a structure; obviously, this may take some time.

Finally, there is the function `utils:allocation-status,' which tells about the
memory management for each kind of structure that has been made allocatable.

This memory management also works for CLOS instances.  The difference is that
ALLOCATE-FOO function takes keyword arguments like `make-instance' and you may
not specify `init-forms,' since you should define an :after initialize-instance
method instead.  Therefore, this restriction gives up almost nothing."
  (let* ((type-name (string-upcase (string type)))
	 (var       (form-symbol "*FREE-"    type-name "*"))
	 (count-var (form-symbol "*"         type-name "S-ALLOCATED*"))
	 (activ-var (form-symbol "*TOTAL-"   type-name "-ACTIVITY*"))
	 (free-len  (form-symbol "*N-FREE-"  type-name "*"))
	 (alloc-fn* (form-symbol "ALLOCATE-" type-name "*"))
	 (alloc-fn  (form-symbol "ALLOCATE-" type-name))
	 (free-fn   (form-symbol "FREE-"     type-name))
	 (clear-fn  (form-symbol "CLEAR-"    type-name))
         (with-fn   (form-symbol "WITH-" type-name))
         (withs-fn  (form-symbol "WITH-" type-name "S"))
	 )
    (when (and class? (or init-forms init-args))
      (cerror "ignore them" "For classes, init-forms and init-args shouldn't be used.")
      (setf init-forms nil init-args nil))
    `(progn
       (pushnew ',type *allocated-types*)
       (setf (get ',type :alloc-info)
	     (make-alloc-info  ',count-var ',activ-var ',free-len 
                               ',alloc-fn ',alloc-fn* ',free-fn))
       (defvar ,var ,(if internal-pointer
                       nil
                       `(make-array ,initial-array-size 
                                    :fill-pointer 0
                                    :adjustable T)))
       (defvar ,count-var 0)
       (defvar ,activ-var 0)
       (defvar ,free-len  0)
       (defun ,free-fn (arg)
	 ,(format nil "Stores an object of type ~s for later re-use.  See ~s" 
                  type alloc-fn)
	 (declare (optimize speed (safety 0)))
	 (decf ,count-var)
	 (incf ,free-len)
	 ,@(if internal-pointer
             `((progn (setf (,internal-pointer arg) ,var)
                      (setf ,var arg)))
             `((vector-push-extend arg ,var)))
	 nil)
       ;; Allocates an object, but does no initialization.  Typically should not
       ;; be called by user.
       (proclaim '(inline ,alloc-fn*))
       (defun ,alloc-fn* (&rest initargs)
	 ,(format nil "Allocates an empty object of type ~s.  Use ~s instead." type alloc-fn)
	 (declare (optimize speed (safety 0))
                  ,@(when (not class?) '((ignore initargs))))
	 (incf ,count-var)
	 (incf ,activ-var)
	 (if (= ,free-len 0)
           ,(or creation-form
                (if class?
                  `(apply #'make-instance ',type initargs)
                  `(,(form-symbol "MAKE-" type-name))))
           ,(if internal-pointer
              `(let ((obj ,var))
                 (decf ,free-len)
                 (setf ,var (,internal-pointer ,var))
                 ;; The following is slightly inefficient if the user is 
                 ;; about to setf the same slot, but prevents the user from 
                 ;; accidentally using or destroying the stack of freed types.
                 (setf (,internal-pointer obj) nil)
                 
                 ,@(when class? '((apply #'reallocate-instance obj initargs)))
                 obj)
              `(progn
                 (decf ,free-len)
                 (let ((obj (vector-pop ,var)))
                   ,@(when class? '((apply #'reallocate-instance obj initargs)))
                   obj)))))
       ;; User interface function
       ,(if class?
          `(defun ,alloc-fn (&rest init-args)
             ,(format nil "Allocates an object of type ~s; like `make-instance' but allows storage management." 
                      type)
             (declare (optimize speed (safety 0)))
             (without-interrupts (apply #',alloc-fn* init-args)))
          `(defun ,alloc-fn (,@init-args)
             ,(format nil "Allocates a structure of type ~s and initializes some slots." 
                      type)
             (declare (optimize speed (safety 0)))
             (let ((,item-var (without-interrupts (,alloc-fn*))))
               ,@init-forms
               ,item-var)))
       
       (defun ,clear-fn ()
	 ,(format nil "Resets the allocation data types of instances of ~s" type)
	 ;; I could add some error checking to the `free-fn.' However, we want
	 ;; things blazingly fast, so we will let the users hang themselves.
	 ;; However we at least give them some clue about the nature of their
	 ;; death.
	 (when (plusp ,count-var)
	   (warn "There ~[~;is~:;are~] ~:*~d allocated ~a type~2:*~p in use. If ~:*~[~;it is~:;they are~]
freed (using ~*~a) you will probably get an error - not now, but someday and then forever."
                 ,count-var
                 ',type-name
                 ',free-fn))
	 (setf ,var ,(if internal-pointer
                       nil
                       `(make-array ,initial-array-size 
                                    :fill-pointer 0
                                    :adjustable t))
	       ,count-var  0
	       ,activ-var 0
	       ,free-len 0
	       ))
       (defmacro ,with-fn ((,type &rest args) &body body)
	 ,(format nil "Temporarily allocates an instance of ~s and frees it when body exits." type)
	 `(let ((,,type (,',alloc-fn ,@args)))
	    (unwind-protect
              (progn ,@body)
	      (when ,,type
                (,',free-fn ,,type)))))
       (defmacro ,withs-fn ((&rest things) &body body)
         ,(format nil "Like ~A only you can do a bunch at once..." with-fn)
         (cond ((null things) `(progn ,@body))
               (t `(,',with-fn ,(ensure-list (first things))
                     (,',withs-fn ,(rest things) ,@body)))))
       ,@(when export?
           `((export '(,with-fn ,withs-fn))))
       
       ',type)))

;;; ---------------------------------------------------------------------------

(defun allocate (type-name &rest initargs)
  "Returns an instance of `type-name.' The instance might be newly consed and
instantiated, or it might be a previously freed instance.  See
`make-allocatable.' Because `make-instance' might not be called, users should be
careful about initialization."
  (let ((info (get type-name :alloc-info)))
    (if (null info)
	;; We might signal an error, but why not just make an instance.
	(apply #'make-instance type-name initargs)
	(apply (symbol-function (alloc-info-alloc-fn info)) initargs))))

;;; ---------------------------------------------------------------------------

(defun deallocate (instance)
  "Deallocates an instance.  The instance will go into a stack of instances of the
same type, to be returned by a future call to `allocate.' See
`make-allocatable.'  You should not keep any pointers to this object."
  (let ((info (get (type-of instance) :alloc-info)))
    (if (null info)
	;; We might signal an error, but why not just do nothing?
	(progn nil)
	(funcall (symbol-function (alloc-info-free-fn info)) instance))))

;;; ---------------------------------------------------------------------------

(defmethod reallocate-instance ((object standard-object) &rest args &key &allow-other-keys)
  (apply #'reinitialize-instance object args))

;;; ---------------------------------------------------------------------------

(defun allocation-status (&optional (types *allocated-types*) (stream *standard-output*))
  "For each type of object that has been made allocatable, this prints the number
of instances currently in use (allocated but not freed), the number free, and
the total number that have been allocated."
  (assert (or (symbolp types) (and (listp types) (every #'symbolp types))) ()
	  "Must be a symbol or list of symbols, the names of types:  ~s" types)
  (cond ((symbolp types)
	 (let ((info (get types :alloc-info)))
	   (format stream
		   "~&Objects of type ~s:~%In Use:~11t~10d~%Free:~11t~10d~%Activity:~11t~10d~%"
		   types
		   (symbol-value (alloc-info-count-var info))
		   (symbol-value (alloc-info-free-len info))
		   (symbol-value (alloc-info-activ-var info)))))
	((and (listp types) (every #'symbolp types))
	 (let* ((head "Type")
		(maxl (max (length head)
			   (reduce #'max *allocated-types*
				   :key #'(lambda (sym)
					    ;; conses, but who cares
					    (length (format nil "~s" sym)))))))
	   (format stream "~&~va ~10@a ~10@a ~10@a~%~%" maxl head "In Use" "Free" "Activity")
	   (dolist (type types)
	     (let ((info (get type  :alloc-info)))
	       ;; Silently skip any symbols that aren't allocatable
	       (when info
		 (format stream "~&~vs ~10d ~10d ~10d"
			 maxl
			 type
			 (symbol-value (alloc-info-count-var info))
			 (symbol-value (alloc-info-free-len info))
			 (symbol-value (alloc-info-activ-var info))))))))
	(t (error "Must be a symbol or list of symbols, the names of types:  ~s"
		  types)))
  (values))

;;; ---------------------------------------------------------------------------

(defmacro with-object ((thing &rest initargs &key class &allow-other-keys) &body body)
  "A way to use with-<foo> when you don't know <foo> until run-time. See
make-allocatable for more details. Note that there is no penalty for constant
classes (though why you would do this I don't know!)."
  (remf initargs :class)
  (if (constantp class)
    (let ((with-foo (form-symbol "WITH-" (remove-leading-quote class))))
      `(,with-foo (,thing ,@initargs) ,@body))
    `(let ((,thing (funcall (alloc-info-alloc-fn (get ,class :alloc-info))
                            ,@(when initargs
                                initargs))))
       (declare (dynamic-extent ,thing))
       (unwind-protect
         (progn ,@body)
         (when ,thing (funcall (alloc-info-free-fn (get ,class :alloc-info)) ,thing))))))

;;; ---------------------------------------------------------------------------

(defmacro with-objects ((&rest things) &body body)
  "Like with-object only you can do a bunch at once..."
  (cond ((null things) `(progn ,@body))
        (t `(with-object ,(first things)
              (with-objects ,(rest things) ,@body)))))

;;; ============================================================================
;;; Testing

;;; Structures

#+test
(progn
  (defstruct test-struct-ip foo (bar 1))
  
  (make-allocatable test-struct-ip nil :internal-pointer test-struct-ip-foo
		    :item-var obj
		    :init-args (foo-val)
		    :init-forms ((setf (test-struct-ip-foo obj) foo-val)))
   
  (defun test-allocation-structures-ip ()
    (format t "~&The EQ should be T; final activity = 6, final free = 4.~%")
    (clear-test-struct-ip)
    (allocation-status 'test-struct-ip)
    (let ((i (allocate-test-struct-ip 'foo3)))
      (free-test-struct-ip i)
      (with-test-struct-ip (i2 'foo4)
	(spy (eq i i2))))
    (let ((l nil))
      (dotimes (i 4)
        (push (allocate 'test-struct-ip 'foo5) l))
      (allocation-status 'test-struct-ip)
      (mapc #'free-test-struct-ip l))
    (allocation-status 'test-struct-ip))

  (defstruct test-struct-array foo (bar 2))

  (make-allocatable test-struct-array nil :initial-array-size 3
		    :item-var obj
		    :init-args (foo-val)
		    :init-forms ((setf (test-struct-ip-foo obj) foo-val)))
  
  (defun test-allocation-structures-array ()
    (format t "~&The EQ should be T; final activity = 6, final free = 4.~%")
    (clear-test-struct-array)
    (allocation-status 'test-struct-array)
    (let ((i (allocate-test-struct-array 'foo6)))
      (free-test-struct-array i)
      (with-test-struct-array (i2 'foo7)
	(spy (eq i i2))))
    (let ((l nil))
      (dotimes (i 4)
        (push (allocate 'test-struct-array 'foo8) l))
      (allocation-status 'test-struct-array)
      ;; 4 allocations will force the array to grow
      (mapc #'free-test-struct-array l))
    (allocation-status 'test-struct-array)))

;;; Class instances

#+test
(progn 
  (defclass test-class-ip ()
    ((foo :accessor test-foo :initarg :foo)
     (bar :initform 1)))
  
  (make-allocatable test-class-ip t :internal-pointer test-foo)
  
  (defun test-allocation-classes-ip ()
    (format t "~&All EQ's should be T.  Final status 0 1 4~%")
    (clear-test-class-ip)
    (allocation-status 'test-class-ip)
    (let ((i (allocate-test-class-ip :foo 1)))
      (free-test-class-ip i)
      (with-test-class-ip (i2 :foo 2)
	(spy (eq i i2))))
    (let ((i (allocate 'test-class-ip :foo 3)))
      (deallocate i)
      (let ((i2 (allocate 'test-class-ip :foo 4)))
	(spy (eq i i2))
	(deallocate i2)))
    ;; The following tests whether allocation is integrated with make-instance
    #+obsolete
    (let ((i (make-instance 'test-class-ip)))
      (describe i)
      (deallocate i)
      (let ((i2 (make-instance 'test-class-ip)))
	(describe i2)
	(spy (eq i i2))
	(deallocate i2)))
    (allocation-status 'test-class-ip))

  (defclass test-class-array ()
    ((foo :initarg :foo)
     (bar :initform 1)))
  
  (make-allocatable test-class-array t :initial-array-size 3)
  
  (defun test-allocation-classes-array ()
    (format t "~&All EQ's should be T.  Final status 0 1 4~%")
    (clear-test-class-array)
    (allocation-status 'test-class-array)
    (let ((i (allocate-test-class-array :foo 1)))
      (free-test-class-array i)
      (with-test-class-array (i2 :foo 2)
	(spy (eq i i2))))
    (let ((i (allocate 'test-class-array :foo 3)))
        (deallocate i)
      (let ((i2 (allocate 'test-class-array :foo 4)))
	(spy (eq i i2))
	(deallocate i2)))
    ;; The following tests whether allocation is integrated with make-instance
    #+obsolete
    (let ((i (make-instance 'test-class-array :foo 3)))
      (describe i)
      (deallocate i)
      (let ((i2 (make-instance 'test-class-array)))
	(describe i2)
	(spy (eq i i2))
	(deallocate i2)))
    (allocation-status 'test-class-array)))

;;; ***************************************************************************
;;; EOF
