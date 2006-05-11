(in-package #:metatilities)

;;; NO DEADLOCK PROTECTION !!

;;; ---------------------------------------------------------------------------
;;; exported API

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(with-eksl-lock with-read-access with-write-access
	    access-lock-p +write-lock+ +read-lock+ 
	    grant-lock)))

#+Ignore
(define-debugging-class eksl-locks ()
  (:export-p t))

;;; ---------------------------------------------------------------------------
;;; locks

(defclass eksl-lock ()
  ((name  :initarg :name :accessor lock-name)
   (owner :initform nil :accessor lock-owner)
   (type  :initform nil :accessor lock-type)
   (queue :initform nil :accessor lock-queue)))

;;; ---------------------------------------------------------------------------

(defmethod reallocate-instance :before ((object eksl-lock) &key)
  (setf (lock-owner object) nil
        (lock-type object) nil
        (lock-queue object) nil))

;;; ---------------------------------------------------------------------------

#+Later
(make-allocatable eksl-lock t)

;;; ---------------------------------------------------------------------------

(defmethod print-object ((lock eksl-lock) stream)
  (format stream "#<LOCK (~A,~A,~S,~D)>"
          (lock-name lock) (lock-type lock) 
          (lock-owner lock) (length (lock-queue lock))))

;;; ---------------------------------------------------------------------------

(defvar *lockbase* (make-hash-table))
(defvar *error-on-reentrant-request* nil)
(defconstant +write-lock+ :write)
(defconstant +read-lock+  :read)

;;; ---------------------------------------------------------------------------

(defun look-locks ()
  (maphash #'(lambda (k e)
               (format t "~A: ~A~%" k e))
           *lockbase*))

;;; ---------------------------------------------------------------------------
;;; internal - not user functions !
;;; ---------------------------------------------------------------------------

(defun find-lock (lock-name)
  (or (gethash lock-name *lockbase*)
      (setf (gethash lock-name *lockbase*)
            (allocate-eksl-lock :name lock-name))))

(defun grant-lock (lock owner type)
  #+Ignore
  (when-debugging-format eksl-locks
                         "LOCKS: ~A[~A] granted to ~A" lock type owner)
  (push owner (lock-owner lock))
  (setf (lock-type lock) type)
  (values :granted))

(defun service-pending-reads (lock)
  (loop while (and (car (lock-queue lock))
		   (eql (cdar (lock-queue lock)) +read-lock+)) do
	(let ((next (pop (lock-queue lock))))
	  (grant-lock lock (car next) (cdr next)))))

(defun enqueue-lock-request (lock owner type)
  #+Ignore
  (when-debugging-format eksl-locks
                         "LOCKS: Queueing request ~A[~A] for ~A" lock type owner)
  (setf (lock-queue lock)
        (nconc (lock-queue lock) (list (cons owner type))))
  
  (thread-wait :waiting (lambda () (access-lock-p owner lock)))
  (values :granted))

;;; ---------------------------------------------------------------------------
;;; somewhat external, but not recommended as user functions

(defun release-eksl-lock (lock owner)
  #+Ignore
  (when-debugging-format eksl-locks
                         "Lock ~A released by ~A.~%" lock owner)
  (setf (lock-owner lock) (delete owner (lock-owner lock) :test 'equal))
  (when (null (lock-owner lock))
    (let ((next (pop (lock-queue lock))))
      (when next
        (grant-lock lock (car next) (cdr next))
        (when (eql (cdr next) +read-lock+)
          (service-pending-reads lock))))))

;;; ---------------------------------------------------------------------------

(defun request-eksl-lock (caller lock type block? allow-reentry?)
  (without-interrupts
    (cond ((not (lock-owner lock))
           (grant-lock lock caller type))
          ((or (member caller (lock-owner lock))
               (member caller (lock-queue lock) :key 'car))
           (if allow-reentry?
             (if *error-on-reentrant-request*
               (error "Reentrant lock request for ~A by caller ~A.~%"
                      (lock-name lock) caller)
               (return-from request-eksl-lock :exists))
             :denied))
          ((and (eq (lock-type lock) +read-lock+)
                (eq type +read-lock+)
                (null (lock-queue lock)))
           (grant-lock lock caller type))
          (block?
             (enqueue-lock-request lock caller type))
          (t
           :denied))))

;;; ---------------------------------------------------------------------------
;;; functions for flushing the lockbase

(defgeneric destroy-lock (lock)
  (:documentation "")
  (:method ((lock-name t))
           (destroy-lock (find-lock lock-name)))
  (:method ((lock eksl-lock))
           (remhash (lock-name lock) *lockbase*)
           (deallocate lock))
  (:method ((lock (eql nil)))
           (error "lock not found")))

(defun safely-destroy-lock (lock-name)
  (declare (ignore lock-name))
  (error "unimplemented function."))

(defun flush-locks ()
  (maphash #'(lambda (k e)
               (declare (ignore e))
               (destroy-lock k)) *lockbase*))

(defun flush-locks-and-take-names ()
  "ASSUMES THAT THE OWNER FIELDS ARE THREADS. IF NOT, THEY WILL BE IGNORED."
  (maphash #'(lambda (k e)
               (mapc #'(lambda (p)
                         (ignore-errors (destroy-thread p)))
                     (lock-owner e))
               (mapc #'(lambda (pp)
                         (ignore-errors (destroy-thread (car pp))))
                     (lock-queue e))
               (destroy-lock k)) *lockbase*))

;;; ---------------------------------------------------------------------------
;;; suggested functions/macros for user code

(defun access-lock-p (caller lock)
  "Does the caller have access to the named lock ?"
  #+Remove
  (when (null (lock-owner lock)) ;;; this is a hack - I dunno how this happens
    (release-eksl-lock lock nil))
  (member caller (lock-owner lock)))

;;; ---------------------------------------------------------------------------

#+Old
(defmacro with-eksl-lock (type (lock-name &key (block? t) (allow-reentry? t))
                               &body body)
  "Execute body within the context of a lock. If 'block?' is true \(the default\),
then the body will wait for the lock to clear before executing. Otherwise, the 
body will simply be skipped. If 'allow-reentry? is nil \(the default\), then the
body will not be re-entered even if the same owner asks for the lock a second time."
  (let ((lockv (gensym "lock"))
        (gotitv (gensym "lock")))
    `(let ((,lockv (find-lock ,lock-name))
           (,gotitv nil))
       (unwind-protect
         (progn
           (when (request-eksl-lock (current-thread) ,lockv ,type ,block? ,allow-reentry?)
             (tf ,gotitv)
             ,@body))
         (when ,gotitv
           (release-eksl-lock ,lockv (current-thread)))))))

(defmacro with-eksl-lock (type (lock-name &key (block? t) (allow-reentry? t))
                               &body body)
  "Execute body within the context of a lock. If 'block?' is true \(the default\),
then the body will wait for the lock to clear before executing. Otherwise, the 
body will simply be skipped. If 'allow-reentry? is nil \(the default\), then the
body will not be re-entered even if the same owner asks for the lock a second time."
  (with-gensyms (lock release?)
    `(let ((,lock (find-lock ,lock-name))
           (,release? nil))
       (unwind-protect
         (progn
           (setf ,release?
                 (request-eksl-lock (current-thread) ,lock ,type ,block? ,allow-reentry?))
           (case ,release?
             ((:granted :exists) ,@body)))
         (when (eq ,release? :granted)
           (release-eksl-lock ,lock (current-thread)))))))

;;; ---------------------------------------------------------------------------

(defmacro with-read-access ((lock-name &key (block? t) (allow-reentry? t))
                            &body body)
  `(with-eksl-lock +read-lock+ (,lock-name 
                                :block? ,block? 
                                :allow-reentry? ,allow-reentry?) ,@body))

;;; ---------------------------------------------------------------------------

(defmacro with-write-access ((lock-name &key (block? t) (allow-reentry? t))
                             &body body)
  `(with-eksl-lock +write-lock+ (,lock-name 
                                 :block? ,block?
                                 :allow-reentry? ,allow-reentry?) ,@body))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
