(in-package #:metatilities)

(defmacro make-obsolete (obsolete-definition new-definition)
  `(define-compiler-macro ,obsolete-definition (&whole form &rest args)
     (declare (ignore args))                          
     (fresh-line *error-output*)
     (write-string ,(format nil "; ~a is an obsolete function; use ~a instead.~%"
                            obsolete-definition new-definition)
                   *error-output*)
     (terpri *error-output*)
     (values form)))

;;; ============================================================================

(defmacro named-lambda (name arglist &body body)
  "Like `lambda,' except that the lambda has a name, which can be very useful if
the function is executing far from where it was created and if it didn't have a
name you'd have a hard time figuring out where it came from.  Returns the
function object."
  (check-type name symbol)
  `(flet ((,name ,arglist . ,body))
     #',name))

#+test
(defun test-named-lambda ()
  (let ((list-of-functions nil))
    (push (named-lambda created-by-test-named-lambda () (print "hi"))
	  list-of-functions)
    (mapcar #'funcall list-of-functions))
  ;; This is supposed to produce an error, to show that you don't get an error in an
  ;; anonymous lambda, but a named function!
  (mapcar (named-lambda not-enough-args () t) '(1 2 3)))

;;; ============================================================================
;;; Can't use define-modify-macro for these because `place' isn't first.  Darn.

(defmacro deletef (item place &rest delete-args)
  "Same as (setf list (apply #'delete item list delete-args))."
  (multiple-value-bind (tmpvar tmpvals storevar storeform refform)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list tmpvar tmpvals)
	    (,@storevar (delete ,item ,refform ,@delete-args)))
       ,storeform)))

#+test
(defun test-deletef ()
  (let ((l '(1 2 3 4 5 6 7 8 9 10))
	(i 3))
    (spy (deletef 3 l))
    (spy (deletef 8 (nthcdr (incf i) l)))
    (spy l i)))

(defmacro removef (item place &rest delete-args)
  "Same as (setf list (apply #'remove item list delete-args))."
  (multiple-value-bind (tmpvar tmpvals storevar storeform refform)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list tmpvar tmpvals)
	    (,@storevar (remove ,item ,refform ,@delete-args)))
       ,storeform)))

#+test
(defun test-removef ()
  (let ((l '(1 2 3 4 5 6 7 8 9 10))
	(i 3))
    (spy (removef 3 l))
    (spy (removef 8 (nthcdr (incf i) l)))
    (spy l i)))

;;;; --------------------------------------------------------------------------
;;;;   Functions used in the following macros
;;;; --------------------------------------------------------------------------

(defmacro doplist ((key value plist) &body body)
  "Iterate over each key/value pair in a plist.  Key and Value are
bound to successive pairs in Plist."
  (let ((atvar (gensym "ATVAR")))
    `(do* ((,atvar ,plist (cddr ,atvar))
	   (,key (car ,atvar) (car ,atvar))
	   (,value (second ,atvar) (second ,atvar)))
	  ((null ,atvar) nil)
       (declare (ignorable ,key ,value))
       ,@body)))

;;;----------------------------------------------------------------------------

(defmacro assert* (test-form &optional place error-message report-message &rest arguments)
  "A variation on the `assert' macro in which the restart handler is bound
during the execution of the test-form, so that uses of this macro can be nested.
As with `assert,' a setf-able `place' may be given, but unlike `assert,' only
one may be given.  As with `assert,' you can give an error message, but you can
also give a message to go in the restart handler.  That report message may also
use the arguments.  It will usually say something like ``supply a new value for
foo.'' The restart is named `continue,' and takes one argument, the value to
setf `place' to.  Like `assert,' NIL is returned if test-form is true."
  (let ((arg-name    (newsym "ARG-"))
	(stream-name (newsym "STREAM-")))
    `(loop (restart-case (return (unless ,test-form
				   (error ,error-message . ,arguments)))
	     (continue
	       (,arg-name)
	       :report
	       #+ignore
	       (lambda (,stream-name)
		 (format ,stream-name ,report-message . ,arguments))
	       ;; The following is stupid looking, but seems to be necessary,
	       ;; since the preceding doesn't work on the Explorer
	       (lambda (,stream-name)
		 (write-string (format nil ,report-message . ,arguments)
			       ,stream-name))
	       :interactive (lambda ()
			      (format *query-io* "Enter a form to eval:  ")
			      (list (eval (read))))
	       (setf ,place ,arg-name))))))

#+test
(defun test-assert* ()
  (let ((list-of-numbers '(a b)))
    (assert* (and (listp list-of-numbers)
		  (do ((rest list-of-numbers (cdr rest)))
		      ((null rest) t)
		    (assert* (numberp (car rest))
			     (car rest)
			     "Must be a number:  ~s"
			     "Supply a new element."
			     (car rest))))
	     list-of-numbers
	     "Must be a list of numbers:  ~s"
	     "Supply a whole new list"
	     list-of-numbers)
    list-of-numbers))



;;;----------------------------------------------------------------------------

(define-modify-macro maxf (value) max "Sets `place' to max of its old value and `value'")
(define-modify-macro minf (value) min "Sets `place' to min of its old value and `value'")
(define-modify-macro multf (delta) * "Sets `place' to (* delta place)")

#+test
(defun test-maxf ()
  (let ((x 2)
	(a #(1 2 3 4 5 6 7 8)))
    (maxf x 4)
    (spy x)
    (maxf (aref a (incf x)) 99)
    (print-array a)
    (spy (aref a x))))

;;;----------------------------------------------------------------------------

#+(and test Lispworks)
(defsetf nthcdr (n list) (value)
  `(setf (cdr (nthcdr (1- ,n) ,list)) ,value))

;;;----------------------------------------------------------------------------

(defmacro some* ((element list) &body body)
  "An iterative version of the function `some'.  Bind each element of list in
turn.  If the body returns non-nil, return the result.  This can be faster than
the regular function, especially when lexical variables are used in the
predicate."
  (let ((result (newsym 'result)))
    (once-only (list)
      `(when ,list
         (let (,result)
           (dolist (,element ,list)
             (when (setf ,result (progn ,@body))
               (return ,result))))))))

;;; ---------------------------------------------------------------------------

(defmacro handler-case-if (test-form protected-form &rest case-forms)
  (with-variables (fn-name)
    `(flet ((,fn-name () ,protected-form))
       (declare (dynamic-extent ,fn-name))
       (if ,test-form
         (handler-case
          (,fn-name)
           ,@case-forms)
         (,fn-name)))))


;;; ---------------------------------------------------------------------------

;;; ONCE-ONLY is a LispM thing that's needlessly baroque because LispMs apparently
;;; had trouble killing off extra lambda bindings.  This is not an issue
;;; with the lisps for stock hardware, so far as I know.
;;;
;;; REBINDING and WITH-UNIQUE-NAMES are a lot easier if you are trying to debug, since
;;; they use gensym* and let.  Also, REBINDING isn't sensitive to the &environment, and
;;; if you are weird enough to do so, it can rebind &rest, etc., which ONCE-ONLY
;;; can't.

;;; This stuff is based on a Usenet discussion I read.  Supposedly Howard Stearns
;;; had an implementation out there, but the link is dead (along with the company 
;;; he was at, I guess).  Anyway, the idea seems to be used by some of the 
;;; popular implementations for stock hardware already, if I understand properly.

;;; This is for people like me, who don't set *print-circle* to t.  MAKE-SYMBOL
;;; is more reasonable if you do.
(defun gensym* (thing)
  (gensym (format nil "~a/" thing)))

(defmacro rebinding ((&rest vars) &body body)
  
  "Similar to ONCE-ONLY.  Typical usage: 
   
   (defmacro square (x) 
     (rebinding (x) 
       `(* ,x ,x))) 
   
   The magic is that the X inside the backquote is actaully a gensym bound to the 
   value of the form the caller gave us."
  
  ;; XXX This code is completely opaque.
  (assert (every #'symbolp vars) () "Can't rebind an expression.")
  (let* ((syms (mapcar #'gensym* vars))
         (binds (mapcar #'(lambda (x y) `(,x ,y)) syms vars))
         (rebinds (mapcar #'(lambda (x y) ``(,,x ,,y)) vars syms)))
    `(let ,binds                ; Evaluate the actual forms and bind to the gensyms in syms.
       (with-unique-names ,vars ; Now make more gensyms and bind them to the variable names in vars.
         `(let (,,@rebinds)     ; Bind the values we computed to the new gensyms.
            ,,@body)))))

;;; ---------------------------------------------------------------------------

(defun funcallable-expression-p (exp)
  (and (consp exp) (member (car exp) '(quote function))))

(defun function-expression-p (exp)
  (and (consp exp) (eq (car exp) 'function)))

(defun extract-head-form (fexp)
  "Give back the thing that can appear as the head of a procedure
application."
  (if (symbolp fexp)
    fexp
    (cadr fexp)))  

;;; ---------------------------------------------------------------------------

;;; This is based on an idea from Drew McDermot (in a Usenet posting).  My 
;;; version is a bit more agressive than his.
;;;
;;; I'm not sure if this is a nifty hack or an abomination.  In general, it 
;;; looks harmless if not overused.  Certainly it's ok for using in the 
;;; listener.

(defmacro \\ (&body forms)
  
  "Similar to Haskell's \\ operator.  Typical usage:

     (\\\\ x y = * x y)

   is like 

     (lambda (x y) (* x y)).

You can also just use Lisp forms on the right of the =, in which case no extra
levels of parens are added."
  
  (loop for (a . d) = forms then d
      until (eq a '=)
      unless d do (error "\\\\: Empty function body.")
      collect a into args
      finally 
        (return `(lambda ,args ,(if (symbolp (car d)) d `(progn ,@d))))))

;;; ---------------------------------------------------------------------------

#|
(defun make-hcase-table (clauses &rest opts)
  (let ((hash (apply #'make-hash-table opts))
        (keys (mappend (compose #'ensure-list #'car) clauses)))
    (dolist (key keys)
      (setf (gethash key hash) key))
    hash))

;;; ---------------------------------------------------------------------------

(defmacro hcase (thing &body clauses)
  "Like CASE, but users a 'EQUAL hash table so you can use strings, vectors, etc. as keys."
  (let ((thing (ensure-list thing)))
    (destructuring-bind (thing &key (test 'equal) hash-function) thing
      (with-unique-names (hash)
        `(let ((,hash (load-time-value 
                       (make-hcase-table ',clauses :test ',test :hash-function ,hash-function) t)))
           (case (gethash ,thing ,hash)
             ,@clauses))))))
|#

;;; ---------------------------------------------------------------------------

(defmacro ensure-type (x type)
  (once-only (x type)
    `(if (typep ,x ,type)
       ,x
       (coerce ,x ,type))))

;;; ---------------------------------------------------------------------------

(defmacro with-slot-bindings (slots instance &body body)
  "makes this: (with-slot-bindings (((x x2) double-float) ((y y2) double-float)) location-2 body)
into this:
 (let ((x2 (ensure-type (slot-value location-2 'x) 'double-float))
       (y2 (ensure-type (slot-value location-2 'y) 'double-float)))
   body)

If the first item of the binding-spec [(x x2) above] is a symbol instead of  a cons, 
that symbol is the name of the slot and is bound in the let. 
"
  `(let ,(mapcar (lambda (slot)
                   (if (consp slot)
                     (if (consp (first slot))
                       `(,(second (first slot)) (ensure-type (slot-value ,instance ',(first (first slot))) ',(second slot)))
                       `(,(first slot) (ensure-type (slot-value ,instance ',(first slot)) ',(second slot))))
                     `(,slot (slot-value ,instance ',slot))))
                 slots)
     ,@body))

;; macros.lisp is too high in the utils system to use deftest
#+damn
(deftestsuite defun*-tests () ()
  (:test
   ((let ((*optimizations-to-ignore* '(type))
          (*add-check-types* t))
     (multiple-value-bind (decl checks)
                          (parse-declare '(declare (type fixnum a)))
       (ensure (equal decl '(declare)))
       (ensure (equal checks '((check-type a fixnum)))))
     (multiple-value-bind (decl checks)
                          (parse-declare '(declare (type fixnum a) (single-float b) (optimize (space 3))))
       (ensure (equal decl '(declare (single-float b) (optimize (space 3)))))
       (ensure (equal checks '((check-type a fixnum) (check-type b single-float)))))))))

;;; ---------------------------------------------------------------------------

(defmacro funcall-if (fn &rest args &aux default)
  (when (consp fn)
    (setf default (cadr fn) fn (car fn)))
  `(if ,fn
     (funcall ,fn ,@args)
     ,default))

;;; ---------------------------------------------------------------------------

(defmacro push-end (value place &environment env)
  "Like PUSH, except that the value goes on the end of the PLACE list.  
If PLACE is (), then (value) is returned."
  #+Explorer (declare (ignore env))
  (multiple-value-bind (tmpvars tmpvals storevar storeform refform)
      (get-setf-expansion place #-Explorer env)
    (let ((oldval  (gensym "OLDVAL"))
	  (newcons (gensym "NEWCONS")))
    `(let* (,@(mapcar #'list tmpvars tmpvals)
	    (,oldval ,refform)
	    (,newcons (cons ,value nil)))
       (if ,oldval
	   (progn (setf (cdr (last ,oldval)) ,newcons)
		  ,oldval)
	   (let ((,@storevar ,newcons))
	     ,storeform)))))
  #+obsolete
  (let ((place-to-setf place))
    (once-only (place value)
      `(if ,place
           (progn
             (setf (cdr (last ,place)) (cons ,value nil))
             ,place)
           (setf ,place-to-setf (cons ,value nil))))))

    
;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
