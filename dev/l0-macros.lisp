(in-package metatilities)

;;; ---------------------------------------------------------------------------

(defmacro deprecated (&body body)
  "Wrap a function definition with `deprecated' to indicate that it should
no longer be used. If the first element of body is a string, it will be used
as additional documentation in the deprecation message. Foo example,

\(deprecated
  \"Use bar instead.\"
  \(defun foo-1 \(x\)
    \(format t \"~%FOO: ~A\" x\)\)\)

Will generate the message:

; warning FOO-1 has been deprecated. Use bar instead.

at compile time whereever foo-1 is used."
  (let ((documentation nil)
        (name nil))
    (when (stringp (first body))
      (setf documentation (first body)
            body (rest body)))
    (setf name (cadar body))
    `(progn
       (define-compiler-macro ,name (&whole form &rest args)
         (declare (ignore args))                          
         (fresh-line *error-output*)
         (write-string ,(format nil "~%; warning ~a has been deprecated.~@[ ~A~]" 
                                name documentation) *error-output*)
         (terpri *error-output*)
         (values form))
       ,@body)))

;;; ---------------------------------------------------------------------------

(defmacro once-only (variable-list &body body)
  "Generate code that evaluates certain expressions only once.
This is used in macros, for computing expansions.
VARIABLE-LIST is a list of symbols, whose values are subexpressions
to be substituted into a larger expression.  BODY is what uses those
symbols' values and constructs the larger expression.

ONCE-ONLY modifies BODY so that it constructs a different expression,
which when run will evaluate the subsexpressions only once, save the
values in temporary variables, and use those from then on.
Example:
\(DEFMACRO DOUBLE (ARG) `(+ ,ARG ,ARG)) expands into code that computes ARG twice.
\(DEFMACRO DOUBLE (ARG) (ONCE-ONLY (ARG) `(+ ,ARG ,ARG))) will not."
  
  (dolist (variable variable-list)
    (if (not (symbolp variable))
	(error "~S is not a variable" variable)))
  (let ((bind-vars (gensym))
	(bind-vals (gensym))
	(tem (gensym)))
    `(let ((,bind-vars nil)
	   (,bind-vals nil))
       (let ((result ((lambda ,variable-list . ,body)
		      . ,(loop for variable in variable-list
			       collect `(if (let ((variable ,variable))
					      (loop
						(when (atom variable) (return t))
						(when (or (eq (car variable) 'quote) 
							  (eq (car variable) 'function))
						  (return t))
						(if  (eq (car variable) 'the)
						     (setf variable (cadr (cdr variable)))
						     (return nil))))
					    ,variable
					    (let ((,tem (gensym)))
					      (push ,tem ,bind-vars)
					      (push ,variable ,bind-vals)
					      ,tem))))))
	 (if (null ,bind-vars)
	     result
	     `((lambda
		,(nreverse ,bind-vars) ,result) . ,(nreverse ,bind-vals)))))))

;;; ---------------------------------------------------------------------------

(defmacro with-variables (symbols &body body)
  "Using gensyms is necessary to prevent variables produced by macro expansions
from interfering with user variables, and naming them mnemonically helps make
macro expansions and compiled code easier to read, but it's a pain to create
them properly.  This macro creates them for you, which makes writing nice macros
easier.  For example, if you are writing a macro to iterate over an array, you
used to have to write:

 (defmacro do-2d-array ((elt array) &body body)
   (let ((row (gensym \"ROW\"))
         (col (gensym \"COL\")))
     `(dotimes (,row (array-dimension 0))
        (dotimes (,col ,(array-dimension 1))
           (let ((,elt (aref ,array ,row ,col)))
               . ,body)))))

Now you can just write the following, which eliminates the need to laboriously
create the mnemonic gensyms.

 (defmacro do-2d-array ((elt array) &body body)
   (with-variables (row col)
      `(dotimes (,row ,(array-dimension 0))
          (dotimes (,col ,(array-dimension 1))
             (let ((,elt (aref ,array ,row ,col)))
                 . ,body))))
"
  `(let ,(mapcar #'(lambda (sym)
		     `(,sym (newsym ,(symbol-name sym))))
		 symbols)
     . ,body))

;;; ---------------------------------------------------------------------------

;; a simple shorthand
(defmacro eval-always (&body body)
  "Expands into an eval-when with all the fixings. It's nothing but a shorthand."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;;; ---------------------------------------------------------------------------

(defmacro defclass-property (property &optional (default nil default-supplied?))
  "Create getter and setter methods for 'property' on symbol's property lists." 
  (once-only (property)
    (let ((real-name (form-keyword property)))
      `(progn
         (defmethod ,property ((class-name symbol))
           (get class-name ,real-name ,@(when default-supplied? (list default))))
         (defmethod (setf ,property) (value (class-name symbol))
           (setf (get class-name ,real-name) value))))))

;;; ---------------------------------------------------------------------------

#+allegro
;; everyone else already defines this...
(defmacro without-interrupts (&body forms)
  "Executes `forms' as a critical section; no other threads can get in."
  ;; Because this macro will appear in user code (eg, the priority queue
  ;; code), whether there is multi-threading or not, we have to expand to
  ;; something, so we expand to progn.  The :eksl-generic-load-utils feature
  ;; is used as an otherwise case, since it will be defined as long as this
  ;; code is being loaded via our loading procedure.
  (or 
   #+ALLEGRO
   `(excl:without-interrupts . ,forms)
  
   ;; just in case...
   ;; default
   `(progn . ,forms)))

;;; ---------------------------------------------------------------------------

;;; This is a more portable name, IMO.  It can't hurt.
(defmacro with-atomic-execution (&body forms)
  `(without-interrupts
     ,@forms))

