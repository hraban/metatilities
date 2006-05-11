(in-package #:metatilities)
#|

Functions are not externalizable objects, so cannot be used in code
given to COMPILE-FILE. See section 3.2.4 of the ANSI standard for
further details. 

You can read about this in the Common Lisp HyperSpec document
available from our web pages:
http://www.xanalys.com/software_tools/reference/HyperSpec/Body/sec_3-2-4.html

   --- from LispWorks support

|#

(defmacro aif (test-form then-form &optional else-form (pred nil predp))
  "Anaphoric IF:  Binds the symbol `it' to the value of the `test.'"
  `(let ((it ,test-form))
     (if ,(if predp `(funcall ,pred it) 'it)
       ,then-form ,else-form)))

(defmacro awhen (test-form &body body)
  "Anaphoric WHEN:  Binds the symbol `it' to the value of the `test.'"
  `(aif ,test-form
        (progn ,@body)))

(defmacro awhile (expr &body body)
  "Anaphoric WHILE loop: Executes `expr' and `body' as long as `expr' is true.
Binds the symbol `it' to the value of the `test.'"
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  "Anaphoric AND:  Binds the symbol `it' to the value of the preceding `arg.'"  
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

(defmacro acond (&rest clauses)
  "Anaphoric COND:  Binds the symbol `it' to the value of the preceding `clause.'"
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
           (let ((it ,sym))
             it ;; prevent compiler warnings about unused IT
             ,@(cdr cl1))
           (acond ,@(cdr clauses)))))))

(defmacro alambda (parms &body body)
  "Anaphoric LAMBDA: Binds the symbol `self' to the function, so that it can
call itself recursively."
  `(labels ((self ,parms ,@body))
     #'self))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         ,(self (cdr args))))))
               args)))

(defmacro aif2 (test &optional then else)
  "Needs to be documented.  See Paul Graham's book."
  (let ((win (gensym)))
    `(multiple-value-bind (it ,win) ,test
       (if (or it ,win) ,then ,else))))

(defmacro awhen2 (test &body body)
  "Needs to be documented.  See Paul Graham's book."
  `(aif2 ,test
         (progn ,@body)))

(defmacro awhile2 (test &body body)
  "Needs to be documented.  See Paul Graham's book."
  (let ((flag (gensym)))
    `(let ((,flag t))
       (while ,flag
         (aif2 ,test
               (progn ,@body)
               (setq ,flag nil))))))

(defmacro acond2 (&rest clauses)
  "Needs to be documented.  See Paul Graham's book."
  (if (null clauses)
    nil
    (let ((cl1 (car clauses))
          (val (gensym))
          (win (gensym)))
      `(multiple-value-bind (,val ,win) ,(car cl1)
         (if (or ,val ,win)
           (let ((it ,val)) it ,@(cdr cl1))
           (acond2 ,@(cdr clauses)))))))

;;; ---------------------------------------------------------------------------

(defmacro aprog1 (first &body rest)
  "Anaphoric prog1. This binds IT to the first form so that it can
be used in the rest of the forms. The whole thing returns IT."
  `(let ((it ,first))
     ,@rest
     it))

;;; ---------------------------------------------------------------------------

(defmacro atypecase (keyform &body body)
  "Atypecase is anaphoric typecase. It is just like typecase except that it binds the thing being tested to `it`."
  `(let ((it ,keyform))
     (typecase it 
       ,@body)))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
