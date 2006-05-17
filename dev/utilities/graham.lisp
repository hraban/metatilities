(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; Chapter 4.4: Search functions
;;; ---------------------------------------------------------------------------

(defun most (fn lst)
  "fn is a scoring function of one argument; returns the element
with the highest score."
  (if (null lst)
    (values nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (when (> score max)
            (setq wins obj
                  max  score))))
      (values wins max))))

(defun best (fn lst)
  "fn must be a predicate of two arguments; returns the element
which, according to the predicate, beats all the others."
  (if (null lst)
    nil
    (let ((wins (car lst)))
      (dolist (obj (cdr lst))
        (if (funcall fn obj wins)
          (setq wins obj)))
      wins)))

(defun mostn (fn lst)
  (if (null lst)
    (values nil nil)
    (let ((result (list (car lst)))
          (max (funcall fn (car lst))))
      (dolist (obj (cdr lst))
        (let ((score (funcall fn obj)))
          (cond ((> score max)
                 (setq max    score
                       result (list obj)))
                ((= score max)
                 (push obj result)))))
      (values (nreverse result) max))))

;;; ---------------------------------------------------------------------------
;;; Chapter 4.5: Mapping functions
;;; ---------------------------------------------------------------------------

(defun map0-n (fn n)
  "Returns list resulting from applying fn to all numbers i in [0, n]."
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  "Returns list resulting from applying fn to all numbers i in [1, n]."
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  "Returns list resulting from applying fn to all numbers i in [a, b]."
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  "Really generalized mapping."
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

#|
cas: Use mapappend
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))
|#

(defun mapcars (fn &rest lsts)
  "Has the some effect as (mapcar fn (apply #'append lsts))."
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

#|
cas: I think we have a map-tree somewhere...
(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar 
             #'(lambda (&rest args) 
                 (apply #'rmapcar fn args))
             args)))
|#

;;; ---------------------------------------------------------------------------
;;; On Lisp - Chapter 6
;;; ---------------------------------------------------------------------------

#|

Ex.

(filter-values (lambda (x) (when (numberp x) (1+ x))) (list 1 3 :a :b 5))
(2 4 6)

|#

(defun filter-values (fn lst)
  "FILTER as defined in _On_Lisp_.  Given a list, collects all the non-nil
   results of applying FN to elements of LST."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (when val (push val acc))))
    (nreverse acc)))

;;; ---------------------------------------------------------------------------
;;; On Lisp - Chapter 12
;;; ---------------------------------------------------------------------------

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))

;;; ---------------------------------------------------------------------------

(defmacro nilf (&rest args)
  `(allf nil ,@args))

;;; ---------------------------------------------------------------------------

(defmacro tf (&rest args)
  `(allf t ,@args))

;;; ---------------------------------------------------------------------------

(defmacro toggle! (&rest places)
  "Switches the values of each place to its opposite as in 
\(setf x \(not x\)\)."
  `(progn ,@(mapcar (lambda (a) `(toggle-aux ,a)) places)))

;;; ---------------------------------------------------------------------------

(define-modify-macro toggle-aux () not)

;;; ---------------------------------------------------------------------------

(define-modify-macro concf (obj) nconc)


;;; ---------------------------------------------------------------------------
;;; On Lisp - Chapter 15: Macros Returning Functions
;;; ---------------------------------------------------------------------------

(defmacro fn (expr)
  "A general-purpose function-builder.  EXPR should be of the
   form (OPERATOR . ARGUMENTS).  Will return a function equivalent
   to (lambda (arg0 arg1 ... arg1) (mapcar #'fn arguments)).
   e.g.  (fn (and integerp oddp)) => (lambda (x) (and (integerp x) (oddp x))).
   U:COMPOSE is treated specially, so that (fn (u:compose integerp oddp)) =>
   (lambda (x) (integerp (oddp x))).  (This is the same as (fn (integerp (oddp))))"
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns)) 
                         ,(rec (cdr fns)))
                       g)))
          (rec fns)))))


;;; ---------------------------------------------------------------------------
;;; On Lisp - Chapter 18: Destructuring
;;; ---------------------------------------------------------------------------

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1) (col 0))
               (mapcan
                (lambda (pat)
                  (incf row)
                  (setq col -1)
                  (mapcar (lambda (p)
                            `(,p (aref ,gar ,row ,(incf col))))
                          pat))
                pats))
         ,@body))))

#+Test
(let ((ar (make-array '(3 3))))
  (loop for r from 0 to 2 do
        (loop for c from 0 to 2 do
              (setf (aref ar r c) (+ (* r 10) c))))
  
  (with-matrix ((a b c)
                (d e f)
                (g h i))
               ar
    (list a b c d e f g h i)))
      
;;; ---------------------------------------------------------------------------

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar (lambda (p)
                       `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))
        
#+Test
(let ((ar (make-array '(3 3))))
  (loop for r from 0 to 2 do
        (loop for c from 0 to 2 do
              (setf (aref ar r c) (+ (* r 10) c))))
  
  (with-array ((a 0 0) (d 1 1) (i 2 2))
               ar
    (list a d i)))

;;; ---------------------------------------------------------------------------

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar (lambda (f)
                       `(,f (,(intern (concatenate 'simple-string 
                                                   (symbol-name name) "-" 
                                                   (symbol-name f)) *package*) ,gs)))
                     fields)
         ,@body))))



#+Test
(progn
  (defstruct visitor name title firm)
  (let ((q (make-visitor :name "Ted"
                         :title 'king
                         :firm 'franks)))
    (with-struct (visitor name firm title) q
      (list name firm title))))

;;; ---------------------------------------------------------------------------


(defun match (x y &optional binds)
  (acond2
    ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
    ((binding x binds) (match it y binds))
    ((binding y binds) (match x it binds))
    ((varsym? x) (values (cons (cons x y) binds) t))
    ((varsym? y) (values (cons (cons y x) binds) t))
    ((and (consp x) (consp y) (match (car x) (car y) binds))
     (match (cdr x) (cdr y) it))
    (t (values nil nil))))

(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))

(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (varsym? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
