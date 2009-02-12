(in-package #:metatilities)

(defun mapappend (fun &rest lists)
  "A non-destructive mapcan."
  (reduce #'append (apply #'mapcar fun lists)))

(eval-when (:load-toplevel)
  (setf (fdefinition 'mappend) #'mapappend))


;;; ---------------------------------------------------------------------------
;;; Wonderfully inefficient functions for trees (i.e., arbitrarily nested
;;;   lists)
;;; ---------------------------------------------------------------------------

(defun tree-map (fn tree)
  "Maps FN over every atom in TREE."
  (cond
   ((null tree) nil)
   ((atom tree) (funcall fn tree))
   (t
    (cons
     (tree-map fn (car tree))
     (tree-map fn (cdr tree))))))

(defun tree-find (item tree &key (test #'eql) (key #'identity))
  "Finds the atom ITEM within the arbitrarily-nested cons TREE."
  (cond
   ((null tree) nil)
   ((atom tree)
    (funcall test item (funcall key tree)))
   ;; overly complicated so this will work for NIL
   ((atom (car tree))
    (or
     (funcall test item 
              (funcall key 
                       (car tree)))
     (tree-find item (cdr tree) :test test :key key)))
   (t
    (or 
     (tree-find item (car tree) :test test :key key)
     (tree-find item (cdr tree) :test test :key key)))))

(defun tree-find-if (test tree)
  "Finds first cons in TREE that satisfies TEST.  This is guaranteed to be
   inefficient."
  (cond
   ((funcall test tree) tree)
   ((atom tree) nil)
   (t
    (or
     (tree-find-if test (car tree))
     (tree-find-if test (cdr tree))))))

(defun tree-remove-if (test tree)
  "Removes all atoms from TREE that satisfy TEST."
  (loop for elem in tree
        unless (and
                (atom elem)
                (funcall test elem))
        collect (cond
                 ((atom elem) elem)
                 (t (tree-remove-if test elem)))))

;;; ---------------------------------------------------------------------------

(defun partition (seq fn &key (key #'identity))
  "Partitions SEQ based on FN.  Returns twa values: (a) the list of all elements
   of SEQ for which FN returns true, and (b) the list of all elements of SEQ for
   which FN returns false."
  (let (true-list false-list)
    (flet ((put-on-list (elem)
             (if (funcall fn
                          (funcall key elem))
               (push elem true-list)
               (push elem false-list))))
      (map nil #'put-on-list seq)
      (values 
       (nreverse true-list)
       (nreverse false-list)))))
  
;;;----------------------------------------------------------------------------

(setf (symbol-function 'find-all-if) #'remove-if-not)

(setf (symbol-function 'find-all-if-not) #'remove-if)

;; No sense paying extra for a function that does not do anything over
;; what is already there.
(declaim (inline find-all))

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  #-Lucid
  (apply #'remove item sequence
	 :test (complement test) keyword-args)
  ;; The :test-not args are deprecated, but Lucid is behind the times.
  #+Lucid
  (apply #'remove item sequence
         :test-not test keyword-args))

;;; ============================================================================

(defun nmerge-list (list1 list2 compare-fn &key (key #'identity))
  "Destructive merge of `list2' into `list1'.  Both lists are may be changed.
This works best if the lists are NOT cdr-coded."
  (when (not list1) (return-from nmerge-list list2))
  (when (not list2) (return-from nmerge-list list1))
  (let (new-list new-list-end)
    (cond ((funcall compare-fn
                    (funcall key (first list1))
                    (funcall key (first list2)))
           (setf new-list list1
                 list1 (cdr list1)))
          (t
           (setf new-list list2
                 list2 (cdr list2))))
    (setf new-list-end new-list)
    (do ()
        (nil)
      (when (not list1)
        (setf (cdr new-list-end) list2)
        (return))
      (when (not list2)
        (setf (cdr new-list-end) list1)
        (return))
      (cond ((funcall compare-fn
                      (funcall key (first list1))
                      (funcall key (first list2)))
             (setf (cdr new-list-end) list1
                   new-list-end list1
                   list1 (cdr list1)))
            (t
             (setf (cdr new-list-end) list2
                   new-list-end list2
                   list2 (cdr list2)))))
    new-list))

;;;----------------------------------------------------------------------------

(defun delete-if! (predicate sequence)
  "Just like `delete-if' except that it returns two values: a list of the
deleted items, and the new sequence."
  (let* ((deleted-items  nil)
         (new-sequence   (delete-if #'(lambda (item)
                                        (if (funcall predicate item)
                                            (push item deleted-items)))
                                    sequence)))
      (values (nreverse deleted-items) new-sequence)))

;;;----------------------------------------------------------------------------

;;; Replacement for (delete-if-not #'identity array) in Allegro which
;;; seems to return a new array without a fill pointer.
;; This could be slightly more efficient, but currently it is only called 
;; once when the firemap is built.

(defun compact-array (array)
  "Destructively compacts an array with fill-pointer. Works on type T
arrays and considers elements with Nil in them to be empty."
  (declare (array array))
  (let* ((next-index-to-fill (aif (position nil array :test #'eq) it -1))
	 (scan-index next-index-to-fill)
	 (length (length array)))
    (declare (fixnum next-index-to-fill scan-index length))
    (when (not (minusp next-index-to-fill))
      ;; Map over the whole array keeping 'next-index-to-fill' pointing at
      ;; next empty element while the 'scan-index' finds full elements to move.
      (loop named scan do
	    ;; look for the next full element
	    ;; or hit the end of the array
	    (loop do (when (= (incf scan-index) length) (return-from scan))
		     (when (aref array scan-index) (return)))
	    ;; if did not hit the end we copy the value to the empty spot
	    (setf (aref array next-index-to-fill) (aref array scan-index))
	    (setf (aref array scan-index) nil)
	    ;; find next empty element to fill
	    (loop until (or (= (incf next-index-to-fill) length)
			    (null (aref array next-index-to-fill)))))
    (setf (fill-pointer array) next-index-to-fill)))
  (values array))

#+test
(defun test-compact-array ()
  (flet ((doit (array)
	   (spy (:no-newline t) "BEFORE" array)
	   (compact-array array)
	   (spy (:no-newline t) "AFTER" array)))
    
    (doit (make-array 0 :fill-pointer 0))
    (doit (make-array 1 :fill-pointer 1  :initial-contents '(1)))
    (doit (make-array 2 :fill-pointer 2  :initial-contents '(nil 1)))
    (doit (make-array 2 :fill-pointer 2  :initial-contents '(1 nil)))
    (doit (make-array 3 :fill-pointer 3  :initial-contents '(nil 1 nil)))
    (doit (make-array 4 :fill-pointer 4  :initial-contents '(nil nil nil nil)))
    (doit (make-array 4 :fill-pointer 4  :initial-contents '(1 2 3 4)))
    (doit (make-array 12 :initial-contents '(1 2 nil 4 5 6 7 8 nil nil nil 12) :fill-pointer 12))
    (doit (make-array 13 :initial-contents '(1 2 nil 4 nil 6 nil 8 nil nil nil 12 nil) :fill-pointer 13))
    (doit (make-array 13 :initial-contents '(nil nil nil nil nil 6 nil 8 nil nil nil 12 nil) :fill-pointer 13))
    ))

#+test
(defun test-compact-array-speed ()
  (flet ((doit (array)
	   (let ((a2 (copy-seq array)))
	     (spy (length (time (compact-array array))))
	     (spy (length (time (setf a2 (delete-if-not #'identity a2))))))))
    
    (doit (make-array 100000 :fill-pointer t 
		      :initial-contents (loop repeat 100000 collect (if (zerop (random 2)) nil t))))))

; -----------------------------------------------------------------------------



;;; ============================================================================

(defun circular-list (&rest elements)
  "Builds a circular list with `elements'."
  (when elements
    (setq elements (copy-list elements))
    (rplacd (last elements) elements)
    elements))

;;; ============================================================================

(defun gensym0 (x)
  "Ignores its argument and returns a gensym.  Useful for mapping of a bunch of
arbitrary forms, since `gensym' requires that its argument be a string or a number.

(See also GENSYM* in macros.lisp.)
"
  (declare (ignore x))
  (gensym "G"))

#+test
(defun test-gensym0 ()
  (print (mapcar #'gensym0 '(1 "A" b (+ 2 3)))))


;;; ============================================================================

(defun group (list n)
  "Return the elements of `list' grouped in sublists of length `n,' which should
be a positive integer.  For example, the list '(a b c d) grouped by 2 yields
'((a b) (c d)).  Definition of `group' taken from On Lisp, by Paul Graham.  The
definition is not hugely efficient, so this function is appropriate for parsing
macro args, but no for inner loops."
  (check-type list list)
  (check-type n (integer 1 #.most-positive-fixnum))
  (labels ((next (list n acc)
	     (let ((rest (nthcdr n list)))
	       (if (consp rest)
		   (next rest n (cons (subseq list 0 n) acc))
		   (nreverse (cons list acc))))))
    (next list n nil)))

#+test
(defun test-group ()
  (print (group '(a b c d) 3))
  (print (group '(a b c d) 2))
  (print (group '(a b c d) 1))
  (print (group '(a b c d) 0)))

;;;----------------------------------------------------------------------------

;; Not a macro, but it could be.
(defun make-initialized-array (&rest inits)
  "Creates an array initialized from using `inits' which is a list of the form
 \(\(<index-1> <init-form-1>\)
  \(<index-2> <init-form-2>\)
  ...
  \(<index-n> <init-form-n>)\)
Each element indexed will be filled with the result of evaluating the
corresponding initialization form. The array returned will be just large
enough to hold the element with the maximum index."
  
  (let ((size 0)
        array)
    (doplist (k v inits)
      (setf size (max size k)))
    (setf array (make-array (1+ size)))
    (doplist (k v inits)
      (setf (aref array k) v))
    array))

;;; ============================================================================

(defun object->string (object)
  "Uses the Common Lisp `string' function to convert `object' into a string, but
also converts objects of the form 'x to \"x\"."
  (if (and (listp object)
	   (eq (car object) 'quote))
      (string (cadr object))
      (string object)))

;;; ============================================================================

(defun float->integer (number)
  "You can't coerce a float to an integer, because they want to you decide
whether you want to use round, floor, ceiling, and so forth.  Sometimes, though,
you know or believe that the float is the floating point representation of an
integer, so all of the above would produce the same result.  This function is
for that case.  It coerces `number' to an integer, even if it's a float.  The
fractional part of the float must be zero or an error is signalled."
  (etypecase number
    (integer number)
    (float   (multiple-value-bind (int frac) (floor number)
	       (if (zerop frac)
		   int
		   (error "Doesn't represent a whole number: ~s" number))))))

#+test
(defun test-float->integer ()
  (spy (float->integer 3))
  (spy (float->integer 3.0))
  (spy (float->integer 3.1))
  (spy (float->integer 1/3)))

;;;----------------------------------------------------------------------------

;; I wouldn't put this in any time critical places if you are dealing with
;; large lists.
(defun sort-using-list-order (x list &key (test #'eq) key (list-key #'identity))
  "Sorts the list `x' using the other list `list' as a index."
  (flet ((item-lessp-function (list  test item-key list-key)
           (if item-key
             #'(lambda (item-1 item-2)
                 (setf item-2 (funcall item-key item-2)
                       item-1 (funcall item-key item-1))
                 (let ((item-2-present (member item-2 list :test test :key list-key))
                       (item-1-present (member item-1 list :test test :key list-key)))
                   (if (and item-1-present item-2-present)
                     (not (member item-1 item-2-present :test test :key list-key))
                     ;; otherwise
                     (if item-1-present t nil))))
             #'(lambda (item-1 item-2)
                 (let ((item-2-present (member item-2 list :test test :key list-key))
                       (item-1-present (member item-1 list :test test :key list-key)))
                   (if (and item-1-present item-2-present)
                     (not (member item-1 item-2-present :test test :key list-key))
                     ;; otherwise
                     (if item-1-present t nil)))))))
    (let ((lessp-function (item-lessp-function list test key list-key)))
      (stable-sort x lessp-function))))

;;; ============================================================================

(defun unused-variables (lambda-list form)
  "Returns which of the variables defined by LAMBDA-LIST do not appear in form.
   This works simply by textual search so it should not be used for (a) special variables,
   and (b) will not catch uses like (intern 'VAR-NAME')."
  (remove-if #'(lambda (var)
                 (tree-find var form))
             (lambda-list->args lambda-list)))

(defun lambda-list->args (lambda-list)
  "Takes a formal parameter list, possibly containing things like &optional or (key
default), and returns a list of the argument variables.  This is useful when processing
arguments in a macro that will define a function, say if you want to declare them all
ignoreable or something.  Returns a flat list of symbols."
  ;; mode must go :required -> &optional -> &rest -> &key -> &allow-other-keys -> &aux
  ;; but modes may be skipped.  &rest is weird because there can be only one &rest arg,
  ;; so we have an intermediate mode &rest1, which means that one &rest arg has been
  ;; read, so error if there's another.
  (let ((mode :required)
	(args nil))
    (flet ((okay? (arg)
	     (and (symbolp arg)
		  (not (keywordp arg))
		  (not (member arg lambda-list-keywords)))))
      (dolist (elt lambda-list)
	(ecase mode
	  (:required
	   (cond ((member elt '(&optional &rest &key &aux))
		  (setf mode elt))
		 ((okay? elt)
		  (push elt args))
		 (t
		  (error "Bad required arg in lambda-list:  ~s" elt))))
	  (&optional
	   (cond ((member elt '(&rest &key &aux))
		  (setf mode elt))
		 ((okay? elt)
		  (push elt args))
		 ;; Default value, but no supplied-p var
		 ((and (listp elt)
		       (null (cddr elt))
		       (okay? (car elt)))
		  (push (car elt) args))
		 ;; Default value and supplied-p var
		 ((and (listp elt)
		       (not (null (cddr elt)))
		       (okay? (car elt))
		       (okay? (caddr elt)))
		  (push (car elt) args)
		  (push (caddr elt) args))
		 (t
		  (error "Bad &optional arg in lambda-list:  ~s" elt))))
	  (&rest
	   (cond ((member elt '(&key &aux))
		  (error "No &rest arg in lambda-list:  ~s" lambda-list))
		 ((okay? elt)
		  (push elt args)
		  ;; Special mode
		  (setf mode '&rest1))
		 (t
		  (error "Bad &rest arg in lambda-list:  ~s" elt))))
	  (&rest1
	   (cond ((member elt '(&key &aux))
		  (setf mode elt))
		 (t
		  (error "Bad &rest arg in lambda-list:  ~s" elt))))
	  (&key
	   (cond ((member elt '(&allow-other-keys &aux))
		  (setf mode elt))
		 ((okay? elt)
		  (push elt args))
		 ;; Default value, but no weird stuff
		 ((and (listp elt)
		       (okay? (car elt)))
		  (push (car elt) args))
		 ;; Weird stuff; that is, arg name supplied
		 ((and (listp elt)
		       (listp (car elt))
		       (okay? (cadar elt)))
		  (push (cadar elt) args))
		 (t
		  (error "Bad &key arg in lambda-list:  ~s" elt))))
	  (&allow-other-keys
	   (if (eq elt '&aux)
	       (setf mode elt)
	       (error "Bad stuff after &allow-other-keys: ~s" lambda-list)))
	  (&aux
	   (if (okay? elt)
	       (push elt args)
	       (error "Bad &aux arg:  ~s" elt)))))
      (nreverse args))))

#+test
(defun test-lambda-list->args ()
  (spy (lambda-list->args '(a b c)))
  (spy (lambda-list->args '(a &optional c)))
  (spy (lambda-list->args '(a &optional (c 1))))
  (spy (lambda-list->args '(a &optional (c 1 c?))))
  (spy (lambda-list->args '(a &optional (c 1 c?) &rest d)))
  (spy (lambda-list->args '(a &optional &rest d)))
  (spy (lambda-list->args '(a &rest d)))
  (spy (lambda-list->args '(a &rest d &key e)))
  (spy (lambda-list->args '(a &key e (f 2))))
  (spy (lambda-list->args '(a &key e ((:f g 2)))))
  (spy (lambda-list->args '(a &key e &allow-other-keys)))
  (spy (lambda-list->args '(a &key e &allow-other-keys &aux g)))
  (spy (lambda-list->args '(a &aux g h)))
  )

;;; ============================================================================

(declaim (inline car-safe))
(defun car-safe (x)
  "Returns the car of `x' if it's a cons, NIL otherwise."
  (and (consp x) (car x)))

(declaim (inline car-safe))
(defun cdr-safe (x)
  "Returns the cdr of `x' if it's a cons, NIL otherwise."
  (and (consp x) (cdr x)))

;;; ---------------------------------------------------------------------------

(defparameter +very-small-number+ 1E-12
  "Someones idea of a very small number. Used in very-small-number-p.")

;;; ---------------------------------------------------------------------------

#+New
;; cons 50-% less, 3x slower
(defun very-small-number-p (number)
  (declare (optimize (speed 3) (space 3) (debug 0) (safety 0)))
  (or (and (plusp number) (< number +very-small-number+))
      (and (minusp number) (> number (- +very-small-number+)))
      (zerop number)))

(defun very-small-number-p (number)
  (declare (optimize (speed 3) (space 3) (debug 0) (safety 0))
           ;; Gary King 2006-06-03: thanks to Cyrus Harmon
           (type real number))
  (< (abs number) +very-small-number+))

;;; ---------------------------------------------------------------------------
;;; Some macro writing utilities...
;;; ---------------------------------------------------------------------------

(defun convert-clauses-into-lists (clauses-and-options clauses-to-convert)
  ;; This is useful (for me at least!) for writing macros
  (let ((parsed-clauses nil))
    (do* ((clauses clauses-and-options (rest clauses))
          (clause (first clauses) (first clauses)))
         ((null clauses))
      (if (and (keywordp clause)
               (or (null clauses-to-convert) (member clause clauses-to-convert))
               (not (length-1-list-p clauses)))
        (progn
          (setf clauses (rest clauses))
          (push (list clause (first clauses)) parsed-clauses))
        (push clause parsed-clauses)))
    (nreverse parsed-clauses)))

#|
example:

  (convert-clauses-into-lists* '(:a 3 :b 2 (:c 4)))
  > ((:a 3) (:b 2) (:c 4))

|#

(defun convert-clauses-into-lists* (clauses-and-options)
  ;; This is useful (for me at least!) for writing macros
  ;;  Like CONVERT-CLAUSES-INTO-LISTS (q.v.), but does not check that keywords are valid.
  (convert-clauses-into-lists clauses-and-options nil))

;;; ---------------------------------------------------------------------------

(defun cleanup-parsed-parameter (parameter)
  (if (length-1-list-p parameter)
    (first parameter)
    parameter))

;;; ---------------------------------------------------------------------------

;;?? Why not use subseq?
(defun firstn (n list)
  "Returns first 'n' elements of list."
  (loop repeat n 
        for element in list collect element))

;;; ---------------------------------------------------------------------------
;;; curry :: function -> arg1 ... argn -> function
;;; ---------------------------------------------------------------------------

(defun curry (fun &rest args1)
  "Curries fun on the fly"
  #'(lambda (&rest args2)
      (apply fun (append args1 args2))))

(define-compiler-macro curry (fun &rest curried-args)
  ;; The general idea here is that the compiler can more easily optimize
  ;; the expansion.  That may or may not be the case.  Since we use
  ;; &rest, the expansion conses on every call, though it might be
  ;; easier to keep things on the stack this way.
  
  ;; Hard to say if it was worth the effort.
  (with-unique-names (args)
    `#'(lambda (&rest ,args)
         ; can I safely declare this to have dynamic extent??? - Well, why not?
	 (declare (dynamic-extent ,args))
         (apply ,fun ,@curried-args ,args))))

;;; FIXME: I can't decide if the 1 argument case should get a special function.  If
;;;        we had fixed arity, we could avoid excess consing on every call, which 
;;;        is kinda lame, when you think about it.

;;; ---------------------------------------------------------------------------
;;; curry-after :: function -> arg1 ... argn -> function
;;; ---------------------------------------------------------------------------

(defun curry-after (fun &rest args1)
  "Sometimes we want to curry a function over some arguments
that appear at the end of the arg list order.  This function
curries 'fun' so that 'args1' are given to it after the input args
of the curry."
  #'(lambda (&rest args2)
      (apply fun (append args2 args1))))

;;; ---------------------------------------------------------------------------

;;?? probably worth a compiler macro
(defun compose (&rest fns)
  "Return a function that is the composition of fn1 and fn2. I.e., 
\(compose f1 f2\)[x] == \(f1 \(f2 [x]]\)\)."
  (let ((fns-reversed (reverse fns)))
    (lambda (&rest args)
      (let ((result (apply (first fns-reversed) args)))
        (loop for fn in (rest fns-reversed) do 
              (setf result (funcall fn result)))
        result))))

(define-compiler-macro compose (&rest fns)
  (let ((last (car (last fns))))
    (with-unique-names (args)
      `(lambda (&rest ,args)
	 ,(reduce #'(lambda (fn arg)
		      (if (funcallable-expression-p fn)
			  `(,(extract-head-form fn) ,arg)
			  `(funcall ,fn ,arg)))
		  (butlast fns)
		  :initial-value `(apply ,last ,args)
		  :from-end t)))))

#+Old
(defun compose (fn1 fn2)
  "Return a function that is the composition of fn1 and fn2. I.e., 
\(compose f1 f2\)[x] == \(f1 \(f2 [x]]\)\)."
  #'(lambda (&rest args)
      (funcall fn1 (apply fn2 args))))

;;; ---------------------------------------------------------------------------
;;; return a function that is the conjuction of fns.  basically a functional and
;;; ---------------------------------------------------------------------------
(defun conjoin (fn &rest fns)
  "Returns the conjuction of the predicates given as arguments.  As with ~
   AND, evaluation stops if one returns NIL."
  (reduce #'(lambda (x y)
              #'(lambda (&rest args)
                  (let ((z (apply x args)))
                    (and z (apply y args))))) (cons fn fns)
          :from-end t))

(define-compiler-macro conjoin (&rest fns)
  (with-unique-names (arg)
    `#'(lambda (,arg)
	 (and ,@(mapcar #'(lambda (fn) 
			    (if (funcallable-expression-p fn)
				`(,(extract-head-form fn) ,arg)
				`(funcall ,fn ,arg))) 
			fns)))))
              
;;; ---------------------------------------------------------------------------
;;; return a function that is the disjunction of fns.  basically a functional or
;;; ---------------------------------------------------------------------------
(defun disjoin (fn &rest fns)
  "Returns the disjunction of the predicates given as arguments.  As with ~
   OR, evaluation stops if one returns non NIL." 
  (reduce #'(lambda (x y)
              #'(lambda (&rest args)
                  (let ((z (apply x args)))
                    (or z (apply y args))))) (cons fn fns)
          :from-end t))

(define-compiler-macro disjoin (&rest fns)
  (with-unique-names (arg)
    `#'(lambda (,arg)
	 (or ,@(mapcar #'(lambda (fn) 
			   (if (funcallable-expression-p fn)
			       `(,(extract-head-form fn) ,arg)
			       `(funcall ,fn ,arg))) 
		       fns)))))

;;; ---------------------------------------------------------------------------

(defun add-classes-if-necessary (class-list superclasses &key (at-end? t))
  (when (find :at-end? superclasses) (remf superclasses :at-end?))
  (loop for superclass in superclasses do
        (setf class-list (add-class-if-necessary class-list superclass
                                                 :at-end? at-end?)))
  class-list)

;;; ---------------------------------------------------------------------------

(defun direct-superclasses-defclass* (class-name)
  (typecase class-name
    (symbol (defclass*-superclasses class-name))
    (standard-class (defclass*-superclasses (class-name class-name)))
    (t nil)))

;;; ---------------------------------------------------------------------------

(defun superclasses-defclass* (class-name)
  (remove-duplicates
   (loop for superclass in (direct-superclasses-defclass* class-name) nconc
         (append (ensure-list superclass)
                 (awhen (superclasses-defclass* superclass) it)))))

;;; ---------------------------------------------------------------------------

(defun add-class-if-necessary (class-list superclass-name &key (at-end? t))
  "Adds the superclass to the class-list unless it's already there."
  (let ((add-action? t))
    (loop for classname in class-list 
          while add-action? do
          (when (or (eq classname superclass-name)
                    (member superclass-name (superclasses-defclass* classname)))
            (setf add-action? nil)))
    
    (when add-action?
      (if at-end?
        (push-end superclass-name class-list)
        (push superclass-name class-list)))
    
    class-list))

;;; ---------------------------------------------------------------------------

(defun remove-leading-quote (list)
  "Removes the first quote from a list if one is there."
  (if (and (consp list) (eql (first list) 'quote))
    (first (rest list))
    list))

#|
(addtest (test-sadl)
  (ensure (length-at-least-p '(0 1 2 3 4) 2)))

(addtest (test-sadl)
  (ensure (not (length-at-least-p '(0 1 2 3 4) 100))))

(addtest (test-sadl)
  (ensure (length-at-least-p '(0 1 2 3 4) 5)))

(addtest (test-sadl)
  (ensure (not (length-at-least-p "" 1))))
|#

;;; ---------------------------------------------------------------------------

;; This code courtesy of Scott Anderson

(defun nth-elt-of-cross-product (n &rest sets)
  "Suppose we created a multidimensional array, with as many dimensions as
we have sets, and in which the size of each dimension were equal to the size of
the corresponding set.  That array would have exactly as many elements as the
cross product of all these sets, and we could easily put the elements of the
cross product into the array by stepping the index for each dimension through
each of the sets.  But this array notion also gives a unique index to every
element of the cross product.  This function works as if we created such an
array and then did an `array-row-major-aref' into it."
  ;; The code looks just like `decode-row-major-index'
  (let ((remainder (mod n (reduce #'* sets :key #'length)))
	index-in-this-dimension)
    (append
     (loop repeat (- (length sets) 1)
           for d from 0 do
           (multiple-value-setq (index-in-this-dimension remainder)
	     (truncate remainder (reduce #'* sets :start (1+ d) :key #'length)))
           collect (elt (elt sets d) index-in-this-dimension))
     (list (elt (elt sets (- (length sets) 1)) remainder)))))

;;; ---------------------------------------------------------------------------

(defun nth-elt-of-cross-product-as-multiple-values (n &rest sets)
  "Returns list of values of `nth-elt-of-cross-product' as multiple values.
See it for documentation."
  (values-list (apply #'nth-elt-of-cross-product n sets)))

#|
(defun test-nth-elt-of-cross-product ()
  (dotimes (i 27)
    (print (nth-elt-of-cross-product i '(a b c) '(1 2 3) '(x y z))))
  #+Explorer
  (timeit (:cpu :repeat 100)
    (nth-elt-of-cross-product (random 27) '(a b c) '(1 2 3) '(x y z))))
|#

;;; ---------------------------------------------------------------------------
;;; constant-expression-p
;;; ---------------------------------------------------------------------------

(defun constant-expression-p (expression)
  (cond ((atom expression)
         (constantp expression))
        ((eq (first expression) 'quote)
         (values t))
        (t
         (and (constant-function-p (first expression))
              (every #'constant-expression-p
                     (rest expression))))))

;;; ---------------------------------------------------------------------------

(defun constant-function-p (operator)
  (declare (ignorable operator))
  
  (not (special-operator-p operator)))

;;; ---------------------------------------------------------------------------
;;; make-sorter
;;; ---------------------------------------------------------------------------

;;?? Should probably do this at compile time, not at run time but that requires
;; more brain juice that I currently have available
(defun make-sorter (keys-and-predicates)
  "Returns a sorter-predicate that returns true if it follows the KEYS-AND-PREDICATES
specification.  KEYS-AND-PREDICATES is a list of key-predicate pairs:
\(<key> <predicate>\), where precidence of key-predicate pair is based on
order in KEYS-AND-PREDICATES list.  E.g., if \(\(first <\) \(second <\) \(third <\)\),
then the sorter-predicate will first check wether the first item of one list is < 
the first item of another, then the second, etc."
  (lambda (item-1 item-2)
    (block sorter
      (loop for datum in keys-and-predicates do
            (bind (((key less-predicate &optional (same-predicate 'samep)) datum)
                   (x-1 (funcall key item-1))
                   (x-2 (funcall key item-2)))
              (cond ((funcall less-predicate x-1 x-2)
                     (return-from sorter t))
                    ((not (funcall same-predicate x-1 x-2))
                     (return-from sorter nil)))))
      ;; they are the same in all respects...
      (values nil))))

#+Ignore
(let ((test-list '((1 1 1)
                   (1 1 2)
                   (1 2 3)
                   (2 1 1)
                   (2 2 1)
                   (2 2 2))))
  (equal
   (sort 
    (shuffle-elements! (copy-list test-list))
    (make-sorter `((first <) (second <) (third <))))
   test-list))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
