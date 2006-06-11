(in-package #:metatilities)

;;; Printing rationals as floats doesn't work in Lispworks; it has no effect. (Westy)
;;; We could do this without too much trouble in spy-princ.  We could also 

(defparameter *spy-debugging* t)
(defparameter *spy-output*
  #-Lispworks *trace-output* #+Lispworks *standard-output*)
(defparameter *spy-no-newline* nil)
(defparameter *spy-no-expand* t)

(defun spy-prin1 (form values)
  (format *spy-output* "~s => ~{~s~^ ~}~:[~&~; ~]" form values *spy-no-newline*))

(defun spy-parse-arglist (forms)
  (cond ((and (consp (car forms)) (keywordp (caar forms)))
         (destructuring-bind (&key (stream '*spy-output*) (no-newline '*spy-no-newline*)
                                   (no-expand '*spy-no-expand*) (debug '*spy-debugging*)) (car forms)
           (declare (ignore debug))
           (values (cdr forms) stream no-newline no-expand 
                   ;; XXX The docstring for debugging-p-fn is wrong viz a viz t.  
                   ;; Let's hope it stays that way...
                   (or #+Ignore (debugging-p-fn debug) '*spy-debugging*))))
        (t (values forms '*spy-output* '*spy-no-newline* '*spy-no-expand* '*spy-debugging*))))

(defun spy-do-form (form)
  (with-unique-names (f v)
    `(let ((,f (if *spy-no-expand* ',form ,(spy-expand-form form)))
           (,v (multiple-value-list ,form)))
       (when *spy-debugging*
         (unless *spy-no-expand*
           (spy-prin1 ',form nil))
         (spy-prin1 ,f ,v))
       (values-list ,v))))

(defun spy-expand-form (form)
  ;; produces a form equivalent to the input, except that all non-car atoms
  ;; are evaluated, so that when it prints, the user can see the "expanded"
  ;; version of the form.  Certain special forms are handled specially.
  (if (atom form)
    #+CRAP
    `(write-nicely ,form)
    #-CRAP
    form
    (let ((car (car form)))
      (case car
        ((setq setf)
         `(list ',car ,@(let ((place? t))
                          (mapcar #'(lambda (subform)
                                      (prog1 (if place?
                                               `',subform
                                               (spy-expand-form subform))
                                        (setf place? (not place?))))
                                  (cdr form)))))
        ((and or prog1 progn)
         `(list ',car ,@(mapcar #'spy-expand-form (cdr form))))
        (t (if (or (macro-function car)
                   (special-operator-p car))
             `',form
             `(list ',car ,@(mapcar #'spy-expand-form (cdr form)))))))))

(defmacro spy (&rest forms)
  "A debugging tool: wrapping this around a form causes both the form and its
values to be printed on *trace-output*.  Like `progn,' this returns the value\(s\)
of the last form, so that it can be safely wrapped about functional code as
well.  In other words, \(spy \(foo\)\) returns the same values as \(foo\).
The first form can optionally be a list of options, eg., keyword value pairs\).
Options supported:
:STREAM <stream>      - directs output to `stream'
:NO-NEWLINE <boolean> - if true, suppresses insertion of newlines between forms
:NO-EXPAND <boolean>  - if true, reprints the form with values replacing subforms
:DEBUG     <symbol>   - print only if we are debugging `symbol' or *spy-debugging* is true (the default)
"
  (multiple-value-bind (forms stream no-newline-p no-expand-p debuggingp) (spy-parse-arglist forms)
    `(let ((*spy-output* ,stream)
           (*spy-no-newline* ,no-newline-p)
           (*spy-no-expand* ,no-expand-p)
           (*spy-debugging* ,debuggingp))
       ,@(mapcar #'spy-do-form forms))))

(defmacro spy* (tag &rest forms)
  "Like spy, but the first argument is a debugging tag and *spy-debugging* is implicitly 
nil."
  `(let ((*spy-debugging* nil))
     (spy (:debug ,tag) ,@forms)))

(defmacro spyx (&rest forms)
  "A version of `spy' which expands the forms.  See the documentation for `spy,'
particularly the :expand option.  This macro just supplies that option."
  `(spy (:no-expand nil) . ,forms))

#+test
(defun test-spy ()
  (let ((c 1/5)) (spy c))
  (spy (/ 1 2))
  (spy (:no-expand t) (floor 4 3))
  (let ((a 1) (b 2)) (spy (/ a b)))
  (let ((a 1) (b 2) c) (spyx (/ (setf c (+ a b)) (+ a b))))
  (let ((a 1) (b 2) c) (spy (and a b (or (prog1 a b) (progn c)))))
  (let ((a 1) (b 2)) (spy (multiple-value-bind (c d) (floor a b) (values c d))))
  )

#|
;;;----------------------------------------------------------------------------
;;; PACKAGE HACK:
;;;
;;; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
;;; 
;;; This macro is put in the COMMON-LISP package so that it will be universally
;;; available without having to type a package prefix in front of it.  That's
;;; because it is a debugging function, and we want to make it quick to
;;; type.  It shouldn't exist in a finished product.
;;; 
;;; Yes, we know this is an abuse of the COMMON-LISP package, but it's the easiest way
;;; to make it universally accessible.

(#+allegro excl::without-package-locks
 #+lispworks let
 ;;; 12/5/2001 MS Updated to work with Lispworks 4.1 
 #+lispworks3.2.2 ((lw:*packages-for-warn-on-redefinition* nil))           
 #+lispworks4     ((hcl:*packages-for-warn-on-redefinition* nil))           
 #-(or allegro lispworks) progn
 
 (import '(spy spyx spy*) :common-lisp)
 (export '(spy spyx spy*) :common-lisp)

 ;;; The following is really awful, since clim-lisp is supposed to be pristine.
 ;;;   Like the stuff above isn't a bit gross? ---L.
 (when (find-package :clim-lisp)
   (import '(spy spyx) :clim-lisp)
   (export '(spy spyx) :clim-lisp))
 
) ; END OF PACKAGE HACK
|#


#|

OLD: The was nothing wrong with this, but it was easier to build in debugging support
     by messing around with it a bit. --L


(defmacro spy (&rest forms)
  "A debugging tool: wrapping this around a form causes both the form and its
values to be printed on *trace-output*.  Like `progn,' this returns the value\(s\)
of the last form, so that it can be safely wrapped about functional code as
well.  In other words, \(spy \(foo\)\) returns the same values as \(foo\).
The first form can optionally be a list of options, eg., keyword value pairs\).
Options supported:
:STREAM <stream>      - directs output to `stream'
:NO-NEWLINE <boolean> - if true, suppresses insertion of newlines between forms
:NO-EXPAND <boolean>  - if true, reprints the form with values replacing subforms
"
  (let ((no-newline nil)
	(no-expand  t)
	(stream #-Lispworks '*trace-output* #+Lispworks '*standard-output*))
    (when (and (consp (first forms)) (keywordp (first (first forms))))
      (let ((options (pop forms)))
	(do () ((null options))
	  (case (car options)
	    (:stream     (setf stream     (cadr options)))
	    (:no-newline (setf no-newline (cadr options)))
	    (:no-expand  (setf no-expand  (cadr options)))
	    (t           (cerror "ignore it and its argument"
				 "not a known option:  ~s" (car options))))
	  (setf options (cddr options)))))
    (labels ((spy-2 (form)
	       ;; produces a form equivalent to the input, except that all non-car atoms
	       ;; are evaluated, so that when it prints, the user can see the "expanded"
	       ;; version of the form.  Certain special forms are handled specially.
	       (if (atom form)
                   #+CRAP
		   `(write-nicely ,form)
                   #-CRAP
                   form
		   (let ((car (car form)))
		     (case car
		       ((setq setf)
			`(list ',car ,@(let ((place? t))
					 (mapcar #'(lambda (subform)
						     (prog1 (if place?
								`',subform
								(spy-2 subform))
							    (setf place? (not place?))))
						 (cdr form)))))
                       ((and or prog1 progn)
                        `(list ',car ,@(mapcar #'spy-2 (cdr form))))
		       (t (if (or (macro-function car)
                                  (special-operator-p car))
			      `',form
			      `(list ',car ,@(mapcar #'spy-2 (cdr form))))))))))
      (let ((formo (gensym "ORIGINAL-FORM"))
	    (formx (gensym "EXPANDED-FORM"))
	    (values (gensym "VALUES")))
	(flet ((spy-1 (form)
		 `(let ((,formo   ',form)
			(,formx  ,(unless no-expand (spy-2 form)))
			(,values  (multiple-value-list ,form)))
		    ,(when no-expand `(declare (ignore ,formx)))
		    ,(unless no-newline
		       `(fresh-line ,stream))
		    ,(if no-expand
			 `(format ,stream "~s =>~{ ~s~}"
                                  ,formo
                                  #-CRAP ,values
                                  #+CRAP (mapcar #'write-incely ,values))
			 `(format ,stream "~s =>~%~s =>~{ ~s~}" ,formo ,formx
                                  #-CRAP ,values
                                  #+CRAP (mapcar #'write-nicely ,values)))
		    ,(if no-newline
			 `(write-char #\space ,stream)
			 `(terpri ,stream))
		    (values-list ,values))))
	  `(progn
	     (let (#+Lispworks (*print-rationals-as-floats?* t))
	       #+Lispworks (declare (special *print-rationals-as-floats?*))
	       ,@(mapcar #'spy-1 forms))
	     ,@(when no-newline
		 `((terpri ,stream)))))))))

|#