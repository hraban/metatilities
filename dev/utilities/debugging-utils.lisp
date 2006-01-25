;;;; -*- Mode:Common-Lisp; Package: metatilities; Fonts:(MEDFNT); Base:10 -*-

;;;; **************************************************************************
;;;; **************************************************************************
;;;; *                                                                        *
;;;; *                       Debugging Utility Functions                      *
;;;; *                                                                        *
;;;; **************************************************************************
;;;; **************************************************************************
;;;
;;; Written by: David L. Westbrook and Scott D. Anderson
;;;             Department of Computer and Information Science
;;;             University of Massachusetts
;;;             Amherst, Massachusetts 01003.
;;;
;;; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
;;; 

(in-package :metatilities)

(export '(define-debugging-class
	  undefine-debugging-class
          
          debug-fn
	  debug
          undebug-fn
	  undebug
          debugging-p-fn
          debugging-p
          
          when-debugging
	  when-debugging-format
	  when-debugging-format-if
          when-debugging-spy
	  *debug*
 	  debug-format ; local function in `when-debugging'
	  with-debugging
	  *debug-output*
	  current-function
	  edit-debug-status
	  *show-function-in-when-debugging-format?*
	  *show-class-in-when-debugging-format?*
          pesky-warnings
          
          debugging-apropos
          all-debug-classes-below-class 
          debug-classes-directly-below-class-that-have-subclasses
          debugging-class-p
          
          define-debugging-class-fn))


;;; --*--
;;; ***************************************************************************
;;; Debugging Classes

;;; This code gives you the ability to define heirarchical classes to control
;;; when debugging code is run. The primary routines in the user interface are
;;; `define-debugging-class', `debug', `undebug' and `when-debugging'.

;;; We define CLOS classes for each debugging-class and use let CLOS's inheritance
;;; mechanism do all the work. See 'define-debugging-class for more details.

;; We also maintain a flat list of all the defined debugging classes in
;; *defined-debugging-classes* and a hierchical list in
;; *debugging-class-classification-alist*, but the latter might be obsolete although
;; it is still usefull for the menu interface.

;; Another interesting part about this is that we convert the name of the debugging
;; class into an obscure name that no one will ever use (effectively creating a new
;; namespace for debugging classes) and we use this obscure name to define the class.

;;; ---------------------------------------------------------------------------
;;; LIST OF ENABLED CLASSES

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *debug* nil
    "This will be non-nil if debugging is enabled. It will contain a list of
debugging classes that are enabled.")
  
  (defparameter *__debug* nil
    "This contains a (private) expanded list of the enabled debug classes*")
  
  (defvar *debug-doc-string*
    "Similar to trace except that it controls execution of `when-debugging' forms.
Specs should be bunch of classes \(unquoted symbols\) which have been used as the
class in a `define-debugging-class' form. Specs can also be T which means enable all
debugging forms or NIL which means disable.
Notes:
The global variable *debug* contains the latest state of debugging.")
  
  (defvar *undebug-doc-string* 
    "Similare to untrace except that it unsets debugging classes.
See `debug'."))

;;; ---------------------------------------------------------------------------
;;; FLAT LISTS OF DEFINED CLASSES  
  
(defvar *defined-debugging-classes* ()
  
  "*DEFINED-DEBUGGING-CLASSES*

This parameter contains a list of debugging classes that 
have been defined by define-debugging-class.")

;; This is probably obsolete, but might still be good for a menu interface.
(defvar *debugging-class-classification-alist* nil
  "An alist of direct subclases of debugging classes.")
  
;;;----------------------------------------------------------------------------
;;; MAPPING FROM DEBUG CLASSES TO INTERNAL CLASSES
  
;; Defining mappings between external and internal class names.
;; We do this so that we have our own namespace of debugging classes.
  
(defun make-debug-class-name (symbol)
  (form-symbol-in-package (symbol-package symbol) "%DEBUG-" symbol))

(defun internal-name->debug-name (symbol)
  (get symbol 'debug-name))

(defun (setf internal-name->debug-name) (value symbol)
  (setf (get symbol 'debug-name) value))

(defun debug-name->internal-name (symbol)
  (get symbol 'internal-debug-name))

(defun (setf debug-name->internal-name) (value symbol)
  (setf (get symbol 'internal-debug-name) value))
  
;;; ---------------------------------------------------------------------------
;;; DEFINE-DEBUGGING-CLASS

;; 'define-debugging-class' defines a class with a debug-class-enabled method. The main
;; `debug-class-enabled' for the class being defined 1) calls
;; `debug-class-enabled' with its class object as the argument. If it is not
;; enabled it 2) calls `debug-class-enabled' defined for each of its superclasses
;; with a class-prototype of the superclass.  Each of the superclasses methods
;; does both of these steps until we find one that is enabled or we bottom out.\
  
(defmacro define-debugging-class
          (class-symbol superclasses &rest class-options)
  
  "DEFINE-DEBUGGING-CLASS class-symbol ({superclass}*) ({class-option}*)
This macro defines `class-symbol' as a legal debugging class."
  `(eval-always
     (define-debugging-class-fn
       ',class-symbol ',superclasses 
       ,@(when class-options 
           `(',@class-options)))))

;;; ---------------------------------------------------------------------------

(defun define-debugging-class-fn
       (class-symbol superclasses &rest class-options
                     &aux internal-debug-class-symbol documentation)
  
  "DEFINE-DEBUGGING-CLASS-FN class-symbol ({superclass}*) ({class-option}*)
This macro defines `class-symbol' as a legal debugging class."
  
  (check-type class-symbol symbol)
  
  ;; Make a hidden symbol that we can use for the class name.
  (setf internal-debug-class-symbol (make-debug-class-name class-symbol))
  
  (when (stringp (first class-options))
    (setf documentation (pop class-options)))
  
  (flet ((delete-option (name)
           ;; inefficient, but who cares, since it's at compile time and
           ;; `class-options' will be short.
           (let ((elt (or (find name class-options)
		          (find name class-options
			        :key #'(lambda (x) (and (consp x) (first x)))))))
             (deletef elt class-options)
             elt)))
    (let ((export (delete-option :EXPORT-P)))
      (when (consp export)
	(setf export (first (rest export))))
      
      (when export (export class-symbol))
      #+MCL 
      (record-source-file class-symbol 'debug-class)
         
      ;; On the symbolic debug class name we store the internal name and...
      ;; on the internal-name-symbol we store the debug class name.
      (setf (debug-name->internal-name class-symbol) internal-debug-class-symbol
            (internal-name->debug-name internal-debug-class-symbol) class-symbol)
           
      (pushnew class-symbol *defined-debugging-classes* :test #'eq)
         
      (when documentation 
        (setf (documentation class-symbol 'debug-class) documentation))
         
      (when superclasses
        (dolist (class superclasses)
          (let ((place (assoc class *debugging-class-classification-alist*)))
            (if place
              (pushnew class-symbol (second place) :test #'eq)
              (push-acons *debugging-class-classification-alist*
                          class
                          (list (list class-symbol))))))))
         
      class-symbol))

;;; ---------------------------------------------------------------------------
;;; undefine-debugging-class
;;
;;?? Probably needs work
;;; ---------------------------------------------------------------------------

(defmacro undefine-debugging-class (class-symbol)
  "UNDEFINE-DEBUGGING-CLASS class-symbol.
This macro undefines `class-symbol' as a legal debugging class."
  `(undefine-debugging-class-fn ',class-symbol))

;;; ---------------------------------------------------------------------------

(defun undefine-debugging-class-fn (class-symbol)
  "UNDEFINE-DEBUGGING-CLASS class-symbol.
This macro undefines `class-symbol' as a legal debugging class."
  
  (check-type class-symbol symbol)
  
  ;; Make a hidden symbol that we can use for the class name.
  (let ((internal-debug-class-symbol (make-debug-class-name class-symbol)))
    (progn
      ;; On the symbolic debug class name we store the internal name and...
      (setf (debug-name->internal-name class-symbol) nil)
      ;; on the internal-name-symbol we store the debug class name.
      (setf (internal-name->debug-name internal-debug-class-symbol) nil)
      
      (eval-always
        (setf *defined-debugging-classes*
              (remove class-symbol *defined-debugging-classes* :test #'eq)))
      
      (values))))


;;;----------------------------------------------------------------------------
;;; DEBUG
  
;;; The following is necessary because we are about to define a macro called
;;; `debug,' and `debug' is a Common Lisp symbol (a quality for compiled code,
;;; like speed or space).  Lispworks detects this as a redefinition (which is
;;; wrong), and will give us an error.  At least this downgrades it to a
;;; warning.  We have to save the current value, because we want to reset it
;;; afterwards.

(#+allegro excl::without-package-locks
 #+lispworks let #+lispworks ((lw:*handle-warn-on-redefinition* :warn))
 #-(or allegro lispworks) progn   

(defmacro debug (&rest specs)
  #.(concatenate 'string *debug-doc-string*)
  `(debug-fn ,@(mapcar (lambda (spec) `',spec) specs))))

;;; ---------------------------------------------------------------------------

(defun debug-fn (&rest specs)
  (if (null specs)
    (values *debug*)
    (progn
      (dolist (class specs)
        (cond ((null class)
               (setf *debug* nil))
              ((eq class t)
               (when (null *debug*)
                 (setf *debug* *defined-debugging-classes*)))
              ((and (listp class)
                    (eq (car class) 'quote)
                    (symbolp (cadr class))
                    ;; This fixes the syntax
                    (setf class (cadr class))
                    (format *error-output* "~&A quote is not needed. Use (DEBUG ~a).~%"
                            class)
                    ;; This makes this COND clause fail, so the next one runs.
                    nil))
              ((symbolp class)
               (assert-debug-class-defined class)
               (if (listp *debug*)
                 (pushnew class *debug*)
                 (setf *debug* (list class))))
              (t
               (error "unknown debugging class: ~a" class))))
      (expand-debug-list)
      *debug*)))

;;; ---------------------------------------------------------------------------

;; Need to add some error checking in here.
(defmacro undebug (&rest specs)
  #.(concatenate 'string *undebug-doc-string*)
  `(undebug-fn ,@(mapcar (lambda (spec) `',spec) specs)))

;;; ---------------------------------------------------------------------------

(defun undebug-fn (&rest specs)
  (if (null specs)
    (values (prog1 *debug* (setf *debug* nil
                                 *__debug* nil)))
    (progn
      (mapcar #'assert-debug-class-defined specs)
      (setf *debug* (nset-difference *debug* specs))
      (expand-debug-list)
      (values *debug*))))

;;; ---------------------------------------------------------------------------

(defun expand-debug-list ()
  (setf *__debug* nil)
  (expand-debug-list-helper *debug*))
  
;;; ---------------------------------------------------------------------------

(defun expand-debug-list-helper (classes &optional classes-already-done)
  (let ((new-classes (mapcan #'(lambda (class)
                                 (copy-list (cadr (assoc class *debugging-class-classification-alist*))))
                             classes)))
    (setf *__debug* (delete-duplicates (append *debug* *__debug* new-classes)))
    (let* ((classes-done (append classes classes-already-done))
           (unique-new-classes (set-difference new-classes classes-done)))
      (when unique-new-classes
        (expand-debug-list-helper unique-new-classes classes-done)))))

;;;----------------------------------------------------------------------------

(defun assert-debug-class-defined (class-symbol)
  (check-type class-symbol symbol)
  (unless (or (eq class-symbol t) ; T as a class in when-debugging means always.
              (debugging-class-p class-symbol))
    (error "Debugging class `~S' was not defined before it was used." class-symbol)
    #+Later
    (cerror "Define ~s now and proceed. To avoid this error add a (define-debugging-class ~:*~s ()) form to your code."
	    "Debugging class `~S' was not defined before it was used."
	    class-symbol)
    
    #+Later
    (define-debugging-class-fn class-symbol)))

;;; ---------------------------------------------------------------------------
  
(defvar *compiler-disable-debugging?* nil) ; checked at compile-time, not runtime

;;; ---------------------------------------------------------------------------

(defmacro debugging-p (&optional (class-symbol t) eval)
  "Returns T when the given debugging class is enabled.  If the T is given as
a class returns T if any debugging class is enabled. If eval is true, then the 
symbol is evaluated before testing for the debug status. See debugging-fn-p too."
  (cond (eval
	 `(debugging-p-fn ,class-symbol))
	(t
	 (restart-case
	     (assert-debug-class-defined class-symbol)
	   (nil () :report "Define the class and proceed."
		(eval `(define-debugging-class ,class-symbol ()))))
	 ;; At the present time these forms will always be inserted into into code and
	 ;; therefore will always test the current debugging state.  If effiency is a
	 ;; big concern we might want to conditionally insert these forms into the
	 ;; code using the check below:
	 ;;  (when (or (eq *debug* t)
	 ;;            (and (listp *debug*)
	 ;;                 (member ',class-symbol *debug*))) ...)
         (ecase *compiler-disable-debugging?*
           ((t) 
            (when (eq *compiler-disable-debugging?* :print)
              (format *standard-output* "~&; Ignoring debug statment for ~a in ~a" 
                      class-symbol
                      *loading-file-source-file*))
            nil)
           ((nil :print) 
            #+MCL
            (when (eq *compiler-disable-debugging?* :print)
              (format *standard-output* "~&; Compiling debug statment for ~a in ~a" 
                      class-symbol
                      *loading-file-source-file*))
            `(when (member ',class-symbol *__debug*) 
               (values t)))))))

;;; ---------------------------------------------------------------------------

(defun debugging-p-fn (debug-name)
  "Returns T when the given debugging class is enabled.  If the T is given as
a class returns T if any debugging class is enabled."
  (when (member debug-name *__debug*) 
    (values t)))

;;; ---------------------------------------------------------------------------

(defmacro when-debugging (class-symbol &body body)
  "Define forms to execute when the specific debug class is enabled.
See `debug' for details. The local macro `debug-format' can be used
in the body of this form."
  (assert class-symbol nil "When-debugging called with no debugging class!")

  (every #'assert-debug-class-defined (ensure-list class-symbol))
  (setf class-symbol (mapcar #'debug-name->internal-name (ensure-list class-symbol)))
  (assert (every (lambda (name)
                   (internal-name->debug-name name))
                 (ensure-list class-symbol))
          nil "When-debugging was called with a symbol that has no mapping ~
               from internal-name to external name!")
  `(progn
     (when (or ,@(mapcar (lambda (name)
                           `(debugging-p-fn (internal-name->debug-name ',name)))
                         class-symbol))
       (macrolet ((debug-format (format-string &rest format-args)
                    `(debugging-format-helper
                      ',',class-symbol ,format-string . ,format-args)))
         ,@body))))

;;; ---------------------------------------------------------------------------

(defmacro debug-format (&rest args)
  "Only to be used in the context of `when-debugging'; Use
`when-debugging-format' instead."
  (declare (ignore args))
  (error "Only to be used in the context of `when-debugging'; Use `when-debugging-format' instead."))
  
;;;----------------------------------------------------------------------------
  
(defvar *debug-output* *debug-io*)
  
(defmacro when-debugging-format (class-symbol format-string &rest format-args)
  "Format output to *debug-output* when the specified debugging class is
enabled."
  `(when-debugging-format-if ,class-symbol T ,format-string . ,format-args))

;;; ---------------------------------------------------------------------------

(defmacro when-debugging-format-if
          (class-symbol conditional format-string &rest format-args)
  "Format output to *debug-output* when the specified debugging class is
enabled and the predicate evaluates to a non-nil value."
  `(when-debugging ,class-symbol
     (when ,conditional
       (debugging-format-helper 
        ',(mapcar #'debug-name->internal-name (ensure-list class-symbol))
        ,format-string . ,format-args))))

;;; ---------------------------------------------------------------------------

(defmacro when-debugging-spy (class-symbol &rest spy-args)
  "Execute `spy' when the specified debugging class is enabled."
  `(when-debugging ,class-symbol
     (spy . ,spy-args)))

#+NEW-POSSIBLY-BETTER-BUT-NOT-QUITE-WHAT-WE-WANT-VERSION
(defmacro when-debugging-spy (class-symbol &rest spy-args)
  "Execute `spy' when the specified debugging class is enabled."
  ;; This bit of hair deals with the fact that the first form of spy can be
  ;; a list of keyword options - Westy
  (let ((%%spy-forms-without-options%% (if (and (consp (first spy-args))
                                                (keywordp (first (first spy-args))))
                                         (rest spy-args)
                                         spy-args)))
    `(if-debugging ,class-symbol
       (spy . ,spy-args) ; XXX style is obtuse
       (progn .  ,%%spy-forms-without-options%%))))

(defparameter *show-function-in-when-debugging-format?* nil)
(defparameter *show-class-in-when-debugging-format?* nil)
  
  
(defun debugging-format-helper (class-symbol format-string &rest format-args)
  (format *debug-output*
          "~&~:[~*~;~s; ~]~:[~*~;<~a>; ~]~?~%"
          *show-function-in-when-debugging-format?*
          (current-function)
          *show-class-in-when-debugging-format?*
          (let ((symbols (mapcar #'internal-name->debug-name (ensure-list class-symbol))))
            (if (length-1-list-p symbols) (first symbols) symbols))
          format-string format-args))

;;; Some of these functions want to report what function they are called from.
;;; With `error' it's trivial to know what function it's in, since you end up in
;;; the debugger, but with warnings and such, it's harder.

(defun current-function ()
  "Returns the name of the function for the current stack frame; useful in error
messages."
  #+Explorer
  (let* ((sg (ticl::process-stack-group w:current-process))
         (name (eh::function-name
                (eh::rp-function-word
                 (eh::sg-regular-pdl sg)
                 (eh::sg-next-frame sg (eh::sg-innermost-frame sg))))))
    (values (cond ((and (consp name) (eq (first name) :METHOD))
                   (third name))
                  (t name))))
  #-Explorer
  "Unknown Function")

(defmacro with-debugging ((&rest tags) &body body)
  `(unwind-protect
     (progn
       (debug ,@tags)
       ,@body)
     (undebug ,@tags)))
  
;;;----------------------------------------------------------------------------

(defun all-debug-classes-below-class (class-name)
  (labels ((helper (class-name)
             (let ((kids (second (assoc class-name *debugging-class-classification-alist*))))
               (when kids
                 (append kids (mapappend #'helper kids))))))
    (nreverse (helper class-name))))

;;; ---------------------------------------------------------------------------

(defun debug-classes-directly-below-class-that-have-subclasses (class-name &aux result)
  (dolist (d (second (assoc class-name *debugging-class-classification-alist*)))
    (when (assoc d *debugging-class-classification-alist*)
      (push d result)))
  (values result))

#+MCL
(defun print-short-symbol-to-string (symbol)
  (let ((name (symbol-name symbol))
        (package (symbol-package symbol)))
    (with-output-to-string (stream)
      (multiple-value-bind (s flag)
                           (find-symbol name *package*)
        (unless (and flag (eq s symbol))
          (multiple-value-setq (s flag)
            (find-symbol name package))
          (write-string (ccl::shortest-package-nickname package) stream)
          (stream-tyo stream #\:)
          (unless (eq flag ':external)
            (stream-tyo stream #\:))))
      (write-string name stream))))

;;; ---------------------------------------------------------------------------
;;; debugging-apropos
;;; ---------------------------------------------------------------------------

(defun debugging-apropos (name)
  "Like APROPOS, but only returns debugging-classes.  Returns all debugging
   classes whose name has a substring equal to NAME.  NAME may be anything
   acceptable to the STRING function."
  (let ((substring (string name)))
    (remove-if-not
     (lambda (class-name)
       (search substring 
               (symbol-name class-name)
               :test #'string-equal))
     *defined-debugging-classes*)))

;;; ---------------------------------------------------------------------------

(defun debugging-class-p (class-name)
  (not (null (member class-name *defined-debugging-classes* :test #'eq))))

;;; ---------------------------------------------------------------------------

(defmacro with-tracing (options &body body)
  (let* ((key-pos (position-if #'keywordp options))
         (tracers (if key-pos (subseq options 0 key-pos) options))
         (keys (when key-pos (subseq options key-pos))))
    (destructuring-bind (&key replace? stream) keys
      (let ((stream-code
             (cond ((or (typep stream 'pathname)
                        (typep stream 'string))
                    `((with-open-file (*trace-output* ,stream
                                                      :if-exists ,(if replace?
                                                                    :supersede
                                                                    :error)
                                                      :if-does-not-exist :create 
                                                      :direction :output)
                        ,@body)))
                   ((null stream)
                    body)
                   (t
                    (error "Don't know how to handle ~A" stream)))))
        `(unwind-protect
           (progn
             (trace ,@tracers)
             ,@stream-code)
           (untrace ,@tracers))))))

;;; ---------------------------------------------------------------------------
;;; what not
;;; ---------------------------------------------------------------------------

#| TESTS

(define-debugging-class foo-1 ())
(define-debugging-class foo-2 ())
(define-debugging-class foo-3 (foo-1))
(define-debugging-class foo-4 (foo-3))

(defun foo ()
  (let ((result))
    (when-debugging foo-1 (push :foo-1 result))
    (when-debugging foo-2 (push :foo-2 result))
    (when-debugging foo-3 (push :foo-3 result))
    (when-debugging foo-4 (push :foo-4 result))
    (nreverse result)))

(foo)

(debug foo-1)

(deftestsuite test-debugging () ())
(addtest (test-debugging)
  (undebug)
  (ensure (eq (foo) nil))
  (debug foo-1)
  (ensure (equal (foo) '(:foo-1 :foo-3 :foo-4))))
(addtest (test-debugging)
  (undebug)
  (ensure (eq (foo) nil))
  (debug foo-2)
  (ensure (equal (foo) '(:foo-2))))
(addtest (test-debugging)
  (undebug)
  (ensure (eq (foo) nil))
  (debug foo-3)
  (ensure (equal (foo) '(:foo-3 :foo-4))))
(addtest (test-debugging)
  (undebug)
  (ensure (eq (foo) nil))
  (debug foo-4)
  (ensure (equal (foo) '(:foo-4))))  
|#

;;; ***************************************************************************
;;; *                              End of File                                *
;;; *************************************************************************** 