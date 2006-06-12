(in-package #:metatilities)

(defvar *automatic-slot-accessors?* nil)
(defvar *automatic-slot-initargs?* nil)
(defvar *clos-slot-options* 
  '(:initform :initarg :reader :writer 
    :accessor :documentation :type
    :allocation))
(defvar *prune-unknown-slot-options* nil)

(defun parse-brief-slot
       (slot &optional
	     (automatic-accessors? *automatic-slot-accessors?*)
	     (automatic-initargs? *automatic-slot-initargs?*)
	     name-prefix
             name-postfix
             (name-separator "-")
             (additional-options nil))
  "Returns a verbose-style slot specification given a brief style, consisting of
a single symbol, the name of the slot, or a list of the slot name, optional
initform, optional symbol specifying whether there is an initarg, reader, or
accessor, and optional documentation string.  The specification of initarg,
reader and accessor is done by the letters I, R and A, respectively; to specify
none of those, give a symbol containing none of those letters, such as the
symbol *.  This function is used in the macro `defclass-brief,' but has been
broken out as a function in its own right for those writing variants on the
`defclass' macro.  If a verbose-style slot specification is given, it is
returned unchanged.

If `automatic-accessors?  is true, an accessor is defined, whether A is
specified or not _unless_ R is specified.  If `automatic-initargs?  is true, 
an initarg is defined whether I is specified or not.  If `name-prefix' or
`name-postfix' is specified, the accessor name has that prepended or appended,
with name-separator, and the slot name in the middle. 

All other CLOS slot options are processed normally."
  
  ;; check types
  (etypecase slot
    (symbol (setf slot (list slot)))
    (list nil))
  
  (let* ((name (pop slot))
	 (new-slot (list name))
         (done-initform? nil)
         (done-spec? nil)
         (done-documentation? nil)
         (reader-added? nil)
         (writer-added? nil)
         (accessor-added? nil)
         (initargs-added? nil)
         (all-allowed-options (if additional-options
                                (append *clos-slot-options* additional-options)
                                *clos-slot-options*)))
    (flet ((make-conc-name ()
             (let (names)
               (when name-postfix
                 (push name-postfix names)
                 (push name-separator names))
               (push name names)
               (when name-prefix
                 (push name-separator names)
                 (push name-prefix names))
               (apply 'form-symbol names)))

           (add-option (option argument)
             (push option new-slot)
             (push argument new-slot))
           
           ;; Remove duplicate options before returning the slot spec.
           (finish-new-slot (slot)
             ;; XXX This code is overly loopy and opaque ---L
             (destructuring-bind (slot-name &rest options) slot
               (let ((opts (make-hash-table)))
                 (loop for (key val . d) = options then d
                       while key
                       doing (pushnew val (gethash key opts nil) :test #'equal))
                 (loop for key being each hash-key of opts using (hash-value vals)
                       nconc (mapcan #'(lambda (x) (list key x)) vals) into spec
                       finally (return (cons slot-name spec)))))))

      (setf accessor-added? (find :accessor slot)
            done-initform? (find :initform slot))
      
      (do* ((items slot (rest items))
            (item (first items) (first items))
            (process-item? t t)
            (allowed-item? (member item all-allowed-options) 
                           (member item all-allowed-options)))
           ((null items) nil)
        (unless done-initform?
          (setf done-initform? t)
          (unless allowed-item?
            (setf  process-item? nil)
            (unless (eq item :unbound)
              (push :initform new-slot)
              (push item new-slot))))
        
        ;;?? maybe want (not (slot-reader-writer-initarg-accessor-spec-p ?!))
        (when (and process-item? (not (keywordp item)))
          (unless (or done-spec? (not (symbolp item)) allowed-item?)
            (setf done-spec? t)
            (setf process-item? nil)
            ;; If you've got an A, who cares about R
            (when (find #\A (string item) :test #'char-equal)
              (setf accessor-added? t)
              (add-option :accessor (make-conc-name)))
            (when (and (not accessor-added?) (find #\R (string item) :test #'char-equal))
              (setf reader-added? t)
              (add-option :reader (make-conc-name)))
            (when (and (not accessor-added?) (find #\W (string item) :test #'char-equal))
              (setf writer-added? t)
              (add-option :writer (list 'setf (make-conc-name))))
            (when (find #\I (string item) :test #'char-equal)
              (setf initargs-added? t)
              (add-option :initarg (intern (string name) (find-package :keyword))))))
        
        (when process-item?
          (unless (or done-documentation? (not (stringp item)))
            (setf done-documentation? t
                  process-item? nil)
            (push :documentation new-slot)
            (push item new-slot)
            ))
        
        (when process-item?
          (when (or (not *prune-unknown-slot-options*) allowed-item?)
            (push item new-slot)
            (pop items)
            ;(assert items)
            (push (first items) new-slot)))
        
        ;(spy new-slot)
        )
      
      (when (and automatic-initargs? (not initargs-added?))
        (add-option :initarg (intern (string name) (find-package :keyword))))
      
      (when (and automatic-accessors? (and (not accessor-added?) 
                                           (not reader-added?)
                                           (not writer-added?)))
        (add-option :accessor (make-conc-name)))
      
      ;; finish-new-slot cleans up duplicates
      (finish-new-slot (nreverse new-slot)))))

(defmacro defclass-brief (name superclasses slots &rest class-options)
  "A macro with simpler syntax than `defclass' that allows some
briefer ways of expressing things.  The syntax is more like `defstruct.' A
documentation string is optional.  Each slot is expressed as either a bare
symbol, or a list of the name of the slot, its initial value, a symbol with the
letters I, R, A, standing for :initarg, :reader and :accessor, and a
documentation string.  The symbol, whose package is unimportant, determines the
generation of reader, accessor and so forth; the names are the same as the slot
name. All other CLOS options are processed normally.

In addition, three new class options are defined.
  :AUTOMATIC-ACCESSORS means that an accessor is defined for every slot
  :AUTOMATIC-INITARGS means that an initarg is defined for every slot
  (:NAME-PREFIX <symbol> <separator>) (:NAME-POSTFIX <separator> <symbol>)
prepends or appends `symbol' with `separator'
to each slot accessor. The default symbol is the class name and the default
separator is the hypen, in which case the wrapping parentheses are optional."
  (let ((docstring (if (stringp slots)
		       (prog1 slots (setf slots (pop class-options)))
		       nil)))
    (flet ((delete-option (name)
	     ;; inefficient, but who cares, since it's at compile time and
	     ;; `options' will be short.
	     (let ((elt (or (find name class-options)
			    (find name class-options
				  :key #'(lambda (x) (and (consp x) (first x)))))))
	       (setf class-options (delete elt class-options))
	       elt)))
      (let ((accessors? (or (delete-option :automatic-accessors)
			   *automatic-slot-accessors?*))
	    (initargs? (or (delete-option :automatic-initargs)
			   *automatic-slot-initargs?*))
            (name-prefix (delete-option :name-prefix))
            (name-postfix (delete-option :name-postfix))
            name-separator)
        (macrolet ((process-name-pre/post-fix (name-pre/post-fix name-separator)
                     `(when ,name-pre/post-fix
                       (setf (values ,name-pre/post-fix ,name-separator)
                             (cond ((cddr ,name-pre/post-fix)
                                    (values (cadr ,name-pre/post-fix) (caddr ,name-pre/post-fix)))
                                   ((cdr ,name-pre/post-fix)
                                    (values (cadr ,name-pre/post-fix) "-"))
                                   (t
                                    (values name "-"))))
                       (setf ,name-pre/post-fix (string-upcase ,name-pre/post-fix)))))
          (process-name-pre/post-fix name-prefix name-separator)
          (process-name-pre/post-fix name-postfix name-separator))
	`(progn
           (defclass ,name ,superclasses
	     ,(mapcar #'(lambda (s) (parse-brief-slot
                                     s accessors? initargs? 
                                     name-prefix name-postfix name-separator))
		      slots)
	     ,@(when docstring
	         `((:documentation ,docstring)))
	     . ,class-options)
             
           #+No
           (defmethod reinitialize-instance :after ((object ,name) &key)
             ,@(loop for s in slots 
                     for parse = (parse-brief-slot s)
                     when (find :initform parse) collect
                     `(setf (slot-value object ',(first (ensure-list s))) 
                            ,(getf (rest parse) :initform))))
           
           (values ',name))))))

;;; ---------------------------------------------------------------------------

(defparameter *defclass-copy-condition-function* #'warn)
(defvar *defclass-generate-make-load-form* nil)

;;; ---------------------------------------------------------------------------

(defclass-property defclass*-superclasses)

;;; ---------------------------------------------------------------------------

(defun class-copyable-p (class-name)
  ;; (spy class-name)
  (or (eq class-name 'copyable-mixin)
      (some #'class-copyable-p
            (defclass*-superclasses class-name))))

;;; ---------------------------------------------------------------------------

(defmacro defclass* (name superclasses slots &rest class-options)
  "Like 'defclass-brief' but also provides the :MAKE-LOAD-FORM-P, :EXPORT-P, 
:EXPORT-SLOTS, :NO-COPY, :COPY-SLOTS and :COPY-SET-SLOTS options."
  (let* ((docstring (if (stringp slots)
                      (prog1 slots (setf slots (pop class-options)))
                      nil))
         (slot-names (mapcar #'(lambda (s)
                                 (etypecase s (symbol s) (cons (first s))))
                             slots)))
    (flet ((delete-option (name &key error-if-atom (default-value nil))
	     ;; inefficient but who cares since it's at compile time and
	     ;; `options' will be short.
	     (let ((elt (or (find name class-options)
			    (find name class-options
				  :key #'(lambda (x) (and (consp x) (first x)))))))
               (setf class-options (delete elt class-options))
               (cond ((null elt) (values nil nil))
                     ((consp elt) (values (rest elt) t))
                     (error-if-atom (error "~a option should be a list" name))
                     (t (values default-value t)))))
           (defclass*-problem (message &rest args)
             (funcall *defclass-copy-condition-function*
                      (format nil "While defining class ~S,~%~A" 
                              name
                              (apply #'format nil message args))))
           (just-the-names (list)
             (mapcar (lambda (elt) (first (ensure-list elt))) list)))
      (let* ((copy-slots (delete-option :copy-slots :default-value slot-names))
             (copy-set-slots (delete-option :copy-set-slots :default-value slot-names))
             (copy-cond-slots (delete-option :copy-cond-slots :error-if-atom t
                                              :default-value slot-names))
             (no-copy (delete-option :no-copy :error-if-atom t :default-value nil))
             (export-p (delete-option :export-p :error-if-atom t))
             (export-slots (delete-option :export-slots :default-value slot-names))
             (make-load-form-p (delete-option :make-load-form-p
                                              :error-if-atom t
                                              #+NotYet :default-value #+NotYet *defclass-generate-make-load-form*))
             (copyable? (some #'class-copyable-p superclasses)))

        (flet ((format-list (list &optional (end-with "."))
                 (apply #'format nil
                        (concatenate 'string "~#[~;~A" end-with "~:;~@{~#[~;~]~A~^, ~}~]")
                        list)))
          (let ((temp nil))
            ;; make sure we can export things
            (setf temp (set-difference export-slots slot-names))
            (when temp                 
              (defclass*-problem "slots ~A are exported but not defined." 
                (format-list temp))
              ;; don't allow it
              (setf export-slots (intersection export-slots slot-names)))
            
            ;;; make sure we can copy things
            ;; can't be :no-copy and specify copy options
            (when (and no-copy (or copy-slots copy-set-slots copy-cond-slots))
              (defclass*-problem "cannot specify :no-copy copy options simultaneously."))
            
            (when no-copy
              (setf copy-slots nil copy-set-slots nil copy-cond-slots nil))
            
            ;; if we specify any copy options, then we must be copyable
            (when (and (or copy-slots copy-set-slots copy-cond-slots)
                       (not copyable?))
              
              ;;?? Gary King 2004-03-02: write copying code even if it is not 
              ;; a copyable-mixin
              #+Ignore
              (setf copy-slots nil copy-set-slots nil copy-cond-slots nil)
              (defclass*-problem "slot copying are specified but ~A does not inherit from copyable-mixin"
                name))
            
            (when copyable?
              ;; if we're copyable then all our superclasses should be too
              (unless (every #'class-copyable-p superclasses)
                
                (defclass*-problem "~A is copyable but some of its superclasses ~
                                    (~A) are not."
                  name
                  (format-list (remove-if #'class-copyable-p superclasses) "")))
              
              (let ((copy-slot-names (nconc (just-the-names copy-slots)
                                            (just-the-names copy-set-slots)
                                            (just-the-names copy-cond-slots))))
                (when copy-slot-names
                  ;; only slots of this class can be copied
                  ;;?? should we allow superclasses too?
                  (setf temp (set-difference copy-slot-names slot-names))
                  (when temp
                    (defclass*-problem "slots ~A have copying options but they are not defined."
                      (format-list temp)))
                  
                  ;; each slot must have only one kind of copy specified
                  (setf temp (loop for elt in copy-slot-names
                                   when (> (count elt copy-slot-names) 1) collect
                                   elt))
                  (when temp
                    (defclass*-problem "slots ~A specify multiple copying mechanisms"
                      (format-list temp))))
                
                ;; if we are copyable, we must specify options for all slots
                (setf temp (set-difference slot-names copy-slot-names))
                (when temp
                  (defclass*-problem "slots ~A do not specify a copying mechanism"
                    (format-list temp)))))))
        
        `(eval-always  ; why does this need to be eval-always? ---L
           (progn
             ,@(when export-p
                 `((export ',name)))
             ,@(when export-slots
                 `((export '(,@export-slots))))
             (defclass-brief ,name ,superclasses ,slots
               ,@(when docstring
                   `((:documentation ,docstring)))
               . ,class-options)
             
             ;;?? Gary King 2004-03-02: write copying code even if it is not 
             ;; a copyable-mixin
             ,@(when (or copy-slots copy-set-slots)
                 `((duplicator-methods 
                    (,name)
                    ,slot-names
                    (,@(when copy-slots `((duplicate-slots ,@copy-slots)))
                     ,@(when copy-cond-slots 
                         (mapcar #'(lambda (pair)
                                     `(duplicate-cond-slots ,@pair))
                                 copy-cond-slots))
                     ,@(when copy-set-slots 
                         (mapcar #'(lambda (pair)
                                     ;; Allows this syntax (:copy-set-slots a1 b2)
                                     (unless (consp pair)
                                       (setf pair (list pair pair)))
                                     `(duplicate-set ,@pair))
                                 copy-set-slots))))))
             (setf (defclass*-superclasses ',name) ',superclasses)
             ,@(when make-load-form-p
                 `((make-load-form* ,name)))
             ',name))))))

;;; ---------------------------------------------------------------------------

(defparameter *defcondition-options*
  '(((:automatic-accessors :generate-accessors) t nil)
    ((:automatic-initargs :generate-initargs) t nil)
    ((:export-p :export?) t nil)
    ((:export-slots-p :export-slots?) t nil))
  "Extra options to defcondition macro. Format is a list of sub-lists. 
Each sublist should be of length three and consists of a list of option 
synonyms, the default value for the option [currently ignored], and whether
or not to signal an error if this option is used as an atom [currently 
ignored]")

;;; ---------------------------------------------------------------------------

(defmacro defcondition (name supers slots &rest options)
  "Defcondition is a handy shortcut for defining Common Lisp conditions. It
supports all of #[H][define-condition]'s options and more."

  (let ((elt nil)
        (extra-options nil))
    (loop for (option-list default error-if-atom?) in *defcondition-options* do
          (progn default error-if-atom?)        ;?? For now
          (setf elt nil)
          (when (some (lambda (name)
                        (setf elt (find name options
                                        :key (lambda (x) 
                                               (or (and (consp x) (first x)) x)))))
                      option-list)
            (setf elt (first (ensure-list elt))
                  options (delete elt options
                                  :key (lambda (x) 
                                         (or (and (consp x) (first x)) x))))
            (pushnew (first option-list) extra-options)))
    (let ((slot-specs 
           (mapcar (lambda (s)
                     (parse-brief-slot s 
                                       (member :automatic-accessors extra-options)
                                       (member :automatic-initargs extra-options)
                                       nil nil nil))
                   slots)))
      `(progn
         ,@(when (or (member :export-p extra-options)
                     (member :export-slots-p extra-options))
             `((eval-when (:compile-toplevel :load-toplevel :execute)
                 ,@(when (member :export-p extra-options)
                     `((export ',name)))
                 ,@(when (member :export-slots-p extra-options)
                     `((export ',(mapcar #'first slot-specs)))))))
         (define-condition 
           ,name ,supers ,slot-specs
           ,@options)))))
