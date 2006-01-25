;;;-*- Mode: Lisp; Package: MACH -*-
;;;
;;;
;;; File:	framework.lisp
;;; Author:	Takehiko Abe <keke@gol.com>
;;; Date:	2004-01-30

;;;
;;; Defines mach:define-entry-point for framework library access.
;;;

;;; Hitory:
;;; 2004-02-05 renamed create-framework-url to create-framework-cfurl
;;; 2004-02-05 added compiler-macro to ff-call-entry-point for speed.
;;; 2004-01-30 first public release


(in-package :mach)

(require "CF-BUNDLE")                   ; CFBundle.lisp

;; from ccl::find-arg-mactype
;; giving error instead of turning record into :pointer
(defun find-arg-mactype (mactype &optional (errorp t) &aux rdesc (type mactype))
  (or (progn
        (setq rdesc (find-record-descriptor mactype nil))
        (cond ((or rdesc (ccl::array-mactype-p mactype))
               (case (ccl::record-field-length mactype)
                 ((1 2)
                  (setq mactype :record-integer))
                 ((3 4)
                  (setq mactype :record-longint))
                 (otherwise
                  (when rdesc           ; what if it is not rdesc?
                    ;; 2004-01-27 keke
                    (error "Sorry. ~S is too big." type)
                    ;; I believe replacing record with :pointer was valid only in the old
                    ;; Pascal days. Correct me if I'm wrong.
                    #+ignore
                    (setq mactype :pointer))))))
        (or (find-mactype mactype nil)
            (and errorp
                 (error "Unrecognized Macintosh argument type ~s" type))))
      (and (symbolp mactype)
           (not (keywordp mactype))
           (find-arg-mactype (ccl::make-keyword mactype) nil))))

(defmacro define-entry-point ((name framework-name &optional (framework-type :system))
                                        args &optional return)
  (let ((sym (intern (string-upcase name) *package*)))
    (when return
      (find-arg-mactype return))        ; was ccl::find-arg-mactype
    ;; check if arg is symbol [from ccl::process-trap-args]
    (mapc #'(lambda (arg)
              (unless (symbolp (car arg))
                (ccl::%badarg (car arg) 'symbol))
              ;; check mactype too
              (find-arg-mactype (cadr arg)))
          args)
    (record-source-file sym 'function)
    (let ((env-var (gensym))
          (lib (get-framework-descriptor (eval framework-type) (eval framework-name))))
      `(progn
         (export ',sym (symbol-package ',sym))          ; should be able to control it. FIXIT
         (defmacro ,sym (&environment ,env-var ,@(mapcar 'car args))
           (expand-macho-trap ',sym (list ,@(mapcar 'car args)) ',args ',return  t ,name
                              ,lib ,env-var))))))


;;;
;;; framework-descriptor
;;;

(defstruct framework-descriptor
  bundle                                ; pointer to framework bundle
  type                                  ; :system :full-path :ccl
  (name nil :type string)               ; must be a string
  (eps (make-hash-table :test #'equal))         ; stores entry-points
  )

(defmethod print-object ((desc framework-descriptor) stream)
  (print-unreadable-object (desc stream :identity t)
    (format stream "FRAMEWORK ~S"
            (framework-descriptor-name desc))))


(defgeneric create-framework-cfurl (framework-type framework-name)
  (:documentation 
   "returns CFURL to framework bundle whose name is
framework-name."))

(defmethod create-framework-cfurl ((framework-type (eql :system)) framework-name)
  (let ((url (bundle-sys:system-frameworks-url)))
    (with-cfstrs ((cfstr framework-name))
      (#_CFURLCreateCopyAppendingPathComponent
       (%null-ptr)
       url
       cfstr
       #$false))))


(defmethod create-framework-cfurl ((framework-type (eql :full-path)) framework-name)
  (with-cfstrs ((path (mac-namestring framework-name)))
    (#_CFURLCreateWithFileSystemPath 
     (%null-ptr)
     path
     #$kCFURLHFSPathStyle
     #$false)))


(ccl::defloadvar *ccl-framework-url* nil)

(defun ccl-framework-url ()
  (or *ccl-framework-url*
      (setq *ccl-framework-url*
            (#_cfbundlecopybundleurl (#_CFbundlegetmainbundle)))))

(defmethod create-framework-cfurl ((framework-type (eql :ccl)) framework-name)
  (with-cfstrs ((cfstr framework-name))
    (#_CFURLCreateCopyAppendingPathComponent
     (%null-ptr)
     (ccl-framework-url)
     cfstr
     #$false)))


(defun load-framework-bundle (desc)
  (let ((cfurl (create-framework-cfurl (framework-descriptor-type desc) 
                                       (framework-descriptor-name desc))))
    (unwind-protect
      (bundle-sys:load-framework-bundle-from-cfurl cfurl)
      (#_cfrelease cfurl))))

(defvar *frameworks* nil)

;; making this to be a method is a mistake. FIXIT
(defmethod find-framework-descriptor (type name)
  (find-if #'(lambda (desc)
               (and (eq (framework-descriptor-type desc) type)
                    (equal (framework-descriptor-name desc) name)))
           *frameworks*))

;; making this to be a method is a mistake. FIXIT
(defmethod (setf find-framework-descriptor) (new-desc type name)
  (declare (ignore type name))
  (push new-desc *frameworks*)
  new-desc)

(defun get-framework-descriptor (type name)
  (let ((desc (find-framework-descriptor type name)))
    (if (and desc (framework-descriptor-bundle desc))
      desc
      (progn
        (unless desc
          (setq desc (make-framework-descriptor :type type :name name))
          ; (push desc *frameworks*)
          ; FIXIT
          (setf (find-framework-descriptor type name) desc))
        (setf (framework-descriptor-bundle desc)
              (load-framework-bundle desc))
        desc))))

;;;
;;; entry-point
;;;

(defstruct entry-point
  address
  name
  framework)

(defmethod print-object ((ep entry-point) stream)
  (print-unreadable-object (ep stream :identity t)
    (format stream "ENTRY-POINT ~A"
            (entry-point-name ep))))

(defun get-framework-entry-point (name framework)
  (or (and framework (gethash name (framework-descriptor-eps framework)))
      (progn
        (unless framework               ; nil when called by #~ reader-macro
          ;; FIXIT : should search for other frameworks too.
          (setq framework (get-framework-descriptor :system "ApplicationServices.framework")))
        (setf (gethash name (framework-descriptor-eps framework))
              (make-entry-point 
               :name name
               :framework framework
               :address (bundle-sys:lookup-function-in-framework
                         name
                         (framework-descriptor-bundle framework)))))))

;; taken from ccl::expand-ff-trap
;; all errcheck codes are removed.
(defun expand-macho-trap (name passed-args inline-args return-type &optional can-errcheck? string library env)
  (declare (ignore can-errcheck?))
  (let ((passed-length (length passed-args))
        (inline-length (length inline-args)))
    (declare (fixnum passed-length inline-length))
    (when (> passed-length inline-length)
      (error "Too many args in trap call:~%~a" `(,name ,@passed-args)))
    (when (< passed-length inline-length)
      (error "Too few args in trap call:~%~a" `(,name ,@passed-args))))
  (let* ((constants-checked
          (mapcar #'(lambda (arg trap-arg &aux ct-check)
                      (if (and (constantp arg)          ; env?
                               (setq ct-check
                                     (ccl::mactype-ct-type-check
                                      (find-arg-mactype (cadr trap-arg)))))     ; was ccl::find-arg-mactype
                        (if (funcall ct-check (eval arg))
                          t
                          (error "Argument ~s (~s) to trap ~a is not of type ~s"
                                 arg (car trap-arg) name (cadr trap-arg)))
                        nil))
                  passed-args inline-args))
         (arg-vars (mapcar #'(lambda (x) (declare (ignore x)) (gensym)) passed-args))
         ;; runtime type check
         (rt-type-check-code (ccl::expand-trap-rt-type-check constants-checked
                                                             (mapcar #'(lambda (var trap-arg)
                                                                         (cons var (cadr trap-arg)))
                                                                     arg-vars inline-args)
                                                             env))
         (inside-args (if rt-type-check-code arg-vars passed-args))
         (type-coerced-args (ccl::ff-type-coerced-args inside-args inline-args t))
         
         (form `(ff-call-entry-point ,(get-framework-entry-point string library)
                                     ,@(ccl::ppc-ff-keywordized-arglist type-coerced-args inline-args)
                                     ,(if return-type 
                                        (ccl::mactype=>ppc-ff-call-type (find-mactype return-type))
                                        :void))))
    (when return-type
      (setq form (ccl::coerced-c-result form return-type t)))
    (when rt-type-check-code
      (setq form `(let ,(mapcar 'list arg-vars passed-args)
                    (declare (dynamic-extent ,@arg-vars))
                    ,@rt-type-check-code ,form)))
    form))


;; 2004-02-06 added compiler-macro
;; FIXIT too slow?
(defun ff-call-entry-point (ep &rest args)
  (declare (dynamic-extent args))
  (let ((address (entry-point-address ep)))
    ;; ep's address may have been cleared by clear-framework-caches
    (unless address
      (let ((lib (entry-point-framework ep)))
        (unless (framework-descriptor-bundle lib)
          (setf (framework-descriptor-bundle lib)
                (load-framework-bundle lib)))
        (setq address
              (setf (entry-point-address ep)
                    (bundle-sys:lookup-function-in-framework 
                     (entry-point-name ep)
                     (framework-descriptor-bundle lib))))))
    (apply #'ccl::ppc-ff-call address args)))

;; eliminated the APPLY above
(define-compiler-macro ff-call-entry-point (ep &rest args)
  (let ((address (gensym "address")))
    `(let ((,address (entry-point-address ,ep)))
       ;; ep's address may have been cleared by clear-framework-caches
       (unless ,address
         (let ((lib (entry-point-framework ,ep)))
           (unless (framework-descriptor-bundle lib)
             (setf (framework-descriptor-bundle lib)
                   (load-framework-bundle lib)))
           (setq ,address
                 (setf (entry-point-address ,ep)
                       (bundle-sys:lookup-function-in-framework 
                        (entry-point-name ,ep)
                        (framework-descriptor-bundle lib))))))
       (ccl::ppc-ff-call ,address ,@args))))



;;;
;;; clear-framework-caches for ccl::save-image
;;;

(defun clear-framework-caches ()
  (when *frameworks*
    (dolist (desc *frameworks*)
      (setf (framework-descriptor-bundle desc) nil)
      (maphash #'(lambda (key ep)
                   (declare (ignore key))
                   (setf (entry-point-address ep) nil))
               (framework-descriptor-eps desc)))))

; (clear-framework-caches)

;; ccl::kill-lisp-pointers is called by ccl::save-image
(advise ccl::kill-lisp-pointers
        (clear-framework-caches)
        :when :after
        :name :clear-framework-caches)


;;;
;;; MAKE-LOAD-FORM
;;;

(defmethod make-load-form ((desc framework-descriptor) &optional env)
  (declare (ignore env))
  `(get-framework-descriptor ,(framework-descriptor-type desc)
                             ,(framework-descriptor-name desc)))

(defmethod make-load-form ((ep entry-point) &optional env)
  (declare (ignore env))
  `(get-framework-entry-point ,(entry-point-name ep)
                              ,(entry-point-framework ep)))


;;;
;;; lookup-framework-symbol
;;;


(defun lookup-framework-symbol (symbol-name framework-name 
                                            &optional (framework-type :system) (dereference-p t))
  (let ((addr (bundle-sys:lookup-symbol-in-framework
               symbol-name
               (framework-descriptor-bundle
                (get-framework-descriptor framework-type framework-name)))))
    (if dereference-p
      (%get-ptr addr)
      addr)))



#|
example:

(mach:define-entry-point ("SetWindowAlpha" "Carbon.framework")
  ((inWindow :windowref)
   (inAlpha :single-float))
  :osstatus)

(setwindowalpha (wptr (front-window)) 0.8s0)
(setwindowalpha (wptr (front-window)) 1.0s0)


|#

