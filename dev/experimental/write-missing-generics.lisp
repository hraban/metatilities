(in-package #:metatilities)

;;; not perfect but not too bad.
;;; needs to strip out defaults from &optional and &key arguments


(defun gf-info (symbol)
  (let ((gf (symbol-function symbol)))
    (list 
     :declarations (ccl:generic-function-declarations gf)
     :lambda-list (ccl:generic-function-lambda-list gf)
     :method-class (ccl:generic-function-method-class gf)
     :method-combination (ccl:generic-function-method-combination  gf)
     :methods (ccl:generic-function-methods  gf) 
     :name (ccl:generic-function-name  gf))))

;;; ---------------------------------------------------------------------------

(defun write-gf-template (symbol stream)
  (let ((info (gf-info symbol)))
    (format stream "\(defgeneric ~(~A~) " (getf info :name))
    (format stream "~(~A~)" (fix-lambda-list (getf info :lambda-list)))
    (when (and (getf info :method-combination)
               (not (eq (ccl::method-combination-name (getf info :method-combination)) 
                        'standard)))
      (format stream "~%  \(:method-combination ~(~A~)\)" (getf info :method-combination)))
    (if (documentation symbol 'function)
      (format stream "~%  \(:documentation ~S\)\)" (documentation symbol 'function))
      (format stream "~%  \(:documentation \"\"\)\)~%")))
  (terpri stream)
  (values))

;;; ---------------------------------------------------------------------------

(defun fix-lambda-list (list)
  ;; not complete but hopefully good enough
  (loop for item in list collect
        (cond ((atom item) item)
              ((consp item) (first item)))))

;;; ---------------------------------------------------------------------------

(defun find-symbol-function (symbol-name)
  (iterate-elements
   (list-all-packages)
   (lambda (package)
     (when (aand (find-symbol symbol-name package)
                 (ignore-errors (symbol-function it))
                 (typep it 'standard-generic-function))
       (return-from find-symbol-function (find-symbol symbol-name package))))))

;;; ---------------------------------------------------------------------------

(defun build-defgenerics-for-undocumented-methods (package file)
  (let* ((p (make-container 'package-container 
                            :packages package
                            :exported-symbols-only-p nil
                            :present-symbols-only-p t))
         (ms
          (sort
           (append
            (collect-elements 
             p
             :filter (lambda (symbol)
                       (and (fboundp symbol)
                            (typep (symbol-function symbol) 'standard-generic-function)
                            (some (lambda (m)
                                    (not (or (reader-method-p m)
                                             (writer-method-p m))))
                                  (mopu:generic-function-methods (symbol-function symbol)))
                            (not (documentation symbol 'function)))))
            (collect-elements 
             p
             :filter (lambda (base-symbol)
                       (let ((symbol (list 'setf base-symbol)))
                         (and (fboundp symbol)
                              (typep (symbol-function symbol) 'standard-generic-function)
                              (some (lambda (m)
                                      (not (or (reader-method-p m)
                                               (writer-method-p m))))
                                    (mopu:generic-function-methods (symbol-function symbol)))
                              (not (documentation symbol 'function)))))
             :transform (lambda (base-symbol)
                          (list 'setf base-symbol))))
           (lambda (a b)
             (cond ((and (consp a) (consp b))
                    (string-lessp (second a) (second b)))
                   ((and (consp a) (not (consp b)))
                    1)
                   ((and (not (consp a)) (consp b))
                    -1)
                   (t
                    (string-lessp a b)))))))
    
    (when ms
      (princ "Creating file...")
      (with-new-file (out file)
        (iterate-elements 
         ms
         (lambda (symbol)
           (write-gf-template symbol out)))))))
         
;;; ---------------------------------------------------------------------------

#+Test
(delete-file "user-home:darcs;lift-generics.lisp")

#+Test
(metatilities::build-defgenerics-for-undocumented-methods
 :cl-containers "user-home:darcs;cl-containers-generics.lisp")

#+Test
(metatilities::build-defgenerics-for-undocumented-methods
 :metatilities "user-home:darcs;generics.lisp")

