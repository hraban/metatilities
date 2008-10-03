(in-package metatilities-base-test)

(defun slot-specs-same-p (spec-1 spec-2)
  (cond ((and (atom spec-1) (atom spec-2))
         (eq spec-1 spec-2))
        ((and (consp spec-1) (consp spec-2))
         (and (slot-specs-same-p (first spec-1) (first spec-2))
              (same-options-p (rest spec-1) (rest spec-2))
              (same-options-p (rest spec-2) (rest spec-1))))))

(defun same-options-p (options-1 options-2)
  (loop for name in options-1 by #'cddr
        for value in (rest options-1) by #'cddr do
        ;; cons up something fresh to ensure that we don't get equality
        (unless (samep value (getf options-2 name (cons nil nil)))
          (return-from same-options-p nil)))
  (values t))

(deftestsuite test-parse-brief-slot (metatilities-base-test)
  ()
  (:dynamic-variables (*automatic-slot-accessors?* nil)
                      (*automatic-slot-initargs?* nil)
                      (*prune-unknown-slot-options* nil))
  (:equality-test #'slot-specs-same-p))

(addtest
  simple-1
  (ensure-same (parse-brief-slot 'foo) '(foo)))

(addtest
  simple-2
  (ensure-same (parse-brief-slot '(foo)) '(foo)))

(addtest
  initform-1
  (ensure-same (parse-brief-slot '(foo t)) '(foo :initform t)))

(addtest
  initform-initarg-1
  (ensure-same (parse-brief-slot '(foo t i)) '(foo  :initform t :initarg :foo)))

(addtest
  initform-reader-1
  (ensure-same (parse-brief-slot '(foo t r)) '(foo :reader foo :initform t)))
  
(addtest
  initform-accessor-1
  (ensure-same (parse-brief-slot '(foo t a)) '(foo :accessor foo :initform t)))

(addtest
  initform-accessor-with-extra
  (ensure-same (parse-brief-slot '(foo t a :wow 2)) 
               '(foo :accessor foo :initform t :wow 2)))

(addtest
  unbound-with-extra
  (ensure-same (parse-brief-slot '(bar :unbound :component nil)) 
               '(bar :component nil)))

(addtest
  initform-accessor-documentation
  (ensure-same (parse-brief-slot '(foo t ia "test slot")) 
               '(foo :accessor foo :initform t :initarg :foo 
                 :documentation "test slot")))

(addtest
  unbound-with-extra
  (ensure-same (parse-brief-slot '(bar :unbound :component nil)) 
               '(bar :component nil)))

#|
  (spy (parse-brief-slot '(foo t ia "test slot")))
  (spy (parse-brief-slot '(foo t * "test slot")))
  (spy (parse-brief-slot '(foo t "slot")))
  (spy (parse-brief-slot '(bar :initform nil)))
  (spy (parse-brief-slot 'baz t t))
  (spy (parse-brief-slot 'baz t t 'class))
  (spy (parse-brief-slot '(baz nil) t t 'class))
  (spy (parse-brief-slot '(baz nil) nil t 'class))
  (spy (parse-brief-slot '(baz nil) t nil 'class))
  (spy (parse-brief-slot '(baz nil) nil nil 'class))
  (spy (parse-brief-slot '(baz nil "the baz slot") t t 'class))
  (spy (parse-brief-slot '(baz nil a) nil nil 'class))
  (spy (parse-brief-slot '(baz nil r) nil nil 'class))
  (spy (parse-brief-slot '(baz nil r) nil nil 'class nil "."))
  (spy (parse-brief-slot '(baz nil r) t t 'class nil "."))
  (spy (parse-brief-slot '(foo 2 :type 'fixnum ia "the foo class" :initarg :what)))
|#

#+test
(defclass-brief foo ()
  "the Foo class"
  (a
    (b)
    (c 1)
    (d 2 i)
    (e 3 ia "The E slot")))

#+test
(defclass-brief foo ()
  "the Foo class"
  (a b)
  :automatic-accessors
  :automatic-initargs
  :name-prefix)

#+test
(defclass-brief foo ()
  "the Foo class"
  ((a 1 r)
   (c 3 a))
  :name-prefix)

#+test
(defclass-brief foo ()
  "the Foo class"
  (a b)
  :automatic-accessors
  :name-prefix)

#+test
(defclass-brief foo ()
  "the Foo class"
  (a b)
  :automatic-accessors
  :automatic-initargs
  (:name-prefix ugly))