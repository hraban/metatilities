(in-package #:metatilities)

(defun set-equal (list1 list2 &rest args
		  &key test key)
  "Returns t if list1 and list2 are equal (as sets). If list1 and list2 are not
equal returns (as multiple values) nil and two lists. The first list contains the 
elements in list1 and not in list2 and the second list contains elements in list2 
and not in list1."
  (declare (ignore test key))
  (let ((in1-not2 (apply #'set-difference list1 list2 args))
        (in2-not1 (apply #'set-difference list2 list1 args)))
    (if (apply #'union in1-not2 in2-not1 args)
      (values nil in1-not2 in2-not1)
      (values t nil nil))))

#+Alternate
;; some very quick testing doesn't show any real improvement
;; but needs more investigation --- should cons less at least
(defun set-equal (list-1 list-2 &rest args)
  (and (every (lambda (elt-1)
                (apply #'member elt-1 list-2 args))
              list-1)
       (every (lambda (elt-2)
                (apply #'member elt-2 list-1 args))
              list-2)))



