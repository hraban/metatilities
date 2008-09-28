(in-package #:metatilities)

;;; ----------------------------------------------------------------------------
;;;
;;;   MAPPING AND FOLDING
;;;


(defun reduce-if (pred seq &rest reduce-keys)
  "See MINIMIZE.  Primary difference: no non-standard keyword behavior."
  (apply #'reduce #'(lambda (x y)
                      (if (funcall pred y x) y x)) seq reduce-keys))

;;; ---------------------------------------------------------------------------

(defun minimize (pred seq &rest reduce-keys &key (key #'identity) &allow-other-keys)
  "Returns the ``smallest'' element.  Supports all the usual reduce keywords. 
PRED sees the new values first when :from-end is nil.  Unlike REDUCE-IF, MINIMIZE 
does not return the value of applying :key."
  (let ((reduce-keys (copy-list reduce-keys)))          ; be safe, &rest may not be new
    (remf reduce-keys :key)
    (flet ((pred (x y)
             (funcall pred (funcall key x) (funcall key y))))
      (apply #'reduce-if #'pred seq reduce-keys))))

;;; ---------------------------------------------------------------------------

(defun mapcan1 (fun list)
  "Like MAPCAN, but for a single list.  Doesn't cons, and is reasonably
well optimized."
  (declare (optimize (speed 3) (compilation-speed 0) (safety 0)))
  (when list
    (let* ((acc (funcall fun (car list)))
           (last acc))
      (dolist (elt (cdr list) acc)
        (let ((val (funcall fun elt)))
          (nconc last val)
          (when val
            (setf last val)))))))

;;; ---------------------------------------------------------------------------

(defun flatten (list)
  "Flattens LIST. Does not handle circular lists but does handle dotted lists."
  (labels ((rec (list)
             (cond ((atom list)
                    (list list))
                   ((dotted-pair-p list)
                    (nconc (rec (car list)) (rec (cdr list))))
                   (t               
                    (mapcan1 #'rec list)))))
    (declare (dynamic-extent #'rec))
    (if (atom list)
      list
      (rec list))))

;;; ---------------------------------------------------------------------------

(defun power-set ( set )
  "Returns the power set of SET.  This of course uses exponential time and space."
  (cond
   ((null set) (list nil))
   (t
    (nconc
     (power-set (cdr set))
     (mapcar
      #'(lambda (x) (cons 
		     (car set)
		     x))
      (power-set (cdr set)))))))

;;; ---------------------------------------------------------------------------

(defun all-pairs (lst)
   "Returns all pairs of elements in list LST."
   (loop for i on lst
         append (loop for j in (cdr i)
                      collect (list (first i) j))))

;;; ---------------------------------------------------------------------------

(defun map-combinations (fn &rest lists)
  "Call fn on all combinations of elements in LISTS, where first
element of each combination is from list1, second element of
each combo is from list2, etc.  E.g.,
    ? \(combinations '\(a b\) '\(1 2 3\) '\(x y\)\)
    \(\(B 3 Y\) \(A 3 Y\) \(B 2 Y\) \(A 2 Y\) \(B 1 Y\) \(A 1 Y\)
      \(B 3 X\) \(A 3 X\) \(B 2 X\) \(A 2 X\) \(B 1 X\) \(A 1 X\)\)"
  (iterate-over-indexes (mapcar #'length lists)
                        (lambda (l)
                          (apply fn (mapcar (lambda (n list)
                                              (nth n list))
                                            l lists)))))

;;; ---------------------------------------------------------------------------

(defun combinations (&rest lists)
  "Returns all combinations of elements in LISTS, where first
element of each combination is from list1, second element of
each combo is from list2, etc.  E.g.,
    ? \(combinations '\(a b\) '\(1 2 3\) '\(x y\)\)
    \(\(B 3 Y\) \(A 3 Y\) \(B 2 Y\) \(A 2 Y\) \(B 1 Y\) \(A 1 Y\)
      \(B 3 X\) \(A 3 X\) \(B 2 X\) \(A 2 X\) \(B 1 X\) \(A 1 X\)\)"
  (bind ((result nil))
    (apply #'map-combinations
     (lambda (&rest x)
       (push x result))
     lists)
    (nreverse result)))


;;; ---------------------------------------------------------------------------
;;; fetched this from this webpage http://www.t3x.org/scipl/lib/permute.html
;;; fixed some typos and made compatible with MCL -jjm
;;; ---------------------------------------------------------------------------

(defun permute (x)
  (labels (; rotate a list by moving the first element to the tail
           (rotate (x)
             (append (cdr x) (list (car x))))
   
           ; distribute A over B by consing A to each member of B
           (dist (a b)
             (cond ((null b) nil)
                   (t (cons (cons a (car b)) (dist a (cdr b))))))
   
           ; create permutations of each member L of X by
           ; distributing (CAR L) over all permutations of (CDR L)
           (permlist (x r)
             (cond ((null x) r)
                   (t (permlist 
                       (cdr x)
                       (append 
                        (dist (caar x) (perm (cdar x) (cdar x) nil)) r)))))
           
           ; create a list of all possible rotations of a list
           ; and pass it to PERMLIST
           (perm (x n r)
             (cond ((null n) (permlist r nil))
                   ((null (cdr x)) x)
                   ((null (cddr x)) (list x (reverse x)))
                   (t (perm (rotate x) (cdr n) (cons x r))))))
  
    (perm x x nil)))

;;; ----------------------------------------------------------------------------
;;;
;;;   ADDING AND REMOVING
;;;

(defun remove-members (bad from &rest keys)
  "Removes all members of BAD from the list FROM."
  (apply #'remove-if
         (lambda (elem)
           (find elem bad))
         from
         keys))

;;; ----------------------------------------------------------------------------
;;; transpose :: (t) -> ... -> ((t))
;;; ----------------------------------------------------------------------------
(defun transpose (list &rest lists)
  (apply #'mapcar #'list list lists))

(defun transpose2 (lists)
  (loop for i from 0 to (1- (length (first lists))) collect
        (loop for lst in lists collect
              (elt lst i))))

#| From CLASP
(defun transpose (data)
  "Takes a list of lists, `data', and makes a list of tuples t where t[i]
contains all the i'th elements of the lists in `data'."
  (if data
      #+(or EXPLORER Lispworks)
      (let ((retval (make-list (length (car data)) :initial-element '())))
	(dolist (datum (reverse data))
	  (dotimes (n (length datum))
	    (push (nth n datum) (nth n retval))))
	retval)
      #-(or EXPLORER Lispworks)
      (apply #'mapcar #'list data)
      nil))
|#

;;; ---------------------------------------------------------------------------

(defun list-choose-k (list k)
  "Return a list of lists of all possible ways of choosing
k-elements from the original list."
  (let ((result))
    (iterate-over-indexes
     (make-list k :initial-element (length list))
     (lambda (index-list)
       (when (apply #'< index-list)
         (push 
          (loop for index in index-list collect
                (elt list index))
          result))))
    
    (nreverse result)))

;;; ---------------------------------------------------------------------------

(defun same-length-p (list-1 list-2)
  "An optimized version of the naive \(= \(length list-1\) \(length list-2\)\)."
  (do ((x list-1 (rest x))
       (y list-2 (rest y)))
      ((not (and (or x (rest x)) (or y (rest y))))
       (and (null x) (null y)))))

;;; ---------------------------------------------------------------------------

(defun length-exactly-p (thing n)
  "Returns true if the lenght of `thing` is exactly `n`... no more, no less."
  (and (length-at-least-p thing n)
       (length-at-most-p thing n)))

;;; ---------------------------------------------------------------------------

(defun percent-overlap (list-a list-b &key (test 'eql) (key 'identity))
  "Returns what percentage of elements in list-a are in list-b
\(alternate definition: Returns what percentage of list-b is made up of element from list-a\)"
  (let* ((len-a (length list-a))
         (number-overlap (- len-a
                            (length
                             (set-difference list-a list-b
                                             :test test
                                             :key key))))
         (len-b (length list-b)))
    (float (/ number-overlap len-b))))



