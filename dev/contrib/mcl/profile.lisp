;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: PROFILE -*-

#|
      profile.lisp - Execution time profiler for Macintosh Common Lisp

      Copyright (c) John Carroll, University of Sussex, 1998-2000

      NO WARRANTY IS MADE AND NO LIABILITY IS ACCEPTED FOR THIS PROGRAM

      Permission to use, copy, modify this program 'profile.lisp' for any
      purpose is hereby granted without fee, provided that
      - the above copyright notice appears in all copies, and
      - you cause modified versions to carry prominent notices stating that
        you changed the file and the date of the change

      1998-6-18  John Carroll  initial version (developed while at CSLI,
                               Stanford University - thanks folks!)
      2000-5-15  John Carroll  first public release
      2004-2-24  Gary King     Added consing sampling
|#

#|
profile.lisp records and displays a report showing the time and consing
spent inside all functions during the execution of some code

For example, profiling the execution of (foo 1):

(defun foo (x) (dotimes (n 500000 x) (setq x (log (1+ x)))))

(pro:profile-clear)
(pro:profile (foo 1))
(pro:profile-results)

or equivalently

(pro:with-profiling (foo 1))

Execution time profile from 102 samples
  Parents
Function
  Children                                   Relative  Absolute Consing       Conses
----
foo                                                        100%    100%   14,803,496
  %double-float-log!                             32%
  %double-float                                  22%
  %fixnum-dfloat                                  3%
  minusp                                          3%
  %double-float-minusp                            3%
----
  foo                                           100%
%double-float-log!                                          32%     32%    4,809,984
...

All execution performed within the call to the profile macro is added to
the profiling record for later display, by profile-results. Profile-clear
resets the record ready for a new profile run. With-profiling packages
this all up, clearing any existing profile record, profiling a form, and
displaying the results

In the report output there is one entry for each function that was
executed (as long as time spent within it accounted for at least 1% of the
total time), showing the function name and the percentage of time spent
within that function. The output is sorted with respect to this
percentage.  Within each entry the function's 'parents' (i.e. direct
callers) are shown, with their names and the relative percentage of time
they were the caller of that function. Similarly for the function's
'children' (i.e. direct callees). Thus, in the example, 64% of execution
time was within CCL::LOG-E and the computations it caused to perform; its
caller 100% of the time was FOO, and 66% of the time taken in CCL::LOG-E
was due to calls to CCL::%DOUBLE-FLOAT-LOG!. Note that (as in this case)
internal MCL system functions may appear in the output; this is not
usually a problem since they tend to appear in a cluster towards the end

The profiler takes around 60 samples of the execution state per second.
The time overhead due to profiling is small, typically only a couple of
percent. Space overheads are in the region of 4-10Kbytes per second of
execution time

Developed in MCL 4.0, 4.3, tested also in 4.2 (demo) - PPC versions only
Also works in MCL 5

WIP: added optional :timeout parameter but current implementation unwinds
  all the way to the top
ER: Allow profiling to attach (and detach) to already running process
|#

#|
Changes needed for OpenMCL

need to use locks to synchronize between processes
can't rely on scheduler, use priority instead?
Has Louis done this? No.
|#

(defpackage "PROFILE" (:nicknames "PRO")
   (:use "COMMON-LISP" "CCL")
   (:export profile-clear profile profile-results with-profiling))

(in-package "PROFILE")


;;; Possible inaccuracies in results - caveats:
;;;
;;; * Profiled code following a without-interrupts is likely to be assigned a
;;; misleadingly high proportion of the cpu time, and the function that invokes
;;; the without-interrupts might never be seen. In particular, small, fast
;;; functions (e.g. built-in type predicates) may appear to consume more CPU
;;; time than they actually do. This is because they are often correlated with
;;; the first re-entry back into the scheduler after a tight CPU-bound
;;; computation
;;;
;;; * Any execution performed in another process will not be profiled
;;;
;;; * Functions containing tail calls may never be seen - but their callees
;;; will, and these may appear to have been directly called by higher level
;;; callers

#+Digitool
(defmacro profile (&body form)
  (let* ((first (if (and (listp (car form))
			 (intersection '(:timeout) (car form)))
                  (pop form)
                  nil))
         (timeout (getf first :timeout))
         (pro (gensym))
         (old-quantum (gensym)) (lwm-ptr (gensym)) (child (gensym))
         (startedp (gensym)))
      `(let*
          ((,pro *current-process*)
           (,old-quantum (ccl::process.quantum ,pro))
           (,lwm-ptr nil)
           (,child nil)
           (,startedp nil))
          (setq ,lwm-ptr
             (ccl::%get-frame-ptr (process-stack-group ,pro)))
          (setq ,child
             (process-run-function (list :name "Profile")
                #'(lambda ()
                    ;; do the profile stats collection in a process wait function
                    ;; rather than in a loop inside the process since will get called
                    ;; more often - once on each entry to the scheduler rather than
                    ;; interleaved with execution of the form being profiled
                    (setf *profile-bytes-allocated* (ccl::total-bytes-allocated))
                    (process-wait-with-timeout
                     "Waiting"
                     ,timeout
                       #'(lambda ()
                           (when ,startedp
                              ;; don't collect stats until profiled form has actually
                              ;; started executing
                              (without-interrupts
                                 (profile-collect-call-counts ,pro ,lwm-ptr)))
                           nil))
                    (when ,timeout
                      (process-reset ,pro :always))))) 
          (unwind-protect
            (progn
              ;; get as fine-grained as we can - profile samples will be taken
              ;; at rate of 1 per tick
              (setf (ccl::process.quantum ,pro) 1)
              (setq ,startedp t)
              ,@form)
            (process-kill ,child)
            (setf (ccl::process.quantum ,pro) ,old-quantum)))))

#+OpenMCL
(defmacro profile (&body form)
  (let* ((first (if (and (listp (car form))
			 (intersection '(:timeout) (car form)))
                  (pop form)
                  nil))
         (timeout (getf first :timeout))
         (pro (gensym))
	 (lock (gensym))
	 #+Ignore
         (old-priority (gensym)) 
	 (lwm-ptr (gensym)) 
	 (child (gensym))
         (startedp (gensym)))
      `(let*
          ((,pro *current-process*)
	   (,lock (make-lock))
	   #+Ignore
           (,old-priority (ccl::process.priority ,pro))
           (,lwm-ptr nil)
           (,child nil)
           (,startedp nil))
          (setq ,lwm-ptr (ccl::%get-frame-ptr))
	  (grab-lock ,lock)
          (setq ,child
             (process-run-function "Profile"
                #'(lambda ()
                    ;; do the profile stats collection in a process wait function
                    ;; rather than in a loop inside the process since will get called
                    ;; more often - once on each entry to the scheduler rather than
                    ;; interleaved with execution of the form being profiled
                    (setf *profile-bytes-allocated* (ccl::total-bytes-allocated))
		    
		    ;; don't collect stats until profiled form has actually
		    ;; started executing
		    (process-wait-with-timeout 
		     "Waiting" ,timeout 
		     #'(lambda ()
			 (with-lock-grabbed (,lock)
			   (without-interrupts
			    (profile-collect-call-counts ,pro ,lwm-ptr))
			   nil)))
                    (when ,timeout
                      (process-reset ,pro :always))))) 
          (unwind-protect
            (progn
              ;; get as fine-grained as we can
	      #+Ignore
              (setf (ccl::process.priority ,pro) 1)
              ,@form)
            (process-kill ,child)
	    #+Ignore
            (setf (ccl::process.priority ,pro) ,old-priority)))))

;;; ---------------------------------------------------------------------------

#+EKSL-UTILITIES
(defmacro with-profiling (&body form)
   `(progn
      (unwind-protect
        (u:timeit (:report t :values t)
                  (profile-clear)
                  (profile ,@form))
        (profile-results))))

#-EKSL-UTILITIES
(defmacro with-profiling (form)
   `(prog2
      (profile-clear)
      (profile ,form)
      (profile-results)))


;;; We see the following at the top of the profiled form's stack when we're
;;; in the scheduler running the process-wait function
;;;
;;; #<Anonymous Function #x11F6F39E> 
;;; #<Compiled-function CCL::%PROCESS-WAIT-P #x6B37DDE> 
;;; #<Compiled-function CCL::SCHEDULER #x6B37F5E> 
;;;,#<Compiled-function CCL::CMAIN #x6B96716> 
;;;,#<Anonymous Function #x6B970BE> 
;;;,#<Compiled-function CCL::FUNCALL-WITH-XP-STACK-FRAMES #x6B97076> 
;;; #<Compiled-function CCL::CMAIN (Non-Global)  #x6B9709E> 
;;; #<Compiled-function CCL::%PASCAL-FUNCTIONS% #x6B14B76> 
;;; <user functions>
;;;
;;; - unless the profiled form called the scheduler itself e.g. via sleep.
;;; Then we get something like
;;;
;;; #<Anonymous Function #x11F6A506> 
;;; #<Compiled-function CCL::%PROCESS-WAIT-P #x6B37DDE> 
;;; #<Compiled-function CCL::SCHEDULER #x6B37F5E>
;;; SLEEP etc
;;; <user functions>
;;;
;;; There may be other cases as well. I'm only treating the first case since
;;; it's not easy to distinguish the second

(defvar *profile-call-counts* nil)
(defvar *profile-bytes-allocated* 0)
(defvar *profile-bytes-per-call* nil)

(defun profile-clear nil
  (unless *profile-bytes-per-call*
    (profile-calibrate))
  (setq *profile-call-counts* (list 0 0))
  (setf *profile-bytes-allocated* (ccl::total-bytes-allocated))
  nil)

;;; ---------------------------------------------------------------------------

#+Digitool
(defun profile-collect-call-counts (process lwm-ptr)
   (assert (ccl::processp process))
   (assert (fixnump lwm-ptr))
   (do* ((memory-used (- (ccl::total-bytes-allocated) *profile-bytes-allocated*))
         (stack-group (process-stack-group process))
         (frame-ptr (ccl::%get-frame-ptr stack-group)
                    (ccl::parent-frame frame-ptr stack-group))
         (calls nil)
         (started-collecting-p nil))
      ((or (null frame-ptr)
           (and (fixnump frame-ptr) (>= (the fixnum frame-ptr) (the fixnum lwm-ptr))))
         (unless *profile-call-counts* (profile-clear))
         (setq *profile-call-counts*
            ;; ignore last call frame - the one corresponding to lwm
            ;; since that's the caller of the form we're profiling
            (profile-add-call-counts (cdr calls) memory-used *profile-call-counts*)))
      (let ((object (ccl::cfp-lfun frame-ptr stack-group)))
         (cond
            ((not (compiled-function-p object)))
            (started-collecting-p
               (pushnew object calls :test #'eq))
            ((eq object (symbol-function 'ccl::%pascal-functions%))
               (setq started-collecting-p t)))))
   (setf *profile-bytes-allocated* (ccl::total-bytes-allocated)))

#+OpenMCL
(defun profile-collect-call-counts (process lwm-ptr)
   (assert (ccl::processp process))
   (assert (fixnump lwm-ptr))
   (do* ((memory-used (- (ccl::total-bytes-allocated) *profile-bytes-allocated*))
         (frame-ptr (ccl::%get-frame-ptr)
                    (ccl::parent-frame frame-ptr nil))
         (calls nil)
         (started-collecting-p nil))
      ((or (null frame-ptr)
           (and (fixnump frame-ptr) (>= (the fixnum frame-ptr) (the fixnum lwm-ptr))))
         (unless *profile-call-counts* (profile-clear))
         (setq *profile-call-counts*
            ;; ignore last call frame - the one corresponding to lwm
            ;; since that's the caller of the form we're profiling
            (profile-add-call-counts (cdr calls) memory-used *profile-call-counts*)))
      (let ((object (ccl::cfp-lfun frame-ptr)))
         (cond
            ((not (compiled-function-p object)))
            (started-collecting-p
               (pushnew object calls :test #'eq))
            ((eq object (symbol-function 'ccl::%pascal-functions%))
               (setq started-collecting-p t)))))
   (setf *profile-bytes-allocated* (ccl::total-bytes-allocated)))

;;; ---------------------------------------------------------------------------
;;; add current stack sample to call counts tree
;;;
;;; format of tree is (n0 x1 (nx1 y1 (ny1 ...) y2 (ny2 ...)) x2 (nx2 ...))
;;; where x1 calls y1 etc, nx1 is the call count for function object x1, n0 is
;;; the total number of samples so far

(defun profile-add-call-counts (calls memory-used tree)
  (incf (car tree))
  (incf (cadr tree) memory-used)
  (when calls
    (let ((subtree (getf (cddr tree) (car calls))))
      (setf (getf (cddr tree) (car calls))
            (if subtree
              (profile-add-call-counts (cddr calls) memory-used subtree)
              (reduce #'(lambda (x y) (list 1 memory-used x y)) (cddr calls) 
                      :from-end t
                      :initial-value (list 1 memory-used))))))
  tree)

;;; ---------------------------------------------------------------------------
;;; (profile-results) (pprint *profile-call-counts*)

(defstruct profile-record fn count memory max-depth callers callees)

(defun profile-results (&key (function-length 48) (cutoff 1)) 
  (let ()
    (unless *profile-call-counts*
      (error "~S must be called before ~S" 'profile-clear 'profile-results))
    (labels ((specializer-name (specializer)
             (or
              #+MCL-COMMON-MOP-SUBSET
              (when (ccl::eql-specializer-p specializer) 
                (list 'eql (ccl:eql-specializer-object specializer)))
              #+MCL
              (when (consp specializer)
                specializer)
              (class-name specializer)))
           (get-function-name (f)
             (etypecase f
               (method-function
                (let* ((method (function-name f))
                       (name (function-name (method-generic-function method)))
                       (specializers (method-specializers method))
                       (qualifiers (method-qualifiers method)))
                  (format nil "~(~A~@[ ~A~]~{ <~A>~}~)" 
                          name qualifiers (mapcar #'specializer-name specializers))))
               (compiled-function 
                (format nil "~(~A~)" (or (function-name f) f))))
             #+Old
             (or (nth-value 2 (function-lambda-expression object)) object)))
      (let* ((tree *profile-call-counts*)
             (total (car tree))
             (total-memory (- (second tree)
                              (* *profile-bytes-per-call* total)))
             (items 
              ;; remove any functions with less than 1% of CPU time - use of
              ;; max-depth is to ensure that even if counts of a caller and callee
              ;; are the same, the caller appears first in the report
              (sort
               (delete-if
                #'(lambda (item)
                    (< (round (* (profile-record-count item) 100) total) 1))
                (profile-results-absolute tree nil 0))
               #'(lambda (i1 i2)
                   (if (= (profile-record-count i1) (profile-record-count i2))
                     (< (profile-record-max-depth i1) (profile-record-max-depth i2))
                     (> (profile-record-count i1) (profile-record-count i2)))))))
        (profile-results-callers tree items nil)
        (profile-results-callees tree items)
        (block report-printing
          (format t "~&Execution time profile from ~A samples
  Parents~%Function~%  Children ~VTRelative  Absolute Consing       Conses~%" 
                  total (- function-length 3))
          (dolist (item items)
            (format t "----~%")
            (let* ((rem 0)
                   (absolute (round (* (profile-record-count item) 100) total))
                   (memory (- (profile-record-memory item)
                              (* *profile-bytes-per-call* (profile-record-count item)))) 
                   (absolute-memory (round (* memory 100) 
                                           total-memory)))
              (when (and (numberp cutoff) (<= absolute cutoff))
                (return-from report-printing))
              
              ;; add remainder back in to next call to round to stop any rounding
              ;; errors accumulating 
              (dolist (pair (sort (profile-record-callers item) #'> :key #'cdr))
                (multiple-value-bind (per new-rem)
                                     (round (+ (* (cdr pair) 100) rem) (profile-record-count item))
                  (setq rem new-rem)
                  (when (> per 0)
                    (format t "  ~A~@?~3D%~%" 
                            (get-function-name (car pair))
                            (format nil "~~~DT" function-length) per))))
              (format t "~A~@?~4D%   ~4,@A% ~12:D~%"
                      (get-function-name (profile-record-fn item))
                      (format nil "~~~DT" (+ 10 function-length))
                      absolute
                      (if (or (<= absolute-memory 0)
                              (> absolute-memory 100))
                        "---"
                        absolute-memory)
                      (profile-record-memory item)))
            (let ((rem 0))
              (dolist (pair (sort (profile-record-callees item) #'> :key #'cdr))
                (multiple-value-bind (per new-rem)
                                     (round (+ (* (cdr pair) 100) rem) (profile-record-count item))
                  (setq rem new-rem)
                  (when (> per 0)
                    (format t "  ~A~@?~3D%~%" 
                            (get-function-name (car pair))
                            (format nil "~~~DT" function-length)
                            per)))))))))))

;;; ---------------------------------------------------------------------------

(defun profile-results-absolute (tree res depth)
  (do ((tail (cddr tree) (cddr tail)))
      ((null tail) res)
    (let ((found (find (car tail) res :key #'profile-record-fn :test #'eq)))
      (unless found
        (setq found
              (make-profile-record
               :fn (car tail) :memory 0 :count 0 :max-depth 0 :callers nil :callees nil))
        (push found res))
      (incf (profile-record-count found) (car (cadr tail)))
      (incf (profile-record-memory found) (cadr (cadr tail)))
      (setf (profile-record-max-depth found)
            (max depth (profile-record-max-depth found))))
    (when (cddr (cadr tail))
      (setq res (profile-results-absolute (cadr tail) res (1+ depth))))))

;;; ---------------------------------------------------------------------------

(defun profile-results-callers (tree items parent)
  (do ((tail (cddr tree) (cddr tail)))
      ((null tail))
    (when parent
      (let ((found (find (car tail) items :key #'profile-record-fn :test #'eq)))
        (when found
          (let ((item (assoc parent (profile-record-callers found) :test #'eq)))
            (unless item
              (setq item (cons parent 0))
              (push item (profile-record-callers found)))
            (incf (cdr item) (car (cadr tail)))))))
    (profile-results-callers (cadr tail) items (car tail))))

;;; ---------------------------------------------------------------------------

(defun profile-results-callees (tree items)
  (do ((tail (cddr tree) (cddr tail)))
      ((null tail))
    (let ((found (find (car tail) items :key #'profile-record-fn :test #'eq)))
      (when found
        (dolist (desc-pair (profile-results-callees1 (cadr tail) items nil))
          (let ((item
                 (assoc (car desc-pair) (profile-record-callees found) :test #'eq)))
            (unless item
              (setq item (cons (car desc-pair) 0))
              (push item (profile-record-callees found)))
            (incf (cdr item) (cdr desc-pair))))))
    (profile-results-callees (cadr tail) items)))

;;; ---------------------------------------------------------------------------

(defun profile-results-callees1 (tree items res)
   ;; return list of callees (plus freqs) which are in items - i.e. occur frequently
   ;; enough that we can be sure they're significant. When we've found one don't
   ;; recurse into its callees because this info will also be output elsewhere
   (do ((tail (cddr tree) (cddr tail)))
      ((null tail) res)
      (if (find (car tail) items :key #'profile-record-fn :test #'eq)
         (push (cons (car tail) (car (cadr tail))) res)
         (setq res (profile-results-callees1 (cadr tail) items res)))))

;;; ---------------------------------------------------------------------------

(defun profile-calibrate ()
  (format *debug-io* "~&;; Profiler calibrating, please wait...")
  (force-output *debug-io*)
  (setf *profile-bytes-per-call* 0)
  (profile-clear)
  (profile (loop repeat 10000000 do
                 (lambda ())))
  ;; rough estimate
  (setf *profile-bytes-per-call* (float (/ (second *profile-call-counts*)
                                           (first *profile-call-counts*))))
  (values))


#| Test 1
(defun foo-1 (count)
  (declare (optimize (debug 3)))
  (dotimes (n count)
    (foo-2 n)))

(defun foo-2 (n)
  (declare (optimize (debug 3)))
  (let ((y (1+ n)))
    (foo-3 y)))

(defun foo-3 (n)
  (declare (optimize (debug 3)))
  (+ (sin n) (cos n) (log n) (sqrt n) (tan n)))

(pro:with-profiling (foo-1 1000000))
|#
   
#| Test 2
(defun bar-1 (count)
  (declare (optimize (debug 3)))
  (dotimes (n count)
    (bar-2 n)))

(defun bar-2 (n)
  (declare (optimize (debug 3)))
  (bar-3 n))

(defun bar-3 (n)
  (declare (optimize (debug 3)))
  (bar-4 n))

(defun bar-4 (n)
  (declare (optimize (debug 3)))
  (values n))

(pro:with-profiling (bar-1 1000000))

(bar-1 1000000)
|#



#|
;;; testing the stack crawl

#+Digitool
(defun print-backtrace-function-names (process)
   (print '----)
   (do* ((sg (process-stack-group process))
         (frame-ptr (ccl::%get-frame-ptr sg) (ccl::parent-frame frame-ptr sg)))
      ((null frame-ptr))
      (let ((object (ccl::cfp-lfun frame-ptr sg)))
         (when (compiled-function-p object)
            (let ((fname (nth-value 2 (function-lambda-expression object))))
               (print (or fname object)))))))

#+OpenMCL
(defun print-backtrace-function-names (process)
   (print '----)
   (do* ((frame-ptr (ccl::%get-frame-ptr) (ccl::parent-frame frame-ptr nil)))
       ((null frame-ptr))
     (let ((object (ccl::cfp-lfun frame-ptr)))
         (when (compiled-function-p object)
            (let ((fname (nth-value 2 (function-lambda-expression object))))
               (print (or fname object)))))))

(print-call-history :detailed-p nil)
(print-backtrace-function-names *current-process*)
|#


#| Notes from Gary King 2004-01-28: 

this is also a source of information about 'actual callers'

we could 'roll up' some functions to show just what is going on in a package

should determine 'function-length' automagically

ok - feature: cut off report at some depth (e.g., 1-%)
ok - improve printing of method names
yes - if we use named lambdas, could we get around the anonymous function stuff?

|#


;;; End of file
