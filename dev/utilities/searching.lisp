;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-

(in-package #:metatilities)

(defun binary-search (predicate min max 
                                &key (tolerance *samep-tolerance*)
                                (verbose? nil)
                                (trust-me? nil)
                                (min-bias 0.5)
                                (max-bias 0.5))
  (when (not trust-me?)
    (awhen (or (and (eq (funcall predicate min) t) min)
               (and (eq (funcall predicate max) t) max))
      (return-from binary-search (values it :exact))))
  
  (unless trust-me?
    (assert (eq (funcall predicate min) :higher) nil
            "~A on ~A (the minimum) should be :higher, but wasn't" predicate min)
    (assert (eq (funcall predicate max) :lower) nil
            "~A on ~A (the maximum) should be :lower, but wasn't" predicate max))
  
  (let ((last-guess nil))
    (labels ((do-it (min max)
               (let ((guess (+ (* min-bias min) (* max-bias max))))
                 (when verbose?
                   (format *debug-io* "~%Guess: ~A" guess))
                 (when (and last-guess
                            (nearly-samep guess last-guess tolerance))
                   (return-from binary-search (values guess :within-tolerance)))
                 (setf last-guess guess)
                 (ecase (funcall predicate guess)
                   ((t)
                    (return-from binary-search (values guess :exact)))
                   (:lower
                    (do-it min guess))
                   (:higher
                    (do-it guess max))))))
      (do-it min max))))

;;; ---------------------------------------------------------------------------

#+Example
(defun percent-sure (n k percent)
  (binary-search
   (lambda (size)
     (let ((value (hypergeometric-distribution
                   n k size k))
           (value-1 (hypergeometric-distribution
                     n k (1- size) k)))
       (cond ((< value percent)
              :higher)
             ((and (>= value percent)
                   (< value-1 percent))
              (values t))
             (t
              :lower))))
   1.0 n
   :tolerance 1.0d0
   :verbose? t))

;;; ---------------------------------------------------------------------------
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; search.lisp: Search routines from section 6.4
;;;; with minor modifications
;;; ---------------------------------------------------------------------------

(defun tree-search (states goal-p successors combiner)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner."
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search
             (funcall combiner
                      (funcall successors (first states))
                      (rest states))
             goal-p successors combiner))))

;;; ---------------------------------------------------------------------------

(defun prepend (x y) "Prepend y to start of x" (append y x))

;;; ---------------------------------------------------------------------------

(defun depth-first-search (start goal-p successors)
  "Search new states first until goal is reached."
  (tree-search (list start) goal-p successors #'append))

;;; ---------------------------------------------------------------------------

(defun breadth-first-search (start goal-p successors)
  "Search old states first until goal is reached."
  (tree-search (list start) goal-p successors #'prepend))

;;; ---------------------------------------------------------------------------

(defun make-sorter-fn (cost-fn)
  "Return a combiner function that sorts according to cost-fn."
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

;;; ---------------------------------------------------------------------------
  
(defun best-first-search (start goal-p successors cost-fn)
  "Search lowest cost states first until goal is reached."
  (tree-search (list start) goal-p successors (make-sorter-fn cost-fn)))

;;; ---------------------------------------------------------------------------

(defun beam-search (start goal-p successors cost-fn beam-width)
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  (tree-search (list start) goal-p successors 
               #'(lambda (old new)
                   (let ((sorted (funcall (make-sorter-fn cost-fn) old new)))
                     (if (> beam-width (length sorted))
                         sorted
                         (subseq sorted 0 beam-width))))))


#+Later
(defun path-saver (successors cost-fn cost-left-fn)
  #'(lambda (old-path)
      (let ((old-state (path-state old-path)))
        (mapcar
          #'(lambda (new-state)
              (let ((old-cost
                      (+ (path-cost-so-far old-path)
                         (funcall cost-fn old-state new-state))))
                (make-path
                  :state new-state
                  :previous old-path
                  :cost-so-far old-cost
                  :total-cost (+ old-cost (funcall cost-left-fn
                                                   new-state)))))
          (funcall successors old-state)))))

#+Later
(defun print-path (path &optional (stream t) depth)
  (declare (ignore depth))
  (format stream "#<Path to ~a cost ~,1f>"
          (path-state path) (path-total-cost path)))


;;; ---------------------------------------------------------------------------

(defun iter-wide-search (start goal-p successors cost-fn
                          &key (width 1) (max 100))
  "Search, increasing beam width from width to max.
  Return the first solution found at any width."
  (unless (> width max)
    (or (beam-search start goal-p successors cost-fn width)
        (iter-wide-search start goal-p successors cost-fn
                           :width (+ width 1) :max max))))

;;; ---------------------------------------------------------------------------

(defun graph-search (states goal-p successors combiner
                     &key (state= #'eql) old-states
                     (new-state-fn #'new-states))
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.  
  Don't try the same state twice."
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
             (funcall
               combiner
               (funcall new-state-fn states successors state= old-states)
               (rest states))
             goal-p successors combiner
             :state= state=
             :old-states (adjoin (first states) old-states
                                 :test state=)
             :new-state-fn new-state-fn))))

#+Old
(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) old-states)
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.  
  Don't try the same state twice."
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (graph-search
             (funcall
               combiner
               (new-states states successors state= old-states)
               (rest states))
             goal-p successors combiner state=
             (adjoin (first states) old-states
                     :test state=)))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
    #'(lambda (state)
        (or (member state states :test state=)
            (member state old-states :test state=)))
    (funcall successors (first states))))


(defvar *a*-count* 0)
(defvar *a*-limit* 1000)

#+Later
(defun a*-search (paths goal-p successors cost-fn cost-left-fn
                        &optional (state= #'eql) old-paths (initial-call t))
  "Find a path whose state satisfies goal-p.  Start with paths,
  and expand successors, exploring least cost first.
  When there are duplicate states, keep the one with the
  lower cost and discard the other."
  (if initial-call
    (setf *a*-count* 0)
    (when (and *a*-limit* (> (incf *a*-count*) *a*-limit*))
      (throw :a*-terminated (values :terminated paths))))
  (cond ((null paths) fail)
        ((funcall goal-p (path-state (first paths)))
         (values (first paths) paths))
        (t (let* ((path (pop paths))
                  (state (path-state path)))
             ;; Update PATHS and OLD-PATHS to reflect
             ;; the new successors of STATE:
             (setf old-paths (insert-path path old-paths))
             (dolist (state2 (funcall successors state))
               (let* ((cost (+ (path-cost-so-far path)
                               (funcall cost-fn state state2)))
                      (cost2 (funcall cost-left-fn state2))
                      (path2 (make-path
                              :state state2 :previous path
                              :cost-so-far cost
                              :total-cost (+ cost cost2)))
                      (old nil))
                 ;; Place the new path, path2, in the right list:
                 (cond
                  ((setf old (find-path state2 paths state=))
                   (when (better-path path2 old)
                     (setf paths (insert-path
                                  path2 (delete old paths)))))
                  ((setf old (find-path state2 old-paths state=))
                   (when (better-path path2 old)
                     (setf paths (insert-path path2 paths))
                     (setf old-paths (delete old old-paths))))
                  (t (setf paths (insert-path path2 paths))))))
             ;; Finally, call A* again with the updated path lists:
             (a*-search paths goal-p successors cost-fn cost-left-fn
                        state= old-paths nil)))))

;;; ---------------------------------------------------------------------------

(defun search-all (start goal-p successors cost-fn beam-width)
  "Find all solutions to a search problem, using beam search."
  ;; Be careful: this can lead to an infinite loop.
  (let ((solutions nil))
    (beam-search
      start #'(lambda (x)
                (when (funcall goal-p x) (push x solutions))
                nil)
      successors cost-fn beam-width)
    solutions))

;;; ---------------------------------------------------------------------------

#+Later
(defclass search-node () 
  ((state :initform nil
          :initarg :state
          :accessor state)
  (parent :initform nil
          :initarg :parent
          :accessor parent)
  (operator :initform nil
            :initarg :operator
            :accessor operator)
  (depth :initform 0
         :initarg :depth
         :accessor depth)
  (path-cost :initform 0
             :initarg :path-cost
             :accessor path-cost)
  (children :initform nil
            :initarg :children
            :accessor children))
  )

#+Later
(defun make-search-node (&key state parent path-cost)
  (make-instance 'search-node
    :state state
    :parent parent
    :path-cost path-cost))

#+Later
(defun general-search (problem queuing-fn)
  (let ((nodes (make-priority-queue-bst 
                :key #'(lambda (node) 
                         (+ (path-cost node)
                            (funcall (heuristic-fn problem) node)))
                :test queuing-fn)))
    ;; Push the starting state onto the queue
    (enqueue nodes (make-node :state (initial-state problem)))
    
    (while (not (empty-p nodes))
      (let ((node (dequeue nodes)))
        #+Ignore
        (when (> (depth node) 5)
          (break))
        (format t "~A " (depth node))
        (when (funcall (goal-test problem) (state node))
          (return-from general-search node))
        
        (enqueue-list nodes (expand node (operators problem)))))

    (unless (empty-p nodes)
      (front nodes))))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************