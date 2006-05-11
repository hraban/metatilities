#| simple-header

http://www.opensource.org/licenses/mit-license.php

Copyright (c) 2004-2005 Gary Warren King, metabang.com

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
Author: Gary King

DISCUSSION

|#

(in-package #:metatilities)

;;; ---------------------------------------------------------------------------
;;; compatibility
;;; ---------------------------------------------------------------------------

(defgeneric map-system-files (system/s fn &key)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defparameter *prefered-system-definer*
  (or #+ASDF :asdf
      #+:GLU-GENERIC-LOAD-UTILS :glu))

(defmethod find-system* ((system-definer (eql :asdf)) system-name)
  #+ASDF
  (asdf:find-system (ensure-system-name* :asdf system-name) nil))

(defmethod find-system* ((system-definer (eql :glu)) system-name)
  #+:GLU-GENERIC-LOAD-UTILS
  (let ((system (ensure-system-name* :glu system-name)))
    (when (and (symbolp system)
               (getf (symbol-plist system) :glu-system))
      system)))

;;?? Gary King 2005-12-02: a very ugly hack
(defmethod find-system-for-mapping* (system-definer system-name)
  (find-system* system-definer system-name))

(defparameter *prefered-definer-for-systems* (make-hash-table))

(defun select-system-definer (system-name)
  (let ((definer (gethash system-name *prefered-definer-for-systems*)))
    (or definer
        (setf (gethash system-name *prefered-definer-for-systems*)
              (let ((asdf-system (find-system* :asdf system-name))
                    (glu-system (find-system* :glu system-name)))
                (cond ((eq system-name nil)
                       *prefered-system-definer*)
                      ((and asdf-system (eq *prefered-system-definer* :asdf))
                       :asdf)
                      ((and glu-system (eq *prefered-system-definer* :glu))
                       :glu)
                      (asdf-system
                       :asdf)
                      (glu-system
                       :glu)
                      (t
                       (error "No system definer for ~A" system-name))))))))

#+ASDF
;;; We use this to keep track of nestings for ASDF components.
(defparameter *relative-pathname* "")

;;; ---------------------------------------------------------------------------

#+ASDF
(defmethod registered-systems* ((system-definer (eql :asdf)))
  (let ((result nil))
    (maphash 
     (lambda (k v)
       (declare (ignore v))
       (push (ensure-system-name* :asdf k) result))
     asdf::*defined-systems*)
    result))

;;; ---------------------------------------------------------------------------

#+ASDF
(defmethod loaded-systems* ((system-definer (eql :asdf)))
  (let ((result nil))
    (maphash 
     (lambda (name time.system)
       (let ((system (cdr time.system)))
         (when (gethash 'load-op (asdf::component-operation-times system))
           (push (ensure-system-name* :asdf name) result))))
     asdf::*defined-systems*)
    (values result)))

#+ASDF
(defmethod associated-test-system* ((system-definer (eql :asdf)) system-name)
  (declare (ignore system-definer))
  (values nil)) 

#+ASDF
(defmethod system-sub-systems* ((system-definer (eql :asdf)) system-name sub-systems?)
  (declare (ignore sub-systems?))
  (collect-system-dependencies system-name))

#+ASDF
(defmethod top-level-system-p* ((system-definer (eql :asdf)) system-name)
  (declare (ignore system-name))
  (values t))

#+ASDF
(defmethod system-name-for-display* ((system-definer (eql :asdf)) system-name)
  (symbol-name system-name))

#+ASDF
(defmethod system-source-file* ((system-definer (eql :asdf)) system-name)
  (let ((system (find-system system-name)))
    (make-pathname 
     :type "asd"
     :name (safe-slot-value system 'asdf::name)
     :defaults (safe-slot-value system 'asdf::relative-pathname))))

#+ASDF
(defmethod ensure-system-name* ((system-definer (eql :asdf)) system-name)
  (form-keyword (asdf::coerce-name system-name)))

#+ASDF
(defmethod ensure-system-name* ((system-definer (eql :asdf)) (system asdf:system))
  (safe-slot-value system 'asdf::name))

#+ASDF
(defmethod filename-looks-like-system-file-p* ((system-definer (eql :asdf)) filename)
  (string-equal "asd" (pathname-type filename)))

#+ASDF
(defmethod system-dependencies* ((system-definer (eql :asdf)) system-name)
  (mapcar
    #'form-keyword 
    (cdadr (assoc 'asdf::load-op 
                  (safe-slot-value (asdf:find-system system-name) 
                                   'asdf::in-order-to)))))

#+ASDF
(defun safe-slot-value (thing name)
  (when (slot-boundp thing name)
    (slot-value thing name)))

;;; ---------------------------------------------------------------------------

#+:GLU-GENERIC-LOAD-UTILS
(defclass glu-system ()
  ((name :initarg :name :accessor glu-name)))

#+:GLU-GENERIC-LOAD-UTILS
(defmethod registered-systems* ((system-definer (eql :glu)))
  user:*glu-defined-systems*)

#+:GLU-GENERIC-LOAD-UTILS
(defmethod loaded-systems* ((system-definer (eql :glu)))
  user:*glu-loaded-systems*)

#+:GLU-GENERIC-LOAD-UTILS
(defmethod system-source-file* ((system-definer (eql :glu)) system-name)
  (user:glu-system-source-file (ensure-system-name system-name)))

#+:GLU-GENERIC-LOAD-UTILS
(defmethod associated-test-system* ((system-definer (eql :glu)) system-name)
  (user:glu-system-test-system system-name))

#+:GLU-GENERIC-LOAD-UTILS
(defmethod top-level-system-p* ((system-definer (eql :glu)) system-name)
  (user:glu-system-top-level system-name))

#+:GLU-GENERIC-LOAD-UTILS
(defmethod system-name-for-display* ((system-definer (eql :glu)) system-name)
  (user::glu-system-name system-name))

#+:GLU-GENERIC-LOAD-UTILS
(defmethod ensure-system-name* ((system-definer (eql :glu)) system-name)
  (user::canonicalize-glu-system-name system-name))

#+(and ASDF GLU-GENERIC-LOAD-UTILS)
;; a bit weird
(defmethod ensure-system-name* ((system-definer (eql :glu)) (system asdf:system))
  (user::canonicalize-glu-system-name 
   (ensure-system-name* :asdf system)))

#+GLU-GENERIC-LOAD-UTILS
(defmethod filename-looks-like-system-file-p* ((system-definer (eql :glu)) filename)
  (string-equal "system" (pathname-type filename))) 
                
#+GLU-GENERIC-LOAD-UTILS
(defmethod system-dependencies* ((system-definer (eql :glu)) system-name)
  (user:glu-system-all-subsystems system-name)) 
                
#+EKSL-GENERIC-LOAD-UTILS
(defmethod find-system-for-mapping* ((system-definer (eql :glu)) system-name)
  (let ((system (find-system* system-definer system-name)))
    (when system
      (make-instance 'glu-system :name system))))

;;; ---------------------------------------------------------------------------

(defun registered-systems ()
  (registered-systems* (select-system-definer nil)))

;;; ---------------------------------------------------------------------------

(defun loaded-systems ()
  (loaded-systems* (select-system-definer nil)))

;;; ---------------------------------------------------------------------------

(defun find-system (system-name)
  (find-system* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun associated-test-system (system-name)
  (associated-test-system* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun system-sub-systems (system-name sub-projects?)
  (system-sub-systems* (select-system-definer system-name) 
                       (ensure-system-name system-name) sub-projects?))

;;; ---------------------------------------------------------------------------

(defun system-source-file (system-name)
  (system-source-file* (select-system-definer system-name) (ensure-system-name system-name)))

;;; ---------------------------------------------------------------------------

(defun top-level-system-p (system-name)
  (top-level-system-p* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun system-name-for-display (system-name)
  (system-name-for-display* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun ensure-system-name (system-name)
  (ensure-system-name* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------

(defun filename-looks-like-system-file-p (filename)
  (filename-looks-like-system-file-p* (select-system-definer nil) filename))

;;; ---------------------------------------------------------------------------

(defun system-dependencies (system-name)
  (system-dependencies* (select-system-definer system-name) system-name))

;;; ---------------------------------------------------------------------------
;;; level-1
;;; ---------------------------------------------------------------------------

(defun system-source-directory (system-name)
  (make-pathname 
   :name nil 
   :type nil
   :defaults (system-source-file* (select-system-definer system-name)
                                  (ensure-system-name system-name))))

;;; ---------------------------------------------------------------------------

(defun collect-system-dependencies (systems)
  "What's in a name...? This form attempts to find and load all of the referenced
system files starting with any systems in systems. It does not include the systems
themselves unless they are syb-systems of some other system in the list."
  (let ((systems-processed nil)
        (result nil))
    (Labels ((compute-system-closure (system &optional (save? t))
               (setf system (ensure-system-name system))
               (unless (member system systems-processed)
                 (push system systems-processed)
                 (when save?
                   (push system result))
                 (dolist (sub-system (system-dependencies system))
                   (setf sub-system (ensure-system-name sub-system))
                   (when save?
                     (push sub-system result))
                   (compute-system-closure sub-system)))))
      
      (setf systems-processed nil)
      (dolist (system (if (consp systems) systems (list systems)))
        (compute-system-closure system nil)))
    (remove-duplicates result)))

;;; ---------------------------------------------------------------------------

(defun map-system-dependencies (systems fn)
  ;; this is weak in that it cones up the whole list and then maps that 
  ;; but efficiency is probably not a big concern here...
  (mapc fn (collect-system-dependencies systems)))

;;; ---------------------------------------------------------------------------

(defmethod map-system-files ((system-names list) function &rest args &key 
                             (system-closure? nil) &allow-other-keys)
  (dolist (system (if system-closure? 
                    (collect-system-dependencies system-names)
                    (if (consp system-names) system-names (list system-names))))
    (let ((system-name (ensure-system-name system)))
      (apply #'map-system-files system-name function args))))

;;; ---------------------------------------------------------------------------

(defmethod map-system-files ((system-name symbol) function &rest args
                             &key &allow-other-keys)
  (apply #'map-system-files (find-system system-name) function args))


#+GLU-GENERIC-LOAD-UTILS
(defmethod map-system-files ((system glu-system) function &rest args 
                             &key system-closure? include-pathname?
                             &allow-other-keys)
  (when system-closure?
    (dolist (system (collect-system-dependencies (glu-name system)))
      (apply #'map-system-files system function args)))
  
  (user:map-glu-system-files 
   (nconc (list (glu-name system))
          #+Ignore
          (when include-tests?
            (list (associated-test-system system-name))))
   function 
   :system-closure? nil
   :include-pathname? include-pathname?
   :include-associates? (get-setting :project-manager :show-associates?)))

;;; ---------------------------------------------------------------------------

#+ASDF
(defmethod map-system-files ((module asdf:module) function &rest args 
                             &key &allow-other-keys)
  (let ((*relative-pathname* 
         (make-pathname 
          :directory (append (pathname-directory *relative-pathname*)
                             (list (safe-slot-value module 'asdf::name))))))
    (mapc (lambda (thing)
            (apply #'map-system-files thing function args)) 
          (safe-slot-value module 'asdf::components))))

;;; ---------------------------------------------------------------------------

#+ASDF
(defmethod map-system-files ((system asdf:system) function &rest args 
                             &key system-closure? &allow-other-keys)
  (when system-closure?
    (dolist (system (collect-system-dependencies system))
      (apply #'map-system-files system function args)))
  
  (let ((*relative-pathname* (safe-slot-value system 'asdf::relative-pathname)))
    (mapc (lambda (thing)
            (apply #'map-system-files thing function args)) 
          (safe-slot-value system 'asdf::components))))

;;; ---------------------------------------------------------------------------

#+ASDF
(defmethod map-system-files ((file asdf:source-file) function  
                             &key (include-pathname? nil) &allow-other-keys)
  (let ((true-name 
         (probe-file
          (make-pathname :name (safe-slot-value file 'asdf::name)
                         :defaults (if include-pathname?
                                     *relative-pathname* nil)
                         :type (asdf:source-file-type 
                                file (safe-slot-value file 'asdf::parent))))))
    (when true-name
      (funcall function true-name))))


;;; ---------------------------------------------------------------------------

(defun collect-system-files (system-names &key 
                                               (system-closure? nil)
                                               (include-pathname? nil)
                                               (include-associates? nil)
                                               (include-files? t)
                                               (options nil))
  (let ((result nil))
    (map-system-files system-names 
                      (lambda (f)
                        (push f result))
                      :system-closure? system-closure?
                      :include-pathname? include-pathname?
                      :include-associates? include-associates?
                      :include-files? include-files?
                      :options options)
    (values (nreverse result))))


;;; ---------------------------------------------------------------------------
;;; GLU
;;; ---------------------------------------------------------------------------

#+:GLU-GENERIC-LOAD-UTILS
(defmethod system-sub-systems* ((system-definer (eql :glu)) system-name sub-projects?)
  (collect-system-dependencies system-name)
  #+Old
  (let ((sorted-systems (sorted-glu-systems))
        (sub-systems (delete-if
                      (lambda (s)
                        (and (not sub-projects?)
                             (not (eq system-name 
                                      (glu-system-top-level-system s sorted-systems)))))
                      (collect-system-dependencies system-name))))
    (values sub-systems)))

;;; ---------------------------------------------------------------------------

#+(and Ignore GLU-GENERIC-LOAD-UTILS)
(defun glu-system-top-level-system (system-name sorted-systems)
  (argmin (user:glu-system-top-level-systems system-name)
          (lambda (x)
            (position x sorted-systems))))

;;; ---------------------------------------------------------------------------

#+(and Ignore GLU-GENERIC-LOAD-UTILS)
;;?? also found in grapher-examples
(defun make-systems-graph (&optional (systems (registered-systems)))
  "Creates a GRAPH-CONTAINER that is the dependency graph for the given
  SYSTEMS."
  (let* ((canon-systems (mapcar #'ensure-system-name systems))
         (graph (make-instance 'graph-container
                  :initial-contents canon-systems
                  :default-edge-type :directed))
         (done nil))
    (labels ((show-parents (system)
               (unless (member system done)
                 (push system done)
                 (dolist (parent (get system :glu-system-depends-on))
                   (let ((pn (ensure-system-name parent)))
                     (add-edge-between-vertexes graph pn system)
                     (show-parents pn))))))
      (mapc #'show-parents canon-systems)
      (values graph))))

;;; ---------------------------------------------------------------------------

#+(and Ignore GLU-GENERIC-LOAD-UTILS)
(defun sorted-glu-systems ()
  (mapcar #'element (topological-sort (make-systems-graph))))

;;; ---------------------------------------------------------------------------

;;?? very expensive
(defun pathname-for-system-file (system filename)
  (let ((name (pathname-name filename))
        (type (pathname-type filename)))
    (block look-for-it
      (map-system-files 
       system
       (lambda (pathname)
         (when (and (string-equal (pathname-name pathname) name)
                    (string-equal (pathname-type pathname) type))
           (return-from look-for-it pathname)))
       :include-pathname? t
       :system-closure? t)
      
      ;; no dice
      (values nil))))
