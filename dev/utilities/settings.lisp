(in-package #+MCL ccl #+allegro sys #-(or MCL allegro) cl-user)

#|

Some way of telling that the directory returned by system-settings-file was valid

|#

(export '(get-setting
          save-settings
          empty-settings
          load-settings
          
          *system-settings-file-name*))

;;; ---------------------------------------------------------------------------
;;; incredibly simple preference thingie
;;; ---------------------------------------------------------------------------

(declaim (special *system-settings* *system-settings-file-name*)) 

;;; ---------------------------------------------------------------------------

(defun make-settings-hash-table ()
  (make-hash-table :test 'equal))

;;; ---------------------------------------------------------------------------

(defvar *system-settings* (make-settings-hash-table))

;;; ---------------------------------------------------------------------------

(defvar *system-settings-file-name* nil)

;;; ---------------------------------------------------------------------------

(defun get-setting (section name &optional default)
  (gethash (cons section name) *system-settings* default))

;;; ---------------------------------------------------------------------------

(defun set-setting (section name value)
  (setf (gethash (cons section name) *system-settings*) value))

;;; ---------------------------------------------------------------------------

(defun (setf get-setting) (value section name)
  (set-setting section name value)
  value)

;;; ---------------------------------------------------------------------------

(defun system-settings-file ()
    #+(and MCL (not OPENMCL))
    (let ((app-name (or *system-settings-file-name*
                        (current-app-name)))
          (host (handler-case 
                (progn (probe-file (make-pathname :host "preferences"))
                       "preferences")
                (condition (c) (declare (ignore c)) "ccl"))))
      (make-pathname
       :host host
       :type "settings"
       :name (subseq app-name 0 (min (length app-name) (- 31 (+ 1 8))))))
    #+OPENMCL
    (or *system-settings-file-name*
        "~/settings.lisp")
    #-MCL
    (or *system-settings-file-name*
        "settings.lisp"))

;;; ---------------------------------------------------------------------------

(defun save-settings (&optional (filename (system-settings-file)))
  (handler-case 
    (with-open-file (stream filename
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((result nil))
        (maphash (lambda (k v)
                   (push (list k v) result))
                 *system-settings*)
        (with-standard-io-syntax 
          (if result
            (format stream "~%~S" result)
            (format stream "")))))
    (error (c) 
           #+Ignore (declare (ignore c))
           (print c)
           (return-from save-settings))))

;;; ---------------------------------------------------------------------------

(defun load-settings (&optional (filename (system-settings-file)))
  (when (probe-file filename)
    (with-open-file (stream filename
                            :direction :input)
      (let ((data (read stream nil :eof nil)))
        (unless (eq data :eof)
          (loop for (k v) in data do
                (set-setting (car k) (cdr k) v)))))))

;;; ---------------------------------------------------------------------------

(defun empty-settings ()
  (setf *system-settings* (make-settings-hash-table)))

;;; ---------------------------------------------------------------------------

(defun list-settings ()
  (let ((result nil))
    (maphash (lambda (k v)
               (push (list (car k) (cdr k) v) result))
             *system-settings*)
    (sort
     result
     #'(lambda (a b)
         (when (eq (type-of a) (type-of b))
           (typecase a
             ((or string symbol) (funcall #'string-lessp a b))
             (number (funcall #'< a b))
             (t nil))))
     :key #'first)))

;;; ---------------------------------------------------------------------------

#+MCL
(pushnew 'save-settings *lisp-cleanup-functions*)

;;; ---------------------------------------------------------------------------

(load-settings)