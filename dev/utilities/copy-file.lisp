(in-package #:metatilities)
        
(define-condition source/target-file-error (file-error)
                  ((pathname :reader source-pathname
                             :initarg :source-pathname)
                   (target-pathname :reader target-pathname 
                                    :initarg :target-pathname :initform nil))
  (:report (lambda (c s)
             (format s "Copy of ~S to ~S failed" 
                     (source-pathname c) (target-pathname c))))
  (:documentation "General condition for file errors that have a source and target."))

;;; ---------------------------------------------------------------------------

(define-condition source/target-target-already-exists-error (source/target-file-error)
                  ()
  (:report (lambda (c s)
             (format s "File action failed because target ~S already exists"
                     (target-pathname c))))
  (:documentation "This error is signaled when the target pathname already exists."))

;;; ---------------------------------------------------------------------------

(define-condition source/target-source-does-not-exist-error
    (source/target-file-error)
                  ()
  (:report (lambda (c s)
             (format s "File action failed because source ~S does not exist"
                     (source-pathname c))))
  (:documentation "This error is signaled when the source file does not exist."))

;;; ---------------------------------------------------------------------------

(defun copy-file (from to &key (if-does-not-exist :error)
                       (if-exists :error))
  "Copies the file designated by the non-wild pathname designator FROM
to the file designated by the non-wild pathname designator TO. The following
keyword parameters are supported:

* :if-exists
    this can be either :supersede or :error (the default). If it is :error then
a source/target-target-already-exists-error will be signaled if the file designated
by the TO pathname already exists.

* :if-does-not-exist 
    this can be either :ignore or :error (the default). If it is :error then
a source/target-source-does-not-exist-error will be signaled if the FROM pathname
designator does not exist.
"
  (assert (member if-exists '(:error :supersede))
          nil
          "The if-exists keyword parameter must be one of :error or :supersede. It is currently set to ~S" 
          if-exists)
  (assert (member if-does-not-exist '(:error :ignore))
          nil
          "The if-does-not-exist keyword parameter must be one of :error or :ignore. It is currently set to ~S" 
          if-does-not-exist)
  (ensure-directories-exist to)
  (cond ((probe-file from)
         #+:allegro 
         (excl.osi:copy-file 
	  from to 
	  :overwrite (if (eq if-exists :supersede) :ignore nil)) 
         #-:allegro
         (let ((element-type #-:cormanlisp '(unsigned-byte 8)
                             #+:cormanlisp 'unsigned-byte))
           (with-open-file (in from :element-type element-type)
             (with-open-file (out to :element-type element-type
                                  :direction :output
                                  :if-exists if-exists)
               (unless out
                 (error (make-condition 'source/target-target-already-exists
                                        :pathname from
                                        :target-pathname to)))
               (copy-stream in out))))
         (values t))
        (t
         ;; no source file!
         (ecase if-does-not-exist
           ((:error) (error 'source/target-source-does-not-exist-error
                            :pathname from :target-pathname to))
           ((:ignore) nil)))))

(defun move-file (from to &rest args &key (if-does-not-exist :error)
		  (if-exists :error))
  (declare (dynamic-extent args)
	   (ignore if-exists if-does-not-exist))
  (when (apply #'copy-file from to args)
    (delete-file from)))