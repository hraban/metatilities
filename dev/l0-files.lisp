(in-package #:metatilities)

(defun pathname-name+type (pathname)
  "Returns a new pathname consisting of only the name and type from a non-wild pathname."
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun physical-pathname-directory-separator ()
  "Returns a string representing the separator used to delimit directories in a physical pathname. For example, Digitool's MCL would return \":\" whereas OpenMCL would return \"/\"."
  (let* ((directory-1 "foo")
         (directory-2 "bar")
         (pn (namestring
              (translate-logical-pathname
               (make-pathname
                :host nil
                :directory `(:absolute ,directory-1 ,directory-2)
                :name nil
                :type nil))))
         (foo-pos (search directory-1 pn :test #'char-equal))
         (bar-pos (search directory-2 pn :test #'char-equal)))
    (subseq pn (+ foo-pos (length directory-1)) bar-pos)))

(defgeneric make-stream-from-specifier (specifier direction &rest args)
  (:documentation "Create and return a stream from specifier, direction and any other argsuments"))

(defgeneric close-stream-specifier (steam)
  (:documentation "Close a stream and handle other bookkeeping as appropriate."))

(defmethod make-stream-from-specifier ((stream-specifier stream) 
				       (direction symbol) &rest args)
  (declare (ignore args))
  (values stream-specifier nil))

(defmethod make-stream-from-specifier ((stream-specifier (eql t)) 
				       (direction symbol) &rest args)
  (declare (ignore args))
  (values *standard-output* nil))

(defmethod make-stream-from-specifier ((stream-specifier (eql nil)) 
				       (direction symbol) &rest args)
  (declare (ignore args))
  (values (make-string-output-stream) t))

(defmethod make-stream-from-specifier ((stream-specifier (eql :none)) 
				       (direction symbol) &rest args)
  (declare (ignore args))
  (values nil nil))

(defmethod make-stream-from-specifier ((stream-specifier pathname) 
				       (direction symbol) &rest args)
  (values (apply #'open stream-specifier :direction direction args)
          t))

(defmethod make-stream-from-specifier ((stream-specifier string) 
				       (direction symbol) &rest args)
  (declare (ignore args))
  (values (make-string-input-stream stream-specifier) nil))

(defmethod close-stream-specifier (s)
  (close s)
  (values nil))

(defmethod close-stream-specifier ((s string-stream))
  (prog1 
    (values (get-output-stream-string s)) 
    (close s)))
