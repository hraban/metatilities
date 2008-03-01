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

(defun relative-pathname (relative-to pathname &key name type)
  (let ((directory (pathname-directory pathname)))
    (when (eq (car directory) :absolute)
      (setf (car directory) :relative))
    (merge-pathnames
     (make-pathname :name (or name (pathname-name pathname))
                    :type (or type (pathname-type pathname))
                    :directory directory
		    )
     relative-to)))

(defun directory-pathname-p (pathname)
  (and (member (pathname-name pathname) (list nil :unspecified))
       (member (pathname-type pathname) (list nil :unspecified))))

(defun ensure-directory-pathname (pathname)
  (if (directory-pathname-p pathname)
      pathname
      (make-pathname
       :directory `(,@(pathname-directory pathname) 
		      ,(namestring (pathname-name+type pathname))))))

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

(defmethod make-stream-from-specifier ((stream-specifier string) 
				       (direction (eql :output)) &rest args)
  (apply #'make-stream-from-specifier
	 (pathname stream-specifier) direction args))

(defmethod close-stream-specifier (s)
  (close s)
  (values nil))

(defmethod close-stream-specifier ((s string-stream))
  (prog1 
    (values (get-output-stream-string s)) 
    (close s)))

;;;;

(defmethod map-forms (input fn &key (ignore-read-errors-p t))
  (with-input (stream input)
    (flet ((next ()
	     (if ignore-read-errors-p
		 (ignore-errors (read stream nil :eof))
		 (read stream nil :eof))))
      (loop for f = (next) then (next)   
	 until (eq f :eof) do
	 (handler-case
	     (funcall fn f)
	   (reader-error (c) (print c)))))))

(defun map-lines (input fn &key include-empty-lines-p filter)
  (with-input (s input)
    (loop for line = (read-line s nil :eof)
       until (eq line :eof)
       when (and 
	     (or include-empty-lines-p
		 (some (complement 'whitespacep) line))
	     (or (not filter)
		 (funcall filter line)))
       do (funcall fn line))))

(defun collect-forms (input &key filter transform)
  (let ((result nil))
    (map-forms input (lambda (form) 
		       (when (or (not filter) 
				 (funcall filter form))
			 (push (if transform (funcall transform form) form)
			       result))))
    (nreverse result)))

(defun collect-lines (input &rest args &key 
		      count-empty-lines-p filter transform)
  (declare (ignore count-empty-lines-p filter))
  (unless transform (setf transform #'identity))
  (remf args :transform)
  (let ((results nil))
    (apply #'map-lines
	   input (lambda (line) (push (funcall transform line) results))
	   args)
    (nreverse results)))
