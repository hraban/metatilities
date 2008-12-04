(in-package #:metatilities)

(defconstant +mac-os-filename-limit+ 31)

(defgeneric map-lines-in-file (function file-specifier)
  (:documentation "Reads the file to which file-specifier resolves one line at a time \(using read-line\) and applies `function` to each line. File-specifier can be a string pointing to a file, a logical or physical pathname or a stream."))

(defgeneric map-forms-in-file (function file-specifier)
  (:documentation "Reads file one form at a time \(using read\) and applies `function` to each one in turn."))

#+(or)
;; it's hard to imagine anyone else wanting this.
(defun nicely-format-filename (file stream &key
                                    (depth 2) (use-ellipsis? nil) (show-type? t) 
                                    (initial-ellipsis? nil))
  "Write out a representation of file to stream. There are options to show or hide the type of the file, to display only part of the files directory structure and to include elipses if the directory shown is not complete."
  (let* ((directories (pathname-directory file))
         (kind (first directories))
         (directories (rest directories)))
    (declare (ignore kind))
    (if use-ellipsis?
      (format stream "~(~a~@[.~A~] {~a:...:~{~a:~}}~)"
              (pathname-name file)
              (and show-type? (pathname-type file))
              (first directories)
              (last directories (1- depth)))
      (format stream "~(~a~@[.~A~] {~@[...:~]~{~a:~}}~)"
              (pathname-name file)
              (and show-type? (pathname-type file))
              (and initial-ellipsis? (<= (length directories) depth))
              (last directories depth)))))

#|
(lift:deftestsuite test-nff ()
  ())

(addtest (test-nff)
  (ensure-cases (file depth use-ellipsis? show-type? initial-ellipsis? result) 
      '((#p"/a/b/c/d/e/f.g" 2 nil t nil "f.g {d:e:}")	
	(#p"/a/b/c/d/e/f.g" 2 nil nil nil "f {d:e:}")
	(#p"/a/b/c/d/e/f.g" 100 nil nil nil "f {a:b:c:d:e:}")
	(#p"/a/b/c/d/e/f.g" 0 nil nil nil "f {}")
	(#p"/a/b/c/d/e/f.g" 2 t t nil "f.g {a:...:e:}")
	)
    (ensure-same 
     (nicely-format-filename file nil :depth depth
			     :use-ellipsis? use-ellipsis?
			     :show-type? show-type?
			     :initial-ellipsis? initial-ellipsis?)
     result :test 'string=)))
|#

(defun file-to-list (&optional (pathname (choose-file-question)))
  "Convert a file into a list by opening it and calling read repeatedly."
  (let ((eof (gensym)))
    (with-open-file (in pathname)
      (loop for line = (read in nil eof)
            while (not (eq line eof))
            collect line))))

#+GLU-GENERIC-LOAD-UTILS
;; Add this to experiment interface someday.
(defun conjure-up-filename (&optional (prefix "FILE-") (type "lisp"))
  "Return a string representing a filename whose value consists of the `perfix`, followed by a representation of the date and time, followed by an file type."
  (multiple-value-bind (ignore minute hour date month year) (get-decoded-time)
    (declare (ignore ignore))
    (format nil "~a~a~2,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~@[.~a~]"
            (load-time-value  ; #. is not the same as load-time-value
             (namestring (make-pathname :directory 
                                        (append (pathname-directory (user::load-pathname))
                                                '("data")))))
            prefix
            date
            (month->string month :short)
            (- year 2000) ;; Y3K bug
            hour
            minute
            type)))

(defun unique-file-name-from-date (name &key (date (get-universal-time))
                                        (type "lisp"))
  "Returns a namestring whose suffix is the `date` in the form YYMMMDDHHMMSS. The names prefix will include as much of the original name as possible given the limitations of the underlying OS. The name is _not_ guaranteed to be unique. [[Bad name]]."
  (bind:bind (((:values second minute hour date month year)
	       (decode-universal-time date))
         (date-part 
          (format nil "~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d~2,'0d"
                  year (month->string month :short) date hour minute second)))
    (namestring
     (make-pathname 
      :name (format nil "~A-~A" 
                    (string-right-trim
                     "-"
                     (subseq name 0 (min 
                                     (- (maximum-filename-length) 
                                        2                 ; the '.' and the '-'
                                        (length type) 
                                        (length date-part))
                                     (length name))))
                    date-part)
      :type type))))

(defun pretty-namestring-from-date (prefix &optional (date (get-universal-time)))
  "Returns a representation of the date \(which defaults to the current date and time\) preceeded by a prefix. The date is printed as MM/DD/YYYY."
  (multiple-value-bind (second minute hour date month year) (decode-universal-time date)
    (declare (ignore second minute hour))
    (uniquify-file-name
     (format nil "~a-~2,'0d/~2,'0d/~4d"
             prefix
             month
             date
             year))))

;;; (pretty-namestring-from-date 'foo) =>"FOO-12/17/2004"

(defun eos-namestring-from-date (prefix &optional (date (get-universal-time)))
  "forms a namestring based on date and time, in the form of
<perfix>-09JUN03-010903 where 010903 is read as 1:09:03"
  ;;excuse the presumption that this is prettier -- jjm moody@cs.umass.edu
  (multiple-value-bind (second minute hour day month year) 
                       (decode-universal-time date)
    (format nil "~a-~2,'0d~2,'0d~2,'0d-~2,'0d~2,'0d~2,'0d"
            prefix
            day
            (month->string month :short)
            (- year 2000) ;; Y3K bug
            hour
            minute
            second)))

(defun short-eos-namestring-from-date (prefix &optional 
                                              (date (get-universal-time)))
  (bind:bind ((str (eos-namestring-from-date prefix date)))
   (substring str 0 (- (length str) 2))))

;; Rename the file based on the time it was created...
(defun rename-file-if-present (filename)
  "Renames a file to a unique name based on its file-write-date. See unique-file-name-from-date."
  (when (probe-file filename)
    (rename-file filename
                 (merge-pathnames 
                  (make-pathname :name (unique-file-name-from-date 
                                        (pathname-name filename)
                                        :date (file-write-date filename)))
                  filename))))

(defgeneric uniquify-file-name (file-specifier)
  (:documentation "Returns a file name that is not currently in use. The strategy used if there is a conflict is to append an integer to the name component until there is no conflict. This could fail in multi-threaded situations."))

(defmethod uniquify-file-name ((namestring pathname))
  (uniquify-file-name (namestring namestring)))

(defmethod uniquify-file-name ((namestring string))
  "Returns a file name with the prefix NAMESTRING but which is guarateed not to
   exist."
  (let ((num-files (length
                    (directory
                     (concatenate 'string
                                  namestring
                                  "*")))))
    (if (> num-files 0)
      (make-pathname 
       :name (concatenate
              'string (pathname-name namestring) "-" (prin1-to-string num-files))
       :defaults namestring)
      namestring)))

(defun map-files (wildcarded-file-spec function &rest args)
  "Apply the function to all of the files in wildcarded-file-spec. Any
additional args are passed along to the function." 
  (dolist (file (directory #+MCL 
                           (ensure-wild-file-spec wildcarded-file-spec)
                           #-MCL
                           wildcarded-file-spec))
    (apply function file args)))

(defmethod map-forms-in-file (fn (filename string))
  (with-open-file (stream filename
                          :direction :input)
    (map-forms-in-file fn stream)))

(defmethod map-forms-in-file (fn (filename pathname))
  (with-open-file (stream filename
                          :direction :input)
    (map-forms-in-file fn stream)))

(defmethod map-forms-in-file (fn stream)
  (loop for f = (read stream nil :eof) then (read stream nil :eof)   
        until (eq f :eof) do
        (handler-case
          (funcall fn f)
          (reader-error (c) (print c)))))

(defmethod map-lines-in-file (fn (filename string))
  (with-open-file (stream filename
                          :direction :input)
    (map-lines-in-file fn stream)))

(defmethod map-lines-in-file (fn (filename pathname))
  (with-open-file (stream filename
                          :direction :input)
    (map-lines-in-file fn stream)))

(defmethod map-lines-in-file (fn stream)
  (flet ((get-next-line ()
           (read-line stream nil :eof nil)))
    (loop for l = (get-next-line) then (get-next-line)
          until (eq l :eof) do
          (funcall fn l))))

(defmacro iterate-lines (fn source)
  "An enpowered version of map-lines-in-file. This one is a macro
so that you can use (return) to stop processing and so that you can
use (next-line) within your code to advance to the next line of the 
source." 
  (with-variables (stream close-stream? line)
    `(block nil
       (let ((,stream nil)
             (,close-stream? t))
         (cond ((typep ,source 'stream)
                (setf ,close-stream? nil)
                (setf ,stream ,source))
               (t
                (setf ,stream (open ,source :direction :input))))
         (unwind-protect
           (flet ((%next-line ()
                    (read-line ,stream nil :eof nil)))
             (macrolet ((next-line ()
                          `(%next-line)))
               (loop for ,line = (next-line) then (next-line)
                     until (eq ,line :eof) do
                     (funcall ,fn ,line))))
           (when ,close-stream?
             (close ,stream)))))))

#+(and mcl (not openmcl))
(defun fix-type-and-creator (&optional (wildcarded-file-spec (choose-directory-question)))
  (map-files (ensure-wild-file-spec wildcarded-file-spec)
             (lambda (f)
               (let ((type (string-upcase (pathname-type f))))
                 (cond ((string-equal type "LISP")
                        (ccl:set-mac-file-type f :text)
                        (ccl:set-mac-file-creator f :ccl2))
                       ((or (string-equal type "PFSL")
                            (string-equal type "CFSL"))
                        (ccl:set-mac-file-type f :pfsl)
                        (ccl:set-mac-file-creator f :ccl2))
                       (t
                        nil))))))

;;?? May be MCL specific; :wild is not necessarily portable
(defun ensure-wild-file-spec (file-spec)
  (if (wild-pathname-p file-spec)
    file-spec
    (merge-pathnames 
     (make-pathname :name :wild :type :wild)
     file-spec)))

(defun remove-dead-versions (wildcarded-file-spec &key (delete? nil))
  (let ((candidates nil))
    (map-files
     (ensure-wild-file-spec wildcarded-file-spec)
     (lambda (f)
       (let* ((file-name (namestring f))
              (position (search "~" file-name :from-end t)))
         (when (and position
                    (ignore-errors 
                     (parse-integer (subseq file-name (1+ position)))))
           (push f candidates)))))
    (when candidates
      (print candidates))
    (when (and candidates delete?)
      (dolist (file candidates)
        (delete-file file)))))

#+MCL
(defun fix-crlfs (wildcarded-file-spec)
  (map-files
   (ensure-wild-file-spec wildcarded-file-spec)
   (lambda (f)
     (convert-newlines f :no-query t :verbose? nil))))

#+MCL
(defun unixify-crlfs (wildcarded-file-spec)
  (map-files
   (ensure-wild-file-spec wildcarded-file-spec)
   (lambda (f)
     (convert-newlines-to-unix f :no-query t :verbose? nil))))

#+MCL
(defun convert-newlines (infile &key outfile (replace? (not outfile)) 
                                no-query report-stream 
                                (verbose? nil))
  (unless outfile
    (setf outfile (ccl::gen-file-name infile)))
  (when (or no-query
            (y-or-n-p "~&; Reading ~a and writing ~a; Ok?" infile outfile))
    (when report-stream
      (format report-stream "~&;; Converting ~a." infile))
    (when verbose? 
      (princ infile))
    (with-open-file (instream infile :direction :input)
      (with-open-file (outstream outfile 
                                 :direction :output 
                                 :if-exists :new-version)
        (do ((char (read-char instream nil) (read-char instream nil)))
            ((null char))
          (if (char= char #\linefeed)
            (write-char #\newline outstream)
            ;; Otherwise, write the character.
            (write-char char outstream))))))
  (when replace?
    (rename-file outfile infile :if-exists :supersede)))

#+MCL
(defun convert-newlines-to-unix (infile &key outfile (replace? (not outfile)) 
                                no-query report-stream 
                                (verbose? nil))
  (unless outfile
    (setf outfile (ccl::gen-file-name infile)))
  (when (or no-query
            (y-or-n-p "~&; Reading ~a and writing ~a; Ok?" infile outfile))
    (when report-stream
      (format report-stream "~&;; Converting ~a." infile))
    (when verbose? 
      (princ infile))
    (with-open-file (instream infile :direction :input)
      (with-open-file (outstream outfile 
                                 :direction :output 
                                 :if-exists :new-version)
        (do ((char (read-char instream nil) (read-char instream nil)))
            ((null char))
          (if (char= char #\newline)
            (write-char #\linefeed outstream)
            ;; Otherwise, write the character.
            (write-char char outstream))))))
  (when replace?
    (rename-file outfile infile :if-exists :supersede)))

(defun file-package (pathname)
  "Tries to determine the package of a file by reading it one form at a time and looking for in-package forms. Though it does handle the case of a file with multiple in-package and defpackages forms -- in which case it returns the last in-package encountered -- it  not handle the general case of files with multiple in-package forms."
  (let ((putative-package nil)) 
    (flet ((stop-form-p (form)
             (and (consp form)
                  (not (member (first form) '(defpackage))))))
      (map-forms-in-file 
       (lambda (form)
         (cond ((and (consp form)
                     (eq (first form) 'in-package))
                (setf putative-package 
                      (intern (package-name (find-package (second form)))
                              :keyword)))
               ((and putative-package (stop-form-p form))
                (return-from file-package putative-package))))
       pathname))))

#+New
(defun file-package (pathname &key (ignore-errors? t))
  (handler-bind* 
   ((error (lambda (c) 
             (print c)
             (cond (ignore-errors?
                    (print :resume)
                    (:resume))
                   (t
                    (print :error)
                    (error c))))))
   (map-forms-in-file 
    (lambda (form)
      (when (and (consp form)
                 (eq (first form) 'in-package))
        (return-from file-package (second form))))
    pathname)))

(defvar *glu-blast-pathname-defaults*
  "glu:root;**;*.*")

(defvar *glu-blast-default-selector*
  (constantly nil))

(defun glu-blast (&optional (deletep *glu-blast-default-selector*)
                   (pathname-defaults *glu-blast-pathname-defaults*)
                   (delete-fn #'delete-file))
  "A tools for wiping out files en masse, according to a predicate.  Use this guy ~
   at your own risk."
  (map-files pathname-defaults
             #'(lambda (pathname)
                 (when (funcall deletep pathname)
                   (format t "~&;Deleting: ~s" pathname)
                   (funcall delete-fn pathname))))) 

(defun pathname-is-old-cvs-junk-p (f)
  (let* ((file-name (namestring f))
         (position (search "~" file-name :from-end t)))
    (and position
         (ignore-errors 
          (parse-integer (subseq file-name (1+ position)))))))


#+Test
(glu-blast #'pathname-is-old-cvs-junk-p  
            (format nil "~A;**;*.*" user::*glu-search-systems-root*)
            #'print)
#+Test
(glu-blast #'pathname-is-old-cvs-junk-p  
            (format nil "~A;**;*.*" user::*glu-search-systems-root*))

#+DIGITOOL
(defun touch-file (file)
  "Updates the file-write-date of `file`."
  (if (probe-file file)
    (ccl::set-file-write-date file (get-universal-time))
    (with-new-file (out file)
      (format out ""))))

(defparameter *filename-escape-characters* 
  (list #\/ #\* #\\ #\  #\< #\> #\@ #\. #\: #\( #\) #\& #\ ))

(defun ensure-filename-safe-for-os (name)
  (let* ((array (make-array (* 2 (length name)) :fill-pointer 0 :adjustable t)))
    (labels ((add-char (ch)
             (vector-push-extend ch array))
           (escape-char (index)
             (add-char #\-)
             (add-char (code-char (+ (char-code #\a) index)))))
      (loop for ch across name do
            (acond ((char-equal ch #\-)
                    (add-char #\-)
                    (add-char #\-)) 
                   ((position ch *filename-escape-characters*)
                    (escape-char (1+ it)))
                   (t
                    (add-char ch))))
      (coerce array 'string))))

;; we may want to conditionalize on OS here...
(defun good-filename-char-p ( char )
  "Returns T if CHAR is legal in a filename."
  (or
   (alphanumericp char)
   (find char "- ")))

(defun remove-illegal-filename-characters (name)
  "Removes illegal characters from the file name NAME."
  (remove-if-not #'good-filename-char-p name))

(defgeneric shorten-filename-for-os (file-specifier)
  (:documentation "Returns a file-name for file-specifier such that it is a valid name for the current underlying OS. Mainly, this means ensuring that the name component is not too long."))

(defmethod shorten-filename-for-os ((name pathname))
  (shorten-filename-for-os (namestring name)))

(defmethod shorten-filename-for-os ((name string))
  "Returns a unique _and_ legal filename for an OS."
  ;; stupid 32-character limit
  (let* ((filetype (pathname-type name))
         (filename (pathname-name name))
         (type-length (1+ (length filetype))))
    (if (> (+ type-length (length filename)) (maximum-filename-length))
	(shorten-filename-for-os
	 (uniquify-file-name
	  (namestring
	   (merge-pathnames 
	    (make-pathname 
	     :name (subseq filename 0 
			   (- (maximum-filename-length) type-length 2)) 
	     :type filetype)
	    name))))
	name)))

(defun maximum-filename-length ()
  (or #+(and DIGITOOL (not CCL-5.1)) +mac-os-filename-limit+
      255)) 

#+MCL
(defmethod samep ((file1 pathname) (file2 pathname))
  (bind:bind ((s1 (open file1 :direction :input
			:if-does-not-exist :error))
	      (s2 (open file2 :direction :input
			:if-does-not-exist :error))
	      (result nil))
    (unwind-protect 
	 (loop with linenumber = 1
            for line1 = (read-line s1 nil :eof)
            for line2 = (read-line s2 nil :eof)
            until (or (eq line1 :eof)
                      (eq line2 :eof))
            do (when (null (string-equal line1 line2))
                 (push (list linenumber line1 line2) result))
            do (incf linenumber))
      (mapc #'close (list s1 s2)))
    (values (null result) (reverse result))))

(defun delete-directory (directory-spec &key (verbose? nil) (dry-run? nil)
			 (if-does-not-exist :error))
  (unless (directory-pathname-p directory-spec)
    (setf directory-spec 
          (make-pathname
           :directory (append (pathname-directory directory-spec)
                              (list (namestring
                                     (make-pathname 
				      :name (pathname-name directory-spec)
				      :type (pathname-type directory-spec)))))
           :defaults directory-spec)))
  (cond ((probe-file directory-spec)
	 (flet ((directory-args ()
		  #+allegro
		  '(:directories t :files nil)
		  #-allegro
		  nil))
	   (let* ((wild-directory 
		   (make-pathname
		    :name :wild
		    :type :wild
		    :directory (append (pathname-directory directory-spec)
				       (list :wild-inferiors))
		    :defaults directory-spec))      
		  (directories (sort
				(apply #'directory
				       wild-directory (directory-args))
				#'>
				:key (compose #'length #'namestring))))
	     (mapc (lambda (directory)
		     (when verbose?
		       (format *debug-io* "~%;;; processing directory ~A"
			       directory))
		     (mapc (lambda (file)
			     (when (probe-file file) 
			       (when verbose?
				 (format *debug-io* "~%;; deleting ~A" file))
			       (unless dry-run?
				 (delete-file file))))
			   (directory (make-pathname :name :wild 
						     :type :wild
						     :defaults directory)))
		     (when verbose?
		       (format *debug-io* "~%;; deleting directory ~A" 
			       directory))
		     (unless dry-run?
		       (delete-file directory)))
		   (append directories
			   (list directory-spec)))
	     (values nil))))
	(t
	 (ecase if-does-not-exist
	   (:ignore
	    )
	   (:error
	    (error "Directory ~a does not exist" directory-spec))))))

#+(or)
(defun directory-name-p (directory-spec)
  (and (null (pathname-name directory-spec))
       (null (pathname-type directory-spec))))
       
#+Test
(progn
  (ensure-directories-exist "user-home:temporary;foo;bar;biz")
  
  (touch-file "user-home:temporary;foo;what.x")
  (touch-file "user-home:temporary;foo;what.y")
  (touch-file "user-home:temporary;foo;what.z")
  (touch-file "user-home:temporary;foo;bar;what.x")
  (touch-file "user-home:temporary;foo;bar;what.y")
  (touch-file "user-home:temporary;foo;bar;what.z")
  (touch-file "user-home:temporary;foo;bar;biz;what.x")
  (touch-file "user-home:temporary;foo;bar;biz;what.y")
  (touch-file "user-home:temporary;foo;bar;biz;what.z"))

#+Test
(delete-directory "user-home:temporary;foo;" :verbose? t :dry-run? t)        

