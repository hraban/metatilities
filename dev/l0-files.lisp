(in-package #:metatilities)

;;; ---------------------------------------------------------------------------

(defun pathname-name+type (pathname)
  "Returns a new pathname consisting of only the name and type from a non-wild pathname."
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))


#+Remove
(export '(macintosh-path->unix))

#+Remove
(defmethod macintosh-path->unix ((path pathname))
  (macintosh-path->unix (namestring (translate-logical-pathname path))))

#+Remove
(defmethod macintosh-path->unix ((path string))
  (concatenate 'string "/Volumes/" (substitute #\/ #\: path)))

#+Ignore
(deftestsuite test-macintosh-path->unix () 
  ()
  (:test ((ensure-same (macintosh-path->unix "users:gwking:test-db.db")
                       "/users/gwking/test-db.db" :test string-equal))))

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