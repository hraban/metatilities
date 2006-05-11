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