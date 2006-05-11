(in-package #:metatilities)

(deftestsuite samep-file () ()
  (:test ((ensure 
           (samep (translate-logical-pathname
                   "eksl-utils:file-utilities.lisp")
                  (translate-logical-pathname
                   "eksl-utils:file-utilities.lisp")))))
  (:test ((ensure
           (null (samep (translate-logical-pathname
                         "eksl-utils:file-utilities.lisp")
                        (translate-logical-pathname
                         "eksl-utils:string-utilities.lisp"))))))
  (:test (ensure
          (null (samep (translate-logical-pathname
                        "eksl-utils:string-utilities.lisp")
                       (translate-logical-pathname
                        "eksl-utils:file-utilities.lisp"))))))