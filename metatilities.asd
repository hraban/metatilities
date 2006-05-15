#| copyright

See the file COPYING for details

|#

(defpackage :asdf-metatilities (:use #:asdf #:cl))
(in-package :asdf-metatilities)

;;; ---------------------------------------------------------------------------

#+Remove
;;Gary King 2006-04-03: borged into metatilities
(defsystem metabang-generic-lisp
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.5"
  :depends-on (metatilities-base)
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :components ((:module "what"
  	        :pathname (make-pathname :directory `(,@(pathname-directory *load-truename*)
                                        "dev"
                                        ,(or #+OpenMCL "openmcl"
                                             #+DIGITOOL "mcl"
                                             #+SBCL     "sbcl"
                                             #+allegro  "allegro" 
                                             #-(or OpenMCL DIGITOOL SBCL allegro) "unsupported")))
               :components ((:file "generic-lisp")
                            #+DIGITOOL (:file "pop-up-menu")
                            (:file "generic-interface-support" 
                                   :depends-on ("generic-lisp" #+DIGITOOL "pop-up-menu"))))))

;;; ---------------------------------------------------------------------------

(defsystem metatilities
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.5"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "These are the rest of metabang.com's Common Lisp utilities"
  :long-description "These are the rest of metabang.com's Common Lisp utilities and what not."
  
  :components ((:module "extensions"
                        :pathname #.(make-pathname :directory '(:relative "dev" "utilities"))
                        :components ((:file "package-additional")
                                     (:file "anaphoric"
                                            :depends-on ("package-additional"))
                                     (:file "graham" :depends-on ("anaphoric" "package-additional"))
                                     (:file "dates-and-times"
                                            :depends-on ("macros" "anaphoric" "package-additional"))
                                     (:file "files"
                                            :depends-on ("package-additional" "graham"))
                                     (:file "macros"
                                            :depends-on ("package-additional"))

                                     #+Remove
                                     (:file "locks"
                                            :depends-on ("package-additional"))

                                     #+Ignore
                                     ;;?? Gary King 2005-11-17: Need priority queue heap
                                     (:file "notifications"
                                            :depends-on ("package-additional" "graham"))

                                     (:file "sequences"
                                            :depends-on ("package-additional"))
                                     (:file "spy"
                                            :depends-on ("package-additional" "macros"))
                                     (:file "strings"
                                            :depends-on ("package-additional"))
                                     
                                     #+Remove
                                     (:file "threads")

                                     (:file "utilities"
                                            :depends-on ("macros" "package-additional" "anaphoric" "macros"))  
                                     (:file "searching"
                                            :depends-on ("package-additional"))
                                     (:file "copy-file"
	                                    :depends-on ("package-additional"))
	                             (:file "views-and-windows"
                                            :depends-on ("package-additional"))))
               
               
               (:module "what"
  	                :pathname #.(make-pathname 
                                     :directory `(,@(pathname-directory *load-truename*)
                                                  "dev"
                                                  ,(or #+OpenMCL "openmcl"
                                                       #+DIGITOOL "mcl"
                                                       #+SBCL     "sbcl"
                                                       #+allegro  "allegro" 
                                                       #-(or OpenMCL DIGITOOL SBCL allegro) "unsupported")))
                        :components ((:file "generic-lisp")
                                     #+DIGITOOL (:file "pop-up-menu")
                                     (:file "generic-interface-support" 
                                            :depends-on ("generic-lisp" #+DIGITOOL "pop-up-menu"))))
               
               #+DIGITOOL
               (:module "contrib"
                        :pathname #.(make-pathname :directory '(:relative "dev" "contrib" "mcl"))
                        :components ((:file "eval-apple-script")))
               
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  
    :depends-on (metatilities-base 
                 moptilities
                 cl-containers
                 metabang-bind
                 #+Remove metabang-generic-lisp
                 defsystem-compatibility
		 cl-fad)
  
  #+Ignore
  (
   #+Digitool
   (("tcp-mcl") :base-dir "metatilities:source;utilities;mcl;")
   #+Lispwork
   (("tcp-lispworks") :base-dir "metatilities:source;utilities;lispworks;")
   #+Allegro
   (("tcp-allegro") :base-dir "metatilities:source;utilities;allegro;")
   #+openmcl
   (("tcp-openmcl") :base-dir "metatilities:source;utilities;openmcl;")))

;;; ---------------------------------------------------------------------------
   
#+(and DIGITOOL IGNORE)
(defsystem :metatilities-development
  ((("profile")
    :base-dir "metatilities:source;contrib;mcl;"))
  :depends-on (METATILITIES PHEX METABANG.INTERFACE))	