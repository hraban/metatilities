#| copyright

See the file COPYING for details

|#

(defpackage :asdf-metatilities (:use #:asdf #:cl))
(in-package :asdf-metatilities)

;; try hard
(unless (find-system 'asdf-system-connections nil)
 (when (find-package 'asdf-install)
   (funcall (intern (symbol-name :install) :asdf-install) 'asdf-system-connections)))
;; give up with a useful (?) error message
(unless (find-system 'asdf-system-connections nil)
  (error "The metatilities system requires asdf-system-connections. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))

;; now make sure it's loaded
(operate 'load-op 'asdf-system-connections)

(defsystem metatilities
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.6.2"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "These are the rest of metabang.com's Common Lisp utilities"
  :long-description "These are the rest of metabang.com's Common Lisp utilities and what not."
  :properties ((:ait-timeout . 10) 
               (:system-applicable-p . 3))
  :components ((:module "extensions"
                        :pathname #.(make-pathname :directory '(:relative "dev" "utilities"))
                        :components ((:file "package-additional")
                                     (:file "anaphoric"
                                            :depends-on ("package-additional"))
                                     (:file "graham" :depends-on ("anaphoric" "package-additional"))
                                     (:file "dates-and-times"
                                            :depends-on ("macros" "anaphoric" "package-additional"))
                                     (:file "files"
                                            :depends-on ("graham" "macros"))
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
                                            :depends-on ("macros" "graham"))  
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
               
               (:module "website"
                        :components ((:module "source"
                                              :components ((:static-file "index.lml"))))))
  
    :depends-on (metatilities-base 
                 moptilities
                 cl-containers
                 metabang-bind
                 defsystem-compatibility
		 cl-fad
                 asdf-system-connections))

(asdf:defsystem-connection lift-and-metatilities
  :requires (lift metatilities-base)
  :perform (load-op :after (op c)
                    (use-package (find-package :lift) 
                                 (find-package :metatilities))
                    (funcall (intern 
                              (symbol-name :export-exported-symbols)
                              'metatilities)
                             :lift :metatilities)))


#+(and DIGITOOL IGNORE)
(defsystem :metatilities-development
  ((("profile")
    :base-dir "metatilities:source;contrib;mcl;"))
  :depends-on (METATILITIES PHEX METABANG.INTERFACE))	
