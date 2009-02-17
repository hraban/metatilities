#| copyright

See the file COPYING for details

|#

(defpackage :metatilities-system (:use #:asdf #:cl))
(in-package :metatilities-system)

;; try hard
#+(or)
;; no, too weird
(unless (find-system 'asdf-system-connections nil)
 (when (find-package 'asdf-install)
   (funcall (intern (symbol-name :install) :asdf-install)
	    'asdf-system-connections)))
;; give up with a useful (?) error message
(unless (find-system 'asdf-system-connections nil)
  (print "The metatilities system works best with asdf-system-connections. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))

;; now try again
(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem metatilities
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.6.18"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "These are the rest of metabang.com's Common Lisp utilities"
  :long-description "These are the rest of metabang.com's Common Lisp utilities and what not."
  :properties ((:ait-timeout . 10) 
               (:system-applicable-p . 3))
  :components ((:module 
		"extensions"
		:pathname #.(make-pathname 
			     :directory '(:relative "dev" "utilities"))
		:components 
		((:file "package-additional")
		 (:file "anaphoric"
			:depends-on ("package-additional"))
		 (:file "graham"
			:depends-on ("anaphoric" "package-additional"))
		 (:file "dates-and-times"
			:depends-on ("macros" "anaphoric" "package-additional"))
		 (:file "files"
			:depends-on ("graham" "macros"))
		 (:file "macros"
			:depends-on ("package-additional"))
		 (:file "sequences"
			:depends-on ("package-additional"))
		 (:file "spy"
			:depends-on ("package-additional" "macros"))
		 (:file "strings"
			:depends-on ("package-additional"))
		 (:file "utilities"
			:depends-on ("macros" "graham"))  
		 (:file "searching"
			:depends-on ("package-additional"))
		 (:file "views-and-windows"
			:depends-on ("package-additional"))))
               (:module 
		"port"
		:pathname #.(concatenate
			     'string "dev/"
			     (or #+OpenMCL "openmcl"
				 #+DIGITOOL "mcl"
				 #+SBCL     "sbcl"
				 #+allegro  "allegro" 
				 #-(or OpenMCL DIGITOOL SBCL allegro)
				 "unsupported")
			     "/")
		:components ((:file "generic-lisp")
			     #+DIGITOOL (:file "pop-up-menu")
			     (:file "generic-interface-support" 
				    :depends-on ("generic-lisp" 
						 #+DIGITOOL "pop-up-menu"))))
               (:module 
		"website"
		:components
		((:module "source"
			  :components ((:static-file "index.md"))))))
    :in-order-to ((test-op (load-op metatilities-test)))
    :perform (test-op :after (op c)
		      (funcall
		       (intern (symbol-name '#:run-tests) :lift)
		       :config :generic))
    :depends-on ((:version :metatilities-base "0.6.0") 
		 :moptilities
		 :cl-containers
		 :metabang-bind
		 :asdf-system-connections))

(defmethod operation-done-p 
           ((o test-op)
            (c (eql (find-system 'metatilities))))
  (values nil))

#+asdf-system-connections
(asdf:defsystem-connection lift-and-metatilities
  :requires (lift metatilities-base)
  :perform (load-op :after (op c)
                    (use-package (find-package :lift) 
                                 (find-package :metatilities))
                    (funcall (intern 
                              (symbol-name :export-exported-symbols)
                              'metatilities)
                             :lift :metatilities)))


