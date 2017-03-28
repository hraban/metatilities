#| copyright

See the file COPYING for details

|#

;; Use asdf-system-connections if available. Otherwise, warn the user.
(if (find-system 'asdf-system-connections nil)
    (load-system 'asdf-system-connections)
    (print "The metatilities system works best with asdf-system-connections. See
http://www.cliki.net/asdf-system-connections for details and download
instructions."))

(defsystem "metatilities"
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.6.18"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "These are the rest of metabang.com's Common Lisp utilities"
  :long-description "These are the rest of metabang.com's Common Lisp utilities and what not."
  :components ((:module
		"extensions"
		:pathname "dev/utilities"
		:components
		((:file "package-additional")
		 (:file "graham"
			:depends-on ("package-additional"))
		 (:file "dates-and-times"
			:depends-on ("macros" "package-additional"))
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
		:pathname #.(strcat "dev/"
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
    :in-order-to ((test-op (test-op "metatilities-test")))
    :depends-on ((:version "metatilities-base" "0.6.0")
		 "moptilities"
		 "cl-containers"
		 "metabang-bind"
		 ;"asdf-system-connections"
		 ))

#+asdf-system-connections
(defsystem-connection "metatilities/with-lift"
  :requires ("lift" "metatilities-base")
  :perform (load-op (o c)
             (use-package (find-package :lift) (find-package :metatilities))
             (symbol-call :metatilities :export-exported-symbols
                          :lift :metatilities)))
