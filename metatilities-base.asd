#| copyright

See the file COPYING for details

|#

(defpackage :asdf-metatilities-base (:use #:asdf #:cl))
(in-package :asdf-metatilities-base)

(defsystem METATILITIES-BASE
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.5"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :description "These are metabang.com's Common Lisp basic utilities."
  :long-description "These are metabang.com's Common Lisp basic utilities and what not."
  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "api" 
                                            :depends-on ("package"))
                                     (:file "l0-utils"
                                            :depends-on ("api"))
                                     (:file "l0-macros"
                                            :depends-on ("api" "l0-utils"))
                                     (:file "l0-arrays"
                                            :depends-on ("api"))
                                     (:file "l0-clos"
                                            :depends-on ("api"))
                                     (:file "l0-files"
                                            :depends-on ("api"))
                                     (:file "set-equal"
                                            :depends-on ("api"))
                                     (:file "generic-lisp"
                                            :depends-on ("api"))
                                     (:file "generic-interface"
                                            :depends-on ("api" "generic-lisp"
                                                         "l0-macros"))
                                     (:file "defclass-star"
                                            :depends-on ("api" "l0-macros"))
                                     (:file "define-class"
                                            :depends-on ("api" "defclass-star")))))
    :depends-on (moptilities))

