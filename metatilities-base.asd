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
                                     (:file "l0-utils"
                                            :depends-on ("package"))
                                     (:file "l0-macros"
                                            :depends-on ("package" "l0-utils"))
                                     (:file "l0-arrays"
                                            :depends-on ("package"))
                                     (:file "l0-clos"
                                            :depends-on ("package"))
                                     (:file "l0-files"
                                            :depends-on ("package"))
                                     (:file "set-equal"
                                            :depends-on ("package"))
                                     (:file "generic-lisp"
                                            :depends-on ("package"))
                                     (:file "generic-interface"
                                            :depends-on ("package" "generic-lisp"
                                                         "l0-macros"))
                                     (:file "defclass-star"
                                            :depends-on ("package" "l0-macros"))
                                     (:file "define-class"
                                            :depends-on ("package" "defclass-star")))))
    :depends-on (moptilities))

