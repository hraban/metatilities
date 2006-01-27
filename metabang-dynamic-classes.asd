(defpackage :asdf.metabang.dynamic-classes (:use #:asdf #:cl))
(in-package :asdf.metabang.dynamic-classes)

(defsystem metabang-dynamic-classes
  :author "Gary Warren King <gwking@metabang.com>"
  :version "0.5"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style license"
  :components ((:module "dev"
                        :components ((:file "dynamic-class"))))
  :depends-on (metatilities-base))

