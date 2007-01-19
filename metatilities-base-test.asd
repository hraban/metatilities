#|

Author: Gary King

|#

(defpackage #:metatilities-base-test-system (:use #:cl #:asdf))
(in-package #:metatilities-base-test-system)

(defsystem metatilities-base-test
  :version "0.1"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :components ((:module 
		"unit-tests"
		:components ((:file "package")
			     (:file "tests"
				    :depends-on ("package"))
			     (:file "test-parse-brief-class"
				    :depends-on ("tests")))))
  :depends-on (lift metatilities-base))