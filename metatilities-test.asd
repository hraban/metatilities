#|
Author: Gary King

See file COPYING for details
|#

(defsystem "metatilities-test"
  :author "Gary Warren King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for metatilities"
  :components ((:module
		"setup"
		:pathname "tests/"
		:components ((:file "package")
			     (:file "tests"
				    :depends-on ("package"))))
	       (:module
		"tests"
		:depends-on ("setup")
		:components ((:file "test-date-and-time"))))
  :depends-on ("lift" "metatilities")
  :perform (test-op (o c) (symbol-call :lift :run-tests :config :generic)))
