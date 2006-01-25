(in-package :cl-user)

(defpackage "BUNDLE-SYS"
  (:use "COMMON-LISP" "CCL"))

(defpackage "MACH"
  (:use "COMMON-LISP" "CCL")
  (:shadow "DEFINE-ENTRY-POINT"
           "REQUIRE-TRAP")
  (:export "DEFINE-ENTRY-POINT"
           "LOOKUP-FRAMEWORK-SYMBOL"
           "REQUIRE-TRAP"
           "REQUIRE-MACH-TRAP"))

