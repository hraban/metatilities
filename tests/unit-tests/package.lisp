(in-package #:common-lisp-user)

(defpackage #:metatilities-base-test
  (:use #:common-lisp #:lift #:metatilities)
  (:import-from #:metatilities
                #:*automatic-slot-accessors?*
                #:*automatic-slot-initargs?*
                #:*prune-unknown-slot-options*))

