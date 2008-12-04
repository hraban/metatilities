(in-package #:common-lisp-user)

(defpackage #:metatilities-test
  (:use #:common-lisp #:lift #:metatilities)
  (:import-from #:metatilities
		#:time-year
		#:time-month
		#:time-date
		#:time-hour
		#:time-minute
		#:time-second
		#:time-day-of-week
		#:time-daylight-savings-time-p)
  (:import-from #:metatilities
                #:*automatic-slot-accessors?*
                #:*automatic-slot-initargs?*
                #:*prune-unknown-slot-options*)
  (:import-from #:metatilities
		#:parse-time
		)
  )

