(in-package #:metatilities-test)

(deftestsuite test-date-and-time (metatilities-test) ())

(deftestsuite test-parse-time (test-date-and-time) ())

(addtest (test-parse-time)
  now
  (ensure-same (get-universal-time) (parse-time "now")))

(addtest (test-parse-time)
  1-minute-from-now
  (ensure-same (+ (get-universal-time) (* 60 1))
	       (parse-time "1 minute from now")))

(addtest (test-parse-time)
  1-minute-from-10-12
  (ensure-same 
   (encode-universal-time 0 13 10 (time-date) (time-month) (time-year))
   (parse-time "1 minute from 10:12")))

(addtest (test-parse-time)
  1-minute-from-specific-date
  (ensure-same 
   (encode-universal-time 43 13 10 9 9 1989)
   (parse-time "1 minute from 9/9/89 10:12:43 AM")))

(addtest (test-parse-time)
  1-minute
  (ensure-same 60 (parse-time "1 minute")))

(addtest (test-parse-time)
  10-12
  (ensure-same 
   (encode-universal-time 
    0 12 10 (time-date) (time-month) (time-year)) 
   (parse-time "10:12")))


