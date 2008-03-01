(in-package #:metatilities)

(defconstant +minutes-per-hour+ 60
  "The number of minutes in one hour.")

(defconstant +seconds-per-minute+ 60
  "The number of seconds in one minute.")

(defconstant +usual-days-per-year+ 365
  "The number of days in an ordinary year.")

(defconstant +seconds-per-hour+ (* +seconds-per-minute+ +minutes-per-hour+)
  "The number of seconds in one hour.")

(defconstant +hours-per-day+ 24
  "The number of hours in one day.")

(defconstant +seconds-per-day+
  (* +hours-per-day+ +seconds-per-hour+)
  "The number of seconds in one day.")

(defparameter +days-per-month+
  '(31 28 31 30 31 30 31 31 30 31 30 31))

(eval-always 
  (defmacro generate-time-part-function (part-name position)
    (let ((function-name (form-symbol (symbol-name 'time) "-" part-name)))
      `(eval-always
         (export ',function-name)
         (defun ,function-name
                (&optional (universal-time (get-universal-time))
                           (time-zone nil))
           ,(format nil "Returns the ~(~A~) part of the given time." part-name)
           (nth-value ,position (apply #'decode-universal-time universal-time time-zone))))))

  (generate-time-part-function second 0)
  (generate-time-part-function minute 1)
  (generate-time-part-function hour 2)
  (generate-time-part-function date 3)
  (generate-time-part-function month 4)
  (generate-time-part-function year 5)
  (generate-time-part-function day-of-week 6)
  (generate-time-part-function daylight-savings-time-p 7))

(defun days-in-month (month &optional leap-year?)
  "Returns the number of days in the specified month. The month should be
between 1 and 12."
  (+ (nth (1- month) +days-per-month+) (if (and (= month 2) leap-year?) 1 0)))

(defun leap-year-p (year)
  "Returns t if the specified year is a leap year. I.e. if the year
is divisible by four but not by 100 or if it is divisible by 400."
  (or (and (= (mod year 4) 0)               ; logand is faster but less perspicuous
           (not (= (mod year 100) 0)))
      (= (mod year 400) 0)))

(defun day-of-year (date)
  "Returns the day of the year [1 to 366] of the specified date [which must be \(CL\) universal time format.]" 
  (let ((leap-year? (leap-year-p (time-year date))))
    (+ (loop for month from 1 to (1- (time-month date)) sum
             (days-in-month month leap-year?))
       (time-date date))))

