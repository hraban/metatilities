(in-package #:metatilities)


;;; ---------------------------------------------------------------------------
;;; obvious constants
;;; ---------------------------------------------------------------------------

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

;;; these are lists of acceptable tokens
;;; make sure to use 'index-of' on these; it allows you to find an index in a deep list

(defparameter +month-list+
  '((january february march april may june july august september october november december)
    (jan feb mar apr may jun jul aug sep oct nov dec)))
(defparameter +day-list+
  '(sunday monday tuesday wednesday thursday friday saturday))
(defparameter +fluff-list+ '(at))

(defconstant +longer-format-index+ 0)
(defconstant +shorter-format-index+ 1)

(defparameter +month-output-list+
  '(("January" "February" "March" "April" "May" "June" "July" "August" "September"
     "October" "November" "December")
    ("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))

(defparameter +dow-output-list
  '(("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday")
    ("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")))

(defparameter +days-per-month+
  '(31 28 31 30 31 30 31 31 30 31 30 31))

;;; ---------------------------------------------------------------------------
;;; en/de-coding functions
;;; ---------------------------------------------------------------------------

(defun day->string (day-of-the-week &optional (format :long))
  "Returns the name of `day-of-the-week`. The parameter should be a number between 0 and 6 where 0 represents Sunday and 6 repressents Saturday. The optional format argument can be either :long or :short. In the latter case, the return string will be of length three; in the former it will be the complete name of the appropriate day."
  (check-type day-of-the-week (mod 7))
  (check-type format (member :long :short))
  (nth day-of-the-week 
       (case format
	 (:long (nth +longer-format-index+ +dow-output-list))
	 (:short (nth +shorter-format-index+ +dow-output-list)))))

(defun month->string (month &optional (format :long))
  "Returns the name \(in English\) of the month. Format can be :long or :short."
  (check-type month (integer 1 12))
  (check-type format (member :long :short))
  (nth (1- month) 
       (case format
	 (:long (nth +longer-format-index+ +month-output-list+))
	 (:short (nth +shorter-format-index+ +month-output-list+)))))

(defun string->month (string)
  (or (position string (first +month-list+) :test #'string-equal)
      (position string (second +month-list+)  :test #'string-equal)))

(defun print-date (stream year month day day-of-the-week)
  "Prints the information in the form `Wednesday, June 7, 1989'"
  (format stream "~a, ~a ~d, ~d"
	  (day->string day-of-the-week)
	  (month->string month)
	  day
	  year))

(defun date-string (&optional (time (get-universal-time)))
  "Date returned as a string, in the format `Wednesday, June 7, 1989'"
  (multiple-value-bind (second minute hour day month year day-of-the-week)
      (decode-universal-time time)
    (declare (ignore second minute hour))
    (print-date nil year month day day-of-the-week)))

(defun date-string-brief (&optional (time (get-universal-time)))
  "Returns a string representing the `time` \(which defaults to the current universal time\). The string is in the form MM-DD-YYYY."
  (multiple-value-bind (second minute hour day month year day-of-the-week)
      (decode-universal-time time)
    (declare (ignore second minute hour day-of-the-week))
    (format nil "~d-~D-~D" month day year)))

#+test
(defun test-date-string ()
  (spy (date-string (encode-universal-time 0 0 0 1 9 1966))))

;; Would be easier to pass in a token, but this will do the trick for now:
(defun print-time-with-no-colons (stream hour minute &optional brief)
  "Prints the information in the form `12-34 pm'"
  (if (< hour 12)
      (format stream "~d~:[-~2,'0d~;~*~] am" (if (= hour 0) 12 hour) (and brief (zerop minute)) minute)
      (format stream "~d~:[-~2,'0d~;~*~] pm" (if (= hour 12) 12 (- hour 12)) (and brief (zerop minute)) minute)))

(defun print-time (stream hour minute &optional brief)
  "Prints the information in the form `12:34 pm'"
  (if (< hour 12)
      (format stream "~d~:[:~2,'0d~;~*~] am" (if (= hour 0) 12 hour) (and brief (zerop minute)) minute)
      (format stream "~d~:[:~2,'0d~;~*~] pm" (if (= hour 12) 12 (- hour 12)) (and brief (zerop minute)) minute)))


(defun print-universal-time-with-no-colons (ut &optional stream brief)
  (multiple-value-bind (second minute hour)
                       (decode-universal-time ut)
    (declare (ignore second))
    (print-time-with-no-colons stream hour minute brief)))

(defun print-universal-time (ut &optional stream brief)
  "Prints a universal time to stream using `print-time`. The output can be normal or brief."
  (multiple-value-bind (second minute hour)
                       (decode-universal-time ut)
    (declare (ignore second))
    (print-time stream hour minute brief)))

(defun time-string (&optional (time (get-universal-time)))
  "Time returned as a string, in the format `12:34 pm'"
  (print-universal-time time nil))

(defun time-string-with-no-colons (&optional (time (get-universal-time)))
  "Time returned as a string, in the format `12-34 pm'"
  (print-universal-time-with-no-colons time nil))

(defun date-and-time-string
       (&optional (time (get-universal-time)))
  "Current date and time returned as a string."
  (concatenate
    'string (date-string time) " at " (time-string time)))

#+test
(defun test-date-and-time-string ()
  (spy (date-and-time-string (encode-universal-time 0 0 12 1 9 1966)))
  (spy (date-and-time-string (encode-universal-time 0 0 1 1 9 1966)))
  (spy (date-and-time-string (encode-universal-time 0 0 0 1 9 1966))))

;;; ============================================================================
;;; TIME PARSER
;;;
;;; Written by: Matthew D. Schmill

;;; ---------------------------------------------------------------------------
;;; these are context-predicates

(declaim (inline month-ok-p day-ok-p yearp nth-ok-p))

;; okay to parse an nTH token
(defun nth-ok-p (context) (member context '(:month)))
;; okay to parse a month token
(defun month-ok-p (context) (not (member context '(:month))))
;; okay to parse a day of the week token
(defun day-ok-p (context) (not (member context '(:month :day-of-week))))
;; okay to parse a year
(defun yearp (n) (> n 1900))

;;; ---------------------------------------------------------------------------
;;; a support function

(defun index-of (item list)
  "finds the index of an item in a list, or, in a list embedded in a list.
this allows the user to make 'nicknames' for items in a list."
  (cond
   ((null list) nil)
   ((consp (car list)) (or (index-of item (car list)) (index-of item (cdr list))))
   (t (let ((k (position item list)))
        (if k (1+ k) nil)))))

;;; ---------------------------------------------------------------------------
;;; reader functions

(defun token-type (string)
  "Determines which reader to use for the next token."
  (let* ((ns (string-trim " " string))
         (pc (position #\: string))      ;; a colon requires the time reader
         (pb (position #\/ string))      ;; for convenience, we use a date reader for slashes
         (ps (position #\space string))) ;; this is the end of the next token
    (cond
     ((and pc (< pc (or ps (length ns)))) :time-reader)
     ((and pb (< pb (or ps (length ns)))) :date-reader)
     (t :lisp-reader))))

(defun read-time (string)
  "strips the time signature off of the front of string. can handle the following format:
[H]H\(:|.\)MM[\(:|.\)[SS]] [AM|PM]"
  (let* ((end (or (position #\space string) (length string)))
         (sbuff (subseq string 0 (min (1+ end) (length string))))
         la hour minute (second 0) (seq '(:h :m :s)))
    (loop until (string= sbuff "") do
          (setq la (min (or (position #\: sbuff) end)
                        (or (position #\. sbuff) end)
                        (length sbuff)))
          (case (pop seq)
	    (:h (setq hour   (read-from-string (subseq sbuff 0 la))))
	    (:m (setq minute (read-from-string (subseq sbuff 0 la))))
	    (:s (setq second (read-from-string (subseq sbuff 0 la))))
            (t  (error "unrecognized time format in ~S" string)))
          (setq sbuff (subseq sbuff (min (1+ la) (length sbuff)) (length sbuff))))
    (unless (and hour minute second)
      (error "incomplete time format in ~S" string))
    ;; Handle "midnight, noon, and 12 AM, 12 PM weirdness
    (cond ((search "noon" string :test #'string-equal)
           (assert (= hour 12) () "12 is the only hour you can use with NOON")
           (assert (and (= second 0)
                        (= minute 0)) () "~2d:~2d:~2d NOON makes no sense"
                   hour minute second)
           (setf hour 12))
          ((search "midnight" string :test #'string-equal)
           (assert (= hour 12) () "12 is the only hour you can use with MIDNIGHT")
           (assert (and (= second 0)
                        (= minute 0)) () "~2d:~2d:~2d MIDNIGHT makes no sense"
                   hour minute second)
           (setf hour 0))
          ((search "AM" string :test #'string-equal)
           ;; 12 AM is a bad use of AM - should use Midnight, but we allow it
           (when (= hour 12)
             (setf hour 0)))
          ((search "PM" string :test #'string-equal)
           ;; 12 PM is a bad use of PM - should use Noon, but we allow it
           (unless (or (> hour 12)
                       (and (= hour 12)
                            (= second 0)
                            (= minute 0)))
             (incf hour 12)))
          (t nil))
    (values (list hour minute second)
            (acond ((search "PM" string :test #'string-equal)
                    (+ it 2))
                   ((search "AM" string :test #'string-equal)
                    (+ it 2))
                   ((search "midnight" string :test #'string-equal)
                    (+ it 8))
                   ((search "noon" string :test #'string-equal)
                    (+ it 4))
                   (t end)))))

(defun read-date (string &optional default-year)
  "strips the date signature off of the front of string. can handle slash-delimited format:
[M]M/[D]D[/YY]"
  (let* ((end (or (position #\space string) (length string)))
         (sbuff (subseq string 0 (min (1+ end) (length string))))
         la da mo yr (seq '(:m :d :y)))
    (loop until (string= sbuff "") do
          (setq la (or (position #\/ sbuff) (length sbuff)))
          (case (pop seq)
	    (:y (setq yr (read-from-string (subseq sbuff 0 la))))
	    (:d (setq da (read-from-string (subseq sbuff 0 la))))
	    (:m (setq mo (read-from-string (subseq sbuff 0 la))))
            (t  (error "unrecognized time format in ~S" string)))
          (setq sbuff (subseq sbuff (min (1+ la) (length sbuff)) (length sbuff))))
    (when (and yr (< yr 100)) (incf yr 1900))
    (unless (and da mo)
      (error "incomplete date format in ~S" string))
    (values (list mo da (or yr default-year))
            end)))

;;; ---------------------------------------------------------------------------
;;; this is the macro for handling tokens that the lisp reader gets

(defmacro handle-token ((mo da yr ho mi se context cont-q) token)
  (declare (ignore ho mi se))
  (let ((n (gensym "N-")))
    `(progn
       (format t "HT:> ~S [~S/~S]~%" ,token ,context ,cont-q)
       (cond
        ;; a month by name
        ((and (index-of ,token +month-list+)
              (month-ok-p ,context))
         (when (eql ,context :number) (setf ,da ,cont-q) (setf ,cont-q nil))
         (setf ,mo (index-of ,token +month-list+) ,context :month))
        ;; a day of the week by name
        ((and (index-of ,token +day-list+)
              (day-ok-p ,context))
         (setf ,context :day-of-week))
        ;; a fluff token
        ((index-of ,token +fluff-list+) nil)
        ;; a symbol is likely to depend on context, but is probably an nth
        ((symbolp ,token)
         (cond
          ((nth-ok-p ,context) (let ((,n (read-from-string (string-trim "DRTHS" (symbol-name ,token)))))
                                 (setf ,da ,n ,context :day)))
          (t (error "unrecognized token ~S" ,token))))
        ;; a number. depends on context
        ((numberp ,token)
	 (cond
          ((yearp ,token) (setf ,yr ,token ,context :year))
          ((and (eql ,context :month) (< ,token 100))
           (setf ,yr (+ ,token 1900) ,context :year))
	  ((null ,cont-q) (setf ,cont-q ,token ,context :number))
          ((not (null ,cont-q)) (error "number token ~S in unexpected location." ,token))))
        (t (error "unparsable token ~S" ,token))))))

;;; ---------------------------------------------------------------------------
;;; this is the grand function for parsing time and date strings

(defun parse-date-and-time-string (time-string &optional encode? (default-time (get-universal-time))
                                               future?)
  "Parses strings expressing the day and/or time, such as 12/4/61 4:03:15.
The date part should look like this: [M]M/[D]D[/YY] and the time part should
look like this: [H]H(:|.)MM[(:|.)[SS]] [AM|PM].
The time part must have a colon; the
seconds are optional.  If the time part is missing, it defaults to midnight.
Six values are returned, suitable for giving to `encode-universal-time.' If the
optional argument is true, this encoding is done for you, and a universal time
is returned. If 'future?' and no date is given the returned time is guaranteed to be 
in the future."

  (let ((ts (string-trim ", " time-string))
        (hour 0)
	(minute 0)
	(second 0)
        (context :none)
        (context-queue nil)
        next-stop token
        (no-date-specified? t))
    ;; set up default date
    (multiple-value-bind (ignore1 ignore2 ignore3 day month year) (decode-universal-time default-time)
      (declare (ignore ignore1 ignore2 ignore3))
      (loop until (string= ts "") do
            (case (token-type ts)
              (:time-reader
               (multiple-value-setq (token next-stop) (read-time ts))
	       (setq hour (first token) minute (second token) second (third token)))
              (:lisp-reader
               (multiple-value-setq (token next-stop) (read-from-string ts))
               (handle-token (month day year ho mi se context context-queue) token))
              (:date-reader
               (setf no-date-specified? nil)
               (multiple-value-setq (token next-stop) (read-date ts year))
               (setq month (first token) day (second token) year (third token)))
              (otherwise (error "unsupported reader call")) ;; shouldn't ever happen
	      )
            (setq ts (string-trim ", " (subseq ts next-stop (length ts)))))
      ;; Possibly force returned time to be in the future if 'future?' and no date specified.
      (when (and future?
                 no-date-specified?
                 (<= (encode-universal-time second minute hour day month year)
                     default-time))
        (incf day))
      (if encode?
        (encode-universal-time second minute hour day month year)
        (values second minute hour day month year)))))

#|

(defun print-test (time-string &optional (base (get-universal-time)))
  (format t "~&~a -> ~a~%" 
          time-string
          (print-universal-date-verbose (parse-date-and-time-string time-string t base t) nil)))

(defun test-parse-date-and-time-string ()
  (print-test "12/4/1961 4:03:15 AM")
  (print-test "12/4/1961 4:03 PM")
  (print-test "12/4/1961")
  (print-test "8/1/88")
  (print-test "8/1/88 4:03")
  (print-test "8/1")
  (print-test "8/1")
  (print-test "12:00")
  (print-test "12:06:20 AM")
  (print-test "12:00 AM")
  (print-test "0:00")
  (print-test "12:00 midnight")
  (print-test "1:00 AM")
  (print-test "2:00 AM")
  (print-test "12:00 PM")
  (print-test "12:00 noon")
  (print-test "4:03 PM")
  (print-test "4:03 AM")
  (print-test "0:03")
  (print-test "16:03")
  (print-test "23:59")
  (print-test "8/1" (encode-universal-time 0 0 0 1 1 1965)))

;; SHOULD BE ERRORS

(print-test "1:00 MIDNIGHT")
(print-test "12:30 NOON")


|#
       
;;; ============================================================================

(defun parse-date-and-time (spec &optional (default-time (get-universal-time)))
  "Converts `spec' to a universal time.  Spec can either be a number, in which
case this is a no-op, a list of either 3 or 6 numbers, in which case it is
passed to `encode-universal-time,' or a string, in which case it is parsed."
  (etypecase spec
    (number spec)
    (list   (ecase (length spec)
	      (3  (apply #'encode-universal-time 0 0 0 spec))
	      (6  (apply #'encode-universal-time spec))))
    (string (parse-date-and-time-string spec t default-time))))

;;; ============================================================================
;;; Time-hacking code `ported' from Phoenix.

;;; ============================================================================
;;; Printing of absolute times

(defun print-brief-ut (ut &optional (stream nil) (ref-time (get-universal-time)))
  "Prints only those aspects of `ut' that differ from `ref-time.' Both times
should be in universal-time format.  Never prints seconds."
  (multiple-value-bind (ut-sec ut-min ut-hour ut-day ut-month ut-year)
      (decode-universal-time ut)
    (declare (ignore ut-sec))
    (multiple-value-bind (ref-sec ref-min ref-hour ref-day ref-month ref-year)
	(decode-universal-time ref-time)
      (declare (ignore ref-sec ref-min ref-hour))
      (let* ((same-year? (= ut-year ref-year))
	     (same-day?  (and same-year? (= ut-month ref-month) (= ut-day ref-day))))
	(format stream "~:[~d/~d~:[/~d~;~*~] ~;~4*~]~d:~2,'0d"
		same-day? ut-month ut-day same-year? ut-year ut-hour ut-min)))))

#+test
(defun test-print-brief-ut ()
  (print-brief-ut (parse-date-and-time-string "12/4/1961 4:03:15" t) t
		  (parse-date-and-time-string "12/4/1961 4:03:15" t))
  (terpri)
  (print-brief-ut (parse-date-and-time-string "12/4/1961 0:00:00" t) t
		  (parse-date-and-time-string "12/4/1961 0:00:00" t))
  (terpri)
  (print-brief-ut (parse-date-and-time-string "12/4/1961 12:00:00" t) t
		  (parse-date-and-time-string "12/4/1961 12:00:00" t))
  (print (print-brief-ut (parse-date-and-time-string "12/4/1961 4:03:15" t) nil
			 (parse-date-and-time-string "12/4/1961 4:03:15" t)))
  (print (print-brief-ut (parse-date-and-time-string "12/5/1961 4:03:15" t) nil
			 (parse-date-and-time-string "12/4/1961 4:03:15" t)))
  (print (print-brief-ut (parse-date-and-time-string "12/5/1960 4:03:15" t) nil
			 (parse-date-and-time-string "12/4/1961 4:03:15" t)))
  (print (print-brief-ut (get-universal-time) nil (get-universal-time))))

(defun print-ut (ut &optional stream time-only? print-seconds?)
  "Prints `ut' in the format [M]M/[D]D [H]H:MM or [M]M/[D]D [H]H:MM:SS if
`print-seconds?' is true.  Omits the day part if `time-only?' is true."
  (multiple-value-bind (seconds minutes hours day month year)
      (decode-universal-time ut #+Explorer nil)
    ;; Necessary to prevent a faulty compiler optimization.  (Westy 12/20/90)
    (declare #+Explorer (optimize (speed 1))
	     (ignore year))
    (if time-only?
	(if print-seconds?
	    (format stream "~2d:~2,'0d:~2,'0d" hours minutes seconds)
	    (format stream "~2d:~2,'0d" hours minutes))
	(if print-seconds?
	    (format stream "~d/~d ~2d:~2,'0d:~2,'0d" month day hours minutes seconds)
	    (format stream "~d/~d ~2d:~2,'0d" month day hours minutes)))))

#+test
(defun test-print-ut ()
  (print-ut (parse-date-and-time-string "12/4/1961 4:03:15" t) t)
  (spy (print-ut (parse-date-and-time-string "12/4/1961 12:00:00" t) nil))
  (spy (print-ut (parse-date-and-time-string "12/4/1961 00:00:00" t) nil))
  (spy (print-ut (parse-date-and-time-string "12/4/1961 4:03:15" t) nil nil t))
  (spy (print-ut (parse-date-and-time-string "12/4/1961 4:03:15" t) nil t))
  (spy (print-ut (parse-date-and-time-string "12/4/1961 4:03:15" t) nil t t)))

;;; ============================================================================
;;; Interval printing, rather than absolute times

(defun print-time-interval (seconds &optional stream print-seconds?)
  "Prints the time elapsed by `seconds.' For example, 125 prints ``2 minutes, 5
seconds'' If `print-seconds?' is true, the residual seconds are reported,
otherwise the result is rounded to the nearest minute."
  (let (days hours minutes secs)
    (multiple-value-setq (days secs) (floor (abs seconds) (* 60 60 24)))
    (multiple-value-setq (hours secs) (floor secs (* 60 60)))
    (multiple-value-setq (minutes secs) (if print-seconds?
					    (floor secs 60)
					    (values (round secs 60) 0)))
    #+ignore
    (spy days hours minutes secs)
    (if print-seconds?
	(if (zerop seconds)
	    (format stream "0 seconds")
	    (format stream "~:[~d day~:p, ~;~*~]~:[~d hour~:p, ~;~*~]~:[~d minute~:p, ~;~*~]~:[~d second~:p~;~*~]~:[ ago ~;~]"
		    (zerop days) days
		    (zerop hours) hours
		    (zerop minutes) minutes
		    (zerop secs) secs
		    (plusp seconds)))
	(if (= 0 days hours minutes)
	    (format stream "0 minutes")
	    (format stream "~:[~d day~:p, ~;~*~]~:[~d hour~:p, ~;~*~]~:[~d minute~:p~;~*~]~:[ ago~;~]"
		    (zerop days) days
		    (zerop hours) hours
		    (zerop minutes) minutes
		    (plusp seconds))))))

#+test
(defun test-print-time-interval ()
  (spy (print-time-interval 123456 nil nil))
  (spy (print-time-interval 123456 nil t))
  (spy (print-time-interval 123456 t))
  (spy (print-time-interval -123456))
  (spy (print-time-interval 12345))
  (spy (print-time-interval 1234))
  (spy (print-time-interval 123))
  (spy (print-time-interval 12))
  (spy (print-time-interval 12 nil t))
  (spy (print-time-interval 0 nil t)))

(defun print-brief-time-interval (seconds &optional stream)
  "Returns a string that describes the time elapsed by `secs' in the format [[H]Hh][M]Mm. For example,
123456 yields ``34h17m''"
  (multiple-value-bind (hours leftover-seconds)
      (truncate (abs seconds) 3600)
    ;; Necessary to prevent a faulty compiler optimization.  (Westy 10/16/91)
    (declare (optimize (speed 1)))
    (format stream "~:[~;-~]~:[~*~;~dh~]~dm"
	    (minusp seconds)
	    (plusp hours)
	    hours
	    (round leftover-seconds 60))))

#+test
(defun test-print-brief-time-interval ()
  (spy (print-brief-time-interval  123456))
  (spy (print-brief-time-interval -123456))
  (spy (print-brief-time-interval 12345))
  (spy (print-brief-time-interval 1234))
  (spy (print-brief-time-interval 123))
  (spy (print-brief-time-interval 12)))

;;; ---------------------------------------------------------------------------
;;; Time interval parsing stuff.

(defun parse-number (string &optional (from 0) to radix fail-if-not-whole-string)
  "return a number parsed from the contents of string, or a part of it.
from and to specify the part of the string; to = nil means the end of it.
radix defaults to decimal.
if the string or part doesn't start with a number, nil is returned.
the second value is the index in string of the first non-digit, or nil if none.
fail-if-not-whole-string means return nil and 0 unless the whole string or
specified part can be parsed as a number."
  (let ((real-end  (or to (length string))))
    (multiple-value-bind (num index)
	(parse-integer string :start from :end real-end :radix (or radix 10.)
		       :junk-allowed t )
      (if fail-if-not-whole-string
	  (if  (= index real-end)
	       (values num index)
	       (values nil 0))
	  (values num (and num index))))))

(defvar time-interval-array (make-array '(50 2))) 

(defvar time-interval-unit-types 0) 

(defun time-interval-to-seconds (string &aux (total 0))
  "return a number of seconds parsed from string.
if the string cannot be parsed, the first value is nil
and the second is a string describing the problem."
  (if (numberp string) string
      (do ((ix 0)
           (l (length string)))
          ((or (null ix) (>= ix l)) total)
        (let ((token-start
               (position #\space (the string (string string)) :start ix :test-not #'char-equal)))
          (if (null token-start) (return total))
          (let* ((token-end
                  (position #\space (the string (string string)) :start token-start :test
                            #'char-equal));;works even if end nil!
                 
                 (units (parse-number string token-start token-end)))
            (if (null units)
                (return
                 (values ()
                         (format () "invalid number: ~a"
                                 (subseq string token-start token-end)))))
            (let ((token-start
                   (position #\space (the string (string string)) :start token-end :test-not
                             #'char-equal)))
              (if (null token-start)
                  (return (values () "units specification missing from time string")))
              (setq ix
                    (position #\space (the string (string string)) :start token-start :test
                              #'char-equal))
              (let ((uval
                     (loop for i from 0 below time-interval-unit-types finally (return ()) do
                           (if
                            (string-equal (aref time-interval-array i 0) string :start1 0 :start2 token-start
					  :end1 () :end2 ix)
                            (return (aref time-interval-array i 1))))))
                (if uval
                    (progn
                      (if (char-equal #\y (aref string token-start));years?
                          
                          (if (> units 3);good till 1999.
                              
                              (incf total (* (floor units 4) (time-interval-to-seconds "1 day")))))
                      (incf total (* uval units)))
                    (return
                     (values ()
                             (format () "unknown time spec: ~a"
                                     (subseq string token-start ix)))))))))))) 

(defun parse-interval-or-never (string &optional from to)
  "parse a string either describing a time interval or \"never\".
for a time interval, the number of seconds is returned.
for \"never\" or variations, nil is returned."
  (if (numberp string) string
      (progn
        (setq string
              (string-trim '(#\space #\tab)
                           (if (null (or from to)) string (subseq string from to))))
        (if (member string '("none" "no" "" "never" "not ever" "nil" "()") :test 'equalp) ()
            (multiple-value-bind (val err) (time-interval-to-seconds string)
              (if err (error "~a: ~a" string err) val)))))) 

(defun parse-time (string &optional (allow-intervals? t))
  (aif (search "from" string :test #'string-equal)
    (+ (parse-interval-or-never (subseq string 0 it))
       (if (search "now" (subseq string (+ it 4)) :test #'string-equal)
         (get-universal-time)
         (parse-date-and-time-string (subseq string (+ it 4)) t)))
    (if (search "now" string :test #'string-equal)
      (get-universal-time)
      (if (search ":" string)
        (parse-date-and-time-string (subseq string (+ it 4)) t)
        (if allow-intervals?
          (parse-interval-or-never string)
          (error "~a is not parseable" string))))))

#+TEST
(defun test-parse-time ()
  (print-universal-date-verbose (parse-time "now") *standard-output*)
  (terpri)
  (print-universal-date-verbose (parse-time "1 minute from now") *standard-output*)
  (terpri)
  (print-universal-date-verbose (parse-time "1 minute from 10:12") *standard-output*)
  (terpri)
  (print-universal-date-verbose (parse-time "1 minute from 9/9/89 10:12:43 AM") *standard-output*)
  (terpri)
  (print-time-interval (parse-time "1 minute") *standard-output*))
  
    
#+LATER
(defun print-interval-or-never (val &optional (stream t))
  "print the interval-or-never val on stream.
val can be a number of seconds, or nil for never."
  (if (null val) (format stream "never") (format stream "~a" (seconds-to-interval-string val))))

;;; ------------------------------------------------------------------------------------------------


(defun init-time-interval-array ()
  (setf (aref time-interval-array 0 0) "second")
  (setf (aref time-interval-array 0 1) 1)
  (setq time-interval-unit-types 1)
  (dolist (l
    '(("1 second" "seconds" "s" "sec" "secs") ("60 seconds" "minute" "minutes" "min" "mins" "m")
     ("60 minutes" "hour" "hours" "hr" "hrs" "h") ("24 hours" "day" "days")
     ("7 days" "week" "weeks" "wk" "wks") ("365 days" "year" "years" "yr" "yrs")))
    (let ((value (time-interval-to-seconds (car l))))
      (dolist (newname (cdr l))
        (setf (aref time-interval-array time-interval-unit-types 0) newname)
        (setf (aref time-interval-array time-interval-unit-types 1) value)
        (incf time-interval-unit-types))))) 


(init-time-interval-array) 

#+ANOTHER-VERSION
(defun print-time-interval (secs &optional stream print-seconds?)
  "return a string describing a time interval secs in seconds."
  (if (zerop secs) "0 seconds"
      (do ((i 0 (1+ i))
           (last ()))
          ((>= i time-interval-unit-types) (print-time-interval-1 last secs stream print-seconds?))
        (if (> (aref time-interval-array i 1) secs)
            (return (print-time-interval-1 last secs stream print-seconds?))
            (if
             (or (null last)
                 (not (= (aref time-interval-array i 1) (aref time-interval-array last 1))))
             (setq last i)))))) 

#+ANOTHER-VERSION
(defvar *four-year-cycle* (time-interval-to-seconds "4 years")) 

#+ANOTHER-VERSION
(defvar *seconds-in-day* (time-interval-to-seconds "1 day")) 

#+ANOTHER-VERSION
(defun print-time-interval-1 (index secs stream print-seconds?)
  (if (not (zerop (floor secs *four-year-cycle*)))
      (decf secs (* (floor secs *four-year-cycle*) *seconds-in-day*)))
  (let ((quo (floor secs (aref time-interval-array index 1)))
        (rem (rem secs (aref time-interval-array index 1))))
    (if (zerop rem) (format stream "~d ~a~p" quo (aref time-interval-array index 0) quo)
        (format stream "~d ~a~p ~a" quo (aref time-interval-array index 0) quo
                (seconds-to-interval-string rem)))))

#+test
(defun test-print-time-interval ()
  (spy (print-time-interval 123456  nil nil))
  (spy (print-time-interval 123456  nil t))
  (spy (print-time-interval 123456  nil))
  (spy (print-time-interval -123456 nil))
  (spy (print-time-interval 12345   nil))
  (spy (print-time-interval 1234    nil))
  (spy (print-time-interval 123     nil))
  (spy (print-time-interval 12      nil))
  (spy (print-time-interval 12      nil t))
  (spy (print-time-interval 0       nil t)))

(defun print-universal-date-verbose (ut &optional (stream *standard-output*))
  "print the universal-time ut in verbose form on stream, decoding for timezone.
if stream is nil, construct and return a string."
  (multiple-value-bind (seconds minutes hours day month year day-of-the-week)
      (decode-universal-time ut)
    (print-date-verbose seconds minutes hours day month year day-of-the-week stream)))

(defun print-date-verbose (seconds minutes hours day month year day-of-the-week
		   &optional (stream *standard-output*))
  "print the date and time in verbose form on stream.
if stream is nil, construct and return a string."
  (setq month (month->string month)
	day-of-the-week (day->string day-of-the-week))
  (format stream
	  "~a the ~:r of ~a, ~d; ~d:~2,'0d:~2,'0d ~a"
	  day-of-the-week day month year (1+ (rem (+ hours 11.) 12.)) minutes seconds
	  (cond ((and (zerop seconds)
		      (zerop minutes)
		      (member hours '(0 12.) :test #'eq))
		 (if (= hours 0) "midnight" "noon"))
		((>= hours 12.) "pm")
		(t "am"))))

;;; ---------------------------------------------------------------------------

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

  ;;; ---------------------------------------------------------------------------
  
  (generate-time-part-function second 0)
  (generate-time-part-function minute 1)
  (generate-time-part-function hour 2)
  (generate-time-part-function date 3)
  (generate-time-part-function month 4)
  (generate-time-part-function year 5)
  (generate-time-part-function day-of-week 6)
  (generate-time-part-function daylight-savings-time-p 7))


;;; ---------------------------------------------------------------------------
;;; format-date
;;; ---------------------------------------------------------------------------

(defun format-date (format date &optional stream time-zone)
  "Formats universal dates using the same format specifiers as NSDateFormatter. The format is:

%% - A '%' character
%a - Abbreviated weekday name
%A - Full weekday name
%b - Abbreviated month name
%B - Full month name
%c - Shorthand for \"%X %x\", the locale format for date and time
%d - Day of the month as a decimal number [01-31]
%e - Same as %d but does not print the leading 0 for days 1 through 9 
     [unlike strftime[], does not print a leading space]
%F - Milliseconds as a decimal number [000-999]
%H - Hour based on a 24-hour clock as a decimal number [00-23]
%I - Hour based on a 12-hour clock as a decimal number [01-12]
%j - Day of the year as a decimal number [001-366]
%m - Month as a decimal number [01-12]
%M - Minute as a decimal number [00-59]
%p - AM/PM designation for the locale
%S - Second as a decimal number [00-59]
%w - Weekday as a decimal number [0-6], where Sunday is 0
%x - Date using the date representation for the locale, including 
     the time zone [produces different results from strftime[]]
%X - Time using the time representation for the locale [produces 
     different results from strftime[]]
%y - Year without century [00-99]
%Y - Year with century [such as 1990]
%Z - Time zone name [such as Pacific Daylight Time; 
     produces different results from strftime[]]
%z - Time zone offset in hours and minutes from GMT [HHMM]

None of %c, %F, %p, %x, %X, %Z, %z are implemented."
  (declare (ignore time-zone))
  (let ((format-length (length format)))
    (format stream "~{~A~}"
            (loop for index = 0 then (1+ index) 
                  while (< index format-length) collect 
                  (let ((char (aref format index)))
                    (cond ((char= #\% char)
                           (setf char (aref format (incf index)))
                           (cond 
                            ;; %% - A '%' character
                            ((char= char #\%) #\%)
                            
                            ;; %a - Abbreviated weekday name
                            ((char= char #\a) (day->string (time-day-of-week date) :short))
                            
                            ;; %A - Full weekday name
                            ((char= char #\A) (day->string (time-day-of-week date) :long))
                            
                            ;; %b - Abbreviated month name
                            ((char= char #\b) (month->string (time-month date) :short))
                            
                            ;; %B - Full month name
                            ((char= char #\B) (month->string (time-month date) :long))
                            
                            ;; %c - Shorthand for "%X, %x", the locale format for date and time
                            ((char= char #\c) (nyi))
                            
                            ;; %d - Day of the month as a decimal number [01-31]
                            ((char= char #\d) (format nil "~2,'0D" (time-date date)))
                            
                            ;; %e - Same as %d but does not print the leading 0 for days 1 through 9 
                            ;;      Unlike strftime, does not print a leading space
                            ((char= char #\e) (format nil "~D" (time-date date)))
                            
                            ;; %F - Milliseconds as a decimal number [000-999]
                            ((char= char #\F) (nyi))
                            
                            ;; %H - Hour based on a 24-hour clock as a decimal number [00-23]
                            ((char= char #\H) (format nil "~2,'0D" (time-hour date)))
                            
                            ;; %I - Hour based on a 12-hour clock as a decimal number [01-12]
                            ((char= char #\I) (format nil "~2,'0D" (1+ (mod (time-hour date) 12))))
                            
                            ;; %j - Day of the year as a decimal number [001-366]
                            ((char= char #\j) (format nil "~3,'0D" (day-of-year date)))
                            
                            ;; %m - Month as a decimal number [01-12]
                            ((char= char #\m) (format nil "~2,'0D" (time-month date)))
                            
                            ;; %M - Minute as a decimal number [00-59]
                            ((char= char #\M) (format nil "~2,'0D" (time-minute date)))
                            
                            ;; %p - AM/PM designation for the locale
                            ((char= char #\p) (nyi))
                            
                            ;; %S - Second as a decimal number [00-59]
                            ((char= char #\S) (format nil "~2,'0D" (time-second date)))
                            
                            ;; %w - Weekday as a decimal number [0-6], where Sunday is 0
                            ((char= char #\w) (format nil "~D" (time-day-of-week date)))
                            
                            ;; %x - Date using the date representation for the locale, 
                            ;;      including the time zone [produces different results from strftime]
                            ((char= char #\x) (nyi))
                            
                            ;; %X - Time using the time representation for the locale 
                            ;;      [produces different results from strftime]
                            ((char= char #\X) (nyi))
                            
                            ;; %y - Year without century [00-99]
                            ((char= char #\y) 
                             (let ((year-string (format nil "~,2A" (time-year date))))
                               (subseq year-string (- (length year-string) 2))))
                            
                            ;; %Y - Year with century [such as 1990]
                            ((char= char #\Y) (format nil "~D" (time-year date)))
                            
                            ;; %Z - Time zone name (such as Pacific Daylight Time; 
                            ;;      produces different results from strftime.
                            ((char= char #\Z) (nyi))
                            
                            ;; %z - Time zone offset in hours and minutes from GMT [HHMM]
                            ((char= char #\z) (nyi))
                            
                            (t
                             (error "Ouch - unknown formatter '%~c" char))))
                          (t char)))))))
  
;;; ---------------------------------------------------------------------------

(defun days-in-month (month &optional leap-year?)
  "Returns the number of days in the specified month. The month should be
between 1 and 12."
  (+ (nth (1- month) +days-per-month+) (if (and (= month 2) leap-year?) 1 0)))

;;; ---------------------------------------------------------------------------

(defun leap-year-p (year)
  "Returns t if the specified year is a leap year. I.e. if the year
is divisible by four but not by 100 or if it is divisible by 400."
  (or (and (= (mod year 4) 0)               ; logand is faster but less perspicuous
           (not (= (mod year 100) 0)))
      (= (mod year 400) 0)))

;;; ---------------------------------------------------------------------------

(defun day-of-year (date)
  "Returns the day of the year [1 to 366] of the specified date [which must be \(CL\) universal time format.]" 
  (let ((leap-year? (leap-year-p (time-year date))))
    (+ (loop for month from 1 to (1- (time-month date)) sum
             (days-in-month month leap-year?))
       (time-date date))))

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************