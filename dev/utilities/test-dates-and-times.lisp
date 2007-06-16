(in-package #:metatilities)

(lift:deftestsuite test-leap-year-p ()
  ()
  (:test ((lift:ensure (not (leap-year-p 1900)))))
  (:test ((lift:ensure (leap-year-p 1904))))
  (:test ((lift:ensure (leap-year-p 2000))))
  (:test ((lift:ensure (leap-year-p 1996))))
  (:test ((lift:ensure (not (leap-year-p 1997))))))

#|
(use-package 'lift)

(deftestsuite test-leap-year-p ()
  ()
  (:tests
   ((ensure (leap-year-p 1904)))
   (div-by-four (ensure (leap-year-p 2000)))
   ((ensure (leap-year-p 1996))))
  (:test ((ensure-null (leap-year-p 1900))))
  (:test ((ensure-null (leap-year-p 1997)))))

(print-tests :start-at 'test-leap-year-p)

|#