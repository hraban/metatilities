(in-package metatilities)

(lift:deftestsuite test-leap-year-p ()
  ()
  (:test ((lift:ensure (not (leap-year-p 1900)))))
  (:test ((lift:ensure (leap-year-p 1904))))
  (:test ((lift:ensure (leap-year-p 2000))))
  (:test ((lift:ensure (leap-year-p 1996))))
  (:test ((lift:ensure (not (leap-year-p 1997))))))