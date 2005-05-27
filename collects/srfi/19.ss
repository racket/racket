;; module loader for SRFI-19
(module |19| mzscheme
  (require (lib "time.ss" "srfi" "19"))
  (provide (all-from-except (lib "time.ss" "srfi" "19")
                            make-srfi:date srfi:date?
                            srfi:date-second 
                            srfi:date-minute 
                            srfi:date-hour 
                            srfi:date-day 
                            srfi:date-month
                            srfi:date-year 
                            srfi:date-year-day
                            srfi:date-week-day)
           (rename make-srfi:date make-date)
           (rename srfi:date? date)
           (rename srfi:date-second date-second)
           (rename srfi:date-minute date-minute)
           (rename srfi:date-hour date-hour)
           (rename srfi:date-day date-day)
           (rename srfi:date-month date-month)
           (rename srfi:date-year date-year)
           (rename srfi:date-year-day date-year-day)
           (rename srfi:date-week-day date-week-day)))
