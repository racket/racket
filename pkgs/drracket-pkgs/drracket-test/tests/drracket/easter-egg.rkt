#lang racket/base
(require racket/class
         racket/date
         "private/easter-egg-lib.rkt")

(define (run-tests)
  (start-up-on-day  2 14 "Valentine's Day")
  (start-up-on-day  3  2 "Texas Indepenence Day")
  (start-up-on-day  3 26 "Prince Kuhio Day")
  (start-up-on-day  6 11 "King Kamehameha Day")
  (start-up-on-day  7 30 "Eli's birthday")
  (start-up-on-day 10 29 "Matthias's birthday")
  (start-up-on-day 10 31 "Halloween")
  (start-up-on-day 11  1 "Matthew's birthday")
  (start-up-on-day 12 10 "Ada Lovelace's birthday")
  (start-up-on-day 12 25 "Christmas")
  
  (define now (current-seconds))
  (define week-day (date-week-day (seconds->date now)))
  (define seconds-in-a-day (* 60 60 24))
  (define sunday-secs (+ (* (- 7 week-day) seconds-in-a-day) now))
  (define sunday (seconds->date sunday-secs))
  (define monday (seconds->date (+ sunday-secs seconds-in-a-day)))
  (start-up-on-day (date-month sunday)
                   (date-day sunday)
                   "Weekend")
  (start-up-on-day (date-month monday)
                   (date-day monday)
                   "Weekday"))



(run-tests)
