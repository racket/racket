
(load-relative "loadtest.rktl")

(Section 'date)

(require racket/date)

(test #t date? (date* 0 0 0 1 1 -3000 0 0 #f -1000 0 "AST"))
(test #t date? (date* 60 59 23 31 12 3000 6 365 #t 1000 999999999 "ZST"))

(define (test-find s m h d mo y)
  (for ([local-time? (in-list '(#f #t))])
    (let* ([secs (find-seconds s m h d mo y local-time?)]
           [date (seconds->date secs local-time?)])
      (test #t 'same
            (and (= s (date-second date))
                 (= m (date-minute date))
                 (= h (date-hour date))
                 (= d (date-day date))
                 (= mo (date-month date))
                 (= y (date-year date)))))))

(test-find 0 0 0 1 4 1975)
(test-find 0 0 0 1 4 2005)

(test 0 find-seconds 0 0 0 1 1 1970 #f)
(test 32416215 find-seconds 15 30 4 11 1 1971 #f)

(let* ([s (current-seconds)]
       [d1 (seconds->date s)]
       [d2 (seconds->date (+ s 1/100000000))])
  (test 0 date*-nanosecond d1)
  (test 10 date*-nanosecond d2)
  (test (date*-time-zone-name d1) date*-time-zone-name d2)
  (test (struct-copy date d1) values (struct-copy date d2)))

; date->string
(let* ([secs (find-seconds 1 2 3 4 5 2006)]
       [d-some-tz (seconds->date secs)]
       [d (struct-copy date d-some-tz
                       [time-zone-offset -21600])]
       [d* (date* (date-second d) (date-minute d) (date-hour d)
                  (date-day d) (date-month d) (date-year d)
                  (date-week-day d) (date-year-day d) (date-dst? d)
                  (date-time-zone-offset d)
                  62500
                  "MDT")]) 
  (define (test-string fmt time? result)
    (test (parameterize ([date-display-format fmt])
            (date->string d time?))
          fmt result))
  (test secs date->seconds d)
  (test secs date*->seconds d)
  (test (+ secs #e0.0625) date*->seconds d*)
  (test (date->seconds d*) date->seconds (seconds->date (date*->seconds d*)))
  
  (test-string 'american #f "Thursday, May 4th, 2006")
  (test-string 'american #t "Thursday, May 4th, 2006 3:02:01am")
  (test-string 'chinese #f "2006/5/4 星期四")
  (test-string 'chinese #t "2006/5/4 星期四 03:02:01")
  (test-string 'german #f "4. Mai 2006")
  (test-string 'german #t "4. Mai 2006, 03.02")
  (test-string 'indian #f "4-5-2006")
  (test-string 'indian #t "4-5-2006 3:02:01am")
  (test-string 'irish #f "Thursday, 4th May 2006")
  (test-string 'irish #t "Thursday, 4th May 2006, 3:02am")
  (test-string 'iso-8601 #f "2006-05-04")
  (test-string 'iso-8601 #t "2006-05-04T03:02:01")
  (test-string 'rfc2822 #f "Thu, 4 May 2006")
  (test-string 'rfc2822 #t "Thu, 4 May 2006 03:02:01 -0600")
  (test-string 'julian #f "JD 2 453 860")
  (test-string 'julian #t "JD 2 453 860, 03:02:01")
  
  (test 2453860 date->julian/scalinger d)
  (test "JD 2 453 860" julian/scalinger->string 2453860))

;; Bad dates
(err/rt-test (find-seconds 0 0 0 0 0 1990) exn:fail?)
(err/rt-test (find-seconds 0 0 0 0 1 1990) exn:fail?)
(err/rt-test (find-seconds 0 0 0 1 0 1990) exn:fail?)

;; Early/late
(unless (eq? (expt 2 40) (eq-hash-code (expt 2 40))) ; 64-bit-machine?
  (err/rt-test (find-seconds 0 0 0 1 1 1490) exn:fail?)
  (err/rt-test (find-seconds 0 0 0 1 1 2890) exn:fail?))

;; Daylight saving checks:

;; March 13 was start of daylight saving in most of the US for 2011.
;; Check whether we seem to be in a US time zone with daylight saving:
(let ([d1 (seconds->date (find-seconds 0 0 1 13 1 2011))]
      [d2 (seconds->date (find-seconds 0 0 1 13 5 2011))])
  (when (and (not (date-dst? d1))
             (>= -10800 (date-time-zone-offset d1) -28800)
             (date-dst? d2)
             (>= -14400 (date-time-zone-offset d2) -25200))
    ;; It looks like we have US daylight saving:
    (test-find 0 0 1 13 3 2011) ; ok
    (let ([s (find-seconds 1 0 3 13 3 2011)]) ; ok
      ;; Since we have daylight savings here; 2:01 AM doesn't exist
      (err/rt-test (find-seconds 0 1 2 13 3 2011) exn:fail?)
      ;; During the end of DST in 2010,
      ;;  this date is ambiguous; find-seconds should find
      ;;  one of the two possible values, though:
      (test-find 0 30 1 7 11 2010))))

;; bug fixes
(test "JD 12" julian/scalinger->string 12)
(test "JD 123" julian/scalinger->string 123)

;; make sure that date* has the correct parent info
(test #t date*?
  (struct-copy date* (seconds->date (current-seconds))
               [hour #:parent date 5]))

(report-errs)
