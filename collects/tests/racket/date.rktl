
(load-relative "loadtest.rktl")

(Section 'date)

(require mzlib/date)

(define (test-find s m h d mo y)
  (let* ([secs (find-seconds s m h d mo y)]
	 [date (seconds->date secs)])
    (test #t 'same
	  (and (= s (date-second date))
	       (= m (date-minute date))
	       (= h (date-hour date))
	       (= d (date-day date))
	       (= mo (date-month date))
	       (= y (date-year date))))))

(test-find 0 0 0 1 4 1975)
(test-find 0 0 0 1 4 2005)

; date->string
(let* ([secs (find-seconds 1 2 3 4 5 2006)]
       [d (seconds->date secs)])
  (define (test-string fmt time? result)
    (test (parameterize ([date-display-format fmt])
            (date->string d time?))
          fmt result))
  (test secs date->seconds d)
  
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
  (test-string 'iso-8601 #t "2006-05-04 03:02:01")
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

;; 1990 April 1 was start of daylight savings:
(test-find 0 0 1 1 4 1990) ; ok
(let ([s (find-seconds 1 0 3 1 4 1990)]) ; ok
  (when (date-dst? (seconds->date s))
    ;; We have daylight savings here; 2:01 AM doesn't exist
    (err/rt-test (find-seconds 0 1 2 1 4 1990) exn:fail?)
    ;; This date is ambiguous; find-seconds should find
    ;;  one of the two possible values, though:
    (test-find 0 30 1 27 10 1996)))

;; bug fixes
(test "JD 12" julian/scalinger->string 12)
(test "JD 123" julian/scalinger->string 123)

(report-errs)
