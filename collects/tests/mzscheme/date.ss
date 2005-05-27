
(load-relative "loadtest.ss")

(SECTION 'date)

(require (lib "date.ss"))

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

; Bad dates
(err/rt-test (find-seconds 0 0 0 0 0 1990) exn:fail?)
(err/rt-test (find-seconds 0 0 0 0 1 1990) exn:fail?)
(err/rt-test (find-seconds 0 0 0 1 0 1990) exn:fail?)

; Early/late
(err/rt-test (find-seconds 0 0 0 1 1 1490) exn:fail?)
(err/rt-test (find-seconds 0 0 0 1 1 2890) exn:fail?)

; 1990 April 1 was start of daylight savings:
(test-find 0 0 1 1 4 1990) ; ok
(let ([s (find-seconds 1 0 3 1 4 1990)]) ; ok
  (when (date-dst? (seconds->date s))
	; We have daylight savings here; 2:01 AM doesn't exist
	(err/rt-test (find-seconds 0 1 2 1 4 1990) exn:fail?)
	; This date is ambiguous; find-seconds should find
	;  one of the two possible values, though:
	(test-find 0 30 1 27 10 1996)))

(report-errs)
