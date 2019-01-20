(define-struct date (second
                     minute
                     hour
                     day
                     month
                     year
                     week-day
                     year-day
                     dst?
                     time-zone-offset)
  :guard (lambda (second
                  minute
                  hour
                  day
                  month
                  year
                  week-day
                  year-day
                  dst?
                  time-zone-offset
                  who)
           (check-integer who 0 60 second)
           (check-integer who 0 59 minute)
           (check-integer who 0 23 hour)
           (check-integer who 1 31 day)
           (check-integer who 1 12 month)
           (check who exact-integer? year)
           (check-integer who 0 6 week-day)
           (check-integer who 0 365 year-day)
           (check who boolean? dst?)
           (check who exact-integer? time-zone-offset)
           (values second
                   minute
                   hour
                   day
                   month
                   year
                   week-day
                   year-day
                   dst?
                   time-zone-offset)))

(define-struct date* date (nanosecond time-zone-name)
  :guard (lambda (second
                  minute
                  hour
                  day
                  month
                  year
                  week-day
                  year-day
                  dst?
                  time-zone-offset
                  nanosecond
                  time-zone-name
                  who)
           (check-integer who 0 999999999 nanosecond)
           (check who string? time-zone-name)
           (values second
                   minute
                   hour
                   day
                   month
                   year
                   week-day
                   year-day
                   dst?
                   time-zone-offset
                   nanosecond
                   (string->immutable-string time-zone-name))))

;; Direct constructor to avoid checks:
(define make-date*/direct
  (record-constructor (make-record-constructor-descriptor struct:date* #f #f)))

(define (time->ms t)
  (+ (* 1000. (time-second t))
     (/ (time-nanosecond t) 1000000.)))

(define (time-apply f extra)
  (let ([stats (statistics)])
    (call-with-values (lambda () (apply f extra))
      (lambda args
        (let ([new-stats (statistics)])
          (values
           args
           (inexact->exact (floor (time->ms
                                   (time-difference (sstats-cpu new-stats)
                                                    (sstats-cpu stats)))))
           (inexact->exact (floor (time->ms
                                   (time-difference (sstats-real new-stats)
                                                    (sstats-real stats)))))
           (inexact->exact (floor (time->ms
                                   (time-difference (sstats-gc-cpu new-stats)
                                                    (sstats-gc-cpu stats)))))))))))

(define (current-gc-milliseconds)
  (let ([stats (statistics)])
    (inexact->exact (floor (time->ms (sstats-gc-cpu stats))))))

(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))

(define (current-inexact-milliseconds)
  (time->ms (current-time 'time-utc)))

(define (current-seconds)
  (let ((t (current-time 'time-utc)))
    (time-second t)))

(define/who seconds->date
  (case-lambda
   [(s) (seconds->date s #t)]
   [(s local?)
    (check who real? s)
    (let* ([s (inexact->exact s)]
           [tm (make-time 'time-utc
                          (floor (* (- s (floor s)) 1000000000))
                          (floor s))]
           [d (if local?
                  (time-utc->date tm)
                  (time-utc->date tm 0))])
      (make-date*/direct (chez:date-second d)
                         (chez:date-minute d)
                         (chez:date-hour d)
                         (chez:date-day d)
                         (chez:date-month d)
                         (chez:date-year d)
                         (chez:date-week-day d)
                         (chez:date-year-day d)
                         (chez:date-dst? d)
                         (date-zone-offset d)
                         (date-nanosecond d)
                         (or (let ([n (date-zone-name d)])
                               (and n (string->immutable-string n)))
                             utc-string)))]))

(define utc-string (string->immutable-string "UTC"))
