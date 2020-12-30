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
  (let ([pre-cpu (current-time 'time-process)]
        [pre-real (current-time 'time-monotonic)]
        [pre-gc (#%$gc-cpu-time)])
    (call-with-values (lambda () (apply f extra))
      (lambda args
        (let ([post-cpu (current-time 'time-process)]
              [post-real (current-time 'time-monotonic)]
              [post-gc (#%$gc-cpu-time)])
          (values
           args
           (inexact->exact (floor (time->ms (time-difference post-cpu pre-cpu))))
           (inexact->exact (floor (time->ms (time-difference post-real pre-real))))
           (inexact->exact (floor (time->ms (time-difference post-gc pre-gc))))))))))

(define (current-gc-milliseconds)
  (inexact->exact (floor (time->ms (#%$gc-cpu-time)))))

(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))

(define (current-inexact-milliseconds)
  (time->ms (current-time 'time-utc)))

(define (current-seconds)
  (let ((t (current-time 'time-utc)))
    (time-second t)))
