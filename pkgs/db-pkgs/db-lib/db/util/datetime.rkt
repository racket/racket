#lang racket/base
(require racket/contract/base
         racket/match
         (prefix-in srfi: srfi/19)
         db/private/generic/sql-data)

(define (sql-datetime->srfi-date datetime)
  (match datetime
    [(struct sql-date (year month day))
     (srfi:make-date 0 0 0 0 day month year 0)]
    [(struct sql-time (hour minute second nanosecond tz))
     (srfi:make-date nanosecond second minute hour 0 0 0 (or tz 0))]
    [(struct sql-timestamp (year month day hour minute second nanosecond tz))
     (srfi:make-date nanosecond second minute hour day month year (or tz 0))]))

(define (srfi-date->sql-date date)
  (make-sql-date (srfi:date-year date)
                 (srfi:date-month date)
                 (srfi:date-day date)))

(define (srfi-date->sql-time* date tz? ns)
  (make-sql-time (srfi:date-hour date)
                 (srfi:date-minute date)
                 (srfi:date-second date)
                 (or ns (srfi:date-nanosecond date))
                 (and tz? (srfi:date-zone-offset date))))

(define (srfi-date->sql-time date [ns #f])
  (srfi-date->sql-time* date #f ns))

(define (srfi-date->sql-time-tz date [ns #f])
  (srfi-date->sql-time* date #t ns))

(define (srfi-date->sql-timestamp* date tz? ns)
  (make-sql-timestamp (srfi:date-year date)
                      (srfi:date-month date)
                      (srfi:date-day date)
                      (srfi:date-hour date)
                      (srfi:date-minute date)
                      (srfi:date-second date)
                      (or ns (srfi:date-nanosecond date))
                      (and tz? (srfi:date-zone-offset date))))

(define (srfi-date->sql-timestamp date [ns #f])
  (srfi-date->sql-timestamp* date #f ns))

(define (srfi-date->sql-timestamp-tz date [ns #f])
  (srfi-date->sql-timestamp* date #t ns))

(define (sql-day-time-interval->seconds x)
  (+ (* (sql-interval-hours x) 60 60)
     (* (sql-interval-minutes x) 60)
     (sql-interval-seconds x)
     (/ (sql-interval-nanoseconds x) #i1e9)))

;; ============================================================

;; Note: MySQL allows 0 month, 0 day, etc.

(provide/contract
 [sql-datetime->srfi-date
  (-> (or/c sql-date? sql-time? sql-timestamp?)
      srfi:date?)]
 [srfi-date->sql-date
  (-> srfi:date? sql-date?)]
 [srfi-date->sql-time
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-time?)]
 [srfi-date->sql-time-tz
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-time?)]
 [srfi-date->sql-timestamp
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-timestamp?)]
 [srfi-date->sql-timestamp-tz
  (->* (srfi:date?) ((or/c exact-nonnegative-integer? #f))
       sql-timestamp?)]

 [sql-day-time-interval->seconds
  (-> sql-day-time-interval? rational?)])
