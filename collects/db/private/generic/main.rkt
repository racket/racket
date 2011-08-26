#lang racket/base
(require racket/contract
         "interfaces.rkt"
         "sql-data.rkt"
         "functions.rkt")
(provide (struct-out simple-result)
         (struct-out rows-result)
         statement-binding?
         (all-from-out "functions.rkt"))

(provide sql-null
         sql-null?
         sql-null->false
         false->sql-null)

(provide/contract
 [struct sql-date ([year exact-integer?]
                   [month (integer-in 0 12)]
                   [day (integer-in 0 31)])]
 [struct sql-time ([hour (integer-in 0 23)]
                   [minute (integer-in 0 59)]
                   [second (integer-in 0 61)] ;; leap seconds
                   [nanosecond (integer-in 0 (sub1 #e1e9))]
                   [tz (or/c #f exact-integer?)])]
 [struct sql-timestamp ([year exact-integer?]
                        [month (integer-in 0 12)]
                        [day (integer-in 0 31)]
                        [hour (integer-in 0 23)]
                        [minute (integer-in 0 59)]
                        [second (integer-in 0 61)]
                        [nanosecond (integer-in 0 (sub1 #e1e9))]
                        [tz (or/c #f exact-integer?)])]
 [struct sql-interval ([years exact-integer?]
                       [months exact-integer?]
                       [days exact-integer?]
                       [hours exact-integer?]
                       [minutes exact-integer?]
                       [seconds exact-integer?]
                       [nanoseconds exact-integer?])]

 [sql-day-time-interval?
  (-> any/c boolean?)]
 [sql-year-month-interval?
  (-> any/c boolean?)]
 [sql-interval->sql-time
  (->* (sql-interval?) (any/c)
       any)]
 [sql-time->sql-interval
  (-> sql-time? sql-day-time-interval?)]

 [make-sql-bits
  (-> exact-nonnegative-integer? sql-bits?)]
 [sql-bits?
  (-> any/c boolean?)]
 [sql-bits-length
  (-> sql-bits? exact-nonnegative-integer?)]
 [sql-bits-ref
  (-> sql-bits? exact-nonnegative-integer? boolean?)]
 [sql-bits-set!
  (-> sql-bits? exact-nonnegative-integer? boolean? void?)]
 [sql-bits->list
  (-> sql-bits? (listof boolean?))]
 [list->sql-bits
  (-> (listof boolean?) sql-bits?)]
 [sql-bits->string
  (-> sql-bits? string?)]
 [string->sql-bits
  (-> string? sql-bits?)])
