#lang racket/base
(require rackunit
         racket/class
         (prefix-in srfi: srfi/19)
         db/base
         db/private/generic/sql-convert
         "../config.rkt")
(require/expose
 db/private/postgresql/dbsystem
 (parse-date
  parse-time
  parse-time-tz
  parse-timestamp
  parse-timestamp-tz))

(provide sql-types:test)

(define sql-types:test
  (test-suite "SQL support utilities"
  (test-suite "Parsing SQL types"
    (test-case "date"
      (check-equal? (parse-date "1980-08-17")
                    (make-sql-date 1980 08 17)))
    (test-case "time"
      (check-equal? (parse-time "12:34:56")
                    (make-sql-time 12 34 56 0 #f))
      (check-equal? (parse-time "12:34:56.789")
                    (make-sql-time 12 34 56 789000000 #f))
      (check-equal? (parse-time "12:34:56.000789")
                    (make-sql-time 12 34 56 000789000 #f)))
    (test-case "timetz"
      (check-equal? (parse-time-tz "12:34:56+0123")
                    (make-sql-time 12 34 56 0 4980))
      (check-equal? (parse-time-tz "12:34:56.789+0123")
                    (make-sql-time 12 34 56 789000000 4980))
      (check-equal? (parse-time-tz "12:34:56.000789-0123")
                    (make-sql-time 12 34 56 000789000 -4980)))
    (test-case "timestamp"
      (check-equal?
       (parse-timestamp "1980-08-17 12:34:56")
       (make-sql-timestamp 1980 08 17 12 34 56 0 #f))
      (check-equal?
       (parse-timestamp "1980-08-17 12:34:56.123")
       (make-sql-timestamp 1980 08 17 12 34 56 123000000 #f))
      (check-equal?
       (parse-timestamp "1980-08-17 12:34:56.000123")
       (make-sql-timestamp 1980 08 17 12 34 56 000123000 #f)))
    (test-case "timestamp-with-time-zone"
      (check-equal?
       (parse-timestamp-tz "1980-08-17 12:34:56+0123")
       (make-sql-timestamp 1980 08 17 12 34 56 0 4980))
      (check-equal?
       (parse-timestamp-tz "1980-08-17 12:34:56.123+0123")
       (make-sql-timestamp 1980 08 17 12 34 56 123000000 4980))
      (check-equal?
       (parse-timestamp-tz "1980-08-17 12:34:56.000123-0123")
       (make-sql-timestamp 1980 08 17 12 34 56 000123000 -4980)))
    (test-case "numeric"
      (check-equal? (parse-decimal "12345678901234567890")
                    12345678901234567890)
      (check-equal? (parse-decimal "-12345678901234567890")
                    -12345678901234567890)))
  (test-suite "Auxiliaries"
    (test-case "exact->decimal-string"
      (check-equal? (exact->decimal-string 12) "12")
      (check-equal? (exact->decimal-string 1000) "1000")
      (check-equal? (exact->decimal-string 1/2) "0.5")
      (check-equal? (exact->decimal-string 1/4) "0.25")
      (check-equal? (exact->decimal-string 1/10) "0.1")
      (check-equal? (exact->decimal-string 1/20) "0.05")
      (check-equal? (exact->decimal-string 1/3) #f))
    (test-case "exact->scaled-integer"
      (check-equal? (exact->scaled-integer 12) (cons 12 0))
      ;; (check-equal? (exact->scaled-integer 1000) "1000")
      (check-equal? (exact->scaled-integer 1/2) (cons 5 1))
      (check-equal? (exact->scaled-integer 1/4) (cons 25 2))
      (check-equal? (exact->scaled-integer 1/10) (cons 1 1))
      (check-equal? (exact->scaled-integer 1/20) (cons 5 2))
      (check-equal? (exact->scaled-integer 1/3) #f)))))
