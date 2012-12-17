#lang racket/base
(require rackunit
         racket/class
         racket/serialize
         (prefix-in srfi: srfi/19)
         db/base
         db/private/generic/sql-convert
         "../config.rkt")

(provide sql-types:test)

(define sql-types:test
  (test-suite "SQL support utilities"
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
      (check-equal? (exact->scaled-integer 1/3) #f))
    (test-case "sql-bits deserialization"
      (check-equal? (deserialize 
                     '((3)
                       1
                       (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-bits-v0))
                       0
                       ()
                       ()
                       (0 0 (u . #"") 0)))
                    (string->sql-bits ""))
      (check-equal? (deserialize
                     '((3)
                       1
                       (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-bits-v0))
                       0
                       ()
                       ()
                       (0 4 (u . #"\260") 0)))
                    (string->sql-bits "1011"))
      (check-equal? (deserialize
                     '((3)
                       1
                       (((lib "db/private/generic/sql-data.rkt") . deserialize-info:sql-bits-v0))
                       0
                       ()
                       ()
                       (0 30 (u . #"\377\377\360\0") 0)))
                    (string->sql-bits (string-append (make-string 20 #\1) (make-string 10 #\0)))))))
