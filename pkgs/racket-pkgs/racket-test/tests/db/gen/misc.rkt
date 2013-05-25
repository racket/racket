#lang racket/base
(require rackunit
         racket/class
         "../config.rkt")

(provide misc:test)

(require db/private/generic/common)

(define misc:test
  (test-suite "Misc internal function tests"
    (test-case "sql-skip-comments"
      (define (eat s [hash? #f]) (substring s (sql-skip-comments s 0 #:hash-comments? hash?)))
      (check-equal? (eat "/* blah ** blah */ insert")
                    " insert")
      (check-equal? (eat "-- blah\n  -- /* \nok")
                    "ok")
      (check-equal? (eat "#a\n# b c d\nok" #t)
                    "ok"))))
