#lang racket/base
(require rackunit)
(check-exn #rx".*expected number of values not received.*"
  (lambda () (for/fold () () 1)))
