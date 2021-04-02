#lang racket/base
(require rackunit)

(check-equal? (parameterize ((print-value-columns 10))
                (print-value-columns))
              10)
(check-equal? (parameterize ((print-value-columns +inf.0))
                (print-value-columns))
              +inf.0)
(check-exn exn:fail? (lambda () (print-value-columns -5)))
(check-exn exn:fail? (lambda () (print-value-columns 'wrong)))
