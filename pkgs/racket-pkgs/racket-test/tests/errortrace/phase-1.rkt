#lang racket/base

(provide phase-1-tests)

;; Check that phase-1 annotations work, even in a namespace that is
;; otherwise empty at phase 1.
(define (phase-1-tests)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (dynamic-require 'errortrace #f)
    (eval #'(struct a (x)))))
