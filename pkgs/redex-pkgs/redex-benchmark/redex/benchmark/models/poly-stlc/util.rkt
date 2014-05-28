#lang racket

(require redex/benchmark)

(provide bug-mod-rw
         test)

(define-rewrite bug-mod-rw
  redex/examples/poly-stlc
  ==> 
  (submod ".." poly-stlc)
  #:context (require))

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
