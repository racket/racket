#lang racket

(require redex/benchmark)

(provide bug-mod-rw
         test)

(define-rewrite bug-mod-rw
  redex/benchmark/models/delim-cont/delim-cont
  ==> 
  (submod ".." delim-cont)
  #:context (require))

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
