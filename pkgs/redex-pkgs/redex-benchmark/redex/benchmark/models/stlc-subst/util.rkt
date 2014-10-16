#lang racket

(require redex/benchmark)

(provide bug-mod-rw
         subst-check-rw
         test)

(define-rewrite bug-mod-rw
  redex/examples/stlc+lists+subst
  ==> 
  (submod ".." stlc-sub)
  #:context (require))

(define-rewrite subst-check-rw
  (define (check term) . rest)
  ==>
  (define (check term)
    (subst-check term))
  #:variables (rest term)
  #:once-only)

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
