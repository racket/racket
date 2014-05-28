#lang racket

(require "../../private/rw-defs.rkt")

(provide bug-mod-rw
         test)

(define-rewrite bug-mod-rw
  (lib "redex/examples/stlc+lists.rkt")
  ==> 
  (submod ".." stlc)
  #:context (require))

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
