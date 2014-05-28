#lang racket

(require redex/benchmark)

(provide bug-mod-rw
         test)

(define-rewrite bug-mod1
  redex/examples/list-machine/list-machine
  ==>
  (submod ".." list-machine)
  #:context (require))

(define-rewrite bug-mod2
  redex/examples/list-machine/list-machine-typing
  ==>
  (submod ".." list-machine-typing)
  #:context (require))

(define-rewrite/compose bug-mod-rw
  bug-mod1 bug-mod2)

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
