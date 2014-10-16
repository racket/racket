#lang racket

(require redex/benchmark)

(provide bug-mod-rw
         exn-rw
         test)

(define-rewrite bug-mod-rw
  redex/examples/let-poly
  ==> 
  (submod ".." let-poly)
  #:context (require))

(define-rewrite exn-rw
 (let ([t-type (type-check M)])
   body)
  ==>
  (with-handlers ([exn:fail? (λ (x) #f)])
    (let ([t-type (type-check M)])
      body))
  #:variables (body)
  #:exactly-once)

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
