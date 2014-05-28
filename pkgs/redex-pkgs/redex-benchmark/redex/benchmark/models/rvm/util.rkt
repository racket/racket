#lang racket/base

(require redex/benchmark)

(provide bug-mod-rw
         rt-rw
         test)

(define-rewrite bug-mod-1
  redex/examples/racket-machine/grammar
  ==> 
  (submod ".." grammar)
  #:context (require))

(define-rewrite bug-mod-2
  redex/examples/racket-machine/verification
  ==> 
  (submod ".." verification)
  #:context (require))

(define-rewrite bug-mod-3
  redex/examples/racket-machine/randomized-tests
  ==> 
  (submod ".." randomized-tests)
  #:context (require))

;; adjust large numbers to keep the reduction from blowing up
(define-rewrite rt-rw
  [`(,(and (or 'let-void 'let-void-box) i) ,n ,e)
       `(,i ,n ,(recur (+ depth n) #f e))]
  ==>
  [`(,(and (or 'let-void 'let-void-box) i) ,n ,e)
       `(,i ,(modulo n 20) ,(recur (+ depth (modulo n 20)) #f e))]
  #:exactly-once
  #:context (match))

(define-rewrite/compose bug-mod-rw bug-mod-1 bug-mod-2 bug-mod-3)

(define-syntax-rule (test cexp)
  (module+ test
    (require (except-in rackunit check)
             (submod ".." generators check-mod))
    (check-equal? (check cexp) #f)))
