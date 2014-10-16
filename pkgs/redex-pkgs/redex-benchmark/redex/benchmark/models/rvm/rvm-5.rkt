#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "mishandling branches when then branch needs more stack than else branch; bug in the let-rec case not checking a stack bound")

(define-rewrite bug5
  ((side-condition (<= (term n) (term n_l)))
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest)
  #:exactly-once)

(define-rewrite bug5-jdg
  ((n<= n n_l)
   . rest)
  ==> 
  rest
  #:context (define-judgment-form)
  #:variables (rest)
  #:exactly-once)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug5)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite (lib "redex/benchmark/models/rvm/verif-jdg.rkt") verif-jdg bug5-jdg)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(let-void 1 
             (branch #f 
                     (let-rec ((lam () (0) 'x)) 'y) 
                     (loc-noclr 0))))

(test small-counter-example)
