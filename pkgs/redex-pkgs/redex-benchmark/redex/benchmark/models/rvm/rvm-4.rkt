#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "mishandling branches when then branch needs more stack than else branch; bug in the boxenv case not checking a stack bound")

(define-rewrite bug3
  ((side-condition (< (term n_p) (term n_l)))
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest)
  #:exactly-once)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug3)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(let-one 'x
            (branch #f (boxenv 0 'y) (loc-box 0))))

(test small-counter-example)
