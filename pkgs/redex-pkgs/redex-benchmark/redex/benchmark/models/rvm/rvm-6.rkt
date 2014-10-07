#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "forgot to implement the case-lam branch in verifier")

(define-rewrite bug6
  ((side-condition (term (AND (lam-verified? l s ?) ellipsis)))
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest ellipsis)
  #:exactly-once)

(define-rewrite bug6-jdg
  [(V (case-lam el) s n_l b γ η f s γ η)
   (lam-verified?* el s ?)]
  ==>
  [(V (case-lam el) s n_l b γ η f s γ η)]
  #:context (define-judgment-form)
  #:exactly-once)

(include/rewrite (lib "redex/examples/racket-machine/grammar.rkt") grammar)

(include/rewrite (lib "redex/examples/racket-machine/verification.rkt") verification bug6)

(include/rewrite (lib "redex/examples/racket-machine/randomized-tests.rkt") randomized-tests rt-rw)

(include/rewrite (lib "redex/benchmark/models/rvm/verif-jdg.rkt") verif-jdg bug6-jdg)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(application
    (case-lam (lam (val) () (loc-noclr 34)))
    'x))

(test small-counter-example)
