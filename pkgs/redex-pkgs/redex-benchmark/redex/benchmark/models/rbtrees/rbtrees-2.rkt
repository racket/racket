#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the first case is removed from balance")

(define-rewrite bug2
  ([(balance (B (R (R t_1 n_1 t_2) n_2 t_3) n_3 t_4))
    (R (B t_1 n_1 t_2) n_2 (B t_3 n_3 t_4))]
   . other-cases)
  ==> 
  other-cases
  #:context (define-metafunction)
  #:variables (other-cases))

(include/rewrite (lib "redex/examples/rbtrees.rkt") rbtrees bug2)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(require (only-in (submod "." rbtrees) num->n))

(define small-counter-example 
  (term (B (R E (num->n 1) E)
           (num->n 2)
           E)))

(test small-counter-example)
