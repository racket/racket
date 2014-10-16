#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "doesn't increment black depth in non-empty case")

(define-rewrite bug3
  (rbt (B (c_1 t_11 n_1 t_12) n (c_2 t_21 n_2 t_22)) n_1min n_2max (s n_bd))
  ==> 
  (rbt (B (c_1 t_11 n_1 t_12) n (c_2 t_21 n_2 t_22)) n_1min n_2max n_bd)
  #:context (define-judgment-form)
  #:once-only)

(include/rewrite (lib "redex/examples/rbtrees.rkt") rbtrees bug3)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(require (only-in (submod "." rbtrees) num->n))

(define small-counter-example 
  (term (B 
         (B 
          (R E (num->n 1) E)
          (num->n 2)
          (R E (num->n 3) E))
         (num->n 4)
         (R E (num->n 5) E))))

(test small-counter-example)
