#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "ins does no rebalancing")

(define-rewrite bug1
  (balance (c . stuff))
  ==> 
  (c . stuff)
  #:context (define-metafunction)
  #:variables (stuff))

(include/rewrite (lib "redex/examples/rbtrees.rkt") rbtrees bug1)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(require (only-in (submod "." rbtrees) num->n))

(define small-counter-example 
  (term (B (R E (num->n 0) E)
           (num->n 2)
           E)))

(test small-counter-example)
