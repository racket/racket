#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "wrong order of arguments to replace call")

(define-rewrite bug2
  (λ (x_new τ) (subst (replace M y x_new) x M_x))
  ==> 
  (λ (x_new τ) (subst (replace M x_new y) x M_x))
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists+subst.rkt") stlc-sub bug2)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example 
  (term ((λ (x int) (λ (y int) y)) 1)))

(test small-counter-example)
