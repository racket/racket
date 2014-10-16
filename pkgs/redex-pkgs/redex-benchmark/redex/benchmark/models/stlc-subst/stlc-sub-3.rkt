#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "swaps function and argument position in application")

(define-rewrite bug3
  ((subst M x M_x) (subst N x M_x))
  ==> 
  ((subst N x M_x) (subst M x M_x))
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists+subst.rkt") stlc-sub bug3)

(include/rewrite "generators.rkt" generators bug-mod-rw subst-check-rw)

(define small-counter-example 
  (term ((λ (x int) (+ 1)) 1)))

(test small-counter-example)
