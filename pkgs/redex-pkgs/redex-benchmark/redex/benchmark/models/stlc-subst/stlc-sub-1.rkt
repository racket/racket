#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "forgot the variable case")

(define-rewrite bug1
  ([(subst x x M_x)
    M_x]
   . rest)
  ==> 
  rest
  #:context (define-metafunction)
  #:variables (rest)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists+subst.rkt") stlc-sub bug1)

(include/rewrite "generators.rkt" generators bug-mod-rw subst-check-rw)

(define small-counter-example 
  (term ((λ (x int) x) 1)))

(test small-counter-example)
