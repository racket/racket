#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "lookup always returns int")

(define-rewrite bug8
  [(lookup (x σ Γ) x)
   σ]
  ==>
  [(lookup (x σ Γ) x)
   int]
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/poly-stlc.rkt") poly-stlc bug8)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((λ (x (list int)) (+ x))
         [nil @ int])))

(test small-counter-example)

