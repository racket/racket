#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the order of the types in the function position of application has been swapped")

(define-rewrite bug3
  (typeof Γ M (σ → σ_2))
  ==>
  (typeof Γ M (σ_2 → σ))
  #:context (define-judgment-form)
  #:once-only)

(include/rewrite (lib "redex/examples/poly-stlc.rkt") poly-stlc bug3)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((λ (x int) [nil @ int])
         [nil @ int])))

(test small-counter-example)

