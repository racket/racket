#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "variables aren't required to match in lookup")

(define-rewrite bug9
  (lookup (x σ Γ) x)
  ==>
  (lookup (x σ Γ) x_2)
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/poly-stlc.rkt") poly-stlc bug9)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((λ (y (list int)) ([hd @ int] x))
         [nil @ int])))

(test small-counter-example)


