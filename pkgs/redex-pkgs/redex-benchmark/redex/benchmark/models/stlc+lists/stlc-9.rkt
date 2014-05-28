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

(include/rewrite (lib "redex/examples/stlc+lists.rkt") stlc bug9)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((λ (x int) (λ (y (list int)) x)) 1)))

(test small-counter-example)


