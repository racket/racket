#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "use a lambda-bound variable where a type variable should have been")

(define-rewrite bug1
  (tc-down (x y Γ) M (λ y κ) σ_ans)
  ==> 
  (tc-down (x y Γ) M (λ x κ) σ_ans)
  #:context (define-judgment-form)
  #:once-only)

(include/rewrite (lib "redex/examples/let-poly.rkt") let-poly bug1)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example '(hd ((λ x x) 1)))

(test small-counter-example)
