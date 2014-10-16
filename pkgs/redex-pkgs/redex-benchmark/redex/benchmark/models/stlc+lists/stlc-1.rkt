#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "app rule the range of the function is matched to the argument")

(define-rewrite bug1
  (typeof Γ M_2 σ)
  ==> 
  (typeof Γ M_2 σ_2)
  #:context (define-judgment-form)
  #:variables (A)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists.rkt") stlc bug1)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example 
  (term (hd 0)))

(test small-counter-example)
