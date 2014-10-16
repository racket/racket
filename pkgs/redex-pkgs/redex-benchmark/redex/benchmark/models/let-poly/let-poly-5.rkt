#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "eliminate-G was written as if it always gets a Gx as input")

(define-rewrite bug5
  ([(eliminate-G x τ (σ_1 σ_2 G))
   ((eliminate-τ x τ σ_1) (eliminate-τ x τ σ_2) (eliminate-G x τ G))]
   . rest)
  ==> 
  ([(eliminate-G x τ (x σ G))
    (τ (eliminate-τ x τ σ) (eliminate-G x τ G))]
   [(eliminate-G x τ (y σ G))
    (y (eliminate-τ x τ σ) (eliminate-G x τ G))]
   . rest)
  #:context (define-metafunction)
  #:variables (rest)
  #:exactly-once)

(include/rewrite (lib "redex/examples/let-poly.rkt") let-poly bug5)

(include/rewrite "generators.rkt" generators bug-mod-rw exn-rw)

(define small-counter-example (term (cons 1)))

(test small-counter-example)
