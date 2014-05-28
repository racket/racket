#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "hd reduction acts on partially applied cons")

(define-rewrite bug6
  (in-hole E ((hd @ τ) (((cons @ τ) v_1) v_2)))
  ==>
  (in-hole E ((hd @ τ) ((cons @ τ) v_1)))
  #:context (reduction-relation)
  #:once-only)

(include/rewrite (lib "redex/examples/poly-stlc.rkt") poly-stlc bug6)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ([hd @ int] (([cons @ int] 1) [nil @ int]))))

(test small-counter-example)
