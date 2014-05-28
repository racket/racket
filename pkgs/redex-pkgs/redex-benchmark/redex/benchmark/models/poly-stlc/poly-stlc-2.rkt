#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the (([cons @ τ] v) v) value has been omitted")

(define-rewrite bug2
  ((([cons @ τ] v) v) . rest)
  ==>
  rest
  #:context (define-language)
  #:variables (rest))

(include/rewrite (lib "redex/examples/poly-stlc.rkt") poly-stlc bug2)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term (([cons @ int] 1) [nil @ int])))

(test small-counter-example)
