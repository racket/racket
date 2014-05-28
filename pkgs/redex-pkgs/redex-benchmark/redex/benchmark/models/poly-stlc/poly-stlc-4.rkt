#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the type of cons is incorrect")

(define-rewrite bug4
  (∀ a (a → ((list a) → (list a))))
  ==>
  (∀ a (a → ((list a) → a)))
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/poly-stlc.rkt") poly-stlc bug4)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((+ 1) (([cons @ int] 1) [nil @ int]))))

(test small-counter-example)
