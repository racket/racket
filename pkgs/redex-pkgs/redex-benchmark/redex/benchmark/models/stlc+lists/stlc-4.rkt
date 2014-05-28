#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the type of cons is incorrect")

(define-rewrite bug4
  (int → ((list int) → (list int)))
  ==>
  (int → ((list int) → int))
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists.rkt") stlc bug4)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((+ 1) ((cons 1) nil))))

(test small-counter-example)
