#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "the ((cons v) v) value has been omitted")

(define-rewrite bug2
  (((cons v) v) . rest)
  ==>
  rest
  #:context (define-language)
  #:variables (rest))

(include/rewrite (lib "redex/examples/stlc+lists.rkt") stlc bug2)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term ((cons 0) nil)))

(test small-counter-example)
