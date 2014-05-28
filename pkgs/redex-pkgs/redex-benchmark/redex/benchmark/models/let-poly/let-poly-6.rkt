#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "∨ has an incorrect duplicated variable, leading to an uncovered case")

(define-rewrite bug6
  [(∨ boolean_1 boolean_2) #t]
  ==> 
  [(∨ boolean boolean) #t]
  #:context (define-metafunction)
  #:exactly-once)

(include/rewrite (lib "redex/examples/let-poly.rkt") let-poly bug6)

(include/rewrite "generators.rkt" generators bug-mod-rw exn-rw)

(define small-counter-example (term (λ x (x x))))

(test small-counter-example)
