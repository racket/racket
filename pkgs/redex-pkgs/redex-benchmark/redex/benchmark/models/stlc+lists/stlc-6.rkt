#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "hd reduction acts on partially applied cons")

(define-rewrite bug6
  (in-hole E (hd ((cons v_1) v_2)))
  ==>
  (in-hole E (hd (cons v_1)))
  #:context (reduction-relation)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists.rkt") stlc bug6)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term (hd ((cons 1) nil))))

(test small-counter-example)
