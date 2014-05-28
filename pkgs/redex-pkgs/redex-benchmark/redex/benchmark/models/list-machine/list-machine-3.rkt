#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics
         racket/list
         racket/match)

(provide (all-defined-out))

(define the-error "cons doesn't actually update the store")

(define-rewrite bug3
  ((p r (begin (cons v_0 v_1 v_2) ι))
   (p r_2 ι)
   . rest)
  ==> 
  ((p r (begin (cons v_0 v_1 v_2) ι))
   (p r ι)
   . rest)
  #:context (reduction-relation)
  #:variables (rest)
  #:exactly-once)

(include/rewrite (lib "redex/examples/list-machine/list-machine.rkt") list-machine bug3)

(include/rewrite (lib "redex/examples/list-machine/list-machine-typing.rkt") list-machine-typing)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  (term (l0 : (begin (cons v0 v0 v0) (begin (fetch-field v0 1 v0) halt)) end)))

(test small-counter-example)
