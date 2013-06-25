#lang racket
(module test typed/racket
  (provide v-ref)

  (: v-ref ((Vectorof Symbol) Index -> Symbol))
  (define (v-ref v i) (vector-ref v i)))

(require (submod "." test))

(v-ref (vector 'foo) 0)
