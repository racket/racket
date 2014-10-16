#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "list/c contracts aren't applied properly in the cons case")

(define-rewrite bug2
  (monitor (list/c ctc) (cons v_1 v_2) k l j)
  ==> 
  (monitor ctc (cons v_1 v_2) k l j)
  #:context (reduction-relation)
  #:once-only)

(include/rewrite (lib "redex/benchmark/models/delim-cont/delim-cont.rkt") delim-cont bug2)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example
  '(<> (monitor (list/c (flat (λ (var:k : Bool) #t))) 
            (cons #t (null Bool)) 
            "" "" "")
       ·))

(test small-counter-example)
