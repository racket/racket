#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error "variable not fresh enough")

(define-rewrite bug4
  (where x_new ,(variable-not-in (term (x y M))
                                 (term y)))
  ==> 
  (where x_new ,(variable-not-in (term M)
                                 (term y)))
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/stlc+lists+subst.rkt") stlc-sub bug4)


(include/rewrite "generators.rkt" generators bug-mod-rw subst-check-rw)

(define small-counter-example 
  (term ((λ (z int) (((λ (y1 int) (λ (y int) y)) z) 1)) 0)))

(test small-counter-example)
