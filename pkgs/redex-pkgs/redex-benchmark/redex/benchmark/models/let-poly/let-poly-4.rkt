#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error
  (string-append "misspelled the name of a metafunction in a side-condition, "
                 "causing the occurs check to not happen"))

(define-rewrite bug4
  [(uh (x τ G) Gx) ⊥ (where #t (in-vars-τ? x τ))]
  ==> 
  [(uh (x τ G) Gx) ⊥ (where #t (in-vars? x τ))]
  #:context (define-metafunction)
  #:once-only)

(include/rewrite (lib "redex/examples/let-poly.rkt") let-poly bug4)

(include/rewrite "generators.rkt" generators bug-mod-rw exn-rw)

(define small-counter-example (term ((λ x (x x)) hd)))

(test small-counter-example)
