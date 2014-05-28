#lang racket/base

(require redex/benchmark
         "util.rkt"
         redex/reduction-semantics)
(provide (all-defined-out))

(define the-error
  (string-append "used let --> left-left-λ rewrite rule for let, "
                 "but the right-hand side is less polymorphic"))

(define-rewrite bug7-a
  ((v E)
   (let ([x E]) M))
  ==> 
  ((v E))
  #:context (define-language)
  #:exactly-once)

(define-rewrite bug7-b
  (--> (Σ (in-hole E (let ([x v]) M)))
       (Σ (in-hole E (subst M x v)))
       . rest)
  ==> 
  (--> (Σ (in-hole E (let ([x M]) N)))
       (Σ (in-hole E ((λ x N) M)))
       . rest)
  #:context (reduction-relation)
  #:variables (rest)
  #:exactly-once)

(include/rewrite (lib "redex/examples/let-poly.rkt") let-poly bug7-a bug7-b)

(include/rewrite "generators.rkt" generators bug-mod-rw)

(define small-counter-example (term (let ((x (λ y y))) (x x))))

(test small-counter-example)
