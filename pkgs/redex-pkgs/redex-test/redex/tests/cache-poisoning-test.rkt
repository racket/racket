#lang racket/base
(require rackunit
         redex)

(define-language L
  (e integer)
  (x variable-not-otherwise-mentioned))

(define-metafunction L
  [(dec e) ,(sub1 (term e))])

(define-metafunction L
  inc : e -> e
  [(inc e) ,(add1 (term e))])

(define-metafunction L
  foo : any_0 -> x
  #:pre ,(> (term any_0) 0)
  [(foo any) ,(gensym)])

(define-metafunction L
  bar : any_1 -> any
  #:cache-poison
  [(bar e) ,(gensym)])

(define-metafunction L
  [(baz e) (foo e)
           (side-condition (> (term e) 0))]
  [(baz any) (bar any)])


;; These should be equal (foo caches)
(define foo1 (term (foo 3)))
(define foo2 (term (foo 3)))
(check-equal? foo1 foo2)

;; These should not be equal (bar does not cache)
(define bar1 (term (bar -3)))
(define bar2 (term (bar -3)))
(check-not-equal? bar1 bar2)

;; These should be equal (baz will only call foo)
(define baz1 (term (baz 4)))
(define baz2 (term (baz 4)))
(check-equal? baz1 baz2)

;; These should not be equal (baz will call bar, which does not cache)
(define baz3 (term (baz -4)))
(define baz4 (term (baz -4)))
(check-not-equal? baz3 baz4)
