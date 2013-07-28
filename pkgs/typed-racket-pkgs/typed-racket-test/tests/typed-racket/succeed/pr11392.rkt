#lang typed/racket
(struct: foo ((n : Number)))
(struct: bar ((n : Number)))


(define-type foobar (U foo bar))
(define-predicate foobar? foobar)

(: baz ((List) -> "two"))

(define (baz x)
  (if (foobar? x) 2 "two"))
