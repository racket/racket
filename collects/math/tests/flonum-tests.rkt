#lang racket

(require math/flonum
         rackunit)

(for* ([x  '(+inf.0 +nan.0 -inf.0)]
       [y  '(+inf.0 +nan.0 -inf.0)])
  (cond [(eqv? x y)
         (check-eqv? (flulp-error x y) 0.0 (format "(flulp-error ~v ~v)" x y))]
        [else
         (check-eqv? (flulp-error x y) +inf.0 (format "(flulp-error ~v ~v)" x y))]))

(check-equal? (flulp-error 55123.135123 55123.135123)
              0.0)

(check-equal? (flulp-error 1.0 (flnext 1.0))
              1.0)

(check-equal? (flulp-error +max.0 (flprev +max.0))
              1.0)
