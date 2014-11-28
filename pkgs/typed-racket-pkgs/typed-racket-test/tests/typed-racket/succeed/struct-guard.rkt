#lang typed/racket/base

(require typed/rackunit)

(struct foo ([x : Integer] [y : Integer])
        #:guard (λ (x y name)
                  (values (add1 x) (add1 y))))

(struct bar ([x : Integer] [y : Integer])
        #:guard #f)

(check-equal? (foo-y (foo 1 2)) 3)
(check-equal? (bar-y (bar 1 2)) 2)
