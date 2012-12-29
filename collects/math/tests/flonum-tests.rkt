#lang racket

(require math/flonum
         math/base
         math/utils
         rackunit)

;; ===================================================================================================
;; Test `flulp-error' heavily; it MUST be correct, or all the FPU tests are suspect

;; Both arguments eqv?
(check-equal? (flulp-error -inf.0 -inf.0) 0.0)
(check-equal? (flulp-error -max.0 -max.0) 0.0)
(check-equal? (flulp-error -1.0 -1.0) 0.0)
(check-equal? (flulp-error -min.0 -min.0) 0.0)
(check-equal? (flulp-error -0.0 -0.0) 0.0)
(check-equal? (flulp-error +0.0 +0.0) 0.0)
(check-equal? (flulp-error +min.0 +min.0) 0.0)
(check-equal? (flulp-error +1.0 +1.0) 0.0)
(check-equal? (flulp-error +max.0 +max.0) 0.0)
(check-equal? (flulp-error +inf.0 +inf.0) 0.0)

;; Both arguments zero
(check-equal? (flulp-error -0.0 +0.0) 0.0)
(check-equal? (flulp-error +0.0 -0.0) 0.0)

;; LHS argument +inf.0
(check-equal? (flulp-error +inf.0 -inf.0) +inf.0)
(check-equal? (flulp-error +inf.0 -max.0) +inf.0)
(check-equal? (flulp-error +inf.0 -1.0) +inf.0)
(check-equal? (flulp-error +inf.0 -min.0) +inf.0)
(check-equal? (flulp-error +inf.0 -0.0) +inf.0)
(check-equal? (flulp-error +inf.0 +0.0) +inf.0)
(check-equal? (flulp-error +inf.0 +min.0) +inf.0)
(check-equal? (flulp-error +inf.0 +1.0) +inf.0)
(check-equal? (flulp-error +inf.0 +max.0) +inf.0)

;; LHS argument -inf.0
(check-equal? (flulp-error -inf.0 -max.0) +inf.0)
(check-equal? (flulp-error -inf.0 -1.0) +inf.0)
(check-equal? (flulp-error -inf.0 -min.0) +inf.0)
(check-equal? (flulp-error -inf.0 -0.0) +inf.0)
(check-equal? (flulp-error -inf.0 +0.0) +inf.0)
(check-equal? (flulp-error -inf.0 +min.0) +inf.0)
(check-equal? (flulp-error -inf.0 +1.0) +inf.0)
(check-equal? (flulp-error -inf.0 +max.0) +inf.0)
(check-equal? (flulp-error -inf.0 +inf.0) +inf.0)

;; RHS argument +inf.0
(check-equal? (flulp-error -max.0 +inf.0) +inf.0)
(check-equal? (flulp-error -1.0 +inf.0) +inf.0)
(check-equal? (flulp-error -min.0 +inf.0) +inf.0)
(check-equal? (flulp-error -0.0 +inf.0) +inf.0)
(check-equal? (flulp-error +0.0 +inf.0) +inf.0)
(check-equal? (flulp-error +min.0 +inf.0) +inf.0)
(check-equal? (flulp-error +1.0 +inf.0) +inf.0)
(check-equal? (flulp-error +max.0 +inf.0) +inf.0)

;; RHS argument -inf.0
(check-equal? (flulp-error -max.0 -inf.0) +inf.0)
(check-equal? (flulp-error -1.0 -inf.0) +inf.0)
(check-equal? (flulp-error -min.0 -inf.0) +inf.0)
(check-equal? (flulp-error -0.0 -inf.0) +inf.0)
(check-equal? (flulp-error +0.0 -inf.0) +inf.0)
(check-equal? (flulp-error +min.0 -inf.0) +inf.0)
(check-equal? (flulp-error +1.0 -inf.0) +inf.0)
(check-equal? (flulp-error +max.0 -inf.0) +inf.0)

;; RHS argument 0.0
(check-equal? (flulp-error -max.0 +0.0) +inf.0)
(check-equal? (flulp-error -1.0 +0.0) +inf.0)
(check-equal? (flulp-error -min.0 +0.0) +inf.0)
(check-equal? (flulp-error +min.0 +0.0) +inf.0)
(check-equal? (flulp-error +1.0 +0.0) +inf.0)
(check-equal? (flulp-error +max.0 +0.0) +inf.0)

;; RHS argument -0.0
(check-equal? (flulp-error -max.0 -0.0) +inf.0)
(check-equal? (flulp-error -1.0 -0.0) +inf.0)
(check-equal? (flulp-error -min.0 -0.0) +inf.0)
(check-equal? (flulp-error +min.0 -0.0) +inf.0)
(check-equal? (flulp-error +1.0 -0.0) +inf.0)
(check-equal? (flulp-error +max.0 -0.0) +inf.0)

;; Small errors
(check-equal? (flulp-error +0.0 -min.0) 1.0)
(check-equal? (flulp-error +0.0 +min.0) 1.0)
(check-equal? (flulp-error -0.0 -min.0) 1.0)
(check-equal? (flulp-error -0.0 +min.0) 1.0)
(check-equal? (flulp-error -min.0 +min.0) 2.0)
(check-equal? (flulp-error +min.0 -min.0) 2.0)

(define large-flulp-error-xys
  (list (list -1.0 -max.0)
        (list -1.0 +max.0)
        (list -min.0 -max.0)
        (list -min.0 +max.0)
        (list -1.0 +1.0)
        (list -max.0 +max.0)
        (list -max.0 -1.0)
        (list -max.0 -min.0)
        (list -max.0 +min.0)
        (list -max.0 +1.0)
        (list -1.0 -min.0)
        (list -1.0 +min.0)
        (list -min.0 -1.0)
        (list -min.0 +1.0)
        (list -0.0 -max.0)
        (list -0.0 -1.0)
        (list -0.0 +1.0)
        (list -0.0 +max.0)
        (list +0.0 -max.0)
        (list +0.0 -1.0)
        (list +0.0 +1.0)
        (list +0.0 +max.0)
        (list +min.0 -max.0)
        (list +min.0 -1.0)
        (list +min.0 +1.0)
        (list +min.0 +max.0)
        (list +1.0 -max.0)
        (list +1.0 -1.0)
        (list +1.0 -min.0)
        (list +1.0 +min.0)
        (list +1.0 +max.0)
        (list +max.0 -max.0)
        (list +max.0 -1.0)
        (list +max.0 -min.0)
        (list +max.0 +min.0)
        (list +max.0 +1.0)))

;; Large errors
(for ([xy  (in-list large-flulp-error-xys)])
  (match-define (list x y) xy)
  (check-true ((flulp-error x y) . >= . (expt 2 52))
              (format "x = ~a  y = ~a" x y)))

(check-equal? (flulp-error 1.0 (flnext 1.0)) 1.0)
(check-equal? (flulp-error +max.0 (flprev +max.0)) 1.0)

(for ([_  (in-range 1000)])
  (define s (random))
  (define e (fl (random-integer -1074 1024)))
  (define x (* s (flexpt 2.0 e)))
  (check-equal? (flulp-error x (flnext x)) 1.0
                (format "x = ~a" x)))

;; ===================================================================================================
;; FPU testing

(parameterize ([print-test-progress? #f])
  (test-fpu 1000))
