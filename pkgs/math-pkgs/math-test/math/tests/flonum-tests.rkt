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
;; fllog2

;; Make sure it's exact for exact powers of two
(for ([x  (in-range -1074.0 1024.0)])
  (define y (flexp2 x))
  (define x0 (fllog2 y))
  (check-equal? x0 x))

;; ===================================================================================================
;; fl2 tests

(for* ([x2  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)]
       [x1  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
  (define n
    (count (λ (b) b)
           (map (λ (f) (f x2 x1))
                (list fl2rational? fl2infinite? fl2nan?))))
  (unless (= n 1) (printf "x2 = ~v  x1 = ~v~n" x2 x1)))

#|
Tests to add

(for*: ([x2  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)]
        [x1  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
  (define n
    (count (λ: ([b : Boolean]) b)
           (map (λ: ([f : (Flonum Flonum -> Boolean)])
                  (f x2 x1))
                (list fl2rational? fl2infinite? fl2nan?))))
  (unless (= n 1) (printf "x2 = ~v  x1 = ~v~n" x2 x1)))

fl2=
fl2>
fl2<
fl2>=
fl2<=

(fl2step x2 x1 n/2) twice = (fl2step x2 x1 n)
|#

(check-true (let-values ([(y2 y1)  (fl+/error +max.hi +max.lo)])
              (fl2= y2 y1 +max.hi +max.lo)))

(check-true (let*-values ([(y2 y1)  (fl2next +max.hi +max.lo)])
              (fl2infinite? y2 y1)))

;; ===================================================================================================
;; FPU testing

(check-equal? (parameterize ([print-fp-test-progress? #f])
                (test-floating-point 1000))
              empty)
