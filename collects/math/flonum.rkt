#lang typed/racket/base

#|
TODO

single-flonum versions of everything here
|#

(provide flonum->bit-field bit-field->flonum
         flonum->ordinal ordinal->flonum
         flstep flnext flprev flonums-between
         -max.0 -min.0 +min.0 +max.0 +epsilon.0
         relative-error)

;; ===================================================================================================
;; Floating-point representation

(: flonum->bit-field (Float -> Nonnegative-Integer))
(define (flonum->bit-field x)
  (define y
    (integer-bytes->integer (real->floating-point-bytes x (ann 8 8)) #f))
  (with-asserts ([y exact-nonnegative-integer?]) y))

(: bit-field->flonum (Integer -> Float))
(define (bit-field->flonum i)
  (cond [(and (i . >= . 0) (i . <= . #xffffffffffffffff))
         (floating-point-bytes->real (integer->integer-bytes i 8 #f))]
        [else
         (raise-type-error 'bit-field->flonum "Integer in [0,#xffffffffffffffff]" i)]))

(: flonum->ordinal (Float -> Integer))
(define (flonum->ordinal x)
  (cond [(x . < . 0)  (- (flonum->bit-field (- x)))]
        [else            (flonum->bit-field (abs x))])) ; abs for -0.0

(: ordinal->flonum (Integer -> Float))
(define (ordinal->flonum i)
  (cond [(and (i . >= . #x-7fffffffffffffff) (i . <= . #x7fffffffffffffff))
         (cond [(i . < . 0)  (- (bit-field->flonum (- i)))]
               [else            (bit-field->flonum i)])]
        [else
         (raise-type-error
          'ordinal->flonum "Integer in [#x-7fffffffffffffff,#xffffffffffffffff]" i)]))

(define +inf-ordinal (flonum->ordinal +inf.0))
(define -inf-ordinal (flonum->ordinal -inf.0))

(: flstep (Float Integer -> Float))
(define (flstep x n)
  (cond [(eqv? x +nan.0)  +nan.0]
        [(and (eqv? x +inf.0) (n . >= . 0))  +inf.0]
        [(and (eqv? x -inf.0) (n . <= . 0))  -inf.0]
        [else  (define i (+ n (flonum->ordinal x)))
               (cond [(i . < . -inf-ordinal)  -inf.0]
                     [(i . > . +inf-ordinal)  +inf.0]
                     [else  (ordinal->flonum i)])]))

(: flnext (Float -> Float))
(define (flnext x) (flstep x 1))

(: flprev (Float -> Float))
(define (flprev x) (flstep x -1))

(: flonums-between (Float Float -> Integer))
(define (flonums-between x y)
  (- (flonum->ordinal y) (flonum->ordinal x)))

;; ===================================================================================================
;; Constants

(define -max.0 (flnext -inf.0))
(define -min.0 (flprev 0.0))
(define +min.0 (flnext 0.0))
(define +max.0 (flprev +inf.0))

;; The smallest flonum that can be added to 1.0 to get a result != 1.0
(define +epsilon.0 (expt 2.0 -52))

;; ===================================================================================================
;; Error measurement

(: relative-error : Float Float -> Float)
(define (relative-error x correct)
  (/ (abs (- x correct)) correct))
