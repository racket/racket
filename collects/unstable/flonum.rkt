#lang racket/base

(require racket/unsafe/ops)

(provide flatan2 flmodulo flexpt
         flonum->bit-field bit-field->flonum
         flonum->ordinal ordinal->flonum
         flstep flnext flprev
         -max.0 -min.0 +min.0 +max.0)

(define (flatan2 y x)
  (cond [(not (flonum? y))  (raise-type-error 'flatan2 "flonum" 0 y x)]
        [(not (flonum? x))  (raise-type-error 'flatan2 "flonum" 1 y x)]
        [else  (exact->inexact (atan y x))]))

(define (flmodulo x y)
  (cond [(not (flonum? x))  (raise-type-error 'flmodulo "flonum" 0 x y)]
        [(not (flonum? y))  (raise-type-error 'flmodulo "flonum" 1 x y)]
        [else  (unsafe-fl- x (unsafe-fl* y (unsafe-flfloor (unsafe-fl/ x y))))]))

(define (flexpt b x)
  (cond [(not (flonum? b))  (raise-type-error 'flexpt "flonum" 0 b x)]
        [(not (flonum? x))  (raise-type-error 'flexpt "flonum" 1 b x)]
        [else  (unsafe-flexp (unsafe-fl* x (unsafe-fllog b)))]))

(define (flonum->bit-field x)
  (cond [(flonum? x)  (integer-bytes->integer (real->floating-point-bytes x 8) #f)]
        [else  (raise-type-error 'flonum->bit-field "flonum" x)]))

(define (bit-field->flonum i)
  (cond [(and (exact-integer? i) (i . >= . 0) (i . <= . #xffffffffffffffff))
         (floating-point-bytes->real (integer->integer-bytes i 8 #f))]
        [else
         (raise-type-error 'bit-field->flonum "exact integer in [0,#xffffffffffffffff]" i)]))

(define (flonum->ordinal x)
  (cond [(flonum? x)  (cond [(x . < . 0)  (- (flonum->bit-field (- x)))]
                            [else            (flonum->bit-field (unsafe-flabs x))])] ; abs for -0.0
        [else  (raise-type-error 'flonum->ordinal "flonum" x)]))

(define (ordinal->flonum i)
  (cond [(and (exact-integer? i) (i . >= . #x-7fffffffffffffff) (i . <= . #x7fffffffffffffff))
         (cond [(i . < . 0)  (- (bit-field->flonum (- i)))]
               [else            (bit-field->flonum i)])]
        [else
         (raise-type-error
          'ordinal->flonum "exact integer in [#x-7fffffffffffffff,#xffffffffffffffff]" i)]))

(define +inf-ordinal (flonum->ordinal +inf.0))
(define -inf-ordinal (flonum->ordinal -inf.0))

(define (flstep x n)
  (cond [(not (flonum? x))  (raise-type-error 'flstep "flonum" 0 x n)]
        [(not (exact-integer? n))  (raise-type-error 'flstep "exact integer" 1 x n)]
        [(eqv? x +nan.0)  +nan.0]
        [(and (eqv? x +inf.0) (n . >= . 0))  +inf.0]
        [(and (eqv? x -inf.0) (n . <= . 0))  -inf.0]
        [else  (define i (+ n (flonum->ordinal x)))
               (cond [(i . < . -inf-ordinal)  -inf.0]
                     [(i . > . +inf-ordinal)  +inf.0]
                     [else  (ordinal->flonum i)])]))

(define (flnext x) (flstep x 1))
(define (flprev x) (flstep x -1))

(define -max.0 (flnext -inf.0))
(define -min.0 (flprev 0.0))
(define +min.0 (flnext 0.0))
(define +max.0 (flprev +inf.0))
