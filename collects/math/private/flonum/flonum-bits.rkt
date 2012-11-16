#lang typed/racket/base

(require racket/flonum
         racket/performance-hint)

(provide flonum->bit-field bit-field->flonum
         flonum->ordinal ordinal->flonum
         flstep flnext flprev flonums-between
         flulp)

(: flonum->bit-field (Flonum -> Natural))
(define (flonum->bit-field x)
  (assert (integer-bytes->integer (real->floating-point-bytes x (ann 8 8)) #f)
          exact-nonnegative-integer?))

(: bit-field->flonum (Integer -> Flonum))
(define (bit-field->flonum i)
  (cond [(and (i . >= . 0) (i . <= . #xffffffffffffffff))
         (floating-point-bytes->real (integer->integer-bytes i 8 #f))]
        [else
         (raise-argument-error 'bit-field->flonum "Integer in [0 .. #xffffffffffffffff]" i)]))

(: flonum->ordinal (Flonum -> Integer))
(define (flonum->ordinal x)
  (cond [(x . fl< . 0.0)  (- (flonum->bit-field (fl- 0.0 x)))]
        [else             (flonum->bit-field (flabs x))])) ; abs for -0.0

(: ordinal->flonum (Integer -> Flonum))
(define (ordinal->flonum i)
  (cond [(and (i . >= . #x-7fffffffffffffff) (i . <= . #x7fffffffffffffff))
         (cond [(i . < . 0)  (fl- 0.0 (bit-field->flonum (- i)))]
               [else         (bit-field->flonum i)])]
        [else
         (raise-argument-error
          'ordinal->flonum "Integer in [#x-7fffffffffffffff .. #x7fffffffffffffff]" i)]))

(define +inf-ordinal (flonum->ordinal +inf.0))
(define -inf-ordinal (flonum->ordinal -inf.0))

(: flstep (Flonum Integer -> Flonum))
(define (flstep x n)
  (cond [(not (and (x . fl>= . -inf.0) (x . fl<= . +inf.0)))  +nan.0]
        [(and (fl= x +inf.0) (n . >= . 0))  +inf.0]
        [(and (fl= x -inf.0) (n . <= . 0))  -inf.0]
        [else  (define i (+ n (flonum->ordinal x)))
               (cond [(i . < . -inf-ordinal)  -inf.0]
                     [(i . > . +inf-ordinal)  +inf.0]
                     [else  (ordinal->flonum i)])]))

(begin-encourage-inline
  
  (: flnext (Flonum -> Flonum))
  (define (flnext x) (flstep x 1))
  
  (: flprev (Flonum -> Flonum))
  (define (flprev x) (flstep x -1))
  
  (: flonums-between (Flonum Flonum -> Integer))
  (define (flonums-between x y)
    (- (flonum->ordinal y) (flonum->ordinal x)))
  
  )  ; begin-encourage-inline
  
(: flulp (Flonum -> (U Flonum-Nan Nonnegative-Flonum)))
(define (flulp x)
  (let ([x  (flabs x)])
    (cond [(fl= x +inf.0)  +nan.0]
          [(eqv? x +nan.0)  +nan.0]
          [(fl= x 0.0)  0.0]
          [else
           (define ulp (flabs (fl- (flnext x) x)))
           (cond [(fl= ulp +inf.0)  (flabs (fl- x (flprev x)))]
                 [else  ulp])])))
