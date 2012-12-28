#lang typed/racket/base

(require racket/flonum
         racket/performance-hint)

(provide flonum->bit-field bit-field->flonum
         flonum->fields fields->flonum
         flonum->sig+exp sig+exp->flonum
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

(define implicit-leading-one (arithmetic-shift 1 52))
(define max-significand (- implicit-leading-one 1))
(define max-exponent 2047)
(define max-signed-exponent 1023)
(define min-signed-exponent -1074)

(: flonum->fields (Flonum -> (Values (U 0 1) Index Natural)))
(define (flonum->fields x)
  (define n (flonum->bit-field x))
  (values (if (zero? (bitwise-bit-field n 63 64)) 0 1)
          (assert (bitwise-bit-field n 52 63) index?)
          (bitwise-bit-field n 0 52)))

(: fields->flonum (Integer Integer Integer -> Flonum))
(define (fields->flonum s e m)
  (cond [(not (or (= s 0) (= s 1)))
         (raise-argument-error 'fields->flonum "(U 0 1)" 0 s e m)]
        [(or (e . < . 0) (e . > . max-exponent))
         (raise-argument-error 'fields->flonum (format "Natural <= ~e" max-exponent) 1 s e m)]
        [(or (m . < . 0) (m . > . max-significand))
         (raise-argument-error 'fields->flonum (format "Natural <= ~a" max-significand) 2 s e m)]
        [else
         (bit-field->flonum (bitwise-ior (arithmetic-shift s 63)
                                         (arithmetic-shift e 52)
                                         m))]))

(: flonum->sig+exp (Flonum -> (Values Integer Fixnum)))
(define (flonum->sig+exp x)
  (define-values (s e m) (flonum->fields x))
  (let-values ([(sig exp)  (if (= e 0)
                               (values m -1074)
                               (values (bitwise-ior m implicit-leading-one)
                                       (assert (- e 1075) fixnum?)))])
    (values (if (zero? s) sig (- sig)) exp)))

(: sig+exp->flonum (Integer Integer -> Flonum))
(define (sig+exp->flonum sig exp)
  (cond [(= sig 0)  0.0]
        [(exp . > . max-signed-exponent)  (if (sig . < . 0) -inf.0 +inf.0)]
        [(exp . < . min-signed-exponent)  (if (sig . < . 0) -0.0 0.0)]
        [else  (real->double-flonum (* sig (expt 2 exp)))]))

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
