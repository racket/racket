#lang typed/racket/base

(require (only-in racket/math conjugate)
         (for-syntax racket/base)
         "array-struct.rkt"
         "array-broadcast.rkt"
         "utils.rkt"
         (only-in "untyped-array-pointwise.rkt" inline-array-map))

(provide (all-defined-out))

(: array-map (All (R A B T ...)
                    (case-> ((-> R) -> (Array R))
                            ((A -> R) (Array A) -> (Array R))
                            ((A B T ... T -> R) (Array A) (Array B) (Array T) ... T -> (Array R)))))
(define array-map
  (case-lambda:
    [([f : (-> R)])
     (inline-array-map f)]
    [([f : (A -> R)] [arr : (Array A)])
     (inline-array-map f arr)]
    [([f : (A B -> R)] [arr0 : (Array A)] [arr1 : (Array B)])
     (inline-array-map f arr0 arr1)]
    [([f : (A B T ... T -> R)] [arr0 : (Array A)] [arr1 : (Array B)] . [arrs : (Array T) ... T])
     (define ds (array-shape-broadcast (list* (array-shape arr0)
                                              (array-shape arr1)
                                              (map array-shape arrs))))
     (let ([arr0  (array-broadcast arr0 ds)]
           [arr1  (array-broadcast arr1 ds)]
           [arrs  (map (λ: ([arr : (Array T)]) (array-broadcast arr ds)) arrs)])
       (define g0 (unsafe-array-proc arr0))
       (define g1 (unsafe-array-proc arr1))
       (define gs (map unsafe-array-proc arrs))
       (unsafe-build-array
        ds (λ: ([js : Indexes]) (apply f (g0 js) (g1 js)
                                       (map (λ: ([g : (Indexes -> T)]) (g js)) gs)))))]))

;; ===================================================================================================
;; Pointwise operation types

(define-syntax (declare-case-type stx)
  (syntax-case stx (->)
    [(_ name [(A ... -> B) ...])
     (syntax/loc stx
       (: name (case-> ((Array A) ... -> (Array B)) ...)))]))

(define-syntax-rule (declare-case-types (name ...) Ts)
  (begin (declare-case-type name Ts) ...))

(declare-case-types
 (array-abs)
 [(Integer -> Integer)
  (Exact-Rational -> Exact-Rational)
  (Float -> Float)
  (Real -> Real)])

(declare-case-types
 (array-round array-floor array-ceiling array-truncate)
 [(Integer -> Integer)
  (Exact-Rational -> Integer)
  (Float -> Float)
  (Real -> Real)])

(declare-case-types
 (array-sqrt array-log)
 [(Number -> Number)])

(declare-case-types
 (array-conjugate array-sqr)
 [(Integer -> Integer)
  (Exact-Rational -> Exact-Rational)
  (Float -> Float)
  (Real -> Real)
  (Float-Complex -> Float-Complex)
  (Number -> Number)])

(declare-case-types
 (array-magnitude)
 [;(Integer -> Integer)  ; should be allowed
  (Exact-Rational -> Exact-Rational)
  (Float -> Real)  ; should be Float -> Float
  (Real -> Real)
  (Float-Complex -> Float)
  (Number -> Real)])

(declare-case-types
 (array-angle)
 [(Real -> Real)
  (Float-Complex -> Float)
  (Number -> Real)])

(declare-case-types
 (array-exp array-sin array-cos array-tan array-asin array-acos array-atan)
 [(Float -> Float)
  (Real -> Real)
  (Float-Complex -> Float-Complex)
  (Number -> Number)])

(declare-case-types
 (array+)
 [(Integer Integer -> Integer)
  (Exact-Rational Exact-Rational -> Exact-Rational)
  (Float Float -> Float)
  (Real Float -> Float)
  (Float Real -> Float)
  (Real Real -> Real)
  (Float-Complex Float-Complex -> Float-Complex)
  (Float-Complex Number -> Float-Complex)
  (Number Float-Complex -> Float-Complex)
  (Number Number -> Number)])

(declare-case-types
 (array*)
 [(Integer Integer -> Integer)
  (Exact-Rational Exact-Rational -> Exact-Rational)
  (Float Float -> Float)
  (Real Real -> Real)
  (Float-Complex Float-Complex -> Float-Complex)
  (Number Number -> Number)])

(declare-case-types
 (array-)
 [(Integer -> Integer)
  (Exact-Rational -> Exact-Rational)
  (Float -> Float)
  (Real -> Real)
  ;(Float-Complex -> Float-Complex)  ; should be allowed
  (Number -> Number)
  (Integer Integer -> Integer)
  (Exact-Rational Exact-Rational -> Exact-Rational)
  (Float Float -> Float)
  (Real Float -> Float)
  (Float Real -> Float)
  (Real Real -> Real)
  (Float-Complex Float-Complex -> Float-Complex)
  (Float-Complex Number -> Float-Complex)
  (Number Float-Complex -> Float-Complex)
  (Number Number -> Number)])

(declare-case-types
 (array/)
 [(Exact-Rational -> Exact-Rational)
  (Float -> Float)
  (Real -> Real)
  ;(Float-Complex -> Float-Complex)  ; should be allowed
  (Number -> Number)
  (Exact-Rational Exact-Rational -> Exact-Rational)
  (Float Float -> Float)
  (Float Real -> Float)
  (Real Real -> Real)
  (Float-Complex Float-Complex -> Float-Complex)
  ;(Float-Complex Number -> Float-Complex)  ; should be allowed
  (Number Number -> Number)])

(: array-scale
   (case->
    ((Array Integer) Integer -> (Array Integer))
    ((Array Exact-Rational) Exact-Rational -> (Array Exact-Rational))
    ((Array Float) Float -> (Array Float))
    ((Array Real) Real -> (Array Real))
    ((Array Float-Complex) Float-Complex -> (Array Float-Complex))
    ((Array Number) Number -> (Array Number))))

(declare-case-types
 (array-expt)
 [(Integer Integer -> Exact-Rational)
  (Exact-Rational Integer -> Exact-Rational)
  ;(Float Float -> Float-Complex)  ; should be allowed
  (Real Real -> Number)
  (Float-Complex Float-Complex -> Float-Complex)
  (Number Number -> Number)])

(declare-case-types
 (array-min array-max)
 [(Integer Integer -> Integer)
  (Exact-Rational Exact-Rational -> Exact-Rational)
  (Float Float -> Float)
  (Real Real -> Real)])

(: array= ((Array Number) (Array Number) -> (Array Boolean)))

(declare-case-types
 (array< array<= array> array>=)
 [(Real Real -> Boolean)])

(: array-not ((Array Any) -> (Array Boolean)))
(: array-and (All (A B) ((Array A) (Array B) -> (Array (U B #f)))))
(: array-or  (All (A) ((Array A) (Array A) -> (Array A))))
(: array-if  (All (A) ((Array Any) (Array A) (Array A) -> (Array A))))

(declare-case-types
 (array-inexact->exact)
 [(Real -> Exact-Rational)
  (Number -> Exact-Number)])

(declare-case-types
 (array-exact->inexact)
 [(Integer -> Float)
  (Exact-Rational -> Float)
  (Float -> Float)
  (Real -> Inexact-Real)
  (Float-Complex -> Float-Complex)
  ;(Exact-Number -> Float-Complex)  ; should be allowed
  (Number -> Number)  ; should be Number -> Inexact-Number
  ])

(: array-real->double-flonum ((Array Real) -> (Array Float)))
(: array-number->float-complex ((Array Number) -> (Array Float-Complex)))

(declare-case-types
 (array-real-part)
 [;(Integer -> Integer)  ; should be allowed
  (Exact-Rational -> Exact-Rational)
  ;(Float -> Float)  ; should be allowed
  (Real -> Real)
  (Float-Complex -> Float)
  (Number -> Real)])

(declare-case-types
 (array-imag-part)
 [;(Real -> Zero)  ; should be allowed
  (Real -> Real)
  (Float-Complex -> Float)
  (Number -> Real)])

(declare-case-types
 (array-make-rectangular)
 [(Exact-Rational Exact-Rational -> Exact-Number)
  (Float Float -> Float-Complex)
  (Float Real -> Float-Complex)
  (Real Float -> Float-Complex)
  (Real Real -> Number)])

;; ===================================================================================================
;; Pointwise operations

#|
The lift operators could be just functions, but then it wouldn't be possible to give the results more
precise types. For example, if `array-lift1' were a higher-order function, (array-lift1 exp) could
only have the type

    ((Array Number) -> (Array Number))

or the type

    ((Array Real) -> (Array Real))

Since `array-lift1' is a macro, (array-lift1 exp) can have the type

    (case-> ((Array Real)   -> (Array Real))
            ((Array Number) -> (Array Number)))

IOW, the macro lift operators allow us to have array-exp do the job of both array-real-exp and
array-number-exp.
|#

(define-syntax (array-lift1 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr) (inline-array-map f arr)))]))

(define-syntax (array-lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2) (inline-array-map f arr1 arr2)))]))

(define-syntax (array-lift3 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2 arr3) (inline-array-map f arr1 arr2 arr3)))]))

(define array-abs       (array-lift1 abs))
(define array-round     (array-lift1 round))
(define array-floor     (array-lift1 floor))
(define array-ceiling   (array-lift1 ceiling))
(define array-truncate  (array-lift1 truncate))

(define array-sqr       (array-lift1 (λ (z) (* z z))))
(define array-sqrt      (array-lift1 sqrt))
(define array-conjugate (array-lift1 conjugate))
(define array-magnitude (array-lift1 magnitude))
(define array-angle     (array-lift1 angle))
(define array-log       (array-lift1 log))
(define array-exp       (array-lift1 exp))
(define array-sin       (array-lift1 sin))
(define array-cos       (array-lift1 cos))
(define array-tan       (array-lift1 tan))
(define array-asin      (array-lift1 asin))
(define array-acos      (array-lift1 acos))
(define array-atan      (array-lift1 atan))

(define array+ (array-lift2 +))
(define array* (array-lift2 *))

(define array-
  (case-lambda
    [(arr)        (inline-array-map - arr)]
    [(arr1 arr2)  (inline-array-map - arr1 arr2)]))

(define array/
  (case-lambda
    [(arr)        (inline-array-map / arr)]
    [(arr1 arr2)  (inline-array-map / arr1 arr2)]))

(define (array-scale arr s) ((array-lift1 (λ (x) (* s x))) arr))

(define array-expt (array-lift2 expt))
(define array-min  (array-lift2 min))
(define array-max  (array-lift2 max))

(define array=  (array-lift2 =))
(define array<  (array-lift2 <))
(define array<= (array-lift2 <=))
(define array>  (array-lift2 >))
(define array>= (array-lift2 >=))

(define array-not (array-lift1 not))
(define array-and (array-lift2 and))
(define array-if  (array-lift3 if))
(define array-or  (array-lift2 or))

(define array-inexact->exact (array-lift1 inexact->exact))
(define array-exact->inexact (array-lift1 exact->inexact))
(define array-real->double-flonum (array-lift1 real->double-flonum))
(define array-number->float-complex (array-lift1 (λ: ([x : Number]) (+ x 0.0+0.0i))))
(define array-real-part (array-lift1 real-part))
(define array-imag-part (array-lift1 imag-part))
(define array-make-rectangular (array-lift2 make-rectangular))
