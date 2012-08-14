#lang typed/racket/base

(require racket/performance-hint
         (only-in racket/math conjugate sqr)
         (for-syntax racket/base)
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-constructors.rkt"
         "array-broadcast.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide
 ;; Mapping
 inline-array-map
 array-map
 ;; Lifted operators
 array-scale
 array-abs
 array-round
 array-floor
 array-ceiling
 array-truncate
 array-sqr
 array-sqrt
 array-conjugate
 array-magnitude
 array-angle
 array-log
 array-exp
 array-sin
 array-cos
 array-tan
 array-asin
 array-acos
 array-atan
 array+
 array-
 array*
 array/
 array-expt
 array-min
 array-max     
 array=
 array<
 array<=
 array>
 array>=
 array-not
 array-and
 array-or
 array-if
 ;; Number conversions
 array-inexact->exact
 array-exact->inexact
 array-real->double-flonum
 array-real->single-flonum
 array-number->float-complex
 array-real-part
 array-imag-part
 array-make-rectangular)

;; ===================================================================================================
;; Mapping

(define-syntax (inline-array-map stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (make-array #() (f)))]
    [(_ f arr-expr)
     (syntax/loc stx
       (let ([arr arr-expr])
         (define ds (array-shape arr))
         (define proc (unsafe-array-proc arr))
         (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js))))))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let ([arr  arr-expr]
               [arrs arr-exprs] ...)
           (define ds (array-shape-broadcast (list (array-shape arr) (array-shape arrs) ...)))
           (let ([arr   (array-broadcast arr ds)]
                 [arrs  (array-broadcast arrs ds)] ...)
             (define proc  (unsafe-array-proc arr))
             (define procs (unsafe-array-proc arrs)) ...
             (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js) (procs js) ...)))))))]))

(: array-map (All (R A T ...)
                  (case-> ((-> R) -> (Array R))
                          ((A -> R) (Array A) -> (Array R))
                          ((A T ... T -> R) (Array A) (Array T) ... T -> (Array R)))))
(define array-map
  (case-lambda:
    [([f : (-> R)])
     (inline-array-map f)]
    [([f : (A -> R)] [arr : (Array A)])
     (inline-array-map f arr)]
    [([f : (A T ... T -> R)] [arr : (Array A)] . [arrs : (Array T) ... T])
     (define ds (array-shape-broadcast (list* (array-shape arr) (map array-shape arrs))))
     (let ([arr   (array-broadcast arr ds)]
           [arrs  (map (λ: ([arr : (Array T)]) (array-broadcast arr ds)) arrs)])
       (define g (unsafe-array-proc arr))
       (define gs (map unsafe-array-proc arrs))
       (unsafe-build-array
        ds (λ: ([js : Indexes]) (apply f (g js) (map (λ: ([g : (Indexes -> T)]) (g js)) gs)))))]))

;; ===================================================================================================
;; Pointwise operations

#|
The lift operators could be just functions, but then it wouldn't be possible to give the results more
precise types. For example, (array-lift exp) can only have the type

    ((Array Number) -> (Array Number))

or the type

    ((Array Real) -> (Array Real))

but (inline-array-lift1 exp) can have the type

    (case-> ((Array Real)   -> (Array Real))
            ((Array Number) -> (Array Number)))

IOW, the macro lift operators allow us to have array-exp do the job of both array-real-exp and
array-number-exp.
|#

(define-syntax (inline-array-lift1 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr) (inline-array-map f arr)))]))

(define-syntax (inline-array-lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2) (inline-array-map f arr1 arr2)))]))

(define-syntax (inline-array-lift3 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2 arr3) (inline-array-map f arr1 arr2 arr3)))]))

(: array-abs      (case-> ((Array Float) -> (Array Float))
                          ((Array Real)  -> (Array Real))))
(: array-round    (case-> ((Array Float) -> (Array Float))
                          ((Array Real)  -> (Array Real))))
(: array-floor    (case-> ((Array Float) -> (Array Float))
                          ((Array Real)  -> (Array Real))))
(: array-ceiling  (case-> ((Array Float) -> (Array Float))
                          ((Array Real)  -> (Array Real))))
(: array-truncate (case-> ((Array Float) -> (Array Float))
                          ((Array Real)  -> (Array Real))))

(: array-sqrt      ((Array Number) -> (Array Number)))
(: array-conjugate ((Array Number) -> (Array Number)))
(: array-magnitude ((Array Number) -> (Array Real)))
(: array-angle     ((Array Number) -> (Array Real)))
(: array-log       ((Array Number) -> (Array Number)))

(: array-sqr  (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-exp  (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-sin  (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-cos  (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-tan  (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-asin (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-acos (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))
(: array-atan (case-> ((Array Float)  -> (Array Float))
                      ((Array Real)   -> (Array Real))
                      ((Array Number) -> (Array Number))))

(: array+ (case-> ((Array Float)  (Array Float)  -> (Array Float))
                  ((Array Real)   (Array Real)   -> (Array Real))
                  ((Array Number) (Array Number) -> (Array Number))))
(: array* (case-> ((Array Float)  (Array Float)  -> (Array Float))
                  ((Array Real)   (Array Real)   -> (Array Real))
                  ((Array Number) (Array Number) -> (Array Number))))

(: array- (case-> ((Array Float)  -> (Array Float))
                  ((Array Real)   -> (Array Real))
                  ((Array Number) -> (Array Number))
                  ((Array Float)  (Array Float)  -> (Array Float))
                  ((Array Real)   (Array Real)   -> (Array Real))
                  ((Array Number) (Array Number) -> (Array Number))))

(: array/ (case-> ((Array Float)  -> (Array Float))
                  ((Array Real)   -> (Array Real))
                  ((Array Number) -> (Array Number))
                  ((Array Float)  (Array Float)  -> (Array Float))
                  ((Array Real)   (Array Real)   -> (Array Real))
                  ((Array Number) (Array Number) -> (Array Number))))

(: array-scale (case-> ((Array Float)  Float   -> (Array Float))
                       ((Array Real)   Real    -> (Array Real))
                       ((Array Number) Number  -> (Array Number))))

(: array-expt ((Array Number) (Array Number) -> (Array Number)))

(: array-min  (case-> ((Array Float) (Array Float) -> (Array Float))
                      ((Array Real)  (Array Real)  -> (Array Real))))
(: array-max  (case-> ((Array Float) (Array Float) -> (Array Float))
                      ((Array Real)  (Array Real)  -> (Array Real))))

(: array=  ((Array Real) (Array Real) -> (Array Boolean)))
(: array<  ((Array Real) (Array Real) -> (Array Boolean)))
(: array<= ((Array Real) (Array Real) -> (Array Boolean)))
(: array>  ((Array Real) (Array Real) -> (Array Boolean)))
(: array>= ((Array Real) (Array Real) -> (Array Boolean)))

(: array-not ((Array Any) -> (Array Boolean)))
(: array-and (All (A B) ((Array A) (Array B) -> (Array (U B #f)))))
(: array-or  (All (A B) ((Array A) (Array B) -> (Array (U A B)))))
(: array-if  (All (A) ((Array Any) (Array A) (Array A) -> (Array A))))

(: array-inexact->exact (case-> ((Array Real)   -> (Array Exact-Rational))
                                ((Array Number) -> (Array Exact-Number))))

(: array-exact->inexact (case-> ((Array Real)   -> (Array Inexact-Real))
                                ((Array Number) -> (Array Number))))

(: array-real->double-flonum ((Array Real) -> (Array Float)))
(: array-real->single-flonum ((Array Real) -> (Array Single-Flonum)))

(: array-number->float-complex ((Array Number) -> (Array Float-Complex)))

(: array-real-part ((Array Number) -> (Array Real)))
(: array-imag-part ((Array Number) -> (Array Real)))
(: array-make-rectangular ((Array Real) (Array Real) -> (Array Number)))

(define array-abs       (inline-array-lift1 abs))
(define array-round     (inline-array-lift1 round))
(define array-floor     (inline-array-lift1 floor))
(define array-ceiling   (inline-array-lift1 ceiling))
(define array-truncate  (inline-array-lift1 truncate))

(define array-sqr       (inline-array-lift1 sqr))
(define array-sqrt      (inline-array-lift1 sqrt))
(define array-conjugate (inline-array-lift1 conjugate))
(define array-magnitude (inline-array-lift1 magnitude))
(define array-angle     (inline-array-lift1 angle))
(define array-log       (inline-array-lift1 log))
(define array-exp       (inline-array-lift1 exp))
(define array-sin       (inline-array-lift1 sin))
(define array-cos       (inline-array-lift1 cos))
(define array-tan       (inline-array-lift1 tan))
(define array-asin      (inline-array-lift1 asin))
(define array-acos      (inline-array-lift1 acos))
(define array-atan      (inline-array-lift1 atan))

(define array+ (inline-array-lift2 +))
(define array* (inline-array-lift2 *))

(define array-
  (case-lambda
    [(arr)        (inline-array-map - arr)]
    [(arr1 arr2)  (inline-array-map - arr1 arr2)]))

(define array/
  (case-lambda
    [(arr)        (inline-array-map / arr)]
    [(arr1 arr2)  (inline-array-map / arr1 arr2)]))

(define (array-scale arr s) ((inline-array-lift1 (λ (x) (* s x))) arr))

(define array-expt (inline-array-lift2 expt))
(define array-min  (inline-array-lift2 min))
(define array-max  (inline-array-lift2 max))

(define array=  (inline-array-lift2 =))
(define array<  (inline-array-lift2 <))
(define array<= (inline-array-lift2 <=))
(define array>  (inline-array-lift2 >))
(define array>= (inline-array-lift2 >=))

(define array-not (inline-array-lift1 not))
(define array-or  (inline-array-lift2 or))
(define array-and (inline-array-lift2 and))
(define array-if  (inline-array-lift3 if))

(define array-inexact->exact (inline-array-lift1 inexact->exact))
(define array-exact->inexact (inline-array-lift1 exact->inexact))
(define array-real->double-flonum (inline-array-lift1 real->double-flonum))
(define array-real->single-flonum (inline-array-lift1 real->single-flonum))
(define array-number->float-complex (inline-array-lift1 (λ: ([x : Number]) (+ x 0.0+0.0i))))
(define array-real-part (inline-array-lift1 real-part))
(define array-imag-part (inline-array-lift1 imag-part))
(define array-make-rectangular (inline-array-lift2 make-rectangular))
