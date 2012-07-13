#lang typed/racket/base

(require racket/unsafe/ops
         racket/performance-hint
         (for-syntax racket/base)
         "array-struct.rkt"
         "../vector/vector-pointwise.rkt"
         "utils.rkt")

(provide array=
         ;; Lifting
         inline-array-lift
         inline-array-lift2
         array-lift
         array-lift2
         array-map
         array-map2
         ;; Lifted operators
         array-abs
         array-round
         array-floor
         array-ceiling
         array-truncate
         array-sqrt
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
         ;; Number conversions
         array-inexact->exact
         array-exact->inexact
         array-real->double-flonum
         array-real->single-flonum
         array-number->float-complex
         array-real-part
         array-imag-part)

;; ===================================================================================================
;; Numeric equality

(: array= ((Array Number) (Array Number) -> Boolean))
(define (array= arr brr)
  (let ([arr  (array-strict arr)]
        [brr  (array-strict brr)])
    (and (equal? (unsafe-array-shape arr)
                 (unsafe-array-shape brr))
         (vector= (unsafe-array-data arr)
                  (unsafe-array-data brr)))))

;; ===================================================================================================
;; Lifting

#|
The lift operators could be just functions, but then it wouldn't be possible to give the results more
precise types. For example, (array-lift exp) can only have the type

    ((Array Number) -> (lazy-array Number))

or the type

    ((Array Real) -> (lazy-array Real))

but (inline-array-lift exp) can have the type

    (case-> ((Array Real)   -> (lazy-array Real))
            ((Array Number) -> (lazy-array Number)))

IOW, the macro lift operators allow us to have array-exp do the job of both array-real-exp and
array-number-exp.
|#

;(: inline-array-lift (All (A B) ((A -> B) -> ((Array A) -> (lazy-array B)))))
(define-syntax (inline-array-lift stx)
  (syntax-case stx ()
    [(_ f)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
           (define g (unsafe-array-proc arr))
           (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) (f (g js)))))))]))

;(: inline-array-lift2 (All (A B C) ((A B -> C) -> ((Array A) (Array B) -> (lazy-array C)))))
(define-syntax (inline-array-lift2 stx)
  (syntax-case stx ()
    [(_ name f)
     (syntax/loc stx
       (λ (arr1 arr2)
         (let ([arr1  (array-lazy arr1)]
               [arr2  (array-lazy arr2)])
           (define ds (unsafe-array-shape arr1))
           (unsafe-check-equal-array-shape! name ds (unsafe-array-shape arr2))
           (define g1 (unsafe-array-proc arr1))
           (define g2 (unsafe-array-proc arr2))
           (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) (f (g1 js) (g2 js)))))))]))

(begin-encourage-inline
  
  (: array-lift (All (A B) ((A -> B) -> ((Array A) -> (lazy-array B)))))
  (define (array-lift f) (inline-array-lift f))
  
  (: array-lift2 (All (A B C) (Symbol (A B -> C) -> ((Array A) (Array B) -> (lazy-array C)))))
  (define (array-lift2 name f) (inline-array-lift2 name f))
  
  (: array-map (All (A B) ((A -> B) (Array A) -> (lazy-array B))))
  (define (array-map f arr) ((inline-array-lift f) arr))
  
  (: array-map2 (All (A B C) ((A B -> C) (Array A) (Array B) -> (lazy-array C))))
  (define (array-map2 f arr1 arr2) ((inline-array-lift2 'array-map2 f) arr1 arr2))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Lifted operations on Real and Number

(: array-abs      (case-> ((Array Float) -> (lazy-array Float))
                          ((Array Real)  -> (lazy-array Real))))
(: array-round    (case-> ((Array Float) -> (lazy-array Float))
                          ((Array Real)  -> (lazy-array Real))))
(: array-floor    (case-> ((Array Float) -> (lazy-array Float))
                          ((Array Real)  -> (lazy-array Real))))
(: array-ceiling  (case-> ((Array Float) -> (lazy-array Float))
                          ((Array Real)  -> (lazy-array Real))))
(: array-truncate (case-> ((Array Float) -> (lazy-array Float))
                          ((Array Real)  -> (lazy-array Real))))

(: array-sqrt ((Array Number) -> (lazy-array Number)))
(: array-log  ((Array Number) -> (lazy-array Number)))

(: array-exp  (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-sin  (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-cos  (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-tan  (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-asin (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-acos (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-atan (case-> ((Array Float)  -> (lazy-array Float))
                      ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))

(: array+ (case-> ((Array Float)  (Array Float)  -> (lazy-array Float))
                  ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))
(: array* (case-> ((Array Float)  (Array Float)  -> (lazy-array Float))
                  ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))

(: array- (case-> ((Array Float)  -> (lazy-array Float))
                  ((Array Real)   -> (lazy-array Real))
                  ((Array Number) -> (lazy-array Number))
                  ((Array Float)  (Array Float)  -> (lazy-array Float))
                  ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))

(: array/ (case-> ((Array Float)  -> (lazy-array Float))
                  ((Array Real)   -> (lazy-array Real))
                  ((Array Number) -> (lazy-array Number))
                  ((Array Float)  (Array Float)  -> (lazy-array Float))
                  ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))

(: array-expt ((Array Number) (Array Number) -> (lazy-array Number)))

(: array-min  (case-> ((Array Float) (Array Float) -> (lazy-array Float))
                      ((Array Real)  (Array Real)  -> (lazy-array Real))))
(: array-max  (case-> ((Array Float) (Array Float) -> (lazy-array Float))
                      ((Array Real)  (Array Real)  -> (lazy-array Real))))

(begin-encourage-inline
  
  (define array-abs      (inline-array-lift abs))
  (define array-round    (inline-array-lift round))
  (define array-floor    (inline-array-lift floor))
  (define array-ceiling  (inline-array-lift ceiling))
  (define array-truncate (inline-array-lift truncate))
  
  (define array-sqrt (inline-array-lift sqrt))
  (define array-log  (inline-array-lift log))
  (define array-exp  (inline-array-lift exp))
  (define array-sin  (inline-array-lift sin))
  (define array-cos  (inline-array-lift cos))
  (define array-tan  (inline-array-lift tan))
  (define array-asin (inline-array-lift asin))
  (define array-acos (inline-array-lift acos))
  (define array-atan (inline-array-lift atan))
  
  (define array+ (inline-array-lift2 'array+ +))
  (define array* (inline-array-lift2 'array* *))
  
  (define array-
    (case-lambda
      [(arr)        (array-map - arr)]
      [(arr1 arr2)  (array-map2 - arr1 arr2)]))
  
  (define array/
    (case-lambda
      [(arr)        (array-map / arr)]
      [(arr1 arr2)  (array-map2 / arr1 arr2)]))
  
  (define array-expt (inline-array-lift2 'array-expt expt))
  (define array-min  (inline-array-lift2 'array-min  min))
  (define array-max  (inline-array-lift2 'array-max  max))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Conversions

(begin-encourage-inline
  
  (: array-inexact->exact (case-> ((Array Real)   -> (lazy-array Exact-Rational))
                                  ((Array Number) -> (lazy-array Exact-Number))))
  
  (: array-exact->inexact (case-> ((Array Real)   -> (lazy-array Inexact-Real))
                                  ((Array Number) -> (lazy-array Number))))
  
  (: array-real->double-flonum ((Array Real) -> (lazy-array Float)))
  (: array-real->single-flonum ((Array Real) -> (lazy-array Single-Flonum)))
  
  (: array-number->float-complex ((Array Number) -> (lazy-array Float-Complex)))
  
  (: array-real-part ((Array Number) -> (lazy-array Real)))
  (: array-imag-part ((Array Number) -> (lazy-array Real)))
  
  (define array-inexact->exact (inline-array-lift inexact->exact))
  (define array-exact->inexact (inline-array-lift exact->inexact))
  (define array-real->double-flonum (array-lift real->double-flonum))
  (define array-real->single-flonum (array-lift real->single-flonum))
  (define array-number->float-complex (array-lift (λ: ([x : Number]) (+ x 0.0+0.0i))))
  (define array-real-part (array-lift real-part))
  (define array-imag-part (array-lift imag-part))
  
  )  ; begin-encourage-inline
