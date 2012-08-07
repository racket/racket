#lang typed/racket/base

(require racket/performance-hint
         (only-in racket/math conjugate sqr)
         (for-syntax racket/base)
         "../unsafe.rkt"
         "array-struct.rkt"
         "for-each.rkt"
         "utils.rkt")

(provide array=
         array-scale
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
         array-sqr
         array-sqrt
         array-conjugate
         array-magnitude
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

(: view-array= ((View-Array Number) (View-Array Number) Indexes -> Boolean))
;; Assumes both arrays have shape `ds'
(define (view-array= arr brr ds)
  (let/ec: return : Boolean
    (define f (unsafe-array-proc arr))
    (define g (unsafe-array-proc brr))
    (for-each-array-index ds (λ (js) (unless (= (f js) (g js))
                                       (return #f))))
    #t))

(: mixed-array= ((View-Array Number) (Strict-Array Number) Indexes -> Boolean))
;; Assumes both arrays have shape `ds'
(define (mixed-array= arr brr ds)
  (let/ec: return : Boolean
    (define f (unsafe-array-proc arr))
    (define vs (strict-array-data brr))
    (for-each-array+data-index ds (λ (js j) (unless (= (f js) (unsafe-vector-ref vs j))
                                              (return #f))))
    #t))

(: strict-array= ((Strict-Array Number) (Strict-Array Number) -> Boolean))
;; Assumes arrays have the same size, and returns nonsense if they have different shapes
(define (strict-array= arr brr)
  (define n (array-size arr))
  (define xs (strict-array-data arr))
  (define ys (strict-array-data brr))
  (let loop ([#{j : Nonnegative-Fixnum} 0])
    (cond [(j . < . n)
           (cond [(not (= (unsafe-vector-ref xs j) (unsafe-vector-ref ys j)))  #f]
                 [else  (loop (+ j 1))])]
          [else  #t])))

(: array= ((Array Number) (Array Number) -> Boolean))
(define (array= arr brr)
  (define ds (array-shape arr))
  (and (equal? ds (array-shape brr))
       (cond [(view-array? arr)
              (cond [(view-array? brr)  (view-array= arr brr ds)]
                    [else  (mixed-array= arr brr ds)])]
             [else
              (cond [(view-array? brr)  (mixed-array= brr arr ds)]
                    [else  (strict-array= arr brr)])])))

;; ===================================================================================================
;; Lifting

#|
The lift operators could be just functions, but then it wouldn't be possible to give the results more
precise types. For example, (array-lift exp) can only have the type

    ((Array Number) -> (View-Array Number))

or the type

    ((Array Real) -> (View-Array Real))

but (inline-array-lift exp) can have the type

    (case-> ((Array Real)   -> (View-Array Real))
            ((Array Number) -> (View-Array Number)))

IOW, the macro lift operators allow us to have array-exp do the job of both array-real-exp and
array-number-exp.
|#

;(: inline-array-lift (All (A B) ((A -> B) -> ((Array A) -> (View-Array B)))))
(define-syntax (inline-array-lift stx)
  (syntax-case stx ()
    [(_ f)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-view arr)])
           (define ds (array-shape arr))
           (define g (unsafe-array-proc arr))
           (unsafe-view-array ds (λ: ([js : Indexes]) (f (g js)))))))]))

;(: inline-array-lift2 (All (A B C) ((A B -> C) -> ((Array A) (Array B) -> (View-Array C)))))
(define-syntax (inline-array-lift2 stx)
  (syntax-case stx ()
    [(_ name f)
     (syntax/loc stx
       (λ (arr1 arr2)
         (let ([arr1  (array-view arr1)]
               [arr2  (array-view arr2)])
           (define ds (array-shape arr1))
           (check-equal-array-shape! name ds (array-shape arr2))
           (define g1 (unsafe-array-proc arr1))
           (define g2 (unsafe-array-proc arr2))
           (unsafe-view-array ds (λ: ([js : Indexes]) (f (g1 js) (g2 js)))))))]))

(begin-encourage-inline
  
  (: array-lift (All (A B) ((A -> B) -> ((Array A) -> (View-Array B)))))
  (define (array-lift f) (inline-array-lift f))
  
  (: array-lift2 (All (A B C) (Symbol (A B -> C) -> ((Array A) (Array B) -> (View-Array C)))))
  (define (array-lift2 name f) (inline-array-lift2 name f))
  
  (: array-map (All (A B) ((A -> B) (Array A) -> (View-Array B))))
  (define (array-map f arr) ((inline-array-lift f) arr))
  
  (: array-map2 (All (A B C) ((A B -> C) (Array A) (Array B) -> (View-Array C))))
  (define (array-map2 f arr1 arr2) ((inline-array-lift2 'array-map2 f) arr1 arr2))
  
  )  ; begin-encourage-inline

(: array-scale (case-> (Float  (Array Float)  -> (View-Array Float))
                       (Real   (Array Real)   -> (View-Array Real))
                       (Number (Array Number) -> (View-Array Number))))
(define (array-scale s arr)
  ((inline-array-lift (λ (x) (* s x))) arr))

;; ===================================================================================================
;; Lifted operations on Real and Number

(: array-abs      (case-> ((Array Float) -> (View-Array Float))
                          ((Array Real)  -> (View-Array Real))))
(: array-round    (case-> ((Array Float) -> (View-Array Float))
                          ((Array Real)  -> (View-Array Real))))
(: array-floor    (case-> ((Array Float) -> (View-Array Float))
                          ((Array Real)  -> (View-Array Real))))
(: array-ceiling  (case-> ((Array Float) -> (View-Array Float))
                          ((Array Real)  -> (View-Array Real))))
(: array-truncate (case-> ((Array Float) -> (View-Array Float))
                          ((Array Real)  -> (View-Array Real))))


(: array-sqrt      ((Array Number) -> (View-Array Number)))
(: array-conjugate ((Array Number) -> (View-Array Number)))
(: array-magnitude ((Array Number) -> (View-Array Number)))
(: array-log       ((Array Number) -> (View-Array Number)))

(: array-sqr  (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-exp  (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-sin  (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-cos  (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-tan  (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-asin (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-acos (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))
(: array-atan (case-> ((Array Float)  -> (View-Array Float))
                      ((Array Real)   -> (View-Array Real))
                      ((Array Number) -> (View-Array Number))))

(: array+ (case-> ((Array Float)  (Array Float)  -> (View-Array Float))
                  ((Array Real)   (Array Real)   -> (View-Array Real))
                  ((Array Number) (Array Number) -> (View-Array Number))))
(: array* (case-> ((Array Float)  (Array Float)  -> (View-Array Float))
                  ((Array Real)   (Array Real)   -> (View-Array Real))
                  ((Array Number) (Array Number) -> (View-Array Number))))

(: array- (case-> ((Array Float)  -> (View-Array Float))
                  ((Array Real)   -> (View-Array Real))
                  ((Array Number) -> (View-Array Number))
                  ((Array Float)  (Array Float)  -> (View-Array Float))
                  ((Array Real)   (Array Real)   -> (View-Array Real))
                  ((Array Number) (Array Number) -> (View-Array Number))))

(: array/ (case-> ((Array Float)  -> (View-Array Float))
                  ((Array Real)   -> (View-Array Real))
                  ((Array Number) -> (View-Array Number))
                  ((Array Float)  (Array Float)  -> (View-Array Float))
                  ((Array Real)   (Array Real)   -> (View-Array Real))
                  ((Array Number) (Array Number) -> (View-Array Number))))

(: array-expt ((Array Number) (Array Number) -> (View-Array Number)))

(: array-min  (case-> ((Array Float) (Array Float) -> (View-Array Float))
                      ((Array Real)  (Array Real)  -> (View-Array Real))))
(: array-max  (case-> ((Array Float) (Array Float) -> (View-Array Float))
                      ((Array Real)  (Array Real)  -> (View-Array Real))))




(begin-encourage-inline
  
  (define array-abs       (inline-array-lift abs))
  (define array-round     (inline-array-lift round))
  (define array-floor     (inline-array-lift floor))
  (define array-ceiling   (inline-array-lift ceiling))
  (define array-truncate  (inline-array-lift truncate))
  
  (define array-sqr       (inline-array-lift sqr))
  (define array-sqrt      (inline-array-lift sqrt))
  (define array-conjugate (inline-array-lift conjugate))
  (define array-magnitude (inline-array-lift magnitude))
  (define array-log       (inline-array-lift log))
  (define array-exp       (inline-array-lift exp))
  (define array-sin       (inline-array-lift sin))
  (define array-cos       (inline-array-lift cos))
  (define array-tan       (inline-array-lift tan))
  (define array-asin      (inline-array-lift asin))
  (define array-acos      (inline-array-lift acos))
  (define array-atan      (inline-array-lift atan))
  
  
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
  
  (: array-inexact->exact (case-> ((Array Real)   -> (View-Array Exact-Rational))
                                  ((Array Number) -> (View-Array Exact-Number))))
  
  (: array-exact->inexact (case-> ((Array Real)   -> (View-Array Inexact-Real))
                                  ((Array Number) -> (View-Array Number))))
  
  (: array-real->double-flonum ((Array Real) -> (View-Array Float)))
  (: array-real->single-flonum ((Array Real) -> (View-Array Single-Flonum)))
  
  (: array-number->float-complex ((Array Number) -> (View-Array Float-Complex)))
  
  (: array-real-part ((Array Number) -> (View-Array Real)))
  (: array-imag-part ((Array Number) -> (View-Array Real)))
  
  (define array-inexact->exact (inline-array-lift inexact->exact))
  (define array-exact->inexact (inline-array-lift exact->inexact))
  (define array-real->double-flonum (array-lift real->double-flonum))
  (define array-real->single-flonum (array-lift real->single-flonum))
  (define array-number->float-complex (array-lift (λ: ([x : Number]) (+ x 0.0+0.0i))))
  (define array-real-part (array-lift real-part))
  (define array-imag-part (array-lift imag-part))
  
  )  ; begin-encourage-inline

