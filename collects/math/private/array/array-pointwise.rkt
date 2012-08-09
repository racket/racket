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
 ;; Equality
 array-all-equal?
 array-all-eqv?
 array-all-eq?
 array-all=
 ;; Lifting
 inline-array-lift1
 inline-array-lift2
 inline-array-lift3
 array-lift1
 array-lift2
 array-lift3
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
 array-imag-part)

;; ===================================================================================================
;; Equality

(: array-all-equal? ((Array Any) (Array Any) -> Boolean))
(define array-all-equal? equal?)

(: array-all-eqv? ((Array Any) (Array Any) -> Boolean))
(define array-all-eqv? (array-lift-comparison eqv?))

(: array-all-eq? ((Array Any) (Array Any) -> Boolean))
(define array-all-eq? (array-lift-comparison eq?))

(: array-all= ((Array Number) (Array Number) -> Boolean))
(define array-all= (array-lift-comparison =))

;; ===================================================================================================
;; Lifting

#|
The lift operators could be just functions, but then it wouldn't be possible to give the results more
precise types. For example, (array-lift exp) can only have the type

    ((Array Number) -> (View-Array Number))

or the type

    ((Array Real) -> (View-Array Real))

but (inline-array-lift1 exp) can have the type

    (case-> ((Array Real)   -> (View-Array Real))
            ((Array Number) -> (View-Array Number)))

IOW, the macro lift operators allow us to have array-exp do the job of both array-real-exp and
array-number-exp.
|#

; (All (A B) ((A -> B) -> ((Array A) -> (View-Array B))))
(define-syntax (inline-array-lift1 stx)
  (syntax-case stx ()
    [(_ f)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-view arr)])
           (define ds (array-shape arr))
           (define g (unsafe-array-proc arr))
           (unsafe-view-array ds (λ: ([js : Indexes]) (f (g js)))))))]))

; (All (A B C) ((A B -> C) -> ((Array A) (Array B) -> (View-Array C))))
(define-syntax (inline-array-lift2 stx)
  (syntax-case stx ()
    [(_ f)
     (syntax/loc stx
       (λ (arr1 arr2)
         (let*-values ([(arr1 arr2)  (array-broadcast arr1 arr2)]
                       [(arr1)  (array-view arr1)]
                       [(arr2)  (array-view arr2)])
           (define ds (array-shape arr1))
           (define g1 (unsafe-array-proc arr1))
           (define g2 (unsafe-array-proc arr2))
           (unsafe-view-array ds (λ: ([js : Indexes]) (f (g1 js) (g2 js)))))))]))

; (All (A B C D) ((A B C -> D) -> ((Array A) (Array B) (Array C) -> (View-Array D))))
(define-syntax (inline-array-lift3 stx)
  (syntax-case stx ()
    [(_ f)
     (syntax/loc stx
       (λ (arr1 arr2 arr3)
         (define ds (shape-broadcast* (list (array-shape arr1)
                                            (array-shape arr2)
                                            (array-shape arr3))))
         (let ([arr1  (array-view (array-broadcast-to-shape arr1 ds))]
               [arr2  (array-view (array-broadcast-to-shape arr2 ds))]
               [arr3  (array-view (array-broadcast-to-shape arr3 ds))])
           (define g1 (unsafe-array-proc arr1))
           (define g2 (unsafe-array-proc arr2))
           (define g3 (unsafe-array-proc arr3))
           (unsafe-view-array ds (λ: ([js : Indexes]) (f (g1 js) (g2 js) (g3 js)))))))]))

(begin-encourage-inline
  
  (: array-lift1 (All (A B) ((A -> B) -> ((Array A) -> (View-Array B)))))
  (define (array-lift1 f) (inline-array-lift1 f))
  
  (: array-lift2 (All (A B C) ((A B -> C) -> ((Array A) (Array B) -> (View-Array C)))))
  (define (array-lift2 f) (inline-array-lift2 f))
  
  (: array-lift3 (All (A B C D) ((A B C -> D) -> ((Array A) (Array B) (Array C) -> (View-Array D)))))
  (define (array-lift3 f) (inline-array-lift3 f))
  
  )  ; begin-encourage-inline

(: array-map (All (R A B T ...)
                  (case-> ((-> R) -> (View-Array R))
                          ((A -> R) (Array A) -> (View-Array R))
                          ((A B -> R) (Array A) (Array B) -> (View-Array R))
                          ((A B T ... T -> R) (Array A) (Array B) (Array T) ... T
                                              -> (View-Array R)))))
(define array-map
  (case-lambda:
    [([f : (-> R)])
     (make-array #() (f))]
    [([f : (A -> R)] [arr : (Array A)])
     ((inline-array-lift1 f) arr)]
    [([f : (A B -> R)] [arr1 : (Array A)] [arr2 : (Array B)])
     ((inline-array-lift2 f) arr1 arr2)]
    [([f : (A B T ... T -> R)] [arr1 : (Array A)] [arr2 : (Array B)] . [arrs : (Array T) ... T])
     (define ds (shape-broadcast* (list* (array-shape arr1)
                                         (array-shape arr2)
                                         (map array-shape arrs))))
     (let ([arr1  (array-view (array-broadcast-to-shape arr1 ds))]
           [arr2  (array-view (array-broadcast-to-shape arr2 ds))]
           [arrs  (map (λ: ([arr : (Array T)]) (array-view (array-broadcast-to-shape arr ds))) arrs)])
       (define g1 (unsafe-array-proc arr1))
       (define g2 (unsafe-array-proc arr2))
       (define gs (map unsafe-array-proc arrs))
       (unsafe-view-array
        ds (λ: ([js : Indexes]) (apply f (g1 js) (g2 js)
                                       (map (λ: ([g : (Indexes -> T)]) (g js)) gs)))))]))

(: array-scale (case-> (Float  (Array Float)  -> (View-Array Float))
                       (Real   (Array Real)   -> (View-Array Real))
                       (Number (Array Number) -> (View-Array Number))))
(define (array-scale s arr)
  ((inline-array-lift1 (λ (x) (* s x))) arr))

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

(: array=  ((Array Real) (Array Real) -> (View-Array Boolean)))
(: array<  ((Array Real) (Array Real) -> (View-Array Boolean)))
(: array<= ((Array Real) (Array Real) -> (View-Array Boolean)))
(: array>  ((Array Real) (Array Real) -> (View-Array Boolean)))
(: array>= ((Array Real) (Array Real) -> (View-Array Boolean)))

(: array-not ((Array Any) -> (View-Array Boolean)))
(: array-and (All (A B) ((Array A) (Array B) -> (View-Array (U B #f)))))
(: array-or  (All (A B) ((Array A) (Array B) -> (View-Array (U A B)))))
(: array-if  (All (A) ((Array Any) (Array A) (Array A) -> (View-Array A))))

(begin-encourage-inline
  
  (define array-abs       (inline-array-lift1 abs))
  (define array-round     (inline-array-lift1 round))
  (define array-floor     (inline-array-lift1 floor))
  (define array-ceiling   (inline-array-lift1 ceiling))
  (define array-truncate  (inline-array-lift1 truncate))
  
  (define array-sqr       (inline-array-lift1 sqr))
  (define array-sqrt      (inline-array-lift1 sqrt))
  (define array-conjugate (inline-array-lift1 conjugate))
  (define array-magnitude (inline-array-lift1 magnitude))
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
      [(arr)        (array-map - arr)]
      [(arr1 arr2)  (array-map - arr1 arr2)]))
  
  (define array/
    (case-lambda
      [(arr)        (array-map / arr)]
      [(arr1 arr2)  (array-map / arr1 arr2)]))
  
  (define array-expt (inline-array-lift2 expt))
  (define array-min  (inline-array-lift2 min))
  (define array-max  (inline-array-lift2 max))
  
  (define array=  (inline-array-lift2 =))
  (define array<  (inline-array-lift2 <))
  (define array<= (inline-array-lift2 <=))
  (define array>  (inline-array-lift2 >))
  (define array>= (inline-array-lift2 >=))
  
  (define array-not (inline-array-lift1  not))
  (define array-or  (inline-array-lift2 or))
  (define array-and (inline-array-lift2 and))
  (define array-if  (inline-array-lift3 if))
  
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
  
  (define array-inexact->exact (inline-array-lift1 inexact->exact))
  (define array-exact->inexact (inline-array-lift1 exact->inexact))
  (define array-real->double-flonum (array-lift1 real->double-flonum))
  (define array-real->single-flonum (array-lift1 real->single-flonum))
  (define array-number->float-complex (array-lift1 (λ: ([x : Number]) (+ x 0.0+0.0i))))
  (define array-real-part (array-lift1 real-part))
  (define array-imag-part (array-lift1 imag-part))
  
  )  ; begin-encourage-inline
