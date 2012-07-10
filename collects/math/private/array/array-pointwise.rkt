#lang typed/racket/base

(require racket/unsafe/ops
         racket/performance-hint
         (for-syntax racket/base)
         "array-struct.rkt"
         "utils.rkt")

(provide inline-array-lift
         inline-array-lift2
         array-lift
         array-lift2
         array-map
         array-map2
         
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
         
         array-flabs
         array-flround
         array-flfloor
         array-flceiling
         array-fltruncate
         array-flsqrt
         array-fllog
         array-flexp
         array-flsin
         array-flcos
         array-fltan
         array-flasin
         array-flacos
         array-flatan
         array-fl+
         array-fl-
         array-fl*
         array-fl/
         array-flexpt
         array-flmin
         array-flmax
         
         array->flarray)

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

(: array-abs      ((Array Real)   -> (lazy-array Real)))
(: array-round    ((Array Real)   -> (lazy-array Real)))
(: array-floor    ((Array Real)   -> (lazy-array Real)))
(: array-ceiling  ((Array Real)   -> (lazy-array Real)))
(: array-truncate ((Array Real)   -> (lazy-array Real)))
(: array-sqrt     ((Array Number) -> (lazy-array Number)))
(: array-log      ((Array Number) -> (lazy-array Number)))

(: array-exp  (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-sin  (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-cos  (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-tan  (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-asin (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-acos (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))
(: array-atan (case-> ((Array Real)   -> (lazy-array Real))
                      ((Array Number) -> (lazy-array Number))))

(: array+ (case-> ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))
(: array* (case-> ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))

(: array- (case-> ((Array Real) -> (lazy-array Real))
                  ((Array Number) -> (lazy-array Number))
                  ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))

(: array/ (case-> ((Array Real)   -> (lazy-array Real))
                  ((Array Number) -> (lazy-array Number))
                  ((Array Real)   (Array Real)   -> (lazy-array Real))
                  ((Array Number) (Array Number) -> (lazy-array Number))))

(: array-expt ((Array Number) (Array Number) -> (lazy-array Number)))
(: array-min  ((Array Real)   (Array Real)   -> (lazy-array Real)))
(: array-max  ((Array Real)   (Array Real)   -> (lazy-array Real)))

(begin-encourage-inline
  
  (define array-abs      (array-lift abs))
  (define array-round    (array-lift round))
  (define array-floor    (array-lift floor))
  (define array-ceiling  (array-lift ceiling))
  (define array-truncate (array-lift truncate))
  
  (define array-sqrt (array-lift sqrt))
  (define array-log  (array-lift log))
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
  
  (define array-expt (array-lift2 'array-expt expt))
  (define array-min  (array-lift2 'array-min  min))
  (define array-max  (array-lift2 'array-max  max))
  
  )  ; begin-encourage-inline
;; ===================================================================================================
;; Lifted operations on Float

(: array-flabs      ((Array Float) -> (lazy-array Float)))
(: array-flround    ((Array Float) -> (lazy-array Float)))
(: array-flfloor    ((Array Float) -> (lazy-array Float)))
(: array-flceiling  ((Array Float) -> (lazy-array Float)))
(: array-fltruncate ((Array Float) -> (lazy-array Float)))

(: array-flsqrt ((Array Float) -> (lazy-array Float)))
(: array-fllog  ((Array Float) -> (lazy-array Float)))
(: array-flexp  ((Array Float) -> (lazy-array Float)))
(: array-flsin  ((Array Float) -> (lazy-array Float)))
(: array-flcos  ((Array Float) -> (lazy-array Float)))
(: array-fltan  ((Array Float) -> (lazy-array Float)))
(: array-flasin ((Array Float) -> (lazy-array Float)))
(: array-flacos ((Array Float) -> (lazy-array Float)))
(: array-flatan ((Array Float) -> (lazy-array Float)))

(: array-fl+ ((Array Float) (Array Float) -> (lazy-array Float)))
(: array-fl* ((Array Float) (Array Float) -> (lazy-array Float)))

(: array-fl- (case-> ((Array Float) -> (lazy-array Float))
                     ((Array Float) (Array Float) -> (lazy-array Float))))
(: array-fl/ (case-> ((Array Float) -> (lazy-array Float))
                     ((Array Float) (Array Float) -> (lazy-array Float))))

(: array-flexpt ((Array Float) (Array Float) -> (lazy-array Float)))
(: array-flmin  ((Array Float) (Array Float) -> (lazy-array Float)))
(: array-flmin  ((Array Float) (Array Float) -> (lazy-array Float)))

(begin-encourage-inline
  
  (define array-flabs      (array-lift unsafe-flabs))
  (define array-flround    (array-lift unsafe-flround))
  (define array-flfloor    (array-lift unsafe-flfloor))
  (define array-flceiling  (array-lift unsafe-flceiling))
  (define array-fltruncate (array-lift unsafe-fltruncate))
  
  (define array-flsqrt (array-lift unsafe-flsqrt))
  (define array-fllog  (array-lift unsafe-fllog))
  (define array-flexp  (array-lift unsafe-flexp))
  (define array-flsin  (array-lift unsafe-flsin))
  (define array-flcos  (array-lift unsafe-flcos))
  (define array-fltan  (array-lift unsafe-fltan))
  (define array-flasin (array-lift unsafe-flasin))
  (define array-flacos (array-lift unsafe-flacos))
  (define array-flatan (array-lift unsafe-flatan))
  
  (define array-fl+ (array-lift2 'array-fl+ unsafe-fl+))
  (define array-fl* (array-lift2 'array-fl* unsafe-fl*))
  
  (define array-fl-
    (case-lambda
      [(arr)        (array-map (λ: ([x : Float]) (unsafe-fl- 0.0 x)) arr)]
      [(arr1 arr2)  (array-map2 unsafe-fl- arr1 arr2)]))
  
  (define array-fl/
    (case-lambda
      [(arr)        (array-map (λ: ([x : Float]) (unsafe-fl/ 1.0 x)) arr)]
      [(arr1 arr2)  (array-map2 unsafe-fl/ arr1 arr2)]))
  
  (define array-flexpt (array-lift2 'array-flexpt unsafe-flexpt))
  (define array-flmin  (array-lift2 'array-flmin  unsafe-flmin))
  (define array-flmax  (array-lift2 'array-flmax  unsafe-flmax))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Conversions

(begin-encourage-inline
  
  (: array->flarray ((Array Real) -> (lazy-array Float)))
  (define array->flarray (array-lift real->double-flonum))
  
  )  ; begin-encourage-inline
