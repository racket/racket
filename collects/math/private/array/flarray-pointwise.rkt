#lang typed/racket/base

(require racket/flonum
         (for-syntax racket/base)
         "../../flonum.rkt"
         "array-struct.rkt"
         "array-broadcast.rkt"
         "array-pointwise.rkt"
         "mutable-array.rkt"
         "flarray-struct.rkt"
         "utils.rkt")

(provide
 ;; Mapping
 inline-flarray-map
 flarray-map
 ;; Pointwise operations
 flarray-scale
 flarray-round
 flarray-floor
 flarray-ceiling
 flarray-truncate
 flarray-abs
 flarray-sqr
 flarray-sqrt
 flarray-log
 flarray-exp
 flarray-sin
 flarray-cos
 flarray-tan
 flarray-asin
 flarray-acos
 flarray-atan
 flarray+
 flarray*
 flarray-
 flarray/
 flarray-expt
 flarray-min
 flarray-max
 flarray=
 flarray<
 flarray<=
 flarray>
 flarray>=)

;; ===================================================================================================
;; Mapping

(define-syntax (inline-flarray-map stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (unsafe-flarray #() (flvector (f))))]
    [(_ f arr-expr)
     (syntax/loc stx
       (let: ([arr : FlArray  arr-expr])
         (unsafe-flarray (array-shape arr) (flvector-map f (flarray-data arr)))))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(dss ...)    (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let: ([arr : FlArray  arr-expr]
                [arrs : FlArray  arr-exprs] ...)
           (define ds (array-shape arr))
           (define dss (array-shape arrs)) ...
           (cond [(and (equal? ds dss) ...)
                  (unsafe-flarray
                   ds (flvector-map f (flarray-data arr) (flarray-data arrs) ...))]
                 [else
                  (define new-ds (array-shape-broadcast (list ds dss ...)))
                  (define proc  (unsafe-array-proc (array-broadcast arr new-ds)))
                  (define procs (unsafe-array-proc (array-broadcast arrs new-ds))) ...
                  (array->flarray
                   (unsafe-build-array new-ds (λ: ([js : Indexes])
                                                (f (proc js) (procs js) ...))))]))))]))

(: flarray-map (case-> ((-> Float) -> FlArray)
                       ((Float -> Float) FlArray -> FlArray)
                       ((Float Float Float * -> Float) FlArray FlArray FlArray * -> FlArray)))
(define flarray-map
  (case-lambda:
    [([f : (-> Float)])
     (inline-flarray-map f)]
    [([f : (Float -> Float)] [arr : FlArray])
     (inline-flarray-map f arr)]
    [([f : (Float Float -> Float)] [arr0 : FlArray] [arr1 : FlArray])
     (inline-flarray-map f arr0 arr1)]
    [([f : (Float Float Float * -> Float)] [arr0 : FlArray] [arr1 : FlArray] . [arrs : FlArray *])
     (define ds (array-shape arr0))
     (define dss (map (λ: ([arr : FlArray]) (array-shape arr)) (cons arr1 arrs)))
     (define new-ds (array-shape-broadcast (list* ds dss)))
     (let: ([arr0 : (Array Float)  (array-broadcast arr0 new-ds)]
            [arr1 : (Array Float)  (array-broadcast arr1 new-ds)]
            [arrs : (Listof (Array Float))
                  (map (λ: ([arr : FlArray]) (array-broadcast arr new-ds)) arrs)])
       (define proc0 (unsafe-array-proc arr0))
       (define proc1 (unsafe-array-proc arr1))
       (define procs (map (λ: ([arr : (Array Float)]) (unsafe-array-proc arr)) arrs))
       (array->flarray
        (unsafe-build-array new-ds (λ: ([js : Indexes])
                                     (apply f (proc0 js) (proc1 js)
                                            (map (λ: ([proc : (Indexes -> Float)]) (proc js))
                                                 procs))))))]))

;; ===================================================================================================
;; Pointwise operations

(define-syntax-rule (lift-flvector1 f)
  (λ (arr) (unsafe-flarray (array-shape arr) (f (flarray-data arr)))))

(define-syntax-rule (lift-flvector2 f array-f)
  (λ (arr1 arr2)
    (define ds1 (array-shape arr1))
    (define ds2 (array-shape arr2))
    (cond [(equal? ds1 ds2)  (unsafe-flarray ds1 (f (flarray-data arr1) (flarray-data arr2)))]
          [else  (array->flarray (array-f arr1 arr2))])))

(define-syntax (lift2 stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (λ (arr1 arr2) (inline-flarray-map f arr1 arr2)))]))

(define-syntax-rule (lift-flvector-compare flvector-comp array-comp)
  (λ (arr1 arr2)
    (define ds1 (array-shape arr1))
    (define ds2 (array-shape arr2))
    (cond [(equal? ds1 ds2)
           (unsafe-mutable-array ds1 (flvector-comp (flarray-data arr1) (flarray-data arr2)))]
          [else  (array-comp arr1 arr2)])))

(: flarray-scale (FlArray Float -> FlArray))

(: flarray-round    (FlArray -> FlArray))
(: flarray-floor    (FlArray -> FlArray))
(: flarray-ceiling  (FlArray -> FlArray))
(: flarray-truncate (FlArray -> FlArray))
(: flarray-abs  (FlArray -> FlArray))
(: flarray-sqr  (FlArray -> FlArray))
(: flarray-sqrt (FlArray -> FlArray))
(: flarray-log  (FlArray -> FlArray))
(: flarray-exp  (FlArray -> FlArray))
(: flarray-sin  (FlArray -> FlArray))
(: flarray-cos  (FlArray -> FlArray))
(: flarray-tan  (FlArray -> FlArray))
(: flarray-asin (FlArray -> FlArray))
(: flarray-acos (FlArray -> FlArray))
(: flarray-atan (FlArray -> FlArray))

(: flarray+ (FlArray FlArray -> FlArray))
(: flarray* (FlArray FlArray -> FlArray))
(: flarray- (case-> (FlArray -> FlArray)
                    (FlArray FlArray -> FlArray)))
(: flarray/ (case-> (FlArray -> FlArray)
                    (FlArray FlArray -> FlArray)))
(: flarray-expt (FlArray FlArray -> FlArray))
(: flarray-min  (FlArray FlArray -> FlArray))
(: flarray-max  (FlArray FlArray -> FlArray))

(: flarray=  (FlArray FlArray -> (Array Boolean)))
(: flarray<  (FlArray FlArray -> (Array Boolean)))
(: flarray<= (FlArray FlArray -> (Array Boolean)))
(: flarray>  (FlArray FlArray -> (Array Boolean)))
(: flarray>= (FlArray FlArray -> (Array Boolean)))

(define (flarray-scale arr y)
  (define-syntax-rule (fun xs) (flvector-scale xs y))
  ((lift-flvector1 fun) arr))

(define flarray-round    (lift-flvector1 flvector-round))
(define flarray-floor    (lift-flvector1 flvector-floor))
(define flarray-ceiling  (lift-flvector1 flvector-ceiling))
(define flarray-truncate (lift-flvector1 flvector-truncate))
(define flarray-abs  (lift-flvector1 flvector-abs))
(define flarray-sqr  (lift-flvector1 flvector-sqr))
(define flarray-sqrt (lift-flvector1 flvector-sqrt))
(define flarray-log  (lift-flvector1 flvector-log))
(define flarray-exp  (lift-flvector1 flvector-exp))
(define flarray-sin  (lift-flvector1 flvector-sin))
(define flarray-cos  (lift-flvector1 flvector-cos))
(define flarray-tan  (lift-flvector1 flvector-tan))
(define flarray-asin (lift-flvector1 flvector-asin))
(define flarray-acos (lift-flvector1 flvector-acos))
(define flarray-atan (lift-flvector1 flvector-atan))

(define flarray+ (lift-flvector2 flvector+ array+))
(define flarray* (lift-flvector2 flvector* array*))

(define flarray-
  (case-lambda
    [(arr)  ((lift-flvector1 flvector-) arr)]
    [(arr1 arr2)  ((lift-flvector2 flvector- array-) arr1 arr2)]))

(define flarray/
  (case-lambda
    [(arr)  ((lift-flvector1 flvector/) arr)]
    [(arr1 arr2)  ((lift-flvector2 flvector/ array/) arr1 arr2)]))

(define flarray-expt (lift2 flexpt))
(define flarray-min  (lift-flvector2 flvector-min array-min))
(define flarray-max  (lift-flvector2 flvector-max array-max))

(define flarray=  (lift-flvector-compare flvector=  array=))
(define flarray<  (lift-flvector-compare flvector<  array<))
(define flarray<= (lift-flvector-compare flvector<= array<=))
(define flarray>  (lift-flvector-compare flvector>  array>))
(define flarray>= (lift-flvector-compare flvector>= array>=))
