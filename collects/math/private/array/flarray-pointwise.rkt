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
 flarray-sqr
 flarray-sqrt
 flarray-abs
 flarray+
 flarray*
 flarray-
 flarray/
 flarray-min
 flarray-max)

;; ===================================================================================================
;; Mapping

(define-syntax (inline-flarray-map stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (unsafe-flarray #() (flvector (f))))]
    [(_ f arr-expr)
     (syntax/loc stx
       (let: ([arr : FlArray  arr-expr])
         (unsafe-flarray (array-shape arr) (inline-flvector-map f (flarray-data arr)))))]
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
                   ds (inline-flvector-map f (flarray-data arr) (flarray-data arrs) ...))]
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

(: flarray-scale (FlArray Float -> FlArray))
(define (flarray-scale arr y)
  (define-syntax-rule (fun xs) (flvector-scale xs y))
  ((lift-flvector1 fun) arr))

(: flarray-sqr (FlArray -> FlArray))
(define flarray-sqr (lift-flvector1 flvector-sqr))

(: flarray-sqrt (FlArray -> FlArray))
(define flarray-sqrt (lift-flvector1 flvector-sqrt))

(: flarray-abs (FlArray -> FlArray))
(define flarray-abs (lift-flvector1 flvector-abs))

(: flarray+ (FlArray FlArray -> FlArray))
(define flarray+ (lift-flvector2 flvector+ array+))

(: flarray* (FlArray FlArray -> FlArray))
(define flarray* (lift-flvector2 flvector* array*))

(: flarray- (case-> (FlArray -> FlArray)
                    (FlArray FlArray -> FlArray)))
(define flarray-
  (case-lambda
    [(arr)  ((lift-flvector1 flvector-) arr)]
    [(arr1 arr2)  ((lift-flvector2 flvector- array-) arr1 arr2)]))

(: flarray/ (case-> (FlArray -> FlArray)
                    (FlArray FlArray -> FlArray)))
(define flarray/
  (case-lambda
    [(arr)  ((lift-flvector1 flvector/) arr)]
    [(arr1 arr2)  ((lift-flvector2 flvector/ array/) arr1 arr2)]))

(: flarray-min  (FlArray FlArray -> FlArray))
(define flarray-min  (lift-flvector2 flvector-min array-min))

(: flarray-max  (FlArray FlArray -> FlArray))
(define flarray-max  (lift-flvector2 flvector-max array-max))
