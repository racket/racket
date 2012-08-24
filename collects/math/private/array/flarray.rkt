#lang typed/racket/base

(require racket/flonum
         racket/list
         (for-syntax racket/base syntax/parse)
         "../unsafe.rkt"
         "../vector/flvector.rkt"
         "array-struct.rkt"
         "array-broadcast.rkt"
         "array-pointwise.rkt"
         "array-syntax.rkt"
         "mutable-array.rkt"
         "utils.rkt"
         "for-each.rkt")

(provide FlArray
         unsafe-flarray
         (rename-out [flarray/syntax flarray])
         array->flarray
         flarray-data
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
;; Struct type and other basics

(struct: (A) flarray Settable-Array ([data : FlVector])
  #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr 'flarray port mode)))

(define-type FlArray (flarray Float))

(: unsafe-flarray (Indexes FlVector -> FlArray))
(define (unsafe-flarray ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-flvector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc Float ds (λ (j v) (unsafe-flvector-set! vs j v))))
  (flarray ds 0 #t proc set-proc vs))

(: unsafe-vector->flarray (Indexes (Vectorof Real) -> FlArray))
(define (unsafe-vector->flarray ds vs)
  (define size (vector-length vs))
  (define xs
    (build-flvector size (λ: ([j : Index]) (real->double-flonum (unsafe-vector-ref vs j)))))
  (unsafe-flarray ds xs))

(define-syntax (flarray/syntax stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (array/syntax flarray (inst vector Real) unsafe-vector->flarray e))]
    [_:id  (raise-syntax-error 'flarray "not allowed as an expression" stx)]))

(: array->flarray ((Array Real) -> FlArray))
(define (array->flarray arr)
  (define ds (array-shape arr))
  (define size (array-size arr))
  (define proc (unsafe-array-proc arr))
  (define vs (make-flvector size))
  (for-each-array+data-index ds (λ (js j) (unsafe-flvector-set!
                                           vs j (real->double-flonum (proc js)))))
  (unsafe-flarray ds vs))

;; ===================================================================================================
;; Mapping

(define-syntax (inline-flarray-map stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (unsafe-flarray #() (flvector (f))))]
    [(_ f arr-expr)
     (syntax/loc stx
       (let: ([arr : FlArray  arr-expr])
         (unsafe-flarray (array-shape arr) (unsafe-flvector-map f (flarray-data arr)))))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(dss ...)    (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let: ([arr : FlArray  arr-expr]
                [arrs : FlArray arr-exprs] ...)
           (define ds (array-shape arr))
           (define dss (array-shape arrs)) ...
           (cond [(and (equal? ds dss) ...)
                  (unsafe-flarray
                   ds (unsafe-flvector-map f (flarray-data arr) (flarray-data arrs) ...))]
                 [else
                  (define new-ds (array-shape-broadcast (list ds dss ...)))
                  (let: ([arr  : (Array Float)  (array-broadcast arr new-ds)]
                         [arrs : (Array Float)  (array-broadcast arrs new-ds)] ...)
                    (define proc  (unsafe-array-proc arr))
                    (define procs (unsafe-array-proc arrs)) ...
                    (array->flarray
                     (unsafe-build-array new-ds (λ: ([js : Indexes])
                                                  (f (proc js) (procs js) ...)))))]))))]))

(: flarray-map (case-> ((-> Float) -> FlArray)
                       ((Float -> Float) FlArray -> FlArray)
                       ((Float Float * -> Float) FlArray FlArray * -> FlArray)))
(define flarray-map
  (case-lambda:
    [([f : (-> Float)])
     (inline-flarray-map f)]
    [([f : (Float -> Float)] [arr : FlArray])
     (inline-flarray-map f arr)]
    [([f : (Float Float * -> Float)] [arr : FlArray] . [arrs : FlArray *])
     (define ds (array-shape arr))
     (define dss (map (λ: ([arr : FlArray]) (array-shape arr)) arrs))
     (cond [(apply all-equal? ds dss)
            (unsafe-flarray ds (apply flvector-map f (flarray-data arr)
                                      (map (λ: ([arr : FlArray]) (flarray-data arr)) arrs)))]
           [else
            (define new-ds (array-shape-broadcast (list* ds dss)))
            (let: ([arr : (Array Float)  (array-broadcast arr new-ds)]
                   [arrs : (Listof (Array Float))
                         (map (λ: ([arr : FlArray]) (array-broadcast arr new-ds)) arrs)])
              (define proc  (unsafe-array-proc arr))
              (define procs (map (λ: ([arr : (Array Float)]) (unsafe-array-proc arr)) arrs))
              (array->flarray
               (unsafe-build-array new-ds (λ: ([js : Indexes])
                                            (apply f (proc js)
                                                   (map (λ: ([proc : (Indexes -> Float)]) (proc js))
                                                        procs))))))])]))

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
