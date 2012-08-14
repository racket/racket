#lang typed/racket/base

(require racket/flonum
         racket/list
         (only-in racket/math conjugate)
         (for-syntax racket/base syntax/parse)
         "../unsafe.rkt"
         "../vector/fcvector.rkt"
         "../vector/flvector.rkt"
         "array-struct.rkt"
         "array-broadcast.rkt"
         "array-pointwise.rkt"
         "array-syntax.rkt"
         "mutable-array.rkt"
         "flarray.rkt"
         "utils.rkt"
         "for-each.rkt")

(provide FCArray
         unsafe-fcarray
         (rename-out [fcarray/syntax fcarray])
         array->fcarray
         fcarray-data
         ;; Mapping
         inline-fcarray-map
         fcarray-map
         ;; Pointwise operations
         fcarray-scale
         fcarray-sqr
         fcarray-sqrt
         fcarray-conjugate
         fcarray-magnitude
         fcarray-angle
         fcarray-log
         fcarray-exp
         fcarray-sin
         fcarray-cos
         fcarray-tan
         fcarray-asin
         fcarray-acos
         fcarray-atan
         fcarray+
         fcarray*
         fcarray-
         fcarray/
         fcarray-expt
         fcarray=
         fcarray-real-part
         fcarray-imag-part
         fcarray-make-rectangular)

;; ===================================================================================================
;; Struct type and other basics

(struct: (A) fcarray Settable-Array ([data : FCVector])
  #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr 'fcarray port mode)))

(define-type FCArray (fcarray Float-Complex))

(: unsafe-fcarray (Indexes FCVector -> FCArray))
(define (unsafe-fcarray ds zs)
  (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-fcvector-ref zs j))))
  (define set-proc (make-unsafe-array-set-proc
                    Float-Complex ds (λ (j v) (unsafe-fcvector-set! zs j v))))
  (fcarray ds 0 #t proc set-proc zs))

(define-syntax (inline-number->float-complex stx)
  (syntax-case stx ()
    [(_ z-expr)  (syntax/loc stx
                   (let: ([z : Number  z-expr])
                     (make-rectangular (real->double-flonum (real-part z))
                                       (real->double-flonum (imag-part z)))))]
    [(_ e ...)  (syntax/loc stx (number->float-complex e ...))]
    [_  (syntax/loc stx number->float-complex)]))

(: number->float-complex (Number -> Float-Complex))
(define (number->float-complex z) (inline-number->float-complex z))

(: unsafe-vector->fcarray (Indexes (Vectorof Number) -> FCArray))
(define (unsafe-vector->fcarray ds zs)
  (define size (vector-length zs))
  (define-syntax-rule (ref j) (inline-number->float-complex (unsafe-vector-ref zs j)))
  (define new-zs (build-fcvector size ref))
  (unsafe-fcarray ds new-zs))

(define-syntax (fcarray/syntax stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (array/syntax fcarray (inst vector Number) unsafe-vector->fcarray e))]
    [_:id  (raise-syntax-error 'fcarray "not allowed as an expression" stx)]))

(: array->fcarray ((Array Number) -> FCArray))
(define (array->fcarray arr)
  (define ds (array-shape arr))
  (define size (array-size arr))
  (define proc (unsafe-array-proc arr))
  (define zs (make-fcvector size))
  (for-each-array+data-index
   ds (λ (js j) (unsafe-fcvector-set! zs j (inline-number->float-complex (proc js)))))
  (unsafe-fcarray ds zs))

;; ===================================================================================================
;; Mapping

(define-syntax (inline-fcarray-map stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx (unsafe-fcarray #() (fcvector (f))))]
    [(_ f arr-expr)
     (syntax/loc stx
       (let: ([arr : FCArray  arr-expr])
         (unsafe-fcarray (array-shape arr) (unsafe-fcvector-map f (fcarray-data arr)))))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(dss ...)    (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let: ([arr : FCArray  arr-expr]
                [arrs : FCArray arr-exprs] ...)
           (define ds (array-shape arr))
           (define dss (array-shape arrs)) ...
           (cond [(and (equal? ds dss) ...)
                  (unsafe-fcarray ds (unsafe-fcvector-map f (fcarray-data arr)
                                                          (fcarray-data arrs) ...))]
                 [else
                  (define new-ds (array-shape-broadcast (list ds dss ...)))
                  (let: ([arr  : (Array Float-Complex)  (array-broadcast arr new-ds)]
                         [arrs : (Array Float-Complex)  (array-broadcast arrs new-ds)] ...)
                    (define proc  (unsafe-array-proc arr))
                    (define procs (unsafe-array-proc arrs)) ...
                    (array->fcarray
                     (unsafe-build-array
                      new-ds (λ: ([js : Indexes]) (f (proc js) (procs js) ...)))))]))))]))

(: fcarray-map
   (case-> ((-> Float-Complex) -> FCArray)
           ((Float-Complex -> Float-Complex) FCArray -> FCArray)
           ((Float-Complex Float-Complex * -> Float-Complex) FCArray FCArray * -> FCArray)))
(define fcarray-map
  (case-lambda:
    [([f : (-> Float-Complex)])
     (inline-fcarray-map f)]
    [([f : (Float-Complex -> Float-Complex)] [arr : FCArray])
     (inline-fcarray-map f arr)]
    [([f : (Float-Complex Float-Complex * -> Float-Complex)] [arr : FCArray] . [arrs : FCArray *])
     (define ds (array-shape arr))
     (define dss (map array-shape arrs))
     (cond [(apply all-equal? ds dss)
            (unsafe-fcarray ds (apply fcvector-map f (fcarray-data arr)
                                      (map (λ: ([arr : FCArray]) (fcarray-data arr)) arrs)))]
           [else
            (define new-ds (array-shape-broadcast (list* ds dss)))
            (let: ([arr : (Array Float-Complex)  (array-broadcast arr new-ds)]
                   [arrs : (Listof (Array Float-Complex))
                         (map (λ: ([arr : FCArray]) (array-broadcast arr new-ds)) arrs)])
              (define proc  (unsafe-array-proc arr))
              (define procs (map (λ: ([arr : (Array Float-Complex)]) (unsafe-array-proc arr)) arrs))
              (array->fcarray
               (unsafe-build-array
                new-ds (λ: ([js : Indexes])
                         (apply f (proc js) (map (λ: ([proc : (Indexes -> Float-Complex)]) (proc js))
                                                 procs))))))])]))

;; ===================================================================================================
;; Pointwise operations

(define-syntax-rule (lift-fcvector1 f)
  (λ (arr) (unsafe-fcarray (array-shape arr) (f (fcarray-data arr)))))

(define-syntax-rule (lift-fc->flvector1 f)
  (λ (arr) (unsafe-flarray (array-shape arr) (f (fcarray-data arr)))))

(define-syntax-rule (lift-fcvector2 f array-f)
  (λ (arr1 arr2)
    (define ds1 (array-shape arr1))
    (define ds2 (array-shape arr2))
    (cond [(equal? ds1 ds2)  (unsafe-fcarray ds1 (f (fcarray-data arr1) (fcarray-data arr2)))]
          [else  (array->fcarray (array-f arr1 arr2))])))

(: fcarray-scale (FCArray (U Float Float-Complex) -> FCArray))

(: fcarray-sqr (FCArray -> FCArray))
(: fcarray-sqrt (FCArray -> FCArray))
(: fcarray-conjugate (FCArray -> FCArray))
(: fcarray-magnitude (FCArray -> FlArray))
(: fcarray-angle (FCArray -> FlArray))
(: fcarray-log (FCArray -> FCArray))
(: fcarray-exp (FCArray -> FCArray))
(: fcarray-sin (FCArray -> FCArray))
(: fcarray-cos (FCArray -> FCArray))
(: fcarray-tan (FCArray -> FCArray))
(: fcarray-asin (FCArray -> FCArray))
(: fcarray-acos (FCArray -> FCArray))
(: fcarray-atan (FCArray -> FCArray))

(: fcarray+ (FCArray FCArray -> FCArray))
(: fcarray* (FCArray FCArray -> FCArray))
(: fcarray- (case-> (FCArray -> FCArray)
                    (FCArray FCArray -> FCArray)))
(: fcarray/ (case-> (FCArray -> FCArray)
                    (FCArray FCArray -> FCArray)))
(: fcarray-expt (FCArray FCArray -> FCArray))
(: fcarray= (FCArray FCArray -> (Array Boolean)))

(: fcarray-real-part (FCArray -> FlArray))
(: fcarray-imag-part (FCArray -> FlArray))
(: fcarray-make-rectangular (FlArray FlArray -> FCArray))

(define (fcarray-scale arr y)
  (define-syntax-rule (fun xs) (fcvector-scale xs y))
  ((lift-fcvector1 fun) arr))

(define fcarray-sqr (lift-fcvector1 fcvector-sqr))
(define fcarray-sqrt (lift-fcvector1 fcvector-sqrt))
(define fcarray-conjugate (lift-fcvector1 fcvector-conjugate))
(define fcarray-magnitude (lift-fc->flvector1 fcvector-magnitude))
(define fcarray-angle (lift-fc->flvector1 fcvector-angle))
(define fcarray-log (lift-fcvector1 fcvector-log))
(define fcarray-exp (lift-fcvector1 fcvector-exp))
(define fcarray-sin (lift-fcvector1 fcvector-sin))
(define fcarray-cos (lift-fcvector1 fcvector-cos))
(define fcarray-tan (lift-fcvector1 fcvector-tan))
(define fcarray-asin (lift-fcvector1 fcvector-asin))
(define fcarray-acos (lift-fcvector1 fcvector-acos))
(define fcarray-atan (lift-fcvector1 fcvector-atan))

(define fcarray+ (lift-fcvector2 fcvector+ array+))
(define fcarray* (lift-fcvector2 fcvector* array*))

(define fcarray-
  (case-lambda
    [(arr)  ((lift-fcvector1 fcvector-) arr)]
    [(arr1 arr2)  ((lift-fcvector2 fcvector- array-) arr1 arr2)]))

(define fcarray/
  (case-lambda
    [(arr)  ((lift-fcvector1 fcvector/) arr)]
    [(arr1 arr2)  ((lift-fcvector2 fcvector/ array/) arr1 arr2)]))

(define fcarray-expt (lift-fcvector2 fcvector-expt array-expt))

(define (fcarray= arr1 arr2)
  (define ds1 (array-shape arr1))
  (define ds2 (array-shape arr2))
  (cond [(equal? ds1 ds2)
         (unsafe-mutable-array ds1 (fcvector= (fcarray-data arr1) (fcarray-data arr2)))]
        [else  (array= arr1 arr2)]))

(define fcarray-real-part (lift-fc->flvector1 fcvector-real-part))
(define fcarray-imag-part (lift-fc->flvector1 fcvector-imag-part))

(define (fcarray-make-rectangular arr1 arr2)
  (define ds1 (array-shape arr1))
  (define ds2 (array-shape arr2))
  (cond [(equal? ds1 ds2)  (unsafe-fcarray ds1 (fcvector-make-rectangular (flarray-data arr1)
                                                                          (flarray-data arr2)))]
        [else  (array->fcarray (array-make-rectangular arr1 arr2))]))
