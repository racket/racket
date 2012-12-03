#lang typed/racket/base

(require racket/fixnum
         (only-in racket/math conjugate)
         (for-syntax racket/base)
         "../../flonum.rkt"
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-broadcast.rkt"
         "array-pointwise.rkt"
         "flarray-struct.rkt"
         "fcarray-struct.rkt"
         "utils.rkt")

(provide
 ;; Mapping
 inline-fcarray-map
 fcarray-map
 ;; Pointwise operations
 fcarray-scale
 fcarray-sqr
 fcarray-sqrt
 fcarray-magnitude
 fcarray-angle
 fcarray+
 fcarray*
 fcarray-
 fcarray/
 fcarray-conjugate
 fcarray-real-part
 fcarray-imag-part
 fcarray-make-rectangular
 fcarray-make-polar
 )

;; ===================================================================================================
;; Mapping

(define-syntax (inline-fcarray-map stx)
  (syntax-case stx ()
    [(_ f)  (syntax/loc stx
              (let ([z  (f)])
                (unsafe-fcarray #() (flvector (real-part z)) (flvector (imag-part z)))))]
    [(_ f arr-expr)
     (syntax/loc stx
       (let: ([arr : FCArray  arr-expr])
         (define ds (array-shape arr))
         (define xs (fcarray-real-data arr))
         (define ys (fcarray-imag-data arr))
         (define n (flvector-length xs))
         (define new-xs (make-flvector n))
         (define new-ys (make-flvector n))
         (let: loop : FCArray ([j : Nonnegative-Fixnum  0])
           (cond [(j . fx< . n)
                  (define z (f (make-rectangular (unsafe-flvector-ref xs j)
                                                 (unsafe-flvector-ref ys j))))
                  (unsafe-flvector-set! new-xs j (real-part z))
                  (unsafe-flvector-set! new-ys j (imag-part z))
                  (loop (fx+ j 1))]
                 [else
                  (unsafe-fcarray ds new-xs new-ys)]))))]
    [(_ f arr-expr arr-exprs ...)
     (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                   [(dss ...)    (generate-temporaries #'(arr-exprs ...))]
                   [(xss ...)    (generate-temporaries #'(arr-exprs ...))]
                   [(yss ...)    (generate-temporaries #'(arr-exprs ...))]
                   [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
       (syntax/loc stx
         (let: ([arr : FCArray  arr-expr]
                [arrs : FCArray arr-exprs] ...)
           (define ds (array-shape arr))
           (define dss (array-shape arrs)) ...
           (cond [(and (equal? ds dss) ...)
                  (define xs (fcarray-real-data arr))
                  (define ys (fcarray-imag-data arr))
                  (define xss (fcarray-real-data arrs)) ...
                  (define yss (fcarray-imag-data arrs)) ...
                  (define n (flvector-length xs))
                  (define new-xs (make-flvector n))
                  (define new-ys (make-flvector n))
                  (let: loop : FCArray ([j : Nonnegative-Fixnum  0])
                    (cond [(j . fx< . n)
                           (define z (f (make-rectangular (unsafe-flvector-ref xs j)
                                                          (unsafe-flvector-ref ys j))
                                        (make-rectangular (unsafe-flvector-ref xss j)
                                                          (unsafe-flvector-ref yss j))
                                        ...))
                           (unsafe-flvector-set! new-xs j (real-part z))
                           (unsafe-flvector-set! new-ys j (imag-part z))
                           (loop (fx+ j 1))]
                          [else
                           (unsafe-fcarray ds new-xs new-ys)]))]
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
           ((Float-Complex Float-Complex Float-Complex * -> Float-Complex) FCArray FCArray FCArray *
                                                                           -> FCArray)))
(define fcarray-map
  (case-lambda:
    [([f : (-> Float-Complex)])
     (inline-fcarray-map f)]
    [([f : (Float-Complex -> Float-Complex)] [arr : FCArray])
     (inline-fcarray-map f arr)]
    [([f : (Float-Complex Float-Complex -> Float-Complex)] [arr0 : FCArray] [arr1 : FCArray])
     (inline-fcarray-map f arr0 arr1)]
    [([f : (Float-Complex Float-Complex Float-Complex * -> Float-Complex)]
      [arr0 : FCArray] [arr1 : FCArray] . [arrs : FCArray *])
     (define ds (array-shape arr0))
     (define dss (map (λ: ([arr : FCArray]) (array-shape arr)) (cons arr1 arrs)))
     (define new-ds (array-shape-broadcast (list* ds dss)))
     (let: ([arr0 : (Array Float-Complex)  (array-broadcast arr0 new-ds)]
            [arr1 : (Array Float-Complex)  (array-broadcast arr1 new-ds)]
            [arrs : (Listof (Array Float-Complex))
                  (map (λ: ([arr : FCArray]) (array-broadcast arr new-ds)) arrs)])
       (define proc0 (unsafe-array-proc arr0))
       (define proc1 (unsafe-array-proc arr1))
       (define procs (map (λ: ([arr : (Array Float-Complex)]) (unsafe-array-proc arr)) arrs))
       (array->fcarray
        (unsafe-build-array
         new-ds (λ: ([js : Indexes])
                  (apply f (proc0 js) (proc1 js)
                         (map (λ: ([proc : (Indexes -> Float-Complex)]) (proc js))
                              procs))))))]))

;; ===================================================================================================
;; Pointwise operations

(define-syntax-rule (lift1->fl f)
  (λ (arr)
    (define ds (array-shape arr))
    (define xs (fcarray-real-data arr))
    (define ys (fcarray-imag-data arr))
    (define n (flvector-length xs))
    (define new-xs (make-flvector n))
    (let: loop : FlArray ([j : Nonnegative-Fixnum  0])
      (cond [(j . fx< . n)
             (define z (f (make-rectangular (unsafe-flvector-ref xs j)
                                            (unsafe-flvector-ref ys j))))
             (unsafe-flvector-set! new-xs j z)
             (loop (fx+ j 1))]
            [else
             (unsafe-flarray ds new-xs)]))))

(define-syntax-rule (lift1 f)
  (λ: ([arr : FCArray]) (inline-fcarray-map f arr)))

(define-syntax-rule (lift2 f)
  (λ: ([arr1 : FCArray] [arr2 : FCArray]) (inline-fcarray-map f arr1 arr2)))

(: fcarray-scale (FCArray (U Float Float-Complex) -> FCArray))
(define (fcarray-scale arr y)
  (if (flonum? y)
      (inline-fcarray-map (λ (z) (* z y)) arr)
      (inline-fcarray-map (λ (z) (* z y)) arr)))

(: fcarray-sqr (FCArray -> FCArray))
(define fcarray-sqr (lift1 (λ: ([z : Float-Complex]) (* z z))))

(: fcarray-sqrt (FCArray -> FCArray))
(define fcarray-sqrt (lift1 sqrt))

(: fcarray-magnitude (FCArray -> FlArray))
(define fcarray-magnitude (lift1->fl magnitude))

(: fcarray-angle (FCArray -> FlArray))
(define fcarray-angle (lift1->fl angle))

(: fcarray-conjugate (FCArray -> FCArray))
(define fcarray-conjugate (lift1 conjugate))

(: fcarray+ (FCArray FCArray -> FCArray))
(define fcarray+ (lift2 +))

(: fcarray* (FCArray FCArray -> FCArray))
(define fcarray* (lift2 *))

(: fcarray- (case-> (FCArray -> FCArray)
                    (FCArray FCArray -> FCArray)))
(define fcarray-
  (case-lambda
    [(arr)  (inline-fcarray-map (λ (z) (- 0.0 z)) arr)]
    [(arr1 arr2)  (inline-fcarray-map - arr1 arr2)]))

(: fcarray/ (case-> (FCArray -> FCArray)
                    (FCArray FCArray -> FCArray)))
(define fcarray/
  (case-lambda
    [(arr)  (inline-fcarray-map (λ (z) (/ 1.0 z)) arr)]
    [(arr1 arr2)  (inline-fcarray-map / arr1 arr2)]))

(: fcarray-real-part (FCArray -> FlArray))
(define fcarray-real-part (lift1->fl real-part))

(: fcarray-imag-part (FCArray -> FlArray))
(define fcarray-imag-part (lift1->fl imag-part))

(: fcarray-make-rectangular (FlArray FlArray -> FCArray))
(define (fcarray-make-rectangular arr1 arr2)
  (array->fcarray (array-make-rectangular arr1 arr2)))

(: fcarray-make-polar (FlArray FlArray -> FCArray))
(define (fcarray-make-polar arr1 arr2)
  (array->fcarray (array-make-polar arr1 arr2)))
