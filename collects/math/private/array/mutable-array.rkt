#lang typed/racket/base

(require racket/promise
         (for-syntax racket/base syntax/parse)
         "../unsafe.rkt"
         "../exception.rkt"
         "array-struct.rkt"
         "array-syntax.rkt"
         "utils.rkt"
         "for-each.rkt")

(provide
 ;; Mutable-Array
 Mutable-Array
 mutable-array?
 mutable-array-data
 make-mutable-array
 unsafe-mutable-array
 mutable-array-copy
 mutable-array
 ;; Conversion
 array->mutable-array
 array-strict
 flat-vector->matrix)

;; ===================================================================================================
;; Mutable array data type

(struct: (A) Mutable-Array Settable-Array ([data : (Vectorof A)])
  #:property prop:custom-write (λ (arr port mode)
                                 ((array-custom-printer) arr 'mutable-array port mode)))

(define mutable-array? Mutable-Array?)
(define mutable-array-data Mutable-Array-data)

(: unsafe-mutable-array (All (A) (Indexes (Vectorof A) -> (Mutable-Array A))))
(define (unsafe-mutable-array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc A ds (λ (j v) (unsafe-vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) #t proc set-proc vs))

(: make-mutable-array (All (A) (User-Indexes (Vectorof A) -> (Mutable-Array A))))
(define (make-mutable-array ds vs)
  (let* ([ds  (check-array-shape
               ds (λ () (raise-argument-error 'make-mutable-array "(Vectorof Index)" 0 ds vs)))]
         [size  (array-shape-size ds)]
         [n  (vector-length vs)])
    (cond [(= size n)  (unsafe-mutable-array ds vs)]
          [else  (raise-argument-error 'mutable-array (format "Vector of length ~e" size) 1 ds vs)])))

(: mutable-array-copy (All (A) ((Mutable-Array A) -> (Mutable-Array A))))
(define (mutable-array-copy arr)
  (unsafe-mutable-array (array-shape arr) (vector-copy-all (mutable-array-data arr))))

(: flat-vector->matrix : (All (A) (Index Index (Vectorof A) -> (Array A))))
(define (flat-vector->matrix m n v)
  (make-mutable-array (vector m n) v))

;; ===================================================================================================
;; Conversions

(: array->mutable-array (All (A) ((Array A) -> (Mutable-Array A))))
(define (array->mutable-array arr)
  (define ds (array-shape arr))
  (define g (unsafe-array-proc arr))
  (unsafe-mutable-array ds (inline-build-array-data ds (λ (js j) (g js)) A)))

(: array-strict (All (A) ((Array A) -> (Array A))))
(define (array-strict arr)
  (cond [(array-strict? arr)  arr]
        [else  (array->mutable-array arr)]))

(define-syntax (mutable-array stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (array/syntax mutable-array vector make-mutable-array e))]
    [(_ e:expr T:expr)
     (syntax/loc stx (array/syntax mutable-array (inst vector T) make-mutable-array e))]
    [_:id  (raise-syntax-error 'mutable-array "not allowed as an expression" stx)]))
