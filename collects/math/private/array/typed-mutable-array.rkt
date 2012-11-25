#lang typed/racket/base

(require "../unsafe.rkt"
         "array-struct.rkt"
         "utils.rkt"
         "for-each.rkt")

(provide (all-defined-out))

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

(: make-mutable-array (All (A) (In-Indexes (Vectorof A) -> (Mutable-Array A))))
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
