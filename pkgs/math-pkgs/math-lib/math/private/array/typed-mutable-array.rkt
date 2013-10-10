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

(: unsafe-vector->array (All (A) (Indexes (Vectorof A) -> (Mutable-Array A))))
(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc A ds (λ (j v) (unsafe-vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))

(: vector->array (All (A) (case-> ((Vectorof A) -> (Mutable-Array A))
                                  (In-Indexes (Vectorof A) -> (Mutable-Array A)))))
(define vector->array
  (case-lambda
    [(vs)  (unsafe-vector->array ((inst vector Index) (vector-length vs)) vs)]
    [(ds vs)
     (let* ([ds  (check-array-shape
                  ds (λ () (raise-argument-error 'vector->array "(Vectorof Index)" 0 ds vs)))]
            [size  (array-shape-size ds)]
            [n  (vector-length vs)])
       (if (= size n)
           (unsafe-vector->array ds vs)
           (raise-argument-error 'vector->array (format "Vector of length ~e" size) 1 ds vs)))]))

(: mutable-array-copy (All (A) ((Mutable-Array A) -> (Mutable-Array A))))
(define (mutable-array-copy arr)
  (unsafe-vector->array (array-shape arr) (vector-copy-all (mutable-array-data arr))))

;; ===================================================================================================
;; Conversions

(: array->mutable-array (All (A) ((Array A) -> (Mutable-Array A))))
(define (array->mutable-array arr)
  (define ds (array-shape arr))
  (define g (unsafe-array-proc arr))
  (unsafe-vector->array ds (inline-build-array-data ds (λ (js j) (g js)) A)))
