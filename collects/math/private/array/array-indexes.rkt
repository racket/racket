#lang typed/racket/base

(require "array-struct.rkt"
         "array-constructors.rkt"
         "array-ref.rkt"
         "array-syntax.rkt"
         "array-slice.rkt"
         "utils.rkt"
         "for-each.rkt")

(provide array-indexes-ref
         array-indexes-set!
         slice-indexes-array)

(: array-indexes-ref (All (A) ((Array A) (Array User-Indexes) -> (View-Array A))))
(define (array-indexes-ref arr idxs)
  (let ([arr   (array-view arr)]
        [idxs  (array-view idxs)])
    (define ds (array-shape idxs))
    (define idxs-proc (unsafe-array-proc idxs))
    (unsafe-view-array
     ds (λ: ([js : Indexes])
          (array-ref arr (idxs-proc js))))))

(: array-indexes-set! (All (A) ((Strict-Array A) (Array User-Indexes) (Array A) -> Void)))
(define (array-indexes-set! arr idxs vals)
  (let ([idxs  (array-view idxs)]
        [vals  (array-view vals)])
    (define ds (array-shape idxs))
    (check-equal-array-shape! 'array-indexes-set! ds (array-shape vals))
    (define idxs-proc (unsafe-array-proc idxs))
    (define vals-proc (unsafe-array-proc vals))
    (for-each-array-index ds (λ (js) (array-set! arr (idxs-proc js) (vals-proc js))))))

(: slice-indexes-array (User-Indexes (Listof Slice-Spec) -> (View-Array Indexes)))
(define (slice-indexes-array ds slices)
  (array-slice-ref (indexes-array ds) slices))
