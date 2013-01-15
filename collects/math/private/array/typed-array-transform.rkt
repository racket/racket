#lang typed/racket/base

(require racket/vector
         "../unsafe.rkt"
         "array-struct.rkt"
         "array-broadcast.rkt"
         "utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Arbitrary transforms

(: array-transform (All (A) ((Array A) In-Indexes (Indexes -> In-Indexes) -> (Array A))))
(define (array-transform arr new-ds idx-fun)
  (define old-ds (array-shape arr))
  (define old-f (unsafe-array-proc arr))
  (build-array
   new-ds (λ: ([js : Indexes])
            (old-f (check-array-indexes 'array-transform old-ds (idx-fun js))))))

(: unsafe-array-transform (All (A) ((Array A) Indexes (Indexes -> Indexes) -> (Array A))))
(define (unsafe-array-transform arr new-ds idx-fun)
  (define old-f (unsafe-array-proc arr))
  (unsafe-build-array new-ds (λ: ([js : Indexes]) (old-f (idx-fun js)))))

;; ===================================================================================================
;; Back permutation and swap

(: array-axis-permute (All (A) ((Array A) (Listof Integer) -> (Array A))))
(define (array-axis-permute arr perm)
  (define ds (array-shape arr))
  (let-values ([(ds perm) (apply-permutation
                           perm ds (λ () (raise-argument-error 'array-axis-permute "permutation"
                                                               1 arr perm)))])
    (define dims (vector-length ds))
    (define old-js (make-thread-local-indexes dims))
    (array-default-strict
     (unsafe-array-transform
      arr ds
      (λ: ([js : Indexes])
        (let ([old-js  (old-js)])
          (let: loop : Indexes ([i : Nonnegative-Fixnum  0])
            (cond [(i . < . dims)  (unsafe-vector-set! old-js
                                                       (unsafe-vector-ref perm i)
                                                       (unsafe-vector-ref js i))
                                   (loop (+ i 1))]
                  [else  old-js]))))))))

(: array-axis-swap (All (A) ((Array A) Integer Integer -> (Array A))))
(define (array-axis-swap arr i0 i1)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (i0 . < . 0) (i0 . >= . dims))
         (raise-argument-error 'array-transpose (format "Index < ~a" dims) 1 arr i0 i1)]
        [(or (i1 . < . 0) (i1 . >= . dims))
         (raise-argument-error 'array-transpose (format "Index < ~a" dims) 2 arr i0 i1)]
        [(= i0 i1)  arr]
        [else
         (define new-ds (vector-copy-all ds))
         (define j0 (unsafe-vector-ref new-ds i0))
         (define j1 (unsafe-vector-ref new-ds i1))
         (unsafe-vector-set! new-ds i0 j1)
         (unsafe-vector-set! new-ds i1 j0)
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          (unsafe-build-array
           new-ds (λ: ([js : Indexes])
                    (define j0 (unsafe-vector-ref js i0))
                    (define j1 (unsafe-vector-ref js i1))
                    (unsafe-vector-set! js i0 j1)
                    (unsafe-vector-set! js i1 j0)
                    (define v (proc js))
                    (unsafe-vector-set! js i0 j0)
                    (unsafe-vector-set! js i1 j1)
                    v)))]))

;; ===================================================================================================
;; Adding/removing axes

(: array-axis-insert (All (A) (case-> ((Array A) Integer -> (Array A))
                                      ((Array A) Integer Integer -> (Array A)))))
(define (array-axis-insert arr k [dk 1])
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . > . dims))
         (raise-argument-error 'array-axis-insert (format "Index <= ~a" dims) 1 arr k dk)]
        [(not (index? dk))
         (raise-argument-error 'array-axis-insert "Index" 2 arr k dk)]
        [else
         (define new-ds (unsafe-vector-insert ds k dk))
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          (unsafe-build-array new-ds (λ: ([js : Indexes]) (proc (unsafe-vector-remove js k)))))]))

(: array-axis-ref (All (A) ((Array A) Integer Integer -> (Array A))))
(define (array-axis-ref arr k jk)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . >= . dims))
         (raise-argument-error 'array-axis-ref (format "Index < ~a" dims) 1 arr k jk)]
        [(or (jk . < . 0) (jk . >= . (unsafe-vector-ref ds k)))
         (raise-argument-error 'array-axis-ref (format "Index < ~a" (unsafe-vector-ref ds k))
                           2 arr k jk)]
        [else
         (define new-ds (unsafe-vector-remove ds k))
         (define proc (unsafe-array-proc arr))
         (array-default-strict
          (unsafe-build-array new-ds (λ: ([js : Indexes]) (proc (unsafe-vector-insert js k jk)))))]))

;; ===================================================================================================
;; Reshape

(: array-reshape (All (A) ((Array A) In-Indexes -> (Array A))))
(define (array-reshape arr ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'array-reshape "(Vectorof Index)" 1 arr ds)))])
    (define size (array-size arr))
    (unless (= size (array-shape-size ds))
      (raise-argument-error 'array-reshape (format "(Vectorof Index) with product ~a" size) 1 arr ds))
    (define old-ds (array-shape arr))
    (cond [(equal? ds old-ds)  arr]
          [else
           (define old-dims (vector-length old-ds))
           (define g (unsafe-array-proc arr))
           (define old-js (make-thread-local-indexes old-dims))
           (array-default-strict
            (unsafe-build-array
             ds (λ: ([js : Indexes])
                  (let ([old-js  (old-js)])
                    (define j (unsafe-array-index->value-index ds js))
                    (unsafe-value-index->array-index! old-ds j old-js)
                    (g old-js)))))])))

(: array-flatten (All (A) ((Array A) -> (Array A))))
(define (array-flatten arr)
  (define size (array-size arr))
  (define: ds : Indexes (vector size))
  (define old-ds (array-shape arr))
  (cond [(equal? ds old-ds)  arr]
        [else
         (define old-dims (vector-length old-ds))
         (define g (unsafe-array-proc arr))
         (define old-js (make-thread-local-indexes old-dims))
         (array-default-strict
          (unsafe-build-array
           ds (λ: ([js : Indexes])
                (let ([old-js  (old-js)])
                  (define j (unsafe-vector-ref js 0))
                  (unsafe-value-index->array-index! old-ds j old-js)
                  (g old-js)))))]))

;; ===================================================================================================
;; Append

(: array-broadcast-for-append (All (A) ((Listof (Array A))
                                        Integer -> (Values (Listof (Array A))
                                                           (Listof Index)))))
(define (array-broadcast-for-append arrs k)
  (define dss (map (λ: ([arr : (Array A)]) (array-shape arr)) arrs))
  (define dims (apply max (map vector-length dss)))
  (cond [(not (index? dims))  (error 'array-broadcast-for-append "can't happen")]
        [(or (k . < . 0) (k . >= . dims))
         (raise-argument-error 'array-append* (format "Index < ~a" dims) k)]
        [else
         (let* ([dss  (map (λ: ([ds : Indexes])
                             (define dms (vector-length ds))
                             (vector-append ((inst make-vector Index) (- dims dms) 1) ds))
                           dss)]
                [dks  (map (λ: ([ds : Indexes]) (unsafe-vector-ref ds k)) dss)]
                [dss  (map (λ: ([ds : Indexes]) (unsafe-vector-remove ds k)) dss)]
                [ds   (array-shape-broadcast dss)]
                [dss  (map (λ: ([dk : Index]) (unsafe-vector-insert ds k dk)) dks)])
           (define new-arrs
             (map (λ: ([arr : (Array A)] [ds : Indexes]) (array-broadcast arr ds)) arrs dss))
           (values new-arrs dks))]))

(: array-append* (All (A) (case-> ((Listof (Array A)) -> (Array A))
                                  ((Listof (Array A)) Integer -> (Array A)))))
(define (array-append* arrs [k 0])
  (when (null? arrs) (raise-argument-error 'array-append* "nonempty (Listof (Array A))" arrs))
  (let-values ([(arrs dks)  (array-broadcast-for-append arrs k)])
    (define new-dk (apply + dks))
    (cond
      [(not (index? new-dk))  (error 'array-append* "resulting axis is too large (not an Index)")]
      [else
       (define dss (map (λ: ([arr : (Array A)]) (array-shape arr)) arrs))
       (define new-ds (vector-copy-all (car dss)))
       (unsafe-vector-set! new-ds k new-dk)
       ;; Make two mappings:
       ;; 1. old-procs : new array index -> old array procedure
       ;; 2. old-jks :   new array index -> old array index
       (define old-procs (make-vector new-dk (unsafe-array-proc (car arrs))))
       (define: old-jks : Indexes (make-vector new-dk 0))
       (let arrs-loop ([arrs arrs] [dks dks] [#{jk : Nonnegative-Fixnum} 0])
         (unless (null? arrs)
           (define arr (car arrs))
           (define proc (unsafe-array-proc arr))
           (define dk (car dks))
           (let i-loop ([#{i : Nonnegative-Fixnum} 0] [#{jk : Nonnegative-Fixnum} jk])
             (cond [(i . < . dk)  (unsafe-vector-set! old-procs jk proc)
                                  (unsafe-vector-set! old-jks jk i)
                                  (i-loop (+ i 1) (unsafe-fx+ jk 1))]
                   [else  (arrs-loop (cdr arrs) (cdr dks) jk)]))))
       (array-default-strict
        (unsafe-build-array
         new-ds (λ: ([js : Indexes])
                  (define jk (unsafe-vector-ref js k))
                  (unsafe-vector-set! js k (unsafe-vector-ref old-jks jk))
                  (define v ((unsafe-vector-ref old-procs jk) js))
                  (unsafe-vector-set! js k jk)
                  v)))])))
