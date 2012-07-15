#lang typed/racket/base

(require racket/unsafe/ops
         racket/sequence
         racket/vector
         "array-struct.rkt"
         "utils.rkt")

(provide array-transform
         unsafe-array-transform
         array-axis-transform
         array-slice
         array-axis-permute
         array-axis-swap
         array-axis-insert
         array-axis-remove
         array-reshape
         array-flatten)

;; ===================================================================================================
;; Arbitrary transforms

(: array-transform (All (A) ((Array A) (Listof Integer) ((Listof Index) -> (Listof Integer))
                                       -> (lazy-array A))))
(define (array-transform arr new-ds idx-fun)
  (let ([arr  (array-lazy arr)])
    (define old-ds (unsafe-array-shape arr))
    (define old-f (unsafe-array-proc arr))
    (make-lazy-array new-ds (λ: ([js : (Listof Index)])
                              (old-f (check-array-indexes 'array-transform old-ds (idx-fun js)))))))

(: unsafe-array-transform (All (A) ((Array A) (Vectorof Index) ((Vectorof Index) -> (Vectorof Index))
                                              -> (lazy-array A))))
(define (unsafe-array-transform arr new-ds idx-fun)
  (let ([arr  (array-lazy arr)])
    (define old-f (unsafe-array-proc arr))
    (unsafe-lazy-array new-ds (λ: ([js : (Vectorof Index)]) (old-f (idx-fun js))))))

;; ===================================================================================================
;; Separable (per-axis) transforms

(: array-axis-transform (All (A) ((Array A) (Listof (Listof Integer)) -> (lazy-array A))))
(define (array-axis-transform arr old-jss)
  (define old-ds (unsafe-array-shape arr))
  (define dims (vector-length old-ds))
  ;; number of indexes should match
  (unless (= dims (length old-jss))
    (error 'array-separable-transform
           "expected ~e index vectors; given ~e index vectors in ~e"
           dims (length old-jss) old-jss))
  ;; check bounds, reconstruct indexes as vectors
  (define: old-jss* : (Vectorof (Vectorof Index)) (make-vector dims (vector)))
  (let i-loop ([old-jss old-jss] [#{i : Nonnegative-Fixnum} 0])
    (when (i . < . dims)
      (define old-js (unsafe-car old-jss))
      (define new-di (length old-js))
      (define old-di (unsafe-vector-ref old-ds i))
      (define: old-js* : (Vectorof Index) (make-vector new-di 0))
      (let k-loop ([old-js old-js] [#{k : Nonnegative-Fixnum} 0])
        (cond [(k . < . new-di)
               (define old-jk (unsafe-car old-js))
               (cond [(and (0 . <= . old-jk) (old-jk . < . old-di))
                      (unsafe-vector-set! old-js* k old-jk)
                      (k-loop (unsafe-cdr old-js) (+ k 1))]
                     [else
                      (error 'array-separable-transform "out of bounds")])]
              [else
               (unsafe-vector-set! old-jss* i old-js*)]))
      (i-loop (unsafe-cdr old-jss) (+ i 1))))
  
  (define: new-ds : (Vectorof Index) (vector-map vector-length old-jss*))
  (case dims
    [(0)  (array-lazy arr)]
    [(1)  (define g (unsafe-array-proc (array-lazy arr)))
          (unsafe-lazy-array
           new-ds
           (λ: ([js : (Vectorof Index)])
             (define j0 (unsafe-vector-ref js 0))
             (unsafe-vector-set! js 0 (unsafe-vector-ref (unsafe-vector-ref old-jss* 0) j0))
             (define v (g js))
             (unsafe-vector-set! js 0 j0)
             v))]
    [(2)  (define g (unsafe-array-proc (array-lazy arr)))
          (unsafe-lazy-array
           new-ds
           (λ: ([js : (Vectorof Index)])
             (define j0 (unsafe-vector-ref js 0))
             (define j1 (unsafe-vector-ref js 1))
             (unsafe-vector-set! js 0 (unsafe-vector-ref (unsafe-vector-ref old-jss* 0) j0))
             (unsafe-vector-set! js 1 (unsafe-vector-ref (unsafe-vector-ref old-jss* 1) j1))
             (define v (g js))
             (unsafe-vector-set! js 0 j0)
             (unsafe-vector-set! js 1 j1)
             v))]
    [(3)  (define g (unsafe-array-proc (array-lazy arr)))
          (unsafe-lazy-array
           new-ds
           (λ: ([js : (Vectorof Index)])
             (define j0 (unsafe-vector-ref js 0))
             (define j1 (unsafe-vector-ref js 1))
             (define j2 (unsafe-vector-ref js 2))
             (unsafe-vector-set! js 0 (unsafe-vector-ref (unsafe-vector-ref old-jss* 0) j0))
             (unsafe-vector-set! js 1 (unsafe-vector-ref (unsafe-vector-ref old-jss* 1) j1))
             (unsafe-vector-set! js 2 (unsafe-vector-ref (unsafe-vector-ref old-jss* 2) j2))
             (define v (g js))
             (unsafe-vector-set! js 0 j0)
             (unsafe-vector-set! js 1 j1)
             (unsafe-vector-set! js 2 j2)
             v))]
    [else
     (unsafe-array-transform
      arr new-ds
      (λ: ([new-js : (Vectorof Index)])
        (define: old-js : (Vectorof Index) (make-vector dims 0))
        (let: loop : (Vectorof Index) ([i : Nonnegative-Fixnum  0])
          (cond [(i . < . dims)
                 (define new-ji (unsafe-vector-ref new-js i))
                 (define old-ji (unsafe-vector-ref (unsafe-vector-ref old-jss* i) new-ji))
                 (unsafe-vector-set! old-js i old-ji)
                 (loop (+ i 1))]
                [else  old-js]))))]))

(: array-slice (All (A) ((Array A) (Listof (Sequenceof Integer)) -> (lazy-array A))))
(define (array-slice arr ss)
  (array-axis-transform arr (map (inst sequence->list Integer) ss)))

;; ===================================================================================================
;; Back permutation and swap

(: array-axis-permute (All (A) ((Array A) (Listof Integer) -> (lazy-array A))))
(define (array-axis-permute arr perm)
  (define ds (unsafe-array-shape arr))
  (let-values ([(ds perm) (apply-permutation
                           perm ds (λ () (raise-type-error 'array-permute "permutation"
                                                           1 arr perm)))])
    (define dims (vector-length ds))
    (define old-js
      (make-delayed-thread-cell (λ () (ann (make-vector dims 0) (Vectorof Index)))))
    
    (unsafe-array-transform
     arr ds
     (λ: ([js : (Vectorof Index)])
       (let ([old-js  (old-js)])
         (let: loop : (Vectorof Index) ([i : Nonnegative-Fixnum  0])
           (cond [(i . < . dims)  (unsafe-vector-set! old-js
                                                      (unsafe-vector-ref perm i)
                                                      (unsafe-vector-ref js i))
                                  (loop (+ i 1))]
                 [else  old-js])))))))

(: array-axis-swap (All (A) ((Array A) Integer Integer -> (lazy-array A))))
(define (array-axis-swap arr i0 i1)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (i0 . < . 0) (i0 . >= . dims))
         (raise-type-error 'array-transpose (format "Index < ~a" dims) 1 arr i0 i1)]
        [(or (i1 . < . 0) (i1 . >= . dims))
         (raise-type-error 'array-transpose (format "Index < ~a" dims) 2 arr i0 i1)]
        [else
         (define new-ds (vector-copy-all ds))
         (define j0 (unsafe-vector-ref new-ds i0))
         (define j1 (unsafe-vector-ref new-ds i1))
         (unsafe-vector-set! new-ds i0 j1)
         (unsafe-vector-set! new-ds i1 j0)
         (let ([arr  (array-lazy arr)])
           (define proc (unsafe-array-proc arr))
           (unsafe-lazy-array
            new-ds (λ: ([js : (Vectorof Index)])
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

(: array-axis-insert (All (A) ((Array A) Integer Integer -> (lazy-array A))))
(define (array-axis-insert arr k dk)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . > . dims))
         (raise-type-error 'array-axis-insert (format "Index <= ~a" dims) 1 arr k dk)]
        [(not (index? dk))
         (raise-type-error 'array-axis-insert "Index" 2 arr k dk)]
        [else
         (let ([arr  (array-lazy arr)])
           (define new-ds (unsafe-vector-insert ds k dk))
           (define proc (unsafe-array-proc arr))
           (unsafe-lazy-array
            new-ds (λ: ([js : (Vectorof Index)])
                     (proc (unsafe-vector-remove js k)))))]))

(: array-axis-remove (All (A) ((Array A) Integer Integer -> (lazy-array A))))
(define (array-axis-remove arr k jk)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . >= . dims))
         (raise-type-error 'array-axis-remove (format "Index < ~a" dims) 1 arr k jk)]
        [(or (jk . < . 0) (jk . >= . (unsafe-vector-ref ds k)))
         (raise-type-error 'array-axis-remove (format "Index < ~a" (unsafe-vector-ref ds k))
                           2 arr k jk)]
        [else
         (let ([arr  (array-lazy arr)])
           (define new-ds (unsafe-vector-remove ds k))
           (define proc (unsafe-array-proc arr))
           (unsafe-lazy-array
            new-ds (λ: ([js : (Vectorof Index)])
                     (proc (unsafe-vector-insert js k jk)))))]))

;; ===================================================================================================
;; Reshape

(: array-reshape (All (A) (case-> ((lazy-array A) (Listof Integer) -> (lazy-array A))
                                  ((strict-array A) (Listof Integer) -> (strict-array A))
                                  ((Array A) (Listof Integer) -> (Array A)))))
(define (array-reshape arr ds)
  (let ([ds  (array-shape-safe->unsafe
              ds (λ () (raise-type-error 'array-reshape "(Listof Index)" 1 arr ds)))])
    (define size (array-size arr))
    (unless (= size (unsafe-array-shape-size ds))
      (raise-type-error 'array-reshape (format "(Listof Index) with product ~a" size) 1 arr ds))
    (define old-ds (unsafe-array-shape arr))
    (cond [(equal? ds old-ds)  arr]
          [(lazy-array? arr)
           (define old-dims (vector-length old-ds))
           (define g (unsafe-array-proc arr))
           (define old-js
             (make-delayed-thread-cell (λ () (ann (make-vector old-dims 0) (Vectorof Index)))))
           (unsafe-lazy-array
            ds (λ: ([js : (Vectorof Index)])
                 (let ([old-js  (old-js)])
                   (define j (unsafe-array-index->value-index ds js))
                   (unsafe-value-index->array-index! old-ds j old-js)
                   (g old-js))))]
          [else
           (unsafe-strict-array ds (unsafe-array-data arr))])))

(: array-flatten (All (A) (case-> ((lazy-array A) -> (lazy-array A))
                                  ((strict-array A) -> (strict-array A))
                                  ((Array A) -> (Array A)))))
(define (array-flatten arr)
  (define size (array-size arr))
  (define: ds : (Vectorof Index) (vector size))
  (define old-ds (unsafe-array-shape arr))
  (cond [(equal? ds old-ds)  arr]
        [(lazy-array? arr)
         (define old-dims (vector-length old-ds))
         (define g (unsafe-array-proc arr))
         (define old-js
           (make-delayed-thread-cell (λ () (ann (make-vector old-dims 0) (Vectorof Index)))))
         (unsafe-lazy-array
          ds (λ: ([js : (Vectorof Index)])
               (let ([old-js  (old-js)])
                 (define j (unsafe-vector-ref js 0))
                 (unsafe-value-index->array-index! old-ds j old-js)
                 (g old-js))))]
        [else
         (unsafe-strict-array ds (unsafe-array-data arr))]))
