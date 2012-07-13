#lang typed/racket/base

(require racket/unsafe/ops
         racket/sequence
         racket/vector
         "array-struct.rkt"
         "utils.rkt")

(provide array-transform
         unsafe-array-transform
         (rename-out [make-new-axis new-axis])
         array-permute
         array-separable-transform
         array-slice
         array-transpose)

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

(: array-separable-transform (All (A) ((Array A) (Listof (Listof Integer)) -> (lazy-array A))))
(define (array-separable-transform arr old-jss)
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
  (array-separable-transform arr (map (inst sequence->list Integer) ss)))

;; ===================================================================================================
;; Back permutation

(struct: new-axis ([length : Index]) #:transparent)

(: make-new-axis (Integer -> new-axis))
(define (make-new-axis len)
  (cond [(index? len)  (new-axis len)]
        [else  (raise-type-error 'new-axis "Index" len)]))

(: array-permute (All (A) ((Array A) (Listof (U Integer new-axis)) -> (lazy-array A))))
(define (array-permute arr orig-axes)
  (define ds (unsafe-array-shape arr))
  (define dims (vector-length ds))
  
  (define new-dims (length orig-axes))
  (define: perm : (Vectorof (U Index #f)) (make-vector new-dims #f))
  (define: new-ds : (Vectorof Index) (make-vector new-dims 0))
  (let loop ([axes orig-axes] [#{i : Nonnegative-Fixnum} 0])
    (when (i . < . new-dims)
      (define k (unsafe-car axes))
      (cond [(new-axis? k)  (unsafe-vector-set! new-ds i (new-axis-length k))]
            [(and (0 . <= . k) (k . < . dims))
             (unsafe-vector-set! new-ds i (unsafe-vector-ref ds k))
             (unsafe-vector-set! perm i k)]
            [else
             (error 'array-permute "nonexistent source axis ~e; given ~e" k orig-axes)])
      (loop (unsafe-cdr axes) (+ i 1))))
  
  (unsafe-array-transform
   arr new-ds
   (λ: ([js : (Vectorof Index)])
     (define: old-js : (Vectorof Index) (make-vector dims 0))
     (let: loop : (Vectorof Index) ([i : Nonnegative-Fixnum  0])
       (cond [(i . < . new-dims)
              (define k (unsafe-vector-ref perm i))
              (when k (unsafe-vector-set! old-js k (unsafe-vector-ref js i)))
              (loop (+ i 1))]
             [else  old-js])))))

;; ===================================================================================================
;; Transpose

(: array-transpose (All (A) (case-> ((Array A) -> (lazy-array A))
                                    ((Array A) Integer Integer -> (lazy-array A)))))
(define array-transpose
  (case-lambda
    [(arr)
     (define dims (array-dims arr))
     (case dims
       [(0)  (array-lazy arr)]
       [(1)  (array-lazy arr)]
       [(2)  (array-transpose arr 0 1)]
       [else  (error 'array-transpose
                     "cannot automatically determine axes for ~e-dimensional Array" dims)])]
    [(arr i0 i1)
     (define ds (unsafe-array-shape arr))
     (define dims (vector-length ds))
     (cond [(or (i0 . < . 0) (i0 . >= . dims))
            (raise-type-error 'array-transpose (format "Index less than ~a" dims) 1 arr i0 i1)]
           [(or (i1 . < . 0) (i1 . >= . dims))
            (raise-type-error 'array-transpose (format "Index less than ~a" dims) 2 arr i0 i1)]
           [(= i0 i1)  (array-lazy arr)]
           [else
            (define new-ds (vector-copy-all ds))
            (unsafe-vector-set! new-ds i0 (unsafe-vector-ref ds i1))
            (unsafe-vector-set! new-ds i1 (unsafe-vector-ref ds i0))
            ;; The following code, which mutates indexes in-place, is currently about 2x faster than
            ;; the commented out code following it, which creates new indexes
            (let ([arr  (array-lazy arr)])
              (define g (unsafe-array-proc arr))
              (unsafe-lazy-array
               new-ds (λ: ([js : (Vectorof Index)])
                        (define j0 (unsafe-vector-ref js i0))
                        (define j1 (unsafe-vector-ref js i1))
                        ;; Swap indexes
                        (unsafe-vector-set! js i0 j1)
                        (unsafe-vector-set! js i1 j0)
                        ;; Get the value at these indexes
                        (define v (g js))
                        ;; Swap indexes back
                        (unsafe-vector-set! js i0 j0)
                        (unsafe-vector-set! js i1 j1)
                        v)))
            #;; Leaving this code around in case it becomes faster in the future
            (unsafe-array-transform
             arr new-ds (λ: ([js : (Vectorof Index)])
                          (define new-js (vector-copy-all js))
                          (unsafe-vector-set! new-js i0 (unsafe-vector-ref js i1))
                          (unsafe-vector-set! new-js i1 (unsafe-vector-ref js i0))
                          new-js))])]))
