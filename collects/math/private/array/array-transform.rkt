#lang typed/racket/base

(require "../unsafe.rkt"
         "array-struct.rkt"
         "utils.rkt")

(provide array-transform
         unsafe-array-transform
         array-axis-permute
         array-axis-swap
         array-axis-insert
         array-axis-remove
         array-reshape
         array-flatten
         array-append)

;; ===================================================================================================
;; Arbitrary transforms

(: array-transform (All (A) ((Array A) User-Indexes (Indexes -> User-Indexes) -> (View-Array A))))
(define (array-transform arr new-ds idx-fun)
  (let ([arr  (array-view arr)])
    (define old-ds (array-shape arr))
    (define old-f (unsafe-array-proc arr))
    (make-view-array
     new-ds (λ: ([js : Indexes])
              (old-f (check-array-indexes 'array-transform old-ds (idx-fun js)))))))

(: unsafe-array-transform (All (A) ((Array A) Indexes (Indexes -> Indexes) -> (View-Array A))))
(define (unsafe-array-transform arr new-ds idx-fun)
  (let ([arr  (array-view arr)])
    (define old-f (unsafe-array-proc arr))
    (unsafe-view-array new-ds (λ: ([js : Indexes]) (old-f (idx-fun js))))))

;; ===================================================================================================
;; Back permutation and swap

(: array-axis-permute (All (A) ((Array A) (Listof Integer) -> (View-Array A))))
(define (array-axis-permute arr perm)
  (define ds (array-shape arr))
  (let-values ([(ds perm) (apply-permutation
                           perm ds (λ () (raise-type-error 'array-axis-permute "permutation"
                                                           1 arr perm)))])
    (define dims (vector-length ds))
    (define old-js (make-thread-local-indexes dims))
    (unsafe-array-transform
     arr ds
     (λ: ([js : Indexes])
       (let ([old-js  (old-js)])
         (let: loop : Indexes ([i : Nonnegative-Fixnum  0])
           (cond [(i . < . dims)  (unsafe-vector-set! old-js
                                                      (unsafe-vector-ref perm i)
                                                      (unsafe-vector-ref js i))
                                  (loop (+ i 1))]
                 [else  old-js])))))))

(: array-axis-swap (All (A) ((Array A) Integer Integer -> (View-Array A))))
(define (array-axis-swap arr i0 i1)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (i0 . < . 0) (i0 . >= . dims))
         (raise-type-error 'array-transpose (format "Index < ~a" dims) 1 arr i0 i1)]
        [(or (i1 . < . 0) (i1 . >= . dims))
         (raise-type-error 'array-transpose (format "Index < ~a" dims) 2 arr i0 i1)]
        [(= i0 i1)  (array-view arr)]
        [else
         (define new-ds (vector-copy-all ds))
         (define j0 (unsafe-vector-ref new-ds i0))
         (define j1 (unsafe-vector-ref new-ds i1))
         (unsafe-vector-set! new-ds i0 j1)
         (unsafe-vector-set! new-ds i1 j0)
         (let ([arr  (array-view arr)])
           (define proc (unsafe-array-proc arr))
           (unsafe-view-array
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

(: array-axis-insert (All (A) (case-> ((Array A) Integer -> (View-Array A))
                                      ((Array A) Integer Integer -> (View-Array A)))))
(define (array-axis-insert arr k [dk 1])
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . > . dims))
         (raise-type-error 'array-axis-insert (format "Index <= ~a" dims) 1 arr k dk)]
        [(not (index? dk))
         (raise-type-error 'array-axis-insert "Index" 2 arr k dk)]
        [else
         (let ([arr  (array-view arr)])
           (define new-ds (unsafe-vector-insert ds k dk))
           (define proc (unsafe-array-proc arr))
           (unsafe-view-array
            new-ds (λ: ([js : Indexes])
                     (proc (unsafe-vector-remove js k)))))]))

(: array-axis-remove (All (A) ((Array A) Integer Integer -> (View-Array A))))
(define (array-axis-remove arr k jk)
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (cond [(or (k . < . 0) (k . >= . dims))
         (raise-type-error 'array-axis-remove (format "Index < ~a" dims) 1 arr k jk)]
        [(or (jk . < . 0) (jk . >= . (unsafe-vector-ref ds k)))
         (raise-type-error 'array-axis-remove (format "Index < ~a" (unsafe-vector-ref ds k))
                           2 arr k jk)]
        [else
         (let ([arr  (array-view arr)])
           (define new-ds (unsafe-vector-remove ds k))
           (define proc (unsafe-array-proc arr))
           (unsafe-view-array
            new-ds (λ: ([js : Indexes])
                     (proc (unsafe-vector-insert js k jk)))))]))

;; ===================================================================================================
;; Reshape

(: array-reshape (All (A) (case-> ((View-Array A) User-Indexes -> (View-Array A))
                                  ((Strict-Array A) User-Indexes -> (Strict-Array A))
                                  ((Array A) User-Indexes -> (Array A)))))
(define (array-reshape arr ds)
  (let ([ds  (check-array-shape
              ds (λ () (raise-type-error 'array-reshape "(Vectorof Index)" 1 arr ds)))])
    (define size (array-size arr))
    (unless (= size (array-shape-size ds))
      (raise-type-error 'array-reshape (format "(Vectorof Index) with product ~a" size) 1 arr ds))
    (define old-ds (array-shape arr))
    (cond [(equal? ds old-ds)  arr]
          [(view-array? arr)
           (define old-dims (vector-length old-ds))
           (define g (unsafe-array-proc arr))
           (define old-js (make-thread-local-indexes old-dims))
           (unsafe-view-array
            ds (λ: ([js : Indexes])
                 (let ([old-js  (old-js)])
                   (define j (unsafe-array-index->value-index ds js))
                   (unsafe-value-index->array-index! old-ds j old-js)
                   (g old-js))))]
          [else
           (unsafe-strict-array ds (strict-array-data arr))])))

(: array-flatten (All (A) (case-> ((View-Array A) -> (View-Array A))
                                  ((Strict-Array A) -> (Strict-Array A))
                                  ((Array A) -> (Array A)))))
(define (array-flatten arr)
  (define size (array-size arr))
  (define: ds : Indexes (vector size))
  (define old-ds (array-shape arr))
  (cond [(equal? ds old-ds)  arr]
        [(view-array? arr)
         (define old-dims (vector-length old-ds))
         (define g (unsafe-array-proc arr))
         (define old-js (make-thread-local-indexes old-dims))
         (unsafe-view-array
          ds (λ: ([js : Indexes])
               (let ([old-js  (old-js)])
                 (define j (unsafe-vector-ref js 0))
                 (unsafe-value-index->array-index! old-ds j old-js)
                 (g old-js))))]
        [else
         (unsafe-strict-array ds (strict-array-data arr))]))

;; ===================================================================================================
;; Append

(: check-compatible-array-shapes! (Symbol (Listof Indexes) Index -> Void))
(define (check-compatible-array-shapes! name dss k)
  (define (raise-error)
    (error name "expected Arrays with the same shape except axis ~e; given shapes ~e" k dss))
  (cond
    [(or (null? dss) (null? (cdr dss)))  (void)]
    [else
     (let ([ds0  (car dss)]
           [ds1  (cadr dss)]
           [dss  (cddr dss)])
       (unless (apply = (vector-length ds0) (vector-length ds1)
                      (map vector-length dss))
         (raise-error))
       (define dims (vector-length ds0))
       (let loop ([#{i : Nonnegative-Fixnum} 0])
         (cond [(i . >= . dims)  (void)]
               [(or (= i k) (apply = (unsafe-vector-ref ds0 i) (unsafe-vector-ref ds1 i)
                                   (map (λ: ([ds : Indexes])
                                          (unsafe-vector-ref ds i))
                                        dss)))
              (loop (+ i 1))]
             [else  (raise-error)])))]))

(: array-append (All (A) ((Array A) Integer (Array A) * -> (View-Array A))))
(define (array-append arr k . arrs)
  (define dims (array-dims arr))
  (cond
    [(or (k . < . 0) (k . >= . dims))
     (apply raise-type-error 'array-append (format "Index < ~a" dims) 0 k arr arrs)]
    [else
     (let ([arrs  (map (λ: ([arr : (Array A)]) (array-view arr)) (cons arr arrs))])
       (define dss (map (λ: ([arr : (View-Array A)]) (array-shape arr)) arrs))
       (check-compatible-array-shapes! 'array-append dss k)
       (define dks (map (λ: ([ds : Indexes]) (unsafe-vector-ref ds k)) dss))
       (define new-dk (apply + dks))
       (cond
         [(index? new-dk)
          (define new-ds (vector-copy-all (car dss)))
          (unsafe-vector-set! new-ds k new-dk)
          ;; Make two mappings:
          ;; 1. old-procs : new array index -> old array procedure
          ;; 2. old-jks :   new array index -> old array index
          (define old-procs (make-vector new-dk (unsafe-array-proc (car arrs))))
          (define: old-jks : Indexes (make-vector new-dk 0))
          (let arrs-loop ([arrs arrs] [dks  dks] [#{jk : Nonnegative-Fixnum} 0])
            (unless (null? arrs)
              (define arr (car arrs))
              (define proc (unsafe-array-proc arr))
              (define dk (car dks))
              (let i-loop ([#{i : Nonnegative-Fixnum} 0]
                           [#{jk : Nonnegative-Fixnum} jk])
                (cond [(i . < . dk)  (unsafe-vector-set! old-procs jk proc)
                                     (unsafe-vector-set! old-jks jk i)
                                     (i-loop (+ i 1) (unsafe-fx+ jk 1))]
                      [else  (arrs-loop (cdr arrs) (cdr dks) jk)]))))
          
          (unsafe-view-array
           new-ds (λ: ([js : Indexes])
                    (define jk (unsafe-vector-ref js k))
                    (unsafe-vector-set! js k (unsafe-vector-ref old-jks jk))
                    (define v ((unsafe-vector-ref old-procs jk) js))
                    (unsafe-vector-set! js k jk)
                    v))]
         [else
          (error 'array-append "result is too large to be an array")]))]))
