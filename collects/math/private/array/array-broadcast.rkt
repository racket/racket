#lang typed/racket

(require racket/fixnum
         "array-struct.rkt"
         "../unsafe.rkt"
         "utils.rkt")

(provide array-broadcasting
         shape-broadcast
         shape-broadcast*
         array-broadcast-to-shape
         array-broadcast)

(: array-broadcasting (Parameterof (U #f #t 'permissive)))
(define array-broadcasting (make-parameter #t))

(: shift-stretch-axes (All (A) ((Array A) Indexes -> (View-Array A))))
(define (shift-stretch-axes arr new-ds)
  (let ([arr  (array-view arr)])
    (define old-ds (array-shape arr))
    (define old-dims (vector-length old-ds))
    (define new-dims (vector-length new-ds))
    (define shift (- new-dims old-dims))
    (define old-js (make-thread-local-indexes old-dims))
    (define old-f (unsafe-array-proc arr))
    (with-asserts ([shift index?])
      (unsafe-view-array
       new-ds
       (λ: ([new-js : Indexes])
         (let ([old-js  (old-js)])
           (let: loop : A ([k : Nonnegative-Fixnum  0])
             (cond [(k . < . old-dims)
                    (define new-jk (unsafe-vector-ref new-js (+ k shift)))
                    (define old-dk (unsafe-vector-ref old-ds k))
                    (define old-jk (unsafe-fxmodulo new-jk old-dk))
                    (unsafe-vector-set! old-js k old-jk)
                    (loop (+ k 1))]
                   [else  (old-f old-js)]))))))))

(: shape-insert-axes (Indexes Fixnum -> Indexes))
(define (shape-insert-axes ds n)
  (vector-append ((inst make-vector Index) n 1) ds))

(: shape-permissive-broadcast (Indexes Indexes Index (-> Nothing) -> Indexes))
(define (shape-permissive-broadcast ds1 ds2 dims fail)
  (define: new-ds : Indexes (make-vector dims 0))
  (let loop ([#{k : Nonnegative-Fixnum} 0])
    (cond [(k . < . dims)
           (define dk1 (unsafe-vector-ref ds1 k))
           (define dk2 (unsafe-vector-ref ds2 k))
           (unsafe-vector-set!
            new-ds k
            (cond [(or (= dk1 0) (= dk2 0))  (fail)]
                  [else  (fxmax dk1 dk2)]))
           (loop (+ k 1))]
          [else  new-ds])))

(: shape-normal-broadcast (Indexes Indexes Index (-> Nothing) -> Indexes))
(define (shape-normal-broadcast ds1 ds2 dims fail)
  (define: new-ds : Indexes (make-vector dims 0))
  (let loop ([#{k : Nonnegative-Fixnum} 0])
    (cond [(k . < . dims)
           (define dk1 (unsafe-vector-ref ds1 k))
           (define dk2 (unsafe-vector-ref ds2 k))
           (unsafe-vector-set!
            new-ds k
            (cond [(= dk1 dk2)  dk1]
                  [(and (= dk1 1) (dk2 . > . 0))  dk2]
                  [(and (= dk2 1) (dk1 . > . 0))  dk1]
                  [else  (fail)]))
           (loop (+ k 1))]
          [else  new-ds])))

(: shape-broadcast (case-> (Indexes Indexes (-> Nothing) -> Indexes)
                           (Indexes Indexes (-> Nothing) (U #f #t 'permissive) -> Indexes)))
(define (shape-broadcast ds1 ds2 fail [broadcasting (array-broadcasting)])
  (define dims1 (vector-length ds1))
  (define dims2 (vector-length ds2))
  (define n (- dims2 dims1))
  (let-values ([(ds1 ds2 dims)  (cond [(n . > . 0)  (values (shape-insert-axes ds1 n) ds2 dims2)]
                                      [(n . < . 0)  (values ds1 (shape-insert-axes ds2 (- n)) dims1)]
                                      [else         (values ds1 ds2 dims1)])])
    (if (eq? broadcasting 'permissive)
        (shape-permissive-broadcast ds1 ds2 dims fail)
        (shape-normal-broadcast ds1 ds2 dims fail))))

(: shape-broadcast* (case-> ((Listof Indexes) -> Indexes)
                            ((Listof Indexes) (U #f #t 'permissive) -> Indexes)))
(define (shape-broadcast* dss [broadcasting (array-broadcasting)])
  (define (fail)
    (error 'array-broadcast "incompatible array shapes: ~a"
           (string-join (map (λ (ds) (format "~e" ds)) dss) ", ")))
  (cond [(empty? dss)  #()]
        [else
         (for/fold ([new-ds  (first dss)]) ([ds  (in-list (rest dss))])
           (shape-broadcast new-ds ds fail broadcasting))]))

(: array-broadcast-to-shape (All (A) ((Array A) Indexes -> (Array A))))
(define (array-broadcast-to-shape arr ds)
  (if (equal? ds (array-shape arr)) arr (shift-stretch-axes arr ds)))

(: array-broadcast
   (All (A B) (case-> ((Array A) (Array B) -> (Values (Array A) (Array B)))
                      ((Array A) (Array B) (U #f #t 'permissive) -> (Values (Array A) (Array B))))))
(define (array-broadcast arr1 arr2 [broadcasting (array-broadcasting)])
  (define ds1 (array-shape arr1))
  (define ds2 (array-shape arr2))
  (cond [(equal? ds1 ds2)  (values arr1 arr2)]
        [broadcasting
         (define (fail) (error 'array-broadcast "incompatible array shapes: ~e and ~e" ds1 ds2))
         (define new-ds (shape-broadcast ds1 ds2 fail broadcasting))
         (values (array-broadcast-to-shape arr1 new-ds)
                 (array-broadcast-to-shape arr2 new-ds))]
        [else
         (define (fail)
           (error 'array-broadcast "incompatible array shapes (with broadcasting): ~e and ~e"
                  ds1 ds2))
         ;; Possibly generate an error from normal broadcasting
         (define _ (shape-broadcast ds1 ds2 fail #t))
         ;; If not, we know normal broadcasting would have handled it, so say so
         (error 'array-broadcast "incompatible array shapes (without broadcasting): ~e and ~e"
                ds1 ds2)]))
