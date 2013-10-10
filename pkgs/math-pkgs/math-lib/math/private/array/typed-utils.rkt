#lang typed/racket/base

(require racket/fixnum
         racket/list
         racket/performance-hint
         (for-syntax racket/base)
         "../unsafe.rkt")

(provide (all-defined-out))

(define-type (Listof* A) (Rec T (U A (Listof T))))
(define-type (Vectorof* A) (Rec T (U A (Vectorof T))))

(define-type Indexes (Vectorof Index))
(define-type In-Indexes (U (Vectorof Integer) Indexes))

(begin-encourage-inline
  
  (: vector->supertype-vector (All (A B) ((Vectorof A) -> (Vectorof (U A B)))))
  (define (vector->supertype-vector js)
    (define dims (vector-length js))
    (cond [(= dims 0)  (vector)]
          [else  (define: new-js : (Vectorof (U A B)) (make-vector dims (unsafe-vector-ref js 0)))
                 (let loop ([#{i : Nonnegative-Fixnum} 1])
                   (cond [(i . < . dims)  (unsafe-vector-set! new-js i (unsafe-vector-ref js i))
                                          (loop (+ i 1))]
                         [else  new-js]))]))
  
  (: vector-copy-all (All (A) ((Vectorof A) -> (Vectorof A))))
  (define (vector-copy-all js) ((inst vector->supertype-vector A A) js))
  
  (: array-shape-size (Indexes -> Natural))
  (define (array-shape-size ds)
    (define dims (vector-length ds))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{n : Natural} 1])
      (cond [(i . < . dims)  (define d (unsafe-vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
  
  (: check-array-shape-size (Symbol Indexes -> Index))
  (define (check-array-shape-size name ds)
    (define size (array-shape-size ds))
    (cond [(index? size)  size]
          [else  (error name "array size ~e (for shape ~e) is too large (is not an Index)" size ds)]))
  
  (: check-array-shape (In-Indexes (-> Nothing) -> Indexes))
  (define (check-array-shape ds fail)
    (define dims (vector-length ds))
    (define: new-ds : Indexes (make-vector dims 0))
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (unsafe-vector-ref ds i))
             (cond [(index? di)  (unsafe-vector-set! new-ds i di)
                                 (loop (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))
  
  (: unsafe-array-index->value-index (Indexes Indexes -> Nonnegative-Fixnum))
  (define (unsafe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{j : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (unsafe-vector-ref ds i))
             (define ji (unsafe-vector-ref js i))
             (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
            [else  j])))
  
  (: unsafe-value-index->array-index! (Indexes Nonnegative-Fixnum Indexes -> Void))
  (define (unsafe-value-index->array-index! ds j js)
    (with-asserts ([j index?])
      (define dims (vector-length ds))
      (let: loop : Index ([i : Nonnegative-Fixnum  dims] [s : Nonnegative-Fixnum  1])
        (cond [(zero? i)  j]
              [else  (let* ([i  (- i 1)]
                            [j  (loop i (unsafe-fx* s (unsafe-vector-ref ds i)))])
                       (unsafe-vector-set! js i (fxquotient j s))
                       (unsafe-fxmodulo j s))]))
      (void)))
  
  )  ; begin-encourage-inline

;; Using this instead of literal #() is currently slightly faster (about 18% on my machine)
(define: empty-vectorof-index : Indexes
  #())

(: raise-array-index-error (Symbol Indexes In-Indexes -> Nothing))
(define (raise-array-index-error name ds js)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))

(: array-index->value-index (Symbol Indexes In-Indexes -> Nonnegative-Fixnum))
(define (array-index->value-index name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{j : Nonnegative-Fixnum}  0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (define ji (unsafe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))

(: check-array-indexes (Symbol Indexes In-Indexes -> Indexes))
(define (check-array-indexes name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (vector-length js)) (raise-index-error))
  (define: new-js : Indexes (make-vector dims 0))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (define ji (unsafe-vector-ref js i))
           (cond [(and (exact-integer? ji) (0 . <= . ji) (ji . < . di))
                  (unsafe-vector-set! new-js i ji)
                  (loop (+ i 1))]
                 [else  (raise-index-error)])]
          [else  new-js])))

(: unsafe-vector-remove (All (I) ((Vectorof I) Index -> (Vectorof I))))
(define (unsafe-vector-remove vec k)
  (define n (vector-length vec))
  (define n-1 (sub1 n))
  (cond
    [(not (index? n-1)) (error 'unsafe-vector-remove "internal error")]
    [else
     (define: new-vec : (Vectorof I) (make-vector n-1 (unsafe-vector-ref vec 0)))
     (let loop ([#{i : Nonnegative-Fixnum} 0])
       (when (i . < . k)
         (unsafe-vector-set! new-vec i (unsafe-vector-ref vec i))
         (loop (+ i 1))))
     (let loop ([#{i : Nonnegative-Fixnum} k])
       (cond [(i . < . n-1)
              (unsafe-vector-set! new-vec i (unsafe-vector-ref vec (+ i 1)))
              (loop (+ i 1))]
             [else  new-vec]))]))

(: unsafe-vector-insert (All (I) ((Vectorof I) Index I -> (Vectorof I))))
(define (unsafe-vector-insert vec k v)
  (define n (vector-length vec))
  (define: dst-vec : (Vectorof I) (make-vector (+ n 1) v))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . k)
      (unsafe-vector-set! dst-vec i (unsafe-vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([#{i : Nonnegative-Fixnum} k])
    (when (i . < . n)
      (let ([i+1  (+ i 1)])
        (unsafe-vector-set! dst-vec i+1 (unsafe-vector-ref vec i))
        (loop i+1))))
  dst-vec)

(: port-next-column (Output-Port -> Natural))
;; Helper to avoid the annoying #f column value
(define (port-next-column port)
  (define-values (_line col _pos) (port-next-location port))
  (if col col 0))

(: apply-permutation (All (A) ((Listof Integer) Indexes (-> Nothing) -> (Values Indexes Indexes))))
(define (apply-permutation perm ds fail)
  (define dims (vector-length ds))
  (unless (= dims (length perm)) (fail))
  (define: visited  : (Vectorof Boolean) (make-vector dims #f))
  (define: new-perm : (Vectorof Index) (make-vector dims 0))
  (define: new-ds   : Indexes (make-vector dims 0))
  ;; This loop fails if it writes to a `visited' element twice, or an element of perm is not an
  ;; Index < dims
  (let loop ([perm perm] [#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . dims)
           (define k (unsafe-car perm))
           (cond [(and (0 . <= . k) (k . < . dims))
                  (cond [(unsafe-vector-ref visited k)  (fail)]
                        [else  (unsafe-vector-set! visited k #t)])
                  (unsafe-vector-set! new-ds i (unsafe-vector-ref ds k))
                  (unsafe-vector-set! new-perm i k)]
                 [else  (fail)])
           (loop (unsafe-cdr perm) (+ i 1))]
          [else  (values new-ds new-perm)])))

(: make-thread-local-indexes (Integer -> (-> Indexes)))
(define (make-thread-local-indexes dims)
  (let: ([val : (Thread-Cellof (U #f Indexes)) (make-thread-cell #f)])
    (λ () (or (thread-cell-ref val)
              (let: ([v : Indexes  (make-vector dims 0)])
                (thread-cell-set! val v)
                v)))))

(: all-equal? (Any Any * -> Boolean))
(define (all-equal? x . xs)
  (cond [(empty? xs)  #t]
        [else  (define first-xs (first xs))
               (cond [(equal? x first-xs)  (all-equal? first-xs (rest xs))]
                     [else  #f])]))

(: next-indexes! (Indexes Index Indexes -> Void))
;; Sets js to the next vector of indexes, in row-major order
(define (next-indexes! ds dims js)
  (let loop ([#{k : Nonnegative-Fixnum}  dims])
    (unless (zero? k)
      (let ([k  (- k 1)])
        (define jk (unsafe-vector-ref js k))
        (define dk (unsafe-vector-ref ds k))
        (let ([jk  (+ jk 1)])
          (cond [(jk . >= . dk)
                 (unsafe-vector-set! js k 0)
                 (loop k)]
                [else
                 (unsafe-vector-set! js k jk)]))))))
