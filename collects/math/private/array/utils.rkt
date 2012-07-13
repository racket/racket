#lang typed/racket/base

(require racket/unsafe/ops
         racket/flonum
         racket/list
         racket/performance-hint)

(provide (all-defined-out))

(define-type (Listof* A) (Rec T (U A (Listof T))))
(define-type (Vectorof* A) (Rec T (U A (Vectorof T))))
(define-predicate listof-index? (Listof Index))

(begin-encourage-inline
  
  (: unsafe-vector-copy-all! (All (A) ((Vectorof A) (Vectorof A) -> Void)))
  (define (unsafe-vector-copy-all! dst-js src-js)
    (define dims (vector-length src-js))
    (let loop ([#{i : Nonnegative-Fixnum} 0])
      (when (i . < . dims)
        (unsafe-vector-set! dst-js i (unsafe-vector-ref src-js i))
        (loop (+ i 1)))))
  
  (: vector-copy-all (All (A) ((Vectorof A) -> (Vectorof A))))
  (define (vector-copy-all js)
    (define dims (vector-length js))
    (cond [(= dims 0)  js]
          [else  (define: new-js : (Vectorof A) (make-vector dims (unsafe-vector-ref js 0)))
                 (let loop ([#{i : Nonnegative-Fixnum} 1])
                   (cond [(i . < . dims)  (unsafe-vector-set! new-js i (unsafe-vector-ref js i))
                                          (loop (+ i 1))]
                         [else  new-js]))]))
  
  (: array-shape? (Any -> Boolean : (Listof Index)))
  (define (array-shape? ds)
    (and (list? ds) (index? (apply * ds)) (listof-index? ds)))
  
  (: unsafe-array-shape-size ((Vectorof Index) -> Nonnegative-Integer))
  (define (unsafe-array-shape-size ds)
    (define dims (vector-length ds))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{n : Nonnegative-Integer} 1])
      (cond [(i . < . dims)  (define d (unsafe-vector-ref ds i))
                             (loop (+ i 1) (* n d))]
            [else  n])))
  
  (: array-shape-safe->unsafe ((Listof Integer) (-> Nothing) -> (Vectorof Index)))
  (define (array-shape-safe->unsafe ds fail)
    (define dims (length ds))
    (define: new-ds : (Vectorof Index) (make-vector dims 0))
    (let loop ([ds ds] [#{i : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (unsafe-car ds))
             (cond [(index? di)  (unsafe-vector-set! new-ds i di)
                                 (loop (unsafe-cdr ds) (+ i 1))]
                   [else  (fail)])]
            [else  new-ds])))
  
  (: unsafe-array-index->value-index ((Vectorof Index) (Vectorof Index) -> Nonnegative-Fixnum))
  (define (unsafe-array-index->value-index ds js)
    (define dims (vector-length ds))
    (let loop ([#{i : Nonnegative-Fixnum} 0] [#{j : Nonnegative-Fixnum} 0])
      (cond [(i . < . dims)
             (define di (unsafe-vector-ref ds i))
             (define ji (unsafe-vector-ref js i))
             (loop (+ i 1) (unsafe-fx+ ji (unsafe-fx* di j)))]
            [else  j])))
  
  )  ; begin-encourage-inline

(: vector-copy-all! (All (A) ((Vectorof A) (Vectorof A) -> Void)))
(define (vector-copy-all! dst-js src-js)
  (define dims (vector-length src-js))
  (unless (= dims (vector-length dst-js))
    (raise-type-error 'vector-copy-all! (format "Vector with ~e elements" dims) 0 dst-js src-js))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . dims)
      (unsafe-vector-set! dst-js i (unsafe-vector-ref src-js i))
      (loop (+ i 1)))))

(: raise-array-index-error (Symbol (Vectorof Index) (Listof Integer) -> Nothing))
(define (raise-array-index-error name ds js)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))

(: array-shape-unsafe->safe ((Vectorof Index) -> (Listof Index)))
(define array-shape-unsafe->safe vector->list)

(: array-index->value-index (Symbol (Vectorof Index) (Listof Integer) -> Nonnegative-Fixnum))
(define (array-index->value-index name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (length js)) (raise-index-error))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [js js] [#{j : Nonnegative-Fixnum}  0])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (define ji (unsafe-car js))
           (cond [(and (0 . <= . ji) (ji . < . di))
                  (loop (+ i 1) (unsafe-cdr js) (unsafe-fx+ ji (unsafe-fx* di j)))]
                 [else  (raise-index-error)])]
          [else  j])))

(: check-array-indexes (Symbol (Vectorof Index) (Listof Integer) -> (Vectorof Index)))
(define (check-array-indexes name ds js)
  (define (raise-index-error) (raise-array-index-error name ds js))
  (define dims (vector-length ds))
  (unless (= dims (length js)) (raise-index-error))
  (define: new-js : (Vectorof Index) (make-vector dims 0))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [js js])
    (cond [(i . < . dims)
           (define di (unsafe-vector-ref ds i))
           (define ji (unsafe-car js))
           (cond [(and (0 . <= . ji) (ji . < . di))
                  (unsafe-vector-set! new-js i ji)
                  (loop (+ i 1) (unsafe-cdr js))]
                 [else  (raise-index-error)])]
          [else  new-js])))

(: unsafe-check-equal-array-shape! (Symbol (Vectorof Index) (Vectorof Index) -> Void))
(define (unsafe-check-equal-array-shape! name ds1 ds2)
  (unless (equal? ds1 ds2)
    (error name
           "expected Arrays with the same shape; given Arrays with shapes ~e and ~e"
           (array-shape-unsafe->safe ds1)
           (array-shape-unsafe->safe ds2))))

(: vector-shape (All (A) (((Vectorof* A) -> Boolean : A) (Vectorof* A) -> (U #f (Listof Integer)))))
(define (vector-shape pred? vec)
  (let: vector-shape : (U #f (Listof Integer)) ([vec : (Vectorof* A)  vec])
    (cond [(pred? vec)  (list)]
          [else
           (define d (vector-length vec))
           (cond [(= d 0)  (list 0)]
                 [else
                  (define ds (vector-shape (vector-ref vec 0)))
                  (if ds
                      (let loop ([#{i : Nonnegative-Fixnum} 1])
                        (cond [(i . >= . d)  (cons d ds)]
                              [(equal? ds (vector-shape (vector-ref vec i)))
                               (loop (+ i 1))]
                              [else  #f]))
                      #f)])])))

(: list-shape (All (A) (((Listof* A) -> Boolean : A) (Listof* A) -> (U #f (Listof Integer)))))
(define (list-shape pred? lst)
  (let: list-shape : (U #f (Listof Integer)) ([lst : (Listof* A)  lst])
    (cond [(pred? lst)  (list)]
          [(null? lst)  (list 0)]
          [else
           (define d (length lst))
           (define ds (list-shape (car lst)))
           (if ds
               (let loop ([lst  (cdr lst)])
                 (cond [(null? lst)  (cons d ds)]
                       [(equal? ds (list-shape (car lst)))
                        (loop (cdr lst))]
                       [else  #f]))
               #f)])))

(: list-flatten (All (A) (((Listof* A) -> Boolean : A) (Listof* A) -> (Listof A))))
(define (list-flatten pred? lst)
  (let loop ([lst lst])
    (cond [(pred? lst)  (list lst)]
          [else  (append* (map loop lst))])))

(: list->flvector ((Listof Float) -> FlVector))
(define (list->flvector vs)
  (define n (length vs))
  (define new-vs (make-flvector n))
  (let loop ([vs vs] [#{i : Nonnegative-Fixnum} 0])
    (cond [(i . < . n)  (unsafe-flvector-set! new-vs i (unsafe-car vs))
                        (loop (unsafe-cdr vs) (+ i 1))]
          [else  new-vs])))

(: unsafe-vector-remove (All (I) ((Vectorof I) Index -> (Vectorof I))))
(define (unsafe-vector-remove vec k)
  (define n (vector-length vec))
  (define: new-vec : (Vectorof I) (make-vector (sub1 n) (unsafe-vector-ref vec 0)))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . k)
      (unsafe-vector-set! new-vec i (unsafe-vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([#{i : Nonnegative-Fixnum} k])
    (cond [(i . < . n)
           (unsafe-vector-set! new-vec i (unsafe-vector-ref vec (+ i 1)))
           (loop (+ i 1))]
          [else  new-vec])))

(: unsafe-vector-insert (All (I) ((Vectorof I) Index I -> (Vectorof I))))
(define (unsafe-vector-insert vec k v)
  (define n (vector-length vec))
  (define: new-vec : (Vectorof I) (make-vector (+ n 1) v))
  (let loop ([#{i : Nonnegative-Fixnum} 0])
    (when (i . < . k)
      (unsafe-vector-set! new-vec i (unsafe-vector-ref vec i))
      (loop (+ i 1))))
  (let loop ([#{i : Nonnegative-Fixnum} k])
    (cond [(i . < . n)
           (unsafe-vector-set! new-vec (+ i 1) (unsafe-vector-ref vec i))
           (loop (+ i 1))]
          [else  new-vec])))

(: port-next-column (Output-Port -> Nonnegative-Integer))
;; Helper to avoid the annoying #f column value
(define (port-next-column port)
  (define-values (_line col _pos) (port-next-location port))
  (if col col 0))
