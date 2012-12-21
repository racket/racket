#lang typed/racket/base

(require racket/fixnum
         racket/list
         racket/vector
         math/array
         "matrix-types.rkt"
         "utils.rkt"
         "../array/utils.rkt"
         "../unsafe.rkt")

(provide
 ;; Flat conversion
 list->matrix
 matrix->list
 vector->matrix
 matrix->vector
 ->row-matrix
 ->col-matrix
 ;; Nested conversion
 list*->matrix
 matrix->list*
 vector*->matrix
 matrix->vector*)

;; ===================================================================================================
;; Flat conversion

(: list->matrix (All (A) (Integer Integer (Listof A) -> (Array A))))
(define (list->matrix m n xs)
  (cond [(or (not (index? m)) (= m 0))
         (raise-argument-error 'list->matrix "Positive-Index" 0 m n xs)]
        [(or (not (index? n)) (= n 0))
         (raise-argument-error 'list->matrix "Positive-Index" 1 m n xs)]
        [else  (list->array (vector m n) xs)]))

(: matrix->list (All (A) ((Array A) -> (Listof A))))
(define (matrix->list a)
  (array->list (ensure-matrix 'matrix->list a)))

(: vector->matrix (All (A) (Integer Integer (Vectorof A) -> (Mutable-Array A))))
(define (vector->matrix m n v)
  (cond [(or (not (index? m)) (= m 0))
         (raise-argument-error 'vector->matrix "Positive-Index" 0 m n v)]
        [(or (not (index? n)) (= n 0))
         (raise-argument-error 'vector->matrix "Positive-Index" 1 m n v)]
        [else  (vector->array (vector m n) v)]))

(: matrix->vector (All (A) ((Array A) -> (Vectorof A))))
(define (matrix->vector a)
  (array->vector (ensure-matrix 'matrix->vector a)))

(: list->row-matrix (All (A) ((Listof A) -> (Array A))))
(define (list->row-matrix xs)
  (cond [(empty? xs)  (raise-argument-error 'list->row-matrix "nonempty List" xs)]
        [else  (list->array ((inst vector Index) 1 (length xs)) xs)]))

(: list->col-matrix (All (A) ((Listof A) -> (Array A))))
(define (list->col-matrix xs)
  (cond [(empty? xs)  (raise-argument-error 'list->col-matrix "nonempty List" xs)]
        [else  (list->array ((inst vector Index) (length xs) 1) xs)]))

(: vector->row-matrix (All (A) ((Vectorof A) -> (Mutable-Array A))))
(define (vector->row-matrix xs)
  (define n (vector-length xs))
  (cond [(zero? n)  (raise-argument-error 'vector->row-matrix "nonempty Vector" xs)]
        [else  (vector->array ((inst vector Index) 1 n) xs)]))

(: vector->col-matrix (All (A) ((Vectorof A) -> (Mutable-Array A))))
(define (vector->col-matrix xs)
  (define n (vector-length xs))
  (cond [(zero? n)  (raise-argument-error 'vector->col-matrix "nonempty Vector" xs)]
        [else  (vector->array ((inst vector Index) n 1) xs)]))

(: find-nontrivial-axis ((Vectorof Index) -> (Values Index Index)))
(define (find-nontrivial-axis ds)
  (define dims (vector-length ds))
  (let: loop : (Values Index Index) ([k : Nonnegative-Fixnum  0])
    (cond [(k . < . dims)  (define dk (unsafe-vector-ref ds k))
                           (if (dk . > . 1) (values k dk) (loop (fx+ k 1)))]
          [else  (values 0 0)])))

(: array->row-matrix (All (A) ((Array A) -> (Array A))))
(define (array->row-matrix arr)
  (define (fail)
    (raise-argument-error 'array->row-matrix "nonempty Array with one axis of length >= 1" arr))
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (define num-ones (vector-count (λ: ([d : Index]) (= d 1)) ds))
  (cond [(zero? (array-size arr))  (fail)]
        [(row-matrix? arr)  arr]
        [(= num-ones dims)
         (define: js : (Vectorof Index) (make-vector dims 0))
         (define proc (unsafe-array-proc arr))
         (unsafe-build-array ((inst vector Index) 1 1)
                             (λ: ([ij : Indexes]) (proc js)))]
        [(= num-ones (- dims 1))
         (define-values (k n) (find-nontrivial-axis ds))
         (define js (make-thread-local-indexes dims))
         (define proc (unsafe-array-proc arr))
         (unsafe-build-array ((inst vector Index) 1 n)
                             (λ: ([ij : Indexes])
                               (let ([js  (js)])
                                 (unsafe-vector-set! js k (unsafe-vector-ref ij 1))
                                 (proc js))))]
        [else  (fail)]))

(: array->col-matrix (All (A) ((Array A) -> (Array A))))
(define (array->col-matrix arr)
  (define (fail)
    (raise-argument-error 'array->col-matrix "nonempty Array with one axis of length >= 1" arr))
  (define ds (array-shape arr))
  (define dims (vector-length ds))
  (define num-ones (vector-count (λ: ([d : Index]) (= d 1)) ds))
  (cond [(zero? (array-size arr))  (fail)]
        [(col-matrix? arr)  arr]
        [(= num-ones dims)
         (define: js : (Vectorof Index) (make-vector dims 0))
         (define proc (unsafe-array-proc arr))
         (unsafe-build-array ((inst vector Index) 1 1)
                             (λ: ([ij : Indexes]) (proc js)))]
        [(= num-ones (- dims 1))
         (define-values (k m) (find-nontrivial-axis ds))
         (define js (make-thread-local-indexes dims))
         (define proc (unsafe-array-proc arr))
         (unsafe-build-array ((inst vector Index) m 1)
                             (λ: ([ij : Indexes])
                               (let ([js  (js)])
                                 (unsafe-vector-set! js k (unsafe-vector-ref ij 0))
                                 (proc js))))]
        [else  (fail)]))

(: ->row-matrix (All (A) ((U (Listof A) (Vectorof A) (Array A)) -> (Array A))))
(define (->row-matrix xs)
  (cond [(list? xs)  (list->row-matrix xs)]
        [(array? xs)  (array->row-matrix xs)]
        [else  (vector->row-matrix xs)]))

(: ->col-matrix (All (A) ((U (Listof A) (Vectorof A) (Array A)) -> (Array A))))
(define (->col-matrix xs)
  (cond [(list? xs)  (list->col-matrix xs)]
        [(array? xs)  (array->col-matrix xs)]
        [else  (vector->col-matrix xs)]))

;; ===================================================================================================
;; Nested conversion

(: list*-shape (All (A) (Listof (Listof A)) (-> Nothing) -> (Values Positive-Index Positive-Index)))
(define (list*-shape xss fail)
  (define m (length xss))
  (cond [(m . > . 0)
         (define n (length (first xss)))
         (cond [(and (n . > . 0) (andmap (λ: ([xs : (Listof A)]) (= n (length xs))) (rest xss)))
                (values m n)]
               [else  (fail)])]
        [else  (fail)]))

(: vector*-shape (All (A) (Vectorof (Vectorof A)) (-> Nothing)
                      -> (Values Positive-Index Positive-Index)))
(define (vector*-shape xss fail)
  (define m (vector-length xss))
  (cond [(m . > . 0)
         (define ns ((inst vector-map Index (Vectorof A)) vector-length xss))
         (define n (vector-length (unsafe-vector-ref xss 0)))
         (cond [(and (n . > . 0)
                     (let: loop : Boolean ([i : Nonnegative-Fixnum  1])
                       (cond [(i . fx< . m)
                              (if (= n (vector-length (unsafe-vector-ref xss i)))
                                  (loop (fx+ i 1))
                                  #f)]
                             [else  #t])))
                (values m n)]
               [else  (fail)])]
        [else  (fail)]))

(: list*->matrix (All (A) (Listof (Listof A)) -> (Matrix A)))
(define (list*->matrix xss)
  (define (fail)
    (raise-argument-error 'list*->matrix
                          "nested lists with rectangular shape and at least one matrix element"
                          xss))
  (define-values (m n) (list*-shape xss fail))
  (list->array ((inst vector Index) m n) (apply append xss)))

(: matrix->list* (All (A) (Matrix A) -> (Listof (Listof A))))
(define (matrix->list* a)
  (cond [(matrix? a)  (array->list (array->list-array a 1))]
        [else  (raise-argument-error 'matrix->list* "matrix?" a)]))

(: vector*->matrix (All (A) (Vectorof (Vectorof A)) -> (Mutable-Array A)))
(define (vector*->matrix xss)
  (define (fail)
    (raise-argument-error 'vector*->matrix
                          "nested vectors with rectangular shape and at least one matrix element"
                          xss))
  (define-values (m n) (vector*-shape xss fail))
  (vector->matrix m n (apply vector-append (vector->list xss))))

(: matrix->vector* : (All (A) (Matrix A) -> (Vectorof (Vectorof A))))
(define (matrix->vector* a)
  (cond [(matrix? a)  (array->vector ((inst array-axis-reduce A (Vectorof A)) a 1 build-vector))]
        [else  (raise-argument-error 'matrix->vector* "matrix?" a)]))
