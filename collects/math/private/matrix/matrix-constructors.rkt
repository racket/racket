#lang racket/base

(provide
 ;; Constructors
 identity-matrix
 make-matrix
 build-matrix
 diagonal-matrix/zero
 diagonal-matrix
 block-diagonal-matrix/zero
 block-diagonal-matrix
 vandermonde-matrix
 ;; Basic conversion
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
 matrix->vector*
 ;; Syntax
 matrix
 row-matrix
 col-matrix)

(module typed-defs typed/racket/base
  (require racket/fixnum
           racket/list
           racket/vector
           math/array
           "../array/utils.rkt"
           "matrix-types.rkt"
           "utils.rkt"
           "../unsafe.rkt")
  
  (provide (all-defined-out))
  
  ;; =================================================================================================
  ;; Constructors
  
  (: identity-matrix (Integer -> (Matrix (U 0 1))))
  (define (identity-matrix m) (diagonal-array 2 m 1 0))
  
  (: make-matrix (All (A) (Integer Integer A -> (Matrix A))))
  (define (make-matrix m n x)
    (make-array (vector m n) x))
  
  (: build-matrix (All (A) (Integer Integer (Index Index -> A) -> (Matrix A))))
  (define (build-matrix m n proc)
    (cond [(or (not (index? m)) (= m 0))
           (raise-argument-error 'build-matrix "Positive-Index" 0 m n proc)]
          [(or (not (index? n)) (= n 0))
           (raise-argument-error 'build-matrix "Positive-Index" 1 m n proc)]
          [else
           (unsafe-build-array
            ((inst vector Index) m n)
            (λ: ([js : Indexes])
              (proc (unsafe-vector-ref js 0)
                    (unsafe-vector-ref js 1))))]))
  
  (: diagonal-matrix/zero (All (A) (Array A) A -> (Array A)))
  (define (diagonal-matrix/zero a zero)
    (define ds (array-shape a))
    (cond [(= 1 (vector-length ds))
           (define m (unsafe-vector-ref ds 0))
           (define proc (unsafe-array-proc a))
           (unsafe-build-array
            ((inst vector Index) m m)
            (λ: ([js : Indexes])
              (define i (unsafe-vector-ref js 0))
              (cond [(= i (unsafe-vector-ref js 1))  (proc ((inst vector Index) i))]
                    [else  zero])))]
          [else
           (raise-argument-error 'diagonal-matrix "Array with one dimension" a)]))
  
  (: diagonal-matrix (case-> ((Array Real) -> (Array Real))
                             ((Array Number) -> (Array Number))))
  (define (diagonal-matrix a)
    (diagonal-matrix/zero a 0))
  
  (: block-diagonal-matrix/zero* (All (A) (Vectorof (Array A)) A -> (Array A)))
  (define (block-diagonal-matrix/zero* as zero)
    (define num (vector-length as))
    (define-values (ms ns)
      (let-values ([(ms ns)  (for/fold: ([ms : (Listof Index)  empty]
                                         [ns : (Listof Index)  empty]
                                         ) ([a  (in-vector as)])
                               (define-values (m n) (matrix-shape a))
                               (values (cons m ms) (cons n ns)))])
        (values (reverse ms) (reverse ns))))
    (define res-m (assert (apply + ms) index?))
    (define res-n (assert (apply + ns) index?))
    (define vs ((inst make-vector Index) res-m 0))
    (define hs ((inst make-vector Index) res-n 0))
    (define is ((inst make-vector Index) res-m 0))
    (define js ((inst make-vector Index) res-n 0))
    (define-values (_res-i _res-j)
      (for/fold: ([res-i : Nonnegative-Fixnum 0]
                  [res-j : Nonnegative-Fixnum 0]
                  ) ([m  (in-list ms)]
                     [n  (in-list ns)]
                     [k : Nonnegative-Fixnum  (in-range num)])
        (let ([k  (assert k index?)])
          (for: ([i : Nonnegative-Fixnum  (in-range m)])
            (vector-set! vs (unsafe-fx+ res-i i) k)
            (vector-set! is (unsafe-fx+ res-i i) (assert i index?)))
          (for: ([j : Nonnegative-Fixnum  (in-range n)])
            (vector-set! hs (unsafe-fx+ res-j j) k)
            (vector-set! js (unsafe-fx+ res-j j) (assert j index?))))
        (values (unsafe-fx+ res-i m) (unsafe-fx+ res-j n))))
    (define procs (vector-map (λ: ([a : (Array A)]) (unsafe-array-proc a)) as))
    (unsafe-build-array
     ((inst vector Index) res-m res-n)
     (λ: ([ij : Indexes])
       (define i (unsafe-vector-ref ij 0))
       (define j (unsafe-vector-ref ij 1))
       (define v (unsafe-vector-ref vs i))
       (cond [(fx= v (vector-ref hs j))
              (define proc (unsafe-vector-ref procs v))
              (define iv (unsafe-vector-ref is i))
              (define jv (unsafe-vector-ref js j))
              (unsafe-vector-set! ij 0 iv)
              (unsafe-vector-set! ij 1 jv)
              (define res (proc ij))
              (unsafe-vector-set! ij 0 i)
              (unsafe-vector-set! ij 1 j)
              res]
             [else
              zero]))))
  
  (: block-diagonal-matrix/zero (All (A) (Listof (Array A)) A -> (Array A)))
  (define (block-diagonal-matrix/zero as zero)
    (let ([as  (list->vector as)])
      (define num (vector-length as))
      (cond [(= num 0)
             (raise-argument-error 'block-diagonal-matrix/zero "nonempty List" as)]
            [(= num 1)
             (unsafe-vector-ref as 0)]
            [else
             (block-diagonal-matrix/zero* as zero)])))
  
  (: block-diagonal-matrix (case-> ((Listof (Array Real)) -> (Array Real))
                                   ((Listof (Array Number)) -> (Array Number))))
  (define (block-diagonal-matrix as)
    (block-diagonal-matrix/zero as 0))
  
  (: expt-hack (case-> (Real Integer -> Real)
                       (Number Integer -> Number)))
  ;; Stop using this when TR correctly derives expt : Real Integer -> Real
  (define (expt-hack x n)
    (cond [(real? x)  (assert (expt x n) real?)]
          [else  (expt x n)]))
  
  (: vandermonde-matrix (case-> ((Listof Real) Integer -> (Array Real))
                                ((Listof Number) Integer -> (Array Number))))
  (define (vandermonde-matrix xs n)
    (cond [(empty? xs)
           (raise-argument-error 'vandermonde-matrix "nonempty List" 0 xs n)]
          [(or (not (index? n)) (zero? n))
           (raise-argument-error 'vandermonde-matrix "Positive-Index" 1 xs n)]
          [else
           (array-axis-expand (list->array xs) 1 n expt-hack)]))
  
  ;; =================================================================================================
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
  
  ;; =================================================================================================
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
  )  ; module

(require (for-syntax racket/base
                     syntax/parse)
         (only-in typed/racket/base :)
         math/array
         (submod "." typed-defs))

(define-syntax (matrix stx)
  (syntax-parse stx #:literals (:)
    [(_ [[x0 xs0 ...] [x xs ...] ...])
     (syntax/loc stx (array #[#[x0 xs0 ...] #[x xs ...] ...]))]
    [(_ [[x0 xs0 ...] [x xs ...] ...] : T)
     (syntax/loc stx (array #[#[x0 xs0 ...] #[x xs ...] ...] : T))]
    [(_ [xs ... (~and [] r) ys ...] (~optional (~seq : T)))
     (raise-syntax-error 'matrix "given empty row" stx #'r)]
    [(_ (~and [] c) (~optional (~seq : T)))
     (raise-syntax-error 'matrix "given empty matrix" stx #'c)]))

(define-syntax (row-matrix stx)
  (syntax-parse stx #:literals (:)
    [(_ [x xs ...])      (syntax/loc stx (array #[#[x xs ...]]))]
    [(_ [x xs ...] : T)  (syntax/loc stx (array #[#[x xs ...]] : T))]
    [(_ (~and [] r) (~optional (~seq : T)))
     (raise-syntax-error 'row-matrix "given empty row" stx #'r)]))

(define-syntax (col-matrix stx)
  (syntax-parse stx #:literals (:)
    [(_ [x xs ...])      (syntax/loc stx (array #[#[x] #[xs] ...]))]
    [(_ [x xs ...] : T)  (syntax/loc stx (array #[#[x] #[xs] ...] : T))]
    [(_ (~and [] c) (~optional (~seq : T)))
     (raise-syntax-error 'row-matrix "given empty column" stx #'c)]))
