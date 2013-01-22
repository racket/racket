#lang typed/racket/base

(require racket/fixnum
         racket/flonum
         racket/list
         racket/vector
         math/base
         "matrix-types.rkt"
         "../unsafe.rkt"
         "../array/array-struct.rkt"
         "../array/array-constructors.rkt"
         "../array/array-unfold.rkt"
         "../array/utils.rkt")

(provide identity-matrix
         make-matrix
         build-matrix
         diagonal-matrix/zero
         diagonal-matrix
         block-diagonal-matrix/zero
         block-diagonal-matrix
         vandermonde-matrix)

;; ===================================================================================================
;; Basic constructors

(: identity-matrix (All (A) (case-> (Integer -> (Matrix (U 1 0)))
                                    (Integer A -> (Matrix (U A 0)))
                                    (Integer A A -> (Matrix A)))))
(define identity-matrix
  (case-lambda
    [(m)  (diagonal-array 2 m 1 0)]
    [(m one)  (diagonal-array 2 m one 0)]
    [(m one zero)  (diagonal-array 2 m one zero)]))

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
         (array-default-strict
          (unsafe-build-array
           ((inst vector Index) m n)
           (λ: ([js : Indexes])
             (proc (unsafe-vector-ref js 0)
                   (unsafe-vector-ref js 1)))))]))

;; ===================================================================================================
;; Diagonal matrices

(: diagonal-matrix/zero (All (A) ((Listof A) A -> (Matrix A))))
(define (diagonal-matrix/zero xs zero)
  (cond [(empty? xs)
         (raise-argument-error 'diagonal-matrix "nonempty List" xs)]
        [else
         (define vs (list->vector xs))
         (define m (vector-length vs))
         (unsafe-build-simple-array
          ((inst vector Index) m m)
          (λ: ([js : Indexes])
            (define i (unsafe-vector-ref js 0))
            (cond [(= i (unsafe-vector-ref js 1))  (unsafe-vector-ref vs i)]
                  [else  zero])))]))

(: diagonal-matrix (All (A) (case-> ((Listof A) -> (Matrix (U A 0)))
                                    ((Listof A) A -> (Matrix A)))))
(define diagonal-matrix
  (case-lambda
    [(xs)  (diagonal-matrix/zero xs 0)]
    [(xs zero)  (diagonal-matrix/zero xs zero)]))

;; ===================================================================================================
;; Block diagonal matrices

(: block-diagonal-matrix/zero* (All (A) (Vectorof (Matrix A)) A -> (Matrix A)))
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
  (define procs (vector-map (λ: ([a : (Matrix A)]) (unsafe-array-proc a)) as))
  (array-default-strict
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
             zero])))))

(: block-diagonal-matrix/zero (All (A) ((Listof (Matrix A)) A -> (Matrix A))))
(define (block-diagonal-matrix/zero as zero)
  (let ([as  (list->vector as)])
    (define num (vector-length as))
    (cond [(= num 0)
           (raise-argument-error 'block-diagonal-matrix/zero "nonempty List" as)]
          [(= num 1)
           (unsafe-vector-ref as 0)]
          [else
           (block-diagonal-matrix/zero* as zero)])))

(: block-diagonal-matrix (All (A) (case-> ((Listof (Matrix A)) -> (Matrix (U A 0)))
                                          ((Listof (Matrix A)) A -> (Matrix A)))))
(define block-diagonal-matrix
  (case-lambda
    [(as)  (block-diagonal-matrix/zero as 0)]
    [(as zero)  (block-diagonal-matrix/zero as zero)]))

;; ===================================================================================================
;; Special matrices

(: sane-expt (case-> (Flonum Index -> Flonum)
                     (Real Index -> Real)
                     (Float-Complex Index -> Float-Complex)
                     (Number Index -> Number)))
(define (sane-expt x n)
  (cond [(flonum? x)  (flexpt x (real->double-flonum n))]
        [(real? x)  (real-part (expt x n))]  ; remove `real-part' when expt : Real Index -> Real
        [(float-complex? x)  (number->float-complex (expt x n))]
        [else  (expt x n)]))

(: vandermonde-matrix (case-> ((Listof Flonum) Integer -> (Matrix Flonum))
                              ((Listof Real) Integer -> (Matrix Real))
                              ((Listof Float-Complex) Integer -> (Matrix Float-Complex))
                              ((Listof Number) Integer -> (Matrix Number))))
(define (vandermonde-matrix xs n)
  (cond [(empty? xs)
         (raise-argument-error 'vandermonde-matrix "nonempty List" 0 xs n)]
        [(or (not (index? n)) (zero? n))
         (raise-argument-error 'vandermonde-matrix "Positive-Index" 1 xs n)]
        [else
         (array-axis-expand (list->array xs) 1 n sane-expt)]))
