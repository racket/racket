#lang typed/racket/base

(require racket/fixnum
         racket/list
         "matrix-types.rkt"
         "matrix-basic.rkt"
         "matrix-gauss-elim.rkt"
         "utils.rkt"
         "../array/array-indexing.rkt"
         "../array/array-constructors.rkt"
         "../array/array-struct.rkt")

(provide 
 matrix-rank
 matrix-nullity
 matrix-col-space)

(: matrix-rank : (Matrix Number) -> Index)
;; Returns the dimension of the column space (equiv. row space) of M
(define (matrix-rank M)
  (define n (matrix-num-cols M))
  (define-values (_ cols-without-pivot) (matrix-gauss-elim M))
  (assert (- n (length cols-without-pivot)) index?))

(: matrix-nullity : (Matrix Number) -> Index)
;; Returns the dimension of the null space of M
(define (matrix-nullity M)
  (define-values (_ cols-without-pivot)
    (matrix-gauss-elim (ensure-matrix 'matrix-nullity M)))
  (length cols-without-pivot))

(: maybe-cons-submatrix (All (A) ((Matrix A) Nonnegative-Fixnum Nonnegative-Fixnum (Listof (Matrix A))
                                             -> (Listof (Matrix A)))))
(define (maybe-cons-submatrix M j0 j1 Bs)
  (cond [(= j0 j1)  Bs]
        [else  (cons (submatrix M (::) (:: j0 j1)) Bs)]))

(: matrix-col-space/ns (All (A) (case-> ((Matrix Flonum) -> (U #f (Matrix Flonum)))
                                        ((Matrix Real) -> (U #f (Matrix Real)))
                                        ((Matrix Float-Complex) -> (U #f (Matrix Float-Complex)))
                                        ((Matrix Number) -> (U #f (Matrix Number))))))
(define (matrix-col-space/ns M)
  (define n (matrix-num-cols M))
  (define-values (_ wps) (matrix-gauss-elim M))
  (cond [(empty? wps)  M]
        [(= (length wps) n)  #f]
        [else
         (define next-j (first wps))
         (define Bs (maybe-cons-submatrix M 0 next-j empty))
         (let loop ([#{j : Index} next-j] [wps (rest wps)] [Bs Bs])
           (cond [(empty? wps)
                  (matrix-augment (reverse (maybe-cons-submatrix M (fx+ j 1) n Bs)))]
                 [else
                  (define next-j (first wps))
                  (loop next-j (rest wps) (maybe-cons-submatrix M (fx+ j 1) next-j Bs))]))]))

(: matrix-col-space (All (A) (case-> ((Matrix Flonum)        -> (Array Flonum))
                                     ((Matrix Flonum) (-> A) -> (U A (Matrix Flonum)))
                                     ((Matrix Real)        -> (Array Real))
                                     ((Matrix Real) (-> A) -> (U A (Matrix Real)))
                                     ((Matrix Float-Complex)        -> (Array Float-Complex))
                                     ((Matrix Float-Complex) (-> A) -> (U A (Matrix Float-Complex)))
                                     ((Matrix Number)        -> (Array Number))
                                     ((Matrix Number) (-> A) -> (U A (Matrix Number))))))
(define matrix-col-space 
  (case-lambda
    [(M)  (matrix-col-space M (λ ()
                                (define x00 (matrix-ref M 0 0))
                                (make-array (vector 0 (matrix-num-cols M)) (zero* x00))))]
    [(M fail)
     (define S (parameterize ([array-strictness #f])
                 (matrix-col-space/ns M)))
     (if S (array-default-strict S) (fail))]))
