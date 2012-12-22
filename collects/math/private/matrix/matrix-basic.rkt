#lang typed/racket

(require racket/list
         racket/fixnum
         math/array
         math/flonum
         "matrix-types.rkt"
         "matrix-arithmetic.rkt"
         "utils.rkt"
         "../unsafe.rkt")

(provide
 ;; Extraction
 matrix-ref
 matrix-diagonal
 submatrix
 matrix-row
 matrix-col
 matrix-rows
 matrix-cols
 ;; Predicates
 matrix-zero?
 ;; Embiggenment
 matrix-augment
 matrix-stack
 ;; Norm and inner product
 matrix-norm
 matrix-dot
 ;; Simple operators
 matrix-transpose
 matrix-conjugate
 matrix-hermitian
 matrix-trace)

;; ===================================================================================================
;; Extraction

(: matrix-ref (All (A) (Array A) Integer Integer -> A))
(define (matrix-ref a i j)
  (define-values (m n) (matrix-shape a))
  (cond [(or (i . < . 0) (i . >= . m))
         (raise-argument-error 'matrix-ref (format "Index < ~a" m) 1 a i j)]
        [(or (j . < . 0) (j . >= . n))
         (raise-argument-error 'matrix-ref (format "Index < ~a" n) 2 a i j)]
        [else
         (unsafe-array-ref a ((inst vector Index) i j))]))

(: matrix-diagonal (All (A) ((Array A) -> (Array A))))
(define (matrix-diagonal a)
  (define m (square-matrix-size a))
  (define proc (unsafe-array-proc a))
  (unsafe-build-array
   ((inst vector Index) m)
   (λ: ([js : Indexes])
     (define i (unsafe-vector-ref js 0))
     (proc ((inst vector Index) i i)))))

(: submatrix (All (A) (Matrix A) Slice-Spec Slice-Spec -> (Matrix A)))
(define (submatrix a row-range col-range)
  (array-slice-ref (ensure-matrix 'submatrix a) (list row-range col-range)))

(: matrix-row (All (A) (Matrix A) Integer -> (Matrix A)))
(define (matrix-row a i)
  (define-values (m n) (matrix-shape a))
  (cond [(or (i . < . 0) (i . >= . m))
         (raise-argument-error 'matrix-row (format "Index < ~a" m) 1 a i)]
        [else
         (define proc (unsafe-array-proc a))
         (unsafe-build-array
          ((inst vector Index) 1 n)
          (λ: ([ij : Indexes])
            (unsafe-vector-set! ij 0 i)
            (define res (proc ij))
            (unsafe-vector-set! ij 0 0)
            res))]))

(: matrix-col (All (A) (Matrix A) Integer -> (Matrix A)))
(define (matrix-col a j)
  (define-values (m n) (matrix-shape a))
  (cond [(or (j . < . 0) (j . >= . n))
         (raise-argument-error 'matrix-row (format "Index < ~a" n) 1 a j)]
        [else
         (define proc (unsafe-array-proc a))
         (unsafe-build-array
          ((inst vector Index) m 1)
          (λ: ([ij : Indexes])
            (unsafe-vector-set! ij 1 j)
            (define res (proc ij))
            (unsafe-vector-set! ij 1 0)
            res))]))

(: matrix-rows (All (A) (Array A) -> (Listof (Array A))))
(define (matrix-rows a)
  (array->array-list (array-axis-insert (ensure-matrix 'matrix-rows a) 1) 0))

(: matrix-cols (All (A) (Array A) -> (Listof (Array A))))
(define (matrix-cols a)
  (array->array-list (array-axis-insert (ensure-matrix 'matrix-cols a) 2) 1))

;; ===================================================================================================
;; Predicates

(: matrix-zero? ((Array Number) -> Boolean))
(define (matrix-zero? a)
  (array-all-and (matrix-map zero? a)))

;; ===================================================================================================
;; Embiggenment (this is a perfectly cromulent word)

(: matrix-augment (All (A) (Listof (Array A)) -> (Array A)))
(define (matrix-augment as)
  (cond [(empty? as)  (raise-argument-error 'matrix-augment "nonempty List" as)]
        [else
         (define m (matrix-num-rows (first as)))
         (cond [(andmap (λ: ([a : (Array A)]) (= m (matrix-num-rows a))) (rest as))
                (array-append* as 1)]
               [else
                (error 'matrix-augment
                       "matrices must have the same number of rows; given ~a"
                       (format-matrices/error as))])]))

(: matrix-stack (All (A) (Listof (Array A)) -> (Array A)))
(define (matrix-stack as)
  (cond [(empty? as)  (raise-argument-error 'matrix-stack "nonempty List" as)]
        [else
         (define n (matrix-num-cols (first as)))
         (cond [(andmap (λ: ([a : (Array A)]) (= n (matrix-num-cols a))) (rest as))
                (array-append* as 0)]
               [else
                (error 'matrix-stack
                       "matrices must have the same number of columns; given ~a"
                       (format-matrices/error as))])]))

;; ===================================================================================================
;; Matrix norms and Frobenius inner product

(: maximum-norm ((Array Number) -> Real))
(define (maximum-norm a)
  (array-all-max (array-magnitude a)))

(: taxicab-norm ((Array Number) -> Real))
(define (taxicab-norm a)
  (array-all-sum (array-magnitude a)))

(: frobenius-norm ((Array Number) -> Real))
(define (frobenius-norm a)
  (let ([a  (array-strict (array-magnitude a))])
    ;; Compute this divided by the maximum to avoid underflow and overflow
    (define mx (array-all-max a))
    (cond [(and (rational? mx) (positive? mx))
           (assert
            (* mx (sqrt (array-all-sum (inline-array-map (λ: ([x : Number]) (sqr (/ x mx))) a))))
            real?)]
          [else  mx])))

(: p-norm ((Array Number) Positive-Real -> Real))
(define (p-norm a p)
  (let ([a  (array-strict (array-magnitude a))])
    ;; Compute this divided by the maximum to avoid underflow and overflow
    (define mx (array-all-max a))
    (cond [(and (rational? mx) (positive? mx))
           (assert
            (* mx (expt (array-all-sum (inline-array-map (λ: ([x : Real]) (expt (/ x mx) p)) a))
                        (/ p)))
            real?)]
          [else  mx])))

(: matrix-norm (case-> ((Array Number) -> Real)
                       ((Array Number) Real -> Real)))
;; Computes the p norm of a matrix
(define (matrix-norm a [p 2])
  (cond [(not (matrix? a))  (raise-argument-error 'matrix-norm "matrix?" 0 a p)]
        [(p . = . 2)       (frobenius-norm a)]
        [(p . = . +inf.0)  (maximum-norm a)]
        [(p . = . 1)       (taxicab-norm a)]
        [(p . > . 1)       (p-norm a p)]
        [else  (raise-argument-error 'matrix-norm "Real >= 1" 1 a p)]))

(: matrix-dot (case-> ((Array Real) (Array Real) -> Real)
                      ((Array Number) (Array Number) -> Number)))
;; Computes the Frobenius inner product of two matrices
(define (matrix-dot a b)
  (define-values (m n) (matrix-shapes 'matrix-dot a b))
  (define aproc (unsafe-array-proc a))
  (define bproc (unsafe-array-proc b))
  (array-all-sum
   (unsafe-build-array
    ((inst vector Index) m n)
    (λ: ([js : Indexes])
      (* (aproc js) (conjugate (bproc js)))))))

;; ===================================================================================================
;; Operators

(: matrix-transpose (All (A) (Array A) -> (Array A)))
(define (matrix-transpose a)
  (array-axis-swap (ensure-matrix 'matrix-transpose a) 0 1))

(: matrix-conjugate (case-> ((Array Real) -> (Array Real))
                            ((Array Number) -> (Array Number))))
(define (matrix-conjugate a)
  (array-conjugate (ensure-matrix 'matrix-conjugate a)))

(: matrix-hermitian (case-> ((Array Real) -> (Array Real))
                            ((Array Number) -> (Array Number))))
(define (matrix-hermitian a)
  (array-axis-swap (array-conjugate (ensure-matrix 'matrix-hermitian a)) 0 1))

(: matrix-trace (case-> ((Array Real) -> Real)
                        ((Array Number) -> Number)))
(define (matrix-trace a)
  (array-all-sum (matrix-diagonal a)))
