#lang typed/racket/base

(require racket/list
         racket/fixnum
         math/flonum
         math/base
         "matrix-types.rkt"
         "matrix-arithmetic.rkt"
         "matrix-constructors.rkt"
         "matrix-conversion.rkt"
         "utils.rkt"
         "../unsafe.rkt"
         "../array/array-struct.rkt"
         "../array/array-indexing.rkt"
         "../array/array-sequence.rkt"
         "../array/array-transform.rkt"
         "../array/array-fold.rkt"
         "../array/array-pointwise.rkt"
         "../array/array-convert.rkt"
         "../array/utils.rkt"
         "../vector/vector-mutate.rkt")

(provide
 ;; Extraction
 matrix-ref
 submatrix
 matrix-row
 matrix-col
 matrix-rows
 matrix-cols
 matrix-diagonal
 matrix-upper-triangle
 matrix-lower-triangle
 ;; Embiggenment
 matrix-augment
 matrix-stack
 ;; Inner product space
 matrix-1norm
 matrix-2norm
 matrix-inf-norm
 matrix-norm
 matrix-dot
 matrix-cos-angle
 matrix-angle
 matrix-normalize
 ;; Simple operators
 matrix-transpose
 matrix-conjugate
 matrix-hermitian
 matrix-trace
 ;; Row/column operators
 matrix-map-rows
 matrix-map-cols
 matrix-normalize-rows
 matrix-normalize-cols
 ;; Predicates
 matrix-rows-orthogonal?
 matrix-cols-orthogonal?)

;; ===================================================================================================
;; Extraction

(: matrix-ref (All (A) (Matrix A) Integer Integer -> A))
(define (matrix-ref a i j)
  (define-values (m n) (matrix-shape a))
  (cond [(or (i . < . 0) (i . >= . m))
         (raise-argument-error 'matrix-ref (format "Index < ~a" m) 1 a i j)]
        [(or (j . < . 0) (j . >= . n))
         (raise-argument-error 'matrix-ref (format "Index < ~a" n) 2 a i j)]
        [else
         (unsafe-array-ref a ((inst vector Index) i j))]))

(: submatrix (All (A) (Matrix A) (U Slice (Sequenceof Integer)) (U Slice (Sequenceof Integer))
                  -> (Array A)))
(define (submatrix a row-range col-range)
  (array-slice-ref (ensure-matrix 'submatrix a) (list row-range col-range)))

(: matrix-row (All (A) (Matrix A) Integer -> (Matrix A)))
(define (matrix-row a i)
  (define-values (m n) (matrix-shape a))
  (cond [(or (i . < . 0) (i . >= . m))
         (raise-argument-error 'matrix-row (format "Index < ~a" m) 1 a i)]
        [else
         (define proc (unsafe-array-proc a))
         (array-default-strict
          (unsafe-build-array
           ((inst vector Index) 1 n)
           (λ: ([ij : Indexes])
             (unsafe-vector-set! ij 0 i)
             (define res (proc ij))
             (unsafe-vector-set! ij 0 0)
             res)))]))

(: matrix-col (All (A) (Matrix A) Integer -> (Matrix A)))
(define (matrix-col a j)
  (define-values (m n) (matrix-shape a))
  (cond [(or (j . < . 0) (j . >= . n))
         (raise-argument-error 'matrix-row (format "Index < ~a" n) 1 a j)]
        [else
         (define proc (unsafe-array-proc a))
         (array-default-strict
          (unsafe-build-array
           ((inst vector Index) m 1)
           (λ: ([ij : Indexes])
             (unsafe-vector-set! ij 1 j)
             (define res (proc ij))
             (unsafe-vector-set! ij 1 0)
             res)))]))

(: matrix-rows (All (A) (Matrix A) -> (Listof (Matrix A))))
(define (matrix-rows a)
  (map (λ: ([a : (Matrix A)]) (array-default-strict a))
       (parameterize ([array-strictness #f])
         (array->array-list (array-axis-insert (ensure-matrix 'matrix-rows a) 1) 0))))

(: matrix-cols (All (A) (Matrix A) -> (Listof (Matrix A))))
(define (matrix-cols a)
  (map (λ: ([a : (Matrix A)]) (array-default-strict a))
       (parameterize ([array-strictness #f])
         (array->array-list (array-axis-insert (ensure-matrix 'matrix-cols a) 2) 1))))

(: matrix-diagonal (All (A) ((Matrix A) -> (Array A))))
(define (matrix-diagonal a)
  (define-values (m n) (matrix-shape a))
  (define proc (unsafe-array-proc a))
  (array-default-strict
   (unsafe-build-array
    ((inst vector Index) (fxmin m n))
    (λ: ([js : Indexes])
      (define i (unsafe-vector-ref js 0))
      (proc ((inst vector Index) i i))))))

(: matrix-upper-triangle (All (A) (case-> ((Matrix A) -> (Matrix (U A 0)))
                                          ((Matrix A) A -> (Matrix A)))))
(define matrix-upper-triangle
  (case-lambda
    [(M)  (matrix-upper-triangle M 0)]
    [(M zero)
     (define-values (m n) (matrix-shape M))
     (define proc (unsafe-array-proc M))
     (array-default-strict
      (unsafe-build-array
       ((inst vector Index) m n)
       (λ: ([ij : Indexes])
         (define i (unsafe-vector-ref ij 0))
         (define j (unsafe-vector-ref ij 1))
         (if (i . fx<= . j) (proc ij) zero))))]))

(: matrix-lower-triangle (All (A) (case-> ((Matrix A) -> (Matrix (U A 0)))
                                          ((Matrix A) A -> (Matrix A)))))
(define matrix-lower-triangle
  (case-lambda
    [(M)  (matrix-lower-triangle M 0)]
    [(M zero)
     (define-values (m n) (matrix-shape M))
     (define proc (unsafe-array-proc M))
     (array-default-strict
      (unsafe-build-array
       ((inst vector Index) m n)
       (λ: ([ij : Indexes])
         (define i (unsafe-vector-ref ij 0))
         (define j (unsafe-vector-ref ij 1))
         (if (i . fx>= . j) (proc ij) zero))))]))

;; ===================================================================================================
;; Embiggenment (this is a perfectly cromulent word)

(: matrix-augment (All (A) (Listof (Matrix A)) -> (Matrix A)))
(define (matrix-augment as)
  (cond [(empty? as)  (raise-argument-error 'matrix-augment "nonempty List" as)]
        [else
         (define m (matrix-num-rows (first as)))
         (cond [(andmap (λ: ([a : (Matrix A)]) (= m (matrix-num-rows a))) (rest as))
                (array-append* as 1)]
               [else
                (error 'matrix-augment
                       "matrices must have the same number of rows; given ~a"
                       (format-matrices/error as))])]))

(: matrix-stack (All (A) (Listof (Matrix A)) -> (Matrix A)))
(define (matrix-stack as)
  (cond [(empty? as)  (raise-argument-error 'matrix-stack "nonempty List" as)]
        [else
         (define n (matrix-num-cols (first as)))
         (cond [(andmap (λ: ([a : (Matrix A)]) (= n (matrix-num-cols a))) (rest as))
                (array-append* as 0)]
               [else
                (error 'matrix-stack
                       "matrices must have the same number of columns; given ~a"
                       (format-matrices/error as))])]))

;; ===================================================================================================
;; Inner product space (entrywise norm)

(: nonstupid-magnitude (case-> (Flonum -> Nonnegative-Flonum)
                               (Real -> Nonnegative-Real)
                               (Float-Complex -> Nonnegative-Flonum)
                               (Number -> Nonnegative-Real)))
(define (nonstupid-magnitude x)
  (if (real? x) (abs x) (magnitude x)))

(: matrix-1norm (case-> ((Matrix Flonum) -> Nonnegative-Flonum)
                        ((Matrix Real) -> Nonnegative-Real)
                        ((Matrix Float-Complex) -> Nonnegative-Flonum)
                        ((Matrix Number) -> Nonnegative-Real)))
(define (matrix-1norm M)
  (parameterize ([array-strictness #f])
    (array-all-sum (inline-array-map nonstupid-magnitude M))))

(: matrix-2norm (case-> ((Matrix Flonum) -> Nonnegative-Flonum)
                        ((Matrix Real) -> Nonnegative-Real)
                        ((Matrix Float-Complex) -> Nonnegative-Flonum)
                        ((Matrix Number) -> Nonnegative-Real)))
(define (matrix-2norm M)
  (parameterize ([array-strictness #f])
    (let ([M  (array-strict (inline-array-map nonstupid-magnitude M))])
      ;; Compute this divided by the maximum to avoid underflow and overflow
      (define mx (array-all-max M))
      (cond [(and (rational? mx) (positive? mx))
             (* mx (sqrt (array-all-sum (inline-array-map (λ (x) (sqr (/ x mx))) M))))]
            [else  mx]))))

(: matrix-inf-norm (case-> ((Matrix Flonum) -> Nonnegative-Flonum)
                           ((Matrix Real) -> Nonnegative-Real)
                           ((Matrix Float-Complex) -> Nonnegative-Flonum)
                           ((Matrix Number) -> Nonnegative-Real)))
(define (matrix-inf-norm M)
  (parameterize ([array-strictness #f])
    (array-all-max (inline-array-map nonstupid-magnitude M))))

(: matrix-p-norm (case-> ((Matrix Flonum) Positive-Real -> Nonnegative-Flonum)
                         ((Matrix Real) Positive-Real -> Nonnegative-Real)
                         ((Matrix Float-Complex) Positive-Real -> Nonnegative-Flonum)
                         ((Matrix Number) Positive-Real -> Nonnegative-Real)))
(define (matrix-p-norm M p)
  (parameterize ([array-strictness #f])
    (let ([M  (array-strict (inline-array-map nonstupid-magnitude M))])
      ;; Compute this divided by the maximum to avoid underflow and overflow
      (define mx (array-all-max M))
      (cond [(and (rational? mx) (positive? mx))
             (fl (* mx (expt (array-all-sum (inline-array-map (λ (x) (expt (abs (/ x mx)) p)) M))
                             (/ p))))]
            [else  mx]))))

(: matrix-norm (case-> ((Matrix Flonum)      -> Nonnegative-Flonum)
                       ((Matrix Flonum) Real -> Nonnegative-Flonum)
                       ((Matrix Real)      -> Nonnegative-Real)
                       ((Matrix Real) Real -> Nonnegative-Real)
                       ((Matrix Float-Complex)      -> Nonnegative-Flonum)
                       ((Matrix Float-Complex) Real -> Nonnegative-Flonum)
                       ((Matrix Number)      -> Nonnegative-Real)
                       ((Matrix Number) Real -> Nonnegative-Real)))
;; Computes the p norm of a matrix
(define (matrix-norm a [p 2])
  (cond [(not (matrix? a))  (raise-argument-error 'matrix-norm "matrix?" 0 a p)]
        [(p . = . 1)       (matrix-1norm a)]
        [(p . = . 2)       (matrix-2norm a)]
        [(p . = . +inf.0)  (matrix-inf-norm a)]
        [(p . > . 1)       (matrix-p-norm a p)]
        [else  (raise-argument-error 'matrix-norm "Real >= 1" 1 a p)]))

(: matrix-dot (case-> ((Matrix Flonum) -> Nonnegative-Flonum)
                      ((Matrix Flonum) (Matrix Flonum) -> Flonum)
                      ((Matrix Real) -> Nonnegative-Real)
                      ((Matrix Real) (Matrix Real) -> Real)
                      ((Matrix Float-Complex) -> Nonnegative-Flonum)
                      ((Matrix Float-Complex) (Matrix Float-Complex) -> Float-Complex)
                      ((Matrix Number) -> Nonnegative-Real)
                      ((Matrix Number) (Matrix Number) -> Number)))
;; Computes the Frobenius inner product of a matrix with itself or of two matrices
(define matrix-dot
  (case-lambda
    [(a)
     (parameterize ([array-strictness #f])
       (assert
        (array-all-sum
         (inline-array-map
          (λ (x) (* x (conjugate x)))
          (ensure-matrix 'matrix-dot a)))
        (make-predicate Nonnegative-Real)))]
    [(a b)
     (define-values (m n) (matrix-shapes 'matrix-dot a b))
     (define aproc (unsafe-array-proc a))
     (define bproc (unsafe-array-proc b))
     (parameterize ([array-strictness #f])
       (array-all-sum
        (unsafe-build-array
         ((inst vector Index) m n)
         (λ: ([js : Indexes])
           (* (aproc js) (conjugate (bproc js)))))))]))

(: matrix-cos-angle (case-> ((Matrix Flonum) (Matrix Flonum) -> Flonum)
                            ((Matrix Real) (Matrix Real) -> Real)
                            ((Matrix Float-Complex) (Matrix Float-Complex) -> Float-Complex)
                            ((Matrix Number) (Matrix Number) -> Number)))
(define (matrix-cos-angle M N)
  (/ (matrix-dot M N) (* (matrix-2norm M) (matrix-2norm N))))

(: matrix-angle (case-> ((Matrix Flonum) (Matrix Flonum) -> Flonum)
                        ((Matrix Real) (Matrix Real) -> Real)
                        ((Matrix Float-Complex) (Matrix Float-Complex) -> Float-Complex)
                        ((Matrix Number) (Matrix Number) -> Number)))
(define (matrix-angle M N)
  (acos (matrix-cos-angle M N)))

(: matrix-normalize
   (All (A) (case-> ((Matrix Flonum)             -> (Matrix Flonum))
                    ((Matrix Flonum) Real        -> (Matrix Flonum))
                    ((Matrix Flonum) Real (-> A) -> (U A (Matrix Flonum)))
                    ((Matrix Real)             -> (Matrix Real))
                    ((Matrix Real) Real        -> (Matrix Real))
                    ((Matrix Real) Real (-> A) -> (U A (Matrix Real)))
                    ((Matrix Float-Complex)             -> (Matrix Float-Complex))
                    ((Matrix Float-Complex) Real        -> (Matrix Float-Complex))
                    ((Matrix Float-Complex) Real (-> A) -> (U A (Matrix Float-Complex)))
                    ((Matrix Number)             -> (Matrix Number))
                    ((Matrix Number) Real        -> (Matrix Number))
                    ((Matrix Number) Real (-> A) -> (U A (Matrix Number))))))
(define matrix-normalize
  (case-lambda
    [(M)  (matrix-normalize M 2)]
    [(M p)  (matrix-normalize M p (λ () (raise-argument-error
                                         'matrix-normalize "nonzero matrix?" 0 M p)))]
    [(M p fail)
     (array-strict! M)
     (define x (matrix-norm M p))
     (cond [(and (zero? x) (exact? x))  (fail)]
           [else  (matrix-scale M (/ x))])]))  

;; ===================================================================================================
;; Operators

(: matrix-transpose (All (A) (Matrix A) -> (Matrix A)))
(define (matrix-transpose a)
  (array-axis-swap (ensure-matrix 'matrix-transpose a) 0 1))

(: matrix-conjugate (case-> ((Matrix Flonum) -> (Matrix Flonum))
                            ((Matrix Real) -> (Matrix Real))
                            ((Matrix Float-Complex) -> (Matrix Float-Complex))
                            ((Matrix Number) -> (Matrix Number))))
(define (matrix-conjugate a)
  (array-conjugate (ensure-matrix 'matrix-conjugate a)))

(: matrix-hermitian (case-> ((Matrix Flonum) -> (Matrix Flonum))
                            ((Matrix Real) -> (Matrix Real))
                            ((Matrix Float-Complex) -> (Matrix Float-Complex))
                            ((Matrix Number) -> (Matrix Number))))
(define (matrix-hermitian a)
  (array-default-strict
   (parameterize ([array-strictness #f])
     (array-axis-swap (array-conjugate (ensure-matrix 'matrix-hermitian a)) 0 1))))

(: matrix-trace (case-> ((Matrix Flonum) -> Flonum)
                        ((Matrix Real) -> Real)
                        ((Matrix Float-Complex) -> Float-Complex)
                        ((Matrix Number) -> Number)))
(define (matrix-trace a)
  (cond [(square-matrix? a)
         (parameterize ([array-strictness #f])
           (array-all-sum (matrix-diagonal a)))]
        [else
         (raise-argument-error 'matrix-trace "square-matrix?" a)]))

;; ===================================================================================================
;; Row/column operations

(: matrix-map-rows
   (All (A B F) (case-> (((Matrix A) -> (Matrix B)) (Matrix A)        -> (Matrix B))
                        (((Matrix A) -> (U #f (Matrix B))) (Matrix A) (-> F)
                                                           -> (U F (Matrix B))))))
(define matrix-map-rows
  (case-lambda
    [(f M)  (matrix-stack (map f (matrix-rows M)))]
    [(f M fail)
     (define ms (matrix-rows M))
     (define n (f (first ms)))
     (cond [n  (let loop ([ms  (rest ms)] [ns (list n)])
                 (cond [(empty? ms)  (matrix-stack (reverse ns))]
                       [else  (define n (f (first ms)))
                              (cond [n  (loop (rest ms) (cons n ns))]
                                    [else  (fail)])]))]
           [else  (fail)])]))

(: matrix-map-cols
   (All (A B F) (case-> (((Matrix A) -> (Matrix B)) (Matrix A)        -> (Matrix B))
                        (((Matrix A) -> (U #f (Matrix B))) (Matrix A) (-> F)
                                                           -> (U F (Matrix B))))))
(define matrix-map-cols
  (case-lambda
    [(f M)  (matrix-augment (map f (matrix-cols M)))]
    [(f M fail)
     (define ms (matrix-cols M))
     (define n (f (first ms)))
     (cond [n  (let loop ([ms  (rest ms)] [ns (list n)])
                 (cond [(empty? ms)  (matrix-augment (reverse ns))]
                       [else  (define n (f (first ms)))
                              (cond [n  (loop (rest ms) (cons n ns))]
                                    [else  (fail)])]))]
           [else  (fail)])]))

(: make-matrix-normalize (Real -> (case-> ((Matrix Flonum) -> (U #f (Matrix Flonum)))
                                          ((Matrix Real) -> (U #f (Matrix Real)))
                                          ((Matrix Float-Complex) -> (U #f (Matrix Float-Complex)))
                                          ((Matrix Number) -> (U #f (Matrix Number))))))
(define ((make-matrix-normalize p) M)
  (matrix-normalize M p (λ () #f)))

(: matrix-normalize-rows
   (All (A) (case-> ((Matrix Flonum)             -> (Matrix Flonum))
                    ((Matrix Flonum) Real        -> (Matrix Flonum))
                    ((Matrix Flonum) Real (-> A) -> (U A (Matrix Flonum)))
                    ((Matrix Real)             -> (Matrix Real))
                    ((Matrix Real) Real        -> (Matrix Real))
                    ((Matrix Real) Real (-> A) -> (U A (Matrix Real)))
                    ((Matrix Float-Complex)             -> (Matrix Float-Complex))
                    ((Matrix Float-Complex) Real        -> (Matrix Float-Complex))
                    ((Matrix Float-Complex) Real (-> A) -> (U A (Matrix Float-Complex)))
                    ((Matrix Number)             -> (Matrix Number))
                    ((Matrix Number) Real        -> (Matrix Number))
                    ((Matrix Number) Real (-> A) -> (U A (Matrix Number))))))
(define matrix-normalize-rows
  (case-lambda
    [(M)  (matrix-normalize-rows M 2)]
    [(M p)
     (define (fail) (raise-argument-error 'matrix-normalize-rows "matrix? with nonzero rows" 0 M p))
     (matrix-normalize-rows M p fail)]
    [(M p fail)
     (matrix-map-rows (make-matrix-normalize p) M fail)]))

(: matrix-normalize-cols
   (All (A) (case-> ((Matrix Flonum)             -> (Matrix Flonum))
                    ((Matrix Flonum) Real        -> (Matrix Flonum))
                    ((Matrix Flonum) Real (-> A) -> (U A (Matrix Flonum)))
                    ((Matrix Real)             -> (Matrix Real))
                    ((Matrix Real) Real        -> (Matrix Real))
                    ((Matrix Real) Real (-> A) -> (U A (Matrix Real)))
                    ((Matrix Float-Complex)             -> (Matrix Float-Complex))
                    ((Matrix Float-Complex) Real        -> (Matrix Float-Complex))
                    ((Matrix Float-Complex) Real (-> A) -> (U A (Matrix Float-Complex)))
                    ((Matrix Number)             -> (Matrix Number))
                    ((Matrix Number) Real        -> (Matrix Number))
                    ((Matrix Number) Real (-> A) -> (U A (Matrix Number))))))
(define matrix-normalize-cols
  (case-lambda
    [(M)  (matrix-normalize-cols M 2)]
    [(M p)
     (define (fail)
       (raise-argument-error 'matrix-normalize-cols "matrix? with nonzero columns" 0 M p))
     (matrix-normalize-cols M p fail)]
    [(M p fail)
     (matrix-map-cols (make-matrix-normalize p) M fail)]))

;; ===================================================================================================

(: pairwise-orthogonal? ((Listof (Matrix Number)) Nonnegative-Real -> Boolean))
(define (pairwise-orthogonal? Ms eps)
  (define rows (list->vector Ms))
  (define m (vector-length rows))
  (let/ec: return : Boolean
    (for*: ([i0  (in-range m)] [i1  (in-range (fx+ i0 1) m)])
      (define r0 (unsafe-vector-ref rows i0))
      (define r1 (unsafe-vector-ref rows i1))
      (when ((magnitude (matrix-cos-angle r0 r1)) . >= . eps) (return #f)))
    #t))

(: matrix-rows-orthogonal? (case-> ((Matrix Number) -> Boolean)
                                   ((Matrix Number) Real -> Boolean)))
(define (matrix-rows-orthogonal? M [eps (* 10 epsilon.0)])
  (cond [(negative? eps)  (raise-argument-error 'matrix-rows-orthogonal? "Nonnegative-Real" 1 M eps)]
        [else  (parameterize ([array-strictness #f])
                 (pairwise-orthogonal? (matrix-rows M) eps))]))
         

(: matrix-cols-orthogonal? (case-> ((Matrix Number) -> Boolean)
                                   ((Matrix Number) Real -> Boolean)))
(define (matrix-cols-orthogonal? M [eps (* 10 epsilon.0)])
  (cond [(negative? eps)  (raise-argument-error 'matrix-cols-orthogonal? "Nonnegative-Real" 1 M eps)]
        [else  (parameterize ([array-strictness #f])
                 (pairwise-orthogonal? (matrix-cols M) eps))]))
