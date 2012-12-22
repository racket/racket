#lang typed/racket/base

(require racket/fixnum
         racket/list
         racket/match
         math/array
         (only-in typed/racket conjugate)
         "../unsafe.rkt"
         "../vector/vector-mutate.rkt"
         "matrix-types.rkt"
         "matrix-constructors.rkt"
         "matrix-conversion.rkt"
         "matrix-arithmetic.rkt"
         "matrix-basic.rkt"
         "matrix-column.rkt"
         "utils.rkt"
         (for-syntax racket))

; TODO:
; 1. compute null space from QR factorization
;    (better numerical stability than from Gauss elimination)
; 2. S+N decomposition
; 3. Linear least squares problems (data fitting)
; 4. Pseudo inverse
; 5. Eigenvalues and eigenvectors

(provide 
 ;; Gaussian elimination
 matrix-gauss-elim
 matrix-row-echelon
 ;; Derived functions
 matrix-rank
 matrix-nullity
 matrix-determinant
 matrix-determinant/row-reduction  ; for testing
 ;; Spaces (TODO: null space, row space, left null space)
 matrix-column-space
 ;; Solving
 matrix-invertible?
 matrix-inverse
 matrix-solve
 ;; Projection
 projection-on-orthogonal-basis
 projection-on-orthonormal-basis
 projection-on-subspace
 gram-schmidt-orthogonal
 gram-schmidt-orthonormal
 ;; Decomposition
 matrix-lu
 matrix-qr
 )

(: unsafe-vector2d-ref (All (A) ((Vectorof (Vectorof A)) Index Index -> A)))
(define (unsafe-vector2d-ref vss i j)
  (unsafe-vector-ref (unsafe-vector-ref vss i) j))

;; ===================================================================================================
;; Gaussian elimination

(: find-partial-pivot
   (case-> ((Vectorof (Vectorof Real)) Index Index Index -> (Values Index Real))
           ((Vectorof (Vectorof Number)) Index Index Index -> (Values Index Number))))
;; Find the element with maximum magnitude in a column
(define (find-partial-pivot rows m i j)
  (define l (fx+ i 1))
  (define pivot (unsafe-vector2d-ref rows i j))
  (define mag-pivot (magnitude pivot))
  (let loop ([#{l : Nonnegative-Fixnum} l] [#{p : Index} i] [pivot pivot] [mag-pivot mag-pivot])
    (cond [(l . fx< . m)
           (define new-pivot (unsafe-vector2d-ref rows l j))
           (define mag-new-pivot (magnitude new-pivot))
           (cond [(mag-new-pivot . > . mag-pivot)  (loop (fx+ l 1) l new-pivot mag-new-pivot)]
                 [else  (loop (fx+ l 1) p pivot mag-pivot)])]
          [else  (values p pivot)])))

(: elim-rows!
   (case-> ((Vectorof (Vectorof Real)) Index Index Index Real Nonnegative-Fixnum -> Void)
           ((Vectorof (Vectorof Number)) Index Index Index Number Nonnegative-Fixnum -> Void)))
(define (elim-rows! rows m i j pivot start)
  (let loop ([#{l : Nonnegative-Fixnum} start])
    (when (l . fx< . m)
      (unless (l . fx= . i)
        (define x_lj (unsafe-vector2d-ref rows l j))
        (unless (zero? x_lj)
          (vector-scaled-add! (unsafe-vector-ref rows l)
                              (unsafe-vector-ref rows i)
                              (- (/ x_lj pivot)))))
      (loop (fx+ l 1)))))

(: matrix-gauss-elim (case-> ((Matrix Real)         -> (Values (Matrix Real) (Listof Index)))
                             ((Matrix Real) Any     -> (Values (Matrix Real) (Listof Index)))
                             ((Matrix Real) Any Any -> (Values (Matrix Real) (Listof Index)))
                             ((Matrix Number)         -> (Values (Matrix Number) (Listof Index)))
                             ((Matrix Number) Any     -> (Values (Matrix Number) (Listof Index)))
                             ((Matrix Number) Any Any -> (Values (Matrix Number) (Listof Index)))))
(define (matrix-gauss-elim M [jordan? #f] [unitize-pivot-row? #f])
  (define-values (m n) (matrix-shape M))
  (define rows (matrix->vector* M))
  (let loop ([#{i : Nonnegative-Fixnum} 0]
             [#{j : Nonnegative-Fixnum} 0]
             [#{without-pivot : (Listof Index)} empty])
    (cond
      [(j . fx>= . n)
       (values (vector*->matrix rows)
               (reverse without-pivot))]
      [(i . fx>= . m)
       (values (vector*->matrix rows)
               ;; None of the rest of the columns can have pivots
               (let loop ([#{j : Nonnegative-Fixnum} j] [without-pivot without-pivot])
                 (cond [(j . fx< . n)  (loop (fx+ j 1) (cons j without-pivot))]
                       [else  (reverse without-pivot)])))]
      [else
       (define-values (p pivot) (find-partial-pivot rows m i j))
       (cond
         [(zero? pivot)  (loop i (fx+ j 1) (cons j without-pivot))]
         [else
          ;; Swap pivot row with current
          (vector-swap! rows i p)
          ;; Possibly unitize the new current row
          (let ([pivot  (if unitize-pivot-row?
                            (begin (vector-scale! (unsafe-vector-ref rows i) (/ pivot))
                                   1)
                            pivot)])
            (elim-rows! rows m i j pivot (if jordan? 0 (fx+ i 1)))
            (loop (fx+ i 1) (fx+ j 1) without-pivot))])])))

;; ===================================================================================================
;; Simple functions derived from Gaussian elimination

(: matrix-row-echelon
   (case-> ((Matrix Real)         -> (Matrix Real))
           ((Matrix Real) Any     -> (Matrix Real))
           ((Matrix Real) Any Any -> (Matrix Real))
           ((Matrix Number)         -> (Matrix Number))
           ((Matrix Number) Any     -> (Matrix Number))
           ((Matrix Number) Any Any -> (Matrix Number))))
(define (matrix-row-echelon M [jordan? #f] [unitize-pivot-row? jordan?])
  (let-values ([(M _) (matrix-gauss-elim M jordan? unitize-pivot-row?)])
    M))

(: matrix-rank : (Matrix Number) -> Index)
;; Returns the dimension of the column space (equiv. row space) of M
(define (matrix-rank M)
  ; TODO: Use QR or SVD instead for inexact matrices
  ; See answer: http://scicomp.stackexchange.com/questions/1861/understanding-how-numpy-does-svd
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

(: matrix-column-space (All (A) (case-> ((Matrix Real)        -> (Matrix Real))
                                        ((Matrix Real) (-> A) -> (U A (Matrix Real)))
                                        ((Matrix Number)        -> (Matrix Number))
                                        ((Matrix Number) (-> A) -> (U A (Matrix Number))))))
(define matrix-column-space
  (case-lambda
    [(M)  (matrix-column-space M (λ () (make-array (vector 0 (matrix-num-cols M)) 0)))]
    [(M fail)
     (define n (matrix-num-cols M))
     (define-values (_ wps) (matrix-gauss-elim M))
     (cond [(empty? wps)  M]
           [(= (length wps) n)  (fail)]
           [else
            (define next-j (first wps))
            (define Bs (maybe-cons-submatrix M 0 next-j empty))
            (let loop ([#{j : Index} next-j] [wps (rest wps)] [Bs Bs])
              (cond [(empty? wps)
                     (matrix-augment (reverse (maybe-cons-submatrix M (fx+ j 1) n Bs)))]
                    [else
                     (define next-j (first wps))
                     (loop next-j (rest wps) (maybe-cons-submatrix M (fx+ j 1) next-j Bs))]))])]))

;; ===================================================================================================
;; Determinant

(: matrix-determinant (case-> ((Matrix Real) -> Real)
                              ((Matrix Number) -> Number)))
(define (matrix-determinant M)
  (define m (square-matrix-size M))
  (cond
    [(= m 1)  (matrix-ref M 0 0)]
    [(= m 2)  (match-define (vector a b c d)
                (mutable-array-data (array->mutable-array M)))
              (- (* a d) (* b c))]
    [(= m 3)  (match-define (vector a b c d e f g h i)
                (mutable-array-data (array->mutable-array M)))
              (+ (*    a  (- (* e i) (* f h)))
                 (* (- b) (- (* d i) (* f g)))
                 (*    c  (- (* d h) (* e g))))]
    [else
     (matrix-determinant/row-reduction M)]))

(: matrix-determinant/row-reduction (case-> ((Matrix Real) -> Real)
                                            ((Matrix Number) -> Number)))
(define (matrix-determinant/row-reduction M)
  (define m (square-matrix-size M))
  (define rows (matrix->vector* M))
  (let loop ([#{i : Nonnegative-Fixnum} 0] [#{sign : Real} 1])
    (cond
      [(i . fx< . m)
       (define-values (p pivot) (find-partial-pivot rows m i i))
       (cond
         [(zero? pivot)  0]  ; no pivot means non-invertible matrix
         [else
          (vector-swap! rows i p)                  ; negates determinant if i != p
          (elim-rows! rows m i i pivot (fx+ i 1))  ; doesn't change the determinant
          (loop (fx+ i 1) (if (= i p) sign (* -1 sign)))])]
      [else
       (define prod (unsafe-vector2d-ref rows 0 0))
       (let loop ([#{i : Nonnegative-Fixnum} 1] [prod prod])
         (cond [(i . fx< . m)
                (loop (fx+ i 1) (* prod (unsafe-vector2d-ref rows i i)))]
               [else  (* prod sign)]))])))

;; ===================================================================================================
;; Inversion and solving linear systems

(: matrix-invertible? ((Matrix Number) -> Boolean))
(define (matrix-invertible? M)
  (not (zero? (matrix-determinant M))))

(: matrix-inverse (All (A) (case-> ((Matrix Real)        -> (Matrix Real))
                                   ((Matrix Real) (-> A) -> (U A (Matrix Real)))
                                   ((Matrix Number)        -> (Matrix Number))
                                   ((Matrix Number) (-> A) -> (U A (Matrix Number))))))
(define matrix-inverse
  (case-lambda
    [(M)  (matrix-inverse M (λ () (raise-argument-error 'matrix-inverse "matrix-invertible?" M)))]
    [(M fail)
     (define m (square-matrix-size M))
     (define I (identity-matrix m))
     (define-values (IM^-1 wps) (matrix-gauss-elim (matrix-augment (list M I)) #t #t))
     (cond [(and (not (empty? wps)) (= (first wps) m))
            (submatrix IM^-1 (::) (:: m #f))]
           [else  (fail)])]))

(: matrix-solve (All (A) (case->
                          ((Matrix Real) (Matrix Real)        -> (Matrix Real))
                          ((Matrix Real) (Matrix Real) (-> A) -> (U A (Matrix Real)))
                          ((Matrix Number) (Matrix Number)        -> (Matrix Number))
                          ((Matrix Number) (Matrix Number) (-> A) -> (U A (Matrix Number))))))
(define matrix-solve
  (case-lambda
    [(M B)  (matrix-solve M B (λ () (raise-argument-error 'matrix-solve "matrix-invertible?" 0 M B)))]
    [(M B fail)
     (define m (square-matrix-size M))
     (define-values (s t) (matrix-shape B))
     (cond [(= m s)
            (define-values (IX wps) (matrix-gauss-elim (matrix-augment (list M B)) #t #t))
            (cond [(and (not (empty? wps)) (= (first wps) m))
                   (submatrix IX (::) (:: m #f))]
                  [else  (fail)])]
           [else
            (error 'matrix-solve
                   "matrices must have the same number of rows; given ~e and ~e"
                   M B)])]))

;; ===================================================================================================
;; LU Factorization

;; An LU factorization exists iff Gaussian elimination can be done without row swaps.

(: matrix-lu
   (All (A) (case-> ((Matrix Real)        -> (Values (Matrix Real) (Matrix Real)))
                    ((Matrix Real) (-> A) -> (Values (U A (Matrix Real)) (Matrix Real)))
                    ((Matrix Number)        -> (Values (Matrix Number) (Matrix Number)))
                    ((Matrix Number) (-> A) -> (Values (U A (Matrix Number)) (Matrix Number))))))
(define matrix-lu
  (case-lambda
    [(M)  (matrix-lu M (λ () (raise-argument-error 'matrix-lu "LU-decomposable matrix" M)))]
    [(M fail)
     (define m (square-matrix-size M))
     (define rows (matrix->vector* M))
     ;; Construct L in a weird way to prove to TR that it has the right type
     (define L (array->mutable-array (matrix-scale M (ann 0 Real))))
     ;; Going to fill in the lower triangle by banging values into `ys'
     (define ys (mutable-array-data L))
     (let loop ([#{i : Nonnegative-Fixnum} 0])
       (cond
         [(i . fx< . m)
          ;; Pivot must be on the diagonal
          (define pivot (unsafe-vector2d-ref rows i i))
          (cond
            [(zero? pivot)  (values (fail) M)]
            [else
             ;; Zero out everything below the pivot
             (let l-loop ([#{l : Nonnegative-Fixnum} (fx+ i 1)])
               (cond
                 [(l . fx< . m)
                  (define x_li (unsafe-vector2d-ref rows l i))
                  (define y_li (/ x_li pivot))
                  (unless (zero? x_li)
                    ;; Fill in lower triangle of L
                    (unsafe-vector-set! ys (+ (* l m) i) y_li)
                    ;; Add row i, scaled
                    (vector-scaled-add! (unsafe-vector-ref rows l)
                                        (unsafe-vector-ref rows i)
                                        (- y_li)))
                  (l-loop (fx+ l 1))]
                 [else
                  (loop (fx+ i 1))]))])]
         [else
          ;; L's lower triangle has been filled; now fill the diagonal with 1s
          (for: ([i : Integer (in-range 0 m)])
            (vector-set! ys (+ (* i m) i) 1))
          (values L (vector*->matrix rows))]))]))

;; ===================================================================================================
;; Projections and orthogonalization

(: projection-on-orthogonal-basis : 
   (Column Number) (Listof (Column Number)) -> (Result-Column Number))
; (projection-on-orthogonal-basis v bs)
;     Project the vector v on the orthogonal basis vectors in bs.
;     The basis bs must be either the column vectors of a matrix
;     or a sequence of column-vectors.
(define (projection-on-orthogonal-basis v bs)
  (if (null? bs)
      (error 'projection-on-orthogonal-basis 
             "received empty list of basis vectors")
      (matrix-sum (map (λ: ([b : (Column Number)])
                         (column-project v (->col-matrix b)))
                       bs))))
  
; (projection-on-orthonormal-basis v bs)
;     Project the vector v on the orthonormal basis vectors in bs.
;     The basis bs must be either the column vectors of a matrix
;     or a sequence of column-vectors.
(: projection-on-orthonormal-basis : 
   (Column Number) (Listof (Column Number)) -> (Result-Column Number))
(define (projection-on-orthonormal-basis v bs)
  #;(for/matrix-sum ([b bs]) (matrix-scale b (column-dot v b)))
  (define: sum : (U False (Result-Column Number)) #f)
  (for ([b1 (in-list bs)])
    (define: b : (Result-Column Number) (->col-matrix b1))
    (cond [(not sum) (set! sum (column-project/unit v b))]
          [else      (set! sum (array+ (assert sum) (column-project/unit v b)))]))
  (cond [sum (assert sum)]
        [else (error 'projection-on-orthonormal-basis 
                     "received empty list of basis vectors")]))

(: gram-schmidt-orthogonal : (Listof (Column Number)) -> (Listof (Result-Column Number)))
; (gram-schmidt-orthogonal ws)
;     Given a list ws of column vectors, produce 
;     an orthogonal basis for the span of the
;     vectors in ws.
(define (gram-schmidt-orthogonal ws1)
  (define ws (map (λ: ([w : (Column Number)]) (->col-matrix w)) ws1))
  (cond 
    [(null? ws)       '()]
    [(null? (cdr ws)) (list (car ws))]
    [else 
     (: loop : (Listof (Result-Column Number)) (Listof (Column-Matrix Number))
        -> (Listof (Result-Column Number)))
     (define (loop vs ws)
       (cond [(null? ws) vs]
             [else
              (define w (car ws))
              (let ([w-proj (projection-on-orthogonal-basis w vs)])
                ; Note: We project onto vs (not on the original ws)
                ;       in order to get numerical stability.
                (let ([w-minus-proj (array-strict (array- w w-proj))])
                  (if (matrix-zero? w-minus-proj)
                      (loop vs (cdr ws)) ; w in span{vs} => omit it
                      (loop (cons w-minus-proj vs) (cdr ws)))))]))
     (reverse (loop (list (car ws)) (cdr ws)))]))

(: gram-schmidt-orthonormal : (Listof (Column Number)) -> (Listof (Result-Column Number)))
; (gram-schmidt-orthonormal ws)
;     Given a list ws of column vectors, produce 
;     an orthonormal basis for the span of the
;     vectors in ws.
(define (gram-schmidt-orthonormal ws)
  (map column-normalize (gram-schmidt-orthogonal ws)))

(: projection-on-subspace :
   (Column Number) (Listof (Column Number)) -> (Result-Column Number))
; (projection-on-subspace v ws)
;    Returns the projection of v on span{w_i}, w_i in ws.
(define (projection-on-subspace v ws)
  (projection-on-orthogonal-basis v (gram-schmidt-orthogonal ws)))

(: extend-span-to-basis :
   (Listof (Matrix Number)) Integer -> (Listof (Matrix Number)))
; Extend the basis in vs to with rdimensional basis
(define (extend-span-to-basis vs r)
  (define-values (m n) (matrix-shape (car vs)))
  (: loop : (Listof (Matrix Number)) (Listof (Matrix Number)) Integer -> (Listof (Matrix Number)))
  (define (loop vs ws i)
    (if (>= i m)
        ws
        (let ()
          (define ei (unit-column m i))
          (define pi (projection-on-subspace ei vs))
          (if (matrix= ei pi)
              (loop vs ws (+ i 1))
              (let ([w (array- ei pi)])
                (loop (cons w vs) (cons w ws) (+ i 1)))))))
  (: norm> : (Matrix Number) (Matrix Number) -> Boolean)
  (define (norm> v w)
    (> (column-norm v) (column-norm w)))
  (if (index? r)
      (take (sort (loop vs '() 0) norm>) r)
      (error 'extend-span-to-basis "expected index as second argument, got ~a" r)))

;; ===================================================================================================
;; QR decomposition

(: matrix-qr : (Matrix Number) -> (Values (Matrix Number) (Matrix Number)))
(define (matrix-qr M)
  ; compute the QR-facorization
  ; 1) QR = M 
  ; 2) columns of Q is are orthonormal
  ; 3) R is upper-triangular
  ; Note: columnspace(A)=columnspace(Q) !
  (define-values (m n) (matrix-shape M))
  (let* ([basis-for-column-space
          (gram-schmidt-orthonormal (matrix-cols M))]
         [extension
          (extend-span-to-basis 
           basis-for-column-space (- n (length basis-for-column-space)))]
         [Q (matrix-augment
             (append basis-for-column-space 
                     (map column-normalize
                          extension)))]
         [R 
          (let ()
            (define v (make-vector (* n n) (ann 0 Number)))
            (for*: ([i (in-range 0 n)]
                    [j (in-range 0 n)])
              (if (> i j) 
                  (void) ; v(i,j)=0 already
                  (let ()
                    (define: sum : Number 0)
                    (for: ([k (in-range m)])
                      (set! sum (+ sum (* (matrix-ref Q k i)
                                          (matrix-ref M k j)))))
                    (vector-set! v (+ (* i n) j) sum))))
            (vector->matrix n n v))])
    (values Q R)))
