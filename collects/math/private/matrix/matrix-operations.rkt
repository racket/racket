#lang typed/racket/base

(require racket/fixnum
         racket/list
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
         (for-syntax racket))

; TODO:
; 1. compute null space from QR factorization
;    (better numerical stability than from Gauss elimnation)
; 2. S+N decomposition
; 3. Linear least squares problems (data fitting)
; 4. Pseudo inverse
; 5. Eigenvalues and eigenvectors

(provide 
 matrix-inverse
 ; row and column
 matrix-scale-row
 matrix-scale-column
 matrix-swap-rows
 matrix-swap-columns
 matrix-add-scaled-row
 ; reduction
 matrix-gauss-eliminate
 matrix-gauss-jordan-eliminate
 matrix-row-echelon-form
 matrix-reduced-row-echelon-form
 ; invariant
 matrix-rank
 matrix-nullity
 matrix-determinant
 ; spaces
 ;matrix-column+null-space
 ; solvers
 matrix-solve
 matrix-solve-many
 ; spaces
 matrix-column-space
 ; projection
 projection-on-orthogonal-basis
 projection-on-orthonormal-basis
 projection-on-subspace
 gram-schmidt-orthogonal
 gram-schmidt-orthonormal
 ; factorization
 matrix-lu
 matrix-qr
 )

;;;
;;; Row and column
;;;

(: matrix-scale-row : (Matrix Number) Integer Number -> (Matrix Number))
(define (matrix-scale-row a i c)
  ((inline-matrix-scale-row i c) a))

(define-syntax (inline-matrix-scale-row stx)
  (syntax-case stx ()
    [(_ i c)
     (syntax/loc stx
       (λ (arr)
         (define ds (array-shape arr))
         (define g (unsafe-array-proc arr))
         (cond
           [(< i 0)
            (error 'matrix-scale-row "row index must be non-negative, got ~a" i)]
           [(not (< i (vector-ref ds 0)))
            (error 'matrix-scale-row "row index must be smaller than the number of rows, got ~a" i)]
           [else
            (unsafe-build-array ds (λ: ([js : (Vectorof Index)]) 
                                     (if (= i (vector-ref js 0))
                                         (* c (g js))
                                         (g js))))])))]))

(: matrix-scale-column : (Matrix Number) Integer Number -> (Matrix Number))
(define (matrix-scale-column a i c)
  ((inline-matrix-scale-column i c) a))

(define-syntax (inline-matrix-scale-column stx)
  (syntax-case stx ()
    [(_ j c)
     (syntax/loc stx
       (λ (arr)
         (define ds (array-shape arr))
         (define g (unsafe-array-proc arr))
         (cond
           [(< j 0)
            (error 'matrix-scale-row "column index must be non-negative, got ~a" j)]
           [(not (< j (vector-ref ds 1)))
            (error 'matrix-scale-row "column index must be smaller than the number of rows, got ~a" j)]
           [else
            (unsafe-build-array ds (λ: ([js : (Vectorof Index)]) 
                                     (if (= j (vector-ref js 1))
                                         (* c (g js))
                                         (g js))))])))]))

(: matrix-swap-rows : (Matrix Number) Integer Integer -> (Matrix Number))
(define (matrix-swap-rows a i j)
  ((inline-matrix-swap-rows i j) a))

(define-syntax (inline-matrix-swap-rows stx)
  (syntax-case stx ()
    [(_ i j)
     (syntax/loc stx
       (λ (arr)
         (define ds (array-shape arr))
         (define g (unsafe-array-proc arr))
         (cond
           [(< i 0)
            (error 'matrix-swap-rows "row index must be non-negative, got ~a" i)]
           [(< j 0)
            (error 'matrix-swap-rows "row index must be non-negative, got ~a" j)]
           [(not (< i (vector-ref ds 0)))
            (error 'matrix-swap-rows "row index must be smaller than the number of rows, got ~a" i)]
           [(not (< j (vector-ref ds 0)))
            (error 'matrix-swap-rows "row index must be smaller than the number of rows, got ~a" j)]
           [else
            (unsafe-build-array ds (λ: ([js : (Vectorof Index)]) 
                                     (cond
                                       [(= i (vector-ref js 0)) 
                                        (g (vector j (vector-ref js 1)))]
                                       [(= j (vector-ref js 0)) 
                                        (g (vector i (vector-ref js 1)))]
                                       [else                    
                                        (g js)])))])))]))

(: matrix-swap-columns : (Matrix Number) Integer Integer -> (Matrix Number))
(define (matrix-swap-columns a i j)
  ((inline-matrix-swap-columns i j) a))

(define-syntax (inline-matrix-swap-columns stx)
  (syntax-case stx ()
    [(_ i j)
     (syntax/loc stx
       (λ (arr)
         (define ds (array-shape arr))
         (define g (unsafe-array-proc arr))
         (cond
           [(< i 0)
            (error 'matrix-swap-columns "column index must be non-negative, got ~a" i)]
           [(< j 0)
            (error 'matrix-swap-columns "column index must be non-negative, got ~a" j)]
           [(not (< i (vector-ref ds 0)))
            (error 'matrix-swap-columns "column index must be smaller than the number of columns, got ~a" i)]
           [(not (< j (vector-ref ds 0)))
            (error 'matrix-swap-columns "column index must be smaller than the number of columns, got ~a" j)]
           [else
            (unsafe-build-array ds (λ: ([js : (Vectorof Index)]) 
                                     (cond
                                       [(= i (vector-ref js 1)) 
                                        (g (vector j (vector-ref js 1)))]
                                       [(= j (vector-ref js 1)) 
                                        (g (vector i (vector-ref js 1)))]
                                       [else                    
                                        (g js)])))])))]))

(: matrix-add-scaled-row : (Matrix Number) Integer Number Integer -> (Matrix Number))
(define (matrix-add-scaled-row a i c j)
  ((inline-matrix-add-scaled-row i c j) a))

(: flmatrix-add-scaled-row : (Matrix Flonum) Index Flonum Index -> (Matrix Flonum))
(define (flmatrix-add-scaled-row a i c j)
  ((inline-matrix-add-scaled-row i c j) a))

(define-syntax (inline-matrix-add-scaled-row stx)
  (syntax-case stx ()
    [(_ i c j)
     (syntax/loc stx
       (λ (arr)
         (define ds (array-shape arr))
         (define g (unsafe-array-proc arr))
         (cond
           [(< i 0)
            (error 'matrix-add-scaled-row "row index must be non-negative, got ~a" i)]
           [(< j 0)
            (error 'matrix-add-scaled-row "row index must be non-negative, got ~a" j)]
           [(not (< i (vector-ref ds 0)))
            (error 'matrix-add-scaled-row 
                   "row index must be smaller than the number of rows, got ~a" i)]
           [(not (< j (vector-ref ds 0)))
            (error 'matrix-add-scaled-row 
                   "row index must be smaller than the number of rows, got ~a" j)]
           [else
            (unsafe-build-array ds (λ: ([js : (Vectorof Index)]) 
                                     (if (= i (vector-ref js 0))
                                         (+ (g js) (* c (g (vector j (vector-ref js 1)))))
                                         (g js))))])))]))


;;; GAUSS ELIMINATION / ROW ECHELON FORM

(: unsafe-vector2d-ref (All (A) ((Vectorof (Vectorof A)) Index Index -> A)))
(define (unsafe-vector2d-ref vss i j)
  (unsafe-vector-ref (unsafe-vector-ref vss i) j))

(: find-partial-pivot (case-> ((Vectorof (Vectorof Real)) Index Index Index -> (U #f Index))
                              ((Vectorof (Vectorof Number)) Index Index Index -> (U #f Index))))
;; Find the element with maximum magnitude in a column
(define (find-partial-pivot rows m i j)
  (let loop ([#{l : Nonnegative-Fixnum} i] [#{max-current : Real} -inf.0] [#{max-index : Index} i])
    (cond [(l . fx< . m)
           (define v (magnitude (unsafe-vector2d-ref rows l j)))
           (cond [(> v max-current)  (loop (fx+ l 1) v l)]
                 [else  (loop (fx+ l 1) max-current max-index)])]
          [else  max-index])))

(: find-pivot (case-> ((Vectorof (Vectorof Real)) Index Index Index -> (U #f Index))
                      ((Vectorof (Vectorof Number)) Index Index Index -> (U #f Index))))
;; Find a non-zero element in a column
(define (find-pivot rows m i j)
  (let loop ([#{l : Nonnegative-Fixnum} i])
    (cond [(l . fx>= . m)  #f]
          [(not (zero? (unsafe-vector2d-ref rows l j)))  l]
          [else  (loop (fx+ l 1))])))

(: matrix-gauss-eliminate : 
   (case-> ((Matrix Real)                 -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Real) Boolean         -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Real) Boolean Boolean -> (Values (Matrix Real) (Listof Index)))
           ((Matrix Number)                 -> (Values (Matrix Number) (Listof Index)))
           ((Matrix Number) Boolean         -> (Values (Matrix Number) (Listof Index)))
           ((Matrix Number) Boolean Boolean -> (Values (Matrix Number) (Listof Index)))))
;; Returns the result of Gaussian elimination and a list of column indexes that had no pivot value
;; If `reduced?' is #t, the result is in *reduced* row-echelon form, and is unique (up to
;; floating-point error)
;; If `partial-pivoting?' is #t, the largest value in each column is used as the pivot
(define (matrix-gauss-eliminate M [reduced? #f] [partial-pivoting? #t])
  (define-values (m n) (matrix-shape M))
  (define rows (matrix->vector* M))
  (let loop ([#{i : Nonnegative-Fixnum} 0]
             [#{j : Nonnegative-Fixnum} 0]
             [#{without-pivot : (Listof Index)}  '()])
    (cond
      [(and (i . fx< . m) (j . fx< . n))
       ;; Find the row with the pivot value
       (define p (cond [partial-pivoting?  (find-partial-pivot rows m i j)]
                       [else  (find-pivot rows m i j)]))
       (define pivot (if p (unsafe-vector2d-ref rows p j) 0))
       (cond
         [(or (not p) (zero? pivot))  ; didn't find pivot?
          (loop i (fx+ j 1) (cons j without-pivot))]
         [else 
          (vector-swap! rows i p)  ; swap pivot row with current
          (let ([pivot  (cond [reduced?  (vector-scale! (unsafe-vector-ref rows i) (/ pivot))
                                         (/ pivot pivot)]
                              [else  pivot])])
            ;; Remove elements below pivot by scaling and adding the pivot's row to each row below
            (let l-loop ([#{l : Nonnegative-Fixnum} (fx+ i 1)])
              (cond [(l . fx< . m)
                     (define x_lj (unsafe-vector2d-ref rows l j))
                     (unless (zero? x_lj)
                       (vector-scaled-add! (unsafe-vector-ref rows l)
                                           (unsafe-vector-ref rows i)
                                           (- (/ x_lj pivot))))
                     (l-loop (fx+ l 1))]
                    [else
                     (loop (fx+ i 1) (fx+ j 1) without-pivot)])))])]
      [else
       (values (vector*->matrix rows)
               (reverse without-pivot))])))

(: matrix-rank : (Matrix Number) -> Integer)
(define (matrix-rank M)
  ; TODO: Use QR or SVD instead for inexact matrices
  ; See answer: http://scicomp.stackexchange.com/questions/1861/understanding-how-numpy-does-svd
  ; rank = dimension of column space = dimension of row space
  (define-values (m n) (matrix-shape M))
  (define-values (_ cols-without-pivot) (matrix-gauss-eliminate M))
  (- n (length cols-without-pivot)))

(: matrix-nullity : (Matrix Number) -> Integer)
(define (matrix-nullity M)
  ; nullity = dimension of null space
  (define-values (m n) (matrix-shape M))
  (define-values (_ cols-without-pivot) (matrix-gauss-eliminate M))
  (length cols-without-pivot))

(: matrix-determinant : (Matrix Number) -> Number)
(define (matrix-determinant M)
  (define-values (m n) (matrix-shape M))
  (cond
    [(= m 1) (matrix-ref M 0 0)]
    [(= m 2) (let ([a (matrix-ref M 0 0)]
                   [b (matrix-ref M 0 1)]
                   [c (matrix-ref M 1 0)]
                   [d (matrix-ref M 1 1)])
               (- (* a d) (* b c)))]
    [(= m 3) (let ([a (matrix-ref M 0 0)]
                   [b (matrix-ref M 0 1)]
                   [c (matrix-ref M 0 2)]
                   [d (matrix-ref M 1 0)]
                   [e (matrix-ref M 1 1)]
                   [f (matrix-ref M 1 2)]
                   [g (matrix-ref M 2 0)]
                   [h (matrix-ref M 2 1)]
                   [i (matrix-ref M 2 2)])
               (+ (*    a  (- (* e i) (* f h)))
                  (* (- b) (- (* d i) (* f g)))
                  (*    c  (- (* d h) (* e g)))))]
    [else           
     (let-values ([(M _) (matrix-gauss-eliminate M #f #f)])
       ; TODO: #f #f turns off partial pivoting
       #; (for/product: : Number ([i (in-range 0 m)])
            (matrix-ref M i i))
       (let ()
         (define: product : Number 1)
         (for: ([i : Integer (in-range 0 m 1)])
           (set! product (* product (matrix-ref M i i))))
         product))]))

(: matrix-column-space : (Matrix Number) -> (Listof (Matrix Number)))
; Returns
;  1) a list of column vectors spanning the column space
;  2) a list of column vectors spanning the null space
(define (matrix-column-space M)  
  (define-values (m n) (matrix-shape M))
  (: M1 (Matrix Number))
  (: cols-without-pivot (Listof Integer))
  (define-values (M1 cols-without-pivot) (matrix-gauss-eliminate M #t))
  (set! M1 (array->mutable-array M1))  
  (define: column-space : (Listof (Matrix Number))
    (for/list:
        ([i : Index n]
         #:when (not (member i cols-without-pivot)))
      (matrix-col M1 i)))
  column-space)

(: matrix-row-echelon-form : 
   (case-> ((Matrix Number) Boolean -> (Matrix Number))
           ((Matrix Number) Boolean -> (Matrix Number))
           ((Matrix Number)         -> (Matrix Number))))
(define (matrix-row-echelon-form M [unitize-pivot-row? #f])
  (let-values ([(M wp) (matrix-gauss-eliminate M unitize-pivot-row?)])
    M))

(: matrix-gauss-jordan-eliminate : 
   (case-> ((Matrix Number) Boolean Boolean -> (Values (Matrix Number) (Listof Integer)))
           ((Matrix Number) Boolean         -> (Values (Matrix Number) (Listof Integer)))
           ((Matrix Number)                 -> (Values (Matrix Number) (Listof Integer)))))
(define (matrix-gauss-jordan-eliminate M [unitize-pivot-row? #f] [partial-pivoting? #t])
  (define-values (m n) (matrix-shape M))
  (: loop : (Integer Integer (Matrix Number) Integer (Listof Integer) 
                     -> (Values (Matrix Number) (Listof Integer))))
  (define (loop i j ; i from 0 to m
                M
                k   ; count rows without pivot
                without-pivot) 
    (cond
      [(or (= i m) (= j n)) (values M without-pivot)]
      [else
       ; find row to become pivot
       (define p
         (if partial-pivoting?
             ; find element with maximal absolute value
             (let: max-loop : (U False Integer) 
               ([l : Integer i] ; i<=l<m
                [max-current : Real -inf.0]
                [max-index : Integer i])
               (cond
                 [(= l m) max-index]
                 [else 
                  (let ([v (magnitude (matrix-ref M l j))])
                    (if (> (magnitude (matrix-ref M l j)) max-current)
                        (max-loop (+ l 1) v l)
                        (max-loop (+ l 1) max-current max-index)))]))
             ; find non-zero element in column
             (let: first-loop : (U False Integer) 
               ([l : Integer i]) ; i<=l<m
               (cond
                 [(= l m) #f]
                 [(not (zero? (matrix-ref M l j))) l]
                 [else (first-loop (+ l 1))]))))
       (cond
         [(eq? p #f)
          ; no pivot found - this implies the matrix is singular (not invertible)
          (loop i (+ j 1) M (+ k 1) (cons j without-pivot))]
         [else 
          ; swap if neccessary
          (let* ([M (if (= i p) M (matrix-swap-rows M i p))]
                 ; now we now (i,j) is a pivot
                 [M ; maybe scale row
                  (if unitize-pivot-row?
                      (let ([pivot (matrix-ref M i j)])
                        (if (zero? pivot)
                            M
                            (matrix-scale-row M i (/ pivot))))
                      M)])
            (let ([pivot (matrix-ref M i j)])
              ; remove elements above and below pivot
              (let l-loop ([l 0] [M M])
                (cond
                  [(= l m) (loop (+ i 1) (+ j 1) M k without-pivot)]
                  [(= l i) (l-loop (+ l 1) M)]
                  [else
                   (let ([x_lj (matrix-ref M l j)])
                     (l-loop (+ l 1)
                             (if (zero? x_lj)
                                 M
                                 (matrix-add-scaled-row M l (- (/ x_lj pivot)) i))))]))))])]))
  (let-values ([(M without) (loop 0 0 M 0 '())])
    (values M without)))

(: matrix-reduced-row-echelon-form : 
   (case-> ((Matrix Number) Boolean -> (Matrix Number))
           ((Matrix Number) Boolean -> (Matrix Number))
           ((Matrix Number)         -> (Matrix Number))))
(define (matrix-reduced-row-echelon-form M [unitize-pivot-row? #f])
  (let-values ([(M wp) (matrix-gauss-jordan-eliminate M unitize-pivot-row?)])
    M))

(: matrix-inverse : (Matrix Number) -> (Matrix Number))
(define (matrix-inverse M)
  (define-values (m n) (matrix-shape M))
  (unless (= m n) (error 'matrix-inverse "matrix not square"))
  (let ([MI (matrix-augment (list M (identity-matrix m)))])
    (define 2m (* 2 m))
    (if (index? 2m)
        (submatrix (matrix-reduced-row-echelon-form MI #t) 
                   (in-range 0 m) (in-range m 2m))
        (error 'matrix-inverse "internal error"))))

(: matrix-solve : (Matrix Number) (Matrix Number) -> (Matrix Number))
;    Return a column-vector x such that Mx = b.
;    If no such vector exists return #f.
(define (matrix-solve M b)
  (define-values (m n) (matrix-shape M))
  (define-values (s t) (matrix-shape b))
  (define m+1 (+ m 1))
  (cond
    [(not (= t 1)) (error 'matrix-solve "expected column vector (i.e. r x 1 - matrix), got: ~a " b)]
    [(not (= m s)) (error 'matrix-solve "expected column vector with same number of rows as the matrix")]
    [(index? m+1)
     (submatrix
      (matrix-reduced-row-echelon-form 
       (matrix-augment (list M b)) #t)
      (in-range 0 m) (in-range m m+1))]
    [else (error 'matrix-solve "internatl error")]))

(: matrix-solve-many : (Matrix Number) (Listof (Matrix Number)) -> (Matrix Number))
(define (matrix-solve-many M bs)
  (define-values (m n) (matrix-shape M))
  (define-values (s t) (matrix-shape (car bs)))
  (define k (length bs))
  (define m+1 (+ m 1))
  (define m+k (+ m k))
  (cond
    [(not (= t 1)) (error 'matrix-solve-many "expected column vector (i.e. r x 1 - matrix), got: ~a " (car bs))]
    [(not (= m s)) (error 'matrix-solve-many "expected column vectors with same number of rows as the matrix")]
    [(and (index? m+1) (index? m+k)) 
     (define bs-as-matrix (matrix-augment bs))     
     (define MB (matrix-augment (list M bs-as-matrix)))
     (define reduced-MB (matrix-reduced-row-echelon-form MB #t))
     (submatrix reduced-MB 
                (in-range 0 m+k)
                (in-range m m+1))]
    [else (error 'matrix-solve-many "internal error")]))


;;; LU Factorization
; Not all matrices can be LU-factored.
; If Gauss-elimination can be done without any row swaps,
; a LU-factorization is possible.

(: matrix-lu : 
   (Matrix Number) -> (U False (List (Matrix Number) (Matrix Number))))
(define (matrix-lu M)
  (define-values (m _) (matrix-shape M))
  (define: ms : (Listof Number) '())
  (define V
    (let/ec: return : (U False (Matrix Number))
      (let: i-loop : (Matrix Number)
        ([i : Integer 0]
         [V : (Matrix Number) M])
        (cond
          [(= i m) V]
          [else
           ; Gauss: find non-zero element
           ; LU:    this has to be the first
           (let ([x_ii (matrix-ref V i i)])
             (cond
               [(zero? x_ii)
                (return #f)] ; no LU - factorization possible
               [else
                ; remove elements below pivot
                (let j-loop ([j (+ i 1)] [V V])
                  (cond
                    [(= j m) (i-loop (+ i 1) V)]
                    [else
                     (let* ([x_ji (matrix-ref V j i)]
                            [m_ij (/ x_ji x_ii)])
                       (set! ms (cons m_ij ms))
                       (j-loop (+ j 1)
                               (if (zero? x_ji)
                                   V
                                   (matrix-add-scaled-row V j (- m_ij) i))))]))]))]))))
  
  ; Now M has been transformed to U.  
  (if (eq? V #f)
      #f
      (let ()  
        (define: L-matrix : (Vectorof Number) (make-vector (* m m) 0))
        ; fill below diagonal
        (set! ms (reverse ms))
        (for*: ([j : Integer (in-range 0 m)]
                [i : Integer (in-range (+ j 1) m)])
          (vector-set! L-matrix (+ (* i m) j) (car ms))
          (set! ms (cdr ms)))
        ; fill diagonal
        (for: ([i : Integer (in-range 0 m)])
          (vector-set! L-matrix (+ (* i m) i) 1))
        
        (define: L : (Matrix Number)
          (let ([ds (array-shape M)])
            (unsafe-build-array
             ds (λ: ([js : (Vectorof Index)])
                  (define i (unsafe-vector-ref js 0))
                  (define j (unsafe-vector-ref js 1))
                  (vector-ref L-matrix (+ (* i m) j))))))
        (list L V))))


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
