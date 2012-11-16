#lang typed/racket/base

(require math/array
         (only-in typed/racket conjugate)
         "../unsafe.rkt"
         "matrix-types.rkt"
         "matrix-constructors.rkt"
         "matrix-pointwise.rkt"
         (for-syntax racket))


; TODO:
; 1. compute null space from QR factorization
;    (better numerical stability than from Gauss elimnation)
; 2. S+N decomposition
; 3. Linear least squares problems (data fitting)
; 4. Pseudo inverse
; 5. Eigenvalues and eigenvectors

; 6. "Bug"
;    (for*/matrix : Number 2 3 ([i (in-naturals)]) i)
;    ought to generate a matrix with numbers from 0 to 5.
;    Problem: In expansion of for/matrix an extra [i (in-range (* m n))]
;    is added to make sure the comprehension stops.
;    But TR has problems with #:when so what is the proper expansion ?

(provide 
 ; basic
 matrix-ref
 matrix-scale
 matrix-row-vector?
 matrix-column-vector?
 matrix/dim     ; construct
 matrix-augment ; horizontally
 matrix-stack   ; vertically
 matrix-block-diagonal
 ; norms
 matrix-norm
 ; operators
 matrix-transpose
 matrix-conjugate
 matrix-hermitian
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
 matrix-trace
 ; spaces
 ;matrix-column+null-space
 ; solvers
 matrix-solve
 matrix-solve-many
 ; spaces
 matrix-column-space
 ; column vectors
 column        ; construct
 unit-column
 result-column ; convert to lazy
 column-dimension
 column-dot
 column-norm
 column-projection
 column-normalize 
 scale-column
 column+
 ; projection
 projection-on-orthogonal-basis
 projection-on-orthonormal-basis
 projection-on-subspace
 gram-schmidt-orthogonal
 gram-schmidt-orthonormal
 ; factorization
 matrix-lu
 matrix-qr
 ; comprehensions
 for/matrix:
 for*/matrix:
 for/matrix-sum:
 ; sequences
 in-row
 in-column
 ; special matrices
 vandermonde-matrix
 )

;;;
;;; Basic
;;;

(: matrix-ref : (Matrix Number) Integer Integer -> Number)
(define (matrix-ref M i j)
  ((inst array-ref Number) M (vector i j)))

(: matrix-scale : Number (Matrix Number) -> (Matrix Number))
(define (matrix-scale s a)
  (array-scale a s))

(: matrix-row-vector? : (Matrix Number) -> Boolean)
(define (matrix-row-vector? a)
  (= (matrix-row-dimension a) 1))

(: matrix-column-vector? : (Matrix Number) -> Boolean)
(define (matrix-column-vector? a)
  (= (matrix-column-dimension a) 1))


;;;
;;; Norms
;;; 

(: matrix-norm : (Matrix Number) -> Real)
(define (matrix-norm a)
  (define n
    (sqrt
     (array-ref
      (array-axis-sum 
       (array-axis-sum 
        (matrix.sqr (matrix.magnitude a)) 0) 0)
      '#())))
  (assert n real?))

;;;
;;; Operators
;;;

(: matrix-transpose : (Matrix Number) -> (Matrix Number))
(define (matrix-transpose a)
  (array-axis-swap a 0 1))

(: matrix-conjugate : (Matrix Number) -> (Matrix Number))
(define (matrix-conjugate a)
  (array-conjugate a))

(: matrix-hermitian : (Matrix Number) -> (Matrix Number))
(define (matrix-hermitian a)
  (matrix-transpose 
   (array-conjugate a)))

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

(: matrix-gauss-eliminate : 
   (case-> ((Matrix Number) Boolean Boolean -> (Values (Matrix Number) (Listof Integer)))
           ((Matrix Number) Boolean         -> (Values (Matrix Number) (Listof Integer)))
           ((Matrix Number)                 -> (Values (Matrix Number) (Listof Integer)))))
(define (matrix-gauss-eliminate M [unitize-pivot-row? #f] [partial-pivoting? #t])
  (define-values (m n) (matrix-dimensions M))
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
         [(or (eq? p #f)
              (zero? (matrix-ref M p j)))
          ; no pivot found
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
              ; remove elements below pivot
              (let l-loop ([l (+ i 1)] [M M])
                (if (= l m)
                    (loop (+ i 1) (+ j 1) M k without-pivot)
                    (let ([x_lj (matrix-ref M l j)])
                      (l-loop (+ l 1)
                              (if (zero? x_lj)
                                  M
                                  (matrix-add-scaled-row M l (- (/ x_lj pivot)) i))))))))])]))
  (let-values ([(M without) (loop 0 0 M 0 '())])
    (values M without)))

(: matrix-rank : (Matrix Number) -> Integer)
(define (matrix-rank M)
  ; TODO: Use QR or SVD instead for inexact matrices
  ; See answer: http://scicomp.stackexchange.com/questions/1861/understanding-how-numpy-does-svd
  ; rank = dimension of column space = dimension of row space
  (define-values (m n) (matrix-dimensions M))
  (define-values (_ cols-without-pivot) (matrix-gauss-eliminate M))
  (- n (length cols-without-pivot)))

(: matrix-nullity : (Matrix Number) -> Integer)
(define (matrix-nullity M)
  ; nullity = dimension of null space
  (define-values (m n) (matrix-dimensions M))
  (define-values (_ cols-without-pivot) (matrix-gauss-eliminate M))
  (length cols-without-pivot))

(: matrix-determinant : (Matrix Number) -> Number)
(define (matrix-determinant M)
  (define-values (m n) (matrix-dimensions M))
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

(: matrix-trace : (Matrix Number) -> Number)
(define (matrix-trace M)
  (define-values (m n) (matrix-dimensions M))
  (for/sum: : Number ([i (in-range 0 m)]) 
    (matrix-ref M i i)))

(: matrix-column-space : (Matrix Number) -> (Listof (Matrix Number)))
; Returns
;  1) a list of column vectors spanning the column space
;  2) a list of column vectors spanning the null space
(define (matrix-column-space M)  
  (define-values (m n) (matrix-dimensions M))
  (: M1 (Matrix Number))
  (: cols-without-pivot (Listof Integer))
  (define-values (M1 cols-without-pivot) (matrix-gauss-eliminate M #t))
  (set! M1 (array->mutable-array M1))  
  (define: column-space : (Listof (Matrix Number))
    (for/list:
        ([i : Index n]
         #:when (not (member i cols-without-pivot)))
      (matrix-column M1 i)))
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
  (define-values (m n) (matrix-dimensions M))
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

(: matrix-augment : (Matrix Number) (Matrix Number) * -> (Matrix Number))
(define (matrix-augment a . as)
  (array-append* (cons a as) 1))

(: matrix-stack : (Matrix Number) * -> (Matrix Number))
(define (matrix-stack . as)
  (if (null? as)
      (error 'matrix-stack 
             "expected non-empty list of matrices")
      (array-append* as 0)))


(: matrix-inverse : (Matrix Number) -> (Matrix Number))
(define (matrix-inverse M)
  (define-values (m n) (matrix-dimensions M))
  (unless (= m n) (error 'matrix-inverse "matrix not square"))
  (let ([MI (matrix-augment M (identity-matrix m))])
    (define 2m (* 2 m))
    (if (index? 2m)
        (submatrix (matrix-reduced-row-echelon-form MI #t) 
                   (in-range 0 m) (in-range m 2m))
        (error 'matrix-inverse "internal error"))))

(: matrix-solve : (Matrix Number) (Matrix Number) -> (Matrix Number))
;    Return a column-vector x such that Mx = b.
;    If no such vector exists return #f.
(define (matrix-solve M b)
  (define-values (m n) (matrix-dimensions M))
  (define-values (s t) (matrix-dimensions b))
  (define m+1 (+ m 1))
  (cond
    [(not (= t 1)) (error 'matrix-solve "expected column vector (i.e. r x 1 - matrix), got: ~a " b)]
    [(not (= m s)) (error 'matrix-solve "expected column vector with same number of rows as the matrix")]
    [(index? m+1)
     (submatrix
      (matrix-reduced-row-echelon-form 
       (matrix-augment M b) #t)
      (in-range 0 m) (in-range m m+1))]
    [else (error 'matrix-solve "internatl error")]))

(: matrix-solve-many : (Matrix Number) (Listof (Matrix Number)) -> (Matrix Number))
(define (matrix-solve-many M bs)
  ; TODO: Rewrite matrix-augment* to use array-append when it is ready
  (: matrix-augment* : (Listof (Matrix Number)) -> (Matrix Number))
  (define (matrix-augment* vs)
    (foldl matrix-augment (car vs) (cdr vs)))
  (define-values (m n) (matrix-dimensions M))
  (define-values (s t) (matrix-dimensions (car bs)))
  (define k (length bs))
  (define m+1 (+ m 1))
  (define m+k (+ m k))
  (cond
    [(not (= t 1)) (error 'matrix-solve-many "expected column vector (i.e. r x 1 - matrix), got: ~a " (car bs))]
    [(not (= m s)) (error 'matrix-solve-many "expected column vectors with same number of rows as the matrix")]
    [(and (index? m+1) (index? m+k)) 
     (define bs-as-matrix (matrix-augment* bs))     
     (define MB (matrix-augment M bs-as-matrix))
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
  (define-values (m _) (matrix-dimensions M))
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


(: column-dimension : (Column Number) -> Index)
(define (column-dimension v)
  (if (vector? v)
      (unsafe-vector-length v)
      (matrix-row-dimension v)))


(: unsafe-column->vector : (Column Number) -> (Vectorof Number))
(define (unsafe-column->vector v)
  (if (vector? v) v
      (let ()
        (define-values (m n) (matrix-dimensions v))
        (if (= n 1)
            (mutable-array-data (array->mutable-array v))
            (error 'unsafe-column->vector
                   "expected a column (vector or mx1 matrix), got ~a" v)))))

(: vector->column : (Vectorof Number) -> (Result-Column Number))
(define (vector->column v)
  (define m (vector-length v))
  (flat-vector->matrix m 1 v))

(: column : Number * -> (Result-Column Number))
(define (column . xs)
  (vector->column 
   (list->vector xs)))

(: result-column : (Column Number) -> (Result-Column Number))
(define (result-column c)
  (if (vector? c)
      (vector->column c)
      c))

(: scale-column : Number (Column Number) -> (Result-Column Number))
(define (scale-column s a)
  (if (vector? a)
      (let*: ([n (vector-length a)]
              [v : (Vectorof Number) (make-vector n 0)])
        (for: ([i (in-range 0 n)]
               [x : Number (in-vector a)]) 
          (vector-set! v i (* s x)))
        (vector->column v))
      (matrix-scale s a)))

(: column+ : (Column Number) (Column Number) -> (Result-Column Number))
(define (column+ v w)
  (cond [(and (vector? v) (vector? w))
         (let ([n (vector-length v)]
               [m (vector-length w)])
           (unless (= m n)
             (error 'column+ 
                    "expected two column vectors of the same length, got ~a and ~a" v w))
           (define: v+w : (Vectorof Number) (make-vector n 0))
           (for: ([i (in-range 0 n)]
                  [x : Number (in-vector v)]
                  [y : Number (in-vector w)])
             (vector-set! v+w i (+ x y)))
           (result-column v+w))]
        [else 
         (unless (= (column-dimension v) (column-dimension w))
           (error 'column+ 
                  "expected two column vectors of the same length, got ~a and ~a" v w))
         (matrix+ (result-column v) (result-column w))]))


(: column-dot : (Column Number) (Column Number) -> Number)
(define (column-dot c d)  
  (define v (unsafe-column->vector c))
  (define w (unsafe-column->vector d))
  (define m (column-dimension v))
  (define s (column-dimension w))
  (cond
    [(not (= m s)) (error 'column-dot 
                          "expected two mx1 matrices with same number of rows, got ~a and ~a"
                          c d)]
    [else
     (for/sum: : Number ([i (in-range 0 m)])
       (assert i index?)
       ; Note: If d is a vector of reals, 
       ;       then the conjugate is a no-op
       (* (unsafe-vector-ref v i)
          (conjugate (unsafe-vector-ref w i))))]))

(: column-norm : (Column Number) -> Real)
(define (column-norm v)
  (define norm (sqrt (column-dot v v)))
  (assert norm real?))


(: column-projection : (Column Number) (Column Number) -> (Result-Column Number))
; (column-projection v w)
;    Return the projection og vector v on vector w.
(define (column-projection v w)
  (let ([w.w (column-dot w w)])
    (if (zero? w.w)
        (error 'column-projection "projection on the zero vector not defined")
        (matrix-scale (/ (column-dot v w) w.w) (result-column w)))))

(: column-projection-on-unit : (Column Number) (Column Number) -> (Result-Column Number))
; (column-projection-on-unit v w)
;    Return the projection og vector v on a unit vector w.
(define (column-projection-on-unit v w)
  (matrix-scale (column-dot v w) (result-column w)))


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
      (for/matrix-sum: : Number ([b (in-list bs)])
                       (column-projection v (result-column b)))))
  
;  #;(for/matrix-sum ([b bs])
;      (matrix-scale (column-dot v b) b))
;  (define: sum : (U False (Result-Column Number)) #f)
;  (for ([b1 (in-list bs)])
;    (define: b : (Result-Column Number) (result-column b1))
;    (cond [(not sum) (set! sum (column-projection v b))]
;          [else      (set! sum (matrix+ (assert sum) (column-projection v b)))]))
;  (cond [sum (assert sum)]
;        [else (error 'projection-on-orthogonal-basis 
;                     "received empty list of basis vectors")])


; (projection-on-orthonormal-basis v bs)
;     Project the vector v on the orthonormal basis vectors in bs.
;     The basis bs must be either the column vectors of a matrix
;     or a sequence of column-vectors.
(: projection-on-orthonormal-basis : 
   (Column Number) (Listof (Column Number)) -> (Result-Column Number))
(define (projection-on-orthonormal-basis v bs)
  #;(for/matrix-sum ([b bs]) (matrix-scale (column-dot v b) b))
  (define: sum : (U False (Result-Column Number)) #f)
  (for ([b1 (in-list bs)])
    (define: b : (Result-Column Number) (result-column b1))
    (cond [(not sum) (set! sum (column-projection-on-unit v b))]
          [else      (set! sum (matrix+ (assert sum) (column-projection-on-unit v b)))]))
  (cond [sum (assert sum)]
        [else (error 'projection-on-orthogonal-basis 
                     "received empty list of basis vectors")]))


(: zero-column-vector? : (Matrix Number) -> Boolean)
(define (zero-column-vector? v)
  (define-values (m n) (matrix-dimensions v))
  (for/and: ([i (in-range 0 m)])
    (zero? (matrix-ref v i 0))))

(: gram-schmidt-orthogonal : (Listof (Column Number)) -> (Listof (Result-Column Number)))
; (gram-schmidt-orthogonal ws)
;     Given a list ws of column vectors, produce 
;     an orthogonal basis for the span of the
;     vectors in ws.
(define (gram-schmidt-orthogonal ws1)
  (define ws (map result-column ws1))
  (cond 
    [(null? ws)       '()]
    [(null? (cdr ws)) (list (car ws))]
    [else 
     (: loop : (Listof (Result-Column Number)) (Listof (Column-Matrix Number)) -> (Listof (Result-Column Number)))
     (define (loop vs ws)
       (cond [(null? ws) vs]
             [else
              (define w (car ws))
              (let ([w-proj (projection-on-orthogonal-basis w vs)])
                ; Note: We project onto vs (not on the original ws)
                ;       in order to get numerical stability.
                (let ([w-minus-proj (matrix- w w-proj)])
                  (if (zero-column-vector? w-minus-proj)
                      (loop vs (cdr ws)) ; w in span{vs} => omit it
                      (loop (cons (matrix- w w-proj) vs) (cdr ws)))))]))
     (reverse (loop (list (car ws)) (cdr ws)))]))



(: column-normalize : (Column Number) -> (Result-Column Number))
; (column-vector-normalize v)
;    Return unit vector with same direction as v.
;    If v is the zero vector, the zero vector is returned.
(define (column-normalize w)
  (let ([norm (column-norm w)]
        [w (result-column w)])
    (cond [(zero? norm) w]
          [else (matrix-scale (/ norm) w)])))

(: gram-schmidt-orthonormal : (Listof (Column Number)) -> (Listof (Result-Column Number)))
; (gram-schmidt-orthonormal ws)
;     Given a list ws of column vectors, produce 
;     an orthonormal basis for the span of the
;     vectors in ws.
(define (gram-schmidt-orthonormal ws)
  (map column-normalize
       (gram-schmidt-orthogonal ws)))

(: projection-on-subspace :
   (Column Number) (Listof (Column Number)) -> (Result-Column Number))
; (projection-on-subspace v ws)
;    Returns the projection of v on span{w_i}, w_i in ws.
(define (projection-on-subspace v ws)
  (projection-on-orthogonal-basis
   v (gram-schmidt-orthogonal ws)))

(: unit-column : Integer Integer -> (Result-Column Number))
(define (unit-column m i)
  (cond
    [(and (index? m) (index? i))
     (define v (make-vector m 0))
     (if (< i m)
         (vector-set! v i 1)
         (error 'unit-vector "dimension must be largest"))
     (flat-vector->matrix m 1 v)]
    [else
     (error 'unit-vector "expected two indices")]))


(: take : (All (A) ((Listof A) Index -> (Listof A))))
(define (take xs n)
  (if (= n 0)
      '()
      (let ([n-1 (- n 1)])
        (if (index? n-1)
            (cons (car xs) (take (cdr xs) n-1))
            (error 'take "can not take more elements than the length of the list")))))

; (list 'take (equal? (take (list 0 1 2 3 4) 2) '(0 1)))

(: matrix->columns : (Matrix Number) -> (Listof (Matrix Number)))
(define (matrix->columns M)
  (define-values (m n) (matrix-dimensions M))  
  (for/list: : (Listof (Matrix Number))
    ([j (in-range 0 n)])    
    (matrix-column M (assert j index?))))

(: matrix-augment* : (Listof (Matrix Number)) -> (Matrix Number))
(define (matrix-augment* Ms)
  (define MM (car Ms))
  (for: ([M (in-list (cdr Ms))])
    (set! MM (matrix-augment MM M)))
  MM)

(: extend-span-to-basis :
   (Listof (Matrix Number)) Integer -> (Listof (Matrix Number)))
; Extend the basis in vs to with rdimensional basis
(define (extend-span-to-basis vs r)
  (define-values (m n) (matrix-dimensions (car vs)))
  (: loop : (Listof (Matrix Number)) (Listof (Matrix Number)) Integer -> (Listof (Matrix Number)))
  (define (loop vs ws i)
    (if (>= i m)
        ws
        (let ()
          (define ei (unit-column m i))
          (define pi (projection-on-subspace ei vs))
          (if (matrix-all= ei pi)
              (loop vs ws (+ i 1))
              (let ([w (matrix- ei pi)])
                (loop (cons w vs) (cons w ws) (+ i 1)))))))
  (: norm> : (Matrix Number) (Matrix Number) -> Boolean)
  (define (norm> v w)
    (> (column-norm v) (column-norm w)))
  (if (index? r)
      ((inst take (Matrix Number)) (sort (loop vs '() 0) norm>) r)
      (error 'extend-span-to-basis "expected index as second argument, got ~a" r)))

(: matrix-qr : (Matrix Number) -> (Values (Matrix Number) (Matrix Number)))
(define (matrix-qr M)
  ; compute the QR-facorization
  ; 1) QR = M 
  ; 2) columns of Q is are orthonormal
  ; 3) R is upper-triangular
  ; Note: columnspace(A)=columnspace(Q) !
  (define-values (m n) (matrix-dimensions M))
  (let* ([basis-for-column-space
          (gram-schmidt-orthonormal (matrix->columns M))]
         [extension
          (extend-span-to-basis 
           basis-for-column-space (- n (length basis-for-column-space)))]
         [Q (matrix-augment*
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
            (flat-vector->matrix n n v))])
    (values Q R)))

(: matrix/dim : Integer Integer Number * -> (Matrix Number))
; construct a mxn matrix with elements from the values xs
; the length of xs must be m*n
(define (matrix/dim m n . xs)
  (cond [(and (index? m) (index? n))
         (flat-vector->matrix m n (list->vector xs))]
        [else (error 'matrix/dim "expected two indices as dimensions, got ~a and ~a" m n)]))

(: matrix-block-diagonal : (Listof (Matrix Number)) -> (Matrix Number))
(define (matrix-block-diagonal as)
  (define sum-m 0)
  (define sum-n 0)
  (define: ms : (Listof Index) '())
  (define: ns : (Listof Index) '())
  (for: ([a (in-list as)])
    (define-values (m n) (matrix-dimensions a))
    (set! sum-m (+ sum-m m))
    (set! sum-n (+ sum-n n))
    (set! ms (cons m ms))
    (set! ns (cons n ns)))
  (set! ms (reverse ms))
  (set! ns (reverse ns))
  (: loop : (Listof (Matrix Number)) (Listof Index) (Listof Index) 
     (Listof (Matrix Number)) Integer -> (Matrix Number))
  (define (loop as ms ns rows left)
    (cond [(null? as) (apply matrix-stack (reverse rows))]
          [else
           (define m (car ms))
           (define n (car ns))
           (define a (car as))
           (define row
             (matrix-augment 
              ((inst make-matrix Number) m left 0) a (make-matrix m (- sum-n n left) 0)))
           (loop (cdr as) (cdr ms) (cdr ns) (cons row rows) (+ left n))]))
  (loop as ms ns '() 0))

(define-syntax (for/column: stx)
  (syntax-case stx ()
    [(_ : type m-expr (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: flat-vector : (Vectorof Number) (make-vector m 0))
         (for: ([i (in-range m)] for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! flat-vector i x))
         (vector->column flat-vector)))]))

(define-syntax (for/matrix: stx)
  (syntax-case stx ()
    [(_ : type m-expr n-expr #:column (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (define: k : Index 0)
         (for: ([i (in-range m*n)] for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v (+ (* n (remainder k m)) (quotient k m)) x)
           (set! k (assert (+ k 1) index?)))         
         (flat-vector->matrix m n v)))]
    [(_ : type m-expr n-expr (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (for: ([i (in-range m*n)] for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v i x))
         (flat-vector->matrix m n v)))]))

(define-syntax (for*/matrix: stx)
  (syntax-case stx ()
    [(_ : type m-expr n-expr #:column (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (define: k : Index 0)
         (for*: (for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v (+ (* n (remainder k m)) (quotient k m)) x)
           (set! k (assert (+ k 1) index?)))
         (flat-vector->matrix m n v)))]
    [(_ : type m-expr n-expr (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: m : Index m-expr)
         (define: n : Index n-expr)
         (define: m*n : Index (assert (* m n) index?))
         (define: v : (Vectorof Number) (make-vector m*n 0))
         (define: i : Index 0) 
         (for*: (for:-clause ...)
           (define x (let () . defs+exprs))
           (vector-set! v i x)
           (set! i (assert (+ i 1) index?)))
         (flat-vector->matrix m n v)))]))



(define-syntax (for/matrix-sum: stx)
  (syntax-case stx ()
    [(_ : type (for:-clause ...) . defs+exprs)
     (syntax/loc stx
       (let ()
         (define: sum : (U False (Matrix Number)) #f)
         (for: (for:-clause ...)
           (define a (let () . defs+exprs))
           (set! sum (if sum (matrix+ (assert sum) a) a)))
         (assert sum)))]))

;;;
;;; SEQUENCES
;;;

(: in-row/proc : (Matrix Number) Integer -> (Sequenceof Number))
(define (in-row/proc M r)
  (define-values (m n) (matrix-dimensions M))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ: ([j : Index]) (matrix-ref M r j))
      ; next-pos
      (λ: ([j : Index]) (assert (+ j 1) index?))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ: ([j : Index ]) (< j n))
      #f #f))))

(: in-column/proc : (Matrix Number) Integer -> (Sequenceof Number))
(define (in-column/proc M s)
  (define-values (m n) (matrix-dimensions M))
  (make-do-sequence
   (λ ()
     (values
      ; pos->element
      (λ: ([i : Index]) (matrix-ref M i s))
      ; next-pos
      (λ: ([i : Index]) (assert (+ i 1) index?))
      ; initial-pos
      0
      ; continue-with-pos?
      (λ: ([i : Index]) (< i m))
      #f #f))))

; (in-row M i]
;     Returns a sequence of all elements of row i,
;     that is xi0, xi1, xi2, ...
(define-sequence-syntax in-row
  (λ () #'in-row/proc)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ M-expr r-expr)]
       #'((x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r))
             (unless (and (integer? r) (and (<= 0 r ) (< r n))) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d (+ (* r n) j))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr r-expr)]
       #'((i x)
          (:do-in
           ([(M r n d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 r-expr rd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? r) 
               (raise-type-error 'in-row "expected row number, got ~a" r)))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d (+ (* r n) j))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))]
      [[_ clause] (raise-syntax-error 
                   'in-row "expected (in-row <matrix> <row>)" #'clause #'clause)])))

; (in-column M j]
;     Returns a sequence of all elements of column j,
;     that is x0j, x1j, x2j, ...

(define-sequence-syntax in-column
  (λ () #'in-column/proc)
  (λ (stx)
    (syntax-case stx ()
      ; M-expr evaluates to column
      [[(x) (_ M-expr)]
       #'((x)
          (:do-in
           ([(M n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 rd cd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (unless (array-matrix? M) 
             (raise-type-error 'in-row "expected matrix, got ~a" M))
           ([j 0])
           (< j n)
           ([(x) (vector-ref d j)])
           #true
           #true
           [(+ j 1)]))]
      ; M-expr evaluats to matrix, s-expr to the column index
      [[(x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 s-expr rd cd 
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-row "expected matrix, got ~a" M))
             (unless (integer? s) 
               (raise-type-error 'in-row "expected col number, got ~a" s))
             (unless (and (integer? s) (and (<= 0 s ) (< s m))) 
               (raise-type-error 'in-col "expected col number, got ~a" s)))
           ([j 0])
           (< j m)
           ([(x) (vector-ref d (+ (* j n) s))])
           #true
           #true
           [(+ j 1)]))]
      [[(i x) (_ M-expr s-expr)]
       #'((x)
          (:do-in
           ([(M s n m d) 
             (let ([M1 M-expr])
               (define-values (rd cd) (matrix-dimensions M1))
               (values M1 s-expr rd cd
                       (mutable-array-data
                        (array->mutable-array M1))))])
           (begin 
             (unless (array-matrix? M) 
               (raise-type-error 'in-column "expected matrix, got ~a" M))
             (unless (integer? s) 
               (raise-type-error 'in-column "expected col number, got ~a" s))
             (unless (and (integer? s) (and (<= 0 s ) (< s m))) 
               (raise-type-error 'in-column "expected col number, got ~a" s)))
           ([j 0])
           (< j m)
           ([(x) (vector-ref d (+ (* j n) s))]
            [(i) j])
           #true
           #true
           [(+ j 1)]))]
      [[_ clause] (raise-syntax-error 
                   'in-column "expected (in-column <matrix> <column>)" #'clause #'clause)])))

(: vandermonde-matrix : (Listof Number) Integer -> (Matrix Number))
(define (vandermonde-matrix xs n)
  ; construct matrix M with M(i,j)=α_i^j ; where i and j begin from 0 ... 
  ; Inefficient version:
  (cond
    [(not (index? n))
     (error 'vandermonde-matrix "expected Index as second argument, got ~a" n)]
    [else       (define: m : Index (length xs))
                (define: αs :  (Vectorof Number) (list->vector xs))
                (define: α^j : (Vectorof Number) (make-vector n 1))
                (for*/matrix: : Number m n #:column
                              ([j (in-range 0 n)]
                               [i (in-range 0 m)])
                              (define αi^j (vector-ref α^j i))
                              (define αi   (vector-ref αs i ))
                              (vector-set! α^j i (* αi^j αi))
                              αi^j)]))

