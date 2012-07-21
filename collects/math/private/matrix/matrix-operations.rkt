#lang typed/racket/base

(require math/array
         racket/unsafe/ops
         "matrix-types.rkt"
         "matrix-constructors.rkt"
         (for-syntax racket))

(provide 
 ; basic
 matrix-ref
 ; operators
 matrix-transpose
 matrix-conjugate
 matrix-hermitian
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
 ; decomposition
 matrix-lu
 ; invariant
 matrix-rank
 matrix-nullity
 ; spaces
 ;matrix-column+null-space
 )

;;;
;;; Basic
;;;

(: matrix-ref : (Matrix Number) Integer Integer -> Number)
(define (matrix-ref M i j)
  ((inst array-ref Number) M (list i j)))


;;;
;;; Operators
;;;

(: matrix-transpose : (Matrix Number) -> (Result-Matrix Number))
(define (matrix-transpose a)
  (array-axis-swap a 0 1))

(: matrix-conjugate : (Matrix Number) -> (Result-Matrix Number))
(define (matrix-conjugate a)
  (array-conjugate a))

(: matrix-hermitian : (Matrix Number) -> (Result-Matrix Number))
(define (matrix-hermitian a)
  (matrix-transpose 
   (array-conjugate a)))

;;;
;;; Row and column
;;;

(: matrix-scale-row : (Matrix Number) Integer Number -> (Result-Matrix Number))
(define (matrix-scale-row a i c)
  ((inline-matrix-scale-row i c) a))

(define-syntax (inline-matrix-scale-row stx)
  (syntax-case stx ()
    [(_ i c)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
           (define g (unsafe-array-proc arr))
           (cond
             [(< i 0)
              (error 'matrix-scale-row "row index must be non-negative, got ~a" i)]
             [(not (< i (vector-ref ds 0)))
              (error 'matrix-scale-row "row index must be smaller than the number of rows, got ~a" i)]
             [else
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (if (= i (vector-ref js 0))
                                          (* c (g js))
                                          (g js))))]))))]))

(: matrix-scale-column : (Matrix Number) Integer Number -> (Result-Matrix Number))
(define (matrix-scale-column a i c)
  ((inline-matrix-scale-column i c) a))

(define-syntax (inline-matrix-scale-column stx)
  (syntax-case stx ()
    [(_ j c)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
           (define g (unsafe-array-proc arr))
           (cond
             [(< j 0)
              (error 'matrix-scale-row "column index must be non-negative, got ~a" j)]
             [(not (< j (vector-ref ds 1)))
              (error 'matrix-scale-row "column index must be smaller than the number of rows, got ~a" j)]
             [else
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (if (= j (vector-ref js 1))
                                          (* c (g js))
                                          (g js))))]))))]))

(: matrix-swap-rows : (Matrix Number) Integer Integer -> (Result-Matrix Number))
(define (matrix-swap-rows a i j)
  ((inline-matrix-swap-rows i j) a))

(define-syntax (inline-matrix-swap-rows stx)
  (syntax-case stx ()
    [(_ i j)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
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
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (cond
                                        [(= i (vector-ref js 0)) 
                                         (g (vector j (vector-ref js 1)))]
                                        [(= j (vector-ref js 0)) 
                                         (g (vector i (vector-ref js 1)))]
                                        [else                    
                                         (g js)])))]))))]))

(: matrix-swap-columns : (Matrix Number) Integer Integer -> (Result-Matrix Number))
(define (matrix-swap-columns a i j)
  ((inline-matrix-swap-columns i j) a))

(define-syntax (inline-matrix-swap-columns stx)
  (syntax-case stx ()
    [(_ i j)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
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
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (cond
                                        [(= i (vector-ref js 1)) 
                                         (g (vector j (vector-ref js 1)))]
                                        [(= j (vector-ref js 1)) 
                                         (g (vector i (vector-ref js 1)))]
                                        [else                    
                                         (g js)])))]))))]))

(: matrix-add-scaled-row : (Matrix Number) Integer Number Integer -> (Result-Matrix Number))
(define (matrix-add-scaled-row a i c j)
  ((inline-matrix-add-scaled-row i c j) a))

(: flmatrix-add-scaled-row : (Matrix Flonum) Index Flonum Index -> (Result-Matrix Flonum))
(define (flmatrix-add-scaled-row a i c j)
  ((inline-matrix-add-scaled-row i c j) a))

(define-syntax (inline-matrix-add-scaled-row stx)
  (syntax-case stx ()
    [(_ i c j)
     (syntax/loc stx
       (λ (arr)
         (let ([arr  (array-lazy arr)])
           (define ds (unsafe-array-shape arr))
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
              (unsafe-lazy-array ds (λ: ([js : (Vectorof Index)]) 
                                      (if (= i (vector-ref js 0))
                                          (+ (g js) (* c (g (vector j (vector-ref js 1)))))
                                          (g js))))]))))]))


;;; GAUSS ELIMINATION / ROW ECHELON FORM

(: matrix-gauss-eliminate : 
   (case-> ((Matrix Number) Boolean Boolean -> (Values (Result-Matrix Number) (Listof Integer)))
           ((Matrix Number) Boolean         -> (Values (Result-Matrix Number) (Listof Integer)))
           ((Matrix Number)                 -> (Values (Result-Matrix Number) (Listof Integer)))))
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
    (values (array-lazy M) without)))

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

#;(: matrix-column+null-space : 
     (Matrix Number) -> (Values (Listof (Result-Matrix Number))
                                (Listof (Result-Matrix Number))))
; Returns
;  1) a list of column vectors spanning the column space
;  2) a list of column vectors spanning the null space
; TODO: Column space works, but wrong space is 
;       not done yet.
#;(define (matrix-column+null-space M)  
    ; TODO:
    ;    Null space from row reduction numerically unstable.
    ;    USE QR or SVD instead.
    ;    See http://en.wikipedia.org/wiki/Kernel_(matrix)  
    (define-values (m n) (matrix-dimensions M))
    (: M1 (Matrix Number))
    (: cols-without-pivot (Listof Integer))
    (define-values (M1 cols-without-pivot) (matrix-gauss-eliminate M #t))
    (set! M1 (array-strict M1))
    (define: null-space : (Listof (Result-Matrix Number))
      (for/list: 
          ([i : Integer (in-list cols-without-pivot)])
        (cond
          [(not (index? i)) (error 'column+null-space "Internal error")]
          [else (matrix-column M1 i)])))
    (define: column-space : (Listof (Result-Matrix Number))
      (for/list:
          ([i : Index n]
           #:when (not (member i cols-without-pivot)))
        (matrix-column M1 i)))
    (values column-space null-space))


(: matrix-row-echelon-form : 
   (case-> ((Matrix Number) Boolean -> (Result-Matrix Number))
           ((Matrix Number) Boolean -> (Result-Matrix Number))
           ((Matrix Number)         -> (Result-Matrix Number))))
(define (matrix-row-echelon-form M [unitize-pivot-row? #f])
  (let-values ([(M wp) (matrix-gauss-eliminate M unitize-pivot-row?)])
    M))

(: matrix-gauss-jordan-eliminate : 
   (case-> ((Matrix Number) Boolean Boolean -> (Values (Result-Matrix Number) (Listof Integer)))
           ((Matrix Number) Boolean         -> (Values (Result-Matrix Number) (Listof Integer)))
           ((Matrix Number)                 -> (Values (Result-Matrix Number) (Listof Integer)))))
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
    (values (array-lazy M) without)))

(: matrix-reduced-row-echelon-form : 
   (case-> ((Matrix Number) Boolean -> (Result-Matrix Number))
           ((Matrix Number) Boolean -> (Result-Matrix Number))
           ((Matrix Number)         -> (Result-Matrix Number))))
(define (matrix-reduced-row-echelon-form M [unitize-pivot-row? #f])
  (let-values ([(M wp) (matrix-gauss-jordan-eliminate M unitize-pivot-row?)])
    M))

;;; LU Factorization
; Not all matrices can be LU-factored.
; If Gauss-elimination can be done without any row swaps,
; a LU-factorization is possible.

(: matrix-lu : 
   (Matrix Number) -> (U False (List (Result-Matrix Number) (Result-Matrix Number))))
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
          (let ([ds (unsafe-array-shape M)])
            (unsafe-lazy-array
             ds (λ: ([js : (Vectorof Index)])
                  (define i (unsafe-vector-ref js 0))
                  (define j (unsafe-vector-ref js 1))
                  (vector-ref L-matrix (+ (* i m) j))))))
        (list (array-lazy L) 
              (array-lazy V)))))
