#lang typed/racket

(require math/array
         math/base
         math/flonum
         math/matrix)

(begin  
  (begin "matrix-types.rkt"
         (list
          'array-matrix?
          (array-matrix? (list*->array '[[1 2] [3 4]] real? ))
          (not (array-matrix? (list*->array '[[[1 2] [3 4]] [[1 2] [3 4]]] real? ))))
         (list
          'square-matrix?
          (square-matrix? (list*->array '[[1 2] [3 4]] real? ))
          (not (square-matrix? (list*->array '[[1 2 3] [4 5 6]] real? ))))
         (list
          'square-matrix-size
          (= 2 (square-matrix-size (list*->array '[[1 2 3] [4 5 6]] real? ))))
         (list
          'matrix-all=-
          (matrix-all= (list*->array '[[1 2] [3 4]] real?) (list*->array '[[1 2] [3 4]] real? ))
          (not (matrix-all= (list*->array '[[1 2] [3 4]] real?) (list*->array '[[1 2]] real? ))))
         (list
          'matrix-dimensions
          (let-values ([(m n) (matrix-dimensions (list->matrix '[[1 2 3] [4 5 6]]))])
            (equal? (list m n) '(2 3)))))
  
  (begin "matrix-constructors.rkt"
         (list
          'identity-matrix
          (equal? (array->list* (identity-matrix 1)) '[[1]])
          (equal? (array->list* (identity-matrix 2)) '[[1 0] [0 1]])
          (equal? (array->list* (identity-matrix 3)) '[[1 0 0] [0 1 0] [0 0 1]]) 
          (equal? (array->list* (flidentity-matrix 1)) '[[1.]])
          (equal? (array->list* (flidentity-matrix 2)) '[[1. 0.] [0. 1.]])
          (equal? (array->list* (flidentity-matrix 3)) '[[1. 0. 0.] [0. 1. 0.] [0. 0. 1.]]))
         (list
          'const-matrix
          (equal? (array->list* (make-matrix 2 3 0)) '((0 0 0) (0 0 0)))
          (equal? (array->list* (make-matrix 2 3 0.)) '((0. 0. 0.) (0. 0. 0.))))
         (list
          'matrix->list
          (equal? (matrix->list (list->matrix '((1 2) (3 4)))) '((1 2) (3 4)))
          (equal? (matrix->list (fllist->matrix '((1. 2.) (3. 4.)))) '((1. 2.) (3. 4.))))
         (list
          'matrix->vector
          (equal? (matrix->vector (vector->matrix '#(#(1 2) #(3 4)))) '#(#(1 2) #(3 4)))
          (equal? (matrix->vector (flvector->matrix '#(#(1. 2.) #(3. 4.)))) '#(#(1. 2.) #(3. 4.))))
         (list
          'matrix-row
          (equal? (matrix-row (identity-matrix 3) 0) (list->matrix '[[1 0 0]]))
          (equal? (matrix-row (identity-matrix 3) 1) (list->matrix '[[0 1 0]]))
          (equal? (matrix-row (identity-matrix 3) 2) (list->matrix '[[0 0 1]])))
         (list
          'matrix-col
          (equal? (matrix-column (identity-matrix 3) 0) (list->matrix '[[1] [0] [0]]))
          (equal? (matrix-column (identity-matrix 3) 1) (list->matrix '[[0] [1] [0]]))
          (equal? (matrix-column (identity-matrix 3) 2) (list->matrix '[[0] [0] [1]])))
         (list
          'submatrix
          (equal? (submatrix (identity-matrix 3) 
                             (in-range 0 1) (in-range 0 2)) (list->matrix '[[1 0]]))
          (equal? (submatrix (identity-matrix 3) 
                             (in-range 0 2) (in-range 0 3)) (list->matrix '[[1 0 0] [0 1 0]]))))

  (begin 
    "matrix-pointwise.rkt"
    (let ()
      (define A   (list->matrix '[[1 2] [3 4]]))
      (define ~A  (list->matrix '[[-1 -2] [-3 -4]]))
      (define B   (list->matrix '[[5 6] [7 8]]))
      (define A+B (list->matrix '[[6 8] [10 12]]))
      (define A-B (list->matrix '[[-4 -4] [-4 -4]]))         
      (list 'matrix+ (equal? (matrix+ A B) A+B))
      (list 'matrix- 
            (equal? (matrix- A B) A-B)
            (equal? (matrix- A)   ~A))))
  
  (begin  
    "matrix-expt.rkt"
    (let ()
      (define A (list->matrix '[[1 2] [3 4]]))
      (list
       'matrix-expt
       (equal? (matrix-expt A 0) (identity-matrix 2))
       (equal? (matrix-expt A 1) A)
       (equal? (matrix-expt A 2) (list->matrix '[[7 10] [15 22]]))
       (equal? (matrix-expt A 3) (list->matrix '[[37 54] [81 118]]))
       (equal? (matrix-expt A 8) (list->matrix '[[165751 241570] [362355 528106]]))))
    #;(list
       (define A (fllist->matrix '[[1. 2.] [3. 4.]]))
       (check-equal? (matrix->list (flmatrix-expt A 0)) (matrix->list (flidentity-matrix 2)))
       (check-equal? (matrix->list (flmatrix-expt A 1)) (matrix->list A))
       (check-equal? (matrix->list (flmatrix-expt A 2)) '[[7. 10.] [15. 22.]])
       (check-equal? (matrix->list (flmatrix-expt A 3)) '[[37. 54.] [81. 118.]])
       (check-equal? (matrix->list (flmatrix-expt A 8)) '[[165751. 241570.] [362355. 528106.]])))

  (begin
    "matrix-operations.rkt"
    (list 'vandermonde-matrix
          (equal? (vandermonde-matrix '(1 2 3) 5)
                  (list->matrix '[[1 1 1 1 1] [1 2 4 8 16] [1 3 9 27 81]])))
    (list 'in-column
          (equal? (for/list: : (Listof Number) ([x : Number (in-column (matrix/dim 2 2  1 2 3 4) 0)]) x)
                  '(1 3))
          (equal? (for/list: : (Listof Number) ([x : Number (in-column (matrix/dim 2 2  1 2 3 4) 1)]) x)
                  '(2 4))
          (equal? (for/list: : (Listof Number) ([x (in-column (column 5 2 3))]) x)
                  '(5 2 3)))
    (list 'in-row
          (equal? (for/list: : (Listof Number) ([x : Number (in-row (matrix/dim 2 2  1 2 3 4) 0)]) x)
                  '(1 2))
          (equal? (for/list: : (Listof Number) ([x : Number (in-row (matrix/dim 2 2  1 2 3 4) 1)]) x)
                  '(3 4)))
    (list 'for/matrix:
          (equal? (for/matrix: : Number 2 4 ([i (in-naturals)]) i)
                  (matrix/dim 2 4 
                              0 1 2 3
                              4 5 6 7))
          (equal? (for/matrix: : Number 2 4 #:column ([i (in-naturals)]) i)
                  (matrix/dim 2 4    
                              0 2 4 6
                              1 3 5 7))
          (equal? (for/matrix: : Number 3 3 ([i (in-range 10 100)]) i)
                  (matrix/dim 3 3 10 11 12 13 14 15 16 17 18)))
    (list 'for*/matrix:
          (equal? (for*/matrix: : Number 3 3 ([i (in-range 3)] [j (in-range 3)]) (+ (* i 10) j))
                  (matrix/dim 3 3 0 1 2 10 11 12 20 21 22)))    
    (list 'matrix-block-diagonal
          (equal? (matrix-block-diagonal (list (matrix/dim 2 2 1 2 3 4) (matrix/dim 1 3 5 6 7)))
                  (list->matrix '[[1 2 0 0 0] [3 4 0 0 0] [0 0 5 6 7]])))
    (list 'matrix-augment
          (equal? (matrix-augment (column 1 2 3) (column 4 5 6) (column 7 8 9))
                  (matrix/dim 3 3  1 4 7  2 5 8  3 6 9)))
    (list 'matrix-stack
          (equal? (matrix-stack (column 1 2 3) (column 4 5 6) (column 7 8 9))
                  (column 1 2 3 4 5 6 7 8 9)))
    (list 'column-dimension
          (= (column-dimension #(1 2 3)) 3)
          (= (column-dimension (flat-vector->matrix 1 2 #(1 2))) 1))
    (let ([matrix: flat-vector->matrix])
      (list 'column-dot
            (= (column-dot (column 1 2)   (column 1 2)) 5)
            (= (column-dot (column 1 2)   (column 3 4)) 11)
            (= (column-dot (column 3 4)   (column 3 4)) 25)
            (= (column-dot (column 1 2 3) (column 4 5 6))
               (+ (* 1 4) (* 2 5) (* 3 6)))
            (= (column-dot (column +3i +4i) (column +3i +4i))
               25)))
    (list 'matrix-trace
          (equal? (matrix-trace (flat-vector->matrix 2 2 #(1 2 3 4))) 5))
    (let ([matrix: flat-vector->matrix])
      (list 'column-norm
            (= (column-norm (column 2 4)) (sqrt 20))))
    (list 'column-projection
          (equal? (column-projection #(1 2 3) #(4 5 6)) (column 128/77 160/77 192/77))
          (equal? (column-projection (column 1 2 3) (column 2 4 3))
                  (matrix-scale 19/29 (column 2 4 3))))
    (list 'projection-on-orthogonal-basis
          (equal? (projection-on-orthogonal-basis #(3 -2 2) (list #(-1 0 2) #( 2 5 1)))
                  (column -1/3 -1/3 1/3))
          (equal? (projection-on-orthogonal-basis 
                   (column 3 -2 2) (list #(-1 0 2) (column 2 5 1)))
                  (column -1/3 -1/3 1/3)))
    (list 'projection-on-orthonormal-basis
          (equal? (projection-on-orthonormal-basis 
                   #(1 2 3 4) 
                   (list (matrix-scale 1/2 (column  1  1  1 1))
                         (matrix-scale 1/2 (column -1  1 -1 1))
                         (matrix-scale 1/2 (column  1 -1 -1 1))))
                  (column 2 3 2 3)))
    (list 'gram-schmidt-orthogonal
          (equal? (gram-schmidt-orthogonal (list #(3 1) #(2 2)))
                  (list (column 3 1) (column -2/5 6/5))))
    (list 'vector-normalize
          (equal? (column-normalize #(3 4)) 
                  (column 3/5 4/5)))
    (list 'gram-schmidt-orthonormal
          (equal? (gram-schmidt-orthonormal (ann '(#(3 1) #(2 2)) (Listof (Column Number))))
                  (list (column-normalize #(3 1))
                        (column-normalize #(-2/5 6/5)))))
    
    (list 'projection-on-subspace
          (equal? (projection-on-subspace #(1 2 3) '(#(2 4 3)))
                  (matrix-scale 19/29 (column 2 4 3))))
    (list 'unit-vector
          (equal? (unit-column 4 1) (column 0 1 0 0)))
    (list 'matrix-qr
          (let-values ([(Q R) (matrix-qr (matrix/dim 3 2  1 1 0 1 1 1))])
            (equal? (list Q R)
                    (list (matrix/dim 3 2  0.7071067811865475 0 0 1 0.7071067811865475 0)
                          (matrix/dim 2 2  1.414213562373095 1.414213562373095 0 1))))
          (let ()
            (define A (matrix/dim 4 4  1 2 3 4  1 2 4 5  1 2 5 6  1 2 6 7))
            (define-values (Q R) (matrix-qr A))
            (equal? (list Q R)
                    (list
                     (flat-vector->matrix 
                      4 4 (ann #(1/2 -0.6708203932499369    0.5477225575051662  -0.0
                                     1/2 -0.22360679774997896  -0.7302967433402214   0.4082482904638629
                                     1/2  0.22360679774997896  -0.18257418583505536 -0.8164965809277259
                                     1/2  0.6708203932499369    0.3651483716701107   0.408248290463863)
                               (Vectorof Number)))
                     (flat-vector->matrix 
                      4 4  (ann #(2 4 9 11 0 0.0 2.23606797749979 2.23606797749979 
                                    0 0 0.0 4.440892098500626e-16 0 0 0 0.0)
                                (Vectorof Number)))))))
    (list 'matrix-solve
          (let* ([M (list->matrix '[[1 5] [2 3]])] 
                 [b (list->matrix '[[5] [5]])])
            (equal? (matrix* M (matrix-solve M b)) b)))
    (list 'matrix-inverse
          (equal? (let ([M (list->matrix '[[1 2] [3 4]])]) (matrix* M (matrix-inverse M)))
                  (identity-matrix 2))
          (equal? (let ([M (list->matrix '[[1 2] [3 4]])]) (matrix* (matrix-inverse M) M))
                  (identity-matrix 2)))
    (list 'matrix-determinant
          (equal? (matrix-determinant (list->matrix '[[3]])) 3)
          (equal? (matrix-determinant (list->matrix '[[1 2] [3 4]])) (- (* 1 4) (* 2 3)))
          (equal? (matrix-determinant (list->matrix '[[1 2 3] [4  5 6] [7 8 9]])) 0)
          (equal? (matrix-determinant (list->matrix '[[1 2 3] [4 -5 6] [7 8 9]])) 120)
          (equal? (matrix-determinant (list->matrix '[[1 2 3 4] [-5 6 7 8] [9 10 -11 12] [13 14 15 16]])) 5280))
    (list 'matrix-scale
          (equal? (matrix-scale 2 (list->matrix '[[1 2] [3 4]]))
                  (list->matrix '[[2 4] [6 8]])))
    (list 'matrix-transpose
          (equal? (matrix-transpose (list->matrix '[[1 2] [3 4]]))
                  (list->matrix '[[1 3] [2 4]])))
    (list 'matrix-hermitian
          (equal? (matrix-hermitian (list->matrix '[[1+i 2-i] [3+i 4-i]]))
                  (list->matrix '[[1-i 3-i] [2+i 4+i]])))
    (let ()
      (: gauss-eliminate : (Matrix Number) Boolean Boolean -> (Matrix Number))
      (define (gauss-eliminate M u? p?)
        (let-values ([(M wp) (matrix-gauss-eliminate M u? p?)])
          M))
      (list 'matrix-gauss-eliminate
            (equal? (let ([M (list->matrix '[[1 2] [3 4]])])
                      (gauss-eliminate M #f #f))
                    (list->matrix '[[1 2] [0 -2]]))
            (equal? (let ([M (list->matrix  '[[2 4] [3 4]])])
                      (gauss-eliminate M #t #f))
                    (list->matrix '[[1 2] [0 1]]))
            (equal? (let ([M (list->matrix  '[[2. 4.] [3. 4.]])])
                      (gauss-eliminate M #t #t))
                    (list->matrix '[[1. 1.3333333333333333] [0. 1.]]))
            (equal? (let ([M (list->matrix  '[[1 4] [2 4]])])
                      (gauss-eliminate M #t #t))
                    (list->matrix '[[1 2] [0 1]]))
            (equal? (let ([M (list->matrix  '[[1 2] [2 4]])])
                      (gauss-eliminate M #f #t))
                    (list->matrix '[[2 4] [0 0]]))))
    (list 
     'matrix-scale-row
     (equal? (matrix-scale-row (identity-matrix 3) 0 2)
             (list*->array '[[2 0 0] [0 1 0] [0 0 1]] real? )))
    (list
     'matrix-swap-rows
     (equal? (matrix-swap-rows (list*->array '[[1 2 3] [4 5 6] [7 8 9]] real? ) 0 1)
             (list*->array '[[4 5 6] [1 2 3] [7 8 9]] real? )))
    (list
     'matrix-add-scaled-row
     (equal? (matrix-add-scaled-row (list*->array '[[1 2 3] [4 5 6] [7 8 9]] real? ) 0 2 1)
             (list*->array '[[9 12 15] [4 5 6] [7 8 9]] real? )))
    (let ()
      (define M (list->matrix '[[1  1  0  3]
                                [2  1 -1  1]
                                [3 -1 -1  2]
                                [-1  2  3 -1]]))
      (define LU (matrix-lu M))
      (if (eq? LU #f)
          (list 'matrix-lu #f)
          (let ()  
            (define L (if (list? LU) (first LU) #f))
            (define V (if (list? LU) (second LU) #f))
            (list
             'matrix-lu
             (equal? L (list->matrix
                        '[[1 0 0 0]
                          [2 1 0 0]
                          [3 4 1 0]
                          [-1 -3 0 1]]))
             (equal? V (list->matrix
                        '[[1  1  0   3]
                          [0 -1 -1  -5]
                          [0  0  3  13]
                          [0  0  0 -13]]))
             (equal? (matrix* L V) M)))))
    (list 
     'matrix-rank
     (equal? (matrix-rank (list->matrix '[[0 0] [0 0]])) 0)
     (equal? (matrix-rank (list->matrix '[[1 0] [0 0]])) 1)
     (equal? (matrix-rank (list->matrix '[[1 0] [0 3]])) 2)
     (equal? (matrix-rank (list->matrix '[[1 2] [2 4]])) 1)
     (equal? (matrix-rank (list->matrix '[[1 2] [3 4]])) 2))
    (list 
     'matrix-nullity
     (equal? (matrix-nullity (list->matrix '[[0 0] [0 0]])) 2)
     (equal? (matrix-nullity (list->matrix '[[1 0] [0 0]])) 1)
     (equal? (matrix-nullity (list->matrix '[[1 0] [0 3]])) 0)
     (equal? (matrix-nullity (list->matrix '[[1 2] [2 4]])) 1)
     (equal? (matrix-nullity (list->matrix '[[1 2] [3 4]])) 0))
    #;(let ()
        (define-values (c1 n1) 
          (matrix-column+null-space (list->matrix '[[0 0] [0 0]])))
        (define-values (c2 n2) 
          (matrix-column+null-space (list->matrix '[[1 2] [2 4]])))
        (define-values (c3 n3) 
          (matrix-column+null-space (list->matrix '[[1 2] [2 5]])))
        (list 
         'matrix-column+null-space
         (equal? c1 '())
         (equal? n1 (list (list->matrix '[[0] [0]])
                          (list->matrix '[[0] [0]])))
         (equal? c2 (list (list->matrix '[[1] [2]])))
         ;(equal? n2 '([0 0]))
         (equal? c3 (list (list->matrix '[[1] [2]])
                          (list->matrix '[[2] [5]])))
         (equal? n3 '()))))
  
  
  
  
  
  
  
  #;(begin
    "matrix-multiply.rkt"
    (list 'matrix*
          (let ()
            (define-values (A B AB) (values '[[1 2] [3 4]] '[[5 6] [7 8]] '[[19 22] [43 50]]))
            (equal? (matrix* (list->matrix A) (list->matrix B)) (list->matrix AB)))
          (let () 
            (define-values (A B AB) (values '[[1 2] [3 4]] '[[5 6 7] [8 9 10]] '[[21 24 27] [47 54 61]]))
            (equal? (matrix* (list->matrix A) (list->matrix B)) (list->matrix AB)))))
  #;(begin
    "matrix-2d.rkt"
    (let ()
      (define  e1  (matrix-transpose (vector->matrix #(#( 1  0)))))
      (define  e2  (matrix-transpose (vector->matrix #(#( 0  1)))))
      (define -e1  (matrix-transpose (vector->matrix #(#(-1  0)))))
      (define -e2  (matrix-transpose (vector->matrix #(#( 0 -1)))))
      (define   O  (matrix-transpose (vector->matrix #(#( 0  0)))))
      (define 2*e1 (matrix-scale 2 e1))
      (define 4*e1 (matrix-scale 4 e1))
      (define 3*e2 (matrix-scale 3 e2))
      (define 4*e2 (matrix-scale 4 e2))
      (begin
        (list 'matrix-2d-rotation
              (<= (matrix-norm (matrix- (matrix* (matrix-2d-rotation (/ pi 2)) e1) e2 )) epsilon.0) 
              (<= (matrix-norm (matrix- (matrix* (matrix-2d-rotation (/ pi 2)) e2) -e1)) epsilon.0))
        (list
         'matrix-2d-scaling
         (equal? (matrix* (matrix-2d-scaling 2 3) (matrix+ e1 e2)) (matrix+ 2*e1 3*e2)))
        (list
         'matrix-2d-shear-x
         (equal? (matrix* (matrix-2d-shear-x 3) (matrix+ e1 e2))   (matrix+ 4*e1   e2)))
        (list
         'matrix-2d-shear-y
         (equal? (matrix* (matrix-2d-shear-y 3) (matrix+ e1 e2))   (matrix+   e1 4*e2)))
        (list
         'matrix-2d-reflection
         (equal? (matrix* (matrix-2d-reflection 0 1) e1)           -e1)
         (equal? (matrix* (matrix-2d-reflection 0 1) e2)            e2)
         (equal? (matrix* (matrix-2d-reflection 1 0) e1)            e1)
         (equal? (matrix* (matrix-2d-reflection 1 0) e2)           -e2))
        (list
         'matrix-2d-orthogonal-projection
         (equal? (matrix* (matrix-2d-orthogonal-projection 1 0) e1) e1)
         (equal? (matrix* (matrix-2d-orthogonal-projection 1 0) e2) O)
         (equal? (matrix* (matrix-2d-orthogonal-projection 0 1) e1) O)
         (equal? (matrix* (matrix-2d-orthogonal-projection 0 1) e2) e2))))))

