#lang typed/racket

(require math/array
         math/constants
         math/flonum)
(require "matrix-types.rkt"
         "matrix-pointwise.rkt"
         "matrix-constructors.rkt"
         "matrix-multiply.rkt"
         "matrix-expt.rkt"
         "matrix-operations.rkt"
         "matrix-2d.rkt")

(begin
  (begin
    "matrix-operations.rkt"
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
      (: gauss-eliminate : (Matrix Number) Boolean Boolean -> (Result-Matrix Number))
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
             (list->array real? '[[2 0 0] [0 1 0] [0 0 1]])))
    (list
     'matrix-swap-rows
     (equal? (matrix-swap-rows (list->array real? '[[1 2 3] [4 5 6] [7 8 9]]) 0 1)
             (list->array real? '[[4 5 6] [1 2 3] [7 8 9]])))
    (list
     'matrix-add-scaled-row
     (equal? (matrix-add-scaled-row (list->array real? '[[1 2 3] [4 5 6] [7 8 9]]) 0 2 1)
             (list->array real? '[[9 12 15] [4 5 6] [7 8 9]])))
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
                                  
  (begin "matrix-types.rkt"
         (list
          'array-matrix?
          (array-matrix? (list->array real? '[[1 2] [3 4]]))
          (not (array-matrix? (list->array real? '[[[1 2] [3 4]] [[1 2] [3 4]]]))))
         (list
          'square-matrix?
          (square-matrix? (list->array real? '[[1 2] [3 4]]))
          (not (square-matrix? (list->array real? '[[1 2 3] [4 5 6]]))))
         (list
          'square-matrix-size
          (= 2 (square-matrix-size (list->array real? '[[1 2 3] [4 5 6]]))))
         (list
          'matrix=-
          (matrix= (list->array real? '[[1 2] [3 4]]) (list->array real? '[[1 2] [3 4]]))
          (not (matrix= (list->array real? '[[1 2] [3 4]]) (list->array real? '[[1 2]]))))
         (list
          'matrix-dimensions
          (let-values ([(m n) (matrix-dimensions (list->matrix '[[1 2 3] [4 5 6]]))])
            (equal? (list m n) '(2 3)))))
  
  (begin "matrix-constructors.rkt"
         (list
          'identity-matrix
          (equal? (array->list (identity-matrix 1)) '[[1]])
          (equal? (array->list (identity-matrix 2)) '[[1 0] [0 1]])
          (equal? (array->list (identity-matrix 3)) '[[1 0 0] [0 1 0] [0 0 1]]) 
          (equal? (array->list (flidentity-matrix 1)) '[[1.]])
          (equal? (array->list (flidentity-matrix 2)) '[[1. 0.] [0. 1.]])
          (equal? (array->list (flidentity-matrix 3)) '[[1. 0. 0.] [0. 1. 0.] [0. 0. 1.]]))
         (list
          'const-matrix
          (equal? (array->list (const-matrix 2 3 0)) '((0 0 0) (0 0 0)))
          (equal? (array->list (const-matrix 2 3 0.)) '((0. 0. 0.) (0. 0. 0.))))
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
    "matrix-multiply.rkt"
    (list 'matrix*
          (let ()
            (define-values (A B AB) (values '[[1 2] [3 4]] '[[5 6] [7 8]] '[[19 22] [43 50]]))
            (equal? (matrix* (list->matrix A) (list->matrix B)) (list->matrix AB)))
          (let () 
            (define-values (A B AB) (values '[[1 2] [3 4]] '[[5 6 7] [8 9 10]] '[[21 24 27] [47 54 61]]))
            (equal? (matrix* (list->matrix A) (list->matrix B)) (list->matrix AB)))))
  (begin
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
      (list
       (list 'matrix-2d-rotation
             (<= (matrix-norm (matrix- (matrix* (matrix-2d-rotation (/ pi.0 2)) e1) e2 )) +epsilon.0) 
             (<= (matrix-norm (matrix- (matrix* (matrix-2d-rotation (/ pi.0 2)) e2) -e1)) +epsilon.0))
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
