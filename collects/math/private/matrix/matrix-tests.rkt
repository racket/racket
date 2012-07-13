#lang typed/racket

(require math/array)
(require "matrix-types.rkt"
         "matrix-pointwise.rkt"
         "matrix-constructors.rkt"
         "matrix-multiply.rkt"
         "matrix-expt.rkt")

(begin "matrix-types.rkt"
  (list
   'array-matrix?
   (array-matrix? (list->array real? '[[1 2] [3 4]]))
   (not (array-matrix? (list->array real? '[[[1 2] [3 4]] [[1 2] [3 4]]])))
   'square-matrix?
   (square-matrix? (list->array real? '[[1 2] [3 4]]))
   (not (square-matrix? (list->array real? '[[1 2 3] [4 5 6]])))
   'square-matrix-size
   (= 2 (square-matrix-size (list->array real? '[[1 2 3] [4 5 6]])))
   'matrix=-
   (matrix= (list->array real? '[[1 2] [3 4]]) (list->array real? '[[1 2] [3 4]]))
   (not (matrix= (list->array real? '[[1 2] [3 4]]) (list->array real? '[[1 2]])))))

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
    (equal? (matrix->vector (flvector->matrix '#(#(1. 2.) #(3. 4.)))) '#(#(1. 2.) #(3. 4.)))))

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
