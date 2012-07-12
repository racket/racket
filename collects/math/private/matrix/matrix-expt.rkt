#lang typed/racket

(require "../../array.rkt"
         "matrix-constructors.rkt"
         "matrix-multiply.rkt"
         "utils.rkt")

(provide square-matrix? 
         square-matrix-size
         matrix-expt
         flmatrix-expt)

(define-type (Matrix A) (Array A))

(: square-matrix? : (All (A) (Matrix A) -> Boolean))
(define (square-matrix? a)
  (and (array-matrix? a)
       (let ([sh (unsafe-array-shape a)])
         (= (vector-ref sh 0) (vector-ref sh 1)))))

(: square-matrix-size : (All (A) (Matrix A) -> Index))
(define (square-matrix-size a)
  (vector-ref (unsafe-array-shape a) 0))

(: matrix-expt : (Matrix Number) Integer -> (Matrix Number))
(define (matrix-expt a n)
  (unless (array-matrix? a)
    (raise-type-error 'matrix-expt "(Matrix Number)" a))
  (unless (square-matrix? a)
    (error 'matrix-expt "Square matrix expected, got ~a" a))
  (cond
    [(= n 0)  (identity-matrix (square-matrix-size a))]
    [(= n 1)  a]
    [(= n 2)  (matrix* a a)]
    [(even? n) (let ([a^n/2 (matrix-expt a (quotient n 2))])
                 (matrix* a^n/2 a^n/2))]
    [else     (matrix* a (matrix-expt a (sub1 n)))]))

(: flmatrix-expt : (Matrix Real) Integer -> (Matrix Real))
(define (flmatrix-expt a n)
  (unless (array-matrix? a)
    (raise-type-error 'flmatrix-expt "(Matrix Real)" a))
  (unless (square-matrix? a)
    (error 'matrix-expt "Square matrix expected, got ~a" a))
  (cond
    [(= n 0)  (flidentity-matrix (square-matrix-size a))]
    [(= n 1)  a]
    [(= n 2)  (matrix* a a)]
    [(even? n) (let ([a^n/2 (flmatrix-expt a (quotient n 2))])
                 (matrix* a^n/2 a^n/2))]
    [else     (matrix* a (flmatrix-expt a (sub1 n)))]))

(module* test typed/racket
  (require typed/rackunit
           (submod "..")
           "matrix-constructors.rkt")
  
  (check-true  (square-matrix? (list->matrix '[[1 2 3] [1 2 3] [1 2 3]])))
  (check-false (square-matrix? (list->matrix '[[1 2 3] [1 2 3]])))
  
  (let ()
    (define A (list->matrix '[[1 2] [3 4]]))
    (check-equal? (matrix->list (matrix-expt A 0)) (matrix->list (identity-matrix 2)))
    (check-equal? (matrix->list (matrix-expt A 1)) (matrix->list A))
    (check-equal? (matrix->list (matrix-expt A 2)) '[[7 10] [15 22]])
    (check-equal? (matrix->list (matrix-expt A 3)) '[[37 54] [81 118]])
    (check-equal? (matrix->list (matrix-expt A 8)) '[[165751 241570] [362355 528106]]))
  (let ()
    (define A (fllist->matrix '[[1. 2.] [3. 4.]]))
    (check-equal? (matrix->list (flmatrix-expt A 0)) (matrix->list (flidentity-matrix 2)))
    (check-equal? (matrix->list (flmatrix-expt A 1)) (matrix->list A))
    (check-equal? (matrix->list (flmatrix-expt A 2)) '[[7. 10.] [15. 22.]])
    (check-equal? (matrix->list (flmatrix-expt A 3)) '[[37. 54.] [81. 118.]])
    (check-equal? (matrix->list (flmatrix-expt A 8)) '[[165751. 241570.] [362355. 528106.]])))
