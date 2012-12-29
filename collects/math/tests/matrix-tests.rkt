#lang typed/racket

(require math/array
         math/base
         math/flonum
         math/matrix
         "../private/matrix/matrix-column.rkt"
         "test-utils.rkt")

(: random-matrix (case-> (Integer Integer -> (Matrix Integer))
                         (Integer Integer Integer -> (Matrix Integer))))
;; Generates a random matrix with Natural elements < k. Useful to test properties.
(define (random-matrix m n [k 100])
  (array-strict (build-array (vector m n) (λ (_) (random k)))))

(define nonmatrices
  (list (make-array #() 0)
        (make-array #(1) 0)
        (make-array #(1 0) 0)
        (make-array #(0 1) 0)
        (make-array #(0 0) 0)
        (make-array #(1 1 1) 0)))

;; ===================================================================================================
;; Literal syntax

(check-equal? (matrix [[1]])
              (array #[#[1]]))

(check-equal? (matrix [[1 2 3 4]])
              (array #[#[1 2 3 4]]))

(check-equal? (matrix [[1 2] [3 4]])
              (array #[#[1 2] #[3 4]]))

(check-equal? (matrix [[1] [2] [3] [4]])
              (array #[#[1] #[2] #[3] #[4]]))

(check-equal? (row-matrix [1 2 3 4])
              (matrix [[1 2 3 4]]))

(check-equal? (col-matrix [1 2 3 4])
              (matrix [[1] [2] [3] [4]]))

;; ===================================================================================================
;; Predicates

(check-true (matrix? (array #[#[1]])))
(check-false (matrix? (array #[1])))
(check-false (matrix? (array 1)))
(check-false (matrix? (array #[])))
(for: ([a  (in-list nonmatrices)])
  (check-false (matrix? a)))

(check-true (square-matrix? (matrix [[1]])))
(check-true (square-matrix? (matrix [[1 1] [1 1]])))
(check-false (square-matrix? (matrix [[1 2]])))
(check-false (square-matrix? (matrix [[1] [2]])))
(for: ([a  (in-list nonmatrices)])
  (check-false (square-matrix? a)))

(check-true (row-matrix? (matrix [[1 2 3 4]])))
(check-true (row-matrix? (matrix [[1]])))
(check-false (row-matrix? (matrix [[1] [2] [3] [4]])))
(for: ([a  (in-list nonmatrices)])
  (check-false (row-matrix? a)))

(check-true (col-matrix? (matrix [[1] [2] [3] [4]])))
(check-true (col-matrix? (matrix [[1]])))
(check-false (col-matrix? (matrix [[1 2 3 4]])))
(check-false (col-matrix? (array #[1])))
(check-false (col-matrix? (array 1)))
(check-false (col-matrix? (array #[])))
(for: ([a  (in-list nonmatrices)])
  (check-false (col-matrix? a)))

(check-true (matrix-zero? (make-matrix 4 3 0)))
(check-true (matrix-zero? (make-matrix 4 3 0.0)))
(check-true (matrix-zero? (make-matrix 4 3 0+0.0i)))
(check-false (matrix-zero? (row-matrix [0 0 0 0 1])))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-zero? a))))

;; ===================================================================================================
;; Accessors

;; matrix-shape

(check-equal? (let-values ([(m n)  (matrix-shape (matrix [[1 2 3] [4 5 6]]))])
                (list m n))
              (list 2 3))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (let-values ([(m n)  (matrix-shape a)])
                                        (void)))))

;; square-matrix-size

(check-equal? (square-matrix-size (matrix [[1 2] [3 4]]))
              2)

(check-exn exn:fail:contract? (λ () (square-matrix-size (matrix [[1 2]]))))
(check-exn exn:fail:contract? (λ () (square-matrix-size (matrix [[1] [2]]))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (square-matrix-size a))))

;; matrix-num-rows

(check-equal? (matrix-num-rows (matrix [[1 2 3] [4 5 6]]))
              2)

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-num-rows a))))

;; matrix-num-cols

(check-equal? (matrix-num-cols (matrix [[1 2 3] [4 5 6]]))
              3)

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-num-cols a))))

;; ===================================================================================================
;; Constructors

;; identity-matrix

(check-equal? (identity-matrix 1) (matrix [[1]]))
(check-equal? (identity-matrix 2) (matrix [[1 0] [0 1]]))
(check-equal? (identity-matrix 3) (matrix [[1 0 0] [0 1 0] [0 0 1]]))
(check-exn exn:fail:contract? (λ () (identity-matrix 0)))

;; make-matrix

(check-equal? (make-matrix 1 1 4) (matrix [[4]]))
(check-equal? (make-matrix 2 2 3) (matrix [[3 3] [3 3]]))
(check-exn exn:fail:contract? (λ () (make-matrix 1 0 4)))
(check-exn exn:fail:contract? (λ () (make-matrix 0 1 4)))

;; build-matrix

(check-equal? (build-matrix 4 4 (λ: ([i : Index] [j : Index])
                                  (+ i j)))
              (build-array #(4 4) (λ: ([js : Indexes])
                                    (+ (vector-ref js 0) (vector-ref js 1)))))
(check-exn exn:fail:contract? (λ () (build-matrix 1 0 (λ: ([i : Index] [j : Index]) (+ i j)))))
(check-exn exn:fail:contract? (λ () (build-matrix 0 1 (λ: ([i : Index] [j : Index]) (+ i j)))))

;; diagonal-matrix

(check-equal? (diagonal-matrix '(1 2 3 4))
              (matrix [[1 0 0 0]
                       [0 2 0 0]
                       [0 0 3 0]
                       [0 0 0 4]]))

(check-exn exn:fail:contract? (λ () (diagonal-matrix '())))

;; block-diagonal-matrix

(let ([m  (random-matrix 4 4 100)])
  (check-equal? (block-diagonal-matrix (list m))
                m))

(check-equal?
 (block-diagonal-matrix
  (list (matrix [[1 2] [3 4]])
        (matrix [[1 2 3] [4 5 6]])
        (matrix [[1] [3] [5]])
        (matrix [[2 4 6]])))
 (matrix [[1 2 0 0 0 0 0 0 0]
          [3 4 0 0 0 0 0 0 0]
          [0 0 1 2 3 0 0 0 0]
          [0 0 4 5 6 0 0 0 0]
          [0 0 0 0 0 1 0 0 0]
          [0 0 0 0 0 3 0 0 0]
          [0 0 0 0 0 5 0 0 0]
          [0 0 0 0 0 0 2 4 6]]))

(check-equal?
 (block-diagonal-matrix (map (λ: ([i : Integer]) (matrix [[i]])) '(1 2 3 4)))
 (diagonal-matrix '(1 2 3 4)))

(check-exn exn:fail:contract? (λ () (block-diagonal-matrix '())))

;; Vandermonde matrix

(check-equal? (vandermonde-matrix '(10) 1)
              (matrix [[1]]))
(check-equal? (vandermonde-matrix '(10) 4)
              (matrix [[1 10 100 1000]]))
(check-equal? (vandermonde-matrix '(1 2 3 4) 3)
              (matrix [[1 1 1] [1 2 4] [1 3 9] [1 4 16]]))
(check-exn exn:fail:contract? (λ () (vandermonde-matrix '() 1)))
(check-exn exn:fail:contract? (λ () (vandermonde-matrix '(1) 0)))

;; ===================================================================================================
;; Flat conversion

(check-equal? (list->matrix 1 3 '(1 2 3)) (row-matrix [1 2 3]))
(check-equal? (list->matrix 3 1 '(1 2 3)) (col-matrix [1 2 3]))
(check-exn exn:fail:contract? (λ () (list->matrix 0 1 '())))
(check-exn exn:fail:contract? (λ () (list->matrix 1 0 '())))
(check-exn exn:fail:contract? (λ () (list->matrix 1 1 '(1 2))))

(check-equal? (vector->matrix 1 3 #(1 2 3)) (row-matrix [1 2 3]))
(check-equal? (vector->matrix 3 1 #(1 2 3)) (col-matrix [1 2 3]))
(check-exn exn:fail:contract? (λ () (vector->matrix 0 1 #())))
(check-exn exn:fail:contract? (λ () (vector->matrix 1 0 #())))
(check-exn exn:fail:contract? (λ () (vector->matrix 1 1 #(1 2))))

(check-equal? (->row-matrix '(1 2 3)) (row-matrix [1 2 3]))
(check-equal? (->row-matrix #(1 2 3)) (row-matrix [1 2 3]))
(check-equal? (->row-matrix (row-matrix [1 2 3])) (row-matrix [1 2 3]))
(check-equal? (->row-matrix (col-matrix [1 2 3])) (row-matrix [1 2 3]))
(check-equal? (->row-matrix (make-array #() 1)) (row-matrix [1]))
(check-equal? (->row-matrix (make-array #(3) 1)) (row-matrix [1 1 1]))
(check-equal? (->row-matrix (make-array #(1 3 1) 1)) (row-matrix [1 1 1]))
(check-exn exn:fail:contract? (λ () (->row-matrix (make-array #(2 3 1) 1))))
(check-exn exn:fail:contract? (λ () (->row-matrix (make-array #(1 3 2) 1))))
(check-exn exn:fail:contract? (λ () (->row-matrix (make-array #(0 3) 1))))
(check-exn exn:fail:contract? (λ () (->row-matrix (make-array #(3 0) 1))))

(check-equal? (->col-matrix '(1 2 3)) (col-matrix [1 2 3]))
(check-equal? (->col-matrix #(1 2 3)) (col-matrix [1 2 3]))
(check-equal? (->col-matrix (col-matrix [1 2 3])) (col-matrix [1 2 3]))
(check-equal? (->col-matrix (row-matrix [1 2 3])) (col-matrix [1 2 3]))
(check-equal? (->col-matrix (make-array #() 1)) (col-matrix [1]))
(check-equal? (->col-matrix (make-array #(3) 1)) (col-matrix [1 1 1]))
(check-equal? (->col-matrix (make-array #(1 3 1) 1)) (col-matrix [1 1 1]))
(check-exn exn:fail:contract? (λ () (->col-matrix (make-array #(2 3 1) 1))))
(check-exn exn:fail:contract? (λ () (->col-matrix (make-array #(1 3 2) 1))))
(check-exn exn:fail:contract? (λ () (->col-matrix (make-array #(0 3) 1))))
(check-exn exn:fail:contract? (λ () (->col-matrix (make-array #(3 0) 1))))

(check-equal? (matrix->list (matrix [[1 2 3] [4 5 6]])) '(1 2 3 4 5 6))
(check-equal? (matrix->list (row-matrix [1 2 3])) '(1 2 3))
(check-equal? (matrix->list (col-matrix [1 2 3])) '(1 2 3))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix->list a))))

(check-equal? (matrix->vector (matrix [[1 2 3] [4 5 6]])) #(1 2 3 4 5 6))
(check-equal? (matrix->vector (row-matrix [1 2 3])) #(1 2 3))
(check-equal? (matrix->vector (col-matrix [1 2 3])) #(1 2 3))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix->vector a))))

;; ===================================================================================================
;; Nested conversion

(check-equal? (list*->matrix '((1 2 3) (4 5 6))) (matrix [[1 2 3] [4 5 6]]))
(check-exn exn:fail:contract? (λ () (list*->matrix '((1 2 3) (4 5)))))
(check-exn exn:fail:contract? (λ () (list*->matrix '(() () ()))))
(check-exn exn:fail:contract? (λ () (list*->matrix '())))

(check-equal? ((inst vector*->matrix Integer) #(#(1 2 3) #(4 5 6))) (matrix [[1 2 3] [4 5 6]]))
(check-exn exn:fail:contract? (λ () ((inst vector*->matrix Integer) #(#(1 2 3) #(4 5)))))
(check-exn exn:fail:contract? (λ () ((inst vector*->matrix Integer) #(#() #() #()))))
(check-exn exn:fail:contract? (λ () ((inst vector*->matrix Integer) #())))

(check-equal? (matrix->list* (matrix [[1 2 3] [4 5 6]])) '((1 2 3) (4 5 6)))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix->list* a))))

(check-equal? (matrix->vector* (matrix [[1 2 3] [4 5 6]])) #(#(1 2 3) #(4 5 6)))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix->vector* a))))

;; ===================================================================================================
;; Equality

(check-true (matrix= (matrix [[1 2 3]
                              [4 5 6]])
                     (matrix [[1.0 2.0 3.0]
                              [4.0 5.0 6.0]])))

(check-true (matrix= (matrix [[1 2 3]
                              [4 5 6]])
                     (matrix [[1.0 2.0 3.0]
                              [4.0 5.0 6.0]])
                     (matrix [[1.0+0.0i 2.0+0.0i 3.0+0.0i]
                              [4.0+0.0i 5.0+0.0i 6.0+0.0i]])))

(check-false (matrix= (matrix [[1 2 3] [4 5 6]])
                      (matrix [[1 2 3] [4 5 7]])))

(check-false (matrix= (matrix [[0 2 3] [4 5 6]])
                      (matrix [[1 2 3] [4 5 7]])))

(check-false (matrix= (matrix [[1 2 3] [4 5 6]])
                      (matrix [[1 4] [2 5] [3 6]])))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix= a (matrix [[1]]))))
  (check-exn exn:fail:contract? (λ () (matrix= (matrix [[1]]) a)))
  (check-exn exn:fail:contract? (λ () (matrix= (matrix [[1]]) (matrix [[1]]) a))))

;; ===================================================================================================
;; Pointwise operations

(define-syntax-rule (test-matrix-map (matrix-map ...) (array-map ...))
  (begin
    (for: ([a  (in-list nonmatrices)])
      (check-exn exn:fail:contract? (λ () (matrix-map ... a)))
      (check-exn exn:fail:contract? (λ () (matrix-map ... (matrix [[1]]) a))))
    
    (for*: ([m  '(2 3 4)]
            [n  '(2 3 4)])
      (define a0 (random-matrix m n))
      (define a1 (random-matrix m n))
      (define a2 (random-matrix m n))
      (check-equal? (matrix-map ... a0)
                    (array-map ... a0))
      (check-equal? (matrix-map ... a0 a1)
                    (array-map ... a0 a1))
      (check-equal? (matrix-map ... a0 a1 a2)
                    (array-map ... a0 a1 a2))
      ;; Don't know why this (void) is necessary, but TR complains without it
      (void))))

(test-matrix-map (matrix-map -) (array-map -))
(test-matrix-map ((values matrix-map) -) (array-map -))

(test-matrix-map (matrix+) (array+))
(test-matrix-map ((values matrix+)) (array+))

(test-matrix-map (matrix-) (array-))
(test-matrix-map ((values matrix-)) (array-))

(check-equal? (matrix-sum (list (matrix [[1 2 3] [4 5 6]])))
              (matrix [[1 2 3] [4 5 6]]))
(check-equal? (matrix-sum (list (matrix [[1 2 3] [4 5 6]])
                                (matrix [[0 1 2] [3 4 5]])))
              (matrix+ (matrix [[1 2 3] [4 5 6]])
                       (matrix [[0 1 2] [3 4 5]])))
(check-exn exn:fail:contract? (λ () (matrix-sum '())))

(check-equal? (matrix-scale (matrix [[1 2 3] [4 5 6]]) 10)
              (matrix [[10 20 30] [40 50 60]]))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-scale a 0))))

;; ===================================================================================================
;; Multiplication

(define-syntax-rule (test-matrix* matrix*)
  (begin
    (for: ([a  (in-list nonmatrices)])
      (check-exn exn:fail:contract? (λ () (matrix* a (matrix [[1]])))))
    
    (check-equal? (matrix* (matrix [[1 2 3] [4 5 6] [7 8 9]])
                           (matrix [[1 2 3] [4 5 6] [7 8 9]]))
                  (matrix [[30 36 42] [66 81 96] [102 126 150]]))
    
    (check-equal? (matrix* (row-matrix [1 2 3 4])
                           (col-matrix [1 2 3 4]))
                  (matrix [[30]]))
    
    (check-equal? (matrix* (col-matrix [1 2 3 4])
                           (row-matrix [1 2 3 4]))
                  (matrix [[1  2  3  4]
                           [2  4  6  8]
                           [3  6  9 12]
                           [4  8 12 16]]))
    
    (check-equal? (matrix* (matrix [[3]]) (matrix [[7]]))
                  (matrix [[21]]))
    
    ;; Left/right identity
    (let ([m  (random-matrix 2 2)])
      (check-equal? (matrix* (identity-matrix 2) m)
                    m)
      (check-equal? (matrix* m (identity-matrix 2))
                    m))
    
    ;; Shape
    (let ([m0  (random-matrix 4 5)]
          [m1  (random-matrix 5 2)]
          [m2  (random-matrix 2 10)])
      (check-equal? (let-values ([(m n)  (matrix-shape (matrix* m0 m1))])
                      (list m n))
                    (list 4 2))
      (check-equal? (let-values ([(m n)  (matrix-shape (matrix* m1 m2))])
                      (list m n))
                    (list 5 10))
      (check-equal? (let-values ([(m n)  (matrix-shape (matrix* m0 m1 m2))])
                      (list m n))
                    (list 4 10)))
    
    (check-exn exn:fail? (λ () (matrix* (random-matrix 1 2) (random-matrix 3 2))))
    
    ;; Associativity
    (let ([m0  (random-matrix 4 5)]
          [m1  (random-matrix 5 2)]
          [m2  (random-matrix 2 10)])
      (check-equal? (matrix* m0 m1 m2)
                    (matrix* (matrix* m0 m1) m2))
      (check-equal? (matrix* (matrix* m0 m1) m2)
                    (matrix* m0 (matrix* m1 m2))))
    ))

(test-matrix* matrix*)
;; `matrix*' is an inlining macro, so we need to check the function version as well
(test-matrix* (values matrix*))

;; ===================================================================================================
;; Exponentiation

(let ([A  (matrix [[1 2] [3 4]])])
  (check-equal? (matrix-expt A 0) (identity-matrix 2))
  (check-equal? (matrix-expt A 1) A)
  (check-equal? (matrix-expt A 2) (matrix [[7 10] [15 22]]))
  (check-equal? (matrix-expt A 3) (matrix [[37 54] [81 118]]))
  (check-equal? (matrix-expt A 8) (matrix [[165751 241570] [362355 528106]])))

(check-equal? (matrix-expt (matrix [[2]]) 10) (matrix [[(expt 2 10)]]))

(check-exn exn:fail:contract? (λ () (matrix-expt (row-matrix [1 2 3]) 0)))
(check-exn exn:fail:contract? (λ () (matrix-expt (col-matrix [1 2 3]) 0)))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-expt a 0))))

;; ===================================================================================================
;; Comprehensions

;; for/matrix and friends are defined in terms of for/array and friends, so we only need to test that
;; it works for one case each, and that they properly raise exceptions when given zero-length axes

(check-equal?
 (for/matrix 2 2 ([i  (in-range 4)]) i)
 (matrix [[0 1] [2 3]]))

#;; TR can't type this, but it's defined using exactly the same wrapper as `for/matrix'
(check-equal?
 (for*/matrix 2 2 ([i  (in-range 2)] [j  (in-range 2)]) (+ i j))
 (matrix [[0 1] [1 2]]))

(check-equal?
 (for/matrix: 2 2 ([i  (in-range 4)]) i)
 (matrix [[0 1] [2 3]]))

(check-equal?
 (for*/matrix: 2 2 ([i  (in-range 2)] [j  (in-range 2)]) (+ i j))
 (matrix [[0 1] [1 2]]))

(check-exn exn:fail:contract? (λ () (for/matrix 2 0 () 0)))
(check-exn exn:fail:contract? (λ () (for/matrix 0 2 () 0)))
(check-exn exn:fail:contract? (λ () (for*/matrix 2 0 () 0)))
(check-exn exn:fail:contract? (λ () (for*/matrix 0 2 () 0)))

(check-exn exn:fail:contract? (λ () (for/matrix: 2 0 () 0)))
(check-exn exn:fail:contract? (λ () (for/matrix: 0 2 () 0)))
(check-exn exn:fail:contract? (λ () (for*/matrix: 2 0 () 0)))
(check-exn exn:fail:contract? (λ () (for*/matrix: 0 2 () 0)))

;; ===================================================================================================
;; Extraction

;; matrix-ref

(let ([a  (matrix [[10 11] [12 13]])])
  (check-equal? (matrix-ref a 0 0) 10)
  (check-equal? (matrix-ref a 0 1) 11)
  (check-equal? (matrix-ref a 1 0) 12)
  (check-equal? (matrix-ref a 1 1) 13)
  (check-exn exn:fail? (λ () (matrix-ref a 2 0)))
  (check-exn exn:fail? (λ () (matrix-ref a 0 2)))
  (check-exn exn:fail? (λ () (matrix-ref a -1 0)))
  (check-exn exn:fail? (λ () (matrix-ref a 0 -1))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-ref a 0 0))))

;; matrix-diagonal

(check-equal? (matrix-diagonal (diagonal-matrix '(1 2 3 4)))
              (array #[1 2 3 4]))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-diagonal a))))

;; submatrix

(check-equal? (submatrix (identity-matrix 8) (:: 2 4) (:: 2 4))
              (identity-matrix 2))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (submatrix a '(0) '(0)))))

;; matrix-row

(let ([a  (matrix [[1 2 3] [4 5 6]])])
  (check-equal? (matrix-row a 0) (row-matrix [1 2 3]))
  (check-equal? (matrix-row a 1) (row-matrix [4 5 6]))
  (check-exn exn:fail? (λ () (matrix-row a -1)))
  (check-exn exn:fail? (λ () (matrix-row a 2))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-row a 0))))

;; matrix-col

(let ([a  (matrix [[1 2 3] [4 5 6]])])
  (check-equal? (matrix-col a 0) (col-matrix [1 4]))
  (check-equal? (matrix-col a 1) (col-matrix [2 5]))
  (check-equal? (matrix-col a 2) (col-matrix [3 6]))
  (check-exn exn:fail? (λ () (matrix-col a -1)))
  (check-exn exn:fail? (λ () (matrix-col a 3))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-col a 0))))

;; matrix-rows

(check-equal? (matrix-rows (matrix [[1 2 3] [4 5 6]]))
              (list (row-matrix [1 2 3])
                    (row-matrix [4 5 6])))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-rows a))))

;; matrix-cols

(check-equal? (matrix-cols (matrix [[1 2 3] [4 5 6]]))
              (list (col-matrix [1 4])
                    (col-matrix [2 5])
                    (col-matrix [3 6])))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-cols a))))

;; ===================================================================================================
;; Embiggenment (it's a perfectly cromulent word)

;; matrix-augment

(let ([a  (random-matrix 3 5)])
  (check-equal? (matrix-augment (list a)) a)
  (check-equal? (matrix-augment (matrix-cols a)) a))

(check-equal? (matrix-augment (list (col-matrix [1 2 3]) (col-matrix [4 5 6])))
              (matrix [[1 4] [2 5] [3 6]]))

(check-equal? (matrix-augment (list (matrix [[1 2] [4 5]]) (col-matrix [3 6])))
              (matrix [[1 2 3] [4 5 6]]))

(check-exn exn:fail? (λ () (matrix-augment (list (matrix [[1 2] [4 5]]) (col-matrix [3])))))
(check-exn exn:fail:contract? (λ () (matrix-augment '())))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-augment (list a))))
  (check-exn exn:fail:contract? (λ () (matrix-augment (list (matrix [[1]]) a)))))

;; matrix-stack

(let ([a  (random-matrix 5 3)])
  (check-equal? (matrix-stack (list a)) a)
  (check-equal? (matrix-stack (matrix-rows a)) a))

(check-equal? (matrix-stack (list (row-matrix [1 2 3]) (row-matrix [4 5 6])))
              (matrix [[1 2 3] [4 5 6]]))

(check-equal? (matrix-stack (list (matrix [[1 2 3] [4 5 6]]) (row-matrix [7 8 9])))
              (matrix [[1 2 3] [4 5 6] [7 8 9]]))

(check-exn exn:fail? (λ () (matrix-stack (list (matrix [[1 2 3] [4 5 6]]) (row-matrix [7 8])))))
(check-exn exn:fail:contract? (λ () (matrix-stack '())))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-stack (list a))))
  (check-exn exn:fail:contract? (λ () (matrix-stack (list (matrix [[1]]) a)))))

;; ===================================================================================================
;; Inner product space

;; matrix-norm

(check-equal? (matrix-norm (matrix [[1 2 3] [4 5 6]]))
              (sqrt (+ (* 1 1) (* 2 2) (* 3 3) (* 4 4) (* 5 5) (* 6 6))))

;; Default norm is Frobenius norm
(check-equal? (matrix-norm (matrix [[1 2 3] [4 5 6]]))
              (matrix-norm (matrix [[1 2 3] [4 5 6]]) 2))

;; This shouldn't overflow (so we check against `flhypot', which also shouldn't overflow)
(check-equal? (matrix-norm (matrix [[1e200 1e199]]))
              (flhypot 1e200 1e199))

;; Taxicab (Manhattan) norm
(check-equal? (matrix-norm (matrix [[1 2 3] [4 5 6]]) 1)
              (+ 1 2 3 4 5 6))

;; Infinity (maximum) norm
(check-equal? (matrix-norm (matrix [[1 2 3] [4 5 6]]) +inf.0)
              (max 1 2 3 4 5 6))

;; The actual norm is indistinguishable from floating-point 6
(check-equal? (matrix-norm (matrix [[1 2 3] [4 5 6]]) 1000)
              6.0)

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-norm a 1)))
  (check-exn exn:fail:contract? (λ () (matrix-norm a)))
  (check-exn exn:fail:contract? (λ () (matrix-norm a 5)))
  (check-exn exn:fail:contract? (λ () (matrix-norm a +inf.0))))

(check-equal? (matrix-norm (row-matrix [1+1i]))
              (sqrt 2))

(check-equal? (matrix-norm (row-matrix [1+1i 2+2i 3+3i]))
              (matrix-norm (row-matrix [(magnitude 1+1i) (magnitude 2+2i) (magnitude 3+3i)])))

;; matrix-dot (induces the Frobenius norm)

(check-equal? (matrix-dot (matrix [[1 -2 3] [-4 5 -6]])
                          (matrix [[-1 2 -3] [4 -5 6]]))
              (+ (* 1 -1) (* -2 2) (* 3 -3) (* -4 4) (* 5 -5) (* -6 6)))

(check-equal? (matrix-dot (row-matrix [1 2 3])
                          (row-matrix [0+4i 0-5i 0+6i]))
              (+ (* 1 0-4i) (* 2 0+5i) (* 3 0-6i)))

(check-exn exn:fail? (λ () (matrix-dot (random-matrix 1 3) (random-matrix 3 1))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-dot a (matrix [[1]]))))
  (check-exn exn:fail:contract? (λ () (matrix-dot (matrix [[1]]) a))))

;; ===================================================================================================
;; Simple operators

;; matrix-transpose

(check-equal? (matrix-transpose (matrix [[1 2 3] [4 5 6]]))
              (matrix [[1 4] [2 5] [3 6]]))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-transpose a))))

;; matrix-conjugate

(check-equal? (matrix-conjugate (matrix [[1+i 2-i] [3+i 4-i]]))
              (matrix [[1-i 2+i] [3-i 4+i]]))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-conjugate a))))

;; matrix-hermitian

(let ([a  (array-make-rectangular (random-matrix 5 6)
                                  (random-matrix 5 6))])
  (check-equal? (matrix-hermitian a)
                (matrix-conjugate (matrix-transpose a)))
  (check-equal? (matrix-hermitian a)
                (matrix-transpose (matrix-conjugate a))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-hermitian a))))

;; matrix-trace

(check-equal? (matrix-trace (matrix [[1 2 3] [4 5 6] [7 8 9]]))
              (+ 1 5 9))

(check-exn exn:fail:contract? (λ () (matrix-trace (row-matrix [1 2 3]))))
(check-exn exn:fail:contract? (λ () (matrix-trace (col-matrix [1 2 3]))))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-trace a))))

;; ===================================================================================================
;; Gaussian elimination

(check-equal? (matrix-row-echelon (matrix [[2 4] [3 4]]) #f #f)
              (matrix [[3 4] [0 4/3]]))

(check-equal? (matrix-row-echelon (matrix [[2 4] [3 4]]) #f #t)
              (matrix [[1 4/3] [0 1]]))

(check-equal? (matrix-row-echelon (matrix [[1 2] [2 4]]) #f #f)
              (matrix [[2 4] [0 0]]))

(check-equal? (matrix-row-echelon (matrix [[1 4] [2 4]]) #f #t)
              (matrix [[1 2] [0 1]]))

(check-equal? (matrix-row-echelon (matrix [[ 2  1 -1   8]
                                           [-3 -1  2 -11]
                                           [-2  1  2  -3]])
                                  #f #t)
              (matrix [[1 1/3 -2/3 11/3]
                       [0  1   2/5 13/5]
                       [0  0    1   -1]]))

(check-equal? (matrix-row-echelon (matrix [[ 2  1 -1   8]
                                           [-3 -1  2 -11]
                                           [-2  1  2  -3]])
                                  #t)
              (matrix [[1 0 0  2]
                       [0 1 0  3]
                       [0 0 1 -1]]))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-row-echelon a))))

(check-equal? (matrix-rank (matrix [[0 0] [0 0]])) 0)
(check-equal? (matrix-rank (matrix [[1 0] [0 0]])) 1)
(check-equal? (matrix-rank (matrix [[1 0] [0 3]])) 2)
(check-equal? (matrix-rank (matrix [[1 2] [2 4]])) 1)
(check-equal? (matrix-rank (matrix [[1 2] [3 4]])) 2)
(check-equal? (matrix-rank (matrix [[1 2 3]])) 1)
(check-equal? (matrix-rank (matrix [[1 2 3] [2 3 5]])) 2)
(check-equal? (matrix-rank (matrix [[1 2 3] [2 3 5] [3 4 7]])) 2)
(check-equal? (matrix-rank (matrix [[1 2 3] [2 3 5] [3 4 7] [4 5 9]])) 2)
(check-equal? (matrix-rank (matrix [[1 2 3 5] [2 3 5 8]])) 2)
(check-equal? (matrix-rank (matrix [[1 5 2 3] [2 8 3 5]])) 2)
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-rank a))))

(check-equal? (matrix-nullity (matrix [[0 0] [0 0]])) 2)
(check-equal? (matrix-nullity (matrix [[1 0] [0 0]])) 1)
(check-equal? (matrix-nullity (matrix [[1 0] [0 3]])) 0)
(check-equal? (matrix-nullity (matrix [[1 2] [2 4]])) 1)
(check-equal? (matrix-nullity (matrix [[1 2] [3 4]])) 0)
(check-equal? (matrix-nullity (matrix [[1 2 3]])) 2)
(check-equal? (matrix-nullity (matrix [[1 2 3] [2 3 5]])) 1)
(check-equal? (matrix-nullity (matrix [[1 2 3] [2 3 5] [3 4 7]])) 1)
(check-equal? (matrix-nullity (matrix [[1 2 3] [2 3 5] [3 4 7] [4 5 9]])) 1)
(check-equal? (matrix-nullity (matrix [[1 2 3 5] [2 3 5 8]])) 2)
(check-equal? (matrix-nullity (matrix [[1 5 2 3] [2 8 3 5]])) 2)
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-nullity a))))

;; ===================================================================================================
;; Determinant

(check-equal? (matrix-determinant (matrix [[3]])) 3)
(check-equal? (matrix-determinant (matrix [[1 2] [3 4]])) (- (* 1 4) (* 2 3)))
(check-equal? (matrix-determinant (matrix [[1 2 3] [4  5 6] [7 8 9]])) 0)
(check-equal? (matrix-determinant (matrix [[1 2 3] [4 -5 6] [7 8 9]])) 120)
(check-equal? (matrix-determinant (matrix [[1 2 3 4]
                                           [-5 6 7 8]
                                           [9 10 -11 12]
                                           [13 14 15 16]]))
              5280)

(for: ([_  (in-range 100)])
  (define a (array- (random-matrix 3 3 7) (array 3)))
  (check-equal? (matrix-determinant/row-reduction a)
                (matrix-determinant a)))

(check-exn exn:fail:contract? (λ () (matrix-determinant (matrix [[1 2 3] [4 5 6]]))))
(check-exn exn:fail:contract? (λ () (matrix-determinant (matrix [[1 4] [2 5] [3 6]]))))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-determinant a))))

;; ===================================================================================================
;; Solving linear systems

(for: ([_  (in-range 100)])
  (define M (array- (random-matrix 3 3 7) (array 3)))
  (define B (array- (random-matrix 3 (+ 1 (random 10)) 7) (array 3)))
  (cond [(matrix-invertible? M)
         (define X (matrix-solve M B))
         (check-equal? (matrix* M X) B (format "M = ~a  B = ~a" M B))]
        [else
         (check-false (matrix-solve M B (λ () #f))
                      (format "M = ~a  B = ~a" M B))]))

(check-exn exn:fail? (λ () (matrix-solve (random-matrix 3 4) (random-matrix 3 1))))
(check-exn exn:fail? (λ () (matrix-solve (random-matrix 4 3) (random-matrix 4 1))))

(check-exn exn:fail:contract? (λ () (matrix-solve (random-matrix 3 4) (random-matrix 4 1))))
(check-exn exn:fail:contract? (λ () (matrix-solve (random-matrix 4 3) (random-matrix 3 1))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-solve a (matrix [[1]]))))
  (check-exn exn:fail:contract? (λ () (matrix-solve (matrix [[1]]) a))))

;; ===================================================================================================
;; Inversion

(for: ([_  (in-range 100)])
  (define a (array- (random-matrix 3 3 7) (array 3)))
  (cond [(matrix-invertible? a)
         (check-equal? (matrix* a (matrix-inverse a))
                       (identity-matrix 3)
                       (format "~a" a))
         (check-equal? (matrix* (matrix-inverse a) a)
                       (identity-matrix 3)
                       (format "~a" a))]
        [else
         (check-false (matrix-inverse a (λ () #f))
                      (format "~a" a))]))

(check-exn exn:fail:contract? (λ () (matrix-inverse (random-matrix 3 4))))
(check-exn exn:fail:contract? (λ () (matrix-inverse (random-matrix 4 3))))

(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-inverse a))))

;; ===================================================================================================
;; LU decomposition

(let ([M  (matrix [[ 1  1  0  3]
                   [ 2  1 -1  1]
                   [ 3 -1 -1  2]
                   [-1  2  3 -1]])])
  (define-values (L V) (matrix-lu M))
  (check-equal? L (matrix [[ 1  0 0 0]
                           [ 2  1 0 0]
                           [ 3  4 1 0]
                           [-1 -3 0 1]]))
  (check-equal? V (matrix [[1  1  0   3]
                           [0 -1 -1  -5]
                           [0  0  3  13]
                           [0  0  0 -13]]))
  (check-equal? (matrix* L V) M))

(: matrix-l ((Matrix Number) -> Any))
(define (matrix-l M)
  (define-values (L U) (matrix-lu M))
  L)

(check-exn exn:fail? (λ () (matrix-l (matrix [[1 1 0 2]
                                              [0 2 0 1]
                                              [1 0 0 0]
                                              [1 1 2 1]]))))

(check-exn exn:fail:contract? (λ () (matrix-l (random-matrix 3 4))))
(check-exn exn:fail:contract? (λ () (matrix-l (random-matrix 4 3))))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-l a))))

#|
;; ===================================================================================================
;; Tests not yet converted to rackunit

(matrix-gram-schmidt
           (matrix [[2 1 2]
                    [2 2 3]
                    [5 1 5]])
            #t)

(begin
  
  (begin
    "matrix-operations.rkt"
    #;
    (list 'column-dimension
          (= (column-dimension #(1 2 3)) 3)
          (= (column-dimension (vector->matrix 1 2 #(1 2))) 1))
    (let ([matrix: vector->matrix])
      (list 'column-dot
            (= (column-dot (col-matrix [1 2])   (col-matrix [1 2])) 5)
            (= (column-dot (col-matrix [1 2])   (col-matrix [3 4])) 11)
            (= (column-dot (col-matrix [3 4])   (col-matrix [3 4])) 25)
            (= (column-dot (col-matrix [1 2 3]) (col-matrix [4 5 6]))
               (+ (* 1 4) (* 2 5) (* 3 6)))
            (= (column-dot (col-matrix [+3i +4i]) (col-matrix [+3i +4i]))
               25)))
    (let ([matrix: vector->matrix])
      (list 'column-norm
            (= (column-norm (col-matrix [2 4])) (sqrt 20))))
    (list 'column-project
          (equal? (column-project #(1 2 3) #(4 5 6)) (col-matrix [128/77 160/77 192/77]))
          (equal? (column-project (col-matrix [1 2 3]) (col-matrix [2 4 3]))
                  (matrix-scale (col-matrix [2 4 3]) 19/29)))
    (list 'projection-on-orthogonal-basis
          (equal? (projection-on-orthogonal-basis #(3 -2 2) (list #(-1 0 2) #( 2 5 1)))
                  (col-matrix [-1/3 -1/3 1/3]))
          (equal? (projection-on-orthogonal-basis 
                   (col-matrix [3 -2 2]) (list #(-1 0 2) (col-matrix [2 5 1])))
                  (col-matrix [-1/3 -1/3 1/3])))
    (list 'projection-on-orthonormal-basis
          (equal? (projection-on-orthonormal-basis 
                   #(1 2 3 4) 
                   (list (matrix-scale (col-matrix [ 1  1  1  1]) 1/2)
                         (matrix-scale (col-matrix [-1  1 -1  1]) 1/2)
                         (matrix-scale (col-matrix [ 1 -1 -1  1]) 1/2)))
                  (col-matrix [2 3 2 3])))
    (list 'gram-schmidt-orthogonal
          (equal? (gram-schmidt-orthogonal (list #(3 1) #(2 2)))
                  (list (col-matrix [3 1]) (col-matrix [-2/5 6/5]))))
    (list 'vector-normalize
          (equal? (column-normalize #(3 4)) 
                  (col-matrix [3/5 4/5])))
    (list 'gram-schmidt-orthonormal
          (equal? (gram-schmidt-orthonormal (ann '(#(3 1) #(2 2)) (Listof (Column Number))))
                  (list (column-normalize #(3 1))
                        (column-normalize #(-2/5 6/5)))))
    
    (list 'projection-on-subspace
          (equal? (projection-on-subspace #(1 2 3) '(#(2 4 3)))
                  (matrix-scale (col-matrix [2 4 3]) 19/29)))
    (list 'unit-vector
          (equal? (unit-column 4 1) (col-matrix [0 1 0 0])))
    (list 'matrix-qr
          (let-values ([(Q R) (matrix-qr (matrix [[1 1] [0 1] [1 1]]))])
            (equal? (list Q R)
                    (list (matrix [[0.7071067811865475 0]
                                   [0 1]
                                   [0.7071067811865475 0]])
                          (matrix [[1.414213562373095 1.414213562373095]
                                   [0 1]]))))
          (let ()
            (define A (matrix [[1 2 3 4] [1 2 4 5] [1 2 5 6] [1 2 6 7]]))
            (define-values (Q R) (matrix-qr A))
            (equal? (list Q R)
                    (list
                     (vector->matrix 
                      4 4 ((inst vector Number)
                           1/2 -0.6708203932499369    0.5477225575051662  -0.0
                           1/2 -0.22360679774997896  -0.7302967433402214   0.4082482904638629
                           1/2  0.22360679774997896  -0.18257418583505536 -0.8164965809277259
                           1/2  0.6708203932499369    0.3651483716701107   0.408248290463863))
                     (vector->matrix 
                      4 4 ((inst vector Number)
                           2 4 9 11 0 0.0 2.23606797749979 2.23606797749979 
                           0 0 0.0 4.440892098500626e-16 0 0 0 0.0))))))
  #;
  (begin
    "matrix-2d.rkt"
    (let ()
      (define  e1  (matrix-transpose (vector->matrix #(#( 1  0)))))
      (define  e2  (matrix-transpose (vector->matrix #(#( 0  1)))))
      (define -e1  (matrix-transpose (vector->matrix #(#(-1  0)))))
      (define -e2  (matrix-transpose (vector->matrix #(#( 0 -1)))))
      (define   O  (matrix-transpose (vector->matrix #(#( 0  0)))))
      (define 2*e1 (matrix-scale e1 2))
      (define 4*e1 (matrix-scale e1 4))
      (define 3*e2 (matrix-scale e2 3))
      (define 4*e2 (matrix-scale e2 4))
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
|#
