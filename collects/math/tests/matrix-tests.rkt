#lang typed/racket

(require math/array
         math/base
         math/flonum
         math/matrix
         "test-utils.rkt")

(define-syntax (check-matrix=? stx)
  (syntax-case stx ()
    [(_ a b)
     (syntax/loc stx (check-true (matrix=? a b) (format "(matrix=? ~v ~v)" a b)))]
    [(_ a b eps)
     (syntax/loc stx (check-true (matrix=? a b eps) (format "(matrix=? ~v ~v ~v)" a b eps)))]))

(: random-matrix (case-> (Integer Integer -> (Matrix Integer))
                         (Integer Integer Integer -> (Matrix Integer))
                         (Integer Integer Integer Integer -> (Matrix Integer))))
;; Generates a random matrix with Natural elements < k. Useful to test properties.
(define random-matrix
  (case-lambda
    [(m n)  (random-matrix m n 100)]
    [(m n k)  (array-strict (build-matrix m n (λ (i j) (random-natural k))))]
    [(m n k0 k1)  (array-strict (build-matrix m n (λ (i j) (random-integer k0 k1))))]))

(define nonmatrices
  (list (make-array #() 0)
        (make-array #(1) 0)
        (make-array #(1 0) 0)
        (make-array #(0 1) 0)
        (make-array #(0 0) 0)
        (make-array #(1 1 1) 0)))

(: matrix-l ((Matrix Number) -> (Matrix Number)))
(define (matrix-l M)
  (define-values (L U) (matrix-lu M))
  L)

(: matrix-q ((Matrix Number) -> (Matrix Number)))
(define (matrix-q M)
  (define-values (Q R) (matrix-qr M))
  Q)

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

;; for:/matrix and friends are defined in terms of for:/array and friends, so we only need to test
;; that it works for one case each, and that they properly raise exceptions when given zero-length
;; axes

(check-equal?
 (for/matrix: 2 2 ([i  (in-range 4)]) i)
 (matrix [[0 1] [2 3]]))

(check-equal?
 (for*/matrix: 2 2 ([i  (in-range 2)] [j  (in-range 2)]) (+ i j))
 (matrix [[0 1] [1 2]]))

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

;; TODO: matrix-upper-triangle

;; TODO: matrix-lower-triangle

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

;; TODO: matrix-angle

;; TODO: matrix-normalize

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

(let ([a  (array-make-rectangular (random-matrix 5 6 -100 100)
                                  (random-matrix 5 6 -100 100))])
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
;; Row/column operators

;; TODO: matrix-map-rows

;; TODO: matrix-map-cols

;; TODO: matrix-normalize-rows

;; TODO: matrix-normalize-cols

;; ===================================================================================================
;; Operator norms

;; TODO: matrix-op-1norm

;; TODO: matrix-op-2norm (after it's implemented)

;; TODO: matrix-op-inf-norm

;; ===================================================================================================
;; Error

(for*: ([x  (in-list '(-inf.0 -10.0 -1.0 -0.1 -0.0 0.0 0.1 1.0 10.0 +inf.0 +nan.0))]
        [y  (in-list '(-inf.0 -10.0 -1.0 -0.1 -0.0 0.0 0.1 1.0 10.0 +inf.0 +nan.0))])
  (check-eqv? (fl (matrix-absolute-error (row-matrix [x])
                                         (row-matrix [y])))
              (fl (absolute-error x y))
              (format "x = ~v  y = ~v" x y))
  (check-eqv? (fl (matrix-relative-error (row-matrix [x])
                                         (row-matrix [y])))
              (fl (relative-error x y))
              (format "x = ~v  y = ~v" x y)))

(check-equal? (matrix-absolute-error (row-matrix [1 2])
                                     (row-matrix [1 2]))
              0)

(check-equal? (matrix-absolute-error (row-matrix [1 2])
                                     (row-matrix [2 2]))
              1)

(check-equal? (matrix-absolute-error (row-matrix [1 2])
                                     (row-matrix [2 +nan.0]))
              +inf.0)

(check-equal? (matrix-relative-error (row-matrix [1 2])
                                     (row-matrix [1 2]))
              0)

(check-equal? (matrix-relative-error (row-matrix [1 2])
                                     (row-matrix [2 2]))
              (/ 1 (matrix-op-inf-norm (row-matrix [2 2]))))

(check-equal? (matrix-relative-error (row-matrix [1 2])
                                     (row-matrix [2 +nan.0]))
              +inf.0)

;; TODO: matrix-basis-angle

;; ===================================================================================================
;; Approximate predicates

;; matrix-zero? (TODO: approximations)

(check-true (matrix-zero? (make-matrix 4 3 0)))
(check-true (matrix-zero? (make-matrix 4 3 0.0)))
(check-true (matrix-zero? (make-matrix 4 3 0+0.0i)))
(check-false (matrix-zero? (row-matrix [0 0 0 0 1])))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-zero? a))))

;; TODO: matrix-rows-orthogonal?

;; TODO: matrix-cols-orthogonal?

;; TODO: matrix-identity?

;; TODO: matrix-orthonormal?

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
                                  #t #t 'partial)
              (matrix [[1 0 0  2]
                       [0 1 0  3]
                       [0 0 1 -1]]))

(check-equal? (matrix-row-echelon (matrix [[ 2  1 -1   8]
                                           [-3 -1  2 -11]
                                           [-2  1  2  -3]])
                                  #t #t 'first)
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
  (define a (random-matrix 3 3 -3 4))
  (check-equal? (matrix-determinant/row-reduction a)
                (matrix-determinant a)))

(check-exn exn:fail:contract? (λ () (matrix-determinant (matrix [[1 2 3] [4 5 6]]))))
(check-exn exn:fail:contract? (λ () (matrix-determinant (matrix [[1 4] [2 5] [3 6]]))))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-determinant a))))

;; ===================================================================================================
;; Solving linear systems

(for: ([_  (in-range 100)])
  (define M (random-matrix 3 3 -3 4))
  (define B (random-matrix 3 (+ 1 (random 10)) -3 4))
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
  (define a (random-matrix 3 3 -3 4))
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

(check-exn exn:fail? (λ () (matrix-l (matrix [[1 1 0 2]
                                              [0 2 0 1]
                                              [1 0 0 0]
                                              [1 1 2 1]]))))

(check-exn exn:fail:contract? (λ () (matrix-l (random-matrix 3 4))))
(check-exn exn:fail:contract? (λ () (matrix-l (random-matrix 4 3))))
(for: ([a  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-l a))))

;; ===================================================================================================
;; Gram-Schmidt

(check-equal? (matrix-gram-schmidt (matrix [[3 2] [1 2]]))
              (matrix [[3 -2/5] [1 6/5]]))

(check-equal? (matrix-gram-schmidt (matrix [[3 2] [1 2]]) #t)
              (matrix-scale (matrix [[3 -1] [1 3]]) (sqrt 1/10)))

(check-equal? (matrix-gram-schmidt (matrix [[12 -51   4]
                                            [ 6 167 -68]
                                            [-4  24 -41]])
                                   #t)
              (matrix [[ 6/7 -69/175 -58/175]
                       [ 3/7 158/175   6/175]
                       [-2/7   6/35  -33/35 ]]))

(check-equal? (matrix-gram-schmidt (matrix [[12 -51]
                                            [ 6 167]
                                            [-4  24]])
                                   #t)
              (matrix [[ 6/7  -69/175]
                       [ 3/7  158/175]
                       [-2/7    6/35 ]]))

(check-equal? (matrix-gram-schmidt (col-matrix [12 6 -4]) #t)
              (col-matrix [6/7 3/7 -2/7]))

(check-equal? (matrix-gram-schmidt (col-matrix [12 6 -4]) #f)
              (col-matrix [12 6 -4]))

;; ===================================================================================================
;; QR decomposition

(check-true (matrix-orthonormal? (matrix-q (index-array #(50 1)))))

(let-values ([(Q R)  (matrix-qr (matrix [[12 -51   4]
                                         [ 6 167 -68]
                                         [-4  24 -41]]))])
  (check-equal? Q (matrix [[ 6/7 -69/175 -58/175]
                           [ 3/7 158/175   6/175]
                           [-2/7   6/35  -33/35 ]]))
  (check-equal? R (matrix [[14  21 -14]
                           [ 0 175 -70]
                           [ 0   0  35]])))

;; A particularly tricky test case used to demonstrate loss of orthogonality
;; QR has to generate a better Q than Gram-Schmidt alone (which fails this test)
(check-true (matrix-orthonormal?
             (matrix-q (matrix [[0.70000 0.70711]
                                [0.70001 0.70711]]))))

;; Fuzz test the heck out of it: 100 matrices, random shape, random entries, sometimes rank-deficient
(for: ([i  (in-range 100)])
  (define m (+ 1 (random 10)))
  (define n (+ 1 (random 10)))
  (define M (random-matrix m n -3 4))
  ;; Full QR, real matrix
  (let-values ([(Q R)  (matrix-qr M #t)])
    (check-true (matrix-orthonormal? Q)
                (format "M = ~a  Q = ~a" M Q))
    (check-true (<= (matrix-relative-error (matrix* Q R) M)
                    (* 10 epsilon.0))))
  ;; Reduced QR, real matrix
  (let-values ([(Q R)  (matrix-qr M #f)])
    (check-true (matrix-cols-orthogonal? Q)
                (format "M = ~a  Q = ~a" M Q))
    (check-true (<= (matrix-relative-error (matrix* Q R) M)
                    (* 10 epsilon.0))))
  (define N (random-matrix m n -3 4))
  (define M+N (array-make-rectangular M N))
  ;; Full QR, complex matrix
  (let-values ([(Q R)  (matrix-qr M+N #t)])
    (check-true (matrix-orthonormal? Q)
                (format "M+N = ~a  Q = ~a" M+N Q))
    (check-true (<= (matrix-relative-error (matrix* Q R) M+N)
                    (* 10 epsilon.0))))
  ;; Reduced QR, complex matrix
  (let-values ([(Q R)  (matrix-qr M+N #f)])
    (check-true (matrix-cols-orthogonal? Q)
                (format "M+N = ~a  Q = ~a" M+N Q))
    (check-true (<= (matrix-relative-error (matrix* Q R) M+N)
                    (* 10 epsilon.0)))))

(for: ([M  (in-list nonmatrices)])
  (check-exn exn:fail:contract? (λ () (matrix-q M))))

#|
;; ===================================================================================================
;; Tests not yet converted to rackunit

(begin
  
  (begin
    "matrix-operations.rkt"
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
    (list 'projection-on-subspace
          (equal? (projection-on-subspace #(1 2 3) '(#(2 4 3)))
                  (matrix-scale (col-matrix [2 4 3]) 19/29)))
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
