#lang typed/racket

(require math/matrix
         math/array
         typed/rackunit)

(: matrix-double ((Matrix Real) -> (Matrix Real)))
(define (matrix-double M) (matrix-scale M 2))

(define nonstrict-2x2-arr
  (parameterize ([array-strictness #f])
    (build-matrix 2 2 (λ: ([i : Index] [j : Index]) (if (= i j) 1 0)))))

(define strict-2x2-arr
  (parameterize ([array-strictness #t])
    (build-matrix 2 2 (λ: ([i : Index] [j : Index]) (if (= i j) 1 0)))))

(check-false (array-strict? nonstrict-2x2-arr))
(check-true (array-strict? strict-2x2-arr))

(define (check-always)
  (printf "(array-strictness) = ~v~n" (array-strictness))
  (check-true (array-strict? (matrix [[1 2] [3 4]])))
  (check-true (array-strict? (row-matrix [1 2 3 4])))
  (check-true (array-strict? (col-matrix [1 2 3 4])))
  (check-true (array-strict? (make-matrix 4 4 0)))
  (check-true (array-strict? (identity-matrix 6)))
  (check-true (array-strict? (diagonal-matrix '(1 2 3 4))))
  (check-true (array-strict? (list->matrix 2 2 '(1 2 3 4))))
  (check-true (array-strict? (vector->matrix 2 2 #(1 2 3 4))))
  (check-true (array-strict? (list*->matrix '((1 2) (3 4)))))
  (check-true (array-strict? ((inst vector*->matrix Integer) #(#(1 2) #(3 4)))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (matrix-row-echelon M)))
    (let-values ([(L U)  (matrix-lu M)])
      (check-true (array-strict? L))
      (check-true (array-strict? U))))
  )

(parameterize ([array-strictness #t])
  (check-always)
  
  (check-true (array-strict? (block-diagonal-matrix (list nonstrict-2x2-arr strict-2x2-arr))))
  (check-true (array-strict? (vandermonde-matrix '(1 2 3 4) 10)))
  
  (check-true (array-strict? (->col-matrix '(1 2 3 4))))
  (check-true (array-strict? (->col-matrix #(1 2 3 4))))
  (check-true (array-strict? (->col-matrix (array #[1 2 3 4]))))
  (check-true (array-strict? (->col-matrix (array #[#[1 2 3 4]]))))
  (check-true (array-strict? (->col-matrix (array #[#[1] #[2] #[3] #[4]]))))
  
  (check-true (array-strict? (->row-matrix '(1 2 3 4))))
  (check-true (array-strict? (->row-matrix #(1 2 3 4))))
  (check-true (array-strict? (->row-matrix (array #[1 2 3 4]))))
  (check-true (array-strict? (->row-matrix (array #[#[1 2 3 4]]))))
  (check-true (array-strict? (->row-matrix (array #[#[1] #[2] #[3] #[4]]))))
  
  (for*: ([M1  (list nonstrict-2x2-arr strict-2x2-arr)]
          [M2  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (matrix* M1 M2)))
    (check-true (array-strict? (matrix+ M1 M2)))
    (check-true (array-strict? (matrix- M1 M2)))
    (check-true (array-strict? (matrix-map * M1 M2)))
    (check-true (array-strict? (matrix-sum (list M1 M2))))
    (check-true (array-strict? (matrix-augment (list M1 M2))))
    (check-true (array-strict? (matrix-stack (list M1 M2))))
    (check-true (array-strict? (matrix-solve M1 M2))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (matrix-scale M -1)))
    (check-true (array-strict? (matrix-expt M 0)))
    (check-true (equal? (array-strict? (matrix-expt M 1)) (array-strict? M)))
    (check-true (array-strict? (matrix-expt M 2)))
    (check-true (array-strict? (matrix-expt M 3)))
    (check-true (array-strict? (matrix-diagonal M)))
    (check-true (andmap (λ: ([M : (Matrix Real)]) (array-strict? M)) (matrix-rows M)))
    (check-true (andmap (λ: ([M : (Matrix Real)]) (array-strict? M)) (matrix-cols M)))
    (check-true (array-strict? (matrix-map-rows matrix-double M)))
    (check-true (array-strict? (matrix-map-cols matrix-double M)))
    (check-true (array-strict? (matrix-conjugate M)))
    (check-true (array-strict? (matrix-transpose M)))
    (check-true (array-strict? (matrix-hermitian M)))
    (check-true (array-strict? (matrix-normalize M)))
    (check-true (array-strict? (matrix-normalize-rows M)))
    (check-true (array-strict? (matrix-normalize-cols M)))
    (check-true (array-strict? (matrix-inverse M)))
    (check-true (array-strict? (matrix-gram-schmidt M)))
    (let-values ([(Q R)  (matrix-qr M)])
      (check-true (array-strict? Q))
      (check-true (array-strict? R))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)]
          [i  (list 0 1)])
    (check-true (array-strict? (matrix-row M i)))
    (check-true (array-strict? (matrix-col M i))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)]
          [spec  (list '(0) (:: #f #f 2))])
    (check-true (array-strict? (submatrix M (::) spec))))
  )

(parameterize ([array-strictness #f])
  (check-always)
  
  (check-false (array-strict? (block-diagonal-matrix (list nonstrict-2x2-arr strict-2x2-arr))))
  (check-false (array-strict? (vandermonde-matrix '(1 2 3 4) 10)))
  
  (check-true (array-strict? (->col-matrix '(1 2 3 4))))
  (check-true (array-strict? (->col-matrix #(1 2 3 4))))
  (check-false (array-strict? (->col-matrix (array #[1 2 3 4]))))
  (check-false (array-strict? (->col-matrix (array #[#[1 2 3 4]]))))
  (check-true (array-strict? (->col-matrix (array #[#[1] #[2] #[3] #[4]]))))
  
  (check-false (array-strict? (->row-matrix '(1 2 3 4))))
  (check-false (array-strict? (->row-matrix #(1 2 3 4))))
  (check-false (array-strict? (->row-matrix (array #[1 2 3 4]))))
  (check-true (array-strict? (->row-matrix (array #[#[1 2 3 4]]))))
  (check-false (array-strict? (->row-matrix (array #[#[1] #[2] #[3] #[4]]))))
  
  (for*: ([M1  (list nonstrict-2x2-arr strict-2x2-arr)]
          [M2  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (matrix* M1 M2)))
    (check-false (array-strict? (matrix+ M1 M2)))
    (check-false (array-strict? (matrix- M1 M2)))
    (check-false (array-strict? (matrix-map * M1 M2)))
    (check-false (array-strict? (matrix-sum (list M1 M2))))
    (check-false (array-strict? (matrix-augment (list M1 M2))))
    (check-false (array-strict? (matrix-stack (list M1 M2))))
    (check-false (array-strict? (matrix-solve M1 M2))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (matrix-scale M -1)))
    (check-false (array-strict? (matrix-expt M 0)))
    (check-false (array-strict? (matrix-expt (array-lazy M) 1)))
    (check-false (array-strict? (matrix-expt M 2)))
    (check-false (array-strict? (matrix-expt M 3)))
    (check-false (array-strict? (matrix-diagonal M)))
    (check-false (ormap (λ: ([M : (Matrix Real)]) (array-strict? M)) (matrix-rows M)))
    (check-false (ormap (λ: ([M : (Matrix Real)]) (array-strict? M)) (matrix-cols M)))
    (check-false (array-strict? (matrix-map-rows matrix-double M)))
    (check-false (array-strict? (matrix-map-cols matrix-double M)))
    (check-false (array-strict? (matrix-conjugate M)))
    (check-false (array-strict? (matrix-transpose M)))
    (check-false (array-strict? (matrix-hermitian M)))
    (check-false (array-strict? (matrix-normalize M)))
    (check-false (array-strict? (matrix-normalize-rows M)))
    (check-false (array-strict? (matrix-normalize-cols M)))
    (check-false (array-strict? (matrix-inverse M)))
    (check-false (array-strict? (matrix-gram-schmidt M)))
    (let-values ([(Q R)  (matrix-qr M)])
      (check-false (array-strict? Q))
      (check-false (array-strict? R))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)]
          [spec  (list '(0) (:: #f #f 2))])
    (check-false (array-strict? (submatrix M (::) spec))))
  
  (for*: ([M  (list nonstrict-2x2-arr strict-2x2-arr)]
          [i  (list 0 1)])
    (check-false (array-strict? (matrix-row M i)))
    (check-false (array-strict? (matrix-col M i))))
  )
