#lang racket/signature

;; CONVENTION: 
;; [Rectangle X] = [Listof [Listof X]] 
;;  where all elements of the non-empty list are of equal non-empty length

;; type: [Matrix X]
matrix? 
;; is this a matrix?

matrix-rows ;; [Matrix X] -> Nat 
;; how many rows does this matrix have? 

matrix-cols ;; [Matrix X] -> Nat 
;; how many columns does this matrix have? 

rectangle->matrix ;; [Rectangle X] -> [Matrix X]
;; create a matrix from a rectangle 

matrix->rectangle ;; [Matrix X] -> [Rectangle X]
;; create a rectangle from matrix

;; ----------------------------------------------------------------------------

make-matrix ;; Nat Nat [Listof X] -> [Matrix X]
;; create an initialized n x m matrix from list l
;; NOTE: make-matrix would consume an optional number of entries, 
;; if it were like make-vector  

build-matrix ;; Nat Nat (Nat Nat -> X) -> [Matrix X]
;; create a matrix from a function 

matrix-ref ;; [Matrix X] Nat Nat -> X
;; retrieve the content of the matrix at (i,j)

matrix-set ;; [Matrix X] Nat Nat X -> [Matrix X]
;; create a new matrix with x at (i,j) and all other places the same 

matrix-where? ;; [Matrix X] (X -> Boolean) -> [Listof Posn]
;; (matrix-where? M P) :: list of (make-posn i j) s.t. (P (matrix-ref M i j))

matrix-render ;; [Matrix X] -> [Rectangle String]
;; render the matrix as a rectangle of strings 

matrix-minor ;; Matrix Nat Nat -> Matrix 
;; create a matrix minor from M at (i,j)

;; matrix-set! ;; [Matrix X] Nat Nat X -> [Matrix X]
;; set the matrix at (i,j)
