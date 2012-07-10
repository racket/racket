#lang typed/racket

(require racket/unsafe/ops
         "../../array.rkt"
         "utils.rkt")

(provide matrix* matrix-fl*)

;; The `make-matrix-*' operators have to be macros; see ../array/array-pointwise.rkt for an
;; explanation.

#;(: make-matrix-multiply (All (A) (Symbol
                                    ((Array A) Integer -> (lazy-array A))
                                    ((Array A) (Array A) -> (lazy-array A))
                                    -> ((Array A) (Array A) -> (lazy-array A)))))
(define-syntax-rule (make-matrix-multiply name array-axis-sum array*)
  (λ (arr brr)
    (unless (array-matrix? arr) (raise-type-error name "matrix" 0 arr brr))
    (unless (array-matrix? brr) (raise-type-error name "matrix" 1 arr brr))
    (match-define (vector ad0 ad1) (unsafe-array-shape arr))
    (match-define (vector bd0 bd1) (unsafe-array-shape brr))
    (unless (= ad1 bd0)
      (error name
             "1st argument column size and 2nd argument row size are not equal; given ~e and ~e"
             arr brr))
    ;; Get strict versions of both because each element in both is evaluated multiple times
    (let ([arr  (array-strict arr)]
          [brr  (array-strict brr)])
      ;; This next part could be done with array-permute, but it's much slower that way
      (define avs (unsafe-array-data arr))
      (define bvs (unsafe-array-data brr))
      ;; Extend arr in the center dimension
      (define: ds-ext : (Vectorof Index) (vector ad0 bd1 ad1))
      (define arr-ext
        (unsafe-lazy-array
         ds-ext (λ: ([js : (Vectorof Index)])
                  (define j0 (unsafe-vector-ref js 0))
                  (define j1 (unsafe-vector-ref js 2))
                  ;(unsafe-array-ref* arr j0 j1)  [twice as slow]
                  (unsafe-vector-ref avs (unsafe-fx+ j1 (unsafe-fx* j0 ad1))))))
      ;; Transpose brr and extend in the leftmost dimension
      ;; Note that ds-ext = (vector ad0 bd1 bd0) because bd0 = ad1
      (define brr-ext
        (unsafe-lazy-array
         ds-ext (λ: ([js : (Vectorof Index)])
                  (define j0 (unsafe-vector-ref js 2))
                  (define j1 (unsafe-vector-ref js 1))
                  ;(unsafe-array-ref* brr j0 j1)  [twice as slow]
                  (unsafe-vector-ref bvs (unsafe-fx+ j1 (unsafe-fx* j0 bd1))))))
      (array-axis-sum (array* arr-ext brr-ext) 2))))

;; ---------------------------------------------------------------------------------------------------

(: matrix* (case-> ((Array Real)   (Array Real)   -> (lazy-array Real))
                   ((Array Number) (Array Number) -> (lazy-array Number))))
(define matrix* (make-matrix-multiply 'matrix* array-axis-sum array*))

(: matrix-fl* ((Array Float) (Array Float) -> (lazy-array Float)))
(define matrix-fl* (make-matrix-multiply 'matrix-fl* array-axis-flsum array-fl*))

(module* test typed/racket
  (require typed/rackunit 
           (submod "..")
           "../../array.rkt")
  
  (define-syntax (check-matrix= stx)
    ; Note: Does define-simple-check work with Typed Racket?
    (syntax-case stx ()
      [(_ a b) 
       (syntax/loc stx (check-equal? (array->list a) (array->list b)))]))
  
  (: matrix : (Rec T (U Real (Listof T))) -> (Array Real))
  (define (matrix rows) (list->array real? rows))
  
  (: flmatrix : (Rec T (U Float (Listof T))) -> (Array Float))
  (define (flmatrix rows) (list->array flonum? rows))
  
  (: rows->flrows : (Listof (Listof Real)) -> (Listof (Listof Float)))
  (define (rows->flrows rows)
    (: row->flrow : (Listof Real) -> (Listof Float))
    (define (row->flrow row) (map real->double-flonum row))
    (map row->flrow rows))
  
  (let ()
    (define-values (A B AB) (values '[[1 2] [3 4]] '[[5 6] [7 8]] '[[19 22] [43 50]]))
    (define-values (A.0 B.0 AB.0) (values (rows->flrows A) (rows->flrows B) (rows->flrows AB)))
    (check-matrix= (matrix* (matrix A) (matrix B)) (matrix AB))
    (check-matrix= (matrix-fl* (flmatrix A.0) (flmatrix B.0)) (flmatrix AB.0)))
  (let () 
    (define-values (A B AB) (values '[[1 2] [3 4]] '[[5 6 7] [8 9 10]] '[[21 24 27] [47 54 61]]))
    (define-values (A.0 B.0 AB.0) (values (rows->flrows A) (rows->flrows B) (rows->flrows AB)))
    (check-matrix= (matrix* (matrix A) (matrix B)) (matrix AB))
    (check-matrix= (matrix-fl* (flmatrix A.0) (flmatrix B.0)) (flmatrix AB.0))))