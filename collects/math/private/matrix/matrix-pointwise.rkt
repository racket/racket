#lang typed/racket

(require racket/unsafe/ops
         "../../array.rkt"
         "utils.rkt")

(provide matrix+ matrix-fl+
         matrix- matrix-fl-)

;; The `make-matrix-*' operators have to be macros; see ../array/array-pointwise.rkt for an
;; explanation.

#;(: make-matrix-pointwise1 (All (A) (Symbol
                                      ((Array A) -> (lazy-array A))
                                      -> ((Array A) -> (lazy-array A)))))
(define-syntax-rule (make-matrix-pointwise1 name array-op1)
  (λ (arr)
    (unless (array-matrix? arr) (raise-type-error name "matrix" arr))
    (array-op1 arr)))

#;(: make-matrix-pointwise2 (All (A) (Symbol
                                      ((Array A) (Array A) -> (lazy-array A))
                                      -> ((Array A) (Array A) -> (lazy-array A)))))
(define-syntax-rule (make-matrix-pointwise2 name array-op2)
  (λ (arr brr)
    (unless (array-matrix? arr) (raise-type-error name "matrix" 0 arr brr))
    (unless (array-matrix? brr) (raise-type-error name "matrix" 1 arr brr))
    (array-op2 arr brr)))

#;(: make-matrix-pointwise1/2 (All (A) (Symbol
                                        (case-> ((Array A) -> (lazy-array A))
                                                ((Array A) (Array A) -> (lazy-array A)))
                                        -> (case-> ((Array A) -> (lazy-array A))
                                                   ((Array A) (Array A) -> (lazy-array A))))))
(define-syntax-rule (make-matrix-pointwise1/2 name array-op1/2)
  (case-lambda
    [(arr)      ((make-matrix-pointwise1 name array-op1/2) arr)]
    [(arr brr)  ((make-matrix-pointwise2 name array-op1/2) arr brr)]))

;; ---------------------------------------------------------------------------------------------------

(: matrix+ (case-> ((Array Real)   (Array Real)   -> (lazy-array Real))
                   ((Array Number) (Array Number) -> (lazy-array Number))))
(: matrix- (case-> ((Array Real)   -> (lazy-array Real))
                   ((Array Number) -> (lazy-array Number))
                   ((Array Real)   (Array Real)   -> (lazy-array Real))
                   ((Array Number) (Array Number) -> (lazy-array Number))))

(define matrix+ (make-matrix-pointwise2 'matrix+ array+))
(define matrix- (make-matrix-pointwise1/2 'matrix- array-))

(: matrix-fl+ ((Array Float) (Array Float) -> (lazy-array Float)))
(: matrix-fl- (case-> ((Array Float) -> (lazy-array Float))
                      ((Array Float) (Array Float) -> (lazy-array Float))))

(define matrix-fl+ (make-matrix-pointwise2 'matrix-fl+ array-fl+))
(define matrix-fl- (make-matrix-pointwise1/2 'matrix-fl- array-fl-))

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
    (define-values (A B A+B) (values '[[1 2] [3 4]] '[[5 6] [7 8]] '[[6 8] [10 12]]))
    (define-values (A.0 B.0 A+B.0) (values (rows->flrows A) (rows->flrows B) (rows->flrows A+B)))
    (check-matrix= (matrix+ (matrix A) (matrix B)) (matrix A+B))
    (check-matrix= (matrix-fl+ (flmatrix A.0) (flmatrix B.0)) (flmatrix A+B.0)))
  (let ()
    (define-values (A B A-B) (values '[[1 2] [3 4]] '[[5 6] [7 8]] '[[-4 -4] [-4 -4]]))
    (define-values (A.0 B.0 A-B.0) (values (rows->flrows A) (rows->flrows B) (rows->flrows A-B)))
    (check-matrix= (matrix- (matrix A) (matrix B)) (matrix A-B))
    (check-matrix= (matrix-fl- (flmatrix A.0) (flmatrix B.0)) (flmatrix A-B.0)))
  (let ()
    (define-values (A ~A) (values '[[1 2 3] [4 5 6]] '[[-1 -2 -3] [-4 -5 -6]]))
    (define-values (A.0 ~A.0) (values (rows->flrows A) (rows->flrows ~A)))
    (check-matrix= (matrix- (matrix A)) (matrix ~A))
    (check-matrix= (matrix- (matrix A.0)) (matrix ~A.0))))