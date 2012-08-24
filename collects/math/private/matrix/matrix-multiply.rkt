#lang typed/racket

(require "../unsafe.rkt"
         "../../array.rkt"
         "matrix-types.rkt")

(provide matrix*)

;; The `make-matrix-*' operators have to be macros; see ../array/array-pointwise.rkt for an
;; explanation.

#;(: make-matrix-multiply (All (A) (Symbol
                                    ((Array A) Integer -> (Array A))
                                    ((Array A) (Array A) -> (Array A))
                                    -> ((Array A) (Array A) -> (Array A)))))
(define-syntax-rule (make-matrix-multiply name array-axis-sum array*)
  (λ (arr brr)
    (unless (array-matrix? arr) (raise-type-error name "matrix" 0 arr brr))
    (unless (array-matrix? brr) (raise-type-error name "matrix" 1 arr brr))
    (match-define (vector ad0 ad1) (array-shape arr))
    (match-define (vector bd0 bd1) (array-shape brr))
    (unless (= ad1 bd0)
      (error name
             "1st argument column size and 2nd argument row size are not equal; given ~e and ~e"
             arr brr))
    ;; Get strict versions of both because each element in both is evaluated multiple times
    (let ([arr  (array->mutable-array arr)]
          [brr  (array->mutable-array brr)])
      ;; This next part could be done with array-permute, but it's much slower that way
      (define avs (mutable-array-data arr))
      (define bvs (mutable-array-data brr))
      ;; Extend arr in the center dimension
      (define: ds-ext : (Vectorof Index) (vector ad0 bd1 ad1))
      (define arr-ext
        (unsafe-build-array 
         ds-ext (λ: ([js : (Vectorof Index)])
                  (define j0 (unsafe-vector-ref js 0))
                  (define j1 (unsafe-vector-ref js 2))
                  ;(unsafe-array-ref* arr j0 j1)  [twice as slow]
                  (unsafe-vector-ref avs (unsafe-fx+ j1 (unsafe-fx* j0 ad1))))))
      ;; Transpose brr and extend in the leftmost dimension
      ;; Note that ds-ext = (vector ad0 bd1 bd0) because bd0 = ad1
      (define brr-ext
        (unsafe-build-array 
         ds-ext (λ: ([js : (Vectorof Index)])
                  (define j0 (unsafe-vector-ref js 2))
                  (define j1 (unsafe-vector-ref js 1))
                  ;(unsafe-array-ref* brr j0 j1)  [twice as slow]
                  (unsafe-vector-ref bvs (unsafe-fx+ j1 (unsafe-fx* j0 bd1))))))
      (array-axis-sum (array* arr-ext brr-ext) 2))))

;; ---------------------------------------------------------------------------------------------------

(: matrix* (case-> ((Matrix Real)   (Matrix Real)   -> (Matrix Real))
                   ((Matrix Number) (Matrix Number) -> (Matrix Number))))
(define matrix* (make-matrix-multiply 'matrix* array-axis-sum array*))

;(: matrix-fl* ((Array Float) (Array Float) -> (Array Float)))
;(define matrix-fl* (make-matrix-multiply 'matrix-fl* array-axis-flsum array-fl*))

