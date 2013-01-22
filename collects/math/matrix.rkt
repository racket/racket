#lang racket/base

(require typed/untyped-utils)

(require "private/matrix/matrix-arithmetic.rkt"
         "private/matrix/matrix-conversion.rkt"
         "private/matrix/matrix-syntax.rkt"
         "private/matrix/matrix-comprehension.rkt"
         "private/matrix/matrix-types.rkt"
         "private/matrix/matrix-2d.rkt"
         ;;"private/matrix/matrix-expt.rkt"  ; all use require/untyped-contract
         ;;"private/matrix/matrix-gauss-elim.rkt"  ; all use require/untyped-contract
         (except-in "private/matrix/matrix-solve.rkt"
                    matrix-determinant
                    matrix-inverse
                    matrix-solve)
         (except-in "private/matrix/matrix-constructors.rkt"
                    vandermonde-matrix)
         (except-in "private/matrix/matrix-basic.rkt"
                    matrix-1norm
                    matrix-2norm
                    matrix-inf-norm
                    matrix-norm
                    matrix-dot
                    matrix-cos-angle
                    matrix-angle
                    matrix-normalize
                    matrix-conjugate
                    matrix-hermitian
                    matrix-trace
                    matrix-normalize-rows
                    matrix-normalize-cols)
         (except-in "private/matrix/matrix-subspace.rkt"
                    matrix-col-space)
         (except-in "private/matrix/matrix-operator-norm.rkt"
                    matrix-op-1norm
                    matrix-op-2norm
                    matrix-op-inf-norm
                    matrix-basis-cos-angle
                    matrix-basis-angle)
         ;;"private/matrix/matrix-qr.rkt"  ; all use require/untyped-contract
         ;;"private/matrix/matrix-lu.rkt"  ; all use require/untyped-contract
         ;;"private/matrix/matrix-gram-schmidt.rkt"  ; all use require/untyped-contract
         )

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-expt.rkt"
 [matrix-expt  ((Matrix Number) Integer -> (Matrix Number))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"
                 "private/matrix/matrix-gauss-elim.rkt"))
 "private/matrix/matrix-gauss-elim.rkt"
 [matrix-gauss-elim
  (case-> ((Matrix Number) -> (Values (Matrix Number) (Listof Index)))
          ((Matrix Number) Any -> (Values (Matrix Number) (Listof Index)))
          ((Matrix Number) Any Any -> (Values (Matrix Number) (Listof Index)))
          ((Matrix Number) Any Any Pivoting -> (Values (Matrix Number) (Listof Index))))]
 [matrix-row-echelon
  (case-> ((Matrix Number) -> (Matrix Number))
          ((Matrix Number) Any -> (Matrix Number))
          ((Matrix Number) Any Any -> (Matrix Number))
          ((Matrix Number) Any Any Pivoting -> (Matrix Number)))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-solve.rkt"
 [matrix-determinant
  ((Matrix Number) -> Number)]
 [matrix-inverse
  (All (A) (case-> ((Matrix Number)        -> (Matrix Number))
                   ((Matrix Number) (-> A) -> (U A (Matrix Number)))))]
 [matrix-solve
  (All (A) (case->
            ((Matrix Number) (Matrix Number)        -> (Matrix Number))
            ((Matrix Number) (Matrix Number) (-> A) -> (U A (Matrix Number)))))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-constructors.rkt"
 [vandermonde-matrix  ((Listof Number) Integer -> (Matrix Number))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-basic.rkt"
 [matrix-1norm  ((Matrix Number) -> Nonnegative-Real)]
 [matrix-2norm  ((Matrix Number) -> Nonnegative-Real)]
 [matrix-inf-norm  ((Matrix Number) -> Nonnegative-Real)]
 [matrix-norm  (case-> ((Matrix Number) -> Nonnegative-Real)
                       ((Matrix Number) Real -> Nonnegative-Real))]
 [matrix-dot
  (case-> ((Matrix Number) -> Nonnegative-Real)
          ((Matrix Number) (Matrix Number) -> Number))]
 [matrix-cos-angle
  ((Matrix Number) (Matrix Number) -> Number)]
 [matrix-angle
  ((Matrix Number) (Matrix Number) -> Number)]
 [matrix-normalize
  (All (A) (case-> ((Matrix Number)             -> (Matrix Number))
                   ((Matrix Number) Real        -> (Matrix Number))
                   ((Matrix Number) Real (-> A) -> (U A (Matrix Number)))))]
 [matrix-conjugate
  ((Matrix Number) -> (Matrix Number))]
 [matrix-hermitian
  ((Matrix Number) -> (Matrix Number))]
 [matrix-trace
  ((Matrix Number) -> Number)]
 [matrix-normalize-rows
  (All (A) (case-> ((Matrix Number)             -> (Matrix Number))
                   ((Matrix Number) Real        -> (Matrix Number))
                   ((Matrix Number) Real (-> A) -> (U A (Matrix Number)))))]
 [matrix-normalize-cols
  (All (A) (case-> ((Matrix Number)             -> (Matrix Number))
                   ((Matrix Number) Real        -> (Matrix Number))
                   ((Matrix Number) Real (-> A) -> (U A (Matrix Number)))))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-subspace.rkt"
 [matrix-col-space
  (All (A) (case-> ((Matrix Number)        -> (Matrix Number))
                   ((Matrix Number) (-> A) -> (U A (Matrix Number)))))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-operator-norm.rkt"
 [matrix-op-1norm  ((Matrix Number) -> Nonnegative-Real)]
 [matrix-op-2norm  ((Matrix Number) -> Nonnegative-Real)]
 [matrix-op-inf-norm  ((Matrix Number) -> Nonnegative-Real)]
 [matrix-basis-cos-angle
  ((Matrix Number) (Matrix Number) -> Number)]
 [matrix-basis-angle
  ((Matrix Number) (Matrix Number) -> Number)])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-qr.rkt"
 [matrix-qr
  (case-> ((Matrix Number)     -> (Values (Matrix Number) (Matrix Number)))
          ((Matrix Number) Any -> (Values (Matrix Number) (Matrix Number))))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"))
 "private/matrix/matrix-lu.rkt"
 [matrix-lu
  (All (A) (case-> ((Matrix Number)        -> (Values (Matrix Number) (Matrix Number)))
                   ((Matrix Number) (-> A) -> (Values (U A (Matrix Number)) (Matrix Number)))))])

(require/untyped-contract
 (begin (require "private/matrix/matrix-types.rkt"
                 "private/array/array-struct.rkt"))
 "private/matrix/matrix-gram-schmidt.rkt"
 [matrix-gram-schmidt
  (case-> ((Matrix Number)             -> (Array Number))
          ((Matrix Number) Any         -> (Array Number))
          ((Matrix Number) Any Integer -> (Array Number)))]
 [matrix-basis-extension
  ((Matrix Number) -> (Array Number))])

(provide (all-from-out
          "private/matrix/matrix-arithmetic.rkt"
          "private/matrix/matrix-constructors.rkt"
          "private/matrix/matrix-conversion.rkt"
          "private/matrix/matrix-syntax.rkt"
          "private/matrix/matrix-basic.rkt"
          "private/matrix/matrix-subspace.rkt"
          "private/matrix/matrix-solve.rkt"
          "private/matrix/matrix-operator-norm.rkt"
          "private/matrix/matrix-comprehension.rkt"
          "private/matrix/matrix-types.rkt"
          "private/matrix/matrix-2d.rkt")
         ;; matrix/matrix-expt.rkt
         matrix-expt
         ;; matrix-gauss-elim.rkt
         matrix-gauss-elim
         matrix-row-echelon
         ;; matrix-solve.rkt
         matrix-determinant
         matrix-inverse
         matrix-solve
         ;; matrix-constructors.rkt
         vandermonde-matrix
         ;; matrix-basic.rkt
         matrix-1norm
         matrix-2norm
         matrix-inf-norm
         matrix-norm
         matrix-dot
         matrix-cos-angle
         matrix-angle
         matrix-normalize
         matrix-conjugate
         matrix-hermitian
         matrix-trace
         matrix-normalize-rows
         matrix-normalize-cols
         ;; matrix-subspace.rkt
         matrix-col-space
         ;; matrix-operator-norm.rkt
         matrix-op-1norm
         matrix-op-2norm
         matrix-op-inf-norm
         matrix-basis-cos-angle
         matrix-basis-angle
         ;; matrix-qr.rkt
         matrix-qr
         ;; matrix-lu.rkt
         matrix-lu
         ;; matrix-gram-schmidt.rkt
         matrix-gram-schmidt
         matrix-basis-extension
         )
