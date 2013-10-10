#lang typed/racket/base

(require "matrix-types.rkt"
         "matrix-syntax.rkt")

(provide matrix-2d-rotation
         matrix-2d-scaling
         matrix-2d-shear-x
         matrix-2d-shear-y
         matrix-2d-reflection
         matrix-2d-orthogonal-projection)

; Transformations from:
;     http://en.wikipedia.org/wiki/Transformation_matrix

(: matrix-2d-rotation : Real -> (Matrix Real))
; matrix representing rotation θ radians counter clockwise
(define (matrix-2d-rotation θ)
  (define cosθ (cos θ))
  (define sinθ (sin θ))
  (matrix [[cosθ (- sinθ)]
           [sinθ cosθ]]))

(: matrix-2d-scaling : Real Real -> (Matrix Real))
(define (matrix-2d-scaling sx sy)
  (matrix [[sx 0]
           [0 sy]]))

(: matrix-2d-shear-x : Real -> (Matrix Real))
(define (matrix-2d-shear-x k)
  (matrix [[1 k]
           [0 1]]))

(: matrix-2d-shear-y : Real -> (Matrix Real))
(define (matrix-2d-shear-y k)
  (matrix [[1 0]
           [k 1]]))

(: matrix-2d-reflection : Real Real -> (Matrix Real))
(define (matrix-2d-reflection a b)
  ; reflection about the line through (0,0) and (a,b)
  (define a2 (* a a))
  (define b2 (* b b))
  (define 2ab (* 2 a b))
  (define norm2 (+ a2 b2))
  (define 2ab/norm2 (/ 2ab norm2))
  (matrix [[(/ (- a2 b2) norm2) 2ab/norm2]
           [2ab/norm2           (/ (- b2 a2) norm2)]]))

(: matrix-2d-orthogonal-projection : Real Real -> (Matrix Real))
; orthogonal projection onto the line through (0,0) and (a,b)
(define (matrix-2d-orthogonal-projection a b)
  (define a2 (* a a))
  (define b2 (* b b))
  (define ab (* a b))
  (define norm2 (+ a2 b2))
  (define ab/norm2 (/ ab norm2))
  (matrix [[(/ a2 norm2) ab/norm2]
           [ab/norm2     (/ b2 norm2)]]))
