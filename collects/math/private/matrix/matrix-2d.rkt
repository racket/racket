#lang typed/racket/base
(require math/matrix)

(provide matrix-2d-rotation
         matrix-2d-scaling
         matrix-2d-shear-x
         matrix-2d-shear-y
         matrix-2d-reflection
         matrix-2d-orthogonal-projection)

; Transformations from:
;     http://en.wikipedia.org/wiki/Transformation_matrix

(: matrix-2d-rotation : Real -> (Matrix Number))
; matrix representing rotation θ radians counter clockwise
(define (matrix-2d-rotation θ)
  (define cosθ (cos θ))
  (define sinθ (sin θ))
  (matrix/dim 2 2 
              cosθ (- sinθ)
              sinθ cosθ))

(: matrix-2d-scaling : Real Real -> (Matrix Number))
(define (matrix-2d-scaling sx sy)
  (matrix/dim 2 2 
              sx 0
              0 sy))

(: matrix-2d-shear-x : Real -> (Matrix Number))
(define (matrix-2d-shear-x k)
  (matrix/dim 2 2 
              1 k
              0 1))

(: matrix-2d-shear-y : Real -> (Matrix Number))
(define (matrix-2d-shear-y k)
  (matrix/dim 2 2 
              1 0
              k 1))

(: matrix-2d-reflection : Real Real -> (Matrix Number))
(define (matrix-2d-reflection a b)
  ; reflection about the line through (0,0) and (a,b)
  (define a2 (* a a))
  (define b2 (* b b))
  (define 2ab (* 2 a b))
  (define norm2 (+ a2 b2))
  (define 2ab/norm2 (/ 2ab norm2))
  (matrix/dim 2 2 
              (/ (- a2 b2) norm2) 2ab/norm2
              2ab/norm2           (/ (- b2 a2) norm2)))

(: matrix-2d-orthogonal-projection : Real Real -> (Matrix Number))
; orthogonal projection onto the line through (0,0) and (a,b)
(define (matrix-2d-orthogonal-projection a b)
  (define a2 (* a a))
  (define b2 (* b b))
  (define ab (* a b))
  (define norm2 (+ a2 b2))
  (define ab/norm2 (/ ab norm2))
  (matrix/dim 2 2 
              (/ a2 norm2) ab/norm2
              ab/norm2     (/ b2 norm2)))
