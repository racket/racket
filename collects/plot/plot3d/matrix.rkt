#lang racket/base

;; A small rotation matrix library, used to transform plot coordinates into view coordinates.

(require racket/match
         "../common/math.rkt")

(provide m3-apply m3-transpose m3* m3-rotate-z m3-rotate-x m3-scale)

(define-syntax-rule (dot x1 y1 z1 x2 y2 z2) (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(define (m3-apply m v)
  (match-define (vector (vector v11 v12 v13) (vector v21 v22 v23) (vector v31 v32 v33)) m)
  (match-define (vector x y z) v)
  (vector (dot x y z v11 v12 v13) (dot x y z v21 v22 v23) (dot x y z v31 v32 v33)))

(define (m3-transpose m)
  (match-define (vector (vector m11 m12 m13)
                        (vector m21 m22 m23)
                        (vector m31 m32 m33)) m)
  (vector (vector m11 m21 m31)
          (vector m12 m22 m32)
          (vector m13 m23 m33)))

(define (m3* m1 m2)
  (match-define (vector v1 v2 v3) m1)
  (define m (m3-transpose m2))
  (vector (m3-apply m v1) (m3-apply m v2) (m3-apply m v3)))

(define (m3-rotate-z theta)
  (define cos-theta (cos theta))
  (define sin-theta (sin theta))
  (vector (vector cos-theta (- sin-theta) 0.0)
          (vector sin-theta cos-theta 0.0)
          (vector 0.0 0.0 1.0)))

(define (m3-rotate-x rho)
  (define cos-rho (cos rho))
  (define sin-rho (sin rho))
  (vector (vector 1.0 0.0 0.0)
          (vector 0.0 cos-rho (- sin-rho))
          (vector 0.0 sin-rho cos-rho)))

(define (m3-scale x-scale y-scale z-scale)
  (vector (vector x-scale 0.0 0.0)
          (vector 0.0 y-scale 0.0)
          (vector 0.0 0.0 z-scale)))
