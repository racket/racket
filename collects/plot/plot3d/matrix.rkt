#lang racket/base

;; A small rotation matrix library, used to transform plot coordinates into view coordinates.

(require racket/match
         "../common/vector.rkt"
         "../common/math.rkt")

(provide (all-defined-out))

(define (m3-apply m v)
  (match-define (vector v1 v2 v3) m)
  (vector (vdot v1 v) (vdot v2 v) (vdot v3 v)))

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

#|
(m3* #(#(1 2 3)
       #(4 5 6)
       #(7 8 9))
     #(#(10 20 30)
       #(40 50 60)
       #(70 80 90)))
#(#(300 360 420) #(660 810 960) #(1020 1260 1500))
|#

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
