#lang racket/base

;; A small rotation matrix library, used to transform plot coordinates into view coordinates.

(require racket/match
         (only-in math/flonum fl)
         racket/flonum)

(provide m3-apply m3-transpose m3* m3-rotate-z m3-rotate-x m3-scale
         fl3-dot)

(define-syntax-rule (dot x1 y1 z1 x2 y2 z2) (fl+ (fl+ (fl* x1 x2) (fl* y1 y2)) (fl* z1 z2)))

(define (m3-apply m v)
  (match-define (vector v1 v2 v3) m)
  (define x (flvector-ref v 0))
  (define y (flvector-ref v 1))
  (define z (flvector-ref v 2))
  (flvector (dot x y z (flvector-ref v1 0) (flvector-ref v1 1) (flvector-ref v1 2))
            (dot x y z (flvector-ref v2 0) (flvector-ref v2 1) (flvector-ref v2 2))
            (dot x y z (flvector-ref v3 0) (flvector-ref v3 1) (flvector-ref v3 2))))

(define (m3-transpose m)
  (match-define (vector v1 v2 v3) m)
  (vector (flvector (flvector-ref v1 0) (flvector-ref v2 0) (flvector-ref v3 0))
          (flvector (flvector-ref v1 1) (flvector-ref v2 1) (flvector-ref v3 1))
          (flvector (flvector-ref v1 2) (flvector-ref v2 2) (flvector-ref v3 2))))

(define (m3* m1 m2)
  (match-define (vector v1 v2 v3) m1)
  (define m (m3-transpose m2))
  (vector (m3-apply m v1) (m3-apply m v2) (m3-apply m v3)))

(define (m3-rotate-z theta)
  (let ([theta  (fl theta)])
    (define cos-theta (flcos theta))
    (define sin-theta (flsin theta))
    (vector (flvector cos-theta (- sin-theta) 0.0)
            (flvector sin-theta cos-theta 0.0)
            (flvector 0.0 0.0 1.0))))

(define (m3-rotate-x rho)
  (let ([rho  (fl rho)])
    (define cos-rho (flcos rho))
    (define sin-rho (flsin rho))
    (vector (flvector 1.0 0.0 0.0)
            (flvector 0.0 cos-rho (- sin-rho))
            (flvector 0.0 sin-rho cos-rho))))

(define (m3-scale x-scale y-scale z-scale)
  (vector (flvector (fl x-scale) 0.0 0.0)
          (flvector 0.0 (fl y-scale) 0.0)
          (flvector 0.0 0.0 (fl z-scale))))

(define (fl3-dot v1 v2)
  (fl+ (fl* (flvector-ref v1 0)
            (flvector-ref v2 0))
       (fl+ (fl* (flvector-ref v1 1)
                 (flvector-ref v2 1))
            (fl* (flvector-ref v1 2)
                 (flvector-ref v2 2)))))
