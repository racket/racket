#lang racket/base

(require racket/match
         racket/list
         (only-in math/flonum fl flvector+ flvector-)
         (only-in racket/unsafe/ops unsafe-vector-ref)
         racket/flonum)

(provide m3-apply m3-transpose m3* m3-rotate-z m3-rotate-x
         flv3-dot
         flv3-normal
         flv3-center
         exact-vector3d
         exact-vector3d-sublists
         exact-polygon3d)

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

(define (flv3-dot v1 v2)
  (fl+ (fl* (flvector-ref v1 0)
            (flvector-ref v2 0))
       (fl+ (fl* (flvector-ref v1 1)
                 (flvector-ref v2 1))
            (fl* (flvector-ref v1 2)
                 (flvector-ref v2 2)))))

(define default-normal (flvector 0.0 -1.0 0.0))

(define (flv3-normal vs)
  (define n (length vs))
  (cond
    [(n . < . 3)  default-normal]
    [else
     (match-define (list v1 v2) (take-right vs 2))
     (define x1 (flvector-ref v1 0))
     (define y1 (flvector-ref v1 1))
     (define z1 (flvector-ref v1 2))
     (define x2 (flvector-ref v2 0))
     (define y2 (flvector-ref v2 1))
     (define z2 (flvector-ref v2 2))
     (define-values (x y z _x1 _y1 _z1 _x2 _y2 _z2)
       (for/fold ([x 0.0] [y 0.0] [z 0.0] [x1 x1] [y1 y1] [z1 z1] [x2 x2] [y2 y2] [z2 z2]
                          ) ([v3  (in-list vs)])
         (define x3 (flvector-ref v3 0))
         (define y3 (flvector-ref v3 1))
         (define z3 (flvector-ref v3 2))
         (define x32 (fl- x3 x2))
         (define y32 (fl- y3 y2))
         (define z32 (fl- z3 z2))
         (define x12 (fl- x1 x2))
         (define y12 (fl- y1 y2))
         (define z12 (fl- z1 z2))
         (values (+ x (fl- (fl* y32 z12) (fl* z32 y12)))
                 (+ y (fl- (fl* z32 x12) (fl* x32 z12)))
                 (+ z (fl- (fl* x32 y12) (fl* y32 x12)))
                 x2 y2 z2
                 x3 y3 z3)))
     (define m (flsqrt (fl+ (fl* x x) (fl+ (fl* y y) (fl* z z)))))
     (if (fl> m 0.0)
         (flvector (fl/ x m) (fl/ y m) (fl/ z m))
         default-normal)]))

(define (flv3-center vs)
  (define xs (map (λ (v) (flvector-ref v 0)) vs))
  (define ys (map (λ (v) (flvector-ref v 1)) vs))
  (define zs (map (λ (v) (flvector-ref v 2)) vs))
  (flvector (* 0.5 (+ (apply min xs) (apply max xs)))
            (* 0.5 (+ (apply min ys) (apply max ys)))
            (* 0.5 (+ (apply min zs) (apply max zs)))))

(define (exact-vector3d v)
  (define n (vector-length v))
  (cond [(= n 3)
         (define v1 (unsafe-vector-ref v 0))
         (define v2 (unsafe-vector-ref v 1))
         (define v3 (unsafe-vector-ref v 2))
         (cond [(and (exact? v1) (exact? v2) (exact? v3))  v]
               [(and (rational? v1) (rational? v2) (rational? v3))
                (vector (inexact->exact v1) (inexact->exact v2) (inexact->exact v3))]
               [else  #f])]
        [else  #f]))

(define (sublists vs)
  (define vss
    (for/fold ([vss  (list null)]) ([v  (in-list vs)])
      (cond [v  (cons (cons v (car vss)) (cdr vss))]
            [(null? (car vss))  vss]
            [else  (cons null vss)])))
  (cond [(null? (car vss))  (cdr vss)]
        [else  vss]))

(define (exact-vector3d-sublists vs)
  (sublists (map exact-vector3d vs)))

(define (exact-polygon3d vs ls)
  (cond
    [(null? vs)  (values null null)]
    [else
     (define-values (new-vs new-ls _)
       (for/fold ([vs null] [ls null] [v1 (last vs)]) ([v2  (in-list vs)]
                                                       [l   (in-list ls)])
         (cond [(equal? v1 v2)  (values vs ls v2)]
               [else
                (define exact-v2 (exact-vector3d v2))
                (if exact-v2
                    (values (cons exact-v2 vs) (cons l ls) v2)
                    (values vs ls v2))])))
     (values (reverse new-vs) (reverse new-ls))]))
