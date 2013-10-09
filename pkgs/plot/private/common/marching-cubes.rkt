#lang racket/base

(require racket/contract racket/flonum racket/fixnum racket/list racket/match racket/unsafe/ops
         unstable/latent-contract/defthing
         (for-syntax racket/base racket/syntax racket/match racket/list)
         "math.rkt"
         "utils.rkt"
         "marching-utils.rkt")

(provide heights->cube-polys heights->cube-polys:doc)

(define-for-syntax (->datum x)
  (if (syntax? x) (syntax->datum x) x))

(define-for-syntax (t=? a b)
  (eq? (->datum a) (->datum b)))

;; edge vertexes 

(define-syntax-rule (edge-1-2 d d1 d2) (vector (unsafe-solve-t d d1 d2) 0.0 0.0))
(define-syntax-rule (edge-2-3 d d2 d3) (vector 1.0 (unsafe-solve-t d d2 d3) 0.0))
(define-syntax-rule (edge-3-4 d d3 d4) (vector (unsafe-solve-t d d4 d3) 1.0 0.0))
(define-syntax-rule (edge-1-4 d d1 d4) (vector 0.0 (unsafe-solve-t d d1 d4) 0.0))

(define-syntax-rule (edge-5-6 d d5 d6) (vector (unsafe-solve-t d d5 d6) 0.0 1.0))
(define-syntax-rule (edge-6-7 d d6 d7) (vector 1.0 (unsafe-solve-t d d6 d7) 1.0))
(define-syntax-rule (edge-7-8 d d7 d8) (vector (unsafe-solve-t d d7 d8) 1.0 1.0))
(define-syntax-rule (edge-5-8 d d5 d8) (vector 0.0 (unsafe-solve-t d d5 d8) 1.0))

(define-syntax-rule (edge-1-5 d d1 d5) (vector 0.0 0.0 (unsafe-solve-t d d1 d5)))
(define-syntax-rule (edge-2-6 d d2 d6) (vector 1.0 0.0 (unsafe-solve-t d d2 d6)))
(define-syntax-rule (edge-3-7 d d3 d7) (vector 1.0 1.0 (unsafe-solve-t d d3 d7)))
(define-syntax-rule (edge-4-8 d d4 d8) (vector 0.0 1.0 (unsafe-solve-t d d4 d8)))

#|
Cube vertex numbers:

   8--------7
  /|       /|
 5--------6 |
 | |      | |      d  y
 | 4------|-3      | /
 |/       |/       |/
 1--------2        +--- x
|#

(define (known-cube-0000-0000 d d1 d2 d3 d4 d5 d6 d7 d8) empty)

(define known-cube-1111-1111 known-cube-0000-0000)

(define (known-cube-1000-0000 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-2 d d1 d2) (edge-1-5 d d1 d5) (edge-1-4 d d1 d4))))

(define known-cube-0111-1111 known-cube-1000-0000)

(define (known-cube-1100-0000 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6)
              (edge-2-3 d d2 d3) (edge-1-4 d d1 d4))))

(define known-cube-0011-1111 known-cube-1100-0000)

(define (known-cube-1110-0000 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6) (edge-3-7 d d3 d7)
              (edge-3-4 d d3 d4) (edge-1-4 d d1 d4))))

(define known-cube-0001-1111 known-cube-1110-0000)

(define (known-cube-1111-0000 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6)
              (edge-3-7 d d3 d7) (edge-4-8 d d4 d8))))

(define known-cube-0000-1111 known-cube-1111-0000)

(define ((make-known-cube-1010-0000 test?) d d1 d2 d3 d4 d5 d6 d7 d8)
  (define da (unsafe-flavg4 d1 d2 d3 d4))
  (cond
    [(test? da d)
     (list (list (edge-1-2 d d1 d2) (edge-2-3 d d2 d3)
                 (edge-3-7 d d3 d7) (edge-1-5 d d1 d5))
           (list (edge-3-4 d d3 d4) (edge-1-4 d d1 d4)
                 (edge-1-5 d d1 d5) (edge-3-7 d d3 d7)))]
    [else
     (list (list (edge-1-2 d d1 d2) (edge-1-5 d d1 d5) (edge-1-4 d d1 d4))
           (list (edge-2-3 d d2 d3) (edge-3-4 d d3 d4) (edge-3-7 d d3 d7)))]))

(define known-cube-1010-0000 (make-known-cube-1010-0000 unsafe-fl>=))
(define known-cube-0101-1111 (make-known-cube-1010-0000 unsafe-fl<))

(define (known-cube-1000-0010 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-2 d d1 d2) (edge-1-5 d d1 d5) (edge-1-4 d d1 d4))
        (list (edge-6-7 d d6 d7) (edge-3-7 d d3 d7) (edge-7-8 d d7 d8))))

(define known-cube-0111-1101 known-cube-1000-0010)

(define ((make-known-cube-1100-0010 test?) d d1 d2 d3 d4 d5 d6 d7 d8)
  (define da (unsafe-flavg4 d2 d6 d7 d3))
  (cond
    [(test? da d)
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6)
                 (edge-6-7 d d6 d7) (edge-7-8 d d7 d8))
           (list (edge-1-4 d d1 d4) (edge-2-3 d d2 d3)
                 (edge-3-7 d d3 d7) (edge-7-8 d d7 d8))
           (list (edge-1-5 d d1 d5) (edge-1-4 d d1 d4) (edge-7-8 d d7 d8)))]
    [else
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6)
                 (edge-2-3 d d2 d3) (edge-1-4 d d1 d4))
           (list (edge-6-7 d d6 d7) (edge-3-7 d d3 d7) (edge-7-8 d d7 d8)))]))

(define known-cube-1100-0010 (make-known-cube-1100-0010 unsafe-fl>=))
(define known-cube-0011-1101 (make-known-cube-1100-0010 unsafe-fl<))

(define ((make-known-cube-1100-0011 test?) d d1 d2 d3 d4 d5 d6 d7 d8)
  (define da (unsafe-flavg4 d1 d5 d8 d4))
  (define db (unsafe-flavg4 d2 d6 d7 d3))
  (cond
    [(and (test? da d) (test? db d))
     (list (list (edge-1-5 d d1 d5) (edge-5-8 d d5 d8)
                 (edge-6-7 d d6 d7) (edge-2-6 d d2 d6))
           (list (edge-1-4 d d1 d4) (edge-2-3 d d2 d3)
                 (edge-3-7 d d3 d7) (edge-4-8 d d4 d8)))]
    [(test? da d)
     (define ec (v* (v+ (v+ (v+ (edge-1-5 d d1 d5) (edge-1-4 d d1 d4))
                            (v+ (edge-4-8 d d4 d8) (edge-5-8 d d5 d8)))
                        (v+ (v+ (edge-6-7 d d6 d7) (edge-2-6 d d2 d6))
                            (v+ (edge-2-3 d d2 d3) (edge-3-7 d d3 d7))))
                    0.125))
     (list (list ec (edge-5-8 d d5 d8) (edge-6-7 d d6 d7))
           (list ec (edge-1-5 d d1 d5) (edge-2-6 d d2 d6))
           (list ec (edge-4-8 d d4 d8) (edge-3-7 d d3 d7))
           (list ec (edge-1-4 d d1 d4) (edge-2-3 d d2 d3))
           (list ec (edge-1-5 d d1 d5) (edge-5-8 d d5 d8))
           (list ec (edge-1-4 d d1 d4) (edge-4-8 d d4 d8))
           (list ec (edge-3-7 d d3 d7) (edge-6-7 d d6 d7))
           (list ec (edge-2-3 d d2 d3) (edge-2-6 d d2 d6)))]
    [(test? db d)
     (define ec (v* (v+ (v+ (v+ (edge-1-5 d d1 d5) (edge-1-4 d d1 d4))
                            (v+ (edge-4-8 d d4 d8) (edge-5-8 d d5 d8)))
                        (v+ (v+ (edge-6-7 d d6 d7) (edge-2-6 d d2 d6))
                            (v+ (edge-2-3 d d2 d3) (edge-3-7 d d3 d7))))
                    0.125))
     (list (list ec (edge-5-8 d d5 d8) (edge-6-7 d d6 d7))
           (list ec (edge-1-5 d d1 d5) (edge-2-6 d d2 d6))
           (list ec (edge-4-8 d d4 d8) (edge-3-7 d d3 d7))
           (list ec (edge-1-4 d d1 d4) (edge-2-3 d d2 d3))
           (list ec (edge-1-5 d d1 d5) (edge-1-4 d d1 d4))
           (list ec (edge-4-8 d d4 d8) (edge-5-8 d d5 d8))
           (list ec (edge-3-7 d d3 d7) (edge-2-3 d d2 d3))
           (list ec (edge-6-7 d d6 d7) (edge-2-6 d d2 d6)))]
    [else
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6)
                 (edge-2-3 d d2 d3) (edge-1-4 d d1 d4))
           (list (edge-5-8 d d5 d8) (edge-6-7 d d6 d7)
                 (edge-3-7 d d3 d7) (edge-4-8 d d4 d8)))]))

(define known-cube-1100-0011 (make-known-cube-1100-0011 unsafe-fl>=))
(define known-cube-0011-1100 (make-known-cube-1100-0011 unsafe-fl<))

#|
Cube vertex numbers:

   8--------7
  /|       /|
 5--------6 |
 | |      | |      d  y
 | 4------|-3      | /
 |/       |/       |/
 1--------2        +--- x
|#


(define ((make-known-cube-1010-0101 test?) d d1 d2 d3 d4 d5 d6 d7 d8)
  (define da (unsafe-flavg4 d1 d2 d3 d4))
  (define db (unsafe-flavg4 d1 d5 d8 d4))
  (define dc (unsafe-flavg4 d3 d4 d8 d7))
  (define dd (unsafe-flavg4 d1 d2 d6 d5))
  (define de (unsafe-flavg4 d2 d3 d7 d6))
  (define df (unsafe-flavg4 d5 d6 d7 d8))
  (append
   (list (list (edge-1-5 d d1 d5) (edge-1-2 d d1 d2) (edge-1-4 d d1 d4))
         (list (edge-2-3 d d2 d3) (edge-3-7 d d3 d7) (edge-3-4 d d3 d4))
         (list (edge-5-6 d d5 d6) (edge-2-6 d d2 d6) (edge-6-7 d d6 d7))
         (list (edge-7-8 d d7 d8) (edge-5-8 d d5 d8) (edge-4-8 d d4 d8)))
   (if (test? da d)
       (list (list (edge-1-2 d d1 d2) (edge-2-3 d d2 d3)
                   (edge-3-4 d d3 d4) (edge-1-4 d d1 d4)))
       empty)
   (if (test? db d)
       (list (list (edge-1-5 d d1 d5) (edge-5-8 d d5 d8)
                   (edge-4-8 d d4 d8) (edge-1-4 d d1 d4)))
       empty)
   (if (test? dc d)
       (list (list (edge-3-4 d d3 d4) (edge-3-7 d d3 d7)
                   (edge-7-8 d d7 d8) (edge-4-8 d d4 d8)))
       empty)
   (if (test? dd d)
       (list (list (edge-1-2 d d1 d2) (edge-2-6 d d2 d6)
                   (edge-5-6 d d5 d6) (edge-1-5 d d1 d5)))
       empty)
   (if (test? de d)
       (list (list (edge-2-3 d d2 d3) (edge-3-7 d d3 d7)
                   (edge-6-7 d d6 d7) (edge-2-6 d d2 d6)))
       empty)
   (if (test? df d)
       (list (list (edge-5-6 d d5 d6) (edge-6-7 d d6 d7)
                   (edge-7-8 d d7 d8) (edge-5-8 d d5 d8)))
       empty)))

(define known-cube-1010-0101 (make-known-cube-1010-0101 unsafe-fl>=))
(define known-cube-0101-1010 (make-known-cube-1010-0101 unsafe-fl<))

(define ((make-known-cube-1110-0001 unsafe-fl>=) d d1 d2 d3 d4 d5 d6 d7 d8)
  (define da (unsafe-flavg4 d1 d5 d8 d4))
  (define db (unsafe-flavg4 d7 d8 d4 d3))
  (cond
    [(and (da . unsafe-fl>= . d) (db . unsafe-fl>= . d))
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6) (edge-3-7 d d3 d7))
           (list (edge-1-5 d d1 d5) (edge-3-7 d d3 d7)
                 (edge-7-8 d d7 d8) (edge-5-8 d d5 d8))
           (list (edge-1-4 d d1 d4) (edge-3-4 d d3 d4) (edge-4-8 d d4 d8)))]
    [(da . unsafe-fl>= . d)
     (define ec (v* (v+ (edge-1-5 d d1 d5) (edge-3-7 d d3 d7)) 0.5))
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6) (edge-3-7 d d3 d7))
           (list ec (edge-3-7 d d3 d7) (edge-3-4 d d3 d4) (edge-1-4 d d1 d4))
           (list ec (edge-1-5 d d1 d5) (edge-5-8 d d5 d8) (edge-7-8 d d7 d8))
           (list ec (edge-1-4 d d1 d4) (edge-4-8 d d4 d8) (edge-7-8 d d7 d8)))]
    [(db . unsafe-fl>= . d)
     (define ec (v* (v+ (edge-1-5 d d1 d5) (edge-3-7 d d3 d7)) 0.5))
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6) (edge-3-7 d d3 d7))
           (list ec (edge-1-5 d d1 d5) (edge-1-4 d d1 d4) (edge-3-4 d d3 d4))
           (list ec (edge-3-7 d d3 d7) (edge-7-8 d d7 d8) (edge-5-8 d d5 d8))
           (list ec (edge-3-4 d d3 d4) (edge-4-8 d d4 d8) (edge-5-8 d d5 d8)))]
    [else
     (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6) (edge-3-7 d d3 d7))
           (list (edge-1-5 d d1 d5) (edge-3-7 d d3 d7)
                 (edge-3-4 d d3 d4) (edge-1-4 d d1 d4))
           (list (edge-7-8 d d7 d8) (edge-5-8 d d5 d8) (edge-4-8 d d4 d8)))]))

(define known-cube-1110-0001 (make-known-cube-1110-0001 unsafe-fl>=))
(define known-cube-0001-1110 (make-known-cube-1110-0001 unsafe-fl<))

(define (known-cube-1110-0100 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-5 d d1 d5) (edge-5-6 d d5 d6) (edge-6-7 d d6 d7)
              (edge-3-7 d d3 d7) (edge-3-4 d d3 d4) (edge-1-4 d d1 d4))))

(define known-cube-0001-1011 known-cube-1110-0100)

(define (known-cube-1110-0010 d d1 d2 d3 d4 d5 d6 d7 d8)
  (list (list (edge-1-5 d d1 d5) (edge-2-6 d d2 d6) (edge-6-7 d d6 d7)
              (edge-7-8 d d7 d8) (edge-3-4 d d3 d4) (edge-1-4 d d1 d4))))

(define known-cube-0001-1101 known-cube-1110-0010)

(define ((make-known-cube-1010-0001 test?) d d1 d2 d3 d4 d5 d6 d7 d8)
  (define da (unsafe-flavg4 d1 d2 d3 d4))
  (define db (unsafe-flavg4 d1 d5 d8 d4))
  (define dc (unsafe-flavg4 d3 d4 d8 d7))
  (append
   (list (list (edge-1-5 d d1 d5) (edge-1-2 d d1 d2) (edge-1-4 d d1 d4))
         (list (edge-2-3 d d2 d3) (edge-3-7 d d3 d7) (edge-3-4 d d3 d4))
         (list (edge-7-8 d d7 d8) (edge-5-8 d d5 d8) (edge-4-8 d d4 d8)))
   (if (test? da d)
       (list (list (edge-1-2 d d1 d2) (edge-2-3 d d2 d3)
                   (edge-3-4 d d3 d4) (edge-1-4 d d1 d4)))
       empty)
   (if (test? db d)
       (list (list (edge-1-5 d d1 d5) (edge-5-8 d d5 d8)
                   (edge-4-8 d d4 d8) (edge-1-4 d d1 d4)))
       empty)
   (if (test? dc d)
       (list (list (edge-3-4 d d3 d4) (edge-3-7 d d3 d7)
                   (edge-7-8 d d7 d8) (edge-4-8 d d4 d8)))
       empty)))

(define known-cube-1010-0001 (make-known-cube-1010-0001 unsafe-fl>=))
(define known-cube-0101-1110 (make-known-cube-1010-0001 unsafe-fl<))

(define-for-syntax known-cubes
  '((0 0 0 0 0 0 0 0)
    (1 0 0 0 0 0 0 0)
    (1 1 0 0 0 0 0 0)
    (1 1 1 0 0 0 0 0)
    (1 1 1 1 0 0 0 0)
    (1 0 1 0 0 0 0 0)
    (1 0 0 0 0 0 1 0)
    (1 1 0 0 0 0 1 0)
    (1 1 0 0 0 0 1 1)
    (1 0 1 0 0 1 0 1)
    (1 1 1 0 0 0 0 1)
    (1 1 1 0 0 1 0 0)
    (1 1 1 0 0 0 1 0)
    (1 0 1 0 0 0 0 1)
    (1 1 1 1 1 1 1 1)
    (0 1 1 1 1 1 1 1)
    (0 0 1 1 1 1 1 1)
    (0 0 0 1 1 1 1 1)
    (0 0 0 0 1 1 1 1)
    (0 1 0 1 1 1 1 1)
    (0 1 1 1 1 1 0 1)
    (0 0 1 1 1 1 0 1)
    (0 0 1 1 1 1 0 0)
    (0 1 0 1 1 0 1 0)
    (0 0 0 1 1 1 1 0)
    (0 0 0 1 1 0 1 1)
    (0 0 0 1 1 1 0 1)
    (0 1 0 1 1 1 1 0)))

;; cube transformations: mirror

(define (mirror-vec-d v)
  (match-define (vector x y d) v)
  (vector x y (unsafe-fl- 1.0 d)))

(define ((mirror-cube-d f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map mirror-vec-d poly))
       (f d d5 d6 d7 d8 d1 d2 d3 d4)))

(define (mirror-vec-y v)
  (match-define (vector x y d) v)
  (vector x (unsafe-fl- 1.0 y) d))

(define ((mirror-cube-y f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map mirror-vec-y poly))
       (f d d4 d3 d2 d1 d8 d7 d6 d5)))

(define (mirror-vec-x v)
  (match-define (vector x y d) v)
  (vector (unsafe-fl- 1.0 x) y d))

(define ((mirror-cube-x f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map mirror-vec-x poly))
       (f d d2 d1 d4 d3 d6 d5 d8 d7)))

;; cube transformations: rotate clockwise (looking positively along axis)

(define (unrotate-vec-d v)
  (match-define (vector x y d) v)
  (vector (unsafe-fl- 1.0 y) x d))

(define ((rotate-cube-d f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map unrotate-vec-d poly))
       (f d d2 d3 d4 d1 d6 d7 d8 d5)))

(define (unrotate-vec-y v)
  (match-define (vector x y d) v)
  (vector d y (unsafe-fl- 1.0 x)))

(define ((rotate-cube-y f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map unrotate-vec-y poly))
       (f d d5 d1 d4 d8 d6 d2 d3 d7)))

(define (unrotate-vec-x v)
  (match-define (vector x y d) v)
  (vector x (unsafe-fl- 1.0 d) y))

(define ((rotate-cube-x f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map unrotate-vec-x poly))
       (f d d4 d3 d7 d8 d1 d2 d6 d5)))

;; cube transformations: rotate counterclockwise (looking negatively along axis)

(define (rotate-vec-d v)
  (match-define (vector x y d) v)
  (vector y (unsafe-fl- 1.0 x) d))

(define ((unrotate-cube-d f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map rotate-vec-d poly))
       (f d d4 d1 d2 d3 d8 d5 d6 d7)))

(define (rotate-vec-y v)
  (match-define (vector x y d) v)
  (vector (unsafe-fl- 1.0 d) y x))

(define ((unrotate-cube-y f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map rotate-vec-y poly))
       (f d d2 d6 d7 d3 d1 d5 d8 d4)))

(define (rotate-vec-x v)
  (match-define (vector x y d) v)
  (vector x d (unsafe-fl- 1.0 y)))

(define ((unrotate-cube-x f) d d1 d2 d3 d4 d5 d6 d7 d8)
  (map (λ (poly) (map rotate-vec-x poly))
       (f d d5 d6 d2 d1 d8 d7 d3 d4)))

(define-for-syntax (cube-points-transform trans src)
  (match-define (list j1 j2 j3 j4 j5 j6 j7 j8) src)
  (cond [(t=? trans #'mirror-cube-d)    (list j5 j6 j7 j8 j1 j2 j3 j4)]
        [(t=? trans #'mirror-cube-y)    (list j4 j3 j2 j1 j8 j7 j6 j5)]
        [(t=? trans #'mirror-cube-x)    (list j2 j1 j4 j3 j6 j5 j8 j7)]
        [(t=? trans #'rotate-cube-d)    (list j4 j1 j2 j3 j8 j5 j6 j7)]
        [(t=? trans #'unrotate-cube-d)  (list j2 j3 j4 j1 j6 j7 j8 j5)]
        [(t=? trans #'rotate-cube-y)    (list j2 j6 j7 j3 j1 j5 j8 j4)]
        [(t=? trans #'unrotate-cube-y)  (list j5 j1 j4 j8 j6 j2 j3 j7)]
        [(t=? trans #'rotate-cube-x)    (list j5 j6 j2 j1 j8 j7 j3 j4)]
        [(t=? trans #'unrotate-cube-x)  (list j4 j3 j7 j8 j1 j2 j6 j5)]))

(define-for-syntax known-transforms
  (list #'mirror-cube-d
        #'mirror-cube-y
        #'mirror-cube-x
        #'rotate-cube-d
        #'rotate-cube-y
        #'rotate-cube-x
        #'unrotate-cube-d
        #'unrotate-cube-y
        #'unrotate-cube-x))

(define-for-syntax (opposite-transform? t1 t2)
  (or (and (t=? t1 #'mirror-d) (t=? t2 #'mirror-d))
      (and (t=? t1 #'mirror-y) (t=? t2 #'mirror-y))
      (and (t=? t1 #'mirror-x) (t=? t2 #'mirror-x))
      (and (t=? t1 #'rotate-d) (t=? t2 #'unrotate-d))
      (and (t=? t1 #'unrotate-d) (t=? t2 #'rotate-d))
      (and (t=? t1 #'rotate-y) (t=? t2 #'unrotate-y))
      (and (t=? t1 #'unrotate-y) (t=? t2 #'rotate-y))
      (and (t=? t1 #'rotate-x) (t=? t2 #'unrotate-x))
      (and (t=? t1 #'unrotate-x) (t=? t2 #'rotate-x))))

(define-for-syntax (first-path-to-cube/src src dst depth)
  (let/ec return
    (let loop ([src src] [path empty] [depth depth])
      ;(printf "path = ~v~n" path)
      (cond [(or (equal? src dst) #;(equal? (invert-cube src) dst))  path]
            [(zero? depth)  #f]
            [else
             (for ([move  (in-list known-transforms)]
                   #:when (or (empty? path)
                              (not (opposite-transform? move (first path)))))
               (define new-src (cube-points-transform move src))
               (define new-path (cons move path))
               (if (or (equal? new-src dst)
                       #;(equal? (invert-cube new-src) dst))
                   (return new-path)
                   (loop new-src new-path (sub1 depth))))
             #f]))))

(define-for-syntax (shortest-path-to-cube dst)
  (let/ec return
    (for ([depth  (in-list '(1 2 3))])
      (for ([src  (in-list known-cubes)])
        (define path (first-path-to-cube/src src dst depth))
        (when path (return (cons src path)))))
    (list #f #f)))

(define-for-syntax (format-cube-id ctxt cube j1 j2 j3 j4 j5 j6 j7 j8)
  (format-id ctxt "~a-~a~a~a~a-~a~a~a~a" cube
             (->datum j1) (->datum j2)
             (->datum j3) (->datum j4)
             (->datum j5) (->datum j6)
             (->datum j7) (->datum j8)))

(define-syntax (define-all-cube-functions stx)
  (syntax-case stx ()
    [(id)
     #`(begin
         #,@(for*/list ([j8  (in-list '(0 1))]
                        [j7  (in-list '(0 1))]
                        [j6  (in-list '(0 1))]
                        [j5  (in-list '(0 1))]
                        [j4  (in-list '(0 1))]
                        [j3  (in-list '(0 1))]
                        [j2  (in-list '(0 1))]
                        [j1  (in-list '(0 1))])
              (define dst (list j1 j2 j3 j4 j5 j6 j7 j8))
              (match-define (cons src path) (shortest-path-to-cube dst))
              (cond [(or (empty? path) (first path))
                     (with-syntax ([define-cube-function
                                     (format-id #'id "define-cube-function")])
                       #`(define-cube-function
                           #,dst #,src #,path))]
                    [else  (void)])))]))

(define-syntax (define-cube-function stx)
  (syntax-case stx ()
    [(id (d1 d2 d3 d4 d5 d6 d7 d8)
         (s1 s2 s3 s4 s5 s6 s7 s8)
         path)
     (with-syntax ([cube-dst  (format-cube-id #'id "cube"
                                              #'d1 #'d2 #'d3 #'d4
                                              #'d5 #'d6 #'d7 #'d8)]
                   [cube-src  (format-cube-id #'id "known-cube"
                                              #'s1 #'s2 #'s3 #'s4
                                              #'s5 #'s6 #'s7 #'s8)])
       (define new-stx
         (syntax-case #'path ()
           [()          #'(define cube-dst cube-src)]
           [(path ...)  #'(define cube-dst ((compose path ...) cube-src))]))
       ;(printf "~a~n" (syntax->datum new-stx))
       new-stx)]))


(define-syntax (make-cube-dispatch-table stx)
  (syntax-case stx ()
    [(id)
     #`(vector
        #,@(for*/list ([j8  (in-list '(0 1))]
                       [j7  (in-list '(0 1))]
                       [j6  (in-list '(0 1))]
                       [j5  (in-list '(0 1))]
                       [j4  (in-list '(0 1))]
                       [j3  (in-list '(0 1))]
                       [j2  (in-list '(0 1))]
                       [j1  (in-list '(0 1))])
             (format-cube-id #'id "cube" j1 j2 j3 j4 j5 j6 j7 j8)))]))

(define-all-cube-functions)

(define cube-dispatch-table
  (make-cube-dispatch-table))

(define-syntax-rule (add-digit j idx)
  (unsafe-fx+ (unsafe-fx* idx 2) j))

(define (unsafe-heights->cube-polys d d1 d2 d3 d4 d5 d6 d7 d8)
  (define j1 (if (d1 . unsafe-fl< . d) 0 1))
  (define j2 (if (d2 . unsafe-fl< . d) 0 1))
  (define j3 (if (d3 . unsafe-fl< . d) 0 1))
  (define j4 (if (d4 . unsafe-fl< . d) 0 1))
  (define j5 (if (d5 . unsafe-fl< . d) 0 1))
  (define j6 (if (d6 . unsafe-fl< . d) 0 1))
  (define j7 (if (d7 . unsafe-fl< . d) 0 1))
  (define j8 (if (d8 . unsafe-fl< . d) 0 1))
  (define facet-num
    (add-digit
     j1 (add-digit
         j2 (add-digit
             j3 (add-digit
                 j4 (add-digit
                     j5 (add-digit
                         j6 (add-digit j7 j8))))))))
  (define f (vector-ref cube-dispatch-table facet-num))
  (f d d1 d2 d3 d4 d5 d6 d7 d8))

(defproc (heights->cube-polys [xa real?] [xb real?] [ya real?] [yb real?] [za real?] [zb real?]
                              [d real?]
                              [d1 real?] [d2 real?] [d3 real?] [d4 real?]
                              [d5 real?] [d6 real?] [d7 real?] [d8 real?]
                              ) (listof (listof (vector/c real? real? real?)))
  (cond [(all inexact-real? xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8)
         (define polys (unsafe-heights->cube-polys d d1 d2 d3 d4 d5 d6 d7 d8))
         (for/list ([poly  (in-list polys)])
           (for/list ([uvw  (in-list poly)])
             (match-define (vector u v w) uvw)
             (vector (unsafe-unsolve-t xa xb u)
                     (unsafe-unsolve-t ya yb v)
                     (unsafe-unsolve-t za zb w))))]
        [(find-failure-index real? xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8)
         => (λ (i) (raise-type-error 'heights->polys "real number"
                                     i xa xb ya yb za zb d d1 d2 d3 d4 d5 d6 d7 d8))]
        [(= d d1 d2 d3 d4 d5 d6 d7 d8)  empty]
        [else
         (let-map
          (d d1 d2 d3 d4 d5 d6 d7 d8) inexact->exact
          (define d-min (min d d1 d2 d3 d4 d5 d6 d7 d8))
          (define d-max (max d d1 d2 d3 d4 d5 d6 d7 d8))
          (define d-scale (- d-max d-min))
          (define polys
            (unsafe-heights->cube-polys (exact->inexact (/ (- d d-min) d-scale))
                                        (exact->inexact (/ (- d1 d-min) d-scale))
                                        (exact->inexact (/ (- d2 d-min) d-scale))
                                        (exact->inexact (/ (- d3 d-min) d-scale))
                                        (exact->inexact (/ (- d4 d-min) d-scale))
                                        (exact->inexact (/ (- d5 d-min) d-scale))
                                        (exact->inexact (/ (- d6 d-min) d-scale))
                                        (exact->inexact (/ (- d7 d-min) d-scale))
                                        (exact->inexact (/ (- d8 d-min) d-scale))))
          (for/list ([poly  (in-list polys)])
            (for/list ([uvw  (in-list poly)])
              (match-define (vector u v w) uvw)
              (vector (unsolve-t xa xb u)
                      (unsolve-t ya yb v)
                      (unsolve-t za zb w)))))]))
