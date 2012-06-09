#lang racket

(require unstable/flonum)

(provide random-exponential random-laplace
         random-flonum random-single-flonum
         random-integer->random-exact-rational
         random-integer->random-flonum
         random-integer->random-single-flonum
         random-integer->random-real)

;; Returns a rational flonum from an exponential distribution
;; About half the numbers from this distribution are < scale, and half are > scale
(define (random-exponential [scale 1.0])
  (let loop ()
    (define r (random))
    ;; appx. probability 1.11e-16 this loops:
    (cond [(= r 0)  (loop)]
          [else  (exact->inexact (* (- (log (random))) scale))])))

;; Randomly signs a flonum from random-exponential
(define (random-laplace [scale 1.0])
  (define r (random-exponential scale))
  (if ((random) . < . 0.5) r (- r)))

;; Returns an integer from Uniform(0,2^64-1)
(define (random-flonum-bits)
  (for/fold ([i 0]) ([_  (in-range 4)])
    (+ (* i #x10000) (random #x10000))))

;; Returns a rational flonum with bits uniformly distributed
(define (random-flonum)
  (let loop ()
    (define x (bit-field->flonum (random-flonum-bits)))
    ;; appx. 0.0005 probability this loops
    (if (rational? x) x (loop))))

;; Returns a single flonum with bits uniformly distributed; sometimes -inf.f or +inf.f
(define (random-single-flonum*)
  (define s (random #b10))  ; sign bit
  (define e (random #b100000000))  ; exponent
  (define f (random #b100000000000000000000000))  ; fractional part
  ;; Formula from Wikipedia page on IEEE 754 binary32 format
  (real->single-flonum
   (* (if (= s 0) 1 -1)
      (+ 1 (/ f #b100000000000000000000000))
      (expt 2 (- e 127)))))

;; Returns a rational single flonum with bits uniformly distributed
(define (random-single-flonum)
  (let loop ()
    (define x (random-single-flonum*))
    (if (rational? x) x (loop))))

;; Converts random integers to random exact rationals
(define (random-integer->random-exact-rational E)
  (cond
    [(= E 0)  0]  ; code below would pick 0/0
    [else
     ;; random fraction
     (define n (exact-ceiling (random-exponential E)))
     (define d (exact-ceiling (random-exponential E)))
     (cond
       [(= d 0)  0]  ; appx. probability 1.11e-16
       [else
        (define x (/ n d))
        (if ((random) . < . 0.5) x (- x))])]))

;; Converts random integers to random double flonums
;; Often returns very large or small numbers (near the limits of floating-point range)
;; Sometimes returns +nan.0, +inf.0 or -inf.0
(define (random-integer->random-flonum E)
  (define r (random))
  (cond
    ;; probability 0.25: random flonum, laplace-distributed with scale E
    [(r . < . 0.25)  (random-laplace (abs E))]
    ;; probability 0.25: random flonum with uniform bits
    [(r . < . 0.50)  (random-flonum)]
    ;; probability 0.35: very small or very large flonum
    [(r . < . 0.85)
     (define r (random))
     (cond [(r . < . 0.5)
            (define x (ordinal->flonum E))
            (cond [(= x 0.0)  (if ((random) . < . 0.5) 0.0 -0.0)]
                  [else  x])]
           [(r . < . 0.75)
            (flstep -inf.0 (abs E))]
           [else
            (flstep +inf.0 (- (abs E)))])]
    ;; probability 0.05 each: +nan.0, +inf.0, -inf.0
    [(r . < . 0.90)  +nan.0]
    [(r . < . 0.95)  +inf.0]
    [else            -inf.0]))

;; Converts random integers to random single flonums
;; Sometimes returns +nan.f, +inf.f or -inf.f
(define (random-integer->random-single-flonum E)
  (define r (random))
  (cond
    ;; probability 0.50: random single flonum, laplace-distributed with scale E
    [(r . < . 0.50)  (real->single-flonum (random-laplace (abs E)))]
    ;; probability 0.35: random single flonum with uniform bits
    [(r . < . 0.85)  (random-single-flonum)]
    ;; probability 0.05 each: +nan.f, +inf.f, -inf.f
    [(r . < . 0.90)  +nan.f]
    [(r . < . 0.95)  +inf.f]
    [else            -inf.f]))

;; Converts random integers to random reals
(define (random-integer->random-real E)
  (define r (random))
  ;; probability 0.25 each
  (cond [(r . < . 0.25)  r]
        [(r . < . 0.50)  (random-integer->random-exact-rational E)]
        [(r . < . 0.75)  (random-integer->random-flonum E)]
        [else            (random-integer->random-single-flonum E)]))
