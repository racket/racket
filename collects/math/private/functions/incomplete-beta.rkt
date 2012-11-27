#lang typed/racket/base

#|
Limited-domain hypergeometric implementations from:

John Pearson.
Computation of Hypergeometric Functions.


Continued fraction and asymptotic expansion from:

Armido R Didonato and Alfred H Morris Jr.
Algorithm 708: Significant Digit Computation of the Incomplete Beta Function Ratios.
ACM Transactions on Mathematical Software, 1992, vol 18, no 3, pp 360--373.
|#

(require (only-in racket/math exact-ceiling)
         "../../flonum.rkt"
         "incomplete-beta-asym.rkt"
         "beta.rkt"
         "continued-fraction.rkt"
         "lanczos.rkt")

(provide fllog-beta-inc
         flbeta-inc
         log-beta-inc
         beta-inc)

(: hypergeom-fac (Flonum Flonum Flonum -> Flonum))
;; Returns the adjustment to the hypergeometric series coefficient at n = 20
;; If this is < 0.5, the series would converge in < 50 iterations or so
(define (hypergeom-fac a b x)
  (define a+b (fl+ a b))
  (define a+1 (fl+ a 1.0))
  (fl* (fl/ (fl+ a+b 20.0) (fl+ a+1 20.0)) x))

;; ===================================================================================================

(: use-asym? (Flonum Flonum Flonum Flonum -> Boolean))
(define (use-asym? a b x y)
  (define p (fl/ a (fl+ a b)))
  (define q (fl/ b (fl+ a b)))
  (or (and (100.0 . fl< . a) (a . fl<= . b) (x . fl>= . (fl* 0.97 p)))
      (and (100.0 . fl< . b) (b . fl< . a) (y . fl<= . (fl* 1.03 q)))))

(: in-bounds? (Flonum Flonum Flonum -> Boolean))
(define (in-bounds? a b x)
  (and (x . fl> . 0.0) (x . fl< . 1.0) 
       (a . fl> . 0.0) (a . fl< . +inf.0)
       (b . fl> . 0.0) (b . fl< . +inf.0)))

(: get-large-params
   (Flonum Flonum Flonum Flonum Flonum Flonum Any
           -> (Values Flonum Flonum Flonum Flonum Flonum Flonum Flonum Any)))
(define (get-large-params a b x y log-x log-y 1-?)
  (define l
    (let ([a  (inexact->exact a)]
          [b  (inexact->exact b)]
          [x  (inexact->exact x)])
      (exact->inexact (- a (* (+ a b) x)))))
  (if (l . fl< . 0.0)
      (values b a y x log-y log-x (- l) (not 1-?))
      (values a b x y log-x log-y l 1-?)))

(: get-hypergeom-params
   (Flonum Flonum Flonum Flonum Flonum Flonum Any
           -> (Values Flonum Flonum Flonum Flonum Flonum Flonum Any)))
(define (get-hypergeom-params a b x y log-x log-y 1-?)
  (if ((hypergeom-fac b a y) . fl< . (hypergeom-fac a b x))
      (values b a y x log-y log-x (not 1-?))
      (values a b x y log-x log-y 1-?)))

(: maybe1- (Flonum Any Any -> Flonum))
(define (maybe1- z log? 1-?)
  (cond [1-?  (if log? (lg1- z) (fl- 1.0 z))]
        [else  z]))

;; ===================================================================================================

(: flbeta-regularized-const-beta (Flonum Flonum Flonum Flonum Any -> Flonum))
(define (flbeta-regularized-const-beta a b log-x log-y log?)
  (define log-t (flsum (list (* a log-x) (* b log-y) (- (fllog-beta a b)))))
  (if log? log-t (flexp log-t)))

(: flbeta-regularized-const-lanczos (Flonum Flonum Flonum Flonum Flonum Flonum Any
                                                  -> Flonum))
(define (flbeta-regularized-const-lanczos a b x y log-x log-y log?)
  (define g (fl- lanczos-g 0.5))
  (define a+g (fl+ a g))
  (define b+g (fl+ b g))
  (define c+g (fl+ (fl+ a b) g))
  (define log-t0  ;; = (log (/ (* x c+g) a+g))
    (let ([t0  (fl/ (fl* x c+g) a+g)])
      (cond [(t0 . fl<= . +max-subnormal.0)  (flsum (list log-x (fllog c+g) (- (fllog a+g))))]
            [(t0 . fl<= . 0.75)  (fllog t0)]
            [else  (define x-rem (fl- (fl- 1.0 x) y))
                   (define-values (c0 c1) (fl*/error c+g x))
                   (fllog1p (fl/ (fl+ (- c0 a+g) (fl+ (fl* c+g x-rem) c1)) a+g))])))
  (define log-t1  ;; = (log (/ (* y c+g) b+g))
    (let ([t1  (fl/ (fl* y c+g) b+g)])
      (cond [(t1 . fl<= . +max-subnormal.0)  (flsum (list log-y (fllog c+g) (- (fllog b+g))))]
            [(t1 . fl<= . 0.75)  (fllog t1)]
            [else  (define y-rem (fl- (fl- 1.0 y) x))
                   (define-values (c0 c1) (fl*/error c+g y))
                   (fllog1p (fl/ (fl+ (fl- c0 b+g) (fl+ (fl* c+g y-rem) c1)) b+g))])))
  (define log-t2
    (let ([t2  (fl/ (fl* a+g b+g) c+g)])
      (cond [(and (t2 . fl> . +max-subnormal.0) (t2 . fl< . +inf.0))  (fl* 0.5 (fllog t2))]
            [else  (fl* 0.5 (flsum (list (fllog a+g) (fllog b+g) (- (fllog c+g)))))])))
  (define t4 (fl/ (fl/ (lanczos-sum (fl+ a b)) (lanczos-sum a)) (lanczos-sum b)))
  (define log-t (flsum (list (fl* a log-t0) (fl* b log-t1) log-t2 g (fllog t4))))
  (if log? log-t (flexp log-t)))

(: flbeta-regularized-const (Flonum Flonum Flonum Flonum Flonum Flonum Any -> Flonum))
(define (flbeta-regularized-const a b x y log-x log-y log?)
  (cond [(and (a . fl> . 1.0) (b . fl> . 1.0)
              ((fl/ (flmax a b) (flmin a b)) . fl< . 100.0))
         (flbeta-regularized-const-lanczos a b x y log-x log-y log?)]
        [else
         (flbeta-regularized-const-beta a b log-x log-y log?)]))

(: flbeta-regularized-frac (Flonum Flonum Flonum Flonum Flonum Flonum Flonum Any -> Flonum))
;; Didonato and Morris's continued fraction
(define (flbeta-regularized-frac a b x y log-x log-y l log?)
  (define-values (s t)
    (continued-fraction-parts
     1.0
     (λ: ([n : Flonum] [s : Flonum])
       (let ([a+n-1  (fl+ a (fl- n 1.0))]
             [a+2n-1  (fl+ a (fl- (fl* 2.0 n) 1.0))]
             [a+b+n-1  (fl+ (fl+ a b) (fl- n 1.0))])
         (fl/ (fl* (fl* (fl* (fl* (fl* a+n-1 a+b+n-1) n) (fl- b n)) x) x)
              (fl* a+2n-1 a+2n-1))))
     (fl* (fl/ a (fl+ a 1.0)) (fl+ l 1.0))
     (λ: ([n : Flonum] [t : Flonum])
       (let ([a+2n  (fl+ a (fl* 2.0 n))])
         (fl+ (fl+ (fl/ (fl* (fl* n (fl- b n)) x)
                        (fl+ a+2n -1.0))
                   (fl* (fl/ (fl+ a n) (fl+ a+2n 1.0))
                        (fl+ (fl+ l 1.0) (fl* n (fl+ 1.0 y)))))
              n)))
     (fl* 0.5 epsilon.0)))
  (cond [log?  (fl+ (flbeta-regularized-const a b x y log-x log-y #t)
                    (fllog-quotient s t))]
        [else  (fl* (flbeta-regularized-const a b x y log-x log-y #f)
                    (fl/ s t))]))

(: flbeta-regularized-hypergeom (Flonum Flonum Flonum Flonum Flonum Flonum Any -> Flonum))
;; Computes lower incomplete beta using the hypergeometric series
(define (flbeta-regularized-hypergeom a b x y log-x log-y log?)
  (define a+b (fl+ a b))
  (define a+1 (fl+ a 1.0))
  (define: z : Flonum
    (let loop ([z 0.0] [dz 1.0] [n 0.0] [i -1.0])
      ;(printf "dz = ~v  i = ~v~n" dz i)
      (define new-dz (fl* (fl* dz (fl/ (fl+ a+b n) (fl+ a+1 n))) x))
      (cond [(zero? i)  (fl+ z new-dz)]
            [else
             (let ([i  (if (and (i . fl< . 0.0)
                                ((flabs new-dz) . fl<= . (flabs dz))
                                ((flabs new-dz) . fl<= . (fl* (fl* 0.5 epsilon.0) (flabs z))))
                           3.0
                           (fl- i 1.0))])
               (loop (fl+ z new-dz) new-dz (fl+ n 1.0) i))])))
  (cond [log?  (flsum (list (flbeta-regularized-const a b x y log-x log-y #t)
                            (- (fllog a))
                            (fllog1p z)))]
        [else  (define c (flbeta-regularized-const a b x y log-x log-y #f))
               (fl/ (fl+ c (fl* z c)) a)]))

(: flbeta-regularized-limits (Flonum Flonum Flonum -> Flonum))
(define (flbeta-regularized-limits a b x)
  (cond [(or (x . fl< . 0.0) (x . fl> . 1.0)
             (a . fl< . 0.0) (b . fl< . 0.0)
             (and (fl= a 0.0) (fl= b 0.0))
             (and (fl= a +inf.0) (fl= b +inf.0)))
         +nan.0]
        [(fl= x 1.0)  1.0]
        [(fl= a +inf.0)  0.0]
        [(fl= b +inf.0)  1.0]
        [(fl= a 0.0)  1.0]
        [(fl= b 0.0)  0.0]
        [(fl= x 0.0)  0.0]
        [else  +nan.0]))

;; ===================================================================================================
;; Main driver

(define: alg : Integer  0)
(define (get-alg) alg)

(: flbeta-regularized (Flonum Flonum Flonum Any Any -> Flonum))
(define (flbeta-regularized a b x log? 1-?)
  (define y (fl- 1.0 x))
  (define log-x (fllog x))
  (define log-y (fllog1p (- x)))
  (cond
    [(not (in-bounds? a b x))
     (set! alg 0)
     (define z (flbeta-regularized-limits a b x))
     (maybe1- (if log? (fllog z) z)
              log? 1-?)]
    [(and (a . fl< . 1.0) (b . fl< . 1.0))
     (let-values ([(a b x y log-x log-y 1-?)
                   (get-hypergeom-params a b x y log-x log-y 1-?)])
       (set! alg 2)
       (maybe1- (flbeta-regularized-hypergeom a b x y log-x log-y log?)
                log? 1-?))]
    [((hypergeom-fac a b x) . fl< . 0.75)
     (set! alg 2)
     (maybe1- (flbeta-regularized-hypergeom a b x y log-x log-y log?)
              log? 1-?)]
    [((hypergeom-fac b a y) . fl< . 0.75)
     (set! alg 2)
     (maybe1- (flbeta-regularized-hypergeom b a y x log-y log-x log?)
              log? (not 1-?))]
    [else
     (let-values ([(a b x y log-x log-y l 1-?)
                   (get-large-params a b x y log-x log-y 1-?)])
       (define z
         (cond [(use-asym? a b x y)
                (set! alg 4)
                (flbeta-lower-regularized-asym a b l log?)]
               [else
                (set! alg 3)
                (flbeta-regularized-frac a b x y log-x log-y l log?)]))
       (maybe1- z log? 1-?))]))

;; ===================================================================================================
;; User-facing functions

(: fllog-beta-inc (Flonum Flonum Flonum Any Any -> Flonum))
(define (fllog-beta-inc a b x upper? regularized?)
  (define z (flbeta-regularized a b x #t upper?))
  (cond [regularized?  z]
        [else  (fl+ z (fllog-beta a b))]))

(: flbeta-inc (Flonum Flonum Flonum Any Any -> Flonum))
(define (flbeta-inc a b x upper? regularized?)
  (define z (flbeta-regularized a b x #f upper?))
  (cond [regularized?  z]
        [else  (fl* z (flbeta a b))]))

(define-syntax-rule (define-incomplete-beta-wrapper name flname)
  (begin
    (: name (case-> (Real Real Real -> Float)
                    (Real Real Real Any -> Float)
                    (Real Real Real Any Any -> Float)))
    (define (name a b x [upper? #f] [regularized? #f])
      (cond [(and (exact? a) (a . <= . 0))
             (raise-argument-error 'name "Positive-Real" 0 a b x)]
            [(and (exact? b) (b . <= . 0))
             (raise-argument-error 'name "Positive-Real" 1 a b x)]
            [(and (exact? x) (or (x . < . 0) (x . > . 1)))
             (raise-argument-error 'name "Nonnegative-Real <= 1" 2 a b x)]
            [else  (flname (fl a) (fl b) (fl x) upper? regularized?)]))))

(define-incomplete-beta-wrapper beta-inc flbeta-inc)
(define-incomplete-beta-wrapper log-beta-inc fllog-beta-inc)
