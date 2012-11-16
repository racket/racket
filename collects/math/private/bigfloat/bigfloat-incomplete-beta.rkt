#lang typed/racket/base

(require "../flonum/flonum-functions.rkt"
         "../flonum/flonum-constants.rkt"
         "bigfloat-struct.rkt"
         "bigfloat-continued-fraction.rkt"
         "bigfloat-log-arithmetic.rkt"
         "bigfloat-beta.rkt")

(provide bfbeta-lower-regularized bflog-beta-lower-regularized
         bfbeta-upper-regularized bflog-beta-upper-regularized)

(: in-bounds? (Bigfloat Bigfloat Bigfloat -> Boolean))
(define (in-bounds? a b x)
  (and (bfpositive? x) (x . bf< . 1.bf)
       (bfpositive? a) (a . bf< . +inf.bf)
       (bfpositive? b) (b . bf< . +inf.bf)))

(: hypergeom-fac (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (hypergeom-fac a b x)
  (define a+b (bf+ a b))
  (define a+1 (bf+ a 1.bf))
  (bf* (bf/ (bf+ a+b (bf 20)) (bf+ a+1 (bf 20))) x))

(: bfbeta-regularized-const (Bigfloat Bigfloat Bigfloat Bigfloat Boolean -> Bigfloat))
(define (bfbeta-regularized-const a b log-x log-y log?)
  (define log-c
    (bf+ (bf* a log-x)
         (bf* b log-y)
         (bf- (bflog-beta a b))))
  (if log? log-c (bfexp log-c)))

(: bfbeta-regularized-frac
   (Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Boolean -> Bigfloat))
(define (bfbeta-regularized-frac a b x y log-x log-y l log?)
  (define z
    (bfcontinued-fraction
     1.bf
     (λ: ([n : Bigfloat] [s : Bigfloat])
       (let ([a+n-1  (bf+ a (bf- n 1.bf))]
             [a+2n-1  (bf+ a (bf- (bf* 2.bf n) 1.bf))])
         (bf/ (bf* a+n-1 (bf+ a+n-1 b) n (bf- b n) x x)
              (bf* a+2n-1 a+2n-1))))
     (bf* (bf/ a (bf+ a 1.bf)) (bf+ l 1.bf))
     (λ: ([n : Bigfloat] [t : Bigfloat])
       (let ([a+2n  (bf+ a (bf* 2.bf n))])
         (bf+ (bf/ (bf* n (bf- b n) x) (bf+ a+2n -1.bf))
              (bf* (bf/ (bf+ a n) (bf+ a+2n 1.bf))
                   (bf+ l 1.bf (bf* n (bf+ 1.bf y))))
              n)))
     (bf (* (flexpt 2.0 -10.0) epsilon.0))))
  (cond [log?  (bf+ (bflog z) (bfbeta-regularized-const a b log-x log-y #t))]
        [else  (bf* z (bfbeta-regularized-const a b log-x log-y #f))]))

(: bfbeta-regularized-hypergeom
   (Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Boolean -> Bigfloat))
(define (bfbeta-regularized-hypergeom a b x y log-x log-y log?)
  (define eps (bf (* (flexpt 2.0 -10.0) epsilon.0)))
  (define a+b (bf+ a b))
  (define a+1 (bf+ a 1.bf))
  (define: dzs : (Listof Bigfloat)
    (let loop ([z 0.bf] [dz 1.bf] [dzs  (list 1.bf)] [n 0.0] [i -1])
      ;(printf "dz = ~v  i = ~v~n" (bigfloat->flonum dz) (abs i))
      (define bf-n (bf n))
      (define new-dz (bf* dz (bf/ (bf+ a+b bf-n) (bf+ a+1 bf-n)) x))
      (cond [(zero? i)  (cons new-dz dzs)]
            [else
             (let ([i  (if (and (i . < . 0.0)
                                ((bfabs new-dz) . bf<= . (bfabs dz))
                                ((bfabs new-dz) . bf<= . (bf* eps (bfabs z))))
                           200
                           (- i 1))])
               (loop (bf+ z new-dz) new-dz (cons new-dz dzs) (+ n 1.0) i))])))
  (define z (apply bf+ dzs))
  (cond [log?  (bf+ (bf- (bflog z) (bflog a))
                    (bfbeta-regularized-const a b log-x log-y #t))]
        [else  (bf* (bf/ z a)
                    (bfbeta-regularized-const a b log-x log-y #f))]))

(: bfbeta-regularized-limits (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bfbeta-regularized-limits a b x)
  (cond [(or (bfnegative? x) (x . bf> . 1.bf) (bfnegative? a) (bfnegative? b))  +nan.bf]
        [(bf= x 1.bf)  x]
        [(and (bfzero? a) (bfzero? b))  (bf 0.5)]
        [(and (bf= a +inf.bf) (bf= b +inf.bf))  (if (x . bf< . (bf 0.5)) 0.bf 1.bf)]
        [(bf= a +inf.bf)  0.bf]
        [(bf= b +inf.bf)  1.bf]
        [(bfzero? a)  1.bf]
        [(bfzero? b)  0.bf]
        [(bfzero? x)  0.bf]
        [else  +nan.bf]))

(: maybe1- (Bigfloat Boolean Boolean -> Bigfloat))
(define (maybe1- z log? 1-?)
  (cond [1-?  (if log? (bflog1- z) (bf- 1.bf z))]
        [else  z]))

(: get-large-params
   (Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Boolean
           -> (Values Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Boolean)))
(define (get-large-params a b x y log-x log-y 1-?)
  (define l
    (cond [(a . bf> . b)  (bf- (bf* (bf+ a b) y) b)]
          [else  (bf- a (bf* (bf+ a b) x))]))
  (if (bfnegative? l)
      (values b a y x log-y log-x (bf- l) (not 1-?))
      (values a b x y log-x log-y l 1-?)))

(: get-hypergeom-params
   (Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Boolean
           -> (Values Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Bigfloat Boolean)))
(define (get-hypergeom-params a b x y log-x log-y 1-?)
  (if ((hypergeom-fac b a y) . bf< . (hypergeom-fac a b x))
      (values b a y x log-y log-x (not 1-?))
      (values a b x y log-x log-y 1-?)))

(: bfbeta-regularized (Bigfloat Bigfloat Bigfloat Boolean Boolean -> Bigfloat))
(define (bfbeta-regularized a b x log? 1-?)
  (define y (bf- 1.bf x))
  (define log-x (bflog x))
  (define log-y (bflog1p (bf- x)))
  (cond [(not (in-bounds? a b x))
         (maybe1- (bfbeta-regularized-limits a b x)
                  log? 1-?)]
        [(and (a . bf< . 1.bf) (b . bf< . 1.bf))
         (let-values ([(a b x y log-x log-y 1-?)  (get-hypergeom-params a b x y log-x log-y 1-?)])
           (maybe1- (bfbeta-regularized-hypergeom a b x y log-x log-y log?)
                    log? 1-?))]
        [else
         (define f1 (hypergeom-fac a b x))
         (define f2 (hypergeom-fac b a y))
         (cond
           [(f1 . bf< . (bf 0.75))
            (maybe1- (bfbeta-regularized-hypergeom a b x y log-x log-y log?)
                     log? 1-?)]
           [(f2 . bf< . (bf 0.75))
            (maybe1- (bfbeta-regularized-hypergeom b a y x log-y log-x log?)
                     log? (not 1-?))]
           [else
            (let-values ([(a b x y log-x log-y l 1-?)  (get-large-params a b x y log-x log-y 1-?)])
              (maybe1- (bfbeta-regularized-frac a b x y log-x log-y l log?)
                       log? 1-?))])]))

(: bfbeta-lower-regularized (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bfbeta-lower-regularized a b x)
  (bfbeta-regularized a b x #f #f))

(: bfbeta-upper-regularized (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bfbeta-upper-regularized a b x)
  (bfbeta-regularized a b x #f #t))

(: bflog-beta-lower-regularized (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bflog-beta-lower-regularized a b x)
  (bfbeta-regularized a b x #t #f))

(: bflog-beta-upper-regularized (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bflog-beta-upper-regularized a b x)
  (bfbeta-regularized a b x #t #t))
