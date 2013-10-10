#lang typed/racket/base

(require racket/fixnum
         "../../flonum.rkt"
         "../../base.rkt"
         "../number-theory/bernoulli.rkt"
         "../number-theory/factorial.rkt"
         "bigfloat-struct.rkt")

(provide bfhurwitz-zeta)

(define 0.5b0 (parameterize ([bf-precision 2]) (bf 0.5)))

(: bfhurwitz-zeta-series (Bigfloat Bigfloat -> Bigfloat))
(define (bfhurwitz-zeta-series s q)
  (define eps (bf* 0.5b0 epsilon.bf))
  (let loop ([i 0] [y 0.bf])
    (define dy (bfexpt (bf+ q (bf i)) (bf- s)))
    (define new-y (bf+ y dy))
    (cond [(or ((bfabs dy) . bf<= . (bf* eps new-y))
               (not (bfrational? new-y)))
           new-y]
          [else
           (loop (+ i 1) new-y)])))

(: bfhurwitz-zeta-euler-maclaurin (Bigfloat Bigfloat -> Bigfloat))
(define (bfhurwitz-zeta-euler-maclaurin s q)
  (define n (exact-ceiling (+ (* 0.5 (bf-precision) (/ (log 2) (log 10))) 10)))
  (: f (Integer -> Bigfloat))
  (define (f k) (bfexpt (bf+ (bf k) q) (bf- s)))
  (define fn (f n))
  (define n+q (bf+ (bf n) q))
  (define sqr-n+q (bf* n+q n+q))
  (define eps epsilon.bf)
  (define y0
    (for/fold: ([y0 : Bigfloat  (bf* fn (bf+ (bf/ n+q (bf- s 1.bf)) 0.5b0))]
                ) ([k  (in-range n)])
      (bf+ y0 (f k))))
  (define max-k 100)
  (let: loop : Bigfloat ([y : Bigfloat  y0]
                         [numer : Bigfloat  s]
                         [denom : Bigfloat  (bf/ fn n+q)]
                         [k : Nonnegative-Fixnum  0])
    (define ck (bf (/ (bernoulli-number (* 2 (fx+ k 1))) (factorial (* 2 (fx+ k 1))))))
    (define dy (bf* (bf* numer denom) ck))
    (define new-y (bf+ y dy))
    (cond [((bfabs dy) . bf<= . (bf* eps (bfabs new-y)))
           new-y]
          [else
           (define k.bf (bf k))
           (loop new-y
                 (bf* (bf* numer (bf+ s (bf+ (bf* 2.bf k.bf) 1.bf)))
                      (bf+ s (bf+ (bf* 2.bf k.bf) 2.bf)))
                 (bf/ denom sqr-n+q)
                 (fx+ k 1))])))

(: bfhurwitz-zeta (Bigfloat Bigfloat -> Bigfloat))
(define (bfhurwitz-zeta s q)
  (cond [(s . bf<= . 1.bf)  (if (bf= s 1.bf) +inf.bf +nan.bf)]
        [(q . bf<= . 0.bf)  (if (bf= q 0.bf) +inf.bf +nan.bf)]
        [(s . bf> . (bf/ (bflog (bf* 0.5b0 epsilon.bf)) (bf- (bflog q) (bflog1p q))))
         ;; At this point, only the first term in the series is necessary; the condition can had by
         ;; solving for s in (q+1)^-s < 0.5 * epsilon.0 * q^-s
         (bfexpt q (bf- s))]
        [(s . bf> . (bf+ (bf* 2.bf q) (bf 15)))
         ;; Determined experimentally that the series computes fewer total iterations here
         (bfhurwitz-zeta-series s q)]
        [else
         (bfhurwitz-zeta-euler-maclaurin s q)]))
