#lang typed/racket/base

(require "bigfloat-struct.rkt")

(provide bflog* bflog/ bflog+ bflog- bflog1- bflogb)

(: bflog* (Bigfloat Bigfloat -> Bigfloat))
(define (bflog* log-x log-y) (bf+ log-x log-y))

(: bflog/ (Bigfloat Bigfloat -> Bigfloat))
(define (bflog/ log-x log-y) (bf- log-x log-y))

(: bflog+ (Bigfloat Bigfloat -> Bigfloat))
(define (bflog+ log-x log-y)
  (let-values ([(log-x log-y)  (if (log-x . bf> . log-y)
                                   (values log-x log-y)
                                   (values log-y log-x))])
    (bf+ log-x (bflog1p (bfexp (bf- log-y log-x))))))

(: bflog- (Bigfloat Bigfloat -> Bigfloat))
(define (bflog- log-x log-y)
  (cond [(log-y . bf> . log-x)  +nan.bf]
        [else  (bf+ log-x (bflog1p (bf- (bfexp (bf- log-y log-x)))))]))

(: bflog1- (Bigfloat -> Bigfloat))
(define (bflog1- log-x)
  (cond [(log-x . bf> . (bflog (bf 0.5)))  (bflog (bf- (bfexpm1 log-x)))]
        [else  (bflog1p (bf- (bfexp log-x)))]))

(: bflogb (Bigfloat Bigfloat -> Bigfloat))
(define (bflogb b x)
  (cond [(bf= x 1.bf)  0.bf]
        [(bf= b 1.bf)  +nan.bf]
        [(not (and (bf<= 0.bf b) (bf<= b +inf.bf) (bf<= 0.bf x) (bf<= x +inf.bf)))  +nan.bf]
        [(bf= b 0.bf)
         (cond [(bf= x 0.bf)  +inf.bf]
               [(bf= x +inf.bf)  -inf.bf]
               [(bf<= x 1.bf)  0.bf]
               [else  -0.bf])]
        [(bf= b +inf.bf)
         (cond [(bf= x 0.bf)  -inf.bf]
               [(bf= x +inf.bf)  +inf.bf]
               [(bf<= 1.bf x)  0.bf]
               [else  -0.bf])]
        [(bf= x 0.bf)  (if (bf< b 1.bf) +inf.bf -inf.bf)]
        [(bf= x +inf.bf)  (if (bf< b 1.bf) -inf.bf +inf.bf)]
        [else
         (bf/ (bflog x) (bflog b))]))
