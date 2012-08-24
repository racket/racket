#lang typed/racket/base

(require "bigfloat-struct.rkt"
         "bigfloat-continued-fraction.rkt"
         "bigfloat-log-arithmetic.rkt")

(provide bfgamma-lower
         bflog-gamma-lower
         bfgamma-upper
         bflog-gamma-upper)

(: bfgamma-lower-iter (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bfgamma-lower-iter k x eps)
  (let: loop : Bigfloat ([y  : Bigfloat  +0.bf]
                         [dy : Bigfloat  (bf/ x (bf+ k +1.bf))]
                         [i  : Bigfloat  +0.bf])
    (define new-y (bf+ y dy))
    (cond [(or (bf= new-y +inf.bf) ((bfabs dy) . bf<= . (bf* eps new-y)))  new-y]
          [else  (loop new-y (bf/ (bf* dy x) (bf+ +2.bf i k)) (bf+ i +1.bf))])))

(: bfgamma-lower-series (Bigfloat Bigfloat -> Bigfloat))
;; Computes the lower gamma function from its series
(define (bfgamma-lower-series k x)
  (define eps +epsilon.bf)
  (bfcopy
   (parameterize ([bf-precision  (+ (bf-precision) 20)])
     (define y (bfgamma-lower-iter k x +epsilon.bf))
     (define log-z (bf- (bf* k (bflog x)) (bf+ x (bflog k))))
     (let ([z  (cond [((bfabs log-z) . bf< . +1.bf)  (bfexp log-z)]
                     [else  (bf/ (bf* (bfexpt x k) (bfexp (bf- x))) k)])])
       (bf+ z (bf* z y))))))

(: bflog-gamma-lower-series (Bigfloat Bigfloat -> Bigfloat))
;; Computes the log of the lower gamma function from its series
(define (bflog-gamma-lower-series k x)
  (bfcopy
   (parameterize ([bf-precision  (+ (bf-precision) 10)])
     (bflog (bfgamma-lower-series k x)))))

;; ===================================================================================================

(: bfgamma-upper-iter (Bigfloat Bigfloat Bigfloat -> Bigfloat))
(define (bfgamma-upper-iter k x eps)
  (bfcontinued-fraction +1.bf
                        (λ (i a) (bf* i (bf- k i)))
                        (bf+ +1.bf (bf- x k))
                        (λ (i b) (bf+ (bf- x k) (bf+ (bf* +2.bf i) +1.bf)))
                        eps))

(: bfgamma-upper-frac (Bigfloat Bigfloat -> Bigfloat))
;; Computes the upper gamma function using Legendre's continued fraction
(define (bfgamma-upper-frac k x)
  (define eps +epsilon.bf)
  (bfcopy
   (parameterize ([bf-precision  (+ (bf-precision) 20)])
     (define y (bfgamma-upper-iter k x eps))
     (define log-z (bf- (bf* k (bflog x)) x))
     (let ([z  (cond [((bfabs log-z) . bf< . +1.bf)  (bfexp log-z)]
                     [else  (bf* (bfexpt x k) (bfexp (bf- x)))])])
       (bf* y z)))))
  
(: bflog-gamma-upper-frac (Bigfloat Bigfloat -> Bigfloat))
;; Computes the log of the upper gamma function using Legendre's continued fraction
(define (bflog-gamma-upper-frac k x)
  (bfcopy
   (parameterize ([bf-precision  (+ (bf-precision) 10)])
     (bflog (bfgamma-upper-frac k x)))))

;; ===================================================================================================

(: use-lower? (Bigfloat Bigfloat -> Boolean))
;; Determines whether to compute an incomplete gamma function using the lower's series or upper's
;; continued fraction
(define (use-lower? k x)
  (or (x . bf< . k) (and (x . bf< . +4.bf) (k . bf< . +3.bf))))

(: bfgamma-lower (Bigfloat Bigfloat -> Bigfloat))
(define (bfgamma-lower k x)
  (cond [(k . bf<= . +0.bf)  +nan.bf]
        [(x . bf<  . +0.bf)  +nan.bf]
        [(use-lower? k x)  (bfgamma-lower-series k x)]
        [else  (bf- (bfgamma k) (bfgamma-upper-frac k x))]))

(: bflog-gamma-lower (Bigfloat Bigfloat -> Bigfloat))
(define (bflog-gamma-lower k x)
  (cond [(k . bf<= . +0.bf)  +nan.bf]
        [(x . bf<  . +0.bf)  +nan.bf]
        [(use-lower? k x)  (bflog-gamma-lower-series k x)]
        [else  (bflog- (bflog-gamma k) (bflog-gamma-upper-frac k x))]))

(: bfgamma-upper (Bigfloat Bigfloat -> Bigfloat))
(define (bfgamma-upper k x)
  (cond [(k . bf<= . +0.bf)  +nan.bf]
        [(x . bf<  . +0.bf)  +nan.bf]
        [(use-lower? k x)  (bf- (bfgamma k) (bfgamma-lower-series k x))]
        [else  (bfgamma-upper-frac k x)]))

(: bflog-gamma-upper (Bigfloat Bigfloat -> Bigfloat))
(define (bflog-gamma-upper k x)
  (cond [(k . bf<= . +0.bf)  +nan.bf]
        [(x . bf<  . +0.bf)  +nan.bf]
        [(use-lower? k x)  (bflog- (bflog-gamma k) (bflog-gamma-lower-series k x))]
        [else  (bflog-gamma-upper-frac k x)]))
