#lang typed/racket/base

#|
Algorithm AS 241: The Percentage Points of the Normal Distribution
M. J. Wichura
Applied Statistics, Vol. 37 (1988), pp. 477-484

Added asymptotic expansions to increase range when given log probabilities, and a Newton iteration to
fix up answers in the tricky spot between -20000 and -744
|#

(require "../../../flonum.rkt"
         "../../functions/log-arithmetic.rkt"
         "normal-pdf.rkt"
         "normal-cdf.rkt")

(provide standard-flnormal-inv-cdf
         standard-flnormal-inv-log-cdf)

(: R1 (Float -> Float))
;; Returns the degree-7 minimax rational approximation for probit(0.5+q) for |q| <= 0.425
(define (R1 q)
  (let* ([r  (- 0.180625 (* q q))]
         [x  2.5090809287301226727e3]
         [x  (+ (* x r) 3.3430575583588128105e4)]
         [x  (+ (* x r) 6.7265770927008700853e4)]
         [x  (+ (* x r) 4.5921953931549871457e4)]
         [x  (+ (* x r) 1.3731693765509461125e4)]
         [x  (+ (* x r) 1.9715909503065514427e3)]
         [x  (+ (* x r) 1.3314166789178437745e2)]
         [x  (+ (* x r) 3.3871328727963666080e0)]
         [y  5.2264952788528545610e3]
         [y  (+ (* y r) 2.8729085735721942674e4)]
         [y  (+ (* y r) 3.9307895800092710610e4)]
         [y  (+ (* y r) 2.1213794301586595867e4)]
         [y  (+ (* y r) 5.3941960214247511077e3)]
         [y  (+ (* y r) 6.8718700749205790830e2)]
         [y  (+ (* y r) 4.2313330701600911252e1)]
         [y  (+ (* y r) 1.0)])
    (/ x y)))

(: R2 (Float -> Float))
;; Returns the degree-7 minimax rational approximation for r <= 5
(define (R2 r)
  (let* ([r  (- r 1.6)]
         [x  7.7454501427834140764e-4]
         [x  (+ (* x r) 2.27238449892691845833e-2)]
         [x  (+ (* x r) 2.41780725177450611770e-1)]
         [x  (+ (* x r) 1.27045825245236838258e0)]
         [x  (+ (* x r) 3.64784832476320460504e0)]
         [x  (+ (* x r) 5.76949722146069140550e0)]
         [x  (+ (* x r) 4.63033784615654529590e0)]
         [x  (+ (* x r) 1.42343711074968357734e0)]
         [y  1.05075007164441684324e-9]
         [y  (+ (* y r) 5.47593808499534494600e-4)]
         [y  (+ (* y r) 1.51986665636164571966e-2)]
         [y  (+ (* y r) 1.48103976427480074590e-1)]
         [y  (+ (* y r) 6.89767334985100004550e-1)]
         [y  (+ (* y r) 1.67638483018380384940e0)]
         [y  (+ (* y r) 2.05319162663775882187e0)]
         [y  (+ (* y r) 1.0)])
    (/ x y)))

(: R3 (Float -> Float))
;; Returns the degree-7 minimax rational approximation for r > 5
(define (R3 r)
  (let* ([r  (- r 5.0)]
         [x  2.01033439929228813265e-7]
         [x  (+ (* x r) 2.71155556874348757815e-5)]
         [x  (+ (* x r) 1.24266094738807843860e-3)]
         [x  (+ (* x r) 2.65321895265761230930e-2)]
         [x  (+ (* x r) 2.96560571828504891230e-1)]
         [x  (+ (* x r) 1.78482653991729133580e0)]
         [x  (+ (* x r) 5.46378491116411436990e0)]
         [x  (+ (* x r) 6.65790464350110377720e0)]
         [y  2.04426310338993978564e-15]
         [y  (+ (* y r) 1.42151175831644588870e-7)]
         [y  (+ (* y r) 1.84631831751005468180e-5)]
         [y  (+ (* y r) 7.86869131145613259100e-4)]
         [y  (+ (* y r) 1.48753612908506148525e-2)]
         [y  (+ (* y r) 1.36929880922735805310e-1)]
         [y  (+ (* y r) 5.99832206555887937690e-1)]
         [y  (+ (* y r) 1.0)])
    (/ x y)))

(: standard-flnormal-inv-cdf (Float -> Float))
;; Assumes 0.0 <= p <= 1.0
(define (standard-flnormal-inv-cdf p)
  (cond [(= p 0.0)  -inf.0]
        [(= p 1.0)  +inf.0]
        [else
         (define q (- p 0.5))
         (cond [((abs q) . <= . 0.425)  (* q (R1 q))]
               [else
                (define r (flsqrt (- (fllog (min p (- 1.0 p))))))
                (cond [(r . <= . 5.0)  (* (flsgn q) (R2 r))]
                      [else  (* (flsgn q) (R3 r))])])]))

;; ===================================================================================================
;; Inverse log cdf

(: inv-log-cdf-huge (Float -> Float))
;; Computes the simplest asymptotic expansion; good for log-p < -1e18 (and probably nearer zero)
(define (inv-log-cdf-huge log-p)
  (* (- (flsqrt 2.0)) (flsqrt (- log-p))))

;; More correct than what the FPU comes up with:
(define logsqrt4pi 1.2655121234846454)
(define logsqrtpi-1 -0.4276350570752999)

(: inv-log-cdf-big (Float -> Float))
;; Computes an asymptotic expansion; good for log-p <= -2000 (and probably a bit nearer zero)
;; It's *better* than R3 around log-p <= -925.0, though not as accurate as we'd like
(define (inv-log-cdf-big log-p)
  (let ([log-p  (- log-p)])
    (define t (* 0.5 (fllog (- log-p (fllog 2.0)))))
    (define u (- log-p t logsqrt4pi))
    (define v (+ t logsqrtpi-1))
    (define a2 (* #i1/4 v))
    (define a3 (+ #i3/16 (* v (+ #i-3/8 (* #i-1/8 v)))))
    (define a4 (+ #i-25/32 (* v (+ #i9/16 (* v (+ #i9/32 (* #i1/12 v)))))))
    (* (- (flsqrt 2.0))
       (+ (flsqrt u)
          (* a2 (flexpt u -1.5))
          (* a3 (flexpt u -2.5))
          (* a4 (flexpt u -3.5))))))

(: newton-log-iter (Float Float -> Float))
(define (newton-log-iter log-p x)
  (define real-log-p (standard-flnormal-log-cdf x))
  (cond [(log-p . < . real-log-p)
         (define dx (exp (fllog/ (fllog- real-log-p log-p)
                                 (standard-flnormal-log-pdf x))))
         (- x dx)]
        [else
         (define dx (exp (fllog/ (fllog- log-p real-log-p)
                                 (standard-flnormal-log-pdf x))))
         (+ x dx)]))

(: standard-flnormal-inv-log-cdf (Float -> Float))
;; Assumes -inf.0 <= log-p <= 0.0
(define (standard-flnormal-inv-log-cdf log-p)
  (cond [(= log-p -inf.0)  -inf.0]
        [(= log-p 0.0)     +inf.0]
        [else
         (cond [(<= (fllog 0.075) log-p (fllog 0.925))
                (define q (- (flexp log-p) 0.5))
                (* q (R1 q))]
               [(log-p . >= . (fllog 0.5))
                (define r (flsqrt (- (fllog1- log-p))))
                (if (r . <= . 5.0) (R2 r) (R3 r))]
               [(log-p . < . -1e18)
                (inv-log-cdf-huge log-p)]
               [(log-p . < . -20000.0)
                (inv-log-cdf-big log-p)]
               [(log-p . < . -925.0)
                (newton-log-iter log-p (inv-log-cdf-big log-p))]
               [else
                (define r (flsqrt (- log-p)))
                (define x (if (r . <= . 5.0) (- (R2 r)) (- (R3 r))))
                (cond [(log-p . < . -744.0)  (newton-log-iter log-p x)]
                      [else  x])])]))
