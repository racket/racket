#lang typed/racket/base
#|
#lang racket

(module defs typed/racket/base
|#
#|
Algorithms taken from:

N M Temme. A Set of Algorithms for the Incomplete Gamma Functions.
Probability in the Engineering and Informational Sciences, 1994, vol. 8, pp. 291--307.

For certain algorithms for a few subdomains of RxR, the above paper references this one:

W Gautschi. A Computational Procedure for Incomplete Gamma Functions.
ACM Transactions on Mathematical Software, 1979, vol. 5, pp. 466--481.

This implementation extends those in the papers in three ways:

 * Results are more correct, generally with relative error <= 1e-14.

 * Log-space results.
 
 * The functions return correct rational answers on the largest domain possible.
|#

(require "../../flonum.rkt"
         "../../constants.rkt"
         "../../vector.rkt"
         "../polynomial/chebyshev.rkt"
         "../distributions/impl/normal-cdf.rkt"
         "continued-fraction.rkt"
         "log1p.rkt"
         "expm1.rkt"
         "gamma.rkt"
         "gammastar.rkt"
         "log-gamma.rkt"
         "log-arithmetic.rkt"
         "polyfun.rkt")
;(provide (all-defined-out))
(provide flgamma-lower
         flgamma-upper
         flgamma-lower-regularized
         flgamma-upper-regularized
         fllog-gamma-lower
         fllog-gamma-upper
         fllog-gamma-lower-regularized
         fllog-gamma-upper-regularized
         gamma-lower
         gamma-upper
         gamma-lower-regularized
         gamma-upper-regularized
         log-gamma-lower
         log-gamma-upper
         log-gamma-lower-regularized
         log-gamma-upper-regularized)

(define sqrt2pi 2.5066282746310007)
(define logsqrt2pi 0.9189385332046728)

;; Debugging parameters
(define use-normal-appx? (make-parameter #t))
(define use-log-appx? (make-parameter #t))
(define use-temme-appx? (make-parameter #t))
(define use-gautschi-appx? (make-parameter #t))

(define temme-upper-thresh (make-parameter 2.0))
(define temme-lower-thresh (make-parameter 6.0))
(define temme-iters (make-parameter 32))

;; The following predicates are applied in order to determine which approximation to use

(: use-normal? (Float Float Any Any -> Boolean))
;; Determines whether to use the normal cdf approximation
(define (use-normal? k x log? upper?)
  (and (use-normal-appx?)
       (k . > . 1e10)
       (cond [log?  (or (if upper? (x . < . k) (x . > . k))
                        (k . > . 1e32))]
             [else  #t])))

(: use-log? (Float Float -> Boolean))
;; Determines whether to compute incomplete gamma functions in log space; i.e. returns #t when
;; computing either incomplete or complete gammas might overflow
(define (use-log? k x)
  (and (use-log-appx?)
       (or (k . < . 1e-20) ((+ x k) . > . 170.0))))

(: use-temme? (Float Float -> Boolean))
;; Determines whether to use Temme's series; i.e. returns #t when k and x are close
(define (use-temme? k x)
  (and (use-temme-appx?)
       (k . > . 10.0)
       (or (and (x . >= . k)
                ((/ (abs (- x k)) k) . < . (temme-upper-thresh)))
           (and (x . < . k)
                ((/ (abs (- x k)) x) . < . (temme-lower-thresh))))))

(: use-gautschi? (Float Float Any -> Boolean))
;; Determines whether to use Gautschi's series; i.e. returns #t near the unit square at the origin
(define (use-gautschi? k x log?)
  (and (use-gautschi-appx?)
       (cond [log?  (and (x . < . 1.0)
                         (or (x . >= . k) (k . < . 0.9))
                         (not (and (= k +min.0) (x . > . 0.01))))]
             [else  (and (x . < . 1.5)
                         (or (x . >= . k) (k . < . 0.9)))])))

(: use-lower? (Float Float -> Boolean))
;; Determines whether to compute incomplete gamma functions using a lower gamma function; i.e.
;; returns #t in the lower half of quadrant 1
(define (use-lower? k x)
  (x . < . k))

;; ===================================================================================================
;; Normal approximation for regularized incomplete gamma functions
;; Using Temme's because it's much better than the standard one

(: flgamma-regularized-normal (Float Float Any Any -> Float))
(define (flgamma-regularized-normal k x log? upper?)
  (define l (/ x k))
  (define norm-x
    (cond [(or (l . < . epsilon.0) (l . > . (/ 1.0 epsilon.0)))
           ;; Avoid under-/overflow in calculating norm-x by doing it in log space
           (define log-l (- (fllog x) (fllog k)))
           (define l-1 (flexpm1 log-l))
           (define l-1-sign (flsgn l-1))
           (define log-n (* 0.5 (+ (fllog 2.0) (fllog (- l-1 log-l)))))
           (* l-1-sign (exp (+ log-n (* 0.5 (fllog k)))))]
          [else
           (define n (* (flsgn (- l 1.0)) (flsqrt (* 2.0 (- (- l 1.0) (fllog l))))))
           (* n (flsqrt k))]))
  (let ([norm-x  (if upper? (- norm-x) norm-x)])
    (cond [log?  (standard-flnormal-log-cdf norm-x)]
          [else  (standard-flnormal-cdf norm-x)])))

;; ===================================================================================================
;; Lower gamma series (found by unfolding its recursive characterization infinitely)

(: flgamma-lower-iter (Float Float -> Float))
(define (flgamma-lower-iter k x)
  (let: loop : Float ([y  : Float  0.0]
                      [dy : Float  (/ x (+ k 1.0))]
                      [i  : Float  0.0])
    (define new-y (+ y dy))
    (cond [(or (not (rational? new-y))
               ((abs dy) . <= . (abs (* epsilon.0 new-y))))
           new-y]
          [else
           (loop new-y (/ (* dy x) (+ 2.0 i k)) (+ i 1.0))])))

(: flgamma-lower-series (Float Float -> Float))
(define (flgamma-lower-series k x)
  (define y (flgamma-lower-iter k x))
  (define z (exp (- (- (* k (fllog x)) x)
                    (+ (fllog k) (fllog-gamma k)))))
  (cond [(y . < . 1.0)  (+ z (* z y))]  ; avoid adding 1.0 if y is near zero
        [else  (* z (+ y 1.0))]))

(: fllog-gamma-lower-series (Float Float -> Float))
(define (fllog-gamma-lower-series k x)
  (define y (flgamma-lower-iter k x))
  (define log-z (- (- (* k (fllog x)) x)
                   (+ (fllog k) (fllog-gamma k))))
  (+ log-z (fllog1p y)))

;; ===================================================================================================
;; Upper gamma continued fraction (Legendre's)

(: flgamma-upper-iter (Float Float -> Float))
(define (flgamma-upper-iter k x)
  (continued-fraction 1.0
                      (λ (i a) (* i (- k i)))
                      (- (+ x 1.0) k)
                      (λ (i b) (+ b 2.0))
                      epsilon.0))

(: flgamma-upper-frac (Float Float -> Float))
(define (flgamma-upper-frac k x)
  (define y (flgamma-upper-iter k x))
  (define z (exp (- (- (* k (fllog x)) x)
                    (fllog-gamma k))))
  (* y z))

(: fllog-gamma-upper-frac (Float Float -> Float))
(define (fllog-gamma-upper-frac k x)
  (define y (flgamma-upper-iter k x))
  (define log-z (- (- (* k (fllog x)) x)
                   (fllog-gamma k)))
  (+ log-z (fllog y)))

;; ===================================================================================================
;; Gautschi's algorithm (as presented by Temme) for the regularized upper gamma for x < 1

(: fl1-1/gamma1p-taylor-0 (Float -> Float))
(define (fl1-1/gamma1p-taylor-0 x)
  (* x ((make-flpolyfun
         (-5.7721566490153286060651209008240243104215933593992e-1
          +6.5587807152025388107701951514539048127976638047858e-1
          +4.2002635034095235529003934875429818711394500401104e-2
          -1.6653861138229148950170079510210523571778150224717e-1
          +4.219773455554433674820830128918739130165268418982e-2
          +9.6219715278769735621149216723481989753629422521117e-3
          -7.2189432466630995423950103404465727099048008802391e-3
          +1.1651675918590651121139710840183886668093337953828e-3
          +2.1524167411495097281572996305364780647824192337778e-4
          -1.2805028238811618615319862632816432339489209969465e-4
          +2.0134854780788238655689391421021818382294833296229e-5
          +1.2504934821426706573453594738330922423226556201567e-6
          -1.1330272319816958823741296203307449433240048389228e-6
          +2.0563384169776071034501541300205728365125790182239e-7
          -6.1160951044814158178624986828553428672758673305935e-9))
        x)))

(: fl1-1/gamma1p-taylor-1 (Float -> Float))
(define (fl1-1/gamma1p-taylor-1 x)
  (* (- x 1.0)
     ((make-flpolyfun
       (+4.2278433509846713939348790991759756895784066406008e-1
        +2.3309373642178674168353160522779291232192571641851e-1
        -1.910911013876915061545276703523630936105312160174e-1
        +2.4552490005400016652826875250257857892749713770226e-2
        +1.7645244550144320095381426038929533408902970419595e-2
        -8.0232730222673465332665043665813344335400281674833e-3
        +8.0432977560424699087149402613476172363522728724358e-4
        +3.6083781625481812124247705788362694317410650813711e-4
        -1.4559614213986714842674709482997913669586458476042e-4
        +1.7545859751750962273548468501814813300972485066968e-5
        +2.5889950290372763821409229192070050813223482271545e-6
        -1.3385015468946057247955634453739128389996926090334e-6
        +2.0547431491290984242143382504316789567568776464973e-7
        +1.5952678485086792358158795888938797557013266594503e-10
        -6.2756218893322837414440866417447308428460020476605e-9))
      (- x 1.0))))

(define fl1-1/gamma1p-0.3-0.7
  (inline-chebyshev-flpoly-fun
   0.3 0.7
   (-0.2357535799644732185145165985112978133459
    0.007192709283280881736611459461064511206541
    0.01049211807832747501559721918849629071077
    -3.459974677274507288096516749252814460585e-4
    -1.027210542517651843057394564412174832553e-5
    8.401234394534847347740864823467611880071e-7
    -1.304830795670152306426264416729164660901e-8
    -4.268734425630906391429002996659143191633e-10
    2.216361071246008579047775791962841508693e-11
    -3.016588637335897973295510846778736973194e-13
    -5.122627533431577404793896156393660398889e-15
    2.777819095314707020162876992918284024147e-16
    -4.289212074552049664300497789553082042299e-18)))

(: fl1-1/gamma1p (Float -> Float))
;; Computes 1-1/gamma(x+1); relative error <= 2*eps
(define (fl1-1/gamma1p x)
  (cond [(x . < . 0.3)  (fl1-1/gamma1p-taylor-0 x)]
        [(x . < . 0.7)  (fl1-1/gamma1p-0.3-0.7 x)]
        [else  (fl1-1/gamma1p-taylor-1 x)]))
  
(: flgamma-gautschi-iter (Float Float -> Float))
;; Calculates the series part of Gautschi's algorithm
(define (flgamma-gautschi-iter k x)
  (let loop ([p  (* k x)] [q  (+ k 1.0)] [r  (+ k 3.0)] [t  1.0] [v  1.0])
    (cond [((abs t) . <= . (abs (* epsilon.0 v)))  v]
          [else  (let* ([p  (+ p x)] [q  (+ q r)] [r  (+ r 2.0)] [t  (/ (* (- p) t) q)] [v  (+ v t)])
                   (loop p q r t v))])))

(: fllog1-prod (Float Float -> Float))
;; Calculates (fllog1- (* k A)) in a way that maintains precision when k or A is very small
(define (fllog1-prod k A)
  (define kA (* k A))
  (cond [((abs kA) . < . 1e-300)
         (let-values ([(k A)  (if ((abs k) . < . (abs A)) (values k A) (values A k))])
           (- (fllog1- (* (* (flexpt 2.0 80.0) k) A))
              ;; Approximates sum_i=1^80 (fllog1p (exp (* (flexpt 2.0 i) k A)))
              ;; (hint: (exp (* (flexpt 2.0 i) k A)) ~= 1 here)
              (fllog (flexpt 2.0 80.0))))]
        [else
         (fllog1- kA)]))

(: flgamma-upper-regularized-gautschi (Float Float Any -> Float))
;; Temme's implementation of Gautschi's series for upper gamma, extended to compute logs
(define (flgamma-upper-regularized-gautschi k x log?)
  (define y (flgamma-gautschi-iter k x))
  (cond [log?
         (define s (fl1-1/gamma1p k))
         ;; Divide by k (by dividing by (flsqrt k) twice) to try to keep some precision when k is very
         ;; small: the computation (* k (fllog x)) loses a lot of bits otherwise
         (define A (/ (+ (- (* (fllog x) (flsqrt k))
                            (/ (fllog1p k) (flsqrt k)))
                         (/ (fllog1p (- s)) (flsqrt k)))
                      (flsqrt k)))
         (fllog- (fllog1-prod k A)
                 (+ (fllog k) (+ (- (* k (fllog x)) (fllog1p k))
                                 (fllog (+ (* s x y) (- (* x y)) (- s) 1.0)))))]
        [else
         (define s (fl1-1/gamma1p k))
         (define u (- s (* (flexpm1 (* k (fllog x))) (- 1.0 s))))
         (define v (/ (* k (- 1.0 s) (exp (* (+ k 1.0) (fllog x))) y) (+ k 1.0)))
         (+ u v)]))

(: flgamma-lower-regularized-gautschi (Float Float Any -> Float))
;; Gautschi's series for upper gamma, altered to compute lower
(define (flgamma-lower-regularized-gautschi k x log?)
  (define y (flgamma-gautschi-iter k x))
  (cond [log?
         ;; Catastrophic cancellation in log-1-s around 1e-16..2e-14: 
         #|
         (define log-1-s (- (+ (fllog k) (fllog-gamma k))))
         (define log-1-u (+ log-1-s (* k (fllog x))))
         (define log-v (- (+ (fllog k) log-1-s (* (+ k 1.0) (fllog x)) (fllog y))
                          (fllog1p k)))
         (fllog- log-1-u log-v)
|#
         (fllog1- (flgamma-upper-regularized-gautschi k x #t))]
        [else
         (define 1-s (/ 1.0 (* k (flgamma k))))
         (define 1-u (* 1-s (flexpt x k)))
         (define v (/ (* k 1-s (exp (* (+ k 1.0) (fllog x))) y) (+ k 1.0)))
         (- 1-u v)]))

;; ===================================================================================================
;; Temme's series for the incomplete gamma functions (used when k ~ x and k is not small)

(define num-fs 100)
(define fs
  (let ()
    (define: start-fs : (Vectorof Real)  (vector 1 -1/3 1/12 -2/135))
    (define: fs : (Vectorof Real)  (make-vector num-fs 0))
    (vector-copy! fs 0 start-fs)
    ;; DP algorithm to compute f coefficients
    (for ([m  (in-range 4 num-fs)])
      (vector-set!
       fs m
       (* (- (/ (+ m 1) (+ m 2)))
          (+ (* (/ (- m 1) (* 3 m)) (vector-ref fs (- m 1)))
             (for/fold: ([sum : Real  0]) ([j  (in-range 3 m)])
               (+ sum (/ (* (vector-ref fs (- j 1)) (vector-ref fs (+ m 1 (- j))))
                         (+ m 2 (- j)))))))))
    (vector->flvector fs)))

(: R-sum (Float Float -> Float))
(define (R-sum k n)
  (define num-fs (temme-iters))
  ;; This originally filled a vector of bs, because imperative programmers don't know how to do
  ;; anything else besides bang an array full of values (sheesh)
  (define-values (sum b2 b1)
    (for/fold: ([sum : Float  0.0]
                [b2 : Float  (flvector-ref fs (- num-fs 1))]
                [b1 : Float  (flvector-ref fs (- num-fs 2))]
                ) ([m  (in-range (- num-fs 3) 0 -1)])
      (define b0 (+ (flvector-ref fs m) (/ (* (+ (->fl m) 1.0) b2) k)))
      (values (+ (* n sum) b0) b1 b0)))
  sum)

(: R (Float Float -> Float))
(define (R k n)
  (/ (* (R-sum k n) (exp (* -0.5 k n n)))
     (* (flsqrt (* 2.0 pi.0 k)) (flgamma* k))))

(: R-log (Float Float -> (Values Float Float)))
;; Log-space version of `R' above
(define (R-log k n)
  (define sum (R-sum k n))
  (values
   (- (+ (fllog (abs sum)) (* -0.5 k n n))
      (* 0.5 (fllog k))
      (* 0.5 (fllog (* 2.0 pi.0)))
      (fllog-gamma* k))
   (flsgn sum)))

(: flgamma-regularized-temme (Float Float Any -> Float))
;; Computes a regularized incomplete gamma using Temme's series
(define (flgamma-regularized-temme k x upper?)
  (define l (/ x k))
  (define n (* (flsgn (- l 1.0)) (flsqrt (* 2.0 (- (- l 1.0) (fllog l))))))
  (define norm-x (let ([norm-x  (* n (flsqrt k))])
                   (if upper? (- norm-x) norm-x)))
  (define r (let ([r  (R k n)])
              (if upper? (- r) r)))
  (cond [(norm-x . <= . 0.0)  (- (standard-flnormal-cdf norm-x) r)]
        [else  (- 1.0 (+ (standard-flnormal-cdf (- norm-x)) r))]))

(: fllog-gamma-regularized-temme (Float Float Any -> Float))
(define (fllog-gamma-regularized-temme k x upper?)
  (define l (/ x k))
  (define n (* (flsgn (- l 1.0)) (flsqrt (* 2.0 (- (- l 1.0) (fllog l))))))
  (define norm-x (let ([norm-x  (* n (flsqrt k))])
                   (if upper? (- norm-x) norm-x)))
  (define-values (log-r r-sgn) (let-values ([(log-r r-sgn)  (R-log k n)])
                                 (if upper? (values log-r (- r-sgn)) (values log-r r-sgn))))
  (cond [(norm-x . <= . 0.0)
         (define norm-log-p (standard-flnormal-log-cdf norm-x))
         (define log-p (if (r-sgn . < . 0.0)
                           (fllog+ norm-log-p log-r)
                           (fllog- norm-log-p log-r)))
         ;; When norm-log-p ~ log-r, the above log-space arithmetic can go bad
         ;; Fortunately, this means we don't need any correctional terms - a normal approximation is
         ;; good enough
         (if (rational? log-p) log-p norm-log-p)]
        [else
         (define norm-log-p (standard-flnormal-log-cdf (- norm-x)))
         (define log-p (if (r-sgn . < . 0.0)
                           (fllog1- (fllog- norm-log-p log-r))
                           (fllog1- (fllog+ norm-log-p log-r))))
         (if (rational? log-p) log-p (fllog1- norm-log-p))]))

;; ===================================================================================================
;; Regularized incomplete gamma functions

(: fllog-gamma-lower-regularized (Float Float -> Float))
(define (fllog-gamma-lower-regularized k x)
  (cond [(or (k . < . 0.0) (x . < . 0.0))  +nan.0]
        [(and (k . > . 0.0) (k . < . +inf.0) (x . > . 0.0) (x . < . +inf.0))
         (cond [(use-normal? k x #t #f)  (flgamma-regularized-normal k x #t #f)]
               [(use-temme? k x)  (fllog-gamma-regularized-temme k x #f)]
               [(use-gautschi? k x #t)  (flgamma-lower-regularized-gautschi k x #t)]
               [(use-lower? k x)  (fllog-gamma-lower-series k x)]
               [else  (fllog1- (fllog-gamma-upper-frac k x))])]
        ;; k = +inf.0: a step function with the step "at infinity"
        [(= k +inf.0)  (if (= x +inf.0) (fllog 1.0) (fllog 0.0))]
        ;; k = 0.0: a step function with the step at 0.0
        [(= k 0.0)  (fllog 1.0)]
        [(= x 0.0)  (fllog 0.0)]
        [(= x +inf.0)  (fllog 1.0)]
        ;; k is +nan.0 and x is +nan.0
        [else  +nan.0]))

(: fllog-gamma-upper-regularized (Float Float -> Float))
(define (fllog-gamma-upper-regularized k x)
  (cond [(or (k . < . 0.0) (x . < . 0.0))  +nan.0]
        [(and (k . > . 0.0) (k . < . +inf.0) (x . > . 0.0) (x . < . +inf.0))
         (cond [(or (and (k . >= . 1.0) ((/ x k) . > . 1e19))
                    (and (k . < . 1.0) (x . > . 1e19)))
                (- x)]
               [(use-normal? k x #t #t)  (flgamma-regularized-normal k x #t #t)]
               [(use-temme? k x)  (fllog-gamma-regularized-temme k x #t)]
               [(use-gautschi? k x #t)  (flgamma-upper-regularized-gautschi k x #t)]
               [(use-lower? k x)  (fllog1- (fllog-gamma-lower-series k x))]
               [else  (fllog-gamma-upper-frac k x)])]
        ;; k = +inf.0: a step function with the step "at infinity"
        [(= k +inf.0)  (if (= x +inf.0) (fllog 0.0) (fllog 1.0))]
        ;; k = 0.0: a step function with the step at 0.0
        [(= k 0.0)  (fllog 0.0)]
        [(= x 0.0)  (fllog 1.0)]
        [(= x +inf.0)  (fllog 0.0)]
        ;; k is +nan.0 and x is +nan.0
        [else  +nan.0]))

(: flgamma-lower-regularized (Float Float -> Float))
(define (flgamma-lower-regularized k x)
  (cond [(or (k . < . 0.0) (x . < . 0.0))  +nan.0]
        [(and (k . > . 0.0) (k . < . +inf.0) (x . > . 0.0) (x . < . +inf.0))
         (cond [(k . < . 1e-20)  1.0]
               [(use-normal? k x #f #f)  (flgamma-regularized-normal k x #f #f)]
               [(use-temme? k x)  (flgamma-regularized-temme k x #f)]
               [(use-gautschi? k x #f)  (flgamma-lower-regularized-gautschi k x #f)]
               [(use-log? k x)  (flexp (fllog-gamma-lower-regularized k x))]
               [(use-lower? k x)  (flgamma-lower-series k x)]
               [else  (- 1.0 (flgamma-upper-frac k x))])]
        ;; k = +inf.0: a step function with the step "at infinity"
        [(= k +inf.0)  (if (= x +inf.0) 1.0 0.0)]
        ;; k = 0.0: a step function with the step at 0.0
        [(= k 0.0)  1.0]
        [(= x 0.0)  0.0]
        [(= x +inf.0)  1.0]
        ;; k is +nan.0 and x is +nan.0
        [else  +nan.0]))

(: flgamma-upper-regularized (Float Float -> Float))
(define (flgamma-upper-regularized k x)
  (cond [(or (k . < . 0.0) (x . < . 0.0))  +nan.0]
        [(and (k . > . 0.0) (k . < . +inf.0) (x . > . 0.0) (x . < . +inf.0))
         (cond [(use-normal? k x #f #t)  (flgamma-regularized-normal k x #f #t)]
               [(use-temme? k x)  (flgamma-regularized-temme k x #t)]
               [(use-gautschi? k x #f)  (flgamma-upper-regularized-gautschi k x #f)]
               [(use-log? k x)  (flexp (fllog-gamma-upper-regularized k x))]
               [(use-lower? k x)  (- 1.0 (flgamma-lower-series k x))]
               [else  (flgamma-upper-frac k x)])]
        ;; k = +inf.0: a step function with the step "at infinity"
        [(= k +inf.0)  (if (= x +inf.0) 0.0 1.0)]
        ;; k = 0.0: a step function with the step at 0.0
        [(= k 0.0)  0.0]
        [(= x 0.0)  1.0]
        [(= x +inf.0)  0.0]
        ;; k is +nan.0 and x is +nan.0
        [else  +nan.0]))

;; ===================================================================================================
;; Unregularized incomplete gamma functions

(: fllog-gamma-lower (Float Float -> Float))
(define (fllog-gamma-lower k x)
  (+ (fllog-gamma-lower-regularized k x) (fllog-gamma k)))

(: fllog-gamma-upper (Float Float -> Float))
(define (fllog-gamma-upper k x)
  (+ (fllog-gamma-upper-regularized k x) (fllog-gamma k)))

(: flgamma-lower (Float Float -> Float))
(define (flgamma-lower k x)
  (cond [(use-log? k x)  (exp (fllog-gamma-lower k x))]
        [else  (* (flgamma-lower-regularized k x) (flgamma k))]))

(: flgamma-upper (Float Float -> Float))
(define (flgamma-upper k x)
  (cond [(use-log? k x)  (exp (fllog-gamma-upper k x))]
        [else  (* (flgamma-upper-regularized k x) (flgamma k))]))

;; ===================================================================================================
;; Wrappers for functions that accept reals

(define-syntax-rule (define-incomplete-gamma-wrapper name flname)
  (begin
    (: name (case-> (Single-Flonum Single-Flonum -> Single-Flonum)
                    (Float Float -> Float)
                    (Real Real -> Real)))
    (define (name k x)
      (cond [(and (double-flonum? k) (double-flonum? x))  (flname x k)]
            [(and (single-flonum? k) (single-flonum? x))
             (real->single-flonum (flname (real->double-flonum k) (real->double-flonum x)))]
            [else
             (flname (real->double-flonum k) (real->double-flonum x))]))))

(define-incomplete-gamma-wrapper gamma-lower flgamma-lower)
(define-incomplete-gamma-wrapper gamma-upper flgamma-upper)
(define-incomplete-gamma-wrapper gamma-lower-regularized flgamma-lower-regularized)
(define-incomplete-gamma-wrapper gamma-upper-regularized flgamma-upper-regularized)
(define-incomplete-gamma-wrapper log-gamma-lower fllog-gamma-lower)
(define-incomplete-gamma-wrapper log-gamma-upper fllog-gamma-upper)
(define-incomplete-gamma-wrapper log-gamma-lower-regularized fllog-gamma-lower-regularized)
(define-incomplete-gamma-wrapper log-gamma-upper-regularized fllog-gamma-upper-regularized)
#|
)

(require plot
         'defs
         math/bigfloat
         "../../flonum.rkt"
         "../../constants.rkt"
         "../../vector.rkt"
         "../polynomial/chebyshev.rkt"
         "../distributions/impl/normal-cdf.rkt"
         "continued-fraction.rkt"
         "log1p.rkt"
         "expm1.rkt"
         "gamma.rkt"
         "gammastar.rkt"
         "log-gamma.rkt"
         "log-arithmetic.rkt"
         "polyfun.rkt")

(define h (make-hash))

(define (gamma-lower* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'lower (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bfgamma-lower (bf k) (bf x)))))))

(define (gamma-upper* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'upper (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bfgamma-upper (bf k) (bf x)))))))

(define (log-gamma-lower* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'log-lower (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bflog-gamma-lower (bf k) (bf x)))))))

(define (log-gamma-upper* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'log-upper (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bflog-gamma-upper (bf k) (bf x)))))))

(define (gamma-lower-regularized* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'lower-reg (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bfgamma-lower-regularized (bf k) (bf x)))))))

(define (gamma-upper-regularized* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'upper-reg (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bfgamma-upper-regularized (bf k) (bf x)))))))

(define (log-gamma-lower-regularized* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'log-lower-reg (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bflog-gamma-lower-regularized (bf k) (bf x)))))))

(define (log-gamma-upper-regularized* k x)
  (let ([k  (fl k)] (x  (fl x)))
    (hash-ref!
     h (list 'log-upper-reg (bf-precision) k x)
     (λ ()
       (printf "k = ~v  x = ~v~n" k x)
       (bigfloat->flonum (bflog-gamma-upper-regularized (bf k) (bf x)))))))
#;
(for* ([f+log-f  (in-list (list (cons fllog-gamma-lower-regularized
                                      flgamma-lower-regularized)
                                (cons fllog-gamma-upper-regularized
                                      flgamma-upper-regularized)))]
       [end  (in-list (list (flstep +min.0 41)
                            epsilon.0
                            1.0
                            10.0
                            200.0))])
  (define log-f (car f+log-f))
  (define f (cdr f+log-f))
  (printf "f = ~a; end = ~v~n" f end)
  (print
   (with-handlers ([exn?  (λ (e) e)])
     (plot3d (contour-intervals3d
              (λ (k x)
                (let ([k  (fl k)] [x  (fl x)])
                  (f k x)
                  #;
                  (relative-error (f k x) (exp (log-f k x)))))
              +min.0 end
              +min.0 end))))
  (newline))

#;
(plot3d
 (contour-intervals3d
  (λ (k x)
    (let ([k  (fl k)] [x  (fl x)])
      (relative-error
       (flgamma-lower-regularized k x)
       (gamma-lower-regularized* k x))))
  +min.0 epsilon.0
  +min.0 epsilon.0))

(plot3d
 (contour-intervals3d
  (λ (k x)
    (let ([k  (fl k)] [x  (fl x)])
      (fllog-gamma-lower-regularized k x)))
  1e-16 1e-15
  0 1.1))
#;
(plot3d
 (contour-intervals3d
  (λ (k x)
    (let ([k  (fl k)] [x  (fl x)])
      (relative-error
       (flgamma-lower-regularized k x)
       (gamma-lower-regularized* k x))))
  +min.0 epsilon.0
  +min.0 epsilon.0))
|#
