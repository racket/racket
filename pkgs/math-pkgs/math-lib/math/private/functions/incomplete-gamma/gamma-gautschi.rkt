#lang typed/racket/base

;; Gautschi's algorithm (as presented by Temme) for the regularized upper gamma for x < 1

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "../../polynomial/chebyshev.rkt"
         "../gamma.rkt"
         "../log-gamma.rkt"
         "../stirling-error.rkt"
         "gamma-utils.rkt")

(provide flgamma-upper-gautschi flgamma-lower-gautschi)

(: fl1-1/gamma1p-taylor-0 (Float -> Float))
(define (fl1-1/gamma1p-taylor-0 x)
  (fl* x ((make-flpolyfun
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
  (fl* (fl- x 1.0)
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
        (fl- x 1.0))))

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
  (cond [(x . fl< . 0.3)  (fl1-1/gamma1p-taylor-0 x)]
        [(x . fl< . 0.7)  (fl1-1/gamma1p-0.3-0.7 x)]
        [else  (fl1-1/gamma1p-taylor-1 x)]))

(: flgamma-gautschi-iter (Float Float -> Float))
;; Calculates the series part of Gautschi's algorithm
(define (flgamma-gautschi-iter k x)
  (let loop ([p  (fl* k x)] [q  (fl+ k 1.0)] [r  (fl+ k 3.0)] [t  1.0] [v  1.0])
    (cond [((flabs t) . fl<= . (flabs (fl* (fl* 0.5 epsilon.0) v)))  v]
          [else  (let* ([p  (fl+ p x)]
                        [q  (fl+ q r)]
                        [r  (fl+ r 2.0)]
                        [t  (fl/ (fl* (- p) t) q)]
                        [v  (fl+ v t)])
                   (loop p q r t v))])))

(: lg1-prod (Float Float -> Float))
;; Calculates (lg1- (* k A)) in a way that maintains precision when k or A is very small
(define (lg1-prod k A)
  (define kA (fl* k A))
  (cond [((flabs kA) . fl< . 1e-300)
         (let-values ([(k A)  (if ((flabs k) . fl< . (flabs A)) (values k A) (values A k))])
           (fl- (lg1- (fl* (fl* (flexpt 2.0 80.0) k) A))
                ;; Approximates sum_i=1^80 (fllog1p (exp (* (flexpt 2.0 i) k A)))
                ;; (hint: (exp (* (flexpt 2.0 i) k A)) ~= 1 here)
                (fllog (flexpt 2.0 80.0))))]
        [else
         (lg1- kA)]))

(: flgamma-upper-gautschi (Float Float Any -> Float))
;; Temme's implementation of Gautschi's series for upper gamma, extended to compute logs
(define (flgamma-upper-gautschi k x log?)
  (cond [log?
         (define p (flgamma-upper-gautschi k x #f))
         (cond
           [(and (p . > . +max-subnormal.0) (p . < . 0.8))  (fllog p)]
           [(k . < . 1e-32)
            (define y (flgamma-gautschi-iter k x))
            (+ (fllog k) (fllog (- (* x y) (+ (fllog x) gamma.0))))]
           [(k . < . 1e-16)
            (define y (flgamma-gautschi-iter k x))
            (lg1- (+ (fllog1p (* k (- 1.0 (* x y))))
                     (* k (+ (fllog x) gamma.0 -1.0))))]
           [else
            (fllog1p (- (flgamma-lower-gautschi k x #f)))])]
        [(k . < . 1e-16)
         (define y (flgamma-gautschi-iter k x))
         ;; Here, log(gamma(k)) ~ -log(k)-gamma*k, and log1p(k) ~ k
         (- (flexpm1 (+ (fllog1p (* k (- 1.0 (* x y))))
                        (* k (+ (fllog x) gamma.0 -1.0)))))]
        [else
         (define y (flgamma-gautschi-iter k x))
         (define s (fl1-1/gamma1p k))
         (define u (fl- s (fl* (flexpm1 (fl* k (fllog x))) (fl- 1.0 s))))
         (define v (fl/ (fl* (fl* (fl* k (fl- 1.0 s))
                                  (flexp (fl* (fl+ k 1.0) (fllog x))))
                             y)
                        (fl+ k 1.0)))
         (fl+ u v)]))

(: flgamma-lower-gautschi (Float Float Any -> Float))
;; Gautschi's series for upper gamma, altered to compute lower
(define (flgamma-lower-gautschi k x log?)
  (cond [log?
         (define p (flgamma-lower-gautschi k x #f))
         (cond
           [(and (p . > . +max-subnormal.0) (p . < . 0.8))  (fllog p)]
           [else  (fllog1p (- (flgamma-upper-gautschi k x #f)))])]
        [(k . < . 1e-16)
         (define y (flgamma-gautschi-iter k x))
         ;; Here, log(gamma(k)) ~ -log(k)-gamma*k, and log1p(k) ~ k
         (flexp (+ (fllog1p (* k (- 1.0 (* x y))))
                   (* k (+ (fllog x) gamma.0 -1.0))))]
        [else
         (define y (flgamma-gautschi-iter k x))
         (define 1-s (fl/ 1.0 (fl* k (flgamma k))))
         (define 1-u (fl* 1-s (flexpt x k)))
         (define v (fl/ (fl* (fl* (fl* k 1-s)
                                  (flexp (fl* (fl+ k 1.0) (fllog x))))
                             y)
                        (fl+ k 1.0)))
         (fl- 1-u v)]))
