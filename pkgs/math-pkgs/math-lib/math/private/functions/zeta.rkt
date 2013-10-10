#lang typed/racket/base

#|
Algorithm for the majority of eta's domain from

P. Borwein. An Efficient Algorithm for the Riemann Zeta Function.
|#

(require racket/fixnum
         "../../flonum.rkt"
         "../../base.rkt"
         "../number-theory/factorial.rkt"
         "../number-theory/bernoulli.rkt"
         "../polynomial/chebyshev.rkt"
         "gamma.rkt"
         "stirling-error.rkt")

(provide fleta flzeta
         eta zeta)

(define +eta-max.0 53.99999999955306)
(define +zeta-max.0 53.000000000670404)

;; Error <= 1 ulp for 0 <= x <= 0.04
(define fleta-taylor-0
  (make-flpolyfun
   (0.5
    +2.25791352644727432363097614947441071785897339e-1
    -3.05145718840280522586220206685108673985433522e-2
    -3.91244691530275106760124536428014526534772501e-3
    +2.08483152359513300940797999527700394516178740e-3
    -3.12274485703922093874348990729689586909277588e-4
    +4.77866286231754699184106581473688337626826621e-6
    +6.81492916627787647157234076681326635240242752e-6)))

(define fleta-cheby-0-1
  (inline-chebyshev-flpoly-fun
   0.0 1.0
   (1.201451085000215786341948016934590750096
    0.09659235221993630986901377398183709103273
    -0.004162536123697811956855631042802249558704
    -1.823459118570897951644789418281880496971e-5
    1.057437661524516894212763161453537729375e-5
    -5.276357969271112185781725261857608168314e-7
    9.552883431770909233660820857569776676994e-9
    2.861275947684961245106758674027061705788e-10
    -2.592164434350750739588078107978177224236e-11
    8.915497281646523022154515860475428889141e-13
    -1.447759589742495761488938114822542491701e-14
    -1.629339340198539399979819526388023402444e-16
    1.846901346319333008905200116299163652954e-17)))

(define fleta-cheby-neg-1.5-0
  (inline-chebyshev-flpoly-fun
   -1.5 0.0
   (0.6249707175603271538769600167574752420857
    0.1919290294856821784120025035346336182627
    -0.003267513343418322858117903176882496211178
    -0.001266607295556663909608241164197267248528
    1.229333530745096620548304292135771666374e-4
    -2.895947350460977592953532943362896903995e-6
    -3.416581692876350748659539557978633940172e-7
    3.837933497475585903016599027301413075615e-8
    -1.775011660163000411058608111537020096243e-9
    1.823510907947069149164464718906140458455e-11
    3.270513023690372128256583347736563273382e-12
    -2.650468446042741339295818711051485655948e-13
    1.078303611477515364506213006324611739967e-14
    -1.971960766890138166024311496731213878603e-16
    -5.923220277992641912614957330192473539923e-18)))


(: make-ds (Positive-Index -> FlVector))
(define (make-ds n)
  (define ds
    (cdr
     (reverse
      (for/fold: ([lst : (Listof Real)  (list 0)]) ([i  (in-range (+ n 1))])
        (cons (+ (car lst)
                 (/ (* n (factorial (+ n i -1)) (expt 4 i))
                    (* (factorial (- n i)) (factorial (* 2 i)))))
              lst)))))
  (apply flvector (map fl ds)))

(define n 22)  ; 21 is enough to get about 16 digits; one more plays it safe
(define ds (make-ds n))
(define dn (flvector-ref ds n))

(: fleta-borwein (Flonum -> Flonum))
;; Error <= 4 ulp for s > 0
(define (fleta-borwein s)
  (let: loop : Flonum ([y : Flonum  0.0]
                       [sgn : Flonum  1.0]
                       [k : Nonnegative-Fixnum 0])
    (cond [(k . fx< . n)
           (define dy (fl/ (fl* sgn (fl- dn (flvector-ref ds k)))
                           (fl* dn (flexp (fl* s (fllog (+ (fl k) 1.0)))))))
           (define new-y (fl+ y dy))
           (cond [((flabs dy) . fl<= . (fl* (fl* 0.5 epsilon.0) (flabs new-y)))
                  new-y]
                 [else
                  (loop new-y (fl* sgn -1.0) (fx+ k 1))])]
          [else  y])))

(define pi.64 14488038916154245685/4611686018427387904)
(define euler.64 12535862302449814171/4611686018427387904)
(define pi.128 267257146016241686964920093290467695825/85070591730234615865843651857942052864)

(: flexppi (Flonum -> Flonum))
(define flexppi (make-flexpt pi.128))

(require math/bigfloat)
(: fleta (Flonum -> Flonum))
(define (fleta s)
  (cond [(s . fl> . 0.0)
         (cond [(s . fl> . +eta-max.0)  1.0]
               [(s . fl> . 1.0)  (fleta-borwein s)]
               [(s . fl> . 0.04)  (fleta-cheby-0-1 s)]
               [else  (fleta-taylor-0 s)])]
        [(s . fl< . 0.0)
         (cond [(s . fl= . -inf.0)  +nan.0]
               [(s . fl< . -224.5)
                (cond [(fleven? s)  0.0]
                      [(fleven? (fltruncate (* 0.5 s)))  +inf.0]
                      [else  -inf.0])]
               [(s . fl< . -171.0)
                (define f (make-flexpt (/ (* euler.64 pi.64) (inexact->exact (- s)))))
                (* (/ 4.0 pi)
                   (flexp-stirling (- s))
                   (flsqrt (/ pi (* -0.5 s)))
                   (f (* 0.5 s))
                   (* s 0.5 (flsinpix (* s 0.5)))
                   (f (* 0.5 s)))]
               [(s . fl< . -54.0)
                (* (/ 4.0 pi)
                   (flexppi s)
                   (* s 0.5 (flsinpix (* s 0.5)))
                   (flgamma (- s)))]
               [(s . fl< . -1.5)
                (define 2^s-1 (- (flexpt 2.0 s) 1.0))
                (* (/ 4.0 pi)
                   (flexppi s)
                   (* s 0.5 (flsinpix (* s 0.5)))
                   (flgamma (- s))
                   (fleta-borwein (- 1.0 s))
                   (/ (* 0.5 (- 2^s-1 1.0)) 2^s-1))]
               [(s . fl< . -0.05)
                (fleta-cheby-neg-1.5-0 s)]
               [else
                (fleta-taylor-0 s)])]
        [(s . fl= . 0.0)  0.5]
        [else  +nan.0]))

(: flzeta (Flonum -> Flonum))
(define (flzeta s)
  (cond [(s . fl> . +zeta-max.0)  1.0]
        [(s . fl> . -171.0)
         (define c
           (cond [(s . fl< . -1.0)  (- (- (fl/ 2.0 (flexpt 2.0 s)) 1.0))]
                 [else  (- (flexpm1 (fl* (fl- 1.0 s) (fllog 2.0))))]))
         (fl/ (fleta s) c)]
        [(and (s . fl> . -266.5) (s . fl< . 0.0))  ; s < 0 keeps TR happy
         (define f (make-flexpt (/ (* euler.64 pi.64) (inexact->exact (- s)))))
         (* (/ 4.0 pi)
            (flexp-stirling (- s))
            (flsqrt (/ pi (* -0.5 s)))
            (f (* 0.5 s))
            (* s 0.5 (flsinpix (* s 0.5)))
            (* (flexpt 2.0 (- s 1.0))
               (- (flexpt 2.0 s) 1.0))
            (f (* 0.5 s)))]
        [(s . fl> . -inf.0)
         (cond [(fleven? s)  0.0]
               [(fleven? (fltruncate (* 0.5 s)))  -inf.0]
               [else  +inf.0])]
        [else  +nan.0]))

(: eta (case-> (Nonpositive-Integer -> Exact-Rational)
               (Flonum -> Flonum)
               (Real -> Real)))
(define (eta s)
  (cond [(flonum? s)  (fleta s)]
        [(single-flonum? s)  (fleta (fl s))]
        [(integer? s)
         (cond [(zero? s)  1/2]
               [(negative? s)  (define k (- 1 s))
                               (* (/ (bernoulli-number k) k) (- (expt 2 k) 1))]
               [else  (fleta (fl s))])]
        [else
         (fleta (fl s))]))

(: zeta (case-> (Nonpositive-Integer -> Exact-Rational)
                (Flonum -> Flonum)
                (Real -> Real)))
(define (zeta s)
  (cond [(flonum? s)  (flzeta s)]
        [(single-flonum? s)  (flzeta (fl s))]
        [(integer? s)
         (cond [(zero? s)  -1/2]
               [(negative? s)  (define k (- 1 s))
                               (- (/ (bernoulli-number k) k))]
               [(eqv? s 1)  (raise-argument-error 'zeta "Real, not One" s)]
               [else  (flzeta (fl s))])]
        [else
         (flzeta (fl s))]))
