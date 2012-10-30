#lang typed/racket/base

#|
erfc for large x implementation from:

C Tellambura and A Annamalai. Efficient Computation of erfc(x) for Large Arguments.
IEEE Transactions on Communications, 2000, vol 48, pp 529--532
|#

(require "../../flonum.rkt"
         "../../base.rkt"
         "../distributions/impl/normal-cdf.rkt"
         "../polynomial/chebyshev.rkt"
         "continued-fraction.rkt")

(provide flerf flerfc*expsqr flerfc
         erf erfc)

;; ===================================================================================================
;; erf

(: flerf-normal (Flonum -> Flonum))
(define (flerf-normal x)
  (fl- (fl* 2.0 (standard-flnormal-cdf (fl* x (flsqrt 2.0)))) 1.0))

(: flerf-taylor-0 (Flonum -> Flonum))
;; Good for -0.5 <= x <= 0.5
(define (flerf-taylor-0 x)
  (fl* x ((make-flpolyfun
           (+1.1283791670955125738961589031215451716881e0
            -3.7612638903183752463205296770718172389603e-1
            +1.1283791670955125738961589031215451716881e-1
            -2.6866170645131251759432354836227265992574e-2
            +5.2239776254421878421118467737108572763338e-3
            -8.5483270234508528325466583569814028158189e-4
            +1.2055332981789664251027338708563516791540e-4
            -1.4925650358406250977462419353459592218097e-5
            +1.6462114365889247401612962522198079652312e-6
            -1.6365844691234924317393003677039026554930e-7
            +1.4807192815879217239546050945892452597318e-8
            -1.2290555301717927352982888136906778835719e-9))
          (fl* x x))))

(: flerf (Flonum -> Flonum))
(define (flerf x)
  (let ([ax  (flabs x)])
    (cond [(ax . fl> . 0.5)  (flerf-normal x)]
          [(ax . fl< . 1e-13)  (fl* x (fl/ 2.0 (flsqrt pi)))]
          [else  (flerf-taylor-0 x)])))

;; ===================================================================================================
;; erfc(x)*exp(x^2)

(define sqrtpi 1.772453850905516027298167483341145182798)

(: flerfc*expsqr-huge (Flonum -> Flonum))
;; Computed using erfc's asymptotic expansion
;; Error < epsilon.0 for x > 1e8
(define (flerfc*expsqr-huge x)
  (fl/ 1.0 (fl* x sqrtpi)))

(: flerfc*expsqr-asym (Flonum -> Flonum))
;; Computed using erfc's asymptotic expansion
;; Error < epsilon.0 for x > 24
(define (flerfc*expsqr-asym x)
  (define 1/2x^2 (fl/ 1.0 (fl* 2.0 (fl* x x))))
  (fl* (fl/ 1.0 (fl* x sqrtpi))
       ((make-flpolyfun
         (1.0 -1.0 3.0 -15.0 105.0 -945.0 10395.0))
        1/2x^2)))

(: foo (Flonum -> Flonum))
(define (foo x)
  (define 2x^2 (fl* 2.0 (fl* x x)))
  (fl* (fl/ x sqrtpi)
       ((make-flpolyfun
         (1.0 -1.0 3.0 -15.0 105.0 -945.0 10395.0))
        2x^2)))

(: flerfc*expsqr-series (Flonum -> Flonum))
;; Good for x > 5.0
(define (flerfc*expsqr-series x)
  (define h (cond [(x . fl< . 5.4)  0.25]
                  [(x . fl< . 8.0)  0.4]
                  [(x . fl< . 16.0)  0.5]
                  [else  0.51]))
  (define h^2 (* h h))
  (define z
    (fl+ (fl/ (fl/ 1.0 x) x)
         (let: loop : Flonum ([y : Flonum  0.0] [n : Flonum  1.0])
           (define n^2*h^2 (fl* (fl* n n) h^2))
           (define dy (fl/ (fl* 2.0 (flexp (- n^2*h^2))) (fl+ n^2*h^2 (fl* x x))))
           ;(printf "~v~n" dy)
           (cond [(dy . fl< . (fl* (fl* 0.5 epsilon.0) y))  (fl+ dy y)]
                 [else  (loop (fl+ y dy) (fl+ n 1.0))]))))
  (fl* (fl/ (fl* x h) pi) z))

(: flerfc*expsqr-3-5 (Flonum -> Flonum))
(define (flerfc*expsqr-3-5 x)
  (fl/ ((inline-chebyshev-flpoly-fun
         3.0 5.0
         (76.91661738312371165176665510251236326338
          27.20465693840961735563948053921797067271
          3.386442685567191365542568636267208877102
          0.140993936422267459775165813598040765649
          -5.153134548269842201105342673544377759352e-6
          1.7508864589759466867096925622177881862e-6
          -3.043007141646851587889970096343295638291e-7
          4.243425771190431100938117624380917251699e-8
          -5.203867155244060760117564922569126026438e-9
          5.794849132093509459568321930026301391629e-10
          -5.921422840095836545874122164110445535066e-11
          5.533766942834863591670434965523217803471e-12
          -4.631550718317617310162582984555616243437e-13
          3.262025831452441605191178585599762596143e-14
          -1.513328953213075725641119234827526370187e-15
          -4.522283605960468274138123625976905984843e-17))
        x)
       (flexpt x 4.0)))

(: flerfc*expsqr-1-3 (Flonum -> Flonum))
(define (flerfc*expsqr-1-3 x)
  (fl/ ((inline-chebyshev-flpoly-fun
         1.0 3.0
         (0.9945093280638061048517513432019848175032
          0.05120904381630891334631711380945424146303
          -0.01422342070434339336273995600351825586225
          0.003357268323337835041126774361327403324835
          -7.114985025457904001609950417739639868647e-4
          1.390876463234288554054665709920181526987e-4
          -2.548042834519374090944060859469479045399e-5
          4.420149801027904714368125294893695251516e-6
          -7.314182887789212376555477464037660326951e-7
          1.160833279816164682586230015667162482652e-7
          -1.77457488690957160865337806653632235146e-8
          2.621904502367314860299110485351868396918e-9
          -3.754481078582135875876211640532994079565e-10
          5.222857161436770247649169925242741865874e-11
          -7.07220022924025262089289170578922817775e-12
          9.337553246718007012188722480344684913175e-13
          -1.203905834239132741551983752552150752983e-13
          1.517763413855051802253894051250284281458e-14
          -1.873156638647946872401895465528599118158e-15
          2.265073220841686859543609057755087846036e-16
          -2.651790674204626906157253423748608352303e-17))
        x)
       x))

(define flerfc*expsqr-0.5-1
  (inline-chebyshev-flpoly-fun
   0.5 1.0
   (1.028508060089628120924659381992051204702
    -0.09353436157631646884296478224639544611431
    0.00734954356796596066014584525500838968569
    -5.170287683170954502817549118454885607861e-4
    3.327489181039831922712166420908522822156e-5
    -1.987760941931570403969164257181219771567e-6
    1.113719839925198075577451421375109070733e-7
    -5.898649609389129304942457516804088201602e-9
    2.971299014285280439436981007380165475227e-10
    -1.430488185776241939959484837699287692925e-11
    6.608616284222898801100004753695010173929e-13
    -2.939555278237892581547471127716213388842e-14
    1.262499691092374606344446613363361460578e-15
    -5.240060859272093179585053345170003668439e-17)))

(: flerfc*expsqr-normal (Flonum -> Flonum))
(define (flerfc*expsqr-normal x)
  (fl* (flexpsqr x) (fl* 2.0 (standard-flnormal-cdf (- (fl* x (flsqrt 2.0)))))))

(: flerfc*expsqr (Flonum -> Flonum))
(define (flerfc*expsqr x)
  (cond [(x . fl> . 1e8)    (flerfc*expsqr-huge x)]
        [(x . fl> . 24.0)   (flerfc*expsqr-asym x)]
        [(x . fl> . 5.0)    (flerfc*expsqr-series x)]
        [(x . fl> . 3.0)    (flerfc*expsqr-3-5 x)]
        [(x . fl> . 1.0)    (flerfc*expsqr-1-3 x)]
        [(x . fl> . 0.5)    (flerfc*expsqr-0.5-1 x)]
        [(x . fl> . -27.0)  (flerfc*expsqr-normal x)]
        [else  +inf.0]))

;; ===================================================================================================
;; erfc

(define -erfc-max.0 -5.8635847487551676)
(define +erfc-max.0 27.226017111108362)

(: flerfc (Flonum -> Flonum))
(define (flerfc x)
  (cond [(x . fl< . 0.0)
         (cond [(x . fl< . -erfc-max.0)  2.0]
               [else  (- 2.0 (flerfc (- x)))])]
        [(x . fl> . 0.0)
         (cond [(x . fl> . +erfc-max.0)  0.0]
               [else  (fl* (flerfc*expsqr x) (flgauss x))])]
        [(x . fl= . 0.0)  1.0]
        [else  +nan.0]))

;; ===================================================================================================

(: erf (case-> (Zero -> Zero)
               (Flonum -> Flonum)
               (Real -> (U Zero Flonum))))
(define (erf x)
  (cond [(flonum? x)  (flerf x)]
        [(eqv? x 0)  x]
        [else  (flerf (fl x))]))

(: erfc (case-> (Zero -> One)
                (Flonum -> Flonum)
                (Real -> (U One Flonum))))
(define (erfc x)
  (cond [(flonum? x)  (flerfc x)]
        [(eqv? x 0)  1]
        [else  (flerfc (fl x))]))
