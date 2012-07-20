#lang typed/racket/base

#|
Compute Gamma(x) for +flgamma-min.0 <= x <= +flgamma-max.0 and non-integer x <= -flgamma-min.0

Identities:
 * Gamma(x) = x * Gamma(x-1)         [definition]
 * Gamma(x) = (x-1)!, integer x > 0  [definition]
 * Gamma(x) = Gamma(x/2) * Gamma(x/2 + 1/2) * 2^(x-1) / sqrt(pi)  [doubling formula]


Floating-point design choices:
 * Gamma(x) = +inf.0, x = +0.0
 * Gamma(x) = -inf.0, x = -0.0
 * Gamma(x) = +nan.0, integer x < 0.0 or x = -inf.0

Approximations:
 * A Lanczos polynomial approximation cribbed from the Boost library
 * Laurent expansion at 0
 * Taylor expansion at 1
 * Gamma(x) ~ 0.0, non-integer x < 184
|#

(require racket/flonum racket/fixnum
         (only-in racket/math exact-truncate)
         "../../constants.rkt"
         "../utils.rkt"
         "factorial.rkt"
         "polyfun.rkt")

(provide gamma flgamma
         +flgamma-max.0
         +flgamma-min.0
         -flgamma-min.0)

(define +flgamma-max.0 171.6243769563027)
(define +flgamma-min.0 5.56268464626801e-309)
;; Gamma(x) ~ 1/x around very small numbers
(define -flgamma-min.0 (- +flgamma-min.0))

(define flonum-fact-table-size 171.0)
(define flonum-fact-table
  (build-flvector (fl->fx flonum-fact-table-size)
                  (compose real->double-flonum factorial)))

(: laurent-sum/0 (Float -> Float))
;; Laurent expansion for -0.01 <= x <= 0.01
(define (laurent-sum/0 x)
  (+ (/ 1.0 x)
     (- gamma.0)
     (* x ((make-polyfun
            Float
            (+0.989055995327972555395395651500634707939184
             -0.907479076080886289016560167356275114928611
             +0.981728086834400187336380294021850850360574
             -0.981995068903145202104701413791374675517427
             +0.993149114621276193153867253328658498037491
             -0.996001760442431533970078419664566686735299))
           x))))

(: taylor-sum/1 (Float -> Float))
;; Taylor expansion for 0.5 <= x <= 1.5
(define (taylor-sum/1 x)
  ((make-polyfun
    Float
    (+1.0
     -0.57721566490153286060651209008240243104216
     +0.98905599532797255539539565150063470793918
     -0.90747907608088628901656016735627511492861
     +0.98172808683440018733638029402185085036057
     -0.98199506890314520210470141379137467551742
     +0.99314911462127619315386725332865849803748
     -0.99600176044243153397007841966456668673529
     +0.99810569378312892197857540308836723752396
     -0.99902526762195486779467805964888808853230
     +0.99951565607277744106705087759437019443449
     -0.99975659750860128702584244914060923599695
     +0.99987827131513327572617164259000321938762
     -0.99993906420644431683585223136895513185793
     +0.99996951776348210449861140509195350726552
     -0.99998475269937704874370963172444753832607
     +0.99999237447907321585539509450510782583380
     -0.99999618658947331202896495779561431380200
     +0.99999809308113089205186619151459489773168
     -0.99999904646891115771748687947054372632469
     +0.99999952321060573957523929299106456816808
     -0.99999976159734438057092470106258744748608
     +0.99999988079601916841665041840424924052652
     -0.99999994039712498374586288797675081784805
     +0.99999997019826758235557449619251141981337
     -0.99999998509903547504708716847676946506238
     +0.99999999254948496246470479925372366703987
     -0.99999999627473155543691433333928727572304
     +0.99999999813736213559466706281273498736619
     -0.99999999906867985370787921791028525589934
     +0.99999999953433952214542133897633392293552
     ;; Maxima had a hard time computing the expansion past here, so these will have to do for now
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0
     -1.0
     +1.0))
   (- x 1.0)))

;; Lanczos polynomial for N=13 G=6.024680040776729583740234375
;; Max experimental error (with arbitary precision arithmetic) is 1.196214e-17
(define lanczos-sum
  (make-quotient-polyfun
   Float
   (23531376880.41075968857200767445163675473
    42919803642.64909876895789904700198885093
    35711959237.35566804944018545154716670596
    17921034426.03720969991975575445893111267
    6039542586.35202800506429164430729792107
    1439720407.311721673663223072794912393972
    248874557.8620541565114603864132294232163
    31426415.58540019438061423162831820536287
    2876370.628935372441225409051620849613599
    186056.2653952234950402949897160456992822
    8071.672002365816210638002902272250613822
    210.8242777515793458725097339207133627117
    2.506628274631000270164908177133837338626)
   (0.0
    39916800.0
    120543840.0
    150917976.0
    105258076.0
    45995730.0
    13339535.0
    2637558.0
    357423.0
    32670.0
    1925.0
    66.0
    1.0)))

(define lanczos-g 6.024680040776729583740234375)

(: flgamma-integer (Float -> Float))
;; Computes Gamma(x) using factorial
(define (flgamma-integer x)
  (cond [(x . >= . 1.0)
         (cond [(x . <= . flonum-fact-table-size)
                (flvector-ref flonum-fact-table (- (fl->fx x) 1))]
               ;; 171! won't fit in a Float
               [else  +inf.0])]
        ;; Gamma(x) undefined for integer x <= 0; just need to determine which special to return
        [(equal? x -0.0)  -inf.0]
        [(equal? x +0.0)  +inf.0]
        [else  +nan.0]))

(: flgamma-large-negative (Float -> Float))
;; Computes Gamma(x) for non-integer x < -170
(define (flgamma-large-negative x)
  (cond [(x . < . -184.0)
         ;; Gamma(x) ~ 0.0 for non-integer x < -184; determine sign
         (if (even? (exact-truncate x)) -0.0 0.0)]
        [else
         ;; The standard argument reduction is horrible with -184 < x < -170
         ;; Fortunately, the doubling formula is great in this subdomain
         (* (flgamma (* 0.5 x))
            (flgamma (+ (* 0.5 x) 0.5))
            (flexpt 2.0 (- x 1.0))
            (/ 1.0 (flsqrt pi.0)))]))

(: flgamma-taylor (Float -> Float))
;; Computes Gamma(x) using Taylor expansion
;; Error is ~ 0.0 when 0.5 <= x <= 1.5
(define (flgamma-taylor x)
  (let loop ([x x] [y 1.0])
    (cond [(x . > . 1.5)  (loop (- x 1.0) (* y (- x 1.0)))]
          [(x . < . 0.5)  (loop (+ x 1.0) (/ y x))]
          [else  (* y (taylor-sum/1 x))])))

(: flgamma-reduce-negative (Float Float -> (Values Float Float)))
;; Argument reduction with Gamma(x-1) = Gamma(x) / x; used when x is a small negative number
(define (flgamma-reduce-negative x mx)
  (let loop ([x x] [y 1.0])
    (cond [(x . <= . mx)  (loop (+ x 1.0) (/ y x))]
          [else  (values x y)])))

(: flgamma-laurent (Float -> Float))
;; Calculates Gamma(x) using Laurent expansion
;; Error is ~ 0.0 when -0.001 < x < 0.01
(define (flgamma-laurent x)
  ;(printf "laurent ~v~n" x)
  (let-values ([(x y)  (flgamma-reduce-negative x -0.5)])
    (* y (laurent-sum/0 x))))

(: flgamma-lanczos (Float -> Float))
;; Computes Gamma(x) using a Lanczos approximation
(define (flgamma-lanczos x)
  ;(printf "lanczos ~v~n" x)
  (let*-values ([(x y)  (flgamma-reduce-negative x 0.0)]
                [(y)    (* y (lanczos-sum x))])
    (cond [(x . > . 140.0)
           (define xgh (+ x lanczos-g -0.5))
           (define hp (flexpt xgh (- (* x 0.5) 0.25)))
           (* (* y (/ hp (exp xgh))) hp)]
          [else
           (define xgh (+ x lanczos-g -0.5))
           (* y (/ (flexpt xgh (- x 0.5)) (exp xgh)))])))

(: flgamma (Float -> Float))
(define (flgamma x)
  (cond [(integer? x)  (flgamma-integer x)]
        ;; Lanczos produces +nan.0 for huge inputs; avoid
        [(x . > . +flgamma-max.0)  +inf.0]
        ;; Limit as x -> -inf doesn't exist
        [(x . = . -inf.0)  +nan.0]
        [(eqv? x +nan.0)   +nan.0]
        [(x . < . -170.0)  (flgamma-large-negative x)]
        ;; If near a pole, use Laurent
        [(and (x . < . 0.5)
              (let ([dx  (- x (round x))])
                (and (dx . > . -0.001) (dx . < . 0.01))))
         (flgamma-laurent x)]
        ;; If small, use Taylor
        [(and (x . > . -4.5) (x . < . 4.5))  (flgamma-taylor x)]
        [else  (flgamma-lanczos x)]))

(: gamma (case-> ;(Exact-Positive-Integer -> Exact-Positive-Integer)
          ;(Single-Flonum -> Single-Flonum)
          ;(Float -> Float)
          (Real -> Real)))
(define (gamma x)
  (cond [(double-flonum? x)  (flgamma x)]
        [(single-flonum? x)  (real->single-flonum (flgamma (real->double-flonum x)))]
        [(integer? x)  (if (x . > . 0)
                           (factorial (- x 1))
                           (error 'gamma "undefined for nonpositive integers"))]
        [else  (flgamma (real->double-flonum x))]))

