#lang typed/racket/base

#|
TODO

Extend domain to > -1.4846954180325517e-306 (generally do better around poles)

Error on exact nonpositive integers
|#

(require (only-in racket/math pi) racket/flonum racket/fixnum
         "../constants.rkt"
         "utils.rkt"
         "factorial.rkt"
         "polyfun.rkt"
         "sinpx.rkt")

(provide gamma flgamma)

(define flonum-fact-table-size 171.0)
(define flonum-fact-table
  (build-flvector (fl->fx flonum-fact-table-size)
                  (compose real->double-flonum factorial)))

;; Calculates Gamma(x) using the identity Gamma(x) = (x-1)! for positive integer x
(: flgamma-integer (Float -> Float))
(define (flgamma-integer x)
  (cond [(x . >= . 1.0)
         (cond [(x . <= . flonum-fact-table-size)  (flvector-ref flonum-fact-table (- (fl->fx x) 1))]
               ;; 171! won't fit in a Float
               [else  +inf.0])]
        ;; Gamma(x) undefined for integer x <= 0
        [else  +nan.0]))

;; Calculates Gamma(x) using Euler's reflection formula Gamma(1-x) * Gamma(x) = pi / sin(pi*x)
(: flgamma-large-negative (Float -> Float))
(define (flgamma-large-negative x)
  (/ (- pi) (* (flgamma (- x)) (sinpx x))))

;; Calculates Gamma(x) using the first three terms of Gamma(x)'s Laurent expansion at 0
;; Error ε is O(x^2), and ε = 0.0 (though positive) when 0.0 < x < 1e-10
(: flgamma-small-positive (Float -> Float))
(define (flgamma-small-positive x)
  (+ (/ 1.0 x)
     (- euler.0)
     (* #i1/6 x (+ (* 3.0 euler.0 euler.0) (* 0.5 pi pi)))))

;; Lanczos polynomial for N=13 G=6.024680040776729583740234375
;; Max experimental error (with arbitary precision arithmetic) 1.196214e-17
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

;; Argument reduction for when x is a small negative number
(: flgamma-reduce-negative (Float -> (Values Positive-Float Float)))
(define (flgamma-reduce-negative x)
  (let loop ([x x] [y 1.0])
    (cond [(x . <= . 0.0)  (loop (+ x 1.0) (/ y x))]
          [else  (values x y)])))

;; Computes Gamma(x) for 5.56268464626801e-309 <= x <= 171.6243769563027 and
;; negative values not near integers
(: flgamma (Float -> Float))
(define (flgamma x)
  (cond [(integer? x)  (flgamma-integer x)]
        [(x . <= . -20.0)  (flgamma-large-negative x)]
        [(and (x . > . 0.0) (x . < . 1e-10))  (flgamma-small-positive x)]
        [else
         (let*-values ([(x y)  (flgamma-reduce-negative x)]
                       [(y)    (* y (lanczos-sum x))])
           (cond [(x . > . 140.0)
                  (define xgh (+ x lanczos-g -0.5))
                  (define hp (flexpt xgh (- (* x 0.5) 0.25)))
                  (* (* y (/ hp (exp xgh))) hp)]
                 [else
                  (define xgh (+ x lanczos-g -0.5))
                  (* y (/ (flexpt xgh (- x 0.5)) (exp xgh)))]))]))

;; Computes Gamma(x)
(: gamma (case-> ;(Exact-Positive-Integer -> Exact-Positive-Integer)
                 ;(Single-Flonum -> Single-Flonum)
                 ;(Float -> Float)
                 (Real -> Real)))
(define (gamma x)
  (cond [(double-flonum? x)  (flgamma x)]
        [(exact-positive-integer? x)  (factorial (- x 1))]
        [(single-flonum? x)  (real->single-flonum (flgamma (real->double-flonum x)))]
        [else  (flgamma (real->double-flonum x))]))

(module* test typed/racket
  (require (submod "..")
           typed/rackunit
           math/constants)
  (define ε +epsilon.0)
  (: relative-error : Float Float -> Float)
  (define (relative-error x correct)
    (/ (abs (- x correct)) correct))
  (: relative-error<= : Float Float Float -> Boolean)
  (define (relative-error<= x correct epsilon)
    (<= (relative-error x correct) epsilon))  
  
  (check-equal? (flgamma 0.0) +nan.0)
  (check-equal? (flgamma 1.0) 1.0)
  (check-equal? (flgamma 2.0) 1.0)
  (check-equal? (flgamma 3.0) 2.0)
  (check-equal? (flgamma 4.0) 6.0)
  (check-= (flgamma -21.5) 1.31844491832155110297694106059e-20 ε)
  (check-true (relative-error<= (flgamma 1e-15) 9.9999999999999942278433509847e14 ε))
  (check-true (relative-error<= (flgamma 142.5) 2.25990910998653224305124991671e244 ε))
  (check-equal? (flgamma 172.0) 1.24101807021766782342484052410e309) ; = +inf.0
  (check-true (relative-error<= (flgamma -1e-5) -100000.577225555552235029678062 ε))
  
  (check-equal? (gamma 0) +nan.0)
  (check-equal? (gamma 4) 6)
  (check-equal? (gamma 4.0) (flgamma 4.0))
  (check-equal? (gamma 4.0f0) (real->single-flonum (flgamma 4.0))))
