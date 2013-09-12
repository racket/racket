#lang typed/racket

(require racket/flonum racket/fixnum racket/unsafe/ops)

;; List of cases where the TR optimizer changes the semantics of programs.
;; These give different results in optimized TR and in untyped Racket.
;; Mostly found using random testing.

;; is:        +inf.0+nan.0i
;; should be: +inf.0-0.0i
(/
 (cosh (tanh (min 0)))
 (sqr (make-polar 9.8813129168249e-324 5.64234687795904e-184)))
(/
 1.0
 (sqr 9.8813129168249e-324+0.0i))
(/ 1.0 0.0+0.0i)

;; is:        +nan.0+nan.0i
;; should be: +nan.0+0.0i
(/
 (*
  (max 3/11 3/2 -0.0f0)
  (unsafe-fl* (real->double-flonum 0.0034637533f0) (real->double-flonum 2)))
 (*
  0.47415978f0
  (max (exact-round 0) (exact-round 2) (exact-round 1.3396765f0))
  (make-rectangular +inf.0 -2.6138428f-05)))

;; is:        +nan.0+nan.0i
;; should be: -0.0+nan.0i
(- (/ (ceiling 1.4821969375237e-323) (make-rectangular 1/3 -inf.0)))

;; is:        +inf.0+nan.0i
;; should be: +inf.0-26.76011085510254i
(+
 (make-rectangular 2.8794524657050558e-173 -26.76011f0)
 (max (sub1 (exact-round -0.0)))
 (*
  (fltruncate (real->double-flonum 3))
  (abs (real->double-flonum -9.194802638300139))
  (real->double-flonum +inf.f)))

;; is:        +nan.0+nan.0i
;; should be: +nan.0+0.0i
(/
 (* (fltruncate (real->double-flonum -0.0961751594300051)))
 (make-polar
  (unsafe-flmin (real->double-flonum 0) (real->double-flonum 9))
  (-
   (real->double-flonum 1.797693134862315e+308)
   (real->double-flonum 4.557755020079188e-100)
   (real->double-flonum -1.7568234f0))))

;; is:        +nan.0+nan.0i
;; should be: 0.0+nan.0i
(/ (make-rectangular (+ (real->double-flonum 7/12)) 1.7976931348623157e+308))

;; is:        +inf.0+nan.0i
;; should be: +inf.0+8.162406973444716e+297i
(+
 (truncate
  (bitwise-xor
   (exact-round 3.4584595208887e-323)
   (exact-round 41.25529f0)
   (exact-round 5)))
 (abs (exact-round 1.0117665851393622))
 (/
  (make-rectangular -inf.0 -1.7976931348623157e+308)
  (+ -22024056634.94161 -3.141574f0 0.0)))

;; is:        -inf.0+nan.0i
;; should be: -inf.0-0.042647640432547915i
(/
 (make-rectangular
  (max (real->double-flonum +inf.f) (real->double-flonum 2))
  (min (real->double-flonum 9) (real->double-flonum 1)))
 -23.447956f0)

;; is:        -15.99999536766402+0.31099018454551697i
;; should be: -15.999995231628418+0.31099018454551697i
(-
 (* 6/11 (/ 1.2125638f0 142778.6f0))
 (sqr (* -4))
 (make-rectangular
  (unsafe-fl* (real->double-flonum 1) (real->double-flonum 0))
  (min (real->double-flonum -0.31099018f0))))

;; is:        +inf.0+nan.0i
;; should be: +inf.0+1.976262583365e-322i
(/
 (+ (sqr 6.1239376f0) (make-rectangular -inf.f -2.4703282292062e-323))
 (max -1/8))

;; is:        +nan.0+nan.0i
;; should be: +nan.0+inf.0i
(-
 (* -1 (+ 2 -1.863321008013001) (/ +nan.0 -3 -0.2859221f0))
 (bitwise-xor
  (exact-round -2.5171401615688167e-250)
  (exact-round 9/5)
  (add1 (exact-round 0)))
 (sinh (/ (make-rectangular 1.7976931348623155e+308 -13/21) 2)))

;; is:        +nan.0+nan.0i
;; should be: -inf.0-inf.0i
(/ (cosh (make-rectangular +inf.0 -1.7082773f0)) 8)

;; is:        +nan.0+nan.0i
;; should be: +nan.0-0.0i
(/
 (make-rectangular
  -1.0020975423559487e+94
  (*
   (real->double-flonum 9.8813129168249e-324)
   (real->double-flonum -5/7)
   (real->double-flonum -3))))

;; type-before = Single-Flonum-Complex
;; type-after = Float-Complex
;; redex-check: <collects>/tests/typed-racket/tr-random-testing.rkt:258
;; counterexample found after 499 attempts:
(log (make-rectangular -inf.f -inf.f))

;; is:        9.5+inf.0i
;; should be: 9.5+inf.0i
(/
 (make-rectangular
  (- (real->double-flonum 9) (real->double-flonum -1/2))
  +inf.0)
 (flexpt
  (abs (real->double-flonum 1.2370933818061895e+67))
  (real->double-flonum 0.0f0))
 (cos (min (exact-round 6.65203545785633) (exact-round -2.6796508f-29))))

;; is:        +nan.0+nan.0i
;; should be: -1.0+nan.0i
(sub1 (/ (make-rectangular -5.5945454f0 -inf.0)))

;; is:        -inf.0+nan.0i
;; should be: -inf.0+3.449695564532713i
(*
 (make-rectangular -inf.0 2.360318017838172)
 (* 1 (max (exact-round 1)))
 19/13)

;; is:        +nan.0+nan.0i
;; should be: 0.0+nan.0i
(/ (make-rectangular 1 +inf.0))
(/ (make-rectangular 1.0 +inf.0))

;; is:        1.0+0.0
;; should be: +inf.0+0.0
(+
  (exp 1.7976931348623151e+308)
  0.0+0.0i)

;; is:        1.0+0.0i
;; should be: 1.0-0.0i
(conjugate 1.0+0.0i)

;; is:        +nan.0+nan.0i
;; should be: 1.0+1.0i
(* (expt 10 500) (expt 10 -500) 1.0+1.0i)

;; is:        +nan.0+1.0i
;; should be: 1.0+1.0i
(+ (expt 10 501) (expt -10 501) 1.0+1.0i)

;; is:        0.0+0.0i
;; should be: -0.0-0.0i
(- (+ 0 0) 0.0+0.0i)
