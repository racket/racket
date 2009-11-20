#lang s-exp "env-lang.ss"

(require
 scheme/tcp
 scheme
 scheme/unsafe/ops
 (only-in rnrs/lists-6 fold-left)
 '#%paramz
 "extra-procs.ss"
 (only-in '#%kernel [apply kernel:apply])
 scheme/promise scheme/system
 (only-in string-constants/private/only-once maybe-print-message)
 (only-in scheme/match/runtime match:error matchable? match-equality-test)
 (for-syntax (only-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym] [-Real R])))

;; numeric operations
[modulo (cl->* (-Integer -Integer . -> . -Integer))]
[=  (->* (list N N) N B)]
[>= (->* (list R R) R B)]
[<  (->* (list R R) R B)]
[<= (->* (list R R) R B)]
[>  (->* (list R R) R B)]
[zero? (N . -> . B)]
[* (cl->* (->* '() -ExactPositiveInteger -ExactPositiveInteger)
          (->* '() -Nat -Nat)
	  (->* '() -Integer -Integer)
          (->* '() -ExactRational -ExactRational)
          ;; Reals are just Rat + Int
          (->* '() -Real -Flonum)
	  (->* '() N N))]
[/ (cl->* (->* (list N) N N))]
[+ (cl->* (->* '() -ExactPositiveInteger -ExactPositiveInteger)
          (->* '() -Nat -Nat)
	  (->* '() -Integer -Integer)
          (->* '() -ExactRational -ExactRational)
          ;; Reals are just Rat + Int
          (->* '() -Real -Flonum)
	  (->* '() N N))]
[- (cl->* (->* (list -Integer) -Integer -Integer) (->* (list N) N N))]
[max (cl->* (->* (list -Integer) -Integer -Integer)
            (->* (list N) N N))]
[min (cl->* (->* (list -Integer) -Integer -Integer)
            (->* (list N) N N))]
[positive? (-> N B)]
[negative? (-> N B)]
[odd? (-> -Integer B)]
[even? (-> -Integer B)]
[add1 (cl->* (-> -Integer -Integer)
             (-> N N))]
[sub1 (cl->* (-> -Integer -Integer)
             (-> N N))]
[quotient (-Integer -Integer . -> . -Integer)]
[remainder (-Integer -Integer . -> . -Integer)]
[quotient/remainder 
 (make-Function (list (make-arr (list -Integer -Integer) (-values (list -Integer -Integer)))))]

[exact? (N . -> . B)]
[inexact? (N . -> . B)]
[exact->inexact (N . -> . N)]
[inexact->exact (N . -> . N)]

[number? (make-pred-ty N)]
[integer? (Univ . -> . B : (-LFS (list (-filter N)) (list (-not-filter -Integer))))]
[exact-integer? (make-pred-ty -Integer)]

[real? (Univ . -> . B : (-LFS (list (-filter N)) (list)))]
[complex? (Univ . -> . B : (-LFS (list (-filter N)) (list)))]
[rational? (Univ . -> . B : (-LFS (list (-filter N)) (list)))]
[floor    (-> N N)]
[ceiling  (-> N N)]
[truncate (-> N N)]
[make-rectangular (N N . -> . N)]
[make-polar (N N . -> . N)]
[real-part (N . -> . N)]
[imag-part (N . -> . N)]
[magnitude (N . -> . N)]
[angle     (N . -> . N)]
[numerator   (N . -> . -Integer)]
[denominator (N . -> . -Integer)]
[rationalize (N N . -> . N)]
[expt (cl->* (-Integer -Integer . -> . -Integer) (N N . -> . N))]
[sqrt (N . -> . N)]
[log  (N . -> . N)]
[exp  (N . -> . N)]
[cos  (N . -> . N)]
[sin  (N . -> . N)]
[tan  (N . -> . N)]
[acos (N . -> . N)]
[asin (N . -> . N)]
[atan (N . -> . N)]
[gcd  (null -Integer . ->* . -Integer)]
[lcm  (null -Integer . ->* . -Integer)]

[round (N . -> . -Integer)]

;; scheme/math

[sgn (-Real . -> . -Real)]
[pi N]
[sqr (N . -> . N)]
[sgn (N . -> . N)]
[conjugate (N . -> . N)]
[sinh (N . -> . N)]
[cosh (N . -> . N)]
[tanh (N . -> . N)]
;; end numeric ops
