#lang s-exp "env-lang.ss"

(begin
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
   (for-syntax (only-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym] [-Real R] [-ExactPositiveInteger -Pos])))
  
  (define-for-syntax all-num-types (list -Pos -Nat -Integer -ExactRational -Flonum -Real N))

  (define-for-syntax fl-comp (-> -Flonum -Flonum B))
  (define-for-syntax fl-op (-> -Flonum -Flonum -Flonum))
  (define-for-syntax fl-unop (-> -Flonum -Flonum))

  (define-for-syntax real-comp (->* (list R R) R B))
  )

;; numeric predicates
[zero? (make-pred-ty (list N) B -Zero)]
[number? (make-pred-ty N)]
[integer? (Univ . -> . B : (-LFS (list (-filter -Real)) (list (-not-filter -Integer))))]
[exact-integer? (make-pred-ty -Integer)]
[real? (make-pred-ty -Real)]
[complex? (make-pred-ty N)]
[rational? (make-pred-ty -Real)]

[positive? (-> -Real B)]
[negative? (-> -Real B)]

[odd? (-> -Integer B)]
[even? (-> -Integer B)]

[modulo (cl->* (-Integer -Integer . -> . -Integer))]

[=  (->* (list N N) N B)]

[>= real-comp]
[<  real-comp]
[<= real-comp]
[>  real-comp]


[* (apply cl->* (for/list ([t all-num-types]) (->* (list) t t)))]
[+ (apply cl->* (for/list ([t all-num-types]) (->* (list) t t)))]

[- (apply cl->* 
            (for/list ([t (list -Integer -ExactRational -Flonum -Real N)])
              (->* (list t) t t)))]
[/ (apply cl->* 
	  (->* (list -Integer) -Integer -ExactRational)
	  (for/list ([t (list -ExactRational -Flonum -Real N)])
	    (->* (list t) t t)))]

[max (apply cl->* (for/list ([t all-num-types]) (->* (list t) t t)))]
[min (apply cl->* (for/list ([t all-num-types]) (->* (list t) t t)))]


[add1 (cl->* (-> -Pos -Pos)
             (-> -Nat -Pos)
             (-> -Integer -Integer)
             (-> -ExactRational -ExactRational)
             (-> -Flonum -Flonum)
             (-> -Real -Real)
             (-> N N))]

[sub1 (cl->* (-> -Pos -Nat)
             (-> -Integer -Integer)
             (-> -ExactRational -ExactRational)
             (-> -Flonum -Flonum)
             (-> -Real -Real)
             (-> N N))]

[quotient (-Integer -Integer . -> . -Integer)]
[remainder (-Integer -Integer . -> . -Integer)]
[quotient/remainder 
 (make-Function (list (make-arr (list -Integer -Integer) (-values (list -Integer -Integer)))))]

;; exactness
[exact? (N . -> . B)]
[inexact? (N . -> . B)]
[exact->inexact (cl->* 
                 (-Real . -> . -Flonum)
                 (N . -> . N))]
[inexact->exact (cl->*
                 (-Real . -> . -ExactRational)
                 (N . -> . N))]

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
[sqrt (cl->*
       (-Real . -> . -Real)
       (N . -> . N))]
[log (cl->*
      (-Pos . -> . -Real)
      (N . -> . N))]
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
[sqr (cl->* (-> -Pos -Pos)
            (-> -Nat -Nat)                          
            (-> -Integer -Integer)
            (-> -ExactRational -ExactRational)
            (-> -Flonum -Flonum)
            (-> -Real -Real)
            (-> N N))]
[sgn (N . -> . N)]
[conjugate (N . -> . N)]
[sinh (N . -> . N)]
[cosh (N . -> . N)]
[tanh (N . -> . N)]
;; unsafe numeric ops

[unsafe-flabs fl-unop]

[unsafe-fl+ fl-op]
[unsafe-fl- fl-op]
[unsafe-fl* fl-op]
[unsafe-fl/ fl-op]

[unsafe-fl= fl-comp]
[unsafe-fl<= fl-comp]
[unsafe-fl>= fl-comp]
[unsafe-fl> fl-comp]
[unsafe-fl< fl-comp]
