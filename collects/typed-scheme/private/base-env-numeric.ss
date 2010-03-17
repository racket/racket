#lang s-exp "env-lang.ss"

(begin
  (require
   scheme/tcp
   scheme scheme/flonum scheme/fixnum
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

  (define-for-syntax binop 
    (lambda (t [r t])
      (t t . -> . r)))
  
  (define-for-syntax (unop t) (-> t t))
  
  (define-for-syntax fl-comp (binop -Flonum B))
  (define-for-syntax fl-op (binop -Flonum))
  (define-for-syntax fl-unop (unop -Flonum))
  
  
  (define-for-syntax int-op (binop -Integer))
  (define-for-syntax nat-op (binop -Nat))
  
  (define-for-syntax fx-comp (binop -Integer B))
  (define-for-syntax fx-op (cl->* nat-op int-op))
  (define-for-syntax fx-intop int-op)
  (define-for-syntax fx-unop (unop -Integer))

  (define-for-syntax real-comp (->* (list R R) R B))
  )

;; numeric predicates
[zero? (make-pred-ty (list N) B -Zero)]
[number? (make-pred-ty N)]
[integer? (Univ . -> . B : (-LFS (list (-filter (Un -Integer -Flonum))) (list (-not-filter -Integer))))]
[exact-integer? (make-pred-ty -Integer)]
[real? (make-pred-ty -Real)]
[inexact-real? (make-pred-ty -Flonum)]
[complex? (make-pred-ty N)]
[rational? (make-pred-ty -Real)]
[exact? (N . -> . B : (-LFS (list) (list (-not-filter -ExactRational))))]
[inexact? (N . -> . B : (-LFS (list) (list (-not-filter -Flonum))))]
[fixnum? (Univ . -> . B : (-LFS (list (-filter -Integer)) null))]
[positive? (-> -Real B)]
[negative? (-> -Real B)]
[exact-positive-integer? (make-pred-ty -Pos)]
[exact-nonnegative-integer? (make-pred-ty -Nat)]

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
[exact->inexact (cl->* 
                 (-Real . -> . -Flonum)
                 (N . -> . N))]
[inexact->exact (cl->*
                 (-Real . -> . -ExactRational)
                 (N . -> . N))]

[floor    (-> -Real -Real)]
[ceiling  (-> -Real -Real)]
[truncate (-> -Real -Real)]
[make-rectangular (-Real -Real . -> . N)]
[make-polar (-Real -Real . -> . N)]
[real-part (N . -> . -Real)]
[imag-part (N . -> . -Real)]
[magnitude (N . -> . -Real)]
[angle     (N . -> . -Real)]
[numerator   (-Real . -> . -Real)]
[denominator (-Real . -> . -Real)]
[rationalize (-Real -Real . -> . N)]
[expt (cl->* (-Integer -Integer . -> . -Integer) (N N . -> . N))]
[sqrt (cl->*
       (-Nat . -> . -Real)
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

[round (-Real . -> . -Real)]

;; scheme/math

[sgn (-Real . -> . -Real)]
[pi -Flonum]
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
[unsafe-flsqrt fl-unop]

[unsafe-fl+ fl-op]
[unsafe-fl- fl-op]
[unsafe-fl* fl-op]
[unsafe-fl/ fl-op]

[unsafe-fl= fl-comp]
[unsafe-fl<= fl-comp]
[unsafe-fl>= fl-comp]
[unsafe-fl> fl-comp]
[unsafe-fl< fl-comp]


[unsafe-fx+ fx-op]
[unsafe-fx- fx-intop]
[unsafe-fx* fx-op]
[unsafe-fxquotient fx-intop]
[unsafe-fxremainder fx-intop]
[unsafe-fxmodulo fx-intop]
[unsafe-fxabs (-Integer . -> . -Nat)]

[unsafe-fxand fx-intop]
[unsafe-fxior fx-intop]
[unsafe-fxxor fx-intop]
[unsafe-fxnot fx-unop]
[unsafe-fxlshift fx-intop]
[unsafe-fxrshift fx-intop]

[unsafe-fx= fx-comp]
[unsafe-fx< fx-comp]
[unsafe-fx> fx-comp]
[unsafe-fx<= fx-comp]
[unsafe-fx>= fx-comp]
[unsafe-fxmin fx-op]
[unsafe-fxmax fx-op]

;; scheme/fixnum

[fx+ fx-op]
[fx- fx-intop]
[fx* fx-op]
[fxquotient fx-intop]
[fxremainder fx-intop]
[fxmodulo fx-intop]
[fxabs (-Integer . -> . -Nat)]

[fxand fx-intop]
[fxior fx-intop]
[fxxor fx-intop]
[fxnot fx-unop]
[fxlshift fx-intop]
[fxrshift fx-intop]

[fx= fx-comp]
[fx< fx-comp]
[fx> fx-comp]
[fx<= fx-comp]
[fx>= fx-comp]
[fxmin fx-op]
[fxmax fx-op]


;; safe flonum ops
[flabs fl-unop]
[flsqrt fl-unop]
[fl+ fl-op]
[fl- fl-op]
[fl* fl-op]
[fl/ fl-op]
[fl= fl-comp]
[fl<= fl-comp]
[fl>= fl-comp]
[fl> fl-comp]
[fl< fl-comp]

;; safe flvector ops

[flvector? (make-pred-ty -FlVector)]
[flvector (->* (list) -Flonum -FlVector)]
[make-flvector (-> -Integer -Flonum -FlVector)]
[flvector-length (-> -FlVector -Nat)]
[flvector-ref (-> -FlVector -Nat -Flonum)]
[flvector-set! (-> -FlVector -Nat -Flonum -Void)]

;; unsafe flvector ops

[unsafe-flvector (->* (list) -Flonum -FlVector)]
[unsafe-make-flvector (-> -Integer -Flonum -FlVector)]
[unsafe-flvector-length (-> -FlVector -Nat)]
[unsafe-flvector-ref (-> -FlVector -Nat -Flonum)]
[unsafe-flvector-set! (-> -FlVector -Nat -Flonum -Void)]
