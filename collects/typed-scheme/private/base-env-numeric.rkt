#lang s-exp "env-lang.rkt"

(begin
  (require
   scheme/tcp
   scheme scheme/flonum scheme/fixnum
   scheme/unsafe/ops
   (only-in rnrs/lists-6 fold-left)
   '#%paramz
   "extra-procs.rkt"
   (only-in '#%kernel [apply kernel:apply])
   scheme/promise scheme/system
   (only-in string-constants/private/only-once maybe-print-message)
   (only-in racket/match/runtime match:error matchable? match-equality-test)
   (for-syntax (only-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym] [-Real R] [-ExactPositiveInteger -Pos])))
  
  (define-for-syntax all-num-types (list -Pos -Nat -Integer -ExactRational -Flonum -Real N))

  (define-for-syntax binop 
    (lambda (t [r t])
      (t t . -> . r)))
  (define-for-syntax rounder 
    (cl->* (-> -PositiveFixnum -PositiveFixnum)
           (-> -NonnegativeFixnum -NonnegativeFixnum)
           (-> -Fixnum -Fixnum)
           (-> -Pos -Pos)
           (-> -Nat -Nat)
           (-> -ExactRational -Integer)
           (-> -Flonum -Flonum)
           (-> -Real -Real)))
  
  (define-for-syntax (unop t) (-> t t))
  
  (define-for-syntax fl-comp (binop -Flonum B))
  (define-for-syntax fl-op (binop -Flonum))
  (define-for-syntax fl-unop (unop -Flonum))
  
  
  (define-for-syntax int-op (binop -Integer))
  (define-for-syntax nat-op (binop -Nat))
  
  (define-for-syntax fx-comp (binop -Integer B))
  (define-for-syntax fx-op (cl->* (-Pos -Pos . -> . -PositiveFixnum)
                                  (-Nat -Nat . -> . -NonnegativeFixnum)
                                  (-Integer -Integer . -> . -Fixnum)))
  (define-for-syntax fx-intop (-Integer -Integer . -> . -Fixnum))
  (define-for-syntax fx-unop (-Integer . -> . -Fixnum))

  (define-for-syntax real-comp (->* (list R R) R B))
  )

;; numeric predicates
[zero? (asym-pred N B (-FS (-filter (Un -Flonum -Zero) 0)
                           (-not-filter -Zero 0)))]
[number? (make-pred-ty N)]
[integer? (asym-pred Univ B (-FS (-filter (Un -Integer -Flonum) 0)
				     (-not-filter -Integer 0)))]
[exact-integer? (make-pred-ty -Integer)]
[real? (make-pred-ty -Real)]
[inexact-real? (make-pred-ty -Flonum)]
[complex? (make-pred-ty N)]
[rational? (make-pred-ty -Real)]
[exact? (asym-pred N B (-FS -top (-not-filter -ExactRational 0)))]
[inexact? (asym-pred N B  (-FS -top (-not-filter -Flonum 0)))]
[fixnum? (make-pred-ty -Fixnum)]
[positive? (-> -Real B)]
[negative? (-> -Real B)]
[exact-positive-integer? (make-pred-ty -Pos)]
[exact-nonnegative-integer? (make-pred-ty -Nat)]

[odd? (-> -Integer B)]
[even? (-> -Integer B)]

[modulo (cl->* (-NonnegativeFixnum -NonnegativeFixnum . -> . -NonnegativeFixnum)
               (-Fixnum -Fixnum . -> . -Fixnum)
               (-Nat -Nat . -> . -Nat)
               (-Integer -Integer . -> . -Integer))]

[=  (->* (list N N) N B)]

[>= real-comp]
[<  (cl->* 
     (-> -Nat -Integer B : (-FS (-filter -Pos 1) -top))
     (-> -Integer -Nat B : (-FS -top (-filter -Nat 0)))
     (-> -Integer (-val 0) B : (-FS -top (-filter -Nat 0)))
     real-comp)]
[<= (cl->* 
     (-> -Nat -Integer B : (-FS (-filter -Nat 1) -top))
     (-> -Pos -Integer B : (-FS (-filter -Pos 1) -top))
     real-comp)]
[>  real-comp]


[* (apply cl->*
          (append (for/list ([t (list -Pos -Nat -Integer -ExactRational -Flonum)]) (->* (list) t t))
                  (list (->* (list) -Real -Real))
                  (list (->* (list) N N))))]
[+ (apply cl->*
          (append (for/list ([t (list -Pos -Nat -Integer -ExactRational -Flonum)]) (->* (list) t t))
                  ;; special cases for promotion to inexact, not exhaustive
                  ;; valid for + and -, but not for * and /, since (* <float> 0) is exact 0 (i.e. not a float)
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real -Flonum) -Real -Flonum))
                  (list (->* (list) -Real -Real))
                  (list (->* (list) N N))))]

[- (apply cl->*
          (append (for/list ([t (list -Integer -ExactRational -Flonum)])
                            (->* (list t) t t))
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real -Flonum) -Real -Flonum))
                  (list (->* (list -Real) -Real -Real))
                  (list (->* (list N) N N))))]
[/ (apply cl->*
          (append (list (->* (list -Integer) -Integer -ExactRational))
                  (for/list ([t (list -ExactRational -Flonum)])
                            (->* (list t) t t))
                  ;; only exact 0 as first argument can cause the result of a division involving inexacts to be exact
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real) -Real -Real))
                  (list (->* (list N) N N))))]

[max (cl->* (->* (list -PositiveFixnum) -Fixnum -PositiveFixnum)
            (->* (list -NonnegativeFixnum) -Fixnum -NonnegativeFixnum)
            (->* (list -NegativeFixnum) -NegativeFixnum -NegativeFixnum)
            (->* (list -Fixnum) -Fixnum -Fixnum)
            (->* (list -Pos) -Integer -Pos)
            (->* (list -Nat) -Integer -Nat)
            (->* (list -Integer) -Integer -Integer)
            (->* (list -ExactRational) -ExactRational -ExactRational)
            (->* (list -Flonum) -Flonum -Flonum)
            (->* (list -Real) -Real -Real))]
[min (cl->* (->* (list -PositiveFixnum) -PositiveFixnum -PositiveFixnum)
            (->* (list -NonnegativeFixnum) -NonnegativeFixnum -NonnegativeFixnum)
            (->* (list -NegativeFixnum) -Fixnum -NegativeFixnum)
            (->* (list -Fixnum) -NegativeFixnum -NegativeFixnum)
            (->* (list -Fixnum) -Fixnum -Fixnum)
            (->* (list -Pos) -Pos -Pos)
            (->* (list -Nat) -Nat -Nat)
            (->* (list -Integer) -Integer -Integer)
            (->* (list -ExactRational) -ExactRational -ExactRational)
            (->* (list -Flonum) -Flonum -Flonum)
            (->* (list -Real) -Real -Real))]


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

[quotient (cl->* (-NonnegativeFixnum -NonnegativeFixnum . -> . -NonnegativeFixnum)
                 (-Fixnum -Fixnum . -> . -Fixnum)
                 (-Nat -Nat . -> . -Nat)
                 (-Integer -Integer . -> . -Integer))]
[remainder (cl->* (-NonnegativeFixnum -NonnegativeFixnum . -> . -NonnegativeFixnum)
                  (-Fixnum -Fixnum . -> . -Fixnum)
                  (-Nat -Nat . -> . -Nat)
                  (-Integer -Integer . -> . -Integer))]
[quotient/remainder (cl->* (-NonnegativeFixnum -NonnegativeFixnum . -> . (-values (list -NonnegativeFixnum -NonnegativeFixnum)))
                           (-Fixnum -Fixnum . -> . (-values (list -Fixnum -Fixnum)))
                           (-Nat -Nat . -> . (-values (list -Nat -Nat)))
                           (-Integer -Integer . -> . (-values (list -Integer -Integer))))]

[arithmetic-shift (cl->* (-Fixnum (Un -NegativeFixnum (-val 0)) . -> . -Fixnum)
                         (-Nat -Nat . -> . -Nat)
                         (-Integer -Integer . -> . -Integer))]
[bitwise-and (cl->* (null -NonnegativeFixnum . ->* . -NonnegativeFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    (null -Nat . ->* . -Nat)
                    (null -Integer . ->* . -Integer))]
[bitwise-ior (cl->* (null -NonnegativeFixnum . ->* . -NonnegativeFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    (null -Nat . ->* . -Nat)
                    (null -Integer . ->* . -Integer))]
[bitwise-not (cl->* (null -Fixnum . ->* . -Fixnum)
                    (null -Integer . ->* . -Integer))]
[bitwise-xor (cl->* (null -NonnegativeFixnum . ->* . -NonnegativeFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    (null -Nat . ->* . -Nat)
                    (null -Integer . ->* . -Integer))]
[bitwise-bit-set? (-> -Integer -Integer B)]
[bitwise-bit-field (-> -Integer -Integer -Integer -Integer)]
[integer-length (-> -Integer -NonnegativeFixnum)]

[abs (cl->* (-Fixnum . -> . -NonnegativeFixnum)
            (-Integer . -> . -Nat)
            (-Real . -> . -Real))]

;; exactness
[exact->inexact (cl->* 
                 (-Real . -> . -Flonum)
                 (N . -> . N))]
[inexact->exact (cl->*
                 (-Real . -> . -ExactRational)
                 (N . -> . N))]

[floor rounder]
[ceiling rounder]
[truncate rounder]
[round rounder]
[make-rectangular (-Real -Real . -> . N)]
[make-polar (-Real -Real . -> . N)]
[real-part (N . -> . -Real)]
[imag-part (N . -> . -Real)]
[magnitude (N . -> . -Real)]
[angle     (N . -> . -Real)]
[numerator   (-Real . -> . -Real)]
[denominator (-Real . -> . -Real)]
[rationalize (-Real -Real . -> . N)]
[expt (cl->* (-Nat -Nat . -> . -Nat)
             (-Integer -Nat . -> . -Integer)
             (-Real -Integer . -> . -Real)
             (N N . -> . N))]
[sqrt (cl->*
       (-Nat . -> . -Real)
       (N . -> . N))]
[log (cl->*
      (-Pos . -> . -Real)
      (N . -> . N))]
[exp  (cl->* (-Real . -> . -Real)
             (N . -> . N))]
[cos  (cl->* (-Flonum . -> . -Flonum) (-Real . -> . -Real) (N . -> . N))]
[sin  (cl->* (-Flonum . -> . -Flonum) (-Real . -> . -Real) (N . -> . N))]
[tan  (cl->* (-Flonum . -> . -Flonum) (-Real . -> . -Real) (N . -> . N))]
[acos (cl->* (-Flonum . -> . -Flonum) (-Real . -> . -Real) (N . -> . N))]
[asin (cl->* (-Flonum . -> . -Flonum) (-Real . -> . -Real) (N . -> . N))]
[atan (cl->* (-Flonum . -> . -Flonum) (-Real . -> . -Real) (N . -> . N) (-Real -Real . -> . N))]
[gcd  (cl->* (null -Fixnum . ->* . -Fixnum) (null -Integer . ->* . -Integer))]
[lcm  (null -Integer . ->* . -Integer)]

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
[unsafe-fl+ fl-op]
[unsafe-fl- fl-op]
[unsafe-fl* fl-op]
[unsafe-fl/ fl-op]
[unsafe-fl= fl-comp]
[unsafe-fl<= fl-comp]
[unsafe-fl>= fl-comp]
[unsafe-fl> fl-comp]
[unsafe-fl< fl-comp]
[unsafe-flmin fl-op]
[unsafe-flmax fl-op]
[unsafe-flround fl-unop]
[unsafe-flfloor fl-unop]
[unsafe-flceiling fl-unop]
[unsafe-fltruncate fl-unop]
[unsafe-flsin fl-unop]
[unsafe-flcos fl-unop]
[unsafe-fltan fl-unop]
[unsafe-flatan fl-unop]
[unsafe-flasin fl-unop]
[unsafe-flacos fl-unop]
[unsafe-fllog fl-unop]
[unsafe-flexp fl-unop]
[unsafe-flsqrt fl-unop]
[unsafe-fx->fl (-Integer . -> . -Flonum)]

[unsafe-fx+ fx-op]
[unsafe-fx- fx-intop]
[unsafe-fx* fx-op]
[unsafe-fxquotient fx-intop]
[unsafe-fxremainder fx-intop]
[unsafe-fxmodulo fx-intop]
[unsafe-fxabs (-Integer . -> . (Un -PositiveFixnum (-val 0)))]

[unsafe-fxand fx-op]
[unsafe-fxior fx-op]
[unsafe-fxxor fx-op]
[unsafe-fxnot fx-unop]
[unsafe-fxlshift fx-intop]
[unsafe-fxrshift (cl->* (-> -NonnegativeFixnum -NonnegativeFixnum -NonnegativeFixnum) fx-intop)]

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
[fxabs (-Integer . -> . (Un -PositiveFixnum (-val 0)))]

[fxand fx-op]
[fxior fx-op]
[fxxor fx-op]
[fxnot fx-unop]
[fxlshift fx-intop]
[fxrshift (cl->* (-> -NonnegativeFixnum -NonnegativeFixnum -NonnegativeFixnum) fx-intop)]

[fx= fx-comp]
[fx< fx-comp]
[fx> fx-comp]
[fx<= fx-comp]
[fx>= fx-comp]
[fxmin fx-op]
[fxmax fx-op]


;; safe flonum ops
[flabs fl-unop]
[fl+ fl-op]
[fl- fl-op]
[fl* fl-op]
[fl/ fl-op]
[fl= fl-comp]
[fl<= fl-comp]
[fl>= fl-comp]
[fl> fl-comp]
[fl< fl-comp]
[flmin fl-op]
[flmax fl-op]
[flround fl-unop]
[flfloor fl-unop]
[flceiling fl-unop]
[fltruncate fl-unop]
[flsin fl-unop]
[flcos fl-unop]
[fltan fl-unop]
[flatan fl-unop]
[flasin fl-unop]
[flacos fl-unop]
[fllog fl-unop]
[flexp fl-unop]
[flsqrt fl-unop]
[->fl (-Integer . -> . -Flonum)]

;; safe flvector ops

[flvector? (make-pred-ty -FlVector)]
[flvector (->* (list) -Flonum -FlVector)]
[make-flvector (cl->* (-> -Integer -FlVector)
                      (-> -Integer -Flonum -FlVector))]
[flvector-length (-> -FlVector -NonnegativeFixnum)]
[flvector-ref (-> -FlVector -Integer -Flonum)]
[flvector-set! (-> -FlVector -Integer -Flonum -Void)]

;; unsafe flvector ops

[unsafe-flvector-length (-> -FlVector -NonnegativeFixnum)]
[unsafe-flvector-ref (-> -FlVector -Integer -Flonum)]
[unsafe-flvector-set! (-> -FlVector -Integer -Flonum -Void)]
