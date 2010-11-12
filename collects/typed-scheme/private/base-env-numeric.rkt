#lang s-exp "env-lang.rkt"

(begin
  (require
   (for-template racket/flonum racket/fixnum racket/math racket/unsafe/ops racket/base)
   (only-in (types abbrev) [-Number N] [-Boolean B] [-Symbol Sym] [-Real R] [-ExactPositiveInteger -Pos]))
  
  (define all-num-types (list -Pos -Nat -Integer -ExactRational -Flonum -InexactReal -Real N))

  (define binop 
    (lambda (t [r t])
      (t t . -> . r)))
  (define rounder 
    (cl->* (-> -PositiveFixnum -PositiveFixnum)
           (-> -NonnegativeFixnum -NonnegativeFixnum)
           (-> -Fixnum -Fixnum)
           (-> -Pos -Pos)
           (-> -Nat -Nat)
           (-> -ExactRational -Integer)
           (-> -NonnegativeFlonum -NonnegativeFlonum)
           (-> -Flonum -Flonum)
           (-> -InexactReal -InexactReal)
           (-> -Real -Real)))
  
  (define (unop t) (-> t t))
  
  (define fl-comp (binop -Flonum B))
  (define fl-op (binop -Flonum))
  (define fl-unop (unop -Flonum))
  (define fl-rounder
    (cl->* (-> -NonnegativeFlonum -NonnegativeFlonum)
           (-> -Flonum -Flonum)))
  
  (define int-op (binop -Integer))
  (define nat-op (binop -Nat))
  
  (define fx-comp (binop -Integer B))
  (define fx-op (cl->* (-Pos -Pos . -> . -PositiveFixnum)
                                  (-Nat -Nat . -> . -NonnegativeFixnum)
                                  (-Integer -Integer . -> . -Fixnum)))
  (define fx-natop (cl->* (-Nat -Nat . -> . -NonnegativeFixnum)
                                     (-Integer -Integer . -> . -Fixnum)))
  (define fx-unop (-Integer . -> . -Fixnum))

  (define real-comp (->* (list R R) R B))

  ;; types for specific operations, to avoid repetition between safe and unsafe versions
  (define fx+-type
    (cl->* (-Pos -Nat . -> . -PositiveFixnum)
           (-Nat -Pos . -> . -PositiveFixnum)
           (-Nat -Nat . -> . -NonnegativeFixnum)
           (-Integer -Integer . -> . -Fixnum)))
  (define fx--type
    (-Integer -Integer . -> . -Fixnum))
  (define fx=-type
    (cl->*
     (-> -Integer (-val 0) B : (-FS (-filter (-val 0) 0) -top))
     (-> (-val 0) -Integer B : (-FS (-filter (-val 0) 1) -top))
     (-> -Integer -Pos B : (-FS (-filter -PositiveFixnum 0) -top))
     (-> -Pos -Integer B : (-FS (-filter -PositiveFixnum 1) -top))
     (-> -Integer -Nat B : (-FS (-filter -NonnegativeFixnum 0) -top))
     (-> -Nat -Integer B : (-FS (-filter -NonnegativeFixnum 1) -top))
     (-> -Integer -NegativeFixnum B : (-FS (-filter -NegativeFixnum 0) -top))
     (-> -NegativeFixnum -Integer B : (-FS (-filter -NegativeFixnum 1) -top))
     fx-comp))
  (define fx<-type
    (cl->*
     (-> -Integer (-val 0) B : (-FS (-filter -NegativeFixnum 0) (-filter -NonnegativeFixnum 0)))
     (-> -Integer -NegativeFixnum B : (-FS (-filter -NegativeFixnum 0) -top))
     (-> -Nat -Integer B : (-FS (-filter -PositiveFixnum 1) -top))
     fx-comp))
  (define fx>-type
    (cl->*
     (-> -Integer (-val 0) B : (-FS (-filter -PositiveFixnum 0) -top))
     (-> -NegativeFixnum -Integer B : (-FS (-filter -NegativeFixnum 1) -top))
     (-> -Integer -Nat B : (-FS (-filter -PositiveFixnum 0) -top))
     fx-comp))
  (define fx<=-type
    (cl->*
     (-> -Integer (-val 0) B : (-FS -top (-filter -PositiveFixnum 0)))
     (-> -Integer -NegativeFixnum B : (-FS (-filter -NegativeFixnum 0) -top))
     (-> -Pos -Integer B : (-FS (-filter -Pos 1) -top))
     (-> -Nat -Integer B : (-FS (-filter -Nat 1) -top))
     fx-comp))
  (define fx>=-type
    (cl->*
     (-> -Integer (-val 0) B : (-FS (-filter -NonnegativeFixnum 0) -top))
     (-> -NegativeFixnum -Integer B : (-FS (-filter -NegativeFixnum 1) -top))
     (-> -Integer -Pos B : (-FS (-filter -Pos 0) -top))
     (-> -Integer -Nat B : (-FS (-filter -Nat 0) -top))
     fx-comp))
  (define fxmin-type
    (cl->*
     (-> -NegativeFixnum -Integer -NegativeFixnum)
     (-> -Integer -NegativeFixnum -NegativeFixnum)
     (-> -Pos -Pos -PositiveFixnum)
     (-> -Nat -Nat -NonnegativeFixnum)
     (-> -Integer -Integer -Fixnum)))
  (define fxmax-type
    (cl->*
     (-> -NegativeFixnum -NegativeFixnum -NegativeFixnum)
     (-> -Pos -Integer -PositiveFixnum)
     (-> -Integer -Pos -PositiveFixnum)
     (-> -Nat -Integer -NonnegativeFixnum)
     (-> -Integer -Nat -NonnegativeFixnum)
     (-> -Integer -Integer -Fixnum)))

  (define fl+*-type
    (cl->* (-NonnegativeFlonum -NonnegativeFlonum . -> . -NonnegativeFlonum)
           (-Flonum -Flonum . -> . -Flonum)))
  (define fl=-type
    (cl->*
     (-> -Flonum -NonnegativeFlonum B : (-FS (-filter -NonnegativeFlonum 0) -top))
     (-> -NonnegativeFlonum -Flonum B : (-FS (-filter -NonnegativeFlonum 1) -top))
     fl-comp))
  (define fl<-type
    (cl->*
     (-> -NonnegativeFlonum -Flonum B : (-FS (-filter -NonnegativeFlonum 1) -top))
     fl-comp))
  (define fl>-type
    (cl->*
     (-> -Flonum -NonnegativeFlonum B : (-FS (-filter -NonnegativeFlonum 0) -top))
     fl-comp))
  (define flmin-type
    (cl->* (-> -NonnegativeFlonum -NonnegativeFlonum -NonnegativeFlonum)
           (-> -Flonum -Flonum -Flonum)))
  (define flmax-type
    (cl->* (-> -NonnegativeFlonum -Flonum -NonnegativeFlonum)
           (-> -Flonum -NonnegativeFlonum -NonnegativeFlonum)
           (-> -Flonum -Flonum -Flonum)))
  )

;; numeric predicates
[zero? (asym-pred N B (-FS (-filter (Un -NonnegativeFlonum -Zero) 0)
                           (-not-filter -Zero 0)))]
[number? (make-pred-ty N)]
[integer? (asym-pred Univ B (-FS (-filter (Un -Integer -Flonum) 0)
                                 (-not-filter -Integer 0)))]
[exact-integer? (make-pred-ty -Integer)]
[real? (make-pred-ty -Real)]
[flonum? (make-pred-ty -Flonum)]
[inexact-real? (make-pred-ty -InexactReal)]
[complex? (make-pred-ty N)]
[rational? (make-pred-ty -Real)]
[exact? (asym-pred N B (-FS -top (-not-filter -ExactRational 0)))]
[inexact? (asym-pred N B  (-FS -top (-not-filter (Un -InexactReal -FloatComplex) 0)))]
[fixnum? (make-pred-ty -Fixnum)]
[positive? (cl->* (-> -Fixnum B : (-FS (-filter -PositiveFixnum 0) -top))
                  (-> -Integer B : (-FS (-filter -ExactPositiveInteger 0) -top))
                  (-> -Flonum B : (-FS (-filter -NonnegativeFlonum 0) -top))
                  (-> -Real B))]
[negative? (cl->* (-> -Fixnum B : (-FS (-filter -NegativeFixnum 0) (-filter -NonnegativeFixnum 0)))
                  (-> -Integer B : (-FS -top (-filter -Nat 0)))
                  (-> -Flonum B : (-FS -top (-filter -NonnegativeFlonum 0)))
                  (-> -Real B))]
[exact-positive-integer? (make-pred-ty -Pos)]
[exact-nonnegative-integer? (make-pred-ty -Nat)]

[odd? (-> -Integer B : (-FS -top (-filter (-val 0) 0)))]
[even? (-> -Integer B)]

[modulo (cl->* (-Nat -NonnegativeFixnum . -> . -NonnegativeFixnum)
               (-Integer -Fixnum . -> . -Fixnum)
               (-Nat -Nat . -> . -Nat)
               (-Integer -Integer . -> . -Integer))]

[=  (cl->*
     (-> -Integer (-val 0) B : (-FS (-filter (-val 0) 0) -top))
     (-> (-val 0) -Integer B : (-FS (-filter (-val 0) 1) -top))
     (-> -Integer -PositiveFixnum B : (-FS (-filter -PositiveFixnum 0) -top))
     (-> -PositiveFixnum -Integer B : (-FS (-filter -PositiveFixnum 1) -top))
     (-> -Integer -NonnegativeFixnum B : (-FS (-filter -NonnegativeFixnum 0) -top))
     (-> -NonnegativeFixnum -Integer B : (-FS (-filter -NonnegativeFixnum 1) -top))
     (-> -Integer -NegativeFixnum B : (-FS (-filter -NegativeFixnum 0) -top))
     (-> -NegativeFixnum -Integer B : (-FS (-filter -NegativeFixnum 1) -top))
     (-> -Integer -Pos B : (-FS (-filter -Pos 0) -top))
     (-> -Pos -Integer B : (-FS (-filter -Pos 1) -top))
     (-> -Integer -Nat B : (-FS (-filter -Nat 0) -top))
     (-> -Nat -Integer B : (-FS (-filter -Nat 1) -top))
     (->* (list N N) N B))]

[>  (cl->*
     (-> -Fixnum (-val 0) B : (-FS (-filter -PositiveFixnum 0) -top))
     (-> -Integer (-val 0) B : (-FS (-filter -Pos 0) -top))
     (-> -NegativeFixnum -Fixnum B : (-FS (-filter -NegativeFixnum 1) -top))
     (-> -Fixnum -NonnegativeFixnum B : (-FS (-filter -PositiveFixnum 1) -top))
     (-> -Fixnum -Nat B : (-FS (-filter -Fixnum 1) -top))
     (-> -Integer -Nat B : (-FS (-filter -ExactPositiveInteger 0) -top))
     (-> -Flonum -NonnegativeFlonum B : (-FS (-filter -NonnegativeFlonum 0) -top))
     (-> -NonnegativeFlonum -Flonum B : (-FS -top (-filter -NonnegativeFlonum 1)))
     real-comp)]
[>= (cl->*
     (-> -Fixnum (-val 0) B : (-FS (-filter -NonnegativeFixnum 0) (-filter -NegativeFixnum 0)))
     (-> -Integer (-val 0) B : (-FS (-filter -ExactNonnegativeInteger 0) -top))
     (-> -Fixnum -PositiveFixnum B : (-FS (-filter -PositiveFixnum 0) -top))
     (-> -Fixnum -NonnegativeFixnum B : (-FS (-filter -NonnegativeFixnum 0) -top))
     (-> -Fixnum -Pos B : (-FS (-filter -PositiveFixnum 1) -top))
     (-> -Fixnum -Nat B : (-FS (-filter -NonnegativeFixnum 1) -top))
     (-> -Integer -Pos B : (-FS (-filter -Pos 0) -top))
     (-> -Integer -Nat B : (-FS (-filter -Nat 0) -top))
     (-> -Flonum -NonnegativeFlonum B : (-FS (-filter -NonnegativeFlonum 0) -top))
     (-> -NonnegativeFlonum -Flonum B : (-FS -top (-filter -NonnegativeFlonum 1)))
     real-comp)]
[<  (cl->*
     (-> -Fixnum (-val 0) B : (-FS (-filter -NegativeFixnum 0) (-filter -NonnegativeFixnum 0)))
     (-> -Integer (-val 0) B : (-FS -top (-filter -ExactNonnegativeInteger 0)))
     (-> -NonnegativeFixnum -Fixnum B : (-FS (-filter -PositiveFixnum 1) -top))
     (-> -Fixnum -NegativeFixnum B : (-FS (-filter -NegativeFixnum 0) -top))
     (-> -Nat -Fixnum B : (-FS (-filter -NonnegativeFixnum 0) -top))
     (-> -Nat -Integer B : (-FS (-filter -Pos 1) -top))
     (-> -Integer -Nat B : (-FS -top (-filter -Nat 0)))
     (-> -NonnegativeFlonum -Flonum B : (-FS (-filter -NonnegativeFlonum 1) -top))
     (-> -Flonum -NonnegativeFlonum B : (-FS -top (-filter -NonnegativeFlonum 0)))
     real-comp)]
[<= (cl->*
     (-> -Fixnum (-val 0) B : (-FS -top (-filter -PositiveFixnum 0)))
     (-> -Integer (-val 0) B : (-FS -top (-filter -ExactPositiveInteger 0)))
     (-> -PositiveFixnum -Fixnum B : (-FS (-filter -PositiveFixnum 1) -top))
     (-> -NonnegativeFixnum -Fixnum B : (-FS (-filter -NonnegativeFixnum 1) -top))
     (-> -Pos -Fixnum B : (-FS (-filter -PositiveFixnum 0) -top))
     (-> -Nat -Fixnum B : (-FS (-filter -NonnegativeFixnum 0) -top))
     (-> -Pos -Integer B : (-FS (-filter -Pos 1) -top))
     (-> -Nat -Integer B : (-FS (-filter -Nat 1) -top))
     (-> -NonnegativeFlonum -Flonum B : (-FS (-filter -NonnegativeFlonum 1) -top))
     (-> -Flonum -NonnegativeFlonum B : (-FS -top (-filter -NonnegativeFlonum 0)))
     real-comp)]


[* (apply cl->*
          (append (for/list ([t (list -Pos -Nat -Integer -ExactRational -NonnegativeFlonum -Flonum)]) (->* (list) t t))
                  (list (->* (list) (Un -Pos -NonnegativeFlonum) -NonnegativeFlonum))
                  (list (->* (list) (Un -Pos -Flonum) -Flonum))
                  (list (->* (list -Flonum) (Un -InexactReal -Flonum) -Flonum))
                  (list (->* (list -InexactReal -Flonum) (Un -InexactReal -Flonum) -Flonum))
                  (list (->* (list) -InexactReal -InexactReal))
                  (list (->* (list) -Real -Real))
                  (list (->* (list) (Un -FloatComplex -Flonum) -FloatComplex))
                  (list (->* (list) N N))))]
[+ (apply cl->*
          (append (list (->* (list -Pos) -Nat -Pos))
                  (list (->* (list -Nat -Pos) -Nat -Pos))
                  (for/list ([t (list -Nat -Integer -ExactRational -NonnegativeFlonum -Flonum)]) (->* (list) t t))
                  ;; special cases for promotion to inexact, not exhaustive
                  ;; valid for + and -, but not for * and /, since (* <float> 0) is exact 0 (i.e. not a float)
                  (list (->* (list) (Un -Nat -NonnegativeFlonum) -NonnegativeFlonum))
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real -Flonum) -Real -Flonum))
                  (list (->* (list) -InexactReal -InexactReal))
                  (list (->* (list) -Real -Real))
                  (list (->* (list) (Un -Real -FloatComplex) -FloatComplex))
                  (list (->* (list -FloatComplex) N -FloatComplex))
                  (list (->* (list N -FloatComplex) N -FloatComplex))
                  (list (->* (list) N N))))]

[- (apply cl->*
          (append (for/list ([t (list -Integer -ExactRational -Flonum)])
                            (->* (list t) t t))
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real -Flonum) -Real -Flonum))
                  (list (->* (list -InexactReal) -InexactReal -InexactReal))
                  (list (->* (list -Real) -Real -Real))
                  (list (->* (list) (Un -Real -FloatComplex) -FloatComplex))
                  (list (->* (list -FloatComplex) N -FloatComplex))
                  (list (->* (list N -FloatComplex) N -FloatComplex))
                  (list (->* (list N) N N))))]
[/ (apply cl->*
          (append (list (->* (list -Integer) -Integer -ExactRational))
                  (for/list ([t (list -ExactRational -Flonum)])
                            (->* (list t) t t))
                  ;; only exact 0 as first argument can cause the result of a division involving inexacts to be exact
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -InexactReal -Flonum) -InexactReal -Flonum))
                  (list (->* (list -InexactReal) -InexactReal -InexactReal))
                  (list (->* (list -Real) -Real -Real))
                  (list (->* (list (Un -Flonum -FloatComplex)) (Un -Real -FloatComplex) -FloatComplex))
                  (list (->* (list -FloatComplex) -FloatComplex -FloatComplex))
                  (list (->* (list N) N N))))]

[max (cl->* (->* (list -PositiveFixnum) -Fixnum -PositiveFixnum)
            (->* (list -NonnegativeFixnum) -Fixnum -NonnegativeFixnum)
            (->* (list -NegativeFixnum) -NegativeFixnum -NegativeFixnum)
            (->* (list -Fixnum) -Fixnum -Fixnum)
            (->* (list -Pos) -Integer -Pos)
            (->* (list -Nat) -Integer -Nat)
            (->* (list -Integer) -Integer -Integer)
            (->* (list -ExactRational) -ExactRational -ExactRational)
            (->* (list -NonnegativeFlonum) -Flonum -NonnegativeFlonum)
            (->* (list -Flonum) -Flonum -Flonum)
            (->* (list -InexactReal) -InexactReal -InexactReal)
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
            (->* (list -NonnegativeFlonum) -NonnegativeFlonum -NonnegativeFlonum)
            (->* (list -Flonum) -Flonum -Flonum)
            (->* (list -InexactReal) -InexactReal -InexactReal)
            (->* (list -Real) -Real -Real))]


[add1 (cl->* (-> -Pos -Pos)
             (-> -Nat -Pos)
             (-> -Integer -Integer)
             (-> -ExactRational -ExactRational)
             (-> -NonnegativeFlonum -NonnegativeFlonum)
             (-> -Flonum -Flonum)
             (-> -InexactReal -InexactReal)
             (-> -Real -Real)
             (-> -FloatComplex -FloatComplex)
             (-> N N))]

[sub1 (cl->* (-> -Pos -Nat)
             (-> -Integer -Integer)
             (-> -ExactRational -ExactRational)
             (-> -Flonum -Flonum)
             (-> -InexactReal -InexactReal)
             (-> -Real -Real)
             (-> -FloatComplex -FloatComplex)
             (-> N N))]

[quotient (cl->* (-NonnegativeFixnum -NonnegativeFixnum . -> . -NonnegativeFixnum)
                 (-Fixnum -Fixnum . -> . -Fixnum)
                 (-Nat -Nat . -> . -Nat)
                 (-Integer -Integer . -> . -Integer))]
[remainder (cl->* (-Nat -NonnegativeFixnum . -> . -NonnegativeFixnum)
                  (-Integer -Fixnum . -> . -Fixnum)
                  (-Nat -Nat . -> . -Nat)
                  (-Integer -Integer . -> . -Integer))]
[quotient/remainder (cl->* (-NonnegativeFixnum -NonnegativeFixnum . -> . (-values (list -NonnegativeFixnum -NonnegativeFixnum)))
                           (-Nat -NonnegativeFixnum . -> . (-values (list -Nat -NonnegativeFixnum)))
                           (-Fixnum -Fixnum . -> . (-values (list -Fixnum -Fixnum)))
                           (-Integer -Fixnum . -> . (-values (list -Integer -Fixnum)))
                           (-Nat -Nat . -> . (-values (list -Nat -Nat)))
                           (-Integer -Integer . -> . (-values (list -Integer -Integer))))]

[arithmetic-shift (cl->* (-Fixnum (Un -NegativeFixnum (-val 0)) . -> . -Fixnum)
                         (-Nat -Nat . -> . -Nat)
                         (-Integer -Integer . -> . -Integer))]
[bitwise-and (cl->* (null -NonnegativeFixnum . ->* . -NonnegativeFixnum)
                    ((list -Integer) -NonnegativeFixnum . ->* . -NonnegativeFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    ((list -Integer) -Fixnum . ->* . -Fixnum)
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

[abs (cl->* (-PositiveFixnum . -> . -PositiveFixnum)
            (-Fixnum . -> . -NonnegativeFixnum)
            (-Pos . -> . -Pos)
            (-Integer . -> . -Nat)
            (-Flonum . -> . -NonnegativeFlonum)
            (-InexactReal . -> . -InexactReal)
            (-Real . -> . -Real))]

;; exactness
[exact->inexact (cl->*
                 (-Flonum . -> . -Flonum)           ; no conversion
                 (-InexactReal . -> . -InexactReal) ; no conversion
                 (-Real . -> . -Flonum)
                 (N . -> . -FloatComplex))]
[inexact->exact (cl->*
                 (-Real . -> . -ExactRational)
                 (N . -> . N))]

[floor rounder]
[ceiling rounder]
[truncate rounder]
[round rounder]
[make-rectangular (cl->* (-Flonum -Flonum . -> . -FloatComplex)
                         (-Real -Real . -> . N))]
[make-polar (cl->* (-Flonum -Flonum . -> . -FloatComplex)
                   (-Real -Real . -> . N))]
[real-part (cl->* (-FloatComplex . -> . -Flonum)
                  (N . -> . -Real))]
[imag-part (cl->* (-FloatComplex . -> . -Flonum)
                  (N . -> . -Real))]
[magnitude (cl->* (-FloatComplex . -> . -Flonum)
                  (N . -> . -Real))]
[angle     (cl->* (-FloatComplex . -> . -Flonum)
                  (N . -> . -Real))]
[numerator   (cl->* (-ExactRational . -> . -Integer)
                    (-Real . -> . -Real))]
[denominator (cl->* (-ExactRational . -> . -Integer)
                    (-Real . -> . -Real))]
[rationalize (cl->* (-ExactRational -ExactRational . -> . -ExactRational)
                    (-Flonum -Flonum . -> . -Flonum)
                    (-InexactReal -InexactReal . -> . -InexactReal)
                    (-Real -Real . -> . -Real))]
[expt (cl->* (-Nat -Nat . -> . -Nat)
             (-Integer -Nat . -> . -Integer)
             (-Integer -Integer . -> . -ExactRational)
             (-Real -Integer . -> . -Real)
             (-FloatComplex -FloatComplex . -> . -FloatComplex)
             (N N . -> . N))]
[sqrt (cl->*
       (-Nat . -> . -Real)
       (-NonnegativeFlonum . -> . -NonnegativeFlonum)
       (-FloatComplex . -> . -FloatComplex)
       (N . -> . N))]
[log (cl->*
      (-Pos . -> . -Real)
      (-FloatComplex . -> . -FloatComplex)
      (N . -> . N))]
[exp  (cl->* (-Flonum . -> . -Flonum)
             (-InexactReal . -> . -InexactReal)
             (-Real . -> . -Real)
             (-FloatComplex . -> . -FloatComplex)
             (N . -> . N))]
[cos  (cl->* (-Flonum . -> . -Flonum) (-InexactReal . -> . -InexactReal) (-Real . -> . -Real) (-FloatComplex . -> . -FloatComplex) (N . -> . N))]
[sin  (cl->* (-Flonum . -> . -Flonum) (-InexactReal . -> . -InexactReal) (-Real . -> . -Real) (-FloatComplex . -> . -FloatComplex) (N . -> . N))]
[tan  (cl->* (-Flonum . -> . -Flonum) (-InexactReal . -> . -InexactReal) (-Real . -> . -Real) (-FloatComplex . -> . -FloatComplex) (N . -> . N))]
[acos (cl->* (-Flonum . -> . -Flonum) (-InexactReal . -> . -InexactReal) (-Real . -> . -Real) (-FloatComplex . -> . -FloatComplex) (N . -> . N))]
[asin (cl->* (-Flonum . -> . -Flonum) (-InexactReal . -> . -InexactReal) (-Real . -> . -Real) (-FloatComplex . -> . -FloatComplex) (N . -> . N))]
[atan (cl->* (-Flonum . -> . -Flonum) (-InexactReal . -> . -InexactReal) (-Real . -> . -Real) (-FloatComplex . -> . -FloatComplex) (N . -> . N) (-Real -Real . -> . N))]
[gcd  (cl->* (null -Fixnum . ->* . -Fixnum) (null -Integer . ->* . -Integer))]
[lcm  (null -Integer . ->* . -Integer)]

;; scheme/math

[sgn (-Real . -> . -Real)]
[pi -NonnegativeFlonum]
[sqr (cl->* (-> -Pos -Pos)
            (-> -Integer -Nat)
            (-> -ExactRational -ExactRational)
            (-> -Flonum -NonnegativeFlonum)
            (-> -InexactReal -InexactReal)
            (-> -Real -Real)
            (-> -FloatComplex -FloatComplex)
            (-> N N))]
[conjugate (cl->* (-FloatComplex . -> . -FloatComplex)
                  (N . -> . N))]
[sinh (cl->* (-FloatComplex . -> . -FloatComplex)
             (N . -> . N))]
[cosh (cl->* (-FloatComplex . -> . -FloatComplex)
             (N . -> . N))]
[tanh (cl->* (-FloatComplex . -> . -FloatComplex)
             (N . -> . N))]

;; unsafe numeric ops
[unsafe-flabs (-> -Flonum -NonnegativeFlonum)]
[unsafe-fl+ fl+*-type]
[unsafe-fl- fl-op]
[unsafe-fl* fl+*-type]
[unsafe-fl/ fl-op]
[unsafe-fl= fl=-type]
[unsafe-fl<= fl<-type]
[unsafe-fl>= fl>-type]
[unsafe-fl> fl>-type]
[unsafe-fl< fl<-type]
[unsafe-flmin flmin-type]
[unsafe-flmax flmax-type]
[unsafe-flround fl-rounder]
[unsafe-flfloor fl-rounder]
[unsafe-flceiling fl-rounder]
[unsafe-fltruncate fl-rounder]
[unsafe-flsin fl-unop]
[unsafe-flcos fl-unop]
[unsafe-fltan fl-unop]
[unsafe-flatan fl-unop]
[unsafe-flasin fl-unop]
[unsafe-flacos fl-unop]
[unsafe-fllog fl-unop]
[unsafe-flexp fl-rounder]
[unsafe-flsqrt fl-rounder]
[unsafe-fx->fl (cl->* (-Nat . -> . -NonnegativeFlonum) (-Integer . -> . -Flonum))]
[unsafe-make-flrectangular (-Flonum -Flonum . -> . -FloatComplex)]
[unsafe-flreal-part (-FloatComplex . -> . -Flonum)]
[unsafe-flimag-part (-FloatComplex . -> . -Flonum)]

[unsafe-fx+ fx+-type]
[unsafe-fx- fx--type]
[unsafe-fx* fx-op]
[unsafe-fxquotient fx-natop]
[unsafe-fxremainder fx-natop]
[unsafe-fxmodulo fx-natop]
[unsafe-fxabs (-Integer . -> . (Un -PositiveFixnum (-val 0)))]

[unsafe-fxand fx-op]
[unsafe-fxior fx-op]
[unsafe-fxxor fx-op]
[unsafe-fxnot fx-unop]
[unsafe-fxlshift fx-natop]
[unsafe-fxrshift fx-natop]

[unsafe-fx= fx=-type]
[unsafe-fx< fx<-type]
[unsafe-fx> fx>-type]
[unsafe-fx<= fx<=-type]
[unsafe-fx>= fx>=-type]
[unsafe-fxmin fx-op]
[unsafe-fxmax fx-op]

;; scheme/fixnum

[fx+ fx+-type]
[fx- fx--type]
[fx* fx-op]
[fxquotient fx-natop]
[fxremainder fx-natop]
[fxmodulo fx-natop]
[fxabs (-Integer . -> . (Un -PositiveFixnum (-val 0)))]

[fxand fx-op]
[fxior fx-op]
[fxxor fx-op]
[fxnot fx-unop]
[fxlshift fx-natop]
[fxrshift fx-natop]

[fx= fx=-type]
[fx< fx<-type]
[fx> fx>-type]
[fx<= fx<=-type]
[fx>= fx>=-type]
[fxmin fx-op]
[fxmax fx-op]


;; safe flonum ops
[flabs (-> -Flonum -NonnegativeFlonum)]
[fl+ fl+*-type]
[fl- fl-op]
[fl* fl+*-type]
[fl/ fl-op]
[fl= fl=-type]
[fl<= fl<-type]
[fl>= fl>-type]
[fl> fl>-type]
[fl< fl<-type]
[flmin flmin-type]
[flmax flmax-type]
[flround fl-rounder]
[flfloor fl-rounder]
[flceiling fl-rounder]
[fltruncate fl-rounder]
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
[make-flrectangular (-Flonum -Flonum . -> . -FloatComplex)]
[flreal-part (-FloatComplex . -> . -Flonum)]
[flimag-part (-FloatComplex . -> . -Flonum)]

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
