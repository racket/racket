#lang s-exp "env-lang.rkt"

(begin
  (require
   (for-template racket/flonum racket/fixnum racket/math racket/unsafe/ops racket/base)
   (only-in (types abbrev numeric-tower) [-Number N] [-Boolean B] [-Symbol Sym] [-Real R] [-PosInt -Pos]))
  
  (define all-num-types (list -Pos -Nat -Int -Rat -Flonum -InexactReal -Real N))

  (define binop 
    (lambda (t [r t])
      (t t . -> . r)))
  (define rounder 
    (cl->* (-> -PosFixnum -PosFixnum)
           (-> -NonNegFixnum -NonNegFixnum)
           (-> -Fixnum -Fixnum)
           (-> -Pos -Pos)
           (-> -Nat -Nat)
           (-> -Rat -Int)
           (-> -NonNegFlonum -NonNegFlonum)
           (-> -NonPosFlonum -NonPosFlonum)
           (-> -Flonum -Flonum)
           (-> -InexactReal -InexactReal)
           (-> -Real -Real)))
  
  (define (unop t) (-> t t))
  
  (define fl-comp (binop -Flonum B))
  (define fl-op (binop -Flonum))
  (define fl-unop (unop -Flonum))
  (define fl-rounder
    (cl->* (-> -NonNegFlonum -NonNegFlonum)
           (-> -Flonum -Flonum)))
  
  (define int-op (binop -Int))
  (define nat-op (binop -Nat))
  
  (define fx-comp (binop -Int B))
  (define fx-op (cl->* (-Pos -Pos . -> . -PosFixnum)
                                  (-Nat -Nat . -> . -NonNegFixnum)
                                  (-Int -Int . -> . -Fixnum)))
  (define fx-natop (cl->* (-Nat -Nat . -> . -NonNegFixnum)
                                     (-Int -Int . -> . -Fixnum)))
  (define fx-unop (-Int . -> . -Fixnum))

  (define real-comp (->* (list R R) R B))

  ;; types for specific operations, to avoid repetition between safe and unsafe versions
  (define fx+-type
    (cl->* (-Pos -Nat . -> . -PosFixnum)
           (-Nat -Pos . -> . -PosFixnum)
           (-Nat -Nat . -> . -NonNegFixnum)
           (-Int -Int . -> . -Fixnum)))
  (define fx--type
    (-Int -Int . -> . -Fixnum))
  (define fx=-type
    (cl->*
     (-> -Int (-val 0) B : (-FS (-filter (-val 0) 0) -top))
     (-> (-val 0) -Int B : (-FS (-filter (-val 0) 1) -top))
     (-> -Int -Pos B : (-FS (-filter -PosFixnum 0) -top))
     (-> -Pos -Int B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Int -Nat B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -Nat -Int B : (-FS (-filter -NonNegFixnum 1) -top))
     (-> -Int -NegFixnum B : (-FS (-filter -NegFixnum 0) -top))
     (-> -NegFixnum -Int B : (-FS (-filter -NegFixnum 1) -top))
     fx-comp))
  (define fx<-type
    (cl->*
     (-> -Int (-val 0) B : (-FS (-filter -NegFixnum 0) (-filter -NonNegFixnum 0)))
     (-> -Int -NegFixnum B : (-FS (-filter -NegFixnum 0) -top))
     (-> -Nat -Int B : (-FS (-filter -PosFixnum 1) -top))
     fx-comp))
  (define fx>-type
    (cl->*
     (-> -Int (-val 0) B : (-FS (-filter -PosFixnum 0) -top))
     (-> -NegFixnum -Int B : (-FS (-filter -NegFixnum 1) -top))
     (-> -Int -Nat B : (-FS (-filter -PosFixnum 0) -top))
     fx-comp))
  (define fx<=-type
    (cl->*
     (-> -Int (-val 0) B : (-FS -top (-filter -PosFixnum 0)))
     (-> -Int -NegFixnum B : (-FS (-filter -NegFixnum 0) -top))
     (-> -Pos -Int B : (-FS (-filter -Pos 1) -top))
     (-> -Nat -Int B : (-FS (-filter -Nat 1) -top))
     fx-comp))
  (define fx>=-type
    (cl->*
     (-> -Int (-val 0) B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -NegFixnum -Int B : (-FS (-filter -NegFixnum 1) -top))
     (-> -Int -Pos B : (-FS (-filter -Pos 0) -top))
     (-> -Int -Nat B : (-FS (-filter -Nat 0) -top))
     fx-comp))
  (define fxmin-type
    (cl->*
     (-> -NegFixnum -Int -NegFixnum)
     (-> -Int -NegFixnum -NegFixnum)
     (-> -Pos -Pos -PosFixnum)
     (-> -Nat -Nat -NonNegFixnum)
     (-> -Int -Int -Fixnum)))
  (define fxmax-type
    (cl->*
     (-> -NegFixnum -NegFixnum -NegFixnum)
     (-> -Pos -Int -PosFixnum)
     (-> -Int -Pos -PosFixnum)
     (-> -Nat -Int -NonNegFixnum)
     (-> -Int -Nat -NonNegFixnum)
     (-> -Int -Int -Fixnum)))

  (define fl+*-type
    (cl->* (-NonNegFlonum -NonNegFlonum . -> . -NonNegFlonum)
           (-Flonum -Flonum . -> . -Flonum)))
  (define fl=-type
    (cl->*
     (-> -Flonum -NonNegFlonum B : (-FS (-filter -NonNegFlonum 0) -top))
     (-> -NonNegFlonum -Flonum B : (-FS (-filter -NonNegFlonum 1) -top))
     fl-comp))
  (define fl<-type
    (cl->*
     (-> -NonNegFlonum -Flonum B : (-FS (-filter -NonNegFlonum 1) -top))
     fl-comp))
  (define fl>-type
    (cl->*
     (-> -Flonum -NonNegFlonum B : (-FS (-filter -NonNegFlonum 0) -top))
     fl-comp))
  (define flmin-type
    (cl->* (-> -NonNegFlonum -NonNegFlonum -NonNegFlonum)
           (-> -Flonum -Flonum -Flonum)))
  (define flmax-type
    (cl->* (-> -NonNegFlonum -Flonum -NonNegFlonum)
           (-> -Flonum -NonNegFlonum -NonNegFlonum)
           (-> -Flonum -Flonum -Flonum)))
  )

;; numeric predicates
[zero? (asym-pred N B (-FS (-filter (Un -NonNegFlonum -Zero) 0)
                           (-not-filter -Zero 0)))]
[number? (make-pred-ty N)]
[integer? (asym-pred Univ B (-FS (-filter (Un -Int -Flonum) 0)
                                 (-not-filter -Int 0)))]
[exact-integer? (make-pred-ty -Int)]
[real? (make-pred-ty -Real)]
[flonum? (make-pred-ty -Flonum)]
[single-flonum? (make-pred-ty -SingleFlonum)]
[double-flonum? (make-pred-ty -Flonum)]
[inexact-real? (make-pred-ty -InexactReal)]
[complex? (make-pred-ty N)]
[rational? (make-pred-ty -Real)]
[exact? (asym-pred N B (-FS -top (-not-filter -Rat 0)))]
[inexact? (asym-pred N B  (-FS -top (-not-filter (Un -InexactReal -FloatComplex) 0)))]
[fixnum? (make-pred-ty -Fixnum)]
[positive? (cl->* (-> -Fixnum B : (-FS (-filter -PosFixnum 0) -top))
                  (-> -Int B : (-FS (-filter -PosInt 0) -top))
                  (-> -Flonum B : (-FS (-filter -NonNegFlonum 0) -top))
                  (-> -Real B))]
[negative? (cl->* (-> -Fixnum B : (-FS (-filter -NegFixnum 0) (-filter -NonNegFixnum 0)))
                  (-> -Int B : (-FS -top (-filter -Nat 0)))
                  (-> -Flonum B : (-FS -top (-filter -NonNegFlonum 0)))
                  (-> -Real B))]
[exact-positive-integer? (make-pred-ty -Pos)]
[exact-nonnegative-integer? (make-pred-ty -Nat)]

[odd? (-> -Int B : (-FS -top (-filter (-val 0) 0)))]
[even? (-> -Int B)]

[modulo (cl->* (-Nat -NonNegFixnum . -> . -NonNegFixnum)
               (-Int -Fixnum . -> . -Fixnum)
               (-Nat -Nat . -> . -Nat)
               (-Int -Int . -> . -Int))]

[=  (cl->*
     (-> -Int (-val 0) B : (-FS (-filter (-val 0) 0) -top))
     (-> (-val 0) -Int B : (-FS (-filter (-val 0) 1) -top))
     (-> -Int -PosFixnum B : (-FS (-filter -PosFixnum 0) -top))
     (-> -PosFixnum -Int B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Int -NonNegFixnum B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -NonNegFixnum -Int B : (-FS (-filter -NonNegFixnum 1) -top))
     (-> -Int -NegFixnum B : (-FS (-filter -NegFixnum 0) -top))
     (-> -NegFixnum -Int B : (-FS (-filter -NegFixnum 1) -top))
     (-> -Int -Pos B : (-FS (-filter -Pos 0) -top))
     (-> -Pos -Int B : (-FS (-filter -Pos 1) -top))
     (-> -Int -Nat B : (-FS (-filter -Nat 0) -top))
     (-> -Nat -Int B : (-FS (-filter -Nat 1) -top))
     (->* (list N N) N B))]

[>  (cl->*
     (-> -Fixnum (-val 0) B : (-FS (-filter -PosFixnum 0) -top))
     (-> -Int (-val 0) B : (-FS (-filter -Pos 0) -top))
     (-> -NegFixnum -Fixnum B : (-FS (-filter -NegFixnum 1) -top))
     (-> -Fixnum -NonNegFixnum B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Fixnum -Nat B : (-FS (-filter -Fixnum 1) -top))
     (-> -Int -Nat B : (-FS (-filter -PosInt 0) -top))
     (-> -Flonum -NonNegFlonum B : (-FS (-filter -NonNegFlonum 0) -top))
     (-> -NonNegFlonum -Flonum B : (-FS -top (-filter -NonNegFlonum 1)))
     real-comp)]
[>= (cl->*
     (-> -Fixnum (-val 0) B : (-FS (-filter -NonNegFixnum 0) (-filter -NegFixnum 0)))
     (-> -Int (-val 0) B : (-FS (-filter -Nat 0) -top))
     (-> -Fixnum -PosFixnum B : (-FS (-filter -PosFixnum 0) -top))
     (-> -Fixnum -NonNegFixnum B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -Fixnum -Pos B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Fixnum -Nat B : (-FS (-filter -NonNegFixnum 1) -top))
     (-> -Int -Pos B : (-FS (-filter -Pos 0) -top))
     (-> -Int -Nat B : (-FS (-filter -Nat 0) -top))
     (-> -Flonum -NonNegFlonum B : (-FS (-filter -NonNegFlonum 0) -top))
     (-> -NonNegFlonum -Flonum B : (-FS -top (-filter -NonNegFlonum 1)))
     real-comp)]
[<  (cl->*
     (-> -Fixnum (-val 0) B : (-FS (-filter -NegFixnum 0) (-filter -NonNegFixnum 0)))
     (-> -Int (-val 0) B : (-FS -top (-filter -Nat 0)))
     (-> -NonNegFixnum -Fixnum B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Fixnum -NegFixnum B : (-FS (-filter -NegFixnum 0) -top))
     (-> -Nat -Fixnum B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -Nat -Int B : (-FS (-filter -Pos 1) -top))
     (-> -Int -Nat B : (-FS -top (-filter -Nat 0)))
     (-> -NonNegFlonum -Flonum B : (-FS (-filter -NonNegFlonum 1) -top))
     (-> -Flonum -NonNegFlonum B : (-FS -top (-filter -NonNegFlonum 0)))
     real-comp)]
[<= (cl->*
     (-> -Fixnum (-val 0) B : (-FS -top (-filter -PosFixnum 0)))
     (-> -Int (-val 0) B : (-FS -top (-filter -PosInt 0)))
     (-> -PosFixnum -Fixnum B : (-FS (-filter -PosFixnum 1) -top))
     (-> -NonNegFixnum -Fixnum B : (-FS (-filter -NonNegFixnum 1) -top))
     (-> -Pos -Fixnum B : (-FS (-filter -PosFixnum 0) -top))
     (-> -Nat -Fixnum B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -Pos -Int B : (-FS (-filter -Pos 1) -top))
     (-> -Nat -Int B : (-FS (-filter -Nat 1) -top))
     (-> -NonNegFlonum -Flonum B : (-FS (-filter -NonNegFlonum 1) -top))
     (-> -Flonum -NonNegFlonum B : (-FS -top (-filter -NonNegFlonum 0)))
     real-comp)]


[* (apply cl->*
          (append (for/list ([t (list -Pos -Nat -Int -Rat -NonNegFlonum -Flonum)]) (->* (list) t t))
                  (list (->* (list) (Un -Pos -NonNegFlonum) -NonNegFlonum))
                  (list (->* (list) (Un -Pos -Flonum) -Flonum))
                  (list (->* (list -Flonum) (Un -InexactReal -Flonum) -Flonum))
                  (list (->* (list -InexactReal -Flonum) (Un -InexactReal -Flonum) -Flonum))
                  (list (->* (list) -InexactReal -InexactReal))
                  (list (->* (list) -Real -Real))
                  (list (->* (list) (Un -FloatComplex -Flonum) -FloatComplex))
                  (list (->* (list) N N))))]
[+ (apply cl->*
          (append (list (-> -PosByte -PosByte -PosIndex))
                  (list (-> -Byte -Byte -Index))
                  (list (->* (list -Pos) -Nat -Pos))
                  (list (->* (list -Nat -Pos) -Nat -Pos))
                  (for/list ([t (list -Nat -Int -Rat -NonNegFlonum -Flonum)]) (->* (list) t t))
                  ;; special cases for promotion to inexact, not exhaustive
                  ;; valid for + and -, but not for * and /, since (* <float> 0) is exact 0 (i.e. not a float)
                  (list (->* (list) (Un -Nat -NonNegFlonum) -NonNegFlonum))
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real -Flonum) -Real -Flonum))
                  (list (->* (list) -InexactReal -InexactReal))
                  (list (->* (list) -Real -Real))
                  (list (->* (list) (Un -Real -FloatComplex) -FloatComplex))
                  (list (->* (list -FloatComplex) N -FloatComplex))
                  (list (->* (list N -FloatComplex) N -FloatComplex))
                  (list (->* (list) N N))))]

[- (apply cl->*
          (append (for/list ([t (list -Int -Rat -Flonum)])
                            (->* (list t) t t))
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -Real -Flonum) -Real -Flonum))
                  (list (->* (list -InexactReal) -InexactReal -InexactReal))
                  (list (->* (list -Real) -Real -Real))
                  (list (->* (list (Un -Real -FloatComplex)) (Un -Real -FloatComplex) -FloatComplex))
                  (list (->* (list -FloatComplex) N -FloatComplex))
                  (list (->* (list N -FloatComplex) N -FloatComplex))
                  (list (->* (list N) N N))))]
[/ (apply cl->*
          (append (list (->* (list -Int) -Int -Rat))
                  (for/list ([t (list -Rat -Flonum)])
                            (->* (list t) t t))
                  ;; only exact 0 as first argument can cause the result of a division involving inexacts to be exact
                  (list (->* (list -Flonum) -Real -Flonum))
                  (list (->* (list -InexactReal -Flonum) -InexactReal -Flonum))
                  (list (->* (list -InexactReal) -InexactReal -InexactReal))
                  (list (->* (list -Real) -Real -Real))
                  (list (->* (list (Un -Flonum -FloatComplex)) (Un -Real -FloatComplex) -FloatComplex))
                  (list (->* (list -FloatComplex) -FloatComplex -FloatComplex))
                  (list (->* (list N) N N))))]

[max (cl->* (->* (list -PosFixnum) -Fixnum -PosFixnum)
            (->* (list -NonNegFixnum) -Fixnum -NonNegFixnum)
            (->* (list -NegFixnum) -NegFixnum -NegFixnum)
            (->* (list -Fixnum) -Fixnum -Fixnum)
            (->* (list -Pos) -Int -Pos)
            (->* (list -Nat) -Int -Nat)
            (->* (list -Int) -Int -Int)
            (->* (list -Rat) -Rat -Rat)
            (->* (list -NonNegFlonum) -Flonum -NonNegFlonum)
            (->* (list -Flonum) -Flonum -Flonum)
            (->* (list -InexactReal) -InexactReal -InexactReal)
            (->* (list -Real) -Real -Real))]
[min (cl->* (->* (list -PosFixnum) -PosFixnum -PosFixnum)
            (->* (list -NonNegFixnum) -NonNegFixnum -NonNegFixnum)
            (->* (list -NegFixnum) -Fixnum -NegFixnum)
            (->* (list -Fixnum) -NegFixnum -NegFixnum)
            (->* (list -Fixnum) -Fixnum -Fixnum)
            (->* (list -Pos) -Pos -Pos)
            (->* (list -Nat) -Nat -Nat)
            (->* (list -Int) -Int -Int)
            (->* (list -Rat) -Rat -Rat)
            (->* (list -NonNegFlonum) -NonNegFlonum -NonNegFlonum)
            (->* (list -Flonum) -Flonum -Flonum)
            (->* (list -InexactReal) -InexactReal -InexactReal)
            (->* (list -Real) -Real -Real))]


[add1 (cl->* (-> -Pos -Pos)
             (-> -Nat -Pos)
             (-> -Int -Int)
             (-> -Rat -Rat)
             (-> -NonNegFlonum -NonNegFlonum)
             (-> -Flonum -Flonum)
             (-> -InexactReal -InexactReal)
             (-> -Real -Real)
             (-> -FloatComplex -FloatComplex)
             (-> N N))]

[sub1 (cl->* (-> -Pos -Nat)
             (-> -Int -Int)
             (-> -Rat -Rat)
             (-> -Flonum -Flonum)
             (-> -InexactReal -InexactReal)
             (-> -Real -Real)
             (-> -FloatComplex -FloatComplex)
             (-> N N))]

[quotient (cl->* (-NonNegFixnum -NonNegFixnum . -> . -NonNegFixnum)
                 (-Fixnum -Fixnum . -> . -Fixnum)
                 (-Nat -Nat . -> . -Nat)
                 (-Int -Int . -> . -Int))]
[remainder (cl->* (-Nat -NonNegFixnum . -> . -NonNegFixnum)
                  (-Int -Fixnum . -> . -Fixnum)
                  (-Nat -Nat . -> . -Nat)
                  (-Int -Int . -> . -Int))]
[quotient/remainder (cl->* (-NonNegFixnum -NonNegFixnum . -> . (-values (list -NonNegFixnum -NonNegFixnum)))
                           (-Nat -NonNegFixnum . -> . (-values (list -Nat -NonNegFixnum)))
                           (-Fixnum -Fixnum . -> . (-values (list -Fixnum -Fixnum)))
                           (-Int -Fixnum . -> . (-values (list -Int -Fixnum)))
                           (-Nat -Nat . -> . (-values (list -Nat -Nat)))
                           (-Int -Int . -> . (-values (list -Int -Int))))]

[arithmetic-shift (cl->* ((-val 0) (Un -NegFixnum (-val 0)) . -> . (-val 0))
                         (-NonNegFixnum (Un -NegFixnum (-val 0)) . -> . -NonNegFixnum)
                         (-Fixnum (Un -NegFixnum (-val 0)) . -> . -Fixnum)
                         (-Nat -Int . -> . -Nat)
                         (-Int -Int . -> . -Int))]

[bitwise-and (cl->* (null -NonNegFixnum . ->* . -NonNegFixnum)
                    ((list -Int) -NonNegFixnum . ->* . -NonNegFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    ((list -Int) -Fixnum . ->* . -Fixnum)
                    (null -Nat . ->* . -Nat)
                    (null -Int . ->* . -Int))]
[bitwise-ior (cl->* (null -NonNegFixnum . ->* . -NonNegFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    (null -Nat . ->* . -Nat)
                    (null -Int . ->* . -Int))]
[bitwise-not (cl->* (null -Fixnum . ->* . -Fixnum)
                    (null -Int . ->* . -Int))]
[bitwise-xor (cl->* (null -NonNegFixnum . ->* . -NonNegFixnum)
                    (null -Fixnum . ->* . -Fixnum)
                    (null -Nat . ->* . -Nat)
                    (null -Int . ->* . -Int))]
[bitwise-bit-set? (-> -Int -Int B)]
[bitwise-bit-field (-> -Int -Int -Int -Int)]
[integer-length (-> -Int -NonNegFixnum)]

[abs (cl->* (-PosFixnum . -> . -PosFixnum)
            (-Fixnum . -> . -NonNegFixnum)
            (-Pos . -> . -Pos)
            (-Int . -> . -Nat)
            (-Rat . -> . -Rat)
            (-Flonum . -> . -NonNegFlonum)
            (-InexactReal . -> . -InexactReal)
            (-Real . -> . -Real))]

;; exactness
[exact->inexact (cl->*
                 (-Flonum . -> . -Flonum)           ; no conversion
                 (-InexactReal . -> . -InexactReal) ; no conversion
                 (-Real . -> . -Flonum)
                 (N . -> . -FloatComplex))]
[inexact->exact (cl->*
                 (-Real . -> . -Rat)
                 (N . -> . N))]
[fl->exact-integer (cl->*
                    (-NonNegFlonum . -> . -Nat)
                    (-Flonum . -> . -Int))]
[real->single-flonum (cl->* (-PosReal . -> . -PosSingleFlonum)
                            (-NegReal . -> . -NegSingleFlonum)
                            (-RealZero . -> . -SingleFlonumZero)
                            (-NonNegReal . -> . -NonNegSingleFlonum)
                            (-NonPosReal . -> . -NonPosSingleFlonum)
                            (-Real . -> . -SingleFlonumZero))]
[real->double-flonum (cl->* (-PosReal . -> . -PosFlonum)
                            (-NegReal . -> . -NegFlonum)
                            (-RealZero . -> . -FlonumZero)
                            (-NonNegReal . -> . -NonNegFlonum)
                            (-NonPosReal . -> . -NonPosFlonum)
                            (-Real . -> . -Flonum))]

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
[numerator   (cl->* (-Rat . -> . -Int)
                    (-Real . -> . -Real))]
[denominator (cl->* (-Rat . -> . -Int)
                    (-Real . -> . -Real))]
[rationalize (cl->* (-Rat -Rat . -> . -Rat)
                    (-Flonum -Flonum . -> . -Flonum)
                    (-InexactReal -InexactReal . -> . -InexactReal)
                    (-Real -Real . -> . -Real))]
[expt (cl->* (-Nat -Nat . -> . -Nat)
             (-Int -Nat . -> . -Int)
             (-Int -Int . -> . -Rat)
             (-Real -Int . -> . -Real)
             (-FloatComplex -FloatComplex . -> . -FloatComplex)
             (N N . -> . N))]
[sqrt (cl->*
       (-Nat . -> . -Real)
       (-NonNegFlonum . -> . -NonNegFlonum)
       (-FloatComplex . -> . -FloatComplex)
       (N . -> . N))]
[integer-sqrt (cl->*
               (-Zero . -> . -Zero)
               (-NonNegFixnum . -> . -NonNegFixnum)
               (-Nat . -> . -Nat)
               (-NonNegFlonum . -> . -NonNegFlonum)
               (-Real . -> . N))]
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
[gcd  (cl->* (null -Fixnum . ->* . -Fixnum) (null -Int . ->* . -Int))]
[lcm  (null -Int . ->* . -Int)]

;; scheme/math

[sgn (cl->* (-Zero . -> . -Zero)
            (-PosInt . -> . -PosFixnum)
            (-Nat . -> . -NonNegFixnum)
            (-Rat . -> . -Fixnum)
            (-Flonum . -> . -Flonum)
            (-InexactReal . -> . -InexactReal)
            (-Real . -> . -Real))]

[pi -NonNegFlonum]
[sqr (cl->* (-> -Pos -Pos)
            (-> -Int -Nat)
            (-> -Rat -Rat)
            (-> -Flonum -NonNegFlonum)
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
[unsafe-flabs (-> -Flonum -NonNegFlonum)]
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
[unsafe-fx->fl (cl->* (-Nat . -> . -NonNegFlonum) (-Int . -> . -Flonum))]
[unsafe-make-flrectangular (-Flonum -Flonum . -> . -FloatComplex)]
[unsafe-flreal-part (-FloatComplex . -> . -Flonum)]
[unsafe-flimag-part (-FloatComplex . -> . -Flonum)]

[unsafe-fx+ fx+-type]
[unsafe-fx- fx--type]
[unsafe-fx* fx-op]
[unsafe-fxquotient fx-natop]
[unsafe-fxremainder fx-natop]
[unsafe-fxmodulo fx-natop]
[unsafe-fxabs (-Int . -> . (Un -PosFixnum (-val 0)))]

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
[fxabs (-Int . -> . (Un -PosFixnum (-val 0)))]

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
[flabs (-> -Flonum -NonNegFlonum)]
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
[->fl (-Int . -> . -Flonum)]
[make-flrectangular (-Flonum -Flonum . -> . -FloatComplex)]
[flreal-part (-FloatComplex . -> . -Flonum)]
[flimag-part (-FloatComplex . -> . -Flonum)]

;; safe flvector ops

[flvector? (make-pred-ty -FlVector)]
[flvector (->* (list) -Flonum -FlVector)]
[make-flvector (cl->* (-> -Int -FlVector)
                      (-> -Int -Flonum -FlVector))]
[flvector-length (-> -FlVector -NonNegFixnum)]
[flvector-ref (-> -FlVector -Int -Flonum)]
[flvector-set! (-> -FlVector -Int -Flonum -Void)]

;; unsafe flvector ops

[unsafe-flvector-length (-> -FlVector -NonNegFixnum)]
[unsafe-flvector-ref (-> -FlVector -Int -Flonum)]
[unsafe-flvector-set! (-> -FlVector -Int -Flonum -Void)]
