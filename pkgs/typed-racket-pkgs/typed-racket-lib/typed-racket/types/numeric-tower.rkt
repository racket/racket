#lang racket/base

(require "../utils/utils.rkt"
         (rename-in (types numeric-predicates base-abbrev)
                    [simple-Un *Un])
         (rename-in (rep type-rep) [make-Base make-Base*])
         unstable/function
         racket/extflonum
         ;; For base type contracts
         (for-template racket/base racket/contract/base racket/extflonum (types numeric-predicates)))

(provide portable-fixnum? portable-index?
         -Zero -One -PosByte -Byte -PosIndex -Index
         -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
         -PosInt -Nat -NegInt -NonPosInt -Int
         -PosRat -NonNegRat -NegRat -NonPosRat -Rat
         -FlonumPosZero -FlonumNegZero -FlonumZero -FlonumNan -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
         -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -SingleFlonumNan -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
         -InexactRealPosZero -InexactRealNegZero -InexactRealZero -InexactRealNan -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
         -RealZero -PosReal -NonNegReal -NegReal -NonPosReal -Real
         -ExactImaginary -FloatImaginary -SingleFlonumImaginary -InexactImaginary -Imaginary
         -ExactNumber -ExactComplex -FloatComplex -SingleFlonumComplex -InexactComplex -Number
         (rename-out (-Int -Integer))
         -ExtFlonumPosZero -ExtFlonumNegZero -ExtFlonumZero -ExtFlonumNan
         -PosExtFlonum -NonNegExtFlonum -NegExtFlonum -NonPosExtFlonum -ExtFlonum)

;; all the types defined here are numeric (except 80-bit flonums)
(define (make-Base name contract predicate)
  (make-Base* name contract predicate #t))


;; Numeric hierarchy
;; All built as unions of non-overlapping base types.
;; This should make encoding mathematical properties in the base env easier.
;; The base types that don't have an interesting mathematical meaning
;; (e.g. -Byte>1, -PosFixnumNotIndex, etc.) should not be used anywhere, as
;; they should not be exposed to the user and could easily be misused in
;; the base type environment. They are not provided.
;; A lot of these contracts will be overriden in type->contract, so their
;; hairiness should not be of much consequence.

;; Is the number a fixnum on *all* the platforms Racket supports?  This
;; works because Racket compiles only on 32+ bit systems.  This check is
;; done at compile time to typecheck literals -- so use it instead of
;; `fixnum?' to avoid creating platform-dependent .zo files.
(define (portable-fixnum? n)
  (and (exact-integer? n)
       (< n (expt 2 30))
       (>= n (- (expt 2 30)))))
;; same, for indexes
(define (portable-index? n)
  (and (exact-integer? n)
       (< n (expt 2 28))
       (>= n 0)))

;; Singletons
(define/decl -Zero (make-Value 0)) ; exact
(define/decl -One  (make-Value 1))

;; Integers
(define/decl -Byte>1 (make-Base 'Byte-Larger-Than-One ; unsigned
				#'(and/c byte? (lambda (x) (> x 1)))
				(conjoin byte? (lambda (x) (> x 1)))))
(define/decl -PosByte (*Un -One -Byte>1))
(define/decl -Byte    (*Un -Zero -PosByte))
(define/decl -PosIndexNotByte
  (make-Base 'Positive-Index-Not-Byte
             ;; index? will be checked at runtime, can be platform-specific
             ;; portable-index? will be checked at compile-time, must be portable
             #'(and/c index? positive? (not/c byte?))
             (lambda (x) (and (portable-index? x)
                              (positive? x)
                              (not (byte? x))))))
(define/decl -PosIndex    (*Un -One -Byte>1 -PosIndexNotByte))
(define/decl -Index       (*Un -Zero -PosIndex))
(define/decl -PosFixnumNotIndex
  (make-Base 'Positive-Fixnum-Not-Index
             #'(and/c fixnum? positive? (not/c index?))
             (lambda (x) (and (portable-fixnum? x)
                              (positive? x)
                              (not (portable-index? x))))))
(define/decl -PosFixnum    (*Un -PosFixnumNotIndex -PosIndex))
(define/decl -NonNegFixnum (*Un -PosFixnum -Zero))
(define/decl -NegFixnum
  (make-Base 'Negative-Fixnum
             #'(and/c fixnum? negative?)
             (lambda (x) (and (portable-fixnum? x)
                              (negative? x)))))
(define/decl -NonPosFixnum (*Un -NegFixnum -Zero))
(define/decl -Fixnum       (*Un -NegFixnum -Zero -PosFixnum))
;; This type, and others like it, should *not* be exported, or used for
;; anything but building unions. Especially, no literals should be given
;; these types.
(define/decl -PosIntNotFixnum
  (make-Base 'Positive-Integer-Not-Fixnum
             #'(and/c exact-integer? positive? (not/c fixnum?))
             (lambda (x) (and (exact-integer? x)
                              (positive? x)
                              (not (portable-fixnum? x))))))
(define/decl -PosInt    (*Un -PosIntNotFixnum -PosFixnum))
(define/decl -NonNegInt (*Un -PosInt -Zero))
(define/decl -Nat       -NonNegInt)
(define/decl -NegIntNotFixnum
  (make-Base 'Negative-Integer-Not-Fixnum
             #'(and/c exact-integer? negative? (not/c fixnum?))
             (lambda (x) (and (exact-integer? x)
                              (negative? x)
                              (not (portable-fixnum? x))))))
(define/decl -NegInt    (*Un -NegIntNotFixnum -NegFixnum))
(define/decl -NonPosInt (*Un -NegInt -Zero))
(define/decl -Int       (*Un -NegInt -Zero -PosInt))

;; Rationals
(define/decl -PosRatNotInt
  (make-Base 'Positive-Rational-Not-Integer
             #'(and/c exact-rational? positive? (not/c integer?))
             (lambda (x) (and (exact-rational? x)
                              (positive? x)
                              (not (exact-integer? x))))))
(define/decl -PosRat    (*Un -PosRatNotInt -PosInt))
(define/decl -NonNegRat (*Un -PosRat -Zero))
(define/decl -NegRatNotInt
  (make-Base 'Negative-Rational-Not-Integer
             #'(and/c exact-rational? negative? (not/c integer?))
             (lambda (x) (and (exact-rational? x)
                              (negative? x)
                              (not (exact-integer? x))))))
(define/decl -NegRat    (*Un -NegRatNotInt -NegInt))
(define/decl -NonPosRat (*Un -NegRat -Zero))
(define/decl -Rat       (*Un -NegRat -Zero -PosRat))

;; Floating-point numbers
;; NaN is included in all floating-point types
(define/decl -FlonumNan
  (make-Base 'Float-Nan
	     #'(and/c flonum? (lambda (x) (eqv? x +nan.0)))
	     (lambda (x) (and (flonum? x) (eqv? x +nan.0)))))
(define/decl -FlonumPosZero
  (make-Base 'Float-Positive-Zero
	     #'(lambda (x) (eqv? x 0.0))
	     (lambda (x) (eqv? x 0.0))))
(define/decl -FlonumNegZero
  (make-Base 'Float-Negative-Zero
	     #'(lambda (x) (eqv? x -0.0))
	     (lambda (x) (eqv? x -0.0))))
(define/decl -FlonumZero (*Un -FlonumPosZero -FlonumNegZero -FlonumNan))
(define/decl -PosFlonumNoNan
  (make-Base 'Positive-Float-No-NaN
             #'(and/c flonum? positive?)
             (lambda (x) (and (flonum? x) (positive? x)))))
(define/decl -PosFlonum (*Un -PosFlonumNoNan -FlonumNan))
(define/decl -NonNegFlonum (*Un -PosFlonum -FlonumZero))
(define/decl -NegFlonumNoNan
  (make-Base 'Negative-Float-No-NaN
             #'(and/c flonum? negative?)
             (lambda (x) (and (flonum? x) (negative? x)))))
(define/decl -NegFlonum (*Un -NegFlonumNoNan -FlonumNan))
(define/decl -NonPosFlonum (*Un -NegFlonum -FlonumZero))
(define/decl -Flonum (*Un -NegFlonumNoNan -FlonumNegZero -FlonumPosZero -PosFlonumNoNan -FlonumNan)) ; 64-bit floats
;; inexact reals can be flonums (64-bit floats) or 32-bit floats
(define/decl -SingleFlonumNan
  (make-Base 'Single-Flonum-Nan
	     #'(and/c single-flonum? (lambda (x) (eqv? x +nan.f)))
	     (lambda (x) (and (single-flonum? x) (eqv? x +nan.f)))))
(define/decl -SingleFlonumPosZero ; disjoint from Flonum 0s
  (make-Base 'Single-Flonum-Positive-Zero
             ;; eqv? equates 0.0f0 with itself, but not eq?
             #'(lambda (x) (eqv? x 0.0f0))
             (lambda (x) (eqv? x 0.0f0))))
(define/decl -SingleFlonumNegZero
  (make-Base 'Single-Flonum-Negative-Zero
             #'(lambda (x) (eqv? x -0.0f0))
             (lambda (x) (eqv? x -0.0f0))))
(define/decl -SingleFlonumZero (*Un -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumNan))
(define/decl -InexactRealNan     (*Un -FlonumNan -SingleFlonumNan))
(define/decl -InexactRealPosZero (*Un -SingleFlonumPosZero -FlonumPosZero))
(define/decl -InexactRealNegZero (*Un -SingleFlonumNegZero -FlonumNegZero))
(define/decl -InexactRealZero    (*Un -InexactRealPosZero
                                 -InexactRealNegZero
                                 -InexactRealNan))
(define/decl -PosSingleFlonumNoNan
  (make-Base 'Positive-Single-Flonum-No-Nan
             #'(and/c single-flonum? positive?)
             (lambda (x) (and (single-flonum? x) (positive? x)))))
(define/decl -PosSingleFlonum    (*Un -PosSingleFlonumNoNan -SingleFlonumNan))
(define/decl -PosInexactReal     (*Un -PosSingleFlonum -PosFlonum))
(define/decl -NonNegSingleFlonum (*Un -PosSingleFlonum -SingleFlonumZero))
(define/decl -NonNegInexactReal  (*Un -PosInexactReal -InexactRealZero))
(define/decl -NegSingleFlonumNoNan
  (make-Base 'Negative-Single-Flonum-No-Nan
             #'(and/c single-flonum? negative?)
             (lambda (x) (and (single-flonum? x) (negative? x)))))
(define/decl -NegSingleFlonum    (*Un -NegSingleFlonumNoNan -SingleFlonumNan))
(define/decl -NegInexactReal     (*Un -NegSingleFlonum -NegFlonum))
(define/decl -NonPosSingleFlonum (*Un -NegSingleFlonum -SingleFlonumZero))
(define/decl -NonPosInexactReal  (*Un -NegInexactReal -InexactRealZero))
(define/decl -SingleFlonum       (*Un -NegSingleFlonum -SingleFlonumNegZero -SingleFlonumPosZero -PosSingleFlonum -SingleFlonumNan))
(define/decl -InexactReal        (*Un -SingleFlonum -Flonum))

;; Reals
(define/decl -RealZero   (*Un -Zero -InexactRealZero))
(define/decl -PosReal    (*Un -PosRat -PosInexactReal))
(define/decl -NonNegReal (*Un -NonNegRat -NonNegInexactReal))
(define/decl -NegReal    (*Un -NegRat -NegInexactReal))
(define/decl -NonPosReal (*Un -NonPosRat -NonPosInexactReal))
(define/decl -Real       (*Un -Rat -InexactReal))

;; Complexes
;; We could go into _much_ more precision here.
;; We could have types that reflect the size/exactness of both components
;; (e.g. PosFixnumNonNegIntComplex), to give more interesting types to
;; real-part, imag-part and others.
;; We could have Complex be a 2-argument type constructor (although it
;; could construct uninhabitable types like (Complex Integer Float), which
;; can't exist in Racket (parts must be both exact, both inexact, or one is
;; exact-zero)).  That's future work.

;; Thus, the only possible kinds of complex numbers are:
;; Zero/Rat, Zero/Flonum, Zero/SingleFlonum.
;; Rat/Rat, Flonum/Flonum, SingleFlonum/SingleFlonum.
(define/decl -ExactImaginary
  (make-Base 'Exact-Imaginary
             #'(and/c number?
                      (not/c real?)
                      (lambda (x)
                        (and
                          (eqv? 0 (real-part x))
                          (exact? (imag-part x)))))
             (lambda (x) (and (number? x)
                              (not (real? x))
                              (eqv? 0 (real-part x))
                              (exact? (imag-part x))))))
(define/decl -ExactComplex
  (make-Base 'Exact-Complex
             #'(and/c number?
                      (not/c real?)
                      (lambda (x)
                        (and
                          (not (eqv? 0 (real-part x)))
                          (exact? (real-part x))
                          (exact? (imag-part x)))))
             (lambda (x) (and (number? x)
                              (not (real? x))
                              (not (eqv? 0 (real-part x)))
                              (exact? (real-part x))
                              (exact? (imag-part x))))))
(define/decl -FloatImaginary
  (make-Base 'Float-Imaginary
	     #'(and/c number?
		      (lambda (x)
			(and (flonum? (imag-part x))
			     (eqv? 0 (real-part x)))))
	     (lambda (x)
	       (and (number? x)
		    (flonum? (imag-part x))
		    (eqv? 0 (real-part x))))))
(define/decl -SingleFlonumImaginary 
  (make-Base 'Single-Flonum-Imaginary
	     #'(and/c number?
		      (lambda (x)
			(and (single-flonum? (imag-part x))
			     (eqv? 0 (real-part x)))))
	     (lambda (x)
	       (and (number? x)
		    (single-flonum? (imag-part x))
		    (eqv? 0 (real-part x))))))
(define/decl -FloatComplex
  (make-Base 'Float-Complex
	     #'(and/c number?
		      (lambda (x)
			(and (flonum? (imag-part x))
			     (flonum? (real-part x)))))
	     (lambda (x)
	       (and (number? x)
		    (flonum? (imag-part x))
		    (flonum? (real-part x))))))
(define/decl -SingleFlonumComplex
  (make-Base 'Single-Flonum-Complex
	     #'(and/c number?
		      (lambda (x)
			(and (single-flonum? (imag-part x))
			     (single-flonum? (real-part x)))))
	     (lambda (x)
	       (and (number? x)
		    (single-flonum? (imag-part x))
		    (single-flonum? (real-part x))))))
(define/decl -ExactNumber (*Un -ExactImaginary -ExactComplex -Rat))
(define/decl -InexactImaginary (*Un -FloatImaginary -SingleFlonumImaginary))
(define/decl -Imaginary (*Un -ExactImaginary -InexactImaginary))
(define/decl -InexactComplex (*Un -FloatComplex -SingleFlonumComplex))
(define/decl -Complex (*Un -Real -Imaginary -ExactComplex -InexactComplex))
(define/decl -Number -Complex)

;; 80-bit floating-point numbers
;; +nan.t is included in all 80-bit floating-point types
(define/decl -ExtFlonumNan
  (make-Base* 'ExtFlonum-Nan
              #'(and/c extflonum? (lambda (x) (eqv? x +nan.t)))
              (lambda (x) (and (extflonum? x) (eqv? x +nan.t)))
              #f))

(define/decl -ExtFlonumPosZero
  (make-Base* 'ExtFlonum-Positive-Zero
              #'(lambda (x) (eqv? x 0.0t0))
              (lambda (x) (eqv? x 0.0t0))
              #f))

(define/decl -ExtFlonumNegZero
  (make-Base* 'ExtFlonum-Negative-Zero
              #'(lambda (x) (eqv? x -0.0t0))
              (lambda (x) (eqv? x -0.0t0))
              #f))

(define/decl -NegExtFlonumNoNan
  (make-Base* 'Negative-ExtFlonum-No-NaN
              #'(and/c extflonum? (λ (x) (extfl<= x 0.0t0)))
              (lambda (x) (and (extflonum? x) (extfl<= x 0.0t0)))
              #f))

(define/decl -PosExtFlonumNoNan
  (make-Base* 'Positive-ExtFlonum-No-NaN
              #'(and/c extflonum? (λ (x) (extfl>= x 0.0t0)))
              (lambda (x) (and (extflonum? x) (extfl>= x 0.0t0)))
              #f))

(define/decl -ExtFlonumZero (*Un -ExtFlonumPosZero -ExtFlonumNegZero -ExtFlonumNan))
(define/decl -PosExtFlonum (*Un -PosExtFlonumNoNan -ExtFlonumNan))
(define/decl -NonNegExtFlonum (*Un -PosExtFlonum -ExtFlonumZero))
(define/decl -NegExtFlonum (*Un -NegExtFlonumNoNan -ExtFlonumNan))
(define/decl -NonPosExtFlonum (*Un -NegExtFlonum -ExtFlonumZero))
(define/decl -ExtFlonum (*Un -NegExtFlonumNoNan -ExtFlonumNegZero -ExtFlonumPosZero -PosExtFlonumNoNan -ExtFlonumNan))
