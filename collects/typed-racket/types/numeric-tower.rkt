#lang racket/base

(require "../utils/utils.rkt")

(require (types abbrev numeric-predicates)
         (rename-in (rep type-rep) [make-Base make-Base*])
         racket/function
         unstable/function
         (for-template racket/base racket/contract racket/flonum (types numeric-predicates)))

(provide portable-fixnum? portable-index?
         -Zero -One -PosByte -Byte -PosIndex -Index
         -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
         -PosInt -Nat -NegInt -NonPosInt -Int
         -PosRat -NonNegRat -NegRat -NonPosRat -Rat
         -FlonumPosZero -FlonumNegZero -FlonumZero -FlonumNan -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
         -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -SingleFlonumNan -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
         -InexactRealPosZero -InexactRealNegZero -InexactRealZero -InexactRealNan -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
         -RealZero -PosReal -NonNegReal -NegReal -NonPosReal -Real
         -ExactNumber -FloatComplex -SingleFlonumComplex -InexactComplex -Number
         (rename-out (-Int -Integer)))

;; all the types defined here are numeric
(define (make-Base name contract predicate marshaled)
  (make-Base* name contract predicate marshaled #t))


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
(define -Zero (-val 0)) ; exact
(define -One  (-val 1))

;; Integers
(define -Byte>1 (make-Base 'Byte-Larger-Than-One ; unsigned
                           #'(and/c byte? (lambda (x) (> x 1)))
                           (conjoin byte? (lambda (x) (> x 1)))
                           #'-Byte>1))
(define -PosByte (*Un -One -Byte>1))
(define -Byte    (*Un -Zero -PosByte))
(define -PosIndexNotByte
  (make-Base 'Positive-Index-Not-Byte
             ;; index? will be checked at runtime, can be platform-specific
             ;; portable-index? will be checked at compile-time, must be portable
             #'(and/c index? positive? (not/c byte?))
             (lambda (x) (and (portable-index? x)
                              (positive? x)
                              (not (byte? x))))
             #'-PosIndexNotByte))
(define -PosIndex    (*Un -One -Byte>1 -PosIndexNotByte))
(define -Index       (*Un -Zero -PosIndex))
(define -PosFixnumNotIndex
  (make-Base 'Positive-Fixnum-Not-Index
             #'(and/c fixnum? positive? (not/c index?))
             (lambda (x) (and (portable-fixnum? x)
                              (positive? x)
                              (not (portable-index? x))))
             #'-PosFixnumNotIndex))
(define -PosFixnum    (*Un -PosFixnumNotIndex -PosIndex))
(define -NonNegFixnum (*Un -PosFixnum -Zero))
(define -NegFixnum
  (make-Base 'Negative-Fixnum
             #'(and/c fixnum? negative?)
             (lambda (x) (and (portable-fixnum? x)
                              (negative? x)))
             #'-NegFixnum))
(define -NonPosFixnum (*Un -NegFixnum -Zero))
(define -Fixnum       (*Un -NegFixnum -Zero -PosFixnum))
(define -PosIntNotFixnum
  (make-Base 'Positive-Integer-Not-Fixnum
             #'(and/c exact-integer? positive? (not/c fixnum?))
             (lambda (x) (and (exact-integer? x)
                              (positive? x)
                              (not (portable-fixnum? x))))
             #'-PosIntNotFixnum))
(define -PosInt    (*Un -PosIntNotFixnum -PosFixnum))
(define -NonNegInt (*Un -PosInt -Zero))
(define -Nat       -NonNegInt)
(define -NegIntNotFixnum
  (make-Base 'Negative-Integer-Not-Fixnum
             #'(and/c exact-integer? negative? (not/c fixnum?))
             (lambda (x) (and (exact-integer? x)
                              (negative? x)
                              (not (portable-fixnum? x))))
             #'-NegIntNotFixnum))
(define -NegInt    (*Un -NegIntNotFixnum -NegFixnum))
(define -NonPosInt (*Un -NegInt -Zero))
(define -Int       (*Un -NegInt -Zero -PosInt))

;; Rationals
(define -PosRatNotInt
  (make-Base 'Positive-Rational-Not-Integer
             #'(and/c exact-rational? positive? (not/c integer?))
             (lambda (x) (and (exact-rational? x)
                              (positive? x)
                              (not (exact-integer? x))))
             #'-PosRatNotInt))
(define -PosRat    (*Un -PosRatNotInt -PosInt))
(define -NonNegRat (*Un -PosRat -Zero))
(define -NegRatNotInt
  (make-Base 'Negative-Rational-Not-Integer
             #'(and/c exact-rational? negative? (not/c integer?))
             (lambda (x) (and (exact-rational? x)
                              (negative? x)
                              (not (exact-integer? x))))
             #'-NegRatNotInt))
(define -NegRat    (*Un -NegRatNotInt -NegInt))
(define -NonPosRat (*Un -NegRat -Zero))
(define -Rat       (*Un -NegRat -Zero -PosRat))

;; Floating-point numbers
(define -FlonumPosZero (make-Base 'Float-Positive-Zero
                                  #'(lambda (x) (eqv? x 0.0))
                                  (lambda (x) (eqv? x 0.0))
                                  #'-FlonumPosZero))
(define -FlonumNegZero (make-Base 'Float-Negative-Zero
                                  #'(lambda (x) (eqv? x -0.0))
                                  (lambda (x) (eqv? x -0.0))
                                  #'-FlonumNegZero))
(define -FlonumZero (*Un -FlonumPosZero -FlonumNegZero))
(define -FlonumNan (make-Base 'Float-Nan
                              #'(and/c flonum? (lambda (x) (eqv? x +nan.0)))
                              (lambda (x) (and (flonum? x) (eqv? x +nan.0)))
                              #'-FlonumNan))
(define -PosFlonum
  (make-Base 'Positive-Float
             #'(and/c flonum? positive?)
             (lambda (x) (and (flonum? x) (positive? x)))
             #'-PosFlonum))
(define -NonNegFlonum (*Un -PosFlonum -FlonumZero))
(define -NegFlonum
  (make-Base 'Negative-Float
             #'(and/c flonum? negative?)
             (lambda (x) (and (flonum? x) (negative? x)))
             #'-NegFlonum))
(define -NonPosFlonum (*Un -NegFlonum -FlonumZero))
(define -Flonum (*Un -NegFlonum -FlonumNegZero -FlonumPosZero -PosFlonum -FlonumNan)) ; 64-bit floats
;; inexact reals can be flonums (64-bit floats) or 32-bit floats
(define -SingleFlonumPosZero ; disjoint from Flonum 0s
  (make-Base 'Single-Flonum-Positive-Zero
             ;; eqv? equates 0.0f0 with itself, but not eq?
             ;; we also need to check for single-flonum? since eqv? also equates
             ;; 0.0f0 and 0.0e0
             #'(and/c single-flonum? (lambda (x) (eqv? x 0.0f0)))
             (lambda (x) #f) ; can't assign that type at compile-time. see tc-lit for more explanation
	     #'-SingleFlonumPosZero))
(define -SingleFlonumNegZero
  (make-Base 'Single-Flonum-Negative-Zero
             #'(and/c single-flonum? (lambda (x) (eqv? x -0.0f0)))
             (lambda (x) #f)
	     #'-SingleFlonumNegZero))
(define -SingleFlonumZero (*Un -SingleFlonumPosZero -SingleFlonumNegZero))
(define -SingleFlonumNan (make-Base 'Single-Flonum-Nan
                                    #'(and/c single-flonum? (lambda (x) (eqv? x +nan.f)))
                                    (lambda (x) (and (single-flonum? x) (eqv? x +nan.f)))
                                    #'-SingleFlonumNan))
(define -InexactRealPosZero (*Un -SingleFlonumPosZero -FlonumPosZero))
(define -InexactRealNegZero (*Un -SingleFlonumNegZero -FlonumNegZero))
(define -InexactRealZero    (*Un -InexactRealPosZero
                                 -InexactRealNegZero))
(define -InexactRealNan     (*Un -FlonumNan -SingleFlonumNan))
(define -PosSingleFlonum
  (make-Base 'Positive-Single-Flonum
             #'(and/c single-flonum? positive?)
             (lambda (x) #f)
	     #'-PosSingleFlonum))
(define -PosInexactReal     (*Un -PosSingleFlonum -PosFlonum))
(define -NonNegSingleFlonum (*Un -PosSingleFlonum -SingleFlonumZero))
(define -NonNegInexactReal  (*Un -PosInexactReal -InexactRealZero))
(define -NegSingleFlonum
  (make-Base 'Negative-Single-Flonum
             #'(and/c single-flonum? negative?)
             (lambda (x) #f)
	     #'-NegSingleFlonum))
(define -NegInexactReal     (*Un -NegSingleFlonum -NegFlonum))
(define -NonPosSingleFlonum (*Un -NegSingleFlonum -SingleFlonumZero))
(define -NonPosInexactReal  (*Un -NegInexactReal -InexactRealZero))
(define -SingleFlonum       (*Un -NegSingleFlonum -SingleFlonumNegZero -SingleFlonumPosZero -PosSingleFlonum -SingleFlonumNan))
(define -InexactReal        (*Un -SingleFlonum -Flonum))

;; Reals
(define -RealZero   (*Un -Zero -InexactRealZero))
(define -PosReal    (*Un -PosRat -PosInexactReal))
(define -NonNegReal (*Un -NonNegRat -NonNegInexactReal))
(define -NegReal    (*Un -NegRat -NegInexactReal))
(define -NonPosReal (*Un -NonPosRat -NonPosInexactReal))
(define -Real       (*Un -Rat -InexactReal))

;; Complexes
;; We could go into _much_ more precision here.
;; We could have types that reflect the size/exactness of both components
;; (e.g. PosFixnumNonNegIntComplex), to give more interesting types to
;; real-part, imag-part and others.
;; We could have Complex be a 2-argument type constructor (although it
;; could construct uninhabitable types like (Complex Integer Float), which
;; can't exist in Racket (parts must be both exact or both inexact)).
;; Imaginaries could have their own type hierarchy as well.
;; That's future work.

;; Both parts of a complex number must be of the same exactness.
;; Thus, the only possible kinds of complex numbers are:
;; Real/Real, Flonum/Flonum, SingleFlonum/SingleFlonum
(define -ExactNumberNotReal
  (make-Base 'Exact-Number-Not-Real
             #'(and/c number?
                      (not/c real?)
                      (lambda (x) (exact? (imag-part x))))
             (lambda (x) (and (number? x)
                              (not (real? x))
                              (exact? (imag-part x))))
             #'-ExactNumberNotReal))
(define -ExactNumber (*Un -ExactNumberNotReal -Rat))
(define -FloatComplex (make-Base 'Float-Complex
                                 #'(and/c number?
                                          (lambda (x)
                                            (and (flonum? (imag-part x))
                                                 (flonum? (real-part x)))))
                                 (lambda (x)
                                   (and (number? x)
                                        (flonum? (imag-part x))
                                        (flonum? (real-part x))))
                                 #'-FloatComplex))
(define -SingleFlonumComplex (make-Base 'Single-Flonum-Complex
                                      #'(and/c number?
                                               (lambda (x)
                                                 (and (single-flonum? (imag-part x))
                                                      (single-flonum? (real-part x)))))
                                      (lambda (x)
                                        (and (number? x)
                                             (single-flonum? (imag-part x))
                                             (single-flonum? (real-part x))))
                                      #'-SingleFlonumComplex))
(define -InexactComplex (*Un -FloatComplex -SingleFlonumComplex))
(define -Complex (*Un -Real -InexactComplex -ExactNumberNotReal))
(define -Number -Complex)
