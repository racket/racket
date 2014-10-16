#lang s-exp "env-lang.rkt"

(begin
  (require
   racket/list racket/math racket/flonum racket/extflonum racket/unsafe/ops unstable/sequence racket/match
   (for-template racket/flonum racket/extflonum racket/fixnum racket/math racket/unsafe/ops racket/base
                 (only-in "../types/numeric-predicates.rkt" index?))
   (only-in (types abbrev numeric-tower) [-Number N] [-Boolean B] [-Symbol Sym] [-Real R] [-PosInt -Pos]))

  ;; TODO having definitions only at the top is really inconvenient.

  (define all-int-types
    (list -Zero -One -PosByte -Byte -PosIndex -Index
          -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
          -PosInt -Nat -NegInt -NonPosInt -Int))
  (define rat-types (list -PosRat -NonNegRat -NegRat -NonPosRat -Rat))

  (define all-rat-types (append all-int-types rat-types))
  (define all-flonum-types
    (list -FlonumPosZero -FlonumNegZero -FlonumZero -FlonumNan
          -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum))
  (define single-flonum-types
    (list -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -SingleFlonumNan
          -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum))
  (define inexact-real-types
    (list -InexactRealPosZero -InexactRealNegZero -InexactRealZero -InexactRealNan
          -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal))
  (define all-float-types (append all-flonum-types single-flonum-types inexact-real-types))
  (define real-types (list -RealZero -PosReal -NonNegReal -NegReal -NonPosReal -Real))
  (define all-real-types (append all-rat-types all-float-types real-types))
  (define number-types
    (list -ExactNumber -FloatComplex -SingleFlonumComplex -InexactComplex -Number))
  (define all-number-types (append all-real-types number-types))


  ;; convenient to build large case-lambda types
  (define (from-cases . cases)
    (apply cl->* (flatten cases)))
  ;; for fixnum-specific operations. if they return at all, we know
  ;; their args were fixnums. otherwise, an error would have been thrown
  ;; for the moment, this is only useful if the result is used as a test
  ;; once we have a set of filters that are true/false based on reaching
  ;; a certain point, this will be more useful
  (define (fx-from-cases . cases)
    (apply from-cases (map (lambda (x)
                             (add-unconditional-filter-all-args
                              x -Fixnum))
                           (flatten cases))))

  (define (binop t [r t])
    (t t . -> . r))
  (define (varop t [r t])
    (->* (list) t r))
  (define (varop-1+ t [r t])
    (->* (list t) t r))

  (define (unop t) (-> t t))

  (define (commutative-binop a1 a2 [r a2])
    (list (-> a1 a2 r) (-> a2 a1 r)))
  ;; when having at least one of a given type matters (e.g. adding one+ Pos and Nats)
  (define (commutative-case t1 t2 [r t1])
    (list (->* (list t1 t2) t2 r)
          (->* (list t2 t1) t2 r)
          (->* (list t2 t2 t1) t2 r)))

  (define (comp t1 [t2 t1])
    (-> t1 t2 B))
  ;; simple case useful with equality predicates.
  ;; if the equality is true, we know that general arg is in fact of specific type.
  (define (commutative-equality/filter general specific)
    (list (-> general specific B : (-FS (-filter specific 0) -top))
          (-> specific general B : (-FS (-filter specific 1) -top))))

  (define (exclude-zero non-neg pos [zero -Zero])
    (list (-> zero non-neg B : (-FS (-filter zero 1) (-filter pos 1)))
          (-> non-neg zero B : (-FS (-filter zero 0) (-filter pos 0)))))


  (define round-type ; also used for truncate
    (lambda ()
      (from-cases
       (map unop all-int-types)
       (-> -NonNegRat -Nat)
       (-> -NonPosRat -NonPosInt)
       (-> -Rat -Int)
       (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                       -NonNegFlonum -NonPosFlonum -Flonum
                       -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero
                       -NonNegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                       -InexactRealPosZero -InexactRealNegZero -InexactRealZero
                       -NonNegInexactReal -NonPosInexactReal -InexactReal
                       -RealZero -NonNegReal -NonPosReal -Real)))))
  
  (define (inexact-zero->exact-zero-type)
    (for/list ([t (in-list
                    (list -FlonumPosZero -FlonumNegZero -FlonumZero
                          -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero
                          -InexactRealPosZero -InexactRealNegZero -InexactRealZero
                          -RealZero))])
      (-> t -Zero)))
  
  (define (exact-round-type) ; also used for exact-truncate
    (from-cases
     (map unop all-int-types)
     (inexact-zero->exact-zero-type)
     (-> (Un -NonNegRat -NonNegFlonum -NonNegSingleFlonum -NonNegInexactReal -NonNegReal) -Nat)
     (-> (Un -NonPosRat -NonPosFlonum -NonPosSingleFlonum -NonPosInexactReal -NonPosReal) -NonPosInt)
     (-> (Un -Rat -Flonum -SingleFlonum -InexactReal -Real) -Int)))
  
  (define fl-unop (lambda () (unop -Flonum)))

  ;; types for specific operations, to avoid repetition between safe and unsafe versions
  (define fx+-type
    (lambda ()
      (fx-from-cases
       (binop -Zero)
       (map (lambda (t) (commutative-binop t -Zero t))
            (list -One -PosByte -Byte -PosIndex -Index))
       (commutative-binop -PosByte -Byte -PosIndex)
       (-Byte -Byte . -> . -PosIndex)
       ;; in other cases, either we stay within fixnum range, or we error
       (commutative-binop -Pos -Nat -PosFixnum)
       (-Nat -Nat . -> . -NonNegFixnum)
       (commutative-binop -NegInt -One -NonPosFixnum)
       (commutative-binop -NegInt -NonPosInt -NegFixnum)
       (-NonPosInt -NonPosInt . -> . -NonPosFixnum)
       (-Int -Int . -> . -Fixnum))))
  (define fx--type
    (lambda ()
      (fx-from-cases
       (binop -Zero)
       (map (lambda (t) (commutative-binop t -Zero t))
            (list -One -PosByte -Byte -PosIndex -Index))
       (-One -One . -> . -Zero)
       (-PosByte -One . -> . -Byte)
       (-PosIndex -One . -> . -Index)
       (-PosFixnum -One . -> . -NonNegFixnum)
       (-PosInt -One . -> . -NonNegFixnum)
       (-NegInt -Nat . -> . -NegFixnum)
       (-NonPosInt -PosInt . -> . -NegFixnum)
       (-NonPosInt -Nat . -> . -NonPosFixnum)
       (-PosInt -NonPosInt . -> . -PosInt)
       (-Nat -NegInt . -> . -PosInt)
       (-Nat -NonPosInt . -> . -Nat)
       (-Int -Int . -> . -Fixnum))))
  (define fx*-type
    (lambda ()
      (fx-from-cases
       (map binop (list -Zero -One))
       (commutative-binop -Zero -Int)
       (-PosByte -PosByte . -> . -PosIndex)
       (-Byte -Byte . -> . -Index)
       (-PosInt -PosInt . -> . -PosFixnum)
       (commutative-binop -PosInt -NegInt -NegFixnum)
       (-NegInt -NegInt . -> . -PosFixnum)
       (-Nat -Nat . -> . -NonNegFixnum)
       (commutative-binop -Nat -NonPosInt -NonPosFixnum)
       (-NonPosFixnum -NonPosFixnum . -> . -NonNegFixnum)
       (-Int -Int . -> . -Fixnum))))
  (define fxquotient-type
    (lambda ()
      (fx-from-cases
       (-Zero -Int . -> . -Zero)
       (map (lambda (t) (-> t -One t)) ; division by one is identity
            (list -PosByte -Byte -PosIndex -Index
                  -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum))
       (-Byte -Nat . -> . -Byte)
       (-Index -Nat . -> . -Index)
       (-Nat -Nat . -> . -NonNegFixnum)
       (commutative-binop -Nat -NonPosInt -NonPosFixnum)
       (-NonPosInt -NonPosInt . -> . -NonNegFixnum)
       (-Int -Int . -> . -Fixnum))))
  (define fxremainder-type ; result has same sign as first arg
    (lambda ()
      (fx-from-cases
       (-One -One . -> . -Zero)
       (map (lambda (t) (list (-> -Nat t t)
                              (-> t -Int t)))
            (list -Byte -Index))
       (-Nat -Int . -> . -NonNegFixnum)
       (-NonPosInt -Int . -> . -NonPosFixnum)
       (-Int -Int . -> . -Fixnum))))
  (define fxmodulo-type ; result has same sign as second arg
    (lambda ()
      (fx-from-cases
       (-One -One . -> . -Zero)
       (map (lambda (t) (list (-> -Int t t)
                              (-> t -Nat t)))
            (list -Byte -Index))
       (-Int -Nat . -> . -NonNegFixnum)
       (-Int -NonPosInt . -> . -NonPosFixnum)
       (-Int -Int . -> . -Fixnum))))
  (define fxabs-type
    (lambda ()
      (fx-from-cases
       (map unop (list -Zero -One -PosByte -Byte -PosIndex -Index))
       ((Un -PosInt -NegInt) . -> . -PosFixnum)
       (-Int . -> . -NonNegFixnum))))
  (define fx=-type
    (lambda ()
      (fx-from-cases
       ;; we could rule out cases like (= Pos Neg), but we currently don't
       (map (lambda (l) (apply exclude-zero l))
            (list (list -Byte -PosByte)
                  (list -Index -PosIndex)
                  (list -Nat -PosFixnum)
                  (list -NonPosInt -NegFixnum)
                  (list -Int (Un -PosFixnum -NegFixnum))))
       (map (lambda (t) (commutative-equality/filter -Int t))
            (list -One -PosByte -Byte -PosIndex -Index -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum))
       (comp -Int))))
  (define fx<-type
    (lambda ()
      (fx-from-cases
       (-> -Pos -One B : (-FS (-filter (Un) 0) -top)) ; can't happen
       (-> -Nat -One B : (-FS (-filter -Zero 0) -top))
       (-> -Int -One B : (-FS (-filter -NonPosFixnum 0) (-filter -PosFixnum 0)))
       ;; bleh, this repeats cases below, but since we can only match a single
       ;; case, we need to put it here as well, or we would not gain that info,
       ;; as another unrelated case would match
       (-> -Byte -Zero B : (-FS (-filter (Un) 0) -top))
       (-> -Byte -One B : (-FS (-filter -Zero 0) -top))
       (-> -Zero -Byte B : (-FS (-filter -PosByte 1) (-filter -Zero 1)))
       (-> -Byte -PosByte B : (-FS -top (-filter -PosByte 0)))
       (-> -Byte -Byte B : (-FS (-filter -PosByte 1) -top))
       (-> -Pos -Byte B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
       (-> -Byte -Pos B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
       (-> -Byte -Nat B : (-FS -top (-filter -Byte 1)))
       (-> -Index -Zero B : (-FS (-filter (Un) 0) -top))
       (-> -Index -One B : (-FS (-filter -Zero 0) -top))
       (-> -Zero -Index B : (-FS (-filter -PosIndex 1) (-filter -Zero 1)))
       (-> -Index -PosIndex B : (-FS -top (-filter -PosIndex 0)))
       (-> -Index -Index B : (-FS (-filter -PosIndex 1) -top))
       (-> -Pos -Index B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
       (-> -Index -Pos B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
       (-> -Nat -Byte B : (-FS (-and (-filter -Byte 0) (-filter -PosByte 1)) -top))
       (-> -Nat -Index B : (-FS (-and (-filter -Index 0) (-filter -PosIndex 1)) -top))
       (-> -Index -Nat B : (-FS -top (-filter -Index 1)))
       ;; general integer cases
       (-> -Int -Zero B : (-FS (-filter -NegFixnum 0) (-filter -NonNegFixnum 0)))
       (-> -Zero -Int B : (-FS (-filter -PosFixnum 1) (-filter -NonPosFixnum 1)))
       (-> -Int -PosInt B : (-FS -top (-filter -PosFixnum 0)))
       (-> -Int -Nat B : (-FS -top (-filter -NonNegFixnum 0)))
       (-> -Nat -Int B : (-FS (-filter -PosFixnum 1) -top))
       (-> -Int -NonPosInt B : (-FS (-filter -NegFixnum 0) -top))
       (-> -NegInt -Int B : (-FS -top (-filter -NegFixnum 1)))
       (-> -NonPosInt -Int B : (-FS -top (-filter -NonPosFixnum 1)))
       (comp -Int))))
  (define fx>-type
    (lambda ()
      (fx-from-cases
       (-> -One -Pos B : (-FS (-filter (Un) 1) -top)) ; can't happen
       (-> -One -Nat B : (-FS (-filter -Zero 1) -top))
       (-> -One -Int B : (-FS (-filter -NonPosFixnum 1) (-filter -PosFixnum 1)))
       (-> -Byte -Zero B : (-FS (-filter -PosByte 0) (-filter -Zero 0)))
       (-> -Zero -Byte B : (-FS (-filter (Un) 1) -top))
       (-> -One -Byte B : (-FS (-filter -Zero 1) (-filter -PosByte 1)))
       (-> -PosByte -Byte B : (-FS -top (-filter -PosByte 1)))
       (-> -Byte -Byte B : (-FS (-filter -PosByte 0) -top))
       (-> -Byte -Pos B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
       (-> -Pos -Byte B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
       (-> -Byte -Nat B : (-FS (-and (-filter -PosByte 0) (-filter -Byte 1)) -top))
       (-> -Zero -Index B : (-FS (-filter (Un) 1) -top))
       (-> -One -Index B : (-FS (-filter -Zero 1) -top))
       (-> -Index -Zero B : (-FS (-filter -PosIndex 0) (-filter -Zero 0)))
       (-> -PosIndex -Index B : (-FS -top (-filter -PosIndex 1)))
       (-> -Index -Index B : (-FS (-filter -PosIndex 0) -top))
       (-> -Index -Pos B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
       (-> -Pos -Index B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
       (-> -Index -Nat B : (-FS (-and (-filter -PosIndex 0) (-filter -Index 1)) -top))
       (-> -Nat -Byte B : (-FS -top (-filter -Byte 0)))
       (-> -Nat -Index B : (-FS -top (-filter -Index 0)))
       ;; general integer cases
       (-> -Zero -Int B : (-FS (-filter -NegFixnum 1) (-filter -NonNegFixnum 1)))
       (-> -Int -Zero  B : (-FS (-filter -PosFixnum 0) (-filter -NonPosFixnum 0)))
       (-> -PosInt -Int B : (-FS -top (-filter -PosFixnum 1)))
       (-> -Nat -Int B : (-FS -top (-filter -NonNegFixnum 1)))
       (-> -Int -Nat B : (-FS (-filter -PosFixnum 0) -top))
       (-> -NonPosInt -Int B : (-FS (-filter -NegFixnum 1) -top))
       (-> -Int -NegInt B : (-FS -top (-filter -NegFixnum 0)))
       (-> -Int -NonPosInt B : (-FS -top (-filter -NonPosFixnum 0)))
       (comp -Int))))
  (define fx<=-type
    (lambda ()
      (fx-from-cases
       (-> -Pos -One B : (-FS (-filter -One 0) -top))
       (-> -Byte -Zero B : (-FS (-filter -Zero 0) (-filter -PosByte 0)))
       (-> -Zero -Byte B : (-FS -top (-filter (Un) 1)))
       (-> -One -Byte B : (-FS (-filter -PosByte 1) (-filter -Zero 1)))
       (-> -PosByte -Byte B : (-FS (-filter -PosByte 1) -top))
       (-> -Byte -Byte B : (-FS -top (-filter -PosByte 0)))
       (-> -Pos -Byte B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
       (-> -Byte -Pos B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
       (-> -Byte -Nat B : (-FS -top (-and (-filter -PosByte 0) (-filter -Byte 1))))
       (-> -Index -Zero B : (-FS (-filter -Zero 0) (-filter -PosIndex 0)))
       (-> -Zero -Index B : (-FS -top (-filter (Un) 1)))
       (-> -One -Index B : (-FS (-filter -PosIndex 1) (-filter -Zero 1)))
       (-> -PosIndex -Index B : (-FS (-filter -PosIndex 1) -top))
       (-> -Index -Index B : (-FS -top (-filter -PosIndex 0)))
       (-> -Pos -Index B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
       (-> -Index -Pos B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
       (-> -Nat -Byte B : (-FS (-filter -Byte 0) -top))
       (-> -Nat -Index B : (-FS (-filter -Index 0) -top))
       (-> -Index -Nat B : (-FS -top (-and (-filter -PosIndex 0) (-filter -Index 1))))
       ;; general integer cases
       (-> -Nat -Zero B : (-FS (-filter -Zero 0) -top))
       (-> -One -Nat B : (-FS (-filter -PosFixnum 1) (-filter -Zero 1)))
       (-> -Int -Zero B : (-FS (-filter -NonPosFixnum 0) (-filter -PosFixnum 0)))
       (-> -Zero -Int B : (-FS (-filter -NonNegFixnum 1) (-filter -NegFixnum 1)))
       (-> -PosInt -Int B : (-FS (-filter -PosFixnum 1) -top))
       (-> -Int -Nat B : (-FS -top (-filter -PosFixnum 0)))
       (-> -Nat -Int B : (-FS (-filter -NonNegFixnum 1) -top))
       (-> -Int -NegInt B : (-FS (-filter -NegFixnum 0) -top))
       (-> -Int -NonPosInt B : (-FS (-filter -NonPosFixnum 0) -top))
       (-> -NonPosInt -Int B : (-FS -top (-filter -NegFixnum 1)))
       (comp -Int))))
  (define fx>=-type
    (lambda ()
      (fx-from-cases
       (-> -One -Pos B : (-FS (-filter -One 1) -top))
       (-> -Zero -Byte B : (-FS (-filter -Zero 1) (-filter -PosByte 1)))
       (-> -Byte -Zero B : (-FS -top (-filter (Un) 0)))
       (-> -Byte -One B : (-FS (-filter -PosByte 0) (-filter -Zero 0)))
       (-> -Byte -PosByte B : (-FS (-filter -PosByte 0) -top))
       (-> -Byte -Byte B : (-FS -top (-filter -PosByte 1)))
       (-> -Byte -Pos B : (-FS (-and (-filter -PosByte 1) (-filter -PosByte 0)) -top))
       (-> -Pos -Byte B : (-FS -top (-and (-filter -PosByte 1) (-filter -PosByte 0))))
       (-> -Byte -Nat B : (-FS (-filter -Byte 1) -top))
       (-> -Zero -Index B : (-FS (-filter -Zero 1) (-filter -PosIndex 1)))
       (-> -Index -Zero B : (-FS -top (-filter (Un) 0)))
       (-> -Index -One B : (-FS (-filter -PosIndex 0) (-filter -Zero 0)))
       (-> -Index -PosIndex B : (-FS (-filter -PosIndex 0) -top))
       (-> -Index -Index B : (-FS -top (-filter -PosIndex 1)))
       (-> -Index -Pos B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
       (-> -Pos -Index B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
       (-> -Index -Nat B : (-FS (-filter -Index 1) -top))
       (-> -Nat -Byte B : (-FS -top (-and (-filter -Byte 0) (-filter -PosByte 1))))
       (-> -Nat -Index B : (-FS -top (-and (-filter -Index 0) (-filter -PosIndex 1))))
       ;; general integer cases
       (-> -Zero -Nat B : (-FS (-filter -Zero 1) -top))
       (-> -Nat -One B : (-FS (-filter -PosFixnum 0) (-filter -Zero 0)))
       (-> -Zero -Int B : (-FS (-filter -NonPosFixnum 1) (-filter -PosFixnum 1)))
       (-> -Int -Zero B : (-FS (-filter -NonNegFixnum 0) (-filter -NegFixnum 0)))
       (-> -Int -PosInt B : (-FS (-filter -PosFixnum 0) -top))
       (-> -Nat -Int B : (-FS -top (-filter -PosFixnum 1)))
       (-> -Int -Nat B : (-FS (-filter -NonNegFixnum 0) -top))
       (-> -NegInt -Int B : (-FS (-filter -NegFixnum 1) -top))
       (-> -NonPosInt -Int B : (-FS (-filter -NonPosFixnum 1) -top))
       (-> -Int -NonPosInt B : (-FS -top (-filter -NegFixnum 0)))
       (comp -Int))))
  (define fxmin-type
    (lambda ()
      (fx-from-cases
       (binop -Zero)
       (binop -One)
       (commutative-binop -Zero (Un -Zero -One) -Zero)
       (commutative-binop -PosByte -PosInt -PosByte)
       (commutative-binop -Byte -Nat -Byte)
       (commutative-binop -PosIndex -PosInt -PosIndex)
       (commutative-binop -Index -Nat -Index)
       (-> -Pos -Pos -PosFixnum)
       (-> -Nat -Nat -NonNegFixnum)
       (commutative-binop -NegInt -Int -NegFixnum)
       (commutative-binop -NonPosInt -Int -NonPosInt)
       (-> -Int -Int -Fixnum))))
  (define fxmax-type
    (lambda ()
      (fx-from-cases
       (binop -Zero)
       (commutative-binop -One (Un -Zero -One) -One)
       (commutative-binop -PosByte -Byte -PosByte)
       (binop -Byte)
       (commutative-binop -PosIndex -Index -PosIndex)
       (map binop (list -Index -NegFixnum -NonPosFixnum))
       (commutative-binop -PosInt -Int -PosFixnum)
       (commutative-binop -Nat -Int -NonNegFixnum)
       (-> -Int -Int -Fixnum))))
  (define fxand-type
    (lambda ()
      (fx-from-cases
       (commutative-binop -Zero -Int -Zero)
       (commutative-binop -Byte -Int -Byte)
       (commutative-binop -Index -Int -Index)
       (binop -Nat -NonNegFixnum)
       (binop -NegInt -NegFixnum)
       (binop -NonPosInt -NonPosFixnum)
       (binop -Int -Fixnum))))
  (define fxior-type
    (lambda ()
      (fx-from-cases
       (binop -Zero)
       (commutative-binop -One -Zero -One)
       (commutative-binop -PosByte -Byte -PosByte)
       (binop -Byte)
       (commutative-binop -PosIndex -Index -PosIndex)
       (binop -Index)
       (commutative-binop -PosInt -Nat -PosFixnum)
       (binop -Nat -NonNegFixnum)
       (commutative-binop -NegInt -Int -NegFixnum) ; as long as there's one negative, the result is negative
       (binop -Int -Fixnum))))
  (define fxxor-type
    (lambda ()
      (fx-from-cases
       (binop -Zero)
       (binop -One -Zero)
       (binop -Byte)
       (binop -Index)
       (binop -Nat -NonNegFixnum)
       (binop -NonPosInt -NonNegFixnum)
       (commutative-binop -NegInt -Nat -NegFixnum)
       (commutative-binop -NonPosInt -Nat -NonPosFixnum)
       (binop -Int -Fixnum))))
  (define fxnot-type
    (lambda ()
      (fx-from-cases
       (-Nat . -> . -NegFixnum)
       (-NegInt . -> . -NonNegFixnum)
       (-Int . -> . -Fixnum))))
  (define fxlshift-type
    (lambda ()
      (fx-from-cases
       (map (lambda (x) (-> x -Zero x))
            (list -Zero -One -PosByte -Byte -PosIndex -Index))
       (-> -PosInt -Int -PosFixnum) ; negative 2nd arg errors, so we can't reach 0
       (-> -Nat -Int -NonNegFixnum)
       (-> -NegInt -Int -NegFixnum)
       (-> -NonPosInt -Int -NonPosFixnum)
       (binop -Int -Fixnum))))
  (define fxrshift-type
    (lambda ()
      (fx-from-cases
       (map (lambda (x) (-> x -Zero x))
            (list -Zero -One -PosByte -Byte -PosIndex -Index))
       (-> -Nat -Int -NonNegFixnum) ; can reach 0
       (-> -NegInt -Int -NegFixnum) ; can't reach 0
       (-> -NonPosInt -Int -NonPosFixnum)
       (binop -Int -Fixnum))))


  (define flabs-type
    (lambda ()
      (cl->* (-> -FlonumZero -FlonumZero)
             (-> (Un -PosFlonum -NegFlonum) -PosFlonum)
             (-> -Flonum -NonNegFlonum))))
  (define fl+-type
    (lambda ()
      (from-cases (map (lambda (t) (commutative-binop t -FlonumZero t))
                       ;; not all float types. singleton types are ruled out, since NaN can arise
                       (list -FlonumZero -FlonumNan -PosFlonum -NonNegFlonum
                             -NegFlonum -NonPosFlonum -Flonum))
                  (commutative-binop -NonNegFlonum -PosFlonum -PosFlonum)
                  (map binop (list -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum))
                  (-Flonum -Flonum . -> . -Flonum))))
  (define fl--type
    (lambda ()
      (from-cases (binop -FlonumZero)
                  (-NegFlonum (Un -NonNegFlonum -FlonumZero) . -> . -NegFlonum)
                  ((Un -NonPosFlonum -FlonumZero) -PosFlonum . -> . -NegFlonum)
                  (-NonPosFlonum -NonNegFlonum . -> . -NonPosFlonum)
                  (binop -Flonum))))
  (define fl*-type
    (lambda ()
      (from-cases (binop -FlonumZero)
                  ;; we don't have Pos Pos -> Pos, possible underflow
                  (binop -PosFlonum -NonNegFlonum)
                  (binop -NonNegFlonum)
                  (commutative-binop -NegFlonum -PosFlonum -NonPosFlonum)
                  (binop -NegFlonum -NonNegFlonum)
                  (binop -Flonum))))
  (define fl/-type
    (lambda ()
      (from-cases (-FlonumZero -Flonum . -> . -FlonumZero)
                  (-PosFlonum -PosFlonum . -> . -NonNegFlonum) ; possible underflow
                  (commutative-binop -PosFlonum -NegFlonum -NonPosFlonum)
                  (-NegFlonum -NegFlonum . -> . -NonNegFlonum)
                  (binop -Flonum))))
  (define fl=-type
    (lambda ()
      (from-cases (map (lambda (l) (exclude-zero (car l) (cadr l) -FlonumZero))
                       (list (list -NonNegFlonum -PosFlonum)
                             (list -NonPosFlonum -NegFlonum)))
                  (map (lambda (t) (commutative-equality/filter -Flonum t))
                       (list -FlonumZero -PosFlonum -NonNegFlonum
                             -NegFlonum -NonPosFlonum))
                  (comp -Flonum))))
  (define fl<-type
    (lambda ()
      (from-cases
       ;; false case, we know nothing, lhs may be NaN. same for all comparison that can involve floats
       (-> -FlonumZero -Flonum B : (-FS (-filter -PosFlonum 1) -top))
       (-> -Flonum -FlonumZero B : (-FS (-filter -NegFlonum 0) -top))
       (-> -PosFlonum -Flonum B : (-FS (-filter -PosFlonum 1) -top))
       (-> -Flonum -PosFlonum B)
       (-> -NonNegFlonum -Flonum B : (-FS (-filter -PosFlonum 1) -top))
       (-> -Flonum -NonNegFlonum B)
       (-> -NegFlonum -Flonum B)
       (-> -Flonum -NegFlonum B : (-FS (-filter -NegFlonum 0) -top))
       (-> -NonPosFlonum -Flonum B)
       (-> -Flonum -NonPosFlonum B : (-FS (-filter -NegFlonum 0) -top))
       (comp -Flonum))))
  (define fl>-type
    (lambda ()
      (from-cases
       (-> -FlonumZero -Flonum B : (-FS (-filter -NegFlonum 1) -top))
       (-> -Flonum -FlonumZero B : (-FS (-filter -PosFlonum 0) -top))
       (-> -PosFlonum -Flonum B)
       (-> -Flonum -PosFlonum B : (-FS (-filter -PosFlonum 0) -top))
       (-> -NonNegFlonum -Flonum B)
       (-> -Flonum -NonNegFlonum B : (-FS (-filter -PosFlonum 0) -top))
       (-> -NegFlonum -Flonum B : (-FS (-filter -NegFlonum 1) -top))
       (-> -Flonum -NegFlonum B)
       (-> -NonPosFlonum -Flonum B : (-FS (-filter -NegFlonum 1) -top))
       (-> -Flonum -NonPosFlonum B)
       (comp -Flonum))))
  (define fl<=-type
    (lambda ()
      (from-cases
       (-> -FlonumZero -Flonum B : (-FS (-filter -NonNegFlonum 1) -top))
       (-> -Flonum -FlonumZero B : (-FS (-filter -NonPosFlonum 0) -top))
       (-> -PosFlonum -Flonum B : (-FS (-filter -PosFlonum 1) -top))
       (-> -Flonum -PosFlonum B)
       (-> -NonNegFlonum -Flonum B : (-FS (-filter -NonNegFlonum 1) -top))
       (-> -Flonum -NonNegFlonum B)
       (-> -NegFlonum -Flonum B)
       (-> -Flonum -NegFlonum B : (-FS (-filter -NegFlonum 0) -top))
       (-> -NonPosFlonum -Flonum B)
       (-> -Flonum -NonPosFlonum B : (-FS (-filter -NonPosFlonum 0) -top))
       (comp -Flonum))))
  (define fl>=-type
    (lambda ()
      (from-cases
       (-> -FlonumZero -Flonum B : (-FS (-filter -NonPosFlonum 1) -top))
       (-> -Flonum -FlonumZero B : (-FS (-filter -NonNegFlonum 0) -top))
       (-> -PosFlonum -Flonum B)
       (-> -Flonum -PosFlonum B : (-FS (-filter -PosFlonum 0) -top))
       (-> -NonNegFlonum -Flonum B)
       (-> -Flonum -NonNegFlonum B : (-FS (-filter -NonNegFlonum 0) -top))
       (-> -NegFlonum -Flonum B : (-FS (-filter -NegFlonum 1) -top))
       (-> -Flonum -NegFlonum B)
       (-> -NonPosFlonum -Flonum B)
       (-> -Flonum -NonPosFlonum B : (-FS (-filter -NonPosFlonum 0) -top))
       (comp -Flonum))))
  (define flmin-type
    (lambda ()
      (from-cases (commutative-case -NegFlonum -Flonum)
                  (commutative-case -NonPosFlonum -Flonum)
                  (map binop (list -PosFlonum -NonNegFlonum -Flonum)))))
  (define flmax-type
    (lambda ()
      (from-cases (commutative-case -PosFlonum -Flonum)
                  (commutative-case -NonNegFlonum -Flonum)
                  (map binop (list -NegFlonum -NonPosFlonum -Flonum)))))
  (define flround-type ; truncate too
    (lambda ()
      (from-cases (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                                  -NonNegFlonum -NonPosFlonum -Flonum)))))
  (define flfloor-type
    (lambda ()
      (from-cases (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                                  -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum)))))
  (define flceiling-type
    (lambda ()
      (from-cases (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                                  -PosFlonum -NonNegFlonum -NonPosFlonum -Flonum)))))
  (define fllog-type
    (lambda ()
      (from-cases (-> -FlonumZero -NegFlonum) ; -inf
                  (unop -Flonum))))
  (define flexp-type
    (lambda ()
      (from-cases (-NonNegFlonum . -> . -PosFlonum)
                  (-NegFlonum . -> . -NonNegFlonum)
                  (-Flonum . -> . -Flonum)))) ; nan is the only non nonnegative case (returns nan)
  (define flsqrt-type
    (lambda ()
      (from-cases (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                                  -NonNegFlonum ; we don't have positive case, possible underflow
                                  -Flonum))))) ; anything negative returns nan
  (define flexpt-type
    (lambda ()
      (from-cases (-FlonumZero -PosFlonum . -> . -FlonumZero) ; (flexpt -0.0 0.1) -> 0.0 ; not sign preserving
                  ((Un -PosFlonum -NegFlonum) -FlonumZero . -> . -PosFlonum) ; always returns 1.0
                  (-NonNegFlonum -Flonum . -> . -NonNegFlonum) ; can underflow
                  (-Flonum -Flonum . -> . -Flonum))))

  (define fx->fl-type
    (lambda ()
      (fx-from-cases
       (-PosInt . -> . -PosFlonum)
       (-Nat . -> . -NonNegFlonum)
       (-NegInt . -> . -NegFlonum)
       (-NonPosInt . -> . -NonPosFlonum)
       (-Int . -> . -Flonum))))
  (define fl->fx-type
    (lambda ()
      (from-cases
       (-FlonumZero . -> . -Zero)
       (-PosFlonum . -> . -PosFixnum)
       (-NegFlonum . -> . -NegFixnum)
       (-NonNegFlonum . -> . -NonNegFixnum)
       (-NonPosFlonum . -> . -NonPosFixnum)
       (-Flonum . -> . -Fixnum))))
  (define make-flrectangular-type (lambda () (-Flonum -Flonum . -> . -FloatComplex)))
  (define flreal-part-type (lambda () (-FloatComplex . -> . -Flonum)))
  (define flimag-part-type (lambda () (-FloatComplex . -> . -Flonum)))
  (define flrandom-type (lambda () (-Pseudo-Random-Generator . -> . -Flonum)))

  ;; There's a repetitive pattern in the types of each comparison operator.
  ;; As explained below, this is because filters don't do intersections.
  (define (<-type-pattern base pos non-neg neg non-pos [zero -RealZero])
    (list (-> base zero B : (-FS (-filter neg 0) (-filter non-neg 0)))
          (-> zero base B : (-FS (-filter pos 1) (-filter non-pos 1)))
          (-> base -PosReal B : (-FS -top (-filter pos 0)))
          (-> base -NonNegReal B : (-FS -top (-filter non-neg 0)))
          (-> -NonNegReal base B : (-FS (-filter pos 1) -top))
          (-> base -NonPosReal B : (-FS (-filter neg 0) -top))
          (-> -NegReal base B : (-FS -top (-filter neg 1)))
          (-> -NonPosReal base B : (-FS -top (-filter non-pos 1)))))
  (define (>-type-pattern base pos non-neg neg non-pos [zero -RealZero])
    (list (-> base zero B : (-FS (-filter pos 0) (-filter non-pos 0)))
          (-> zero base B : (-FS (-filter neg 1) (-filter non-neg 1)))
          (-> base -NonNegReal B : (-FS (-filter pos 0) -top))
          (-> -PosReal base B : (-FS -top (-filter pos 1)))
          (-> -NonNegReal base B : (-FS -top (-filter non-neg 1)))
          (-> -NonPosReal base B : (-FS (-filter neg 1) -top))
          (-> base -NegReal B : (-FS -top (-filter neg 0)))
          (-> base -NonPosReal B : (-FS -top (-filter non-pos 0)))))
  ;; this is > with flipped filters
  (define (<=-type-pattern base pos non-neg neg non-pos [zero -RealZero])
    (list (-> base zero B : (-FS (-filter non-pos 0) (-filter pos 0)))
          (-> zero base B : (-FS (-filter non-neg 1) (-filter neg 1)))
          (-> base -NonNegReal B : (-FS -top (-filter pos 0)))
          (-> -PosReal base B : (-FS (-filter pos 1) -top))
          (-> -NonNegReal base B : (-FS (-filter non-neg 1) -top))
          (-> -NonPosReal base B : (-FS -top (-filter neg 1)))
          (-> base -NegReal B : (-FS (-filter neg 0) -top))
          (-> base -NonPosReal B : (-FS (-filter non-pos 0) -top))))
  (define (>=-type-pattern base pos non-neg neg non-pos [zero -RealZero])
    (list (-> base zero B : (-FS (-filter non-neg 0) (-filter neg 0)))
          (-> zero base B : (-FS (-filter non-pos 1) (-filter pos 1)))
          (-> base -PosReal B : (-FS (-filter pos 0) -top))
          (-> base -NonNegReal B : (-FS (-filter non-neg 0) -top))
          (-> -NonNegReal base B : (-FS -top (-filter pos 1)))
          (-> base -NonPosReal B : (-FS -top (-filter neg 0)))
          (-> -NegReal base B : (-FS (-filter neg 1) -top))
          (-> -NonPosReal base B : (-FS (-filter non-pos 1) -top))))

  (define (negation-pattern pos neg non-neg non-pos)
    (list (-> pos neg)
          (-> non-neg non-pos)
          (-> neg pos)
          (-> non-pos non-neg)
          (-> -Zero pos neg)
          (-> -Zero non-neg non-pos)
          (-> -Zero neg pos)
          (-> -Zero non-pos non-neg)))

  ;; Used because (- min-fixnum) > max-fixnum
  (define (half-negation-pattern pos neg non-neg non-pos)
    (list (-> pos neg)
          (-> non-neg non-pos)
          (-> -Zero pos neg)
          (-> -Zero non-neg non-pos)))

  (define abs-cases ; used both for abs and magnitude
    (list
     (map unop (list -Zero -One -PosByte -Byte -PosIndex -Index -PosFixnum -NonNegFixnum))
     ;; abs may not be closed on fixnums. (abs min-fixnum) is not a fixnum
     ((Un -PosInt -NegInt) . -> . -PosInt)
     (-Int . -> . -Nat)
     ((Un -PosRat -NegRat) . -> . -PosRat)
     (-Rat . -> . -NonNegRat)
     (-FlonumZero . -> . -FlonumZero)
     ((Un -PosFlonum -NegFlonum) . -> . -PosFlonum)
     (-Flonum . -> . -NonNegFlonum)
     (-SingleFlonumZero . -> . -SingleFlonumZero)
     ((Un -PosSingleFlonum -NegSingleFlonum) . -> . -PosSingleFlonum)
     (-SingleFlonum . -> . -NonNegSingleFlonum)
     (-InexactRealZero . -> . -InexactRealZero)
     ((Un -PosInexactReal -NegInexactReal) . -> . -PosInexactReal)
     (-InexactReal . -> . -NonNegInexactReal)
     ((Un -PosReal -NegReal) . -> . -PosReal)
     (-Real . -> . -NonNegReal)))

  (define extfl-unop (lambda () (unop -ExtFlonum)))
  
  (define extflabs-type
    (lambda ()
      (cl->* (-> -ExtFlonumZero -ExtFlonumZero)
             (-> (Un -PosExtFlonum -NegExtFlonum) -PosExtFlonum)
             (-> -ExtFlonum -NonNegExtFlonum))))
  (define extfl+-type
    (lambda ()
      (from-cases (map (lambda (t) (commutative-binop t -ExtFlonumZero t))
                       ;; not all float types. singleton types are ruled out, since NaN can arise
                       (list -ExtFlonumZero -ExtFlonumNan -PosExtFlonum -NonNegExtFlonum
                             -NegExtFlonum -NonPosExtFlonum -ExtFlonum))
                  (commutative-binop -NonNegExtFlonum -PosExtFlonum -PosExtFlonum)
                  (map binop (list -NonNegExtFlonum -NegExtFlonum -NonPosExtFlonum -ExtFlonum))
                  (-ExtFlonum -ExtFlonum . -> . -ExtFlonum))))
  (define extfl--type
    (lambda ()
      (from-cases (binop -ExtFlonumZero)
                  (-NegExtFlonum (Un -NonNegExtFlonum -ExtFlonumZero) . -> . -NegExtFlonum)
                  ((Un -NonPosExtFlonum -ExtFlonumZero) -PosExtFlonum . -> . -NegExtFlonum)
                  (-NonPosExtFlonum -NonNegExtFlonum . -> . -NonPosExtFlonum)
                  (binop -ExtFlonum))))
  (define extfl*-type
    (lambda ()
      (from-cases (binop -ExtFlonumZero)
                  ;; we don't have Pos Pos -> Pos, possible underflow
                  (binop -PosExtFlonum -NonNegExtFlonum)
                  (binop -NonNegExtFlonum)
                  (commutative-binop -NegExtFlonum -PosExtFlonum -NonPosExtFlonum)
                  (binop -NegExtFlonum -NonNegExtFlonum)
                  (binop -ExtFlonum))))
  (define extfl/-type
    (lambda ()
      (from-cases (-ExtFlonumZero -ExtFlonum . -> . -ExtFlonumZero)
                  (-PosExtFlonum -PosExtFlonum . -> . -NonNegExtFlonum) ; possible underflow
                  (commutative-binop -PosExtFlonum -NegExtFlonum -NonPosExtFlonum)
                  (-NegExtFlonum -NegExtFlonum . -> . -NonNegExtFlonum)
                  (binop -ExtFlonum))))
  (define extfl=-type
    (lambda ()
      (from-cases (map (lambda (l) (exclude-zero (car l) (cadr l) -ExtFlonumZero))
                       (list (list -NonNegExtFlonum -PosExtFlonum)
                             (list -NonPosExtFlonum -NegExtFlonum)))
                  (map (lambda (t) (commutative-equality/filter -ExtFlonum t))
                       (list -ExtFlonumZero -PosExtFlonum -NonNegExtFlonum
                             -NegExtFlonum -NonPosExtFlonum))
                  (comp -ExtFlonum))))
  (define extfl<-type
    (lambda ()
      (from-cases
       ;; false case, we know nothing, lhs may be NaN. same for all comparison that can involve floats
       (-> -ExtFlonumZero -ExtFlonum B : (-FS (-filter -PosExtFlonum 1) -top))
       (-> -ExtFlonum -ExtFlonumZero B : (-FS (-filter -NegExtFlonum 0) -top))
       (-> -PosExtFlonum -ExtFlonum B : (-FS (-filter -PosExtFlonum 1) -top))
       (-> -ExtFlonum -PosExtFlonum B)
       (-> -NonNegExtFlonum -ExtFlonum B : (-FS (-filter -PosExtFlonum 1) -top))
       (-> -ExtFlonum -NonNegExtFlonum B)
       (-> -NegExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NegExtFlonum B : (-FS (-filter -NegExtFlonum 0) -top))
       (-> -NonPosExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NonPosExtFlonum B : (-FS (-filter -NegExtFlonum 0) -top))
       (comp -ExtFlonum))))
  (define extfl>-type
    (lambda ()
      (from-cases
       (-> -ExtFlonumZero -ExtFlonum B : (-FS (-filter -NegExtFlonum 1) -top))
       (-> -ExtFlonum -ExtFlonumZero B : (-FS (-filter -PosExtFlonum 0) -top))
       (-> -PosExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -PosExtFlonum B : (-FS (-filter -PosExtFlonum 0) -top))
       (-> -NonNegExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NonNegExtFlonum B : (-FS (-filter -PosExtFlonum 0) -top))
       (-> -NegExtFlonum -ExtFlonum B : (-FS (-filter -NegExtFlonum 1) -top))
       (-> -ExtFlonum -NegExtFlonum B)
       (-> -NonPosExtFlonum -ExtFlonum B : (-FS (-filter -NegExtFlonum 1) -top))
       (-> -ExtFlonum -NonPosExtFlonum B)
       (comp -ExtFlonum))))
  (define extfl<=-type
    (lambda ()
      (from-cases
       (-> -ExtFlonumZero -ExtFlonum B : (-FS (-filter -NonNegExtFlonum 1) -top))
       (-> -ExtFlonum -ExtFlonumZero B : (-FS (-filter -NonPosExtFlonum 0) -top))
       (-> -PosExtFlonum -ExtFlonum B : (-FS (-filter -PosExtFlonum 1) -top))
       (-> -ExtFlonum -PosExtFlonum B)
       (-> -NonNegExtFlonum -ExtFlonum B : (-FS (-filter -NonNegExtFlonum 1) -top))
       (-> -ExtFlonum -NonNegExtFlonum B)
       (-> -NegExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NegExtFlonum B : (-FS (-filter -NegExtFlonum 0) -top))
       (-> -NonPosExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NonPosExtFlonum B : (-FS (-filter -NonPosExtFlonum 0) -top))
       (comp -ExtFlonum))))
  (define extfl>=-type
    (lambda ()
      (from-cases
       (-> -ExtFlonumZero -ExtFlonum B : (-FS (-filter -NonPosExtFlonum 1) -top))
       (-> -ExtFlonum -ExtFlonumZero B : (-FS (-filter -NonNegExtFlonum 0) -top))
       (-> -PosExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -PosExtFlonum B : (-FS (-filter -PosExtFlonum 0) -top))
       (-> -NonNegExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NonNegExtFlonum B : (-FS (-filter -NonNegExtFlonum 0) -top))
       (-> -NegExtFlonum -ExtFlonum B : (-FS (-filter -NegExtFlonum 1) -top))
       (-> -ExtFlonum -NegExtFlonum B)
       (-> -NonPosExtFlonum -ExtFlonum B)
       (-> -ExtFlonum -NonPosExtFlonum B : (-FS (-filter -NonPosExtFlonum 0) -top))
       (comp -ExtFlonum))))
  (define extflmin-type
    (lambda ()
      (from-cases (commutative-case -NegExtFlonum -ExtFlonum)
                  (commutative-case -NonPosExtFlonum -ExtFlonum)
                  (map binop (list -PosExtFlonum -NonNegExtFlonum -ExtFlonum)))))
  (define extflmax-type
    (lambda ()
      (from-cases (commutative-case -PosExtFlonum -ExtFlonum)
                  (commutative-case -NonNegExtFlonum -ExtFlonum)
                  (map binop (list -NegExtFlonum -NonPosExtFlonum -ExtFlonum)))))
  (define extflround-type ; truncate too
    (lambda ()
      (from-cases (map unop (list -ExtFlonumPosZero -ExtFlonumNegZero -ExtFlonumZero
                                  -NonNegExtFlonum -NonPosExtFlonum -ExtFlonum)))))
  (define extflfloor-type
    (lambda ()
      (from-cases (map unop (list -ExtFlonumPosZero -ExtFlonumNegZero -ExtFlonumZero
                                  -NonNegExtFlonum -NegExtFlonum -NonPosExtFlonum -ExtFlonum)))))
  (define extflceiling-type
    (lambda ()
      (from-cases (map unop (list -ExtFlonumPosZero -ExtFlonumNegZero -ExtFlonumZero
                                  -PosExtFlonum -NonNegExtFlonum -NonPosExtFlonum -ExtFlonum)))))
  (define extfllog-type
    (lambda ()
      (from-cases (-> -ExtFlonumZero -NegExtFlonum) ; -inf
                  (unop -ExtFlonum))))
  (define extflexp-type
    (lambda ()
      (from-cases (-NonNegExtFlonum . -> . -PosExtFlonum)
                  (-NegExtFlonum . -> . -NonNegExtFlonum)
                  (-ExtFlonum . -> . -ExtFlonum)))) ; nan is the only non nonnegative case (returns nan)
  (define extflsqrt-type
    (lambda ()
      (from-cases (map unop (list -ExtFlonumPosZero -ExtFlonumNegZero -ExtFlonumZero
                                  -NonNegExtFlonum ; we don't have positive case, possible underflow
                                  -ExtFlonum))))) ; anything negative returns nan
  (define extflexpt-type
    (lambda ()
      (from-cases (-ExtFlonumZero -PosExtFlonum . -> . -ExtFlonumZero) ; (extflexpt -0.0t0 0.1t0) -> 0.0t0 ; not sign preserving
                  ((Un -PosExtFlonum -NegExtFlonum) -ExtFlonumZero . -> . -PosExtFlonum) ; always returns 1.0t0
                  (-NonNegExtFlonum -ExtFlonum . -> . -NonNegExtFlonum) ; can underflow
                  (-ExtFlonum -ExtFlonum . -> . -ExtFlonum))))

  (define fx->extfl-type
    (lambda ()
      (fx-from-cases
       (-PosInt . -> . -PosExtFlonum)
       (-Nat . -> . -NonNegExtFlonum)
       (-NegInt . -> . -NegExtFlonum)
       (-NonPosInt . -> . -NonPosExtFlonum)
       (-Int . -> . -ExtFlonum))))
  (define extfl->fx-type
    (lambda ()
      (from-cases
       (-ExtFlonumZero . -> . -Zero)
       (-PosExtFlonum . -> . -PosFixnum)
       (-NegExtFlonum . -> . -NegFixnum)
       (-NonNegExtFlonum . -> . -NonNegFixnum)
       (-NonPosExtFlonum . -> . -NonPosFixnum)
       (-ExtFlonum . -> . -Fixnum))))

  ;Check to ensure we fail fast if the flonum bindings change
  (define-namespace-anchor anchor)
  (let ((flonum-ops #'([unsafe-flround    flround]
                       [unsafe-flfloor    flfloor]
                       [unsafe-flceiling  flceiling]
                       [unsafe-fltruncate fltruncate]
                       [unsafe-flsin      flsin]
                       [unsafe-flcos      flcos]
                       [unsafe-fltan      fltan]
                       [unsafe-flatan     flatan]
                       [unsafe-flasin     flasin ]
                       [unsafe-flacos     flacos]
                       [unsafe-fllog      fllog]
                       [unsafe-flexp      flexp]
                       [unsafe-flexpt     flexpt]
                       [unsafe-extflround    extflround]
                       [unsafe-extflfloor    extflfloor]
                       [unsafe-extflceiling  extflceiling]
                       [unsafe-extfltruncate extfltruncate]
                       [unsafe-extflsin      extflsin]
                       [unsafe-extflcos      extflcos]
                       [unsafe-extfltan      extfltan]
                       [unsafe-extflatan     extflatan]
                       [unsafe-extflasin     extflasin]
                       [unsafe-extflacos     extflacos]
                       [unsafe-extfllog      extfllog]
                       [unsafe-extflexp      extflexp]
                       [unsafe-extflexpt     extflexpt])))
    (define phase (namespace-base-phase (namespace-anchor->namespace anchor)))

    (for ([op-pair (in-syntax flonum-ops)])
      (match op-pair
       [(app syntax->list (list id1 id2))
        (unless (free-identifier=? id1 id2 (sub1 phase))
          (error 'flonum-operations "The assumption that the safe and unsafe flonum-ops are the same binding has been violated. ~a and ~a are diffferent bindings." id1 id2))])))

  )

;; numeric predicates
;; There are 25 values that answer true to zero?. They are either reals, or inexact complexes.
[zero?
  (cl->*
    (-> -ExactNumber B : (-FS (-filter (-val 0) 0) (-not-filter (-val 0) 0)))
    (-> -Real B : (-FS (-filter -RealZero 0) (-not-filter -RealZero 0)))
    (-> N B : (-FS (-filter (Un -RealZero -InexactComplex -InexactImaginary) 0)
                   (-not-filter -RealZero 0))))]

[number? (make-pred-ty N)]
[integer? (asym-pred Univ B (-FS (-filter (Un -Int -Flonum -SingleFlonum) 0) ; inexact-integers exist...
                                 (-not-filter -Int 0)))]
[exact-integer? (make-pred-ty -Int)]
[real? (make-pred-ty -Real)]
[flonum? (make-pred-ty -Flonum)]
[single-flonum? (make-pred-ty -SingleFlonum)]
[double-flonum? (make-pred-ty -Flonum)]
[inexact-real? (make-pred-ty -InexactReal)]
[complex? (make-pred-ty N)]
;; `rational?' includes all Reals, except infinities and NaN.
[rational? (asym-pred Univ B (-FS (-filter -Real 0) (-not-filter -Rat 0)))]
[exact? (make-pred-ty -ExactNumber)]
[inexact? (make-pred-ty (Un -InexactReal -InexactImaginary -InexactComplex))]
[fixnum? (make-pred-ty -Fixnum)]
[index? (make-pred-ty -Index)]
[positive? (cl->* (-> -Byte B : (-FS (-filter -PosByte 0) (-filter -Zero 0)))
                  (-> -Index B : (-FS (-filter -PosIndex 0) (-filter -Zero 0)))
                  (-> -Fixnum B : (-FS (-filter -PosFixnum 0) (-filter -NonPosFixnum 0)))
                  (-> -Int B : (-FS (-filter -PosInt 0) (-filter -NonPosInt 0)))
                  (-> -Rat B : (-FS (-filter -PosRat 0) (-filter -NonPosRat 0)))
                  (-> -Flonum B : (-FS (-filter -PosFlonum 0) (-filter -NonPosFlonum 0)))
                  (-> -SingleFlonum B : (-FS (-filter -PosSingleFlonum 0) (-filter -NonPosSingleFlonum 0)))
                  (-> -InexactReal B : (-FS (-filter -PosInexactReal 0) (-filter -NonPosInexactReal 0)))
                  (-> -Real B : (-FS (-filter -PosReal 0) (-filter -NonPosReal 0))))]
[negative? (cl->* (-> -Fixnum B : (-FS (-filter -NegFixnum 0) (-filter -NonNegFixnum 0)))
                  (-> -Int B : (-FS (-filter -NegInt 0) (-filter -Nat 0)))
                  (-> -Rat B : (-FS (-filter -NegRat 0) (-filter -NonNegRat 0)))
                  (-> -Flonum B : (-FS (-filter -NegFlonum 0) (-filter -NonNegFlonum 0)))
                  (-> -SingleFlonum B : (-FS (-filter -NegSingleFlonum 0) (-filter -NonNegSingleFlonum 0)))
                  (-> -InexactReal B : (-FS (-filter -NegInexactReal 0) (-filter -NonNegInexactReal 0)))
                  (-> -Real B : (-FS (-filter -NegReal 0) (-filter -NonNegReal 0))))]
[exact-positive-integer? (make-pred-ty -Pos)]
[exact-nonnegative-integer? (make-pred-ty -Nat)]

[odd? (-> -Int B : (-FS (-not-filter -Zero 0) (-not-filter -One 0)))]
[even? (-> -Int B : (-FS (-not-filter -One 0) (-not-filter -Zero 0)))]

[=
 (from-cases
  (map (lambda (l) (apply exclude-zero l))
       (list (list -Byte -PosByte)
             (list -Index -PosIndex)
             (list -NonNegFixnum -PosFixnum)
             (list -NonPosFixnum -NegFixnum)
             (list -Nat -PosInt)
             (list -NonPosInt -NegInt)
             (list -Int (Un -PosInt -NegInt))
             (list -NonNegRat -PosRat)
             (list -NonPosRat -NegRat)
             (list -Rat (Un -PosRat -NegRat))
             (list -NonNegFlonum -PosFlonum -FlonumZero)
             (list -NonPosFlonum -NegFlonum -FlonumZero)
             (list -Flonum (Un -PosFlonum -NegFlonum) -FlonumZero)
             (list -NonNegSingleFlonum -PosSingleFlonum -SingleFlonumZero)
             (list -NonPosSingleFlonum -NegSingleFlonum -SingleFlonumZero)
             (list -SingleFlonum (Un -PosSingleFlonum -NegSingleFlonum) -SingleFlonumZero)
             (list -NonNegInexactReal -PosInexactReal -InexactRealZero)
             (list -NonPosInexactReal -NegInexactReal -InexactRealZero)
             (list -InexactReal (Un -PosInexactReal -NegInexactReal) -InexactRealZero)
             (list -NonNegReal -PosReal -RealZero)
             (list -NonPosReal -NegReal -RealZero)
             (list -Real (Un -PosReal -NegReal) -RealZero)))
  (map (lambda (t) (commutative-equality/filter -ExactNumber t))
       (list -One -PosByte -Byte -PosIndex -Index
             -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum
             -PosInt -Nat -NegInt -NonPosInt -Int
             -PosRat -NonNegRat -NegRat -NonPosRat -Rat
             -ExactNumber))
  (map (lambda (t) (commutative-equality/filter -Flonum t))
       (list -FlonumZero -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum))
  (map (lambda (t) (commutative-equality/filter -SingleFlonum t))
       (list -SingleFlonumZero -PosSingleFlonum -NonNegSingleFlonum
             -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum))
  (map (lambda (t) (commutative-equality/filter -InexactReal t))
       (list -InexactRealZero -PosInexactReal -NonNegInexactReal
             -NegInexactReal -NonPosInexactReal -InexactReal))
  ;; this case should take care of mixed type equality. the filters give
  ;; sign information, and we get exactness information from the original
  ;; types
  (map (lambda (t) (commutative-equality/filter -Real t))
       (list -RealZero -PosReal -NonNegReal -NegReal -NonPosReal -Real))
  (->* (list N N) N B))]

[<  (from-cases
     (-> -Pos -One B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Nat -One B : (-FS (-filter -Zero 0) -top))
     (-> -Byte -RealZero B : (-FS (-filter (Un) 0) -top))
     (-> -Byte -One B : (-FS (-filter -Zero 0) -top))
     (-> -Zero -Byte B : (-FS (-filter -PosByte 1) (-filter -Zero 1))) ; can't be -RealZero, which includes NaN
     (-> -Zero -Index B : (-FS (-filter -PosIndex 1) (-filter -Zero 1)))
     (-> -Zero -NonNegFixnum B : (-FS (-filter -PosFixnum 1) (-filter -Zero 1)))
     (-> -Zero -Nat B : (-FS (-filter -PosInt 1) (-filter -Zero 1)))
     (-> -Byte -PosByte B : (-FS -top (-filter -PosByte 0)))
     (-> -Byte -Byte B : (-FS (-filter -PosByte 1) -top))
     (-> -PosInt -Byte B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
     (-> -PosReal -Byte B : (-FS (-filter -PosByte 1) -top)) ; -PosReal is ok here, no filter for #f
     (-> -Byte -PosInt B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
     (-> -Byte -PosRat B : (-FS -top (-filter -PosByte 0))) ; can't be -PosReal, which includes NaN
     (-> -Nat -Byte B : (-FS (-and (-filter -Byte 0) (-filter -PosByte 1)) -top))
     (-> -NonNegReal -Byte B : (-FS (-filter -PosByte 1) -top))
     (-> -Byte -Nat B : (-FS -top (-filter -Byte 1)))
     (-> -Index -RealZero B : (-FS (-filter (Un) 0) -top))
     (-> -Index -One B : (-FS (-filter -Zero 0) -top))
     (-> -Zero -Index B : (-FS (-filter -PosIndex 1) (-filter -Zero 1))) ; can't be -RealZero, which includes NaN
     (-> -Index -PosIndex B : (-FS -top (-filter -PosIndex 0)))
     (-> -Index -Index B : (-FS (-filter -PosIndex 1) -top))
     (-> -PosInt -Index B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
     (-> -PosReal -Index B : (-FS (-filter -PosIndex 1) -top))
     (-> -Index -PosInt B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
     (-> -Index -PosRat B : (-FS -top (-filter -PosIndex 0))) ; can't be -PosReal, which includes NaN
     (-> -Nat -Index B : (-FS (-and (-filter -Index 0) (-filter -PosIndex 1)) -top))
     (-> -NonNegReal -Index B : (-FS (-filter -PosIndex 1) -top))
     (-> -Index -Nat B : (-FS -top (-filter -Index 1)))
     (-> -Fixnum -Zero B : (-FS (-filter -NegFixnum 0) (-filter -NonNegFixnum 0))) ; can't be -RealZero
     (-> -Zero -Fixnum B : (-FS (-filter -PosFixnum 1) (-filter -NonPosFixnum 1)))
     (-> -Fixnum -PosInt B : (-FS -top (-and (-filter -PosFixnum 0) (-filter -PosFixnum 1))))
     (-> -Fixnum -PosRat B : (-FS -top (-filter -PosFixnum 0)))
     (-> -Fixnum -Nat B : (-FS -top (-and (-filter -NonNegFixnum 0) (-filter -NonNegFixnum 1))))
     (-> -Fixnum -NonNegRat B : (-FS -top (-filter -NonNegFixnum 0)))
     (-> -Nat -Fixnum B : (-FS (-and (-filter -PosFixnum 1) (-filter -NonNegFixnum 0)) -top))
     (-> -NonNegReal -Fixnum B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Fixnum -NonPosInt B : (-FS (-and (-filter -NegFixnum 0) (-filter -NonPosFixnum 1)) -top))
     (-> -Fixnum -NonPosReal B : (-FS (-filter -NegFixnum 0) -top))
     (-> -NegInt -Fixnum B : (-FS -top (-and (-filter -NegFixnum 0) (-filter -NegFixnum 1))))
     (-> -NegRat -Fixnum B : (-FS -top (-filter -NegFixnum 1)))
     (-> -NonPosInt -Fixnum B : (-FS -top (-and (-filter -NonPosFixnum 0) (-filter -NonPosFixnum 1))))
     (-> -NonPosRat -Fixnum B : (-FS -top (-filter -NonPosFixnum 1)))
     (-> -Rat (-val +inf.0) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> (-val +inf.0) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.0) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.0) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> -Rat (-val +inf.f) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> (-val +inf.f) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.f) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.f) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     ;; If applying filters resulted in the interesection of the filter and the
     ;; original type, we'd only need the cases for Fixnums and those for Reals.
     ;; Cases for Integers and co would fall out naturally from the Real cases,
     ;; since we'd keep track of the representation knowledge we'd already have,
     ;; and the Real cases are enough to give us sign information.
     ;; In the meantime, repetition is hard to avoid.
     (<-type-pattern -Int -PosInt -Nat -NegInt -NonPosInt -Zero)
     (<-type-pattern -Rat -PosRat -NonNegRat -NegRat -NonPosRat -Zero)
     (<-type-pattern -Flonum -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum)
     (<-type-pattern -SingleFlonum -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum)
     (<-type-pattern -InexactReal -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal)
     (<-type-pattern -Real -PosReal -NonNegReal -NegReal -NonPosReal)
     (->* (list R R) R B))]
[>  (from-cases
     (-> -One -Pos B : (-FS (-filter (Un) 1) -top)) ; can't happen
     (-> -One -Nat B : (-FS (-filter -Zero 1) -top))
     (-> -RealZero -Byte B : (-FS (-filter (Un) 1) -top))
     (-> -One -Byte B : (-FS (-filter -Zero 1) -top))
     (-> -Byte -Zero B : (-FS (-filter -PosByte 0) (-filter -Zero 0)))
     (-> -Index -Zero B : (-FS (-filter -PosIndex 0) (-filter -Zero 0)))
     (-> -NonNegFixnum -Zero B : (-FS (-filter -PosFixnum 0) (-filter -Zero 0)))
     (-> -Nat -Zero B : (-FS (-filter -PosInt 0) (-filter -Zero 0)))
     (-> -PosByte -Byte B : (-FS -top (-filter -PosByte 1)))
     (-> -Byte -Byte B : (-FS (-filter -PosByte 0) -top))
     (-> -Byte -PosInt B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
     (-> -Byte -PosReal B : (-FS (-filter -PosByte 0) -top))
     (-> -PosInt -Byte B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
     (-> -PosRat -Byte B : (-FS -top (-filter -PosByte 1)))
     (-> -Byte -Nat B : (-FS (-and (-filter -PosByte 0) (-filter -Byte 1)) -top))
     (-> -Byte -NonNegReal B : (-FS (-filter -PosByte 0) -top))
     (-> -Nat -Byte B : (-FS -top (-filter -Byte 0)))
     (-> -RealZero -Index B : (-FS (-filter (Un) 1) -top))
     (-> -One -Index B : (-FS (-filter -Zero 1) -top))
     (-> -Index -Zero B : (-FS (-filter -PosIndex 0) (-filter -Zero 0)))
     (-> -PosIndex -Index B : (-FS -top (-filter -PosIndex 1)))
     (-> -Index -Index B : (-FS (-filter -PosIndex 0) -top))
     (-> -Index -PosInt B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
     (-> -Index -PosReal B : (-FS (-filter -PosIndex 0) -top))
     (-> -PosInt -Index B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
     (-> -PosRat -Index B : (-FS -top (-filter -PosIndex 1)))
     (-> -Index -Nat B : (-FS (-and (-filter -PosIndex 0) (-filter -Index 1)) -top))
     (-> -Index -NonNegReal B : (-FS (-filter -PosIndex 0) -top))
     (-> -Nat -Index B : (-FS -top (-filter -Index 0)))
     (-> -Zero -Fixnum B : (-FS (-filter -NegFixnum 1) (-filter -NonNegFixnum 1)))
     (-> -Fixnum -RealZero B : (-FS (-filter -PosFixnum 0) (-filter -NonPosFixnum 0)))
     (-> -PosInt -Fixnum B : (-FS -top (-and (-filter -PosFixnum 0) (-filter -PosFixnum 1))))
     (-> -PosRat -Fixnum B : (-FS -top (-filter -PosFixnum 1)))
     (-> -Nat -Fixnum B : (-FS -top (-and (-filter -NonNegFixnum 0) (-filter -NonNegFixnum 1))))
     (-> -NonNegRat -Fixnum B : (-FS -top (-filter -NonNegFixnum 1)))
     (-> -Fixnum -Nat B : (-FS (-and (-filter -PosFixnum 0) (-filter -NonNegFixnum 1)) -top))
     (-> -Fixnum -NonNegReal B : (-FS (-filter -PosFixnum 0) -top))
     (-> -NonPosInt -Fixnum B : (-FS (-and (-filter -NonPosFixnum 0) (-filter -NegFixnum 1)) -top))
     (-> -NonPosReal -Fixnum B : (-FS (-filter -NegFixnum 1) -top))
     (-> -Fixnum -NegInt B : (-FS -top (-and (-filter -NegFixnum 0) (-filter -NegFixnum 1))))
     (-> -Fixnum -NegRat B : (-FS -top (-filter -NegFixnum 0)))
     (-> -Fixnum -NonPosInt B : (-FS -top (-and (-filter -NonPosFixnum 0) (-filter -NonPosFixnum 1))))
     (-> -Fixnum -NonPosRat B : (-FS -top (-filter -NonPosFixnum 0)))
     (-> (-val +inf.0) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> -Rat (-val +inf.0) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.0) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.0) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> (-val +inf.f) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> -Rat (-val +inf.f) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.f) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.f) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (>-type-pattern -Int -PosInt -Nat -NegInt -NonPosInt -Zero)
     (>-type-pattern -Rat -PosRat -NonNegRat -NegRat -NonPosRat -Zero)
     (>-type-pattern -Flonum -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum)
     (>-type-pattern -SingleFlonum -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum)
     (>-type-pattern -InexactReal -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal)
     (>-type-pattern -Real -PosReal -NonNegReal -NegReal -NonPosReal)
     (->* (list R R) R B))]
[<= (from-cases
     (-> -Pos -One B : (-FS (-filter -One 0) -top))
     (-> -Byte -Zero B : (-FS (-filter -Zero 0) (-filter -PosByte 0)))
     (-> -Index -Zero B : (-FS (-filter -Zero 0) (-filter -PosIndex 0)))
     (-> -NonNegFixnum -Zero B : (-FS (-filter -Zero 0) (-filter -PosFixnum 0)))
     (-> -Nat -Zero B : (-FS (-filter -Zero 0) (-filter -PosInt 0)))
     (-> -Zero -Nat B : (-FS -top (-filter (Un) 1)))
     (-> -One -Byte B : (-FS (-filter -PosByte 1) (-filter -Zero 1)))
     (-> -PosByte -Byte B : (-FS (-filter -PosByte 1) -top))
     (-> -Byte -Byte B : (-FS -top (-filter -PosByte 0)))
     (-> -PosInt -Byte B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
     (-> -PosReal -Byte B : (-FS (-filter -PosByte 1) -top))
     (-> -Byte -PosInt B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
     (-> -Byte -PosRat B : (-FS -top (-filter -PosByte 0)))
     (-> -Nat -Byte B : (-FS (-filter -Byte 0) -top))
     (-> -Byte -Nat B : (-FS -top (-and (-filter -PosByte 0) (-filter -Byte 1))))
     (-> -Byte -NonNegRat B : (-FS -top (-filter -PosByte 0)))
     (-> -Index -Zero B : (-FS (-filter -Zero 0) (-filter -PosIndex 0)))
     (-> -Zero -Index B : (-FS -top (-filter (Un) 1)))
     (-> -One -Index B : (-FS (-filter -PosIndex 1) (-filter -Zero 1)))
     (-> -PosIndex -Index B : (-FS (-filter -PosIndex 1) -top))
     (-> -Index -Index B : (-FS -top (-filter -PosIndex 0)))
     (-> -Pos -Index B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
     (-> -PosReal -Index B : (-FS (-filter -PosIndex 1) -top))
     (-> -Index -Pos B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
     (-> -Index -PosRat B : (-FS -top (-filter -PosIndex 0)))
     (-> -Nat -Index B : (-FS (-filter -Index 0) -top))
     (-> -Index -Nat B : (-FS -top (-and (-filter -PosIndex 0) (-filter -Index 1))))
     (-> -Index -NonNegRat B : (-FS -top (-filter -PosIndex 0)))
     (-> -NonNegFixnum -Zero B : (-FS (-filter -Zero 0) (-filter -PosFixnum 0)))
     (-> -Zero -NonNegFixnum B : (-FS (-filter -Zero 1) (-filter (Un) 1)))
     (-> -One -NonNegFixnum B : (-FS -top (-filter -Zero 1)))
     (-> -Zero -Fixnum B : (-FS (-filter -NonNegFixnum 1) (-filter -NegFixnum 1)))
     (-> -Fixnum -Zero B : (-FS (-filter -NonPosFixnum 0) (-filter -PosFixnum 0)))
     (-> -PosInt -Fixnum B : (-FS (-and (-filter -PosFixnum 0) (-filter -PosFixnum 1)) -top))
     (-> -PosReal -Fixnum B : (-FS (-filter -PosFixnum 1) -top))
     (-> -Nat -Fixnum B : (-FS (-and (-filter -NonNegFixnum 0) (-filter -NonNegFixnum 1)) -top))
     (-> -NonNegReal -Fixnum B : (-FS (-filter -NonNegFixnum 1) -top))
     (-> -Fixnum -Nat B : (-FS -top (-and (-filter -PosFixnum 0) (-filter -NonNegFixnum 1))))
     (-> -Fixnum -NonNegRat B : (-FS -top (-filter -PosFixnum 0)))
     (-> -NonPosInt -Fixnum B : (-FS -top (-and (-filter -NonPosFixnum 0) (-filter -NegFixnum 1))))
     (-> -NonPosRat -Fixnum B : (-FS -top (-filter -NegFixnum 1)))
     (-> -Fixnum -NegInt B : (-FS (-and (-filter -NegFixnum 0) (-filter -NegFixnum 1)) -top))
     (-> -Fixnum -NegReal B : (-FS (-filter -NegFixnum 0) -top))
     (-> -Fixnum -NonPosInt B : (-FS (-and (-filter -NonPosFixnum 0) (-filter -NonPosFixnum 1)) -top))
     (-> -Fixnum -NonPosReal B : (-FS (-filter -NonPosFixnum 0) -top))
     (-> -Rat (-val +inf.0) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> (-val +inf.0) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.0) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.0) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> -Rat (-val +inf.f) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> (-val +inf.f) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.f) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.f) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (<=-type-pattern -Int -PosInt -Nat -NegInt -NonPosInt -Zero)
     (<=-type-pattern -Rat -PosRat -NonNegRat -NegRat -NonPosRat -Zero)
     (<=-type-pattern -Flonum -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum)
     (<=-type-pattern -SingleFlonum -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum)
     (<=-type-pattern -InexactReal -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal)
     (<=-type-pattern -Real -PosReal -NonNegReal -NegReal -NonPosReal)
     (->* (list R R) R B))]
[>= (from-cases
     (-> -One -Pos B : (-FS (-filter -One 1) -top))
     (-> -Zero -Byte B : (-FS (-filter -Zero 1) (-filter -PosByte 1)))
     (-> -Zero -Index B : (-FS (-filter -Zero 1) (-filter -PosIndex 1)))
     (-> -Zero -NonNegFixnum B : (-FS (-filter -Zero 1) (-filter -PosFixnum 1)))
     (-> -Zero -Nat B : (-FS (-filter -Zero 1) (-filter -PosInt 1)))
     (-> -Nat -Zero B : (-FS -top (-filter (Un) 0)))
     (-> -Byte -One B : (-FS (-filter -PosByte 0) (-filter -Zero 0)))
     (-> -Byte -PosByte B : (-FS (-filter -PosByte 0) -top))
     (-> -Byte -Byte B : (-FS -top (-filter -PosByte 1)))
     (-> -Byte -PosInt B : (-FS (-and (-filter -PosByte 0) (-filter -PosByte 1)) -top))
     (-> -Byte -PosReal B : (-FS (-filter -PosByte 0) -top))
     (-> -PosInt -Byte B : (-FS -top (-and (-filter -PosByte 0) (-filter -PosByte 1))))
     (-> -PosRat -Byte B : (-FS -top (-filter -PosByte 1)))
     (-> -Byte -Nat B : (-FS (-filter -Byte 1) -top))
     (-> -Nat -Byte B : (-FS -top (-and (-filter -Byte 0) (-filter -PosByte 1))))
     (-> -NonNegRat -Byte B : (-FS -top (-filter -PosByte 1)))
     (-> -Zero -Index B : (-FS (-filter -Zero 1) (-filter -PosIndex 1)))
     (-> -Index -Zero B : (-FS -top (-filter (Un) 0)))
     (-> -Index -One B : (-FS (-filter -PosIndex 0) (-filter -Zero 0)))
     (-> -Index -PosIndex B : (-FS (-filter -PosIndex 0) -top))
     (-> -Index -Index B : (-FS -top (-filter -PosIndex 1)))
     (-> -Index -Pos B : (-FS (-and (-filter -PosIndex 0) (-filter -PosIndex 1)) -top))
     (-> -Index -PosReal B : (-FS (-filter -PosIndex 0) -top))
     (-> -Pos -Index B : (-FS -top (-and (-filter -PosIndex 0) (-filter -PosIndex 1))))
     (-> -PosRat -Index B : (-FS -top (-filter -PosIndex 1)))
     (-> -Index -Nat B : (-FS (-filter -Index 1) -top))
     (-> -Nat -Index B : (-FS -top (-and (-filter -Index 0) (-filter -PosIndex 1))))
     (-> -NonNegRat -Index B : (-FS -top (-filter -PosIndex 1)))
     (-> -Zero -NonNegFixnum B : (-FS (-filter -Zero 1) (-filter -PosFixnum 1)))
     (-> -NonNegFixnum -Zero B : (-FS (-filter -Zero 0) (-filter (Un) 0)))
     (-> -NonNegFixnum -One B : (-FS -top (-filter -Zero 0)))
     (-> -Fixnum -Zero B : (-FS (-filter -NonNegFixnum 0) (-filter -NegFixnum 0)))
     (-> -Zero -Fixnum B : (-FS (-filter -NonPosFixnum 1) (-filter -PosFixnum 1)))
     (-> -Fixnum -PosInt B : (-FS (-and (-filter -PosFixnum 0) (-filter -PosFixnum 1)) -top))
     (-> -Fixnum -PosReal B : (-FS (-filter -PosFixnum 0) -top))
     (-> -Fixnum -Nat B : (-FS (-and (-filter -NonNegFixnum 0) (-filter -NonNegFixnum 1)) -top))
     (-> -Fixnum -NonNegReal B : (-FS (-filter -NonNegFixnum 0) -top))
     (-> -Nat -Fixnum B : (-FS -top (-and (-filter -NonNegFixnum 0) (-filter -PosFixnum 1))))
     (-> -NonNegRat -Fixnum B : (-FS -top (-filter -PosFixnum 1)))
     (-> -Fixnum -NonPosInt B : (-FS -top (-and (-filter -NegFixnum 0) (-filter -NonPosFixnum 1))))
     (-> -Fixnum -NonPosRat B : (-FS -top (-filter -NegFixnum 0)))
     (-> -NegInt -Fixnum B : (-FS (-and (-filter -NegFixnum 0) (-filter -NegFixnum 1)) -top))
     (-> -NegReal -Fixnum B : (-FS (-filter -NegFixnum 1) -top))
     (-> -NonPosInt -Fixnum B : (-FS (-and (-filter -NonPosFixnum 0) (-filter -NonPosFixnum 1)) -top))
     (-> -NonPosReal -Fixnum B : (-FS (-filter -NonPosFixnum 1) -top))
     (-> (-val +inf.0) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> -Rat (-val +inf.0) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.0) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.0) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> (-val +inf.f) -Rat B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (-> -Rat (-val +inf.f) B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> (-val -inf.f) -Rat B : (-FS (-filter (Un) 0) -top)) ; can't happen
     (-> -Rat (-val -inf.f) B : (-FS -top (-filter (Un) 0))) ; guaranteed
     (>=-type-pattern -Int -PosInt -Nat -NegInt -NonPosInt -Zero)
     (>=-type-pattern -Rat -PosRat -NonNegRat -NegRat -NonPosRat -Zero)
     (>=-type-pattern -Flonum -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum)
     (>=-type-pattern -SingleFlonum -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum)
     (>=-type-pattern -InexactReal -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal)
     (>=-type-pattern -Real -PosReal -NonNegReal -NegReal -NonPosReal)
     (->* (list R R) R B))]

[* (from-cases
    (-> -One)
    (commutative-case -Zero N -Zero)
    (map (lambda (t) (commutative-binop -One t))
         all-number-types)
    (-> -PosByte -PosByte -PosIndex)
    (-> -Byte -Byte -Index)
    (-> -PosByte -PosByte -PosByte -PosFixnum)
    (-> -Byte -Byte -Byte -NonNegFixnum)
    (map unop all-int-types)
    (varop -PosInt)
    (varop -Nat)
    (-> -NegInt -NegInt)
    (-> -NonPosInt -NonPosInt)
    (-> -NegInt -NegInt -PosInt)
    (commutative-binop -NegInt -PosInt -NegInt)
    (-> -NonPosInt -NonPosInt -Nat)
    (commutative-binop -NonPosInt -Nat -NonPosInt)
    (-> -NegInt -NegInt -NegInt -NegInt)
    (-> -NonPosInt -NonPosInt -NonPosInt -NonPosInt)
    (map varop (list -Int -PosRat -NonNegRat))
    (-> -NegRat -NegRat)
    (-> -NonPosRat -NonPosRat)
    (-> -NegRat -NegRat -PosRat)
    (commutative-binop -NegRat -PosRat -NegRat)
    (-> -NonPosRat -NonPosRat -NonNegRat)
    (commutative-binop -NonPosRat -NonNegRat -NonPosRat)
    (-> -NegRat -NegRat -NegRat -NegRat)
    (-> -NonPosRat -NonPosRat -NonPosRat -NonPosRat)
    (map unop rat-types)
    (varop -Rat)
    (varop-1+ -FlonumZero)
    ; no pos * -> pos, possible underflow
    (varop-1+ -NonNegFlonum)
    ;; can't do NonPos NonPos -> NonNeg: (* -1.0 0.0) -> NonPos!
    (-> -NegFlonum -NegFlonum -NonNegFlonum) ; possible underflow, so no neg neg -> pos
    (-> -NegFlonum -NegFlonum -NegFlonum -NonPosFlonum) ; see above
    ;; limited flonum contagion rules
    ;; (* <float> 0) is exact 0 (i.e. not a float)
    (commutative-case -NonNegFlonum -PosReal) ; real args don't include 0
    (commutative-case -Flonum (Un -PosReal -NegReal) -Flonum)
    (map unop all-flonum-types)
    (map varop-1+ (list -Flonum -SingleFlonumZero -NonNegSingleFlonum))
    ;; we could add contagion rules for negatives, but we haven't for now
    (-> -NegSingleFlonum -NegSingleFlonum -NonNegSingleFlonum) ; possible underflow, so no neg neg -> pos
    (-> -NegSingleFlonum -NegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum)
    (commutative-case -NonNegSingleFlonum (Un -PosRat -NonNegSingleFlonum))
    (commutative-case -SingleFlonum (Un -PosRat -NegRat -SingleFlonum) -SingleFlonum)
    (map unop single-flonum-types)
    (map varop-1+ (list -SingleFlonum -InexactRealZero -NonNegInexactReal))
    (-> -NegInexactReal -NegInexactReal -NonNegInexactReal)
    (-> -NegInexactReal -NegInexactReal -NegInexactReal -NonPosInexactReal)
    (commutative-case -NonNegInexactReal (Un -PosRat -NonNegInexactReal))
    (commutative-case -InexactReal (Un -PosRat -NegRat -InexactReal) -InexactReal)
    (map unop inexact-real-types)
    (varop-1+ -InexactReal)
    ;; reals
    (map unop real-types)
    (varop -NonNegReal) ; (* +inf.0 0.0) -> +nan.0
    (-> -NegReal -NegReal -NonNegReal)
    (commutative-binop -NegReal -PosReal -NonPosReal)
    (-> -NegReal -NegReal -NegReal -NonPosReal)
    (varop -Real)
    ;; complexes
    (commutative-case -FloatComplex (Un -InexactComplex -InexactReal -PosRat -NegRat) -FloatComplex)
    (commutative-case -SingleFlonumComplex (Un -SingleFlonumComplex -SingleFlonum -PosRat -NegRat) -SingleFlonumComplex)
    (commutative-case -InexactComplex (Un -InexactComplex -InexactReal -PosRat -NegRat) -InexactComplex)
    (map unop number-types)
    (varop N))]
[+ (from-cases
    (-> -Zero)
    (binop -Zero)
    (map (lambda (t) (commutative-binop t -Zero t))
         (list -One -PosByte -Byte -PosIndex -Index
               -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum))
    (-> -PosByte -PosByte -PosIndex)
    (-> -Byte -Byte -Index)
    (-> -PosByte -PosByte -PosByte -PosIndex)
    (-> -Byte -Byte -Byte -Index)
    (commutative-binop -PosIndex -Index -PosFixnum)
    (-> -PosIndex -Index -Index -PosFixnum)
    (-> -Index -PosIndex -Index -PosFixnum)
    (-> -Index -Index -PosIndex -PosFixnum)
    (-> -Index -Index -NonNegFixnum)
    (-> -Index -Index -Index -NonNegFixnum)
    (commutative-binop -NegFixnum -One -NonPosFixnum)
    (commutative-binop -NonPosFixnum -NonNegFixnum -Fixnum)
    (commutative-case -PosInt -Nat -PosInt)
    (commutative-case -NegInt -NonPosInt -NegInt)
    (map unop all-int-types)
    (map varop (list -Nat -NonPosInt -Int))
    (commutative-case -PosRat -NonNegRat -PosRat)
    (commutative-case -NegRat -NonPosRat -NegRat)
    (map unop rat-types)
    (map varop (list -NonNegRat -NonPosRat -Rat))
    ;; flonum + real -> flonum
    (commutative-case -PosFlonum -NonNegReal -PosFlonum)
    (commutative-case -PosReal -NonNegFlonum -PosFlonum)
    (commutative-case -NegFlonum -NonPosReal -NegFlonum)
    (commutative-case -NegReal -NonPosFlonum -NegFlonum)
    (commutative-case -NonNegFlonum -NonNegReal -NonNegFlonum)
    (commutative-case -NonPosFlonum -NonPosReal -NonPosFlonum)
    (commutative-case -Flonum -Real -Flonum)
    (map unop all-flonum-types)
    (varop-1+ -Flonum)
    ;; single-flonum + rat -> single-flonum
    (commutative-case -PosSingleFlonum (Un -NonNegRat -NonNegSingleFlonum) -PosSingleFlonum)
    (commutative-case (Un -PosRat -PosSingleFlonum) -NonNegSingleFlonum -PosSingleFlonum)
    (commutative-case -NegSingleFlonum (Un -NonPosRat -NonPosSingleFlonum) -NegSingleFlonum)
    (commutative-case (Un -NegRat -NegSingleFlonum) -NonPosSingleFlonum -NegSingleFlonum)
    (commutative-case -NonNegSingleFlonum (Un -NonNegRat -NonNegSingleFlonum) -NonNegSingleFlonum)
    (commutative-case -NonPosSingleFlonum (Un -NonPosRat -NonPosSingleFlonum) -NonPosSingleFlonum)
    (commutative-case -SingleFlonum (Un -Rat -SingleFlonum) -SingleFlonum)
    (map unop single-flonum-types)
    (varop-1+ -SingleFlonum)
    ;; inexact-real + real -> inexact-real
    (commutative-case -PosInexactReal -NonNegReal -PosInexactReal)
    (commutative-case -PosReal -NonNegInexactReal -PosInexactReal)
    (commutative-case -NegInexactReal -NonPosReal -NegInexactReal)
    (commutative-case -NegReal -NonPosInexactReal -NegInexactReal)
    (commutative-case -NonNegInexactReal -NonNegReal -NonNegInexactReal)
    (commutative-case -NonPosInexactReal -NonPosReal -NonPosInexactReal)
    (commutative-case -InexactReal -Real -InexactReal)
    (map unop inexact-real-types)
    ;; real
    (commutative-case -PosReal -NonNegReal -PosReal)
    (commutative-case -NegReal -NonPosReal -NegReal)
    (map unop real-types)
    (map varop (list -NonNegReal -NonPosReal -Real -ExactNumber))
    ;; complex
    (commutative-case -FloatComplex N -FloatComplex)
    (commutative-case -Flonum -InexactComplex -FloatComplex)
    (commutative-case -SingleFlonumComplex (Un -Rat -SingleFlonum -SingleFlonumComplex) -SingleFlonumComplex)
    (commutative-case -InexactComplex (Un -Rat -InexactReal -InexactComplex) -InexactComplex)
    (map unop number-types)
    (varop N))]

[- (from-cases
    (binop -Zero)
    (half-negation-pattern -PosFixnum -NegFixnum -NonNegFixnum -NonPosFixnum)
    (negation-pattern -PosInt -NegInt -Nat -NonPosInt)
    (negation-pattern -PosRat -NegRat -NonNegRat -NonPosRat)
    (negation-pattern -PosFlonum -NegFlonum -NonNegFlonum -NonPosFlonum)
    (negation-pattern -PosSingleFlonum -NegSingleFlonum -NonNegSingleFlonum -NonPosSingleFlonum)
    (negation-pattern -PosInexactReal -NegInexactReal -NonNegInexactReal -NonPosInexactReal)
    (negation-pattern -PosReal -NegReal -NonNegReal -NonPosReal)
    (map (lambda (t) (-> t -Zero t))
         (list -One -PosByte -Byte -PosIndex -Index
               -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum))
    (-> -One -One -Zero)
    (-> -PosByte -One -Byte)
    (-> -PosIndex -One -Index)
    (-> -PosFixnum -One -NonNegFixnum)
    (-> -PosInt -One -Nat)
    (-> -NonNegFixnum -NonNegFixnum -Fixnum)
    (-> -NegFixnum -NonPosFixnum -Fixnum)
    (->* (list -PosInt -NonPosInt) -NonPosInt -PosInt)
    (->* (list -Nat -NonPosInt) -NonPosInt -Nat)
    (->* (list -NegInt -Nat) -Nat -NegInt)
    (->* (list -NonPosInt -Nat) -Nat -NonPosInt)
    (varop-1+ -Int)
    (->* (list -PosRat -NonPosRat) -NonPosRat -PosRat)
    (->* (list -NonNegRat -NonPosRat) -NonPosRat -NonNegRat)
    (->* (list -NegRat -NonNegRat) -NonNegRat -NegRat)
    (->* (list -NonPosRat -NonNegRat) -NonNegRat -NonPosRat)
    (varop-1+ -Rat)
    ;; floats - uncertain about sign properties in the presence of
    ;; under/overflow, so these are left out
    (varop-1+ -Flonum)
    (commutative-case -Flonum -Real -Flonum)
    (varop-1+ -SingleFlonum)
    (commutative-case -SingleFlonum (Un -SingleFlonum -Rat) -SingleFlonum)
    (varop-1+ -InexactReal)
    (commutative-case -InexactReal (Un -InexactReal -Rat) -InexactReal)
    (map varop-1+ (list -Real -ExactNumber))
    (varop-1+ -FloatComplex)
    (commutative-case -FloatComplex N -FloatComplex)
    (varop-1+ -SingleFlonumComplex)
    (commutative-case -SingleFlonumComplex (Un -SingleFlonumComplex -ExactNumber) -SingleFlonumComplex)
    (varop-1+ -InexactComplex)
    (commutative-case -InexactComplex (Un -InexactComplex -ExactNumber) -InexactComplex)
    (varop-1+ N))]
[/ (from-cases ; very similar to multiplication, without closure properties for integers
    (commutative-case -Zero N -Zero)
    (unop -One)
    (map (lambda (t) (-> t -One t))
         all-number-types)
    (varop-1+ -PosRat)
    (varop-1+ -NonNegRat)
    (-> -NegRat -NegRat)
    (-> -NonPosRat -NonPosRat)
    (-> -NegRat -NegRat -PosRat)
    (commutative-binop -NegRat -PosRat -NegRat)
    (-> -NonPosRat -NonPosRat -NonNegRat)
    (commutative-binop -NonPosRat -NonNegRat -NonPosRat)
    (-> -NegRat -NegRat -NegRat -NegRat)
    (-> -NonPosRat -NonPosRat -NonPosRat -NonPosRat)
    (varop-1+ -Rat)
    (-> -FlonumZero (Un -PosFlonum -NegFlonum)) ; one of the infinities
    ;; No (-> -NonNegFlonum -NonNegFlonum -NonNegFlonum), (/ 0.1 -0.0) => -inf.0
    ;; No (-> -NonPosFlonum -NonPosFlonum), (/ 0.0) => +inf.0
    (-> -NegFlonum -NegFlonum -NonNegFlonum)
    (-> -NegFlonum -NegFlonum -NegFlonum -NonPosFlonum)
    ;; limited flonum contagion rules
    ;; (/ 0 <float>) is exact 0 (i.e. not a float)
    (-> -PosFlonum -PosReal -NonNegFlonum)
    (-> -PosReal -PosFlonum -NonNegFlonum)
    (commutative-case -PosFlonum -PosReal -NonNegFlonum)
    (->* (list (Un -PosReal -NegReal -Flonum) -Flonum) -Flonum -Flonum)
    (->* (list -Flonum) -Real -Flonum) ; if any argument after the first is exact 0, not a problem
    (varop-1+ -Flonum)
    (-> -SingleFlonumZero (Un -PosSingleFlonum -NegSingleFlonum)) ; one of the infinities
    ;; we could add contagion rules for negatives, but we haven't for now
    (-> -NegSingleFlonum -NegSingleFlonum -NonNegSingleFlonum) ; possible underflow, so no neg neg -> pos
    (-> -NegSingleFlonum -NegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum)
    (commutative-case -PosSingleFlonum (Un -PosRat -PosSingleFlonum) -NonNegSingleFlonum)
    (commutative-case -SingleFlonum (Un -PosRat -NegRat -SingleFlonum) -SingleFlonum)
    (varop-1+ -SingleFlonum)
    (-> -InexactRealZero (Un -PosInexactReal -NegInexactReal))
    (-> -NegInexactReal -NegInexactReal -NonNegInexactReal)
    (-> -NegInexactReal -NegInexactReal -NegInexactReal -NonPosInexactReal)
    (commutative-case -PosInexactReal (Un -PosRat -PosInexactReal) -NonNegInexactReal)
    (commutative-case -InexactReal (Un -PosRat -NegRat -InexactReal) -InexactReal)
    (varop-1+ -InexactReal)
    ;; reals
    (varop-1+ -PosReal -NonNegReal)
    (-> -NegReal -NonPosReal)
    (-> -NegReal -NegReal -NonNegReal)
    (-> -NegReal -PosReal -NonPosReal)
    (-> -PosReal -NegReal -NonPosReal)
    (-> -NegReal -NegReal -NegReal -NonPosReal)
    (varop-1+ -Real)
    ;; complexes
    (varop-1+ -FloatComplex)
    (commutative-case -FloatComplex (Un -InexactComplex -InexactReal -PosRat -NegRat) -FloatComplex)
    (->* (list -FloatComplex) N -FloatComplex) ; if any argument after the first is exact 0, not a problem
    (varop-1+ -SingleFlonumComplex)
    (commutative-case -SingleFlonumComplex (Un -SingleFlonumComplex -SingleFlonum -PosRat -NegRat) -SingleFlonumComplex)
    (varop-1+ -InexactComplex)
    (commutative-case -InexactComplex (Un -InexactComplex -InexactReal -PosRat -NegRat) -InexactComplex)
    (varop-1+ N))]

[max
 (from-cases (map varop (list -Zero -One))
             (commutative-case -One -Zero)
             (commutative-case -PosByte -Byte)
             (commutative-case -PosIndex -Index)
             (commutative-case -PosFixnum -Fixnum)
             (commutative-case -NonNegFixnum -Fixnum)
             (map varop (list -NegFixnum -NonPosFixnum -PosFixnum -NonNegFixnum -Fixnum))
             (commutative-case -PosInt -Int)
             (commutative-case -Nat -Int)
             (map varop (list -NegInt -NonPosInt -PosInt -Nat -Int))
             ;; we could have more cases here. for instance, when mixing PosInt
             ;; and NegRats, we get a result of type PosInt (not just PosRat)
             ;; there's a lot of these, but they may not be worth including
             (commutative-case -PosRat -Rat)
             (commutative-case -NonNegRat -Rat)
             (map varop (list -NegRat -NonPosRat -PosRat -NonNegRat -Rat
                                 -FlonumPosZero -FlonumNegZero -FlonumZero))
             ;; inexactness is contagious: (max 3 2.3) => 3.0
             ;; we could add cases to encode that
             (commutative-case -PosFlonum -Flonum)
             (commutative-case -NonNegFlonum -Flonum)
             (map varop (list -NegFlonum -NonPosFlonum -PosFlonum -NonNegFlonum -Flonum
                              -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero))
             (varop -PosSingleFlonum)
             (commutative-case -PosSingleFlonum -SingleFlonum)
             (varop -NonNegSingleFlonum)
             (commutative-case -NonNegSingleFlonum -SingleFlonum)
             (map varop (list -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                              -InexactRealPosZero -InexactRealNegZero -InexactRealZero))
             (commutative-case -PosInexactReal -InexactReal)
             (commutative-case -NonNegInexactReal -InexactReal)
             (map varop (list -NegInexactReal -NonPosInexactReal -PosInexactReal -NonNegInexactReal
                              -InexactReal -RealZero))
             (commutative-case -PosReal -Real)
             (commutative-case -NonNegReal -Real)
             (map varop (list -NegReal -NonPosReal -PosReal -NonNegReal -Real)))]
[min
 (from-cases (map varop (list -Zero -One))
             (commutative-case -Zero -One)
             (map varop (list -PosByte -Byte -PosIndex -Index -PosFixnum -NonNegFixnum))
             (commutative-case -NegFixnum -Fixnum)
             (commutative-case -NonPosFixnum -Fixnum)
             (map varop (list -NegFixnum -NonPosFixnum -Fixnum -PosInt -Nat))
             (commutative-case -NegInt -Int)
             (commutative-case -NonPosInt -Int)
             (map varop (list -NegInt -NonPosInt -Int -PosRat -NonNegRat))
             (commutative-case -NegRat -Rat)
             (commutative-case -NonPosRat -Rat)
             (map varop (list -NegRat -NonPosRat -Rat
                              -FlonumPosZero -FlonumNegZero -FlonumZero
                              -PosFlonum -NonNegFlonum))
             (commutative-case -NegFlonum -Flonum)
             (commutative-case -NonPosFlonum -Flonum)
             (map varop (list -NegFlonum -NonPosFlonum -Flonum
                              -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero
                              -PosSingleFlonum -NonNegSingleFlonum))
             (commutative-case -NegSingleFlonum -SingleFlonum)
             (commutative-case -NonPosSingleFlonum -SingleFlonum)
             (map varop (list -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                              -InexactRealPosZero -InexactRealNegZero -InexactRealZero
                              -PosInexactReal -NonNegInexactReal))
             (commutative-case -NegInexactReal -InexactReal)
             (commutative-case -NonPosInexactReal -InexactReal)
             (map varop (list -NegInexactReal -NonPosInexactReal -InexactReal
                              -RealZero -PosReal -NonNegReal))
             (commutative-case -NegReal -Real)
             (commutative-case -NonPosReal -Real)
             (map varop (list -NegReal -NonPosReal -Real)))]

[add1 (from-cases
       (-> -Zero -One)
       (-> -One -PosByte)
       (-> -Byte -PosIndex)
       (-> -Index -PosFixnum)
       (-> -NegFixnum -NonPosFixnum)
       (-> -NonPosFixnum -Fixnum)
       (-> -Nat -Pos)
       (-> -NegInt -NonPosInt)
       (unop -Int)
       (-> -NonNegRat -PosRat)
       (unop -Rat)
       (-> -NonNegFlonum -PosFlonum)
       (unop -Flonum)
       (-> -NonNegSingleFlonum -PosSingleFlonum)
       (unop -SingleFlonum)
       (-> -NonNegInexactReal -PosInexactReal)
       (unop -InexactReal)
       (-> -NonNegReal -PosReal)
       (map unop (list -Real -FloatComplex -SingleFlonumComplex -InexactComplex N)))]

[sub1 (from-cases
       (-> -One -Zero)
       (-> -PosByte -Byte)
       (-> -PosIndex -Index)
       (-> -Index -Fixnum)
       (-> -PosFixnum -NonNegFixnum)
       (-> -NonNegFixnum -Fixnum)
       (-> -Pos -Nat)
       (-> -NonPosInt -NegInt)
       (unop -Int)
       (-> -NonPosRat -NegRat)
       (unop -Rat)
       (-> -NonPosFlonum -NegFlonum)
       (unop -Flonum)
       (-> -NonPosSingleFlonum -NegSingleFlonum)
       (unop -SingleFlonum)
       (-> -NonPosInexactReal -NegInexactReal)
       (unop -InexactReal)
       (-> -NonPosReal -NegReal)
       (map unop (list -Real -FloatComplex -SingleFlonumComplex -InexactComplex N)))]

[quotient
 (from-cases
  (-Zero -Int . -> . -Zero)
  (map (lambda (t) (-> t -One t)) ; division by one is identity
       (list -PosByte -Byte -PosIndex -Index
             -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum))
  (-Byte -Nat . -> . -Byte)
  (-Byte -Int . -> . -Fixnum) ; may be negative
  (-Index -Nat . -> . -Index)
  (-Index -Int . -> . -Fixnum) ; same.
  ;; we don't have equivalent for fixnums:
  ;; (quotient min-fixnum -1) -> max-fixnum + 1
  (commutative-binop -NonNegFixnum -NonPosFixnum -NonPosFixnum)
  (-NonPosFixnum -NonPosFixnum . -> . -Nat)
  (-NonNegFixnum -Nat . -> . -NonNegFixnum)
  (-NonNegFixnum -Int . -> . -Fixnum)
  (binop -Nat)
  (commutative-binop -Nat -NonPosInt -NonPosInt)
  (-NonPosInt -NonPosInt . -> . -Nat)
  (binop -Int))]
[remainder ; result has same sign as first arg
 (from-cases
  (-One -One . -> . -Zero)
  (map (lambda (t) (list (-> -Nat t t)
                         (-> t -Int t)))
       (list -Byte -Index -NonNegFixnum -Nat))
  (-NonPosFixnum -Int . -> . -NonPosFixnum)
  (-NonPosInt -Int . -> . -NonPosInt)
  (commutative-binop -Fixnum -Int)
  (binop -Int))]
[modulo ; result has same sign as second arg
 (from-cases
  (-One -One . -> . -Zero)
  (map (lambda (t) (list (-> -Int t t)
                         (-> t -Nat t)))
       (list -Byte -Index -NonNegFixnum -Nat))
  (-Int -NonPosFixnum . -> . -NonPosFixnum)
  (-Int -NonPosInt . -> . -NonPosInt)
  (commutative-binop -Fixnum -Int)
  (binop -Int))]
;; should be consistent with quotient and remainder
[quotient/remainder
 (from-cases
  (-Zero -Int . -> . (-values (list -Zero -Zero)))
  (-One -One . -> . (-values (list -Zero -One)))
  ;; division by one is identity, and has no remainder
  (map (lambda (t) (t -One . -> . (-values (list t -Zero))))
       (list -PosByte -Byte -PosIndex -Index
             -PosFixnum -NonNegFixnum -NegFixnum -NonPosFixnum -Fixnum))
  (-Byte -Nat . -> . (-values (list -Byte -Byte)))
  (-Byte -Int . -> . (-values (list -Fixnum -Byte)))
  (-Index -Nat . -> . (-values (list -Index -Index)))
  (-Index -Int . -> . (-values (list -Fixnum -Index)))
  (-Nat -Byte . -> . (-values (list -Nat -Byte)))
  (-Nat -Index . -> . (-values (list -Nat -Index)))
  (-NonNegFixnum -NonNegFixnum . -> . (-values (list -NonNegFixnum -NonNegFixnum)))
  (-NonNegFixnum -NonPosFixnum . -> . (-values (list -NonPosFixnum -NonNegFixnum)))
  (-NonPosFixnum -NonNegFixnum . -> . (-values (list -NonPosFixnum -NonPosFixnum)))
  (-NonPosFixnum -NonPosFixnum . -> . (-values (list -NonNegFixnum -NonPosFixnum)))
  (-NonNegFixnum -Nat . -> . (-values (list -NonNegFixnum -NonNegFixnum)))
  (-NonNegFixnum -Int . -> . (-values (list -Fixnum -NonNegFixnum)))
  (-Nat -NonNegFixnum . -> . (-values (list -Nat -NonNegFixnum)))
  ;; in the following cases, we can't guarantee that the quotient is within
  ;; fixnum range: (quotient min-fixnum -1) -> max-fixnum + 1
  (-NonPosFixnum -Int . -> . (-values (list -Int -NonPosFixnum)))
  (-Fixnum -Int . -> . (-values (list -Int -Fixnum)))
  (-Int -Fixnum . -> . (-values (list -Int -Fixnum)))
  (-Nat -Nat . -> . (-values (list -Nat -Nat)))
  (-Nat -NonPosInt . -> . (-values (list -NonPosInt -Nat)))
  (-Nat -Int . -> . (-values (list -Int -Nat)))
  (-NonPosInt -Nat . -> . (-values (list -NonPosInt -NonPosInt)))
  (-NonPosInt -NonPosInt . -> . (-values (list -Nat -NonPosInt)))
  (-NonPosInt -Int . -> . (-values (list -Int -NonPosInt)))
  (-Int -Int . -> . (-values (list -Int -Int))))]

[arithmetic-shift (cl->* (-Zero -NonPosInt . -> . -Zero)
                         (-Byte -NonPosInt . -> . -Byte)
                         (-Index -NonPosInt . -> . -Index)
                         (-NonNegFixnum -NonPosInt . -> . -NonNegFixnum)
                         (-Fixnum -NonPosInt . -> . -Fixnum)
                         (-Nat -Int . -> . -Nat)
                         (-Int -Int . -> . -Int))]

[bitwise-and
 (let ([mix-with-int
        (lambda (t)
          (list (->* (list t) t t) ; closed
                (->* (list -Int t) t t) ; brings result down
                (->* (list t -Int) t t)))])
   (from-cases (-> -NegFixnum) ; no args -> -1
               (map mix-with-int (list -Zero -Byte -Index -NonNegFixnum))
               ;; closed on negatives, but not closed if we mix with positives
               (map varop-1+ (list -NegFixnum -NonPosFixnum))
               (map mix-with-int (list -Fixnum -Nat))
               (map varop-1+ (list -NegInt -NonPosInt))
               (null -Int . ->* . -Int)))]
[bitwise-ior
 (from-cases (varop -Zero)
             (map (lambda (l) (apply commutative-case l))
                  (list (list -One -Zero)
                        (list -PosByte -Byte)
                        (list -Byte -Byte) ; doesn't need commutative case (varop would do), but saves code to put it here
                        (list -PosIndex -Index)
                        (list -Index -Index) ; same
                        (list -PosFixnum -NonNegFixnum)
                        (list -NonNegFixnum -NonNegFixnum)
                        (list -NegFixnum -Fixnum) ; as long as there's one negative, the result is negative
                        (list -Fixnum -Fixnum)
                        (list -PosInt -Nat)
                        (list -Nat -Nat)
                        (list -NegInt -Int)
                        (list -Int -Int))))]
[bitwise-not (cl->* (-> -NonNegFixnum -NegFixnum)
                    (-> -NegFixnum -NonNegFixnum)
                    (-> -Fixnum -Fixnum)
                    (-> -Nat -NegInt)
                    (-> -NegInt -Nat)
                    (-> -Int -Int))]
[bitwise-xor
 (from-cases
  (-> -One -One)
  (-> -One -One -Zero)
  (-> -One -One -One -One)
  (map varop-1+ (list -Zero -Byte -Index -NonNegFixnum))
  (-> -NegFixnum -NegFixnum)
  (-> -NonPosFixnum -NonPosFixnum)
  (-> -NegFixnum -NegFixnum -NonNegFixnum) ; both have to be negative: (0 -1) -> -1
  (commutative-binop -NegFixnum -NonNegFixnum -NegFixnum)
  (-> -NegFixnum -NonNegFixnum -NonPosFixnum) ; not commutative: (<pos> (ann 0 <non-pos>)) -> <pos>
  (map varop-1+ (list -Fixnum -Nat))
  (-> -NegInt -NegInt)
  (-> -NonPosInt -NonPosInt)
  (-> -NegInt -NegInt -Nat)
  (commutative-binop -NegInt -Nat -NegInt)
  (-> -NegInt -Nat -NonPosInt) ; see above
  (varop -Int))]
[bitwise-bit-set? (-> -Int -Int B)]
[bitwise-bit-field
 (from-cases (map (lambda (t [r t]) (-> t -Int -Int t))
                  (list -Byte -Index -NonNegFixnum -Nat))
             ;; you can extract as many bits as you want from any negative number
             (list (-> -Int -Int -Int -Int)))]
[integer-length (-> -Int -NonNegFixnum)]

[abs (from-cases abs-cases)]

;; exactness
[exact->inexact
 (from-cases (map unop all-float-types)
             (-Zero . -> . -FlonumZero)
             (-PosRat . -> . -NonNegFlonum)
             (-NegRat . -> . -NonPosFlonum)
             (-Rat . -> . -Flonum)
             (map unop (list -FlonumZero -PosFlonum -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
                             -SingleFlonumZero -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum))
             (-NonNegReal . -> . -NonNegInexactReal) ; not for Pos, possible underflow
             (-NonPosReal . -> . -NonPosInexactReal)
             (-Real . -> . -InexactReal)
             (-FloatComplex . -> . -FloatComplex)
             (-SingleFlonumComplex . -> . -SingleFlonumComplex)
             (-InexactComplex . -> . -InexactComplex)
             (N . -> . (Un -InexactReal -InexactComplex)))]
[inexact->exact
 (from-cases (map unop all-rat-types)
             (-RealZero . -> . -Zero)
             (-PosReal . -> . -PosRat)
             (-NonNegReal . -> . -NonNegRat)
             (-NegReal . -> . -NegRat)
             (-NonPosReal . -> . -NonPosRat)
             (-Real . -> . -Rat)
             (N . -> . -ExactNumber))]
[fl->exact-integer (cl->*
                    (-FlonumZero . -> . -Zero)
                    (-PosFlonum . -> . -PosInt)
                    (-NonNegFlonum . -> . -Nat)
                    (-NegFlonum . -> . -NegInt)
                    (-NonPosFlonum . -> . -NonPosInt)
                    (-Flonum . -> . -Int))]
[real->single-flonum (cl->* (-RealZero . -> . -SingleFlonumZero)
                            ;; no positive / negative cases, possible underflow
                            (-NonNegReal . -> . -NonNegSingleFlonum)
                            (-NonPosReal . -> . -NonPosSingleFlonum)
                            (-Real . -> . -SingleFlonumZero))]
[real->double-flonum (cl->* (-RealZero . -> . -FlonumZero)
                            ;; no positive / negative cases, possible underflow
                            (-NonNegReal . -> . -NonNegFlonum)
                            (-NonPosReal . -> . -NonPosFlonum)
                            (-Real . -> . -Flonum))]

[floor
 (from-cases
  (map unop all-int-types)
  (-> -NonNegRat -Nat)
  (-> -NegRat -NegInt)
  (-> -NonPosRat -NonPosInt)
  (-> -Rat -Int)
  (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                  -NonNegFlonum -NegFlonum -NonPosFlonum -Flonum
                  -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero
                  -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                  -InexactRealPosZero -InexactRealNegZero -InexactRealZero
                  -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
                  -RealZero -NonNegReal -NegReal -NonPosReal -Real)))]
[ceiling
 (from-cases
  (map unop all-int-types)
  (-> -PosRat -PosInt)
  (-> -NonNegRat -Nat)
  (-> -NonPosRat -NonPosInt)
  (-> -Rat -Int)
  (map unop (list -FlonumPosZero -FlonumNegZero -FlonumZero
                  -PosFlonum -NonNegFlonum -NonPosFlonum -Flonum
                  -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero
                  -PosSingleFlonum -NonNegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                  -InexactRealPosZero -InexactRealNegZero -InexactRealZero
                  -PosInexactReal -NonNegInexactReal -NonPosInexactReal -InexactReal
                  -RealZero -PosReal -NonNegReal -NonPosReal -Real)))]
[truncate (round-type)]
[round (round-type)]

[make-rectangular (cl->* (-Rat -Rat . -> . -ExactNumber)
                         (-Flonum -Flonum . -> . -FloatComplex)
                         (-Flonum (Un -PosReal -NegReal) . -> . -FloatComplex) ; no exact 0
                         ((Un -PosReal -NegReal) -Flonum . -> . -FloatComplex)
                         (-SingleFlonum -SingleFlonum . -> . -SingleFlonumComplex)
                         (-InexactReal -InexactReal . -> . -InexactComplex)
                         (-Real -Real . -> . N))]
[make-polar (cl->* (-Flonum -Flonum . -> . -FloatComplex)
                   (-SingleFlonum -SingleFlonum . -> . -SingleFlonumComplex)
                   (-InexactReal -InexactReal . -> . -InexactComplex)
                   (-Real -Real . -> . N))]
[real-part (from-cases
            (map unop all-real-types)
            (-ExactNumber . -> . -Rat)
            (-FloatComplex . -> . -Flonum)
            (-SingleFlonumComplex . -> . -SingleFlonum)
            (-InexactComplex . -> . -InexactReal)
            (N . -> . -Real))]
[imag-part (cl->* (-Real . -> . -Zero)
                  (-ExactNumber . -> . -Rat)
                  (-FloatComplex . -> . -Flonum)
                  (-InexactComplex . -> . -InexactReal)
                  (N . -> . -Real))]
[magnitude (from-cases abs-cases
                       (-FloatComplex . -> . -NonNegFlonum)
                       (-SingleFlonumComplex . -> . -NonNegSingleFlonum)
                       (-InexactComplex . -> . -NonNegInexactReal)
                       (N . -> . -NonNegReal))]
[angle     (cl->* (-PosReal . -> . -Zero)
                  (-FloatComplex . -> . -Flonum)
                  (-InexactComplex . -> . -InexactReal)
                  (N . -> . (Un -InexactReal -Zero)))]
[numerator
 (from-cases (map unop all-int-types)
             (-PosRat . -> . -PosInt)
             (-NonNegRat . -> . -Nat)
             (-NegRat . -> . -NegInt)
             (-NonPosRat . -> . -NonPosInt)
             (-Rat . -> . -Int)
             ;; includes rational types, but these have been matched already
             (map unop all-real-types))]
[denominator (cl->* (-Integer . -> . -One)
                    (-Rat . -> . -PosInt)
                    (-Flonum . -> . -PosFlonum)
                    (-SingleFlonum . -> . -PosSingleFlonum)
                    (-InexactReal . -> . -PosInexactReal)
                    (-Real . -> . -PosReal))]
[rationalize
 (from-cases (map (lambda (t) (-> t -Rat t))
                  all-int-types)
             (-NonNegRat -Rat . -> . -NonNegRat) ; non-zero args produce zero
             (-NonPosRat -Rat . -> . -NonPosRat)
             (-Rat -Rat . -> . -Rat)
             (map (lambda (l) (apply commutative-binop l))
                  ;; actually, second argument could be negative in all cases,
                  ;; and it would still work, but this would require twice as
                  ;; many cases
                  (list (list -NonNegReal -NonNegFlonum)
                        (list -NonPosReal -NonPosFlonum)
                        (list -Real -Flonum)
                        (list -NonNegReal -NonNegSingleFlonum)
                        (list -NonPosReal -NonPosSingleFlonum)
                        (list -Real -SingleFlonum)
                        (list -NonNegReal -NonNegInexactReal)
                        (list -NonPosReal -NonPosInexactReal)
                        (list -Real -InexactReal)))
             (map binop (list -NonNegReal -NonPosReal -Real)))]
[expt
 (from-cases (-> -One -Rat -One)
             (map (lambda (t) (-> t -Zero -One)) all-number-types) ; will error if negative
             (-PosInt -Nat . -> . -PosInt)
             (-Nat -Nat . -> . -Nat)
             (-Int -Nat . -> . -Int)
             (-PosInt -Int . -> . -PosRat)
             (-Nat -Int . -> . -NonNegRat)
             (-Int -Int . -> . -Rat)
             (-PosRat -Int . -> . -PosRat)
             (-NonNegRat -Int . -> . -NonNegRat)
             (-Rat -Int . -> . -Rat)
             (-PosFlonum -Real . -> . -PosFlonum)
             (-PosReal -Flonum . -> . -PosFlonum)
             (-NonNegFlonum -Real . -> . -NonNegFlonum)
             (-NonNegReal -Flonum . -> . -NonNegFlonum)
             (-Flonum (Un -NegInt -PosInt) . -> . -Flonum)
             (-Flonum -Int . -> . (Un -Flonum -One))
             (-Flonum -Flonum . -> . (Un -Flonum -FloatComplex))
             (-NonNegSingleFlonum (Un -SingleFlonum -NegRat -PosRat) . -> . -NonNegSingleFlonum)
             (-SingleFlonum (Un -NegInt -PosInt) . -> . -SingleFlonum)
             (-SingleFlonum -Int . -> . (Un -SingleFlonum -One))
             (-SingleFlonum -SingleFlonum . -> . (Un -SingleFlonum -SingleFlonumComplex))
             (-NonNegInexactReal (Un -NegReal -PosReal) . -> . -NonNegInexactReal)
             (-NonNegReal -Real . -> . -NonNegReal)
             (-InexactReal (Un -NegInt -PosInt) . -> . -InexactReal)
             (-InexactReal -InexactReal . -> . (Un -InexactReal -InexactComplex))
             (-Real -Int . -> . -Real)
             (-FloatComplex (Un -InexactComplex -InexactReal) . -> . -FloatComplex)
             (-SingleFlonumComplex (Un -SingleFlonum -SingleFlonumComplex) . -> . -SingleFlonumComplex)
             ((Un -InexactReal -InexactComplex) -InexactComplex . -> . -InexactComplex)
             (-InexactComplex (Un -InexactReal -InexactComplex) . -> . -InexactComplex)
             (N N . -> . N))]
[sqrt
 (from-cases
  (map unop (list -Zero -One
                  -FlonumPosZero -FlonumNegZero -FlonumZero -PosFlonum -NonNegFlonum
                  -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -PosSingleFlonum -NonNegSingleFlonum
                  -InexactRealPosZero -InexactRealNegZero -InexactRealZero -PosInexactReal -NonNegInexactReal
                  -RealZero -PosReal -NonNegReal))
  (-FloatComplex . -> . -FloatComplex)
  (-SingleFlonumComplex . -> . -SingleFlonumComplex)
  (-InexactComplex . -> . -InexactComplex)
  (N . -> . N))]
[integer-sqrt
 (from-cases
  (map unop (list -Zero -One -Byte -Index))
  (-NonNegFixnum . -> . -Index)
  (map unop (list -Nat -NonNegRat
                  -FlonumPosZero -FlonumNegZero -FlonumZero -NonNegFlonum
                  -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -NonNegSingleFlonum
                  -InexactRealPosZero -InexactRealNegZero -InexactRealZero -NonNegInexactReal
                  -RealZero -NonNegReal))
  (-Rat . -> . -ExactNumber)
  (-Real . -> . N))] ; defined on inexact integers too, but not complex
[integer-sqrt/remainder
 (from-cases
  (-Zero . -> . (-values -Zero -Zero))
  (-One . -> . (-values -One -Zero))
  (-Byte . -> . (-values -Byte -Byte))
  (-Index . -> . (-values -Index -Index))
  (-NonNegFixnum . -> . (-values -Index -NonNegFixnum))

  (map (λ (t) (t . -> . (-values t -Int)))
       (list -Nat -NonNegRat
             -FlonumPosZero -FlonumNegZero -FlonumZero -NonNegFlonum
             -SingleFlonumPosZero -SingleFlonumNegZero -SingleFlonumZero -NonNegSingleFlonum
             -InexactRealPosZero -InexactRealNegZero -InexactRealZero -NonNegInexactReal
             -RealZero -NonNegReal))

  (-Rat . -> . (-values -ExactNumber -Int))
  (-Real . -> . (-values N -Int)))] ; defined on inexact integers too

[log (cl->*
      (-NonNegRat . -> . -Real)
      (-FlonumZero . -> . -NegFlonum)
      (-NonNegFlonum . -> . -Flonum)
      (-SingleFlonumZero . -> . -NegSingleFlonum)
      (-NonNegSingleFlonum . -> . -SingleFlonum)
      (-InexactRealZero . -> . -NegInexactReal)
      (-NonNegInexactReal . -> . -InexactReal)
      (-NonNegReal . -> . -Real)
      (-FloatComplex . -> . -FloatComplex)
      (-SingleFlonumComplex . -> . -SingleFlonumComplex)
      (-InexactComplex . -> . -InexactComplex)
      (N . -> . N))]
[exp (from-cases (-Zero . -> . -One)
                 (map unop
                      (list -Flonum -SingleFlonum -InexactReal -Real
                            -FloatComplex -SingleFlonumComplex -InexactComplex N)))]

[cos (from-cases (-Zero . -> . -One)
                 (map unop
                      (list -Flonum -SingleFlonum -InexactReal -Real
                            -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[sin (from-cases (map unop
                      (list -Zero -Flonum -SingleFlonum -InexactReal -Real
                            -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[tan (from-cases (map unop
                      (list -Zero -Flonum -SingleFlonum -InexactReal -Real
                            -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[acos (from-cases (-One . -> . -Zero)
                  (map unop
                       (list -Flonum -SingleFlonum -InexactReal -Real
                             -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[asin (from-cases (-Zero . -> . -One)
                  (map unop
                       (list -Flonum -SingleFlonum -InexactReal -Real
                             -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[atan (from-cases
       (map unop (list -Zero -Flonum -SingleFlonum -InexactReal -Real
                       -FloatComplex -SingleFlonumComplex -InexactComplex N))
       ;; 2-arg case, atan2
       (-Zero -PosRat . -> . -Zero)
       (map binop (list -Flonum -SingleFlonum -InexactReal -Real)))]

[gcd (from-cases (varop -Zero)
                 (varop-1+ -One)
                 (varop-1+ -PosByte)
                 (varop -Byte)
                 (varop-1+ -PosIndex)
                 (varop -Index)
                 (varop-1+ -PosFixnum)
                 (varop -Fixnum -NonNegFixnum)
                 (varop-1+ -PosInt)
                 (varop -Int -Nat)
                 (varop-1+ -PosRat)
                 (varop -Rat -NonNegRat)
                 ;; also supports inexact integers
                 (varop-1+ -PosFlonum)
                 (commutative-case -PosFlonum -PosReal -PosFlonum)
                 (varop -Flonum -NonNegFlonum)
                 (commutative-case -Flonum -Real -NonNegFlonum)
                 (varop-1+ -PosSingleFlonum)
                 (varop -SingleFlonum -NonNegSingleFlonum)
                 (varop-1+ -PosInexactReal)
                 (varop -InexactReal -NonNegInexactReal)
                 ;; Note: this will mess up error messages, since only integers
                 ;; (exact or not) are accepted, not any reals.
                 ;; should we only accept exact integers?
                 (varop-1+ -PosReal)
                 (varop -Real -NonNegReal))]
[lcm (from-cases (map unop (list -Zero -One -PosByte -Byte -PosIndex -Index -PosFixnum))
                 (-NegFixnum . -> . -PosFixnum)
                 (-Fixnum . -> . -NonNegFixnum)
                 (commutative-case -Zero -Rat) ; zero anywhere -> zero
                 (commutative-case -RealZero -Real)
                 (map (lambda (t) (commutative-binop -One t))
                      (list -PosByte -Byte -PosIndex -Index -PosFixnum))
                 (commutative-binop -One -NegFixnum -PosFixnum)
                 (commutative-binop -One -Fixnum -NonNegFixnum)
                 (binop -PosByte -PosIndex)
                 (binop -Byte -Index)
                 (varop (Un -PosInt -NegInt) -PosInt)
                 (varop -Int -Nat)
                 (varop (Un -PosRat -NegRat) -PosRat)
                 (varop -Rat -NonNegRat)
                 ;; also supports inexact integers
                 (commutative-case -FlonumZero -Real -FlonumZero)
                 (commutative-case -SingleFlonumZero -Real -SingleFlonumZero)
                 (commutative-case -InexactRealZero -Real -InexactRealZero)
                 (varop-1+ (Un -PosFlonum -NegFlonum) -PosFlonum)
                 (varop-1+ -Flonum -NonNegFlonum)
                 (commutative-case (Un -PosFlonum -NegFlonum) (Un -PosReal -NegReal) -PosFlonum)
                 (commutative-case -Flonum (Un -PosReal -NegReal) -NonNegFlonum) ; exact 0 -> exact 0
                 (varop-1+ (Un -PosSingleFlonum -NegSingleFlonum) -PosSingleFlonum)
                 (varop-1+ -SingleFlonum -NonNegSingleFlonum)
                 (varop-1+ (Un -PosInexactReal -NegInexactReal) -PosInexactReal)
                 (varop-1+ -InexactReal -NonNegInexactReal)
                 ;; Note: same as above.
                 (varop (Un -PosReal -NegReal) -PosReal)
                 (varop -Real -NonNegReal))]

;; racket/math

[sgn (cl->* (-Zero . -> . -Zero)
            (-PosRat . -> . -One)
            (-NonNegRat . -> . -Index) ; 0 or 1
            (-NegRat . -> . -NegFixnum) ; -1
            (-NonPosRat . -> . -NonPosFixnum) ; 0 or -1
            (-Rat . -> . -Fixnum)
            (-Flonum . -> . -Flonum)
            (-SingleFlonum . -> . -SingleFlonum)
            (-Real . -> . -Real))]

[pi -PosFlonum]
[pi.f -PosSingleFlonum]
[sqr (from-cases (map unop (list -Zero -One))
                 (-> -PosByte -PosIndex)
                 (-> -Byte -Index)
                 (unop -PosInt)
                 (-> -Int -Nat)
                 (unop -PosRat)
                 (-> -Rat -NonNegRat)
                 ;; possible underflow, no pos -> pos
                 (-> -Flonum -NonNegFlonum)
                 (-> -SingleFlonum -NonNegSingleFlonum)
                 (-> -InexactReal -NonNegInexactReal)
                 (-> -Real -NonNegReal)
                 (map unop (list -FloatComplex -SingleFlonumComplex
                                 -InexactComplex -ExactNumber N)))]
[conjugate (from-cases
            (map unop all-real-types)
            (-FloatComplex . -> . -FloatComplex)
            (-SingleFlonumComplex . -> . -SingleFlonumComplex)
            (-InexactComplex . -> . -InexactComplex)
            (-ExactNumber . -> . -ExactNumber)
            (N . -> . N))]
[sinh (from-cases
       (unop -Zero) ; only exact case
       ;; possible underflow, no pos -> pos. 0 -> 0, no -NonNegRat -> -NonNegFlonum
       ((Un -PosRat -NonNegFlonum) . -> . -NonNegFlonum)
       ((Un -NegRat -NonPosFlonum) . -> . -NonPosFlonum)
       (map unop (list -FlonumNan -Flonum
                       -SingleFlonumNan -NonNegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                       -NonNegInexactReal -NonPosInexactReal -InexactReal
                       -NonNegReal -NonPosReal -Real
                       -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[cosh (from-cases ; no exact cases
       (map unop (list -FlonumNan -SingleFlonumNan))
       ((Un -Rat -Flonum) . -> . -PosFlonum)
       (-SingleFlonum . -> . -PosSingleFlonum)
       (-InexactReal . -> . -PosInexactReal)
       (-Real . -> . -PosReal)
       (map unop (list -FloatComplex -SingleFlonumComplex -InexactComplex N)))]
[tanh (from-cases ; same as sinh
       (unop -Zero) ; only exact case
       ;; possible underflow, no pos -> pos. 0 -> 0, no -NonNegRat -> -NonNegFlonum
       ((Un -PosRat -NonNegFlonum) . -> . -NonNegFlonum)
       ((Un -NegRat -NonPosFlonum) . -> . -NonPosFlonum)
       (map unop (list -FlonumNan -Flonum
                       -SingleFlonumNan -NonNegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                       -NonNegInexactReal -NonPosInexactReal -InexactReal
                       -NonNegReal -NonPosReal -Real
                       -FloatComplex -SingleFlonumComplex -InexactComplex N)))]

[degrees->radians
 (from-cases
  (unop -Zero) ; only exact case
  ((Un -PosRat -PosFlonum) . -> . -NonNegFlonum) ; possible underflow, no pos -> pos
  ((Un -NegRat -NegFlonum) . -> . -NonPosFlonum)
  ((Un -PosSingleFlonum) . -> . -NonNegSingleFlonum)
  ((Un -NegSingleFlonum) . -> . -NonPosSingleFlonum)
  (map unop (list -FlonumNan -SingleFlonumNan
                  -NonNegFlonum -NonPosFlonum -Flonum
                  -NonNegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                  -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
                  -PosReal -NonNegReal -NegReal -NonPosReal -Real)))]

[radians->degrees
 (from-cases
  (unop -Zero) ; only exact case
  ((Un -PosRat -PosFlonum) . -> . -PosFlonum)
  ((Un -NegRat -NegFlonum) . -> . -NegFlonum)
  (map unop (list -FlonumNan -SingleFlonumNan
                  -NonNegFlonum -NonPosFlonum -Flonum
                  -PosSingleFlonum -NonNegSingleFlonum -NegSingleFlonum -NonPosSingleFlonum -SingleFlonum
                  -PosInexactReal -NonNegInexactReal -NegInexactReal -NonPosInexactReal -InexactReal
                  -PosReal -NonNegReal -NegReal -NonPosReal -Real)))]

[exact-round (exact-round-type)]
[exact-truncate (exact-round-type)]

[exact-floor
 (from-cases
  (map unop all-int-types)
  (inexact-zero->exact-zero-type)
  (-> (Un -NonNegRat -NonNegFlonum -NonNegSingleFlonum -NonNegInexactReal -NonNegReal) -Nat)
  (-> (Un -NegRat -NegFlonum -NegSingleFlonum -NegInexactReal -NegReal) -NegInt)
  (-> (Un -NonPosRat -NonPosFlonum -NonPosSingleFlonum -NonPosInexactReal -NonPosReal) -NonPosInt)
  (-> (Un -Rat -Flonum -SingleFlonum -InexactReal -Real) -Int))]

[exact-ceiling
 (from-cases
  (map unop all-int-types)
  (inexact-zero->exact-zero-type)
  (-> (Un -PosRat -PosFlonum -PosSingleFlonum -PosInexactReal -PosReal) -PosInt)
  (-> (Un -NonNegRat -NonNegFlonum -NonNegSingleFlonum -NonNegInexactReal -NonNegReal) -Nat)
  (-> (Un -NonPosRat -NonPosFlonum -NonPosSingleFlonum -NonPosInexactReal -NonPosReal) -NonPosInt)
  (-> (Un -Rat -Flonum -SingleFlonum -InexactReal -Real) -Int))]

[nan? (from-cases
       (-> -Rat (-val #f))
       (-> (Un -FlonumNan -SingleFlonumNan) (-val #t))
       (-> -Real B))]

[infinite? (from-cases
            (-> (Un -FlonumNan -FlonumNegZero -FlonumPosZero
                    -SingleFlonumNan -SingleFlonumNegZero -SingleFlonumPosZero
                    -Rat)
                (-val #f))
            (-> -Real B))]

;; racket/fixnum
[fx+ (fx+-type)]
[fx- (fx--type)]
[fx* (fx*-type)]
[fxquotient (fxquotient-type)]
[fxremainder (fxremainder-type)]
[fxmodulo (fxmodulo-type)]
[fxabs (fxabs-type)]

[fxand (fxand-type)]
[fxior (fxior-type)]
[fxxor (fxxor-type)]
[fxnot (fxnot-type)]
[fxlshift (fxlshift-type)]
[fxrshift (fxrshift-type)]

[fx= (fx=-type)]
[fx< (fx<-type)]
[fx> (fx>-type)]
[fx<= (fx<=-type)]
[fx>= (fx>=-type)]
[fxmin (fxmin-type)]
[fxmax (fxmax-type)]

[unsafe-fx+ (fx+-type)]
[unsafe-fx- (fx--type)]
[unsafe-fx* (fx*-type)]
[unsafe-fxquotient (fxquotient-type)]
[unsafe-fxremainder (fxremainder-type)]
[unsafe-fxmodulo (fxmodulo-type)]
[unsafe-fxabs (fxabs-type)]

[unsafe-fxand (fxand-type)]
[unsafe-fxior (fxior-type)]
[unsafe-fxxor (fxxor-type)]
[unsafe-fxnot (fxnot-type)]
[unsafe-fxlshift (fxlshift-type)]
[unsafe-fxrshift (fxrshift-type)]

[unsafe-fx= (fx=-type)]
[unsafe-fx< (fx<-type)]
[unsafe-fx> (fx>-type)]
[unsafe-fx<= (fx<=-type)]
[unsafe-fx>= (fx>=-type)]
[unsafe-fxmin (fxmin-type)]
[unsafe-fxmax (fxmax-type)]


;; flonum ops
[flabs (flabs-type)]
[fl+ (fl+-type)]
[fl- (fl--type)]
[fl* (fl*-type)]
[fl/ (fl/-type)]
[fl= (fl=-type)]
[fl<= (fl<=-type)]
[fl>= (fl>=-type)]
[fl> (fl>-type)]
[fl< (fl<-type)]
[flmin (flmin-type)]
[flmax (flmax-type)]
[flround (flround-type)]
[flfloor (flfloor-type)]
[flceiling (flceiling-type)]
[fltruncate (flround-type)]
[flsin (fl-unop)] ; special cases (0s) not worth special-casing
[flcos (fl-unop)]
[fltan (fl-unop)]
[flatan (fl-unop)]
[flasin (fl-unop)]
[flacos (fl-unop)]
[fllog (fllog-type)]
[flexp (flexp-type)]
[flsqrt (flsqrt-type)]
[flexpt (flexpt-type)]
[->fl (fx->fl-type)]
[fx->fl (fx->fl-type)]
[fl->fx (fl->fx-type)]
[make-flrectangular (make-flrectangular-type)]
[flreal-part (flreal-part-type)]
[flimag-part (flimag-part-type)]
[flrandom (flrandom-type)]

[unsafe-flabs (flabs-type)]
[unsafe-fl+ (fl+-type)]
[unsafe-fl- (fl--type)]
[unsafe-fl* (fl*-type)]
[unsafe-fl/ (fl/-type)]
[unsafe-fl= (fl=-type)]
[unsafe-fl<= (fl<=-type)]
[unsafe-fl>= (fl>=-type)]
[unsafe-fl> (fl>-type)]
[unsafe-fl< (fl<-type)]
[unsafe-flmin (flmin-type)]
[unsafe-flmax (flmax-type)]

;These are currently the same binding as the safe versions
;and so are not needed. If this changes they should be
;uncommented. There is a check in the definitions part of
;the file that makes sure that they are the same binding.
;
;[unsafe-flround (flround-type)]
;[unsafe-flfloor (flfloor-type)]
;[unsafe-flceiling (flceiling-type)]
;[unsafe-fltruncate (flround-type)]
;[unsafe-flsin (fl-unop)]
;[unsafe-flcos (fl-unop)]
;[unsafe-fltan (fl-unop)]
;[unsafe-flatan (fl-unop)]
;[unsafe-flasin (fl-unop)]
;[unsafe-flacos (fl-unop)]
;[unsafe-fllog (fllog-type)]
;[unsafe-flexp (flexp-type)]
;[unsafe-flexpt (flexpt-type)]
;
[unsafe-flsqrt (flsqrt-type)]
[unsafe-fx->fl (fx->fl-type)]
[unsafe-fl->fx (fl->fx-type)]
[unsafe-make-flrectangular (make-flrectangular-type)]
[unsafe-flreal-part (flreal-part-type)]
[unsafe-flimag-part (flimag-part-type)]
[unsafe-flrandom (flrandom-type)]

; racket/extflonum
[extflonum? (make-pred-ty -ExtFlonum)]
[extflonum-available? (-> B)]
[pi.t -PosExtFlonum]

[extflabs (extflabs-type)]
[extfl+ (extfl+-type)]
[extfl- (extfl--type)]
[extfl* (extfl*-type)]
[extfl/ (extfl/-type)]
[extfl= (extfl=-type)]
[extfl<= (extfl<=-type)]
[extfl>= (extfl>=-type)]
[extfl> (extfl>-type)]
[extfl< (extfl<-type)]
[extflmin (extflmin-type)]
[extflmax (extflmax-type)]
[extflround (extflround-type)]
[extflfloor (extflfloor-type)]
[extflceiling (extflceiling-type)]
[extfltruncate (extflround-type)]
[extflsin (extfl-unop)] ; special cases (0s) not worth special-casing
[extflcos (extfl-unop)]
[extfltan (extfl-unop)]
[extflatan (extfl-unop)]
[extflasin (extfl-unop)]
[extflacos (extfl-unop)]
[extfllog (extfllog-type)]
[extflexp (extflexp-type)]
[extflexpt (extflexpt-type)]
[extflsqrt (extflsqrt-type)]
[->extfl (fx->extfl-type)]
[extfl->exact-integer (cl->* (-ExtFlonumZero . -> . -Zero)
                             (-PosExtFlonum . -> . -PosInt)
                             (-NonNegExtFlonum . -> . -Nat)
                             (-NegExtFlonum . -> . -NegInt)
                             (-NonPosExtFlonum . -> . -NonPosInt)
                             (-ExtFlonum . -> . -Int))]
[real->extfl (cl->* (-RealZero . -> . -ExtFlonumZero)
                    ;; no positive / negative cases, possible underflow
                    (-NonNegReal . -> . -NonNegExtFlonum)
                    (-NonPosReal . -> . -NonPosExtFlonum)
                    (-Real . -> . -ExtFlonum))]
[extfl->exact (cl->* (-ExtFlonumZero . -> . -Zero)
                     (-PosExtFlonum . -> . -PosRat)
                     (-NonNegExtFlonum . -> . -NonNegRat)
                     (-NegExtFlonum . -> . -NegRat)
                     (-NonPosExtFlonum . -> . -NonPosRat)
                     (-ExtFlonum . -> . -Rat))]
[extfl->inexact (cl->* (-ExtFlonumZero . -> . -FlonumZero)
                       ;; no positive / negative cases, possible underflow
                       (-NonNegExtFlonum . -> . -NonNegFlonum)
                       (-NonPosExtFlonum . -> . -NonPosFlonum)
                       (-ExtFlonum . -> . -Flonum))]
[unsafe-extflabs (extflabs-type)]
[unsafe-extfl+ (extfl+-type)]
[unsafe-extfl- (extfl--type)]
[unsafe-extfl* (extfl*-type)]
[unsafe-extfl/ (extfl/-type)]
[unsafe-extfl= (extfl=-type)]
[unsafe-extfl<= (extfl<=-type)]
[unsafe-extfl>= (extfl>=-type)]
[unsafe-extfl> (extfl>-type)]
[unsafe-extfl< (extfl<-type)]
[unsafe-extflmin (extflmin-type)]
[unsafe-extflmax (extflmax-type)]

;These are currently the same binding as the safe versions
;and so are not needed. If this changes they should be
;uncommented. There is a check in the definitions part of
;the file that makes sure that they are the same binding.
;
;[unsafe-extflround (extflround-type)]
;[unsafe-extflfloor (extflfloor-type)]
;[unsafe-extflceiling (extflceiling-type)]
;[unsafe-extfltruncate (extflround-type)]
;[unsafe-extflsin (extfl-unop)]
;[unsafe-extflcos (extfl-unop)]
;[unsafe-extfltan (extfl-unop)]
;[unsafe-extflatan (extfl-unop)]
;[unsafe-extflasin (extfl-unop)]
;[unsafe-extflacos (extfl-unop)]
;[unsafe-extfllog (extfllog-type)]
;[unsafe-extflexp (extflexp-type)]
;[unsafe-extflexpt (extflexpt-type)]
;
[unsafe-extflsqrt (extflsqrt-type)]
[unsafe-fx->extfl (fx->extfl-type)]
[unsafe-extfl->fx (extfl->fx-type)]
