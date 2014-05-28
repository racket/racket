#lang racket

(require typed-racket/infer/infer
         typed-racket/types/union
         typed-racket/types/filter-ops
         typed-racket/types/abbrev
         typed-racket/rep/type-rep
         typed-racket/types/numeric-tower
         typed-racket/types/base-abbrev
         typed-racket/types/subtype
         typed-racket/base-env/base-env-numeric
         (only-in (combine-in typed-racket/types/numeric-tower
                              typed-racket/types/abbrev)
                  [-Number N] [-Boolean B] [-Symbol Sym] [-Real R] [-PosInt -Pos]))



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

(define mul-ty
  (from-cases
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
    (varop N)))

[define /-ty
  (from-cases ; very similar to multiplication, without closure properties for integers
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

(define minus-ty
  (from-cases
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
    (varop-1+ N)))

(define plus-ty
  (from-cases
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
   (varop N)))

(define (bigtree n b1 b2)
  (if (zero? n) b1
      (-pair (bigtree (sub1 n) b2 b1)
             (bigtree (sub1 n) b1 b2))))

(define (tree-ty e)
  (-mu x (Un e (-pair x x))))

(define (treecall n)
  (define b1 -String)
  (define b2 -Symbol)
  (infer (list 'v) null
         (list (bigtree n b1 b2))
         (list (tree-ty (-v v)))
         -Nat -Nat))

(define (bigcall n types)
  (define vars (for/list ([_ n]) (gensym 'g)))
  (define vars-t (map make-F vars))
  (define fun-tys (for/list ([t vars-t]) (->* vars-t t)))
  (define res-ty (->* vars-t (-Tuple vars-t)))
  (infer vars null (take types n) fun-tys res-ty Univ))

(define ts (list plus-ty mul-ty minus-ty /-ty))

(define (go n hsbencher)
  (displayln `(big ,n))
  (define ty-list (append ts ts))
  (collect-garbage) (collect-garbage) (collect-garbage)
  (define run (λ () (void (bigcall n ty-list))))
  (cond [hsbencher
         (define-values (vs t r gc)
           (time-apply run null))
         (printf "SELFTIMED: ~a\n" (/ t 1000.))]
        [else (time (run))]))

(module+ main
  (require racket/cmdline)
  (define hsbencher #f)
  (command-line 
   #:once-each ["--hsbencher" "format for hsbencher" (set! hsbencher #t)]
   #:args ([n "4"]) (go (string->number n) hsbencher)))
