#lang racket/unit

(require "../utils/utils.rkt"
         racket/match
         (typecheck signatures check-below)
         (types abbrev numeric-tower resolve subtype union generalize)
         (rep type-rep)
         (only-in (infer infer) restrict)
         (utils stxclass-util)
         syntax/parse
         unstable/function
         unstable/sequence)

(import)
(export tc-literal^)

;; return the type of a literal value
;; tc-literal: racket-value-syntax [type] -> type
(define (tc-literal v-stx [expected #f])
  (define-syntax-class exp
    (pattern (~and i (~or :number :str :bytes))
             #:fail-unless expected #f
             #:fail-unless (subtype (-val (syntax-e #'i)) expected) #f))
  (syntax-parse v-stx
    [i:exp expected]
    [i:boolean (-val (syntax-e #'i))]
    [i:identifier (-val (syntax-e #'i))]
    ;; Numbers
    [0 -Zero]
    [1 -One]
    [(~var i (3d (conjoin byte? positive?))) -PosByte]
    [(~var i (3d byte?)) -Byte]
    [(~var i (3d (conjoin portable-index? positive?))) -PosIndex]
    [(~var i (3d (conjoin portable-fixnum? positive?))) -PosFixnum]
    [(~var i (3d (conjoin portable-fixnum? negative?))) -NegFixnum]
    [(~var i (3d exact-positive-integer?)) -PosInt]
    [(~var i (3d (conjoin exact-integer? negative?))) -NegInt]
    [(~var i (3d (conjoin number? exact? rational? positive?))) -PosRat]
    [(~var i (3d (conjoin number? exact? rational? negative?))) -NegRat]
    [(~var i (3d (lambda (x) (eqv? x 0.0)))) -FlonumPosZero]
    [(~var i (3d (lambda (x) (eqv? x -0.0)))) -FlonumNegZero]
    [(~var i (3d (lambda (x) (eqv? x +nan.0)))) -FlonumNan]
    [(~var i (3d(lambda (x) (eqv? x +inf.0)))) (-val +inf.0)]
    [(~var i (3d (lambda (x) (eqv? x -inf.0)))) (-val -inf.0)]
    [(~var i (3d (conjoin flonum? positive?))) -PosFlonum]
    [(~var i (3d (conjoin flonum? negative?))) -NegFlonum]
    [(~var i (3d flonum?)) -Flonum] ; for nan
    [(~var i (3d (lambda (x) (eqv? x 0.0f0)))) -SingleFlonumPosZero]
    [(~var i (3d (lambda (x) (eqv? x -0.0f0)))) -SingleFlonumNegZero]
    [(~var i (3d (lambda (x) (eqv? x +nan.f)))) -SingleFlonumNan]
    [(~var i (3d(lambda (x) (eqv? x +inf.f)))) (-val +inf.f)]
    [(~var i (3d (lambda (x) (eqv? x -inf.f)))) (-val -inf.f)]
    [(~var i (3d (conjoin single-flonum? positive?))) -PosSingleFlonum]
    [(~var i (3d (conjoin single-flonum? negative?))) -NegSingleFlonum]
    [(~var i (3d single-flonum?)) -SingleFlonum] ; for nan
    [(~var i (3d inexact-real?)) -InexactReal] ; catch-all, just in case
    [(~var i (3d real?)) -Real] ; catch-all, just in case
    ;; a complex number can't have a float imaginary part and an exact real part
    [(~var i (3d (conjoin number? exact?)))
     -ExactNumber]
    [(~var i (3d (conjoin number? (lambda (x) (and (flonum? (imag-part x))
                                                   (flonum? (real-part x)))))))
     -FloatComplex]
    [(~var i (3d (conjoin number? (lambda (x) (and (single-flonum? (imag-part x))
                                                   (single-flonum? (real-part x)))))))
     -SingleFlonumComplex]
    ;; can't have real and imaginary parts that are both inexact, but not the same precision
    [(~var i (3d number?)) -Number] ; otherwise, Number
    
    [i:str -String]
    [i:char -Char]
    [i:keyword (-val (syntax-e #'i))]
    [i:bytes -Bytes]
    [i:byte-pregexp -Byte-PRegexp]
    [i:byte-regexp -Byte-Regexp]
    [i:pregexp -PRegexp]
    [i:regexp  -Regexp]
    [(~and i ()) (-val '())]
    [(i . r)
     (match (and expected (resolve (restrict expected (-pair Univ Univ) 'orig)))
       [(Pair: a-ty d-ty)
        (-pair
         (tc-literal #'i a-ty)
         (tc-literal #'r d-ty))]
       [t 
        (-pair (tc-literal #'i) (tc-literal #'r))])]
    [(~var i (3d vector?))
     (match (and expected (resolve (restrict expected -VectorTop 'orig)))
       [(Vector: t)
        (make-Vector
          (check-below
            (apply Un
              (for/list ([l (in-vector (syntax-e #'i))])
                (tc-literal l t)))
            t))]
       [(HeterogeneousVector: ts)
        (make-HeterogeneousVector
         (for/list ([l (in-vector (syntax-e #'i))]
                    [t (in-sequence-forever (in-list ts) #f)])
           (cond-check-below (tc-literal l t) t)))]
       [_ (make-HeterogeneousVector (for/list ([l (in-vector (syntax-e #'i))])
                                      (generalize (tc-literal l #f))))])]
    [(~var i (3d hash?))
     (match (and expected (resolve (restrict expected -HashTop 'orig)))
       [(Hashtable: k v)
        (let* ([h (syntax-e #'i)]
               [ks (hash-map h (lambda (x y) (tc-literal x k)))]
               [vs (hash-map h (lambda (x y) (tc-literal y v)))])
          (make-Hashtable
            (check-below (apply Un ks) k)
            (check-below (apply Un vs) v)))]
       [_ (let* ([h (syntax-e #'i)]
                 [ks (hash-map h (lambda (x y) (tc-literal x)))]
                 [vs (hash-map h (lambda (x y) (tc-literal y)))])
            (make-Hashtable (generalize (apply Un ks)) (generalize (apply Un vs))))])]
    [_ Univ]))




