#lang typed/racket/base

(require racket/flonum
         (only-in "mpfr.rkt" 1ary-funs 1ary-preds 1ary2-funs 2ary-funs)
         "../base/base-random.rkt"
         "utils.rkt")

(define-type Rounding-Mode (U 'nearest 'zero 'up 'down))

(require/typed
 "mpfr.rkt"
 ;; Parameters
 [bf-rounding-mode  (Parameterof Rounding-Mode)]
 [bf-min-precision  Exact-Positive-Integer]
 [bf-max-precision  Exact-Positive-Integer]
 [bf-precision  (Parameterof Integer)]
 ;; Type and predicate
 [opaque Bigfloat bigfloat?]
 [bfcanonicalize  (Bigfloat -> Bigfloat)]
 ;; Accessors
 [bigfloat-precision    (Bigfloat -> Exact-Positive-Integer)]
 [bigfloat-signbit      (Bigfloat -> (U 0 1))]
 [bigfloat-exponent     (Bigfloat -> Integer)]
 [bigfloat-significand  (Bigfloat -> Integer)]
 ;; Conversions from Bigfloat
 [bigfloat->sig+exp   (Bigfloat -> (Values Integer Integer))]
 [bigfloat->flonum    (Bigfloat -> Flonum)]
 [bigfloat->integer   (Bigfloat -> Integer)]
 [bigfloat->rational  (Bigfloat -> Exact-Rational)]
 [bigfloat->real      (Bigfloat -> (U Exact-Rational Flonum))]
 [bigfloat->string    (Bigfloat -> String)]
 ;; Conversions to Bigfloat
 [sig+exp->bigfloat   (Integer Integer -> Bigfloat)]
 [flonum->bigfloat    (Flonum -> Bigfloat)]
 [integer->bigfloat   (Integer -> Bigfloat)]
 [rational->bigfloat  (Exact-Rational -> Bigfloat)]
 [real->bigfloat      (Real -> Bigfloat)]
 [string->bigfloat    (String -> (U #f Bigfloat))]
 ;; Main constructor
 [bf  (case-> ((U String Real) -> Bigfloat)
              (Integer Integer -> Bigfloat))]
 ;; Functions that will only be provided wrapped
 [bfadd  (Bigfloat Bigfloat -> Bigfloat)]
 [bfsub  (Bigfloat Bigfloat -> Bigfloat)]
 [bfmul  (Bigfloat Bigfloat -> Bigfloat)]
 [bfdiv  (Bigfloat Bigfloat -> Bigfloat)]
 [bfneg  (Bigfloat -> Bigfloat)]
 [bfsum  ((Listof Bigfloat) -> Bigfloat)]
 [bfmax2  (Bigfloat Bigfloat -> Bigfloat)]
 [bfmin2  (Bigfloat Bigfloat -> Bigfloat)]
 [bf=?    (Bigfloat Bigfloat -> Boolean)]
 [bflt?   (Bigfloat Bigfloat -> Boolean)]
 [bflte?  (Bigfloat Bigfloat -> Boolean)]
 [bfgt?   (Bigfloat Bigfloat -> Boolean)]
 [bfgte?  (Bigfloat Bigfloat -> Boolean)]
 ;; Functions with non-uniform types
 [bffactorial  (Integer -> Bigfloat)]
 [bfbesj  (Integer Bigfloat -> Bigfloat)]
 [bfbesy  (Integer Bigfloat -> Bigfloat)]
 [bfroot  (Bigfloat Integer -> Bigfloat)]
 [bfshift  (Bigfloat Integer -> Bigfloat)]
 [bigfloat->ordinal  (Bigfloat -> Integer)]
 [ordinal->bigfloat  (Integer -> Bigfloat)]
 [bigfloats-between  (Bigfloat Bigfloat -> Integer)]
 [bfstep  (Bigfloat Integer -> Bigfloat)]
 [bfnext  (Bigfloat -> Bigfloat)]
 [bfprev  (Bigfloat -> Bigfloat)]
 [bflog-gamma/sign  (Bigfloat -> (Values Bigfloat (U -1 1)))])

(req/prov-uniform-collection "mpfr.rkt" 1ary-funs (Bigfloat -> Bigfloat))
(req/prov-uniform-collection "mpfr.rkt" 1ary-preds (Bigfloat -> Boolean))
(req/prov-uniform-collection "mpfr.rkt" 1ary2-funs (Bigfloat -> (Values Bigfloat Bigfloat)))
(req/prov-uniform-collection "mpfr.rkt" 2ary-funs (Bigfloat Bigfloat -> Bigfloat))

;; Rackety wrappers

(: bf+ (Bigfloat * -> Bigfloat))
(define (bf+ . xs)
  (cond [(null? xs)  (bf 0)]
        [else
         (define xs1 (cdr xs))
         (cond [(null? xs1)  (car xs)]
               [else
                (define xs2 (cdr xs1))
                (cond [(null? xs2)  (bfadd (car xs) (car xs1))]
                      [else  (bfsum xs)])])]))

(: bf- (Bigfloat Bigfloat * -> Bigfloat))
(define (bf- x . xs)
  (cond [(null? xs)  (bfneg x)]
        [(null? (cdr xs))  (bfsub x (car xs))]
        [else  (bfneg (apply bf+ (bfneg x) xs))]))

(: bf* (Bigfloat * -> Bigfloat))
(define (bf* . xs)
  (cond [(null? xs)  (bf 1)]
        [else  (let loop ([x  (car xs)] [xs  (cdr xs)])
                 (cond [(null? xs)  x]
                       [else  (loop (bfmul x (car xs)) (cdr xs))]))]))

(: bf/ (Bigfloat Bigfloat * -> Bigfloat))
(define (bf/ x . xs)
  (cond [(null? xs)  (bfdiv (bf 1) x)]
        [else  (bfdiv x (apply bf* xs))]))

(: bfmin (Bigfloat * -> Bigfloat))
(define (bfmin . xs)
  (cond [(null? xs)  (bf +inf.0)]
        [else  (foldl bfmin2 (car xs) (cdr xs))]))

(: bfmax (Bigfloat * -> Bigfloat))
(define (bfmax . xs)
  (cond [(null? xs)  (bf -inf.0)]
        [else  (foldl bfmax2 (car xs) (cdr xs))]))

(: fold-binary-pred (All (A) ((A A -> Boolean) A (Listof A) -> Boolean)))
(define (fold-binary-pred pred? x xs)
  (let loop ([x x] [xs xs])
    (cond [(null? xs)  #t]
          [else  (define fst-xs (car xs))
                 (cond [(pred? x fst-xs)  (loop fst-xs (cdr xs))]
                       [else  #f])])))

(define-syntax-rule (define-nary-pred bfpred? bfpred2?)
  (begin
    (: bfpred? (Bigfloat Bigfloat * -> Boolean))
    (define (bfpred? x . xs) (fold-binary-pred bfpred2? x xs))))

(define-nary-pred bf=  bf=?)
(define-nary-pred bf<  bflt?)
(define-nary-pred bf<= bflte?)
(define-nary-pred bf>  bfgt?)
(define-nary-pred bf>= bfgte?)

(: bfrandom (-> Bigfloat))
(define (bfrandom)
  (define bits (bf-precision))
  (bf (random-bits bits) (- bits)))

(: bigfloat->fl2 (Bigfloat -> (Values Flonum Flonum)))
(define (bigfloat->fl2 x)
  (define x2 (bigfloat->flonum x))
  (cond [(rational? x2)
         (let ([x2  (+ x2 (bigfloat->flonum (bf- x (flonum->bigfloat x2))))])
           (cond [(rational? x2)
                  (values x2 (bigfloat->flonum (bf- x (flonum->bigfloat x2))))]
                 [else
                  (values x2 0.0)]))]
        [else  (values x2 0.0)]))

(: fl2->bigfloat (Flonum Flonum -> Bigfloat))
(define (fl2->bigfloat x2 x1)
  (cond [(fl= x1 0.0)  (bf x2)]
        [else  (bf+ (flonum->bigfloat x1) (flonum->bigfloat x2))]))

(provide
 ;; Parameters
 bf-rounding-mode
 bf-min-precision
 bf-max-precision
 bf-precision
 ;; Type and predicate
 Bigfloat bigfloat?
 bfcanonicalize
 ;; Accessors
 bigfloat-precision
 bigfloat-signbit
 bigfloat-exponent
 bigfloat->sig+exp
 bigfloat-significand
 ;; Conversions
 sig+exp->bigfloat
 flonum->bigfloat
 integer->bigfloat
 rational->bigfloat
 real->bigfloat
 fl2->bigfloat
 string->bigfloat
 bigfloat->flonum
 bigfloat->integer
 bigfloat->rational
 bigfloat->real
 bigfloat->fl2
 bigfloat->string
 bigfloat->sig+exp
 ;; Main constructor
 bf
 ;; Functions with non-uniform types
 bffactorial
 bfbesj
 bfbesy
 bfshift
 bflog-gamma/sign
 bfrandom
 bfroot
 bigfloat->ordinal
 ordinal->bigfloat
 bigfloats-between
 bfstep
 bfnext
 bfprev
 ;; Function wrappers with Rackety APIs
 bf+
 bf-
 bf*
 bf/
 bfmin
 bfmax
 bf=
 bf<
 bf<=
 bf>
 bf>=)
