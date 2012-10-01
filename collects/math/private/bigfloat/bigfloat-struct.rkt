#lang typed/racket/base

(require (only-in "mpfr.rkt"
                  consts 0ary-funs 1ary-funs 1ary-preds 1ary2-funs 2ary-funs)
         racket/promise
         (for-syntax racket/base racket/syntax syntax/strip-context))

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
 ;; Accessors
 [bigfloat-precision    (Bigfloat -> Integer)]
 [bigfloat-sign         (Bigfloat -> (U 0 1))]
 [bigfloat-exponent     (Bigfloat -> Integer)]
 [bigfloat-sig+exp      (Bigfloat -> (Values Integer Integer))]
 [bigfloat-significand  (Bigfloat -> Integer)]
 ;; Conversion to and from Real
 [bigfloat->flonum    (Bigfloat -> Float)]
 [bigfloat->integer   (Bigfloat -> Integer)]
 [bigfloat->rational  (Bigfloat -> Exact-Rational)]
 [flonum->bigfloat    (Float -> Bigfloat)]
 [integer->bigfloat   (Integer -> Bigfloat)]
 [rational->bigfloat  (Exact-Rational -> Bigfloat)]
 [real->bigfloat      (Real -> Bigfloat)]
 ;; String conversion
 [bigfloat->string  (Bigfloat -> String)]
 [string->bigfloat  (String -> (U #f Bigfloat))]
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
 [bfeqv?  (Bigfloat Bigfloat -> Boolean)]
 [bflt?   (Bigfloat Bigfloat -> Boolean)]
 [bflte?  (Bigfloat Bigfloat -> Boolean)]
 [bfgt?   (Bigfloat Bigfloat -> Boolean)]
 [bfgte?  (Bigfloat Bigfloat -> Boolean)]
 ;; Functions with non-uniform types
 [bffactorial  (Integer -> Bigfloat)]
 [bfjn  (Integer Bigfloat -> Bigfloat)]
 [bfyn  (Integer Bigfloat -> Bigfloat)]
 [bfshift  (Bigfloat Integer -> Bigfloat)]
 [bflog-gamma/sign  (Bigfloat -> (Values Bigfloat (U -1 1)))]
 [bfrandom  (-> Bigfloat)])

;; Rackety wrappers

(: bf+ (Bigfloat * -> Bigfloat))
(define (bf+ . xs)
  (cond [(null? xs)  (force 0.bf)]
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
  (cond [(null? xs)  (force 1.bf)]
        [else  (let loop ([x  (car xs)] [xs  (cdr xs)])
                 (cond [(null? xs)  x]
                       [else  (loop (bfmul x (car xs)) (cdr xs))]))]))

(: bf/ (Bigfloat Bigfloat * -> Bigfloat))
(define (bf/ x . xs)
  (cond [(null? xs)  (bfdiv (force 1.bf) x)]
        [else  (bfdiv x (apply bf* xs))]))

(: bfmin (Bigfloat * -> Bigfloat))
(define (bfmin . xs)
  (cond [(null? xs)  (force +inf.bf)]
        [else  (foldl bfmin2 (car xs) (cdr xs))]))

(: bfmax (Bigfloat * -> Bigfloat))
(define (bfmax . xs)
  (cond [(null? xs)  (force -inf.bf)]
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

(define-nary-pred bf=  bfeqv?)
(define-nary-pred bf<  bflt?)
(define-nary-pred bf<= bflte?)
(define-nary-pred bf>  bfgt?)
(define-nary-pred bf>= bfgte?)

(provide
 ;; Parameters
 bf-rounding-mode
 bf-min-precision
 bf-max-precision
 bf-precision
 ;; Type and predicate
 Bigfloat bigfloat?
 ;; Accessors
 bigfloat-precision
 bigfloat-sign
 bigfloat-exponent
 bigfloat-sig+exp
 bigfloat-significand
 ;; Conversion to and from Real
 flonum->bigfloat
 integer->bigfloat
 rational->bigfloat
 real->bigfloat
 bigfloat->flonum
 bigfloat->integer
 bigfloat->rational
 ;; String conversion
 bigfloat->string
 string->bigfloat
 ;; Main constructor
 bf
 ;; Functions with non-uniform types
 bffactorial
 bfjn
 bfyn
 bfshift
 bflog-gamma/sign
 bfrandom
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

(define-syntax (req/prov-uniform-collection stx)
  (syntax-case stx ()
    [(_ module collection type)
     (with-syntax ([require-it-name  (datum->syntax stx (gensym 'require-it))])
       (syntax/loc stx
         (begin
           (define-syntax (require-it-name stx1)
             (syntax-case stx1 ()
               [(require-it-name)
                (with-syntax ([(obj (... ...))  (replace-context #'require-it-name collection)])
                  #'(begin (require/typed module [obj  type] (... ...))
                           (provide obj (... ...))))]))
           (require-it-name))))]))

(req/prov-uniform-collection "mpfr.rkt" 1ary-funs (Bigfloat -> Bigfloat))
(req/prov-uniform-collection "mpfr.rkt" 1ary-preds (Bigfloat -> Boolean))
(req/prov-uniform-collection "mpfr.rkt" 1ary2-funs (Bigfloat -> (Values Bigfloat Bigfloat)))
(req/prov-uniform-collection "mpfr.rkt" 2ary-funs (Bigfloat Bigfloat -> Bigfloat))

(define-syntax consts-syntax consts)
(define-syntax 0ary-syntax 0ary-funs)

(define-syntax (req/prov-constants stx)
  (syntax-case stx ()
    [(_ module collection type force)
     (with-syntax* ([(macro-name)  (generate-temporaries #'(collection))]
                    [(obj ...)  (replace-context stx (syntax-local-value #'collection))]
                    [(name ...)  (generate-temporaries #'(obj ...))])
       #'(begin
           (require/typed module [obj  type] ...)
           (define-syntax (name stx2)
             (syntax-case stx2 ()
               [(_ args (... ...))
                (syntax/loc stx2 ((force obj) args (... ...)))]
               [_
                (syntax/loc stx2 (force obj))]))
           ...
           (provide (rename-out [name obj] ...))))]))

(req/prov-constants "mpfr.rkt" consts-syntax (Promise Bigfloat) force)
(req/prov-constants "mpfr.rkt" 0ary-syntax (-> Bigfloat) (λ (f) (f)))
