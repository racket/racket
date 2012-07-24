#lang typed/racket/base

(require (only-in "private/mpfr.rkt"
                  consts 0ary-funs 1ary-funs 1ary-preds 1ary2-funs 2ary-funs 2ary-preds)
         racket/promise
         (for-syntax racket/base racket/syntax syntax/strip-context))

(define-type Rounding-Mode (U 'nearest 'zero 'up 'down))
(define-type Print-Scientific (U 'always 'never 'shorter))

(require/typed
 "private/mpfr.rkt"
 ;; Parameters
 [bf-rounding-mode  (Parameterof Rounding-Mode)]
 [bf-min-precision  Exact-Positive-Integer]
 [bf-max-precision  Exact-Positive-Integer]
 [bf-precision  (Parameterof Integer)]
 [bf-scientific  (Parameterof Print-Scientific)]
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
 ;; Functions with non-uniform types
 [bfjn  (Integer Bigfloat -> Bigfloat)]
 [bfyn  (Integer Bigfloat -> Bigfloat)]
 [bfshift  (Bigfloat Integer -> Bigfloat)]
 [bflog-gamma/sign  (Bigfloat -> (Values Bigfloat (U -1 1)))])

(provide
 ;; Parameters
 bf-rounding-mode
 bf-min-precision
 bf-max-precision
 bf-precision
 bf-scientific
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
 bfjn
 bfyn
 bfshift
 bflog-gamma/sign)

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

(req/prov-uniform-collection "private/mpfr.rkt" 1ary-funs (Bigfloat -> Bigfloat))
(req/prov-uniform-collection "private/mpfr.rkt" 1ary-preds (Bigfloat -> Boolean))
(req/prov-uniform-collection "private/mpfr.rkt" 1ary2-funs (Bigfloat -> (Values Bigfloat Bigfloat)))
(req/prov-uniform-collection "private/mpfr.rkt" 2ary-funs (Bigfloat Bigfloat -> Bigfloat))
(req/prov-uniform-collection "private/mpfr.rkt" 2ary-preds (Bigfloat Bigfloat -> Boolean))

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

(req/prov-constants "private/mpfr.rkt" consts-syntax (Promise Bigfloat) force)
(req/prov-constants "private/mpfr.rkt" 0ary-syntax (-> Bigfloat) (λ (f) (f)))
