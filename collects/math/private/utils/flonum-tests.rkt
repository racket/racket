#lang typed/racket/base

(require racket/math
         racket/list
         "../base/base-random.rkt"
         "../flonum/expansion/expansion-base.rkt"
         "../flonum/expansion/expansion-exp.rkt"
         "../flonum/expansion/expansion-log.rkt"
         "../flonum/flonum-functions.rkt"
         "../flonum/flonum-constants.rkt"
         "../flonum/flonum-bits.rkt"
         "../flonum/flonum-error.rkt"
         "../flonum/flonum-log.rkt"
         "../bigfloat/bigfloat-log-arithmetic.rkt"
         "../distributions/dist-struct.rkt"
         "../distributions/geometric-dist.rkt"
         "../../bigfloat.rkt")

(provide
 print-fp-test-progress?
 ;; Unary flonum tests
 test-flabs
 test-flsqrt
 test-fllog
 test-flexp
 test-flsin
 test-flcos
 test-fltan
 test-flasin
 test-flacos
 test-flatan
 test-fllog2
 ;; Binary flonum tests
 test-fl+
 test-fl*
 test-fl-
 test-fl/
 test-flexpt
 test-fllogb
 ;; Unary flop/error tests
 test-flsqr/error
 test-flsqrt/error
 test-flexp/error
 ;; Binary flop/error tests
 test-fl+/error
 test-fl-/error
 test-fl*/error
 test-fl//error
 ;; fl2 conversion test
 test-fl2
 ;; Unary fl2 tests
 test-fl2abs
 test-fl2sqr
 test-fl2sqrt
 test-fl2exp
 test-fl2expm1
 test-fl2log
 test-fl2log1p
 ;; Binary fl2 tests
 test-fl2+
 test-fl2-
 test-fl2*
 test-fl2/
 ;; Comprehensive test
 test-floating-point)

;; Allowable error for different kinds of functions, in ulps
(define flonum-fun-ulps 0.5)
(define fllog2-ulps 1.0)
(define fllogb-ulps 2.5)
(define flonum/error-fun-ulps 0.5)
(define flexp/error-fun-ulps 3.0)
(define fl2-conversion-ulps 0.5)
(define unary-fl2-fun-ulps 1.0)
(define binary-fl2-fun-ulps 8.0)
(define fl2exp-fun-ulps 3.0)
(define fl2log-fun-ulps 2.0)

(: current-max-ulp-error (Parameterof Nonnegative-Flonum))
(define current-max-ulp-error (make-parameter 0.0))

;; ===================================================================================================
;; Helpers

(: different-zero? (Real Real -> Boolean))
(define (different-zero? x y)
  (or (and (eqv? x -0.0) (eqv? y 0.0))
      (and (eqv? x 0.0) (eqv? y -0.0))))

(: fl2->real* (Flonum Flonum -> Real))
;; Like `fl2->real', but returns signed flonum zeros
(define (fl2->real* x2 x1)
  (define x.0 (fl+ x2 x1))
  (cond [(zero? x.0)  x2]
        [else  (fl2->real x2 x1)]))

(: bigfloat->real* (Bigfloat -> Real))
;; Like `bigfloat->real*', but returns a signed infinity or a signed flonum zero if conversion would
;; overflow or underflow a flonum
(define (bigfloat->real* x)
  (define x.0 (bigfloat->flonum x))
  (cond [(fl= x.0 0.0)  x.0]
        [(flrational? x.0)  (bigfloat->real x)]
        [else  x.0]))

(: filter/ulp-error (All (A B) ((Listof (List A (U B Flonum)))
                                Flonum -> (Listof (List A (U B Flonum))))))
(define (filter/ulp-error xes ulps)
  (filter (λ: ([xe : (List A (U B Flonum))])
            (define e (second xe))
            (or (not (flonum? e)) (e . fl> . ulps)))
          xes))

(: print-fp-test-progress? (Parameterof Boolean))
(define print-fp-test-progress? (make-parameter #t))

(define progress-chunk-size 200)
(define progress-superchunk-chunks 5)

(: maybe-print-progress (Symbol Integer Natural -> Void))
(define (maybe-print-progress name i m)
  (when (and (print-fp-test-progress?) (i . > . 0) (i . <= . m))
    (let* ([flush?  (cond [(= i 1)  (printf "~a: " name)]
                          [else  #f])]
           [flush?  (cond [(= 0 (modulo i progress-chunk-size))
                           (cond [(= 0 (modulo i (* progress-superchunk-chunks
                                                    progress-chunk-size)))
                                  (printf "* ~a " i)]
                                 [else  (printf "*")])]
                          [else  flush?])]
           [flush?  (cond [(= i m)  (printf "* ~a~n" m)]
                          [else  flush?])])
      (when flush? (flush-output)))))

;; ===================================================================================================
;; Test case generation

;; Deteriministic test cases

(define standard-xs
  (list
   ;; Test the sign of the return value of `flexpt'
   -1001.0 -10.0 -0.1 +0.1 +10.0 +1001.0
   ;; Test squaring
   (- (flsqrt +min.0)) (flsqrt +min.0)
   (- (flsqrt +max-subnormal.0)) (flsqrt +max-subnormal.0)
   (- (flsqrt +max-subnormal.hi)) (flsqrt +max-subnormal.hi)
   (- (flsqrt +max.0)) (flsqrt +max.0)
   ;; Test exp limits
   (fllog +min.0) (fllog +max-subnormal.0) (fllog +max-subnormal.hi) (fllog +max.0)
   ;; Standard special values
   -inf.0 -max.0 -1.0 -max-subnormal.hi -max-subnormal.0 -min.0 -0.0
   +inf.0 +max.0 +1.0 +max-subnormal.hi +max-subnormal.0 +min.0 +0.0
   +nan.0))

(define standard-rs
  (append standard-xs
          (list +10 +1 +1/7 +1/10 +1/13
                -10 -1 -1/7 -1/10 -1/13
                0)))

(: product (All (A B) ((Listof A) (Listof B) -> (Values (Listof A) (Listof B)))))
(define (product as bs)
  (define abs
    (append*
     (for/list: : (Listof (Listof (Pair A B))) ([a  (in-list as)])
       (for/list: : (Listof (Pair A B)) ([b  (in-list bs)])
         (cons a b)))))
  (values (map (inst car A B) abs) (map (inst cdr A B) abs)))

;; Random test cases

(define min-subnormal-ord (flonum->ordinal -max-subnormal.0))
(define max-subnormal-ord (+ 1 (flonum->ordinal +max-subnormal.0)))

(define min-fl2-subnormal-ord (flonum->ordinal -max-subnormal.hi))
(define max-fl2-subnormal-ord (+ 1 (flonum->ordinal +max-subnormal.hi)))

(: sample-flonum (case-> (Integer -> (Listof Flonum))
                         (Integer Flonum Flonum -> (Listof Flonum))))
(define (sample-flonum n [mn -inf.0] [mx +inf.0])
  (define min-ord (flonum->ordinal mn))
  (define max-ord (+ 1 (flonum->ordinal mx)))
  (let ([min-subnormal-ord  (max min-ord min-subnormal-ord)]
        [max-subnormal-ord  (min max-ord max-subnormal-ord)]
        [min-fl2-subnormal-ord  (max min-ord min-fl2-subnormal-ord)]
        [max-fl2-subnormal-ord  (min max-ord max-fl2-subnormal-ord)])
    (build-list
     n (λ (_)
         (define r (random))
         (ordinal->flonum
          (cond [(and (min-subnormal-ord . < . max-subnormal-ord) (r . < . 0.1))
                 (random-integer min-subnormal-ord max-subnormal-ord)]
                [(and (min-fl2-subnormal-ord . < . max-fl2-subnormal-ord) (r . < . 0.2))
                 (random-integer min-subnormal-ord max-subnormal-ord)]
                [else
                 (random-integer min-ord max-ord)]))))))

(define denom-dist (geometric-dist 1e-32))

(: sample-rational (Integer -> (Listof Exact-Rational)))
(define (sample-rational n)
  (map (λ: ([f1 : Flonum] [d : Integer])
         (+ (inexact->exact f1)
            (* (if ((random) . > . 0.5) -1 1)
               (/ (random-natural (+ 1 d)) d)
               (expt 2 (- (exact-round (/ (fllog (flabs f1)) (fllog 2.0))) 52)))))
       (sample-flonum n)
       (map (λ: ([x : Flonum]) (+ 1 (exact-floor x))) (sample denom-dist n))))

;; ===================================================================================================
;; Flonum functions

(define-type Flonum-Error (U Flonum (List Symbol Flonum Real)))
(define-type Unary-Flonum-Failure (List (List Symbol Flonum) Flonum-Error))
(define-type Binary-Flonum-Failure (List (List Symbol Flonum Flonum) Flonum-Error))

(: flonum-error (Flonum Bigfloat -> Flonum-Error))
(define (flonum-error z z0.bf)
  (define z0 (bigfloat->real* z0.bf))
  (cond [(different-zero? z z0)  (list 'different-zero? z z0)]
        [else  (flulp-error z z0)]))

(: unary-flonum-fun-error ((Flonum -> Flonum) (Bigfloat -> Bigfloat) Flonum -> Flonum-Error))
(define (unary-flonum-fun-error f g x)
  (flonum-error (f x) (parameterize ([bf-precision 53])
                        (g (bf x)))))

(: test-unary-flonum-fun
   (Symbol (Flonum -> Flonum) (Bigfloat -> Bigfloat) Integer Flonum Flonum
           -> (Listof Unary-Flonum-Failure)))
(define (test-unary-flonum-fun name f g n mn mx)
  (define xs (append standard-xs (sample-flonum n mn mx)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Unary-Flonum-Failure) ([x  (in-list xs)]
                                               [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x) (unary-flonum-fun-error f g x)))
   (current-max-ulp-error)))

(: binary-flonum-fun-error
   ((Flonum Flonum -> Flonum) (Bigfloat Bigfloat -> Bigfloat) Flonum Flonum -> Flonum-Error))
(define (binary-flonum-fun-error f g x y)
  (flonum-error (f x y) (parameterize ([bf-precision 53])
                          (g (bf x) (bf y)))))

(: test-binary-flonum-fun
   (Symbol (Flonum Flonum -> Flonum) (Bigfloat Bigfloat -> Bigfloat) Integer
           -> (Listof Binary-Flonum-Failure)))
(define (test-binary-flonum-fun name f g n)
  (define-values (pre-xs pre-ys) (product standard-xs standard-xs))
  (define xs (append pre-xs (sample-flonum n)))
  (define ys (append pre-ys (sample-flonum n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Binary-Flonum-Failure) ([x  (in-list xs)]
                                                [y  (in-list ys)]
                                                [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x y) (binary-flonum-fun-error f g x y)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; fl2 conversion

(define-type Fl2-Error (U Flonum (List Symbol Flonum Real)))
(define-type Fl2-Failure (List (List 'fl2 Real) Fl2-Error))

(: fl2-error (Flonum Flonum Real -> Fl2-Error))
(define (fl2-error x2 x1 x)
  (cond [(not (fl2? x2 x1))  (list 'not-fl2? x2 x1)]
        [(different-zero? x2 x)  (list 'different-zero? x2 x)]
        [else  (fl2ulp-error x2 x1 x)]))

(: fl2-conversion-error (Real -> Fl2-Error))
(define (fl2-conversion-error x)
  (define-values (x2 x1) (fl2 x))
  (fl2-error x2 x1 x))

(: test-fl2-conversion (Integer -> (Listof Fl2-Failure)))
(define (test-fl2-conversion n)
  (define xs (append standard-rs (sample-rational n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Fl2-Failure) ([x  (in-list xs)]
                                      [i  (in-naturals 1)])
     (maybe-print-progress 'fl2 i m)
     (list (list 'fl2 x) (fl2-conversion-error x)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; Flonum arithmetic with error

(define-type Unary-Fl/Error-Failure (List (List Symbol Flonum) Fl2-Error))
(define-type Binary-Fl/Error-Failure (List (List Symbol Flonum Flonum) Fl2-Error))

(: unary-flonum/error-fun-error ((Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat) Flonum
                                                                    -> Fl2-Error))
(define (unary-flonum/error-fun-error f g x)
  (define-values (z2 z1) (f x))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf x))))))

(: binary-flonum/error-fun-error ((Flonum Flonum -> (Values Flonum Flonum))
                                  (Bigfloat Bigfloat -> Bigfloat)
                                  Flonum Flonum
                                  -> Fl2-Error))
(define (binary-flonum/error-fun-error f g x y)
  (define-values (z2 z1) (f x y))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf x) (bf y))))))

(: test-unary-flonum/error-fun
   (Symbol (Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat) Integer
           -> (Listof Unary-Fl/Error-Failure)))
(define (test-unary-flonum/error-fun name f g n)
  (define xs (append standard-xs (sample-flonum n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Unary-Fl/Error-Failure) ([x  (in-list xs)]
                                                 [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x) (unary-flonum/error-fun-error f g x)))
   (current-max-ulp-error)))

(: test-binary-flonum/error-fun
   (Symbol (Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat Bigfloat -> Bigfloat) Integer
           -> (Listof Binary-Fl/Error-Failure)))
(define (test-binary-flonum/error-fun name f g n)
  (define-values (pre-xs pre-ys) (product standard-xs standard-xs))
  (define xs (append pre-xs (sample-flonum n)))
  (define ys (append pre-ys (sample-flonum n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Binary-Fl/Error-Failure) ([x  (in-list xs)]
                                                  [y  (in-list ys)]
                                                  [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x y) (binary-flonum/error-fun-error f g x y)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; Flonum expansions

(define-type Unary-Fl2-Failure (List (List Symbol Flonum Flonum) Fl2-Error))
(define-type Binary-Fl2-Failure (List (List Symbol Flonum Flonum Flonum Flonum) Fl2-Error))

(: unary-fl2-fun-error ((Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat)
                                                                  Flonum Flonum -> Fl2-Error))
(define (unary-fl2-fun-error f g x2 x1)
  (define-values (z2 z1) (f x2 x1))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf (fl2->real* x2 x1)))))))

(: test-unary-fl2-fun
   (Symbol (Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat) Integer
           -> (Listof Unary-Fl2-Failure)))
(define (test-unary-fl2-fun name f g n)
  (define xs (append standard-rs (sample-rational n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Unary-Fl2-Failure) ([x  (in-list xs)]
                                            [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (define-values (x2 x1) (fl2 x))
     (list (list name x2 x1) (unary-fl2-fun-error f g x2 x1)))
   (current-max-ulp-error)))

(: binary-fl2-fun-error ((Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))
                         (Bigfloat Bigfloat -> Bigfloat)
                         Flonum Flonum Flonum Flonum
                         -> Fl2-Error))
(define (binary-fl2-fun-error f g x2 x1 y2 y1)
  (define-values (z2 z1) (f x2 x1 y2 y1))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf (fl2->real* x2 x1)) (bf (fl2->real* y2 y1)))))))

(: test-binary-fl2-fun
   (Symbol (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat Bigfloat -> Bigfloat)
           Integer -> (Listof Binary-Fl2-Failure)))
(define (test-binary-fl2-fun name f g n)
  (define-values (pre-xs pre-ys) (product standard-rs standard-rs))
  (define xs (append pre-xs (sample-rational n)))
  (define ys (append pre-ys (sample-rational n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof Binary-Fl2-Failure) ([x  (in-list xs)]
                                             [y  (in-list ys)]
                                             [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (define-values (x2 x1) (fl2 x))
     (define-values (y2 y1) (fl2 y))
     (list (list name x2 x1 y2 y1) (binary-fl2-fun-error f g x2 x1 y2 y1)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; Flonum tests

(define-syntax-rule (define-unary-flonum-test test-name flop bfop mn mx ulps)
  (begin (: test-name (Natural -> (Listof Unary-Flonum-Failure)))
         (define (test-name n)
           (parameterize ([current-max-ulp-error  ulps])
             (test-unary-flonum-fun 'flop flop bfop n mn mx)))))

(define-syntax-rule (define-binary-flonum-test test-name flop bfop ulps)
  (begin (: test-name (Natural -> (Listof Binary-Flonum-Failure)))
         (define (test-name n)
           (parameterize ([current-max-ulp-error  ulps])
             (test-binary-flonum-fun 'flop flop bfop n)))))

(define-unary-flonum-test test-flabs flabs bfabs -inf.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-flsqrt flsqrt bfsqrt 0.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-fllog fllog bflog 0.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-flexp flexp bfexp -746.0 710.0 flonum-fun-ulps)
(define-unary-flonum-test test-flsin flsin bfsin -inf.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-flcos flcos bfcos -inf.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-fltan fltan bftan -inf.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-flasin flasin bfasin -1.0 1.0 flonum-fun-ulps)
(define-unary-flonum-test test-flacos flacos bfacos -1.0 1.0 flonum-fun-ulps)
(define-unary-flonum-test test-flatan flatan bfatan -inf.0 +inf.0 flonum-fun-ulps)
(define-unary-flonum-test test-fllog2 fllog2 bflog2 0.0 +inf.0 fllog2-ulps)
(define-binary-flonum-test test-fl+ fl+ bf+ flonum-fun-ulps)
(define-binary-flonum-test test-fl- fl- bf- flonum-fun-ulps)
(define-binary-flonum-test test-fl* fl* bf* flonum-fun-ulps)
(define-binary-flonum-test test-fl/ fl/ bf/ flonum-fun-ulps)
(define-binary-flonum-test test-flexpt flexpt bfexpt flonum-fun-ulps)
(define-binary-flonum-test test-fllogb fllogb bflogb fllogb-ulps)

;; ===================================================================================================
;; Flonum/error tests

(define-syntax-rule (define-unary-flop/error-test test-name flop bfop ulps)
  (begin (: test-name (Natural -> (Listof Unary-Fl/Error-Failure)))
         (define (test-name n)
           (parameterize ([current-max-ulp-error  ulps])
             (test-unary-flonum/error-fun 'flop flop bfop n)))))

(define-syntax-rule (define-binary-flop/error-test test-name flop bfop ulps)
  (begin (: test-name (Natural -> (Listof Binary-Fl/Error-Failure)))
         (define (test-name n)
           (parameterize ([current-max-ulp-error  ulps])
             (test-binary-flonum/error-fun 'flop flop bfop n)))))

(define-unary-flop/error-test test-flsqr/error flsqr/error bfsqr flonum/error-fun-ulps)
(define-unary-flop/error-test test-flsqrt/error flsqrt/error bfsqrt flonum/error-fun-ulps)
(define-unary-flop/error-test test-flexp/error flexp/error bfexp flexp/error-fun-ulps)
(define-binary-flop/error-test test-fl+/error fl+/error bf+ flonum/error-fun-ulps)
(define-binary-flop/error-test test-fl-/error fl-/error bf- flonum/error-fun-ulps)
(define-binary-flop/error-test test-fl*/error fl*/error bf* flonum/error-fun-ulps)
(define-binary-flop/error-test test-fl//error fl//error bf/ flonum/error-fun-ulps)

;; ===================================================================================================
;; fl2 tests

(: test-fl2 (Natural -> (Listof Fl2-Failure)))
(define (test-fl2 n)
  (parameterize ([current-max-ulp-error  fl2-conversion-ulps])
    (test-fl2-conversion n)))

(define-syntax-rule (define-unary-fl2-test test-name fl2op bfop ulps)
  (begin (: test-name (Natural -> (Listof Unary-Fl2-Failure)))
         (define (test-name n)
           (parameterize ([current-max-ulp-error  ulps])
             (test-unary-fl2-fun 'fl2op fl2op bfop n)))))

(define-syntax-rule (define-binary-fl2-test test-name fl2op bfop ulps)
  (begin (: test-name (Natural -> (Listof Binary-Fl2-Failure)))
         (define (test-name n)
           (parameterize ([current-max-ulp-error  ulps])
             (test-binary-fl2-fun 'fl2op fl2op bfop n)))))

(define-unary-fl2-test test-fl2abs fl2abs bfabs unary-fl2-fun-ulps)
(define-unary-fl2-test test-fl2sqr fl2sqr bfsqr unary-fl2-fun-ulps)
(define-unary-fl2-test test-fl2sqrt fl2sqrt bfsqrt unary-fl2-fun-ulps)
(define-unary-fl2-test test-fl2exp fl2exp bfexp fl2exp-fun-ulps)
(define-unary-fl2-test test-fl2expm1 fl2expm1 bfexpm1 fl2exp-fun-ulps)
(define-unary-fl2-test test-fl2log fl2log bflog fl2log-fun-ulps)
(define-unary-fl2-test test-fl2log1p fl2log1p bflog1p fl2log-fun-ulps)
(define-binary-fl2-test test-fl2+ fl2+ bf+ binary-fl2-fun-ulps)
(define-binary-fl2-test test-fl2- fl2- bf- binary-fl2-fun-ulps)
(define-binary-fl2-test test-fl2* fl2* bf* binary-fl2-fun-ulps)
(define-binary-fl2-test test-fl2/ fl2/ bf/ binary-fl2-fun-ulps)

;; ===================================================================================================
;; Comprehensive test

(: test-floating-point (Natural -> (Listof (U Unary-Flonum-Failure
                                              Binary-Flonum-Failure
                                              Unary-Fl/Error-Failure
                                              Binary-Fl/Error-Failure
                                              Fl2-Failure
                                              Unary-Fl2-Failure
                                              Binary-Fl2-Failure))))
(define (test-floating-point n)
  (append
   ;; Hardware implementation tests
   (test-flabs n)
   (test-fl+ n)
   (test-fl* n)
   (test-fl- n)
   (test-fl/ n)
   (test-flsqrt n)
   (test-fllog n)
   (test-flexp n)
   (test-flexpt n)
   (test-flsin n)
   (test-flcos n)
   (test-fltan n)
   (test-flasin n)
   (test-flacos n)
   (test-flatan n)
   (test-fllog2 n)
   (test-fllogb n)
   ;; Derived tests
   (test-fl+/error n)
   (test-fl-/error n)
   (test-fl*/error n)
   (test-flsqr/error n)
   (test-fl//error n)
   (test-fl2 n)
   (test-fl2abs n)
   (test-fl2+ n)
   (test-fl2- n)
   (test-fl2* n)
   (test-fl2sqr n)
   (test-fl2/ n)
   (test-flsqrt/error n)
   (test-fl2sqrt n)
   (test-flexp/error n)
   (test-fl2exp n)
   (test-fl2expm1 n)
   (test-fl2log n)
   (test-fl2log1p n)
   ))
