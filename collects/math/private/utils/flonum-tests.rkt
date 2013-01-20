#lang typed/racket/base

(require racket/math
         racket/flonum
         racket/list
         typed/rackunit
         "../base/base-random.rkt"
         "../flonum/expansion/expansion-base.rkt"
         "../flonum/expansion/expansion-exp.rkt"
         "../flonum/expansion/expansion-log.rkt"
         "../flonum/flonum-functions.rkt"
         "../flonum/flonum-constants.rkt"
         "../flonum/flonum-bits.rkt"
         "../flonum/flonum-error.rkt"
         "../distributions/dist-struct.rkt"
         "../distributions/geometric-dist.rkt"
         "../../bigfloat.rkt")

(provide print-test-progress?
         test-fpu-arith
         test-fpu-trig
         test-fpu-non-trig
         test-fpu-arith/error
         test-fpu-arith/fl2
         test-fpu-non-trig/fl2
         test-fpu)

;; Allowable error for different kinds of functions, in ulps
(define flonum-fun-ulps 0.5)
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

(: print-test-progress? (Parameterof Boolean))
(define print-test-progress? (make-parameter #t))

(define progress-chunk-size 200)
(define progress-superchunk-chunks 5)

(: maybe-print-progress (Symbol Integer Natural -> Void))
(define (maybe-print-progress name i m)
  (when (and (print-test-progress?) (i . > . 0) (i . <= . m))
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

(: flonum-error (Flonum Bigfloat -> Any))
(define (flonum-error z z0.bf)
  (define z0 (bigfloat->real* z0.bf))
  (cond [(different-zero? z z0)  (list 'different-zero? z z0)]
        [else  (flulp-error z z0)]))

(: unary-flonum-fun-error ((Flonum -> Flonum) (Bigfloat -> Bigfloat) Flonum -> Any))
(define (unary-flonum-fun-error f g x)
  (flonum-error (f x) (parameterize ([bf-precision 53])
                        (g (bf x)))))

(: test-unary-flonum-fun
   (Symbol (Flonum -> Flonum) (Bigfloat -> Bigfloat) Integer Flonum Flonum
           -> (Listof (List (List Symbol Flonum) Any))))
(define (test-unary-flonum-fun name f g n mn mx)
  (define xs (append standard-xs (sample-flonum n mn mx)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List Symbol Flonum) Any)) ([x  (in-list xs)]
                                                          [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x) (unary-flonum-fun-error f g x)))
   (current-max-ulp-error)))

(: binary-flonum-fun-error
   ((Flonum Flonum -> Flonum) (Bigfloat Bigfloat -> Bigfloat) Flonum Flonum -> Any))
(define (binary-flonum-fun-error f g x y)
  (flonum-error (f x y) (parameterize ([bf-precision 53])
                          (g (bf x) (bf y)))))

(: test-binary-flonum-fun
   (Symbol (Flonum Flonum -> Flonum) (Bigfloat Bigfloat -> Bigfloat) Integer
           -> (Listof (List (List Symbol Flonum Flonum) Any))))
(define (test-binary-flonum-fun name f g n)
  (define-values (pre-xs pre-ys) (product standard-xs standard-xs))
  (define xs (append pre-xs (sample-flonum n)))
  (define ys (append pre-ys (sample-flonum n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List Symbol Flonum Flonum) Any)) ([x  (in-list xs)]
                                                                 [y  (in-list ys)]
                                                                 [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x y) (binary-flonum-fun-error f g x y)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; fl2 conversion

(: fl2-error (Flonum Flonum Real -> Any))
(define (fl2-error x2 x1 x)
  (cond [(not (fl2? x2 x1))  (list 'not-fl2? x2 x1)]
        [(different-zero? x2 x)  (list 'different-zero? x2 x)]
        [else  (fl2ulp-error x2 x1 x)]))

(: fl2-conversion-error (Real -> Any))
(define (fl2-conversion-error x)
  (define-values (x2 x1) (fl2 x))
  (fl2-error x2 x1 x))

(: test-fl2-conversion (Integer -> (Listof (List (List 'fl2 Real) Any))))
(define (test-fl2-conversion n)
  (define xs (append standard-rs (sample-rational n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List 'fl2 Real) Any)) ([x  (in-list xs)]
                                                      [i  (in-naturals 1)])
     (maybe-print-progress 'fl2 i m)
     (list (list 'fl2 x) (fl2-conversion-error x)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; Flonum arithmetic with error

(: unary-flonum/error-fun-error ((Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat) Flonum
                                                                    -> Any))
(define (unary-flonum/error-fun-error f g x)
  (define-values (z2 z1) (f x))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf x))))))

(: binary-flonum/error-fun-error ((Flonum Flonum -> (Values Flonum Flonum))
                                  (Bigfloat Bigfloat -> Bigfloat)
                                  Flonum Flonum
                                  -> Any))
(define (binary-flonum/error-fun-error f g x y)
  (define-values (z2 z1) (f x y))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf x) (bf y))))))

(: test-unary-flonum/error-fun
   (Symbol (Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat) Integer
           -> (Listof (List (List Symbol Flonum) Any))))
(define (test-unary-flonum/error-fun name f g n)
  (define xs (append standard-xs (sample-flonum n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List Symbol Flonum) Any)) ([x  (in-list xs)]
                                                          [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x) (unary-flonum/error-fun-error f g x)))
   (current-max-ulp-error)))

(: test-binary-flonum/error-fun
   (Symbol (Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat Bigfloat -> Bigfloat) Integer
           -> (Listof (List (List Symbol Flonum Flonum) Any))))
(define (test-binary-flonum/error-fun name f g n)
  (define-values (pre-xs pre-ys) (product standard-xs standard-xs))
  (define xs (append pre-xs (sample-flonum n)))
  (define ys (append pre-ys (sample-flonum n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List Symbol Flonum Flonum) Any)) ([x  (in-list xs)]
                                                                 [y  (in-list ys)]
                                                                 [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (list (list name x y) (binary-flonum/error-fun-error f g x y)))
   (current-max-ulp-error)))

;; ===================================================================================================
;; Flonum expansions

(: unary-fl2-fun-error ((Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat)
                                                                  Flonum Flonum -> Any))
(define (unary-fl2-fun-error f g x2 x1)
  (define-values (z2 z1) (f x2 x1))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf (fl2->real* x2 x1)))))))

(: test-unary-fl2-fun
   (Symbol (Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat -> Bigfloat) Integer
           -> (Listof (List (List Symbol Flonum Flonum) Any))))
(define (test-unary-fl2-fun name f g n)
  (define xs (append standard-rs (sample-rational n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List Symbol Flonum Flonum) Any)) ([x  (in-list xs)]
                                                                 [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (define-values (x2 x1) (fl2 x))
     (list (list name x2 x1) (unary-fl2-fun-error f g x2 x1)))
   (current-max-ulp-error)))

(: binary-fl2-fun-error ((Flonum Flonum Flonum Flonum -> (Values Flonum Flonum))
                         (Bigfloat Bigfloat -> Bigfloat)
                         Flonum Flonum Flonum Flonum
                         -> Any))
(define (binary-fl2-fun-error f g x2 x1 y2 y1)
  (define-values (z2 z1) (f x2 x1 y2 y1))
  (fl2-error z2 z1 (parameterize ([bf-precision 256])
                     (bigfloat->real* (g (bf (fl2->real* x2 x1)) (bf (fl2->real* y2 y1)))))))

(: test-binary-fl2-fun
   (Symbol (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)) (Bigfloat Bigfloat -> Bigfloat)
           Integer -> (Listof (List (List Symbol Flonum Flonum Flonum Flonum) Any))))
(define (test-binary-fl2-fun name f g n)
  (define-values (pre-xs pre-ys) (product standard-rs standard-rs))
  (define xs (append pre-xs (sample-rational n)))
  (define ys (append pre-ys (sample-rational n)))
  (define m (length xs))
  (filter/ulp-error
   (for/list: : (Listof (List (List Symbol Flonum Flonum Flonum Flonum) Any)
                        ) ([x  (in-list xs)]
                           [y  (in-list ys)]
                           [i  (in-naturals 1)])
     (maybe-print-progress name i m)
     (define-values (x2 x1) (fl2 x))
     (define-values (y2 y1) (fl2 y))
     (list (list name x2 x1 y2 y1) (binary-fl2-fun-error f g x2 x1 y2 y1)))
   (current-max-ulp-error)))

;; ===================================================================================================

(: test-fpu-arith (Natural -> Any))
(define (test-fpu-arith n)
  (parameterize ([current-max-ulp-error  flonum-fun-ulps])
    (check-equal? (test-unary-flonum-fun 'flabs flabs bfabs n -inf.0 +inf.0)
                  '())
    (check-equal? (test-binary-flonum-fun 'fl+ fl+ bf+ n)
                  '())
    (check-equal? (test-binary-flonum-fun 'fl- fl- bf- n)
                  '())
    (check-equal? (test-binary-flonum-fun 'fl* fl* bf* n)
                  '())
    (check-equal? (test-binary-flonum-fun 'fl/ fl/ bf/ n)
                  '())))

(: test-fpu-trig (Natural -> Any))
(define (test-fpu-trig n)
  (parameterize ([current-max-ulp-error  flonum-fun-ulps])
    (check-equal? (test-unary-flonum-fun 'flsin flsin bfsin n -inf.0 +inf.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'flcos flcos bfcos n -inf.0 +inf.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'fltan fltan bftan n -inf.0 +inf.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'flasin flasin bfasin n -1.0 1.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'flacos flacos bfacos n -1.0 1.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'flatan flatan bfatan n -inf.0 +inf.0)
                  '())))

(: test-fpu-non-trig (Natural -> Any))
(define (test-fpu-non-trig n)
  (parameterize ([current-max-ulp-error  flonum-fun-ulps])
    (check-equal? (test-unary-flonum-fun 'flsqrt flsqrt bfsqrt n 0.0 +inf.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'fllog fllog bflog n 0.0 +inf.0)
                  '())
    (check-equal? (test-unary-flonum-fun 'flexp flexp bfexp n -746.0 710.0)
                  '())
    (check-equal? (test-binary-flonum-fun 'flexpt flexpt bfexpt n)
                  '())))

(: test-fpu-arith/error (Natural -> Any))
(define (test-fpu-arith/error n)
  (parameterize ([current-max-ulp-error  flonum/error-fun-ulps])
    (check-equal? (test-binary-flonum/error-fun 'fl+/error fl+/error bf+ n)
                  '())
    (check-equal? (test-binary-flonum/error-fun 'fl-/error fl-/error bf- n)
                  '())
    (check-equal? (test-binary-flonum/error-fun 'fl*/error fl*/error bf* n)
                  '())
    (check-equal? (test-unary-flonum/error-fun 'flsqr/error flsqr/error bfsqr n)
                  '())
    (check-equal? (test-binary-flonum/error-fun 'fl//error fl//error bf/ n)
                  '())))

(: test-fpu-non-trig/error (Natural -> Any))
(define (test-fpu-non-trig/error n)
  (parameterize ([current-max-ulp-error  flexp/error-fun-ulps])
    (check-equal? (test-unary-flonum/error-fun 'flexp/error flexp/error bfexp n)
                  '())))

(: test-fpu-arith/fl2 (Natural -> Any))
(define (test-fpu-arith/fl2 n)
  (parameterize ([current-max-ulp-error  fl2-conversion-ulps])
    (check-equal? (test-fl2-conversion n)
                  '()))
  (parameterize ([current-max-ulp-error  unary-fl2-fun-ulps])
    (check-equal? (test-unary-fl2-fun 'fl2abs fl2abs bfabs n)
                  '())
    (check-equal? (test-unary-fl2-fun 'fl2sqr fl2sqr bfsqr n)
                  '()))
  (parameterize ([current-max-ulp-error  binary-fl2-fun-ulps])
    (check-equal? (test-binary-fl2-fun 'fl2+ fl2+ bf+ n)
                  '())
    (check-equal? (test-binary-fl2-fun 'fl2- fl2- bf- n)
                  '())
    (check-equal? (test-binary-fl2-fun 'fl2* fl2* bf* n)
                  '())
    (check-equal? (test-binary-fl2-fun 'fl2/ fl2/ bf/ n)
                  '())))

(: test-fpu-non-trig/fl2 (Natural -> Any))
(define (test-fpu-non-trig/fl2 n)
  (parameterize ([current-max-ulp-error  unary-fl2-fun-ulps])
    (check-equal? (test-unary-fl2-fun 'fl2sqrt fl2sqrt bfsqrt n)
                  '()))
  (parameterize ([current-max-ulp-error  fl2exp-fun-ulps])
    (check-equal? (test-unary-fl2-fun 'fl2exp fl2exp bfexp n)
                  '()))
  (parameterize ([current-max-ulp-error  fl2exp-fun-ulps])
    (check-equal? (test-unary-fl2-fun 'fl2expm1 fl2expm1 bfexpm1 n)
                  '()))
  (parameterize ([current-max-ulp-error  fl2log-fun-ulps])
    (check-equal? (test-unary-fl2-fun 'fl2log fl2log bflog n)
                  '()))
  (parameterize ([current-max-ulp-error  fl2log-fun-ulps])
    (check-equal? (test-unary-fl2-fun 'fl2log1p fl2log1p bflog1p n)
                  '()))
  )

(: test-fpu (Natural -> Any))
(define (test-fpu n)
  (test-fpu-arith n)
  (test-fpu-trig n)
  (test-fpu-non-trig n)
  (test-fpu-arith/error n)
  (test-fpu-non-trig/error n)
  (test-fpu-arith/fl2 n)
  (test-fpu-non-trig/fl2 n))

(for*: ([x2  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)]
        [x1  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
  (define n
    (count (λ: ([b : Boolean]) b)
           (map (λ: ([f : (Flonum Flonum -> Boolean)])
                  (f x2 x1))
                (list fl2rational? fl2infinite? fl2nan?))))
  (unless (= n 1) (printf "x2 = ~v  x1 = ~v~n" x2 x1)))

#|
Tests to add

(for*: ([x2  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)]
        [x1  (list -inf.0 -max.0 -1.0 -min.0 -0.0 0.0 +min.0 +1.0 +max.0 +inf.0 +nan.0)])
  (define n
    (count (λ: ([b : Boolean]) b)
           (map (λ: ([f : (Flonum Flonum -> Boolean)])
                  (f x2 x1))
                (list fl2rational? fl2infinite? fl2nan?))))
  (unless (= n 1) (printf "x2 = ~v  x1 = ~v~n" x2 x1)))

fl2=
fl2>
fl2<
fl2>=
fl2<=

(fl2step x2 x1 n/2) twice = (fl2step x2 x1 n)

|#

(check-true (let-values ([(y2 y1)  (fl+/error +max.hi +max.lo)])
              (fl2= y2 y1 +max.hi +max.lo)))

(check-true (let*-values ([(y2 y1)  (fl2next +max.hi +max.lo)])
              (fl2infinite? y2 y1)))
