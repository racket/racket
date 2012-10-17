#lang typed/racket/base

(require racket/flonum
         "../flonum/flonum-sum.rkt"
         "../flonum/flonum-functions.rkt"
         "../flonum/flonum-more-functions.rkt")

(provide phi.0
         euler.0
         gamma.0
         catalan.0
         power-of-two?
         absolute-error
         relative-error
         sum
         sinh cosh tanh
         asinh acosh atanh)

(define phi.0 (fl #e1.61803398874989484820458683436563811772))
(define euler.0 (fl #e2.718281828459045235360287471352662497759))
(define gamma.0 (fl #e0.5772156649015328606065120900824024310432))
(define catalan.0 (fl #e0.9159655941772190150546035149323841107734))

;; Returns #t if x is an integer power of 2
(: power-of-two? (Real -> Boolean))
(define (power-of-two? x)
  (cond [(not (positive? x))  #f]
        [(flonum? x)  (fl= x (flexpt 2.0 (flround (fl/ (fllog x) (fllog 2.0)))))]
        [(single-flonum? x)  (power-of-two? (fl x))]
        [(integer? x)  (= x (expt 2 (- (integer-length x) 1)))]
        [else  (and (= 1 (numerator x))
                    (power-of-two? (denominator x)))]))

(: fix-exact-return (Real Real Real -> Real))
(define (fix-exact-return x r e)
  (cond [(or (single-flonum? x) (single-flonum? r))  (real->single-flonum e)]
        [(or (flonum? x) (flonum? r))  (fl e)]
        [else  e]))

(: absolute-error (Real Real -> Real))
(define (absolute-error x r)
  (fix-exact-return
   x r (cond [(eqv? x r)  0]
             [(and (rational? x) (rational? r))
              (abs (- (inexact->exact x) (inexact->exact r)))]
             [else  +inf.0])))

(: relative-error (Real Real -> Real))
(define (relative-error x r)
  (fix-exact-return
   x r (cond [(eqv? x r)  0]
             [(and (zero? x) (zero? r))  0]
             [(zero? r)  +inf.0]
             [(and (rational? x) (rational? r))
              (define exact-r (inexact->exact r))
              (abs (/ (- (inexact->exact x) exact-r) exact-r))]
             [else  +inf.0])))

(: sum ((Listof Real) -> Real))
(define (sum xs)
  (let loop ([xs xs]
             [#{r : Exact-Rational}  0]
             [#{fs : (Listof Flonum)}  null])
    (cond [(null? xs)
           (cond [(null? fs)  r]
                 [(zero? r)  (flsum fs)]
                 [else  (fl (+ r (inexact->exact (flsum fs))))])]
          [else
           (let ([x  (car xs)]
                 [xs  (cdr xs)])
             (cond [(double-flonum? x)  (loop xs r (cons x fs))]
                   [(single-flonum? x)  (loop xs r (cons (fl x) fs))]
                   [else  (loop xs (+ x r) fs)]))])))

;; ===================================================================================================
;; Hyperbolic

(: sinh (case-> (Zero -> Zero)
                (Float -> Float)
                (Real -> Real)
                (Float-Complex -> Float-Complex)
                (Number -> Number)))
(define (sinh x)
  (cond [(real? x)  (cond [(flonum? x)  (flsinh x)]
                          [(eqv? x 0)  0]
                          [else  (flsinh (fl x))])]
        [else  (cond [(float-complex? x)  (* 0.0-1.0i (sin (* 0.0+1.0i x)))]
                     [else  (* -i (sin (* +i x)))])]))

(: cosh (case-> (Zero -> One)
                (Float -> Float)
                (Real -> Real)
                (Float-Complex -> Float-Complex)
                (Number -> Number)))
(define (cosh x)
  (cond [(real? x)  (cond [(flonum? x)  (flcosh x)]
                          [(eqv? x 0)  1]
                          [else  (flcosh (fl x))])]
        [else  (cond [(float-complex? x)  (cos (* 0.0+1.0i x))]
                     [else  (cos (* 0+i x))])]))

(: tanh (case-> (Zero -> Zero)
                (Float -> Float)
                (Real -> Real)
                (Float-Complex -> Float-Complex)
                (Number -> Number)))
(define (tanh x)
  (cond [(real? x)  (cond [(flonum? x)  (fltanh x)]
                          [(eqv? x 0)  0]
                          [else  (fltanh (fl x))])]
        [else  (cond [(float-complex? x)  (* 0.0-1.0i (tan (* 0.0+1.0i x)))]
                     [else  (* 0-i (tan (* 0+i x)))])]))

;; ===================================================================================================
;; Inverse hyperbolic

(: asinh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (asinh x)
  (cond [(flonum? x)  (flasinh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flasinh (fl x))]
        [(float-complex? x)  (log (+ x (sqrt (+ (* x x) 1.0))))]
        [else  (log (+ x (sqrt (+ (* x x) 1))))]))

(: acosh (case-> (One -> Zero)
                 (Float -> Float)
                 (Real -> Number)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (acosh x)
  (cond [(flonum? x)  (flacosh x)]
        [(eqv? x 1)  0]
        [(and (real? x) (x . >= . 1))  (flacosh (fl x))]
        [(float-complex? x)  (log (+ x (* (sqrt (+ x 1.0)) (sqrt (- x 1.0)))))]
        [else  (log (+ x (* (sqrt (+ x 1)) (sqrt (- x 1)))))]))

(: atanh (case-> (Zero -> Zero)
                 (Float -> Float)
                 (Real -> Real)
                 (Float-Complex -> Float-Complex)
                 (Number -> Number)))
(define (atanh x)
  (cond [(flonum? x)  (flatanh x)]
        [(eqv? x 0)  0]
        [(real? x)  (flatanh (fl x))]
        [(float-complex? x)  (* 0.5 (- (log (+ 1.0 x)) (log (- 1.0 x))))]
        [else  (* 1/2 (- (log (+ 1 x)) (log (- 1 x))))]))
