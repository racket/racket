#lang typed/racket/base

(require racket/flonum
         (except-in racket/math sinh cosh tanh)
         "private/functions/hyperbolic.rkt"
         "private/functions/inverse-hyperbolic.rkt"
         "private/functions/random.rkt")

(provide (all-from-out
          racket/math
          "private/functions/hyperbolic.rkt"
          "private/functions/inverse-hyperbolic.rkt"
          "private/functions/random.rkt"))

(provide power-of-two?
         phi.0
         euler.0
         gamma.0
         catalan.0)

(define phi.0 (real->double-flonum #e1.61803398874989484820458683436563811772))
(define euler.0 (real->double-flonum #e2.718281828459045235360287471352662497759))
(define gamma.0 (real->double-flonum #e0.5772156649015328606065120900824024310432))
(define catalan.0 (real->double-flonum #e0.9159655941772190150546035149323841107734))

;; Returns #t if x is an integer power of 2
(: power-of-two? (Real -> Boolean))
(define (power-of-two? x)
  (cond [(not (positive? x))  #f]
        [(flonum? x)  (fl= x (flexpt 2.0 (flround (fl/ (fllog x) (fllog 2.0)))))]
        [(single-flonum? x)  (power-of-two? (real->double-flonum x))]
        [(integer? x)  (= x (expt 2 (- (integer-length x) 1)))]
        [else  (and (= 1 (numerator x))
                    (power-of-two? (denominator x)))]))
