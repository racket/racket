#lang typed/racket/base

(require racket/flonum
         "../flonum/flvector.rkt"
         "../flonum/flonum-functions.rkt"
         "../flonum/flonum-more-functions.rkt")

(provide power-of-two?
         absolute-error
         relative-error
         sum
         asinh acosh atanh
         float-complex?
         inline-number->float-complex
         number->float-complex)

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

;; ===================================================================================================
;; Float-Complex functions

(define-predicate float-complex? Float-Complex)

(module syntax-defs racket/base
  (require (for-syntax racket/base
                       typed/untyped-utils)
           (only-in typed/racket/base : Number let:)
           racket/flonum)
  
  (provide inline-number->float-complex)
  
  (define-syntax (inline-number->float-complex stx)
    (syntax-case stx ()
      [(_ z-expr)
       (syntax/loc stx
         (let: ([z : Number  z-expr])
           (if (number? z)
               (make-rectangular (real->double-flonum (real-part z))
                                 (real->double-flonum (imag-part z)))
               (raise-argument-error 'number->float-complex "number?" z))))]))
  
  )  ; module

(require 'syntax-defs)

(: number->float-complex (Number -> Float-Complex))
(define (number->float-complex z)
  (make-rectangular (fl (real-part z))
                    (fl (imag-part z))))
