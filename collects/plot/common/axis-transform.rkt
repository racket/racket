#lang racket/base

(require racket/math racket/flonum racket/contract racket/match unstable/latent-contract/defthing
         "math.rkt"
         "contract.rkt")

(provide (all-defined-out))

(struct invertible-function (f g) #:transparent)

(defproc (invertible-compose [f1 invertible-function?] [f2 invertible-function?]
                             ) invertible-function?
  (match-let ([(invertible-function f1 g1)  f1]
              [(invertible-function f2 g2)  f2])
    (invertible-function (compose f1 f2) (compose g2 g1))))

(defproc (invertible-inverse [h invertible-function?]) invertible-function? #:document-body
  (match-define (invertible-function f g) h)
  (invertible-function g f))

(defcontract axis-transform/c (real? real? invertible-function? . -> . invertible-function?))

(defthing id-transform axis-transform/c
  (λ (x-min x-max old-function) old-function))

(defthing id-function invertible-function? (invertible-function (λ (x) x) (λ (x) x)))

(defproc (apply-axis-transform [t axis-transform/c] [x-min real?] [x-max real?]
                               ) invertible-function? #:document-body
  (t x-min x-max id-function))

;; Turns any total, surjective, monotone real function and its inverse into an axis transform
(defproc (make-axis-transform [fun invertible-function?]) axis-transform/c
  (match-define (invertible-function f g) fun)
  (λ (x-min x-max old-function)
    (define fx-min (f x-min))
    (define fx-scale (/ (- x-max x-min) (- (f x-max) fx-min)))
    (define (new-f x) (+ x-min (* (- (f x) fx-min) fx-scale)))
    (define (new-g y) (g (+ fx-min (/ (- y x-min) fx-scale))))
    (invertible-compose (invertible-function new-f new-g) old-function)))

;; ===================================================================================================
;; Axis transform combinators

(defproc (axis-transform-compose [t1 axis-transform/c] [t2 axis-transform/c]) axis-transform/c
  (λ (x-min x-max old-function)
    (t1 x-min x-max (t2 x-min x-max old-function))))

(defproc (axis-transform-append [t1 axis-transform/c] [t2 axis-transform/c] [mid real?]
                                ) axis-transform/c
  (λ (x-min x-max old-function)
    (match-define (invertible-function old-f old-g) old-function)
    (let ([mid  (old-f mid)])
      (cond [(mid . >= . x-max)  (t1 x-min x-max old-function)]
            [(mid . <= . x-min)  (t2 x-min x-max old-function)]
            [else
             (match-define (invertible-function f1 g1) (t1 x-min mid old-function))
             (match-define (invertible-function f2 g2) (t2 mid x-max old-function))
             ((make-axis-transform
               (invertible-function
                (λ (x) (cond [((old-f x) . < . mid)  (f1 x)]
                             [else  (f2 x)]))
                (λ (x) (cond [(x . < . mid)  (g1 x)]
                             [else  (g2 x)]))))
              x-min x-max id-function)]))))

(defproc (axis-transform-bound [t axis-transform/c] [a real?] [b real?]
                               ) axis-transform/c #:document-body
  (axis-transform-append
   (axis-transform-append id-transform t a) id-transform b))

;; ===================================================================================================
;; Specific axis transforms

(define ((flnewton-invert f f-diff f-inv-guess n) y)
  (let ([y  (exact->inexact y)])
    (let loop ([x  (f-inv-guess y)] [n n])
      (let/ec return
        (when (zero? n) (return x))
        
        (define dx (fl/ (fl- y (f x)) (f-diff x)))
        (when (zero? dx) (return x))
        
        (loop (fl- x dx) (sub1 n))))))

(define (sine-diag d)
  (let ([d  (exact->inexact d)])
    (λ (x) (let ([x  (exact->inexact x)])
             (fl+ x (fl* (fl/ 1.0 (fl* 4.0 d)) (flsin (fl* d x))))))))

(define (sine-diag-diff d)
  (let ([d  (exact->inexact d)])
    (λ (x) (let ([x  (exact->inexact x)])
             (fl- (fl/ (flcos (fl* d x)) 4.0) 1.0)))))

(define (sine-diag-inv d)
  (flnewton-invert (sine-diag d) (sine-diag-diff d) values 10))

(define cbrt
  (let ([e  (exact->inexact 1/3)])
    (λ (x) (let ([x  (exact->inexact x)])
             (fl* (sgn x) (expt (flabs x) e))))))

(define (cube x)
  (let ([x  (exact->inexact x)])
    (fl* x (fl* x x))))

(define (real-log x)
  (fllog (exact->inexact x)))

(define (real-exp x)
  (flexp (exact->inexact x)))

(defthing log-transform axis-transform/c
  (λ (x-min x-max old-function)
    (when ((exact->inexact x-min) . <= . 0)
      (raise-type-error 'log-transform "positive real" 0 x-min x-max))
    ((make-axis-transform (invertible-function real-log real-exp)) x-min x-max old-function)))

(defthing cbrt-transform axis-transform/c
  (λ (x-min x-max old-function)
    ((make-axis-transform (invertible-function cbrt cube)) x-min x-max old-function)))

(defproc (hand-drawn-transform [freq (>/c 0)]) axis-transform/c
  (λ (x-min x-max old-function)
    (define d (/ freq (- x-max x-min)))
    ((make-axis-transform (invertible-function (sine-diag d) (sine-diag-inv d)))
     x-min x-max old-function)))

;; ===================================================================================================

(define (stretch a b s)
  (define d (- b a))
  (define ds (* d s))
  (λ (x)
    (cond [(x . < . a)  x]
          [(x . > . b)  (+ (- x d) ds)]
          [else         (+ a (* (- x a) s))])))

(defproc (stretch-transform [a real?] [b real?] [scale (>/c 0)]) axis-transform/c
  (when (a . > . b) (error 'stretch-transform "expected a <= b; given ~e and ~e" a b))
  (λ (x-min x-max old-function)
    (match-define (invertible-function old-f old-g) old-function)
    (let ([a  (old-f a)]
          [b  (old-f b)])
      (define f (stretch a b scale))
      (define g (stretch (f a) (f b) (/ 1 scale)))
      ((make-axis-transform (invertible-function f g)) x-min x-max old-function))))

(defproc (collapse-transform [a real?] [b real?]) axis-transform/c
  (when (a . > . b) (error 'stretch-transform "expected a <= b; given ~e and ~e" a b))
  (λ (x-min x-max old-function)
    (match-define (invertible-function old-f old-g) old-function)
    (let ([a  (old-f a)]
          [b  (old-f b)])
      (define 1/2size (* 1/2 (- b a)))
      (define center (* 1/2 (+ a b)))
      (define (f x) (cond [(x . < . a)  (+ x 1/2size)]
                          [(x . > . b)  (- x 1/2size)]
                          [else  center]))
      (define (g x) (cond [(x . < . center)  (- x 1/2size)]
                          [(x . > . center)   (+ x 1/2size)]
                          [else  center]))
      ((make-axis-transform (invertible-function f g)) x-min x-max old-function))))
