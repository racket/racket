#lang racket/base

(require racket/math racket/flonum racket/contract racket/match
         "math.rkt"
         "contract.rkt" "contract-doc.rkt")

(provide (struct-out invertible-function)
         make-axis-transform
         id-transform
         log-transform
         cbrt-transform
         hand-drawn-transform)

(define-struct/contract invertible-function ([f (real? . -> . real?)] [finv (real? . -> . real?)])
  #:transparent)

;; Turns any total, surjective, monotone flonum op and its inverse into an axis transform
(define ((make-axis-transform flop flinv) x-min x-max)
  (let ([x-min  (exact->inexact x-min)]
        [x-max  (exact->inexact x-max)])
    (define fx-min (flop x-min))
    (define fx-scale (fl/ (fl- x-max x-min)
                          (fl- (flop x-max) fx-min)))
    (define (f x)
      (fl+ x-min (fl* (fl- (flop (exact->inexact x)) fx-min)
                      fx-scale)))
    (define (finv y)
      (flinv (fl+ fx-min (fl/ (fl- (exact->inexact y) x-min)
                              fx-scale))))
    (invertible-function f finv)))

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


(defproc (id-transform [x-min real?] [x-max real?]) invertible-function?
  (invertible-function values values))

(defproc (log-transform [x-min real?] [x-max real?]) invertible-function?
  (when ((exact->inexact x-min) . <= . 0)
    (raise-type-error 'log-transform "positive real" 0 x-min x-max))
  ((make-axis-transform fllog flexp) x-min x-max))

(define cbrt-trans (make-axis-transform cbrt cube))

(defproc (cbrt-transform [x-min real?] [x-max real?]) invertible-function?
  (cbrt-trans x-min x-max))

(defproc (hand-drawn-transform [freq (and/c real? (>/c 0))]) (real? real? . -> . invertible-function?)
  (λ (mn mx)
    (define d (/ freq (- mx mn)))
    ((make-axis-transform (sine-diag d) (sine-diag-inv d)) mn mx)))
