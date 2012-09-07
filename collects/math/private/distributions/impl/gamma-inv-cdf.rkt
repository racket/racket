#lang racket

#|
D. J. Best and D. E. Roberts
Algorithm AS 91: The Percentage Points of the Chi^2 Distribution

Used for starting points in Newton's method
|#

(module defs typed/racket/base
  
  (require racket/fixnum
           "../../../flonum.rkt"
           "../../functions/log1p.rkt"
           "../../functions/log-arithmetic.rkt"
           "../../functions/gamma.rkt"
           "../../functions/log-gamma.rkt"
           "../../functions/incomplete-gamma.rkt"
           "gamma-pdf.rkt"
           "normal-inv-cdf.rkt")
  
  (provide (all-defined-out))
  
  #;
  (provide flgamma-inv-cdf)
  
  (: z02 (Float Float -> Float))
  ;; Derived from z02 in Best-Roberts, or from asymptotic behavior of lower gamma
  (define (z02 k log-p)
    (exp (/ (+ log-p (fllog k) (fllog-gamma k)) k)))
  
  (: z01 (Float Float -> Float))
  ;; Normal approximation, and if p tends to 1, use z03
  (define (z01 k log-p)
    (define x (* k (flexpt (- (/ (standard-flnormal-inv-log-cdf log-p)
                                 (* 3.0 (flsqrt k)))
                              -1.0
                              (/ #i1/9 k))
                           3.0)))
    (cond [(x . > . (+ (* 2.2 2.0 k) 6.0))
           (z03 k log-p x)]
          [else  x]))
  
  (: z03 (Float Float Float -> Float))
  ;; Alteration from Best-Roberts: negate instead of multiplying by -2
  ;; Why: z03 is usually used when z01 *over*estimates, but multiplying by 2 *way* overestimates
  (define (z03 k log-p x)
    (- (+ (- (fllog1- log-p) (* (- k 1.0) (fllog (* 0.5 x))))
          (fllog-gamma k))))
  
  (: z04 (Float Float -> Float))
  ;; Derived from z04 in Best-Roberts
  (define (z04 k log-p)
    (define a (+ (fllog1- log-p) (fllog-gamma k) (* (- k 1.0) (fllog 2.0))))
    (let loop ([ch  0.4] [n 99])
      (define p1 (/ 1.0 (+ 1.0 (* ch (+ 4.67 ch)))))
      (define p2 (* ch (+ 6.73 (* ch (+ 6.66 ch)))))
      (define t (- (+ -0.5 (* (+ 4.67 (* 2.0 ch)) p1))
                   (/ (+ 6.73 (* ch (+ 13.32 (* 3.0 ch)))) p2)))
      (define new-ch (- ch (/ (- 1.0 (* (exp (+ a (* 0.5 ch))) p2 p1)) t)))
      (cond [(or (= n 0) ((abs (- ch new-ch)) . <= . (abs (* 4.0 +epsilon.0 new-ch))))
             (* 0.5 new-ch)]
            [((abs (- ch new-ch)) . <= . (abs (* 1000.0 +epsilon.0 new-ch)))
             (loop new-ch (min 1 (- n 1)))]
            [else  (loop new-ch (- n 1))])))
  
  (: flgamma-inv-cdf-appx (Float Float -> Float))
  (define (flgamma-inv-cdf-appx k log-p)
    (cond [(k . < . (* -0.62 log-p))  (z02 k log-p)]
          [(k . > . 0.16)  (z01 k log-p)]
          [else  (z04 k log-p)]))
  
  (: newton-log-iter (Float Float Float -> Float))
  (define (newton-log-iter k log-p x)
    ;(printf "newton-log-iter ~v ~v ~v~n" k log-p x)
    (define real-log-p (fllog-gamma-lower-regularized k x))
    (define new-x
      (cond [(log-p . < . real-log-p)
             (define dx (exp (fllog/ (fllog- real-log-p log-p)
                                     (standard-flgamma-log-pdf k x))))
             (- x dx)]
            [else
             (define dx (exp (fllog/ (fllog- log-p real-log-p)
                                     (standard-flgamma-log-pdf k x))))
             (+ x dx)]))
    (if (and (new-x . >= . 0.0) (new-x . < . +inf.0)) new-x x))
  
  (define: max-c : Integer  0)
  (define: max-c-values : (Listof Float)  null)
  
  (: flgamma-inv-log-cdf-newton (Float Float Float -> Float))
  (define (flgamma-inv-log-cdf-newton k log-p x)
    (define-values (new-x c)
      (let: loop : (Values Float Fixnum) ([x : Float  x] [n : Fixnum  99] [c : Fixnum  1])
        (define new-x (newton-log-iter k log-p x))
        (cond [(or (= n 0) ((abs (- x new-x)) . <= . (abs (* 4.0 +epsilon.0 new-x))))
               (values new-x c)]
              [((abs (- x new-x)) . <= . (abs (* 1000.0 +epsilon.0 new-x)))
               ;; Attempt to be smart: when we're less than three digits off, iterating sometimes
               ;; oscillates around the true answer, so give it a couple more iterations and quit
               (loop new-x (fxmin 1 (fx- n 1)) (fx+ c 1))]
              [else
               (loop new-x (fx- n 1) (fx+ c 1))])))
    (when (c . > . max-c)
      (set! max-c c)
      (set! max-c-values (list k log-p x new-x)))
    new-x)
  
  (: standard-flgamma-inv-log-cdf (Float Float -> Float))
  ;; Alteration from Best-Roberts: don't use the normal approximation directly
  ;; Why: applying the cdf to the result gives a `p' with millions of epsilons relative error
  (define (standard-flgamma-inv-log-cdf k log-p)
    (cond [(k . < . 0.0)  +nan.0]
          [(log-p . = . -inf.0)  0.0]
          [(log-p . = . 0.0)  +inf.0]
          [else  (flgamma-inv-log-cdf-newton k log-p (flgamma-inv-cdf-appx k log-p))]))
  
  (: standard-flgamma-inv-cdf (Float Float -> Float))
  (define (standard-flgamma-inv-cdf k p)
    (standard-flgamma-inv-log-cdf k (fllog p)))
  )

(require 'defs plot
         "../../../flonum.rkt"
         "../../functions/log1p.rkt"
         "../../functions/log-arithmetic.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt"
         "../../functions/incomplete-gamma.rkt"
         "gamma-pdf.rkt"
         "normal-inv-cdf.rkt")
#;
(plot3d (contour-intervals3d
         (λ (k p)
           (let ([k  (fl k)] [p  (fl p)])
             (printf "k = ~v  p = ~v~n" k p)
             (standard-flgamma-inv-cdf k p)))
         1.0 5
         0.99 (flstep 1.0 -1))
        #:x-label "k"
        #:y-label "p")

(define min-k 0.5)

(time
 (plot3d (contour-intervals3d
          (λ (k p)
            (let ([k  (fl k)] [p  (fl p)])
              (printf "k = ~v  p = ~v~n" k p)
              (define x (standard-flgamma-inv-cdf k p))
              (define new-p (flgamma-lower-regularized k x))
              #;(define err (abs (- new-p p)))
              (define err
                (abs (relative-error new-p p)))
              (if (rational? err) err -1.0)))
          min-k 1.0
          0.0 1.0)
         #:x-label "k"
         #:y-label "p"))

max-c
max-c-values