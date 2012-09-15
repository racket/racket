#lang typed/racket/base

#|
D. J. Best and D. E. Roberts
Algorithm AS 91: The Percentage Points of the Chi^2 Distribution

Used for starting points in Newton's method
|#

#|
#lang racket

(module defs typed/racket/base
|#

(require racket/fixnum
         "../../../flonum.rkt"
         "../../functions/log1p.rkt"
         "../../functions/log-arithmetic.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt"
         "../../functions/incomplete-gamma.rkt"
         "../../functions/lambert.rkt"
         "gamma-pdf.rkt"
         "normal-inv-cdf.rkt")

(provide standard-flgamma-inv-cdf)

(: log-z01 (Float Float Float Any -> Float))
;; Normal approximation
(define (log-z01 k log-p log-1-p 1-p?)
  (define norm-x (cond [1-p?  (- (standard-flnormal-inv-log-cdf log-1-p))]
                       [else     (standard-flnormal-inv-log-cdf log-p)]))
  (+ (fllog k)
     (* 3.0 (fllog1p (+ (/ norm-x (* 3.0 (flsqrt k))) (/ #i-1/9 k))))))

(: z02 (Float Float -> Float))
;; Derived from z02 in Best-Roberts, or from asymptotic behavior of lower gamma
(define (z02 k log-p)
  (exp (/ (+ log-p (fllog k) (fllog-gamma k)) k)))

(: z03 (Float Float Float -> Float))
;; Derived from z03 in Best-Roberts
(define (z03 k log-1-p log-x)
  (- (+ (- log-1-p (* (- k 1.0) (+ (fllog 0.5) log-x)))
        (fllog-gamma k))))

(: z04 (Float Float -> Float))
;; Derived from z04 in Best-Roberts
(define (z04 k log-1-p)
  (define a (+ log-1-p (fllog-gamma k) (* (- k 1.0) (fllog 2.0))))
  (let loop ([ch  0.4] [n 99])
    (define p1 (/ 1.0 (+ 1.0 (* ch (+ 4.67 ch)))))
    (define p2 (* ch (+ 6.73 (* ch (+ 6.66 ch)))))
    (define t (- (+ -0.5 (* (+ 4.67 (* 2.0 ch)) p1))
                 (/ (+ 6.73 (* ch (+ 13.32 (* 3.0 ch)))) p2)))
    (define new-ch (- ch (/ (- 1.0 (* (exp (+ a (* 0.5 ch))) p2 p1)) t)))
    (cond [(or (= n 0) ((abs (- ch new-ch)) . <= . (abs (* 4.0 epsilon.0 new-ch))))
           (* 0.5 new-ch)]
          [((abs (- ch new-ch)) . <= . (abs (* 1000.0 epsilon.0 new-ch)))
           (loop new-ch (min 1 (- n 1)))]
          [else  (loop new-ch (- n 1))])))

;; For testing: tells which approximation `flgamma-inv-log-cdf-appx' chooses
#;;(: flgamma-inv-log-cdf-which-appx (Float Float Float Any -> Float))
(define (flgamma-inv-log-cdf-which-appx k log-p log-1-p 1-p?)
  (cond [(k . < . (* -0.62 log-p))  2.0]
        [(k . > . 0.16)
         (define log-x (log-z01 k log-p log-1-p 1-p?))
         (define x (exp log-x))
         (if (x . > . (+ (* 2.2 2.0 k) 6.0)) 3.0 1.0)]
        [else  4.0]))

(: flgamma-inv-log-cdf-appx (Float Float Float Any -> Float))
(define (flgamma-inv-log-cdf-appx k log-p log-1-p 1-p?)
  (cond [(k . < . (* -0.62 log-p))  (z02 k log-p)]
        [(k . > . 0.16)
         (define log-x (log-z01 k log-p log-1-p 1-p?))
         (define x (exp log-x))
         (if (x . > . (+ (* 2.2 2.0 k) 6.0)) (z03 k log-1-p log-x) x)]
        [else  (z04 k log-1-p)]))

(: newton-lower-log-iter (Float Float Float -> Float))
(define (newton-lower-log-iter k log-p x)
  (define real-log-p (fllog-gamma-lower-regularized k x))
  (define pdf-log-p (standard-flgamma-log-pdf k x))
  (define dx (/ (- log-p real-log-p) (exp (- pdf-log-p real-log-p))))
  (define new-x (+ x dx))
  (if (and (new-x . >= . 0.0) (new-x . < . +inf.0)) new-x x))

(: newton-upper-log-iter (Float Float Float -> Float))
(define (newton-upper-log-iter k log-1-p x)
  (define real-log-1-p (fllog-gamma-upper-regularized k x))
  (define pdf-log-p (standard-flgamma-log-pdf k x))
  (define dx (/ (- real-log-1-p log-1-p) (exp (- pdf-log-p real-log-1-p))))
  (define new-x (+ x dx))
  (if (and (new-x . >= . 0.0) (new-x . < . +inf.0)) new-x x))

;(define: max-c : Integer  0)
;(define: max-c-values : (Listof Any)  null)

(: flgamma-inv-log-cdf-newton (Float Float Float Any Float -> Float))
(define (flgamma-inv-log-cdf-newton k log-p log-1-p 1-p? x)
  (define-values (new-x c)
    (let: loop : (Values Float Fixnum) ([dx : Float  0.0]
                                        [x : Float  x]
                                        [c : Fixnum  1])
      (define new-x (cond [1-p?  (newton-upper-log-iter k log-1-p x)]
                          [else  (newton-lower-log-iter k log-p x)]))
      (define new-dx (- new-x x))
      ;(printf "~v ~v~n" x new-x)
      (cond [(or ((abs (- x new-x)) . <= . (abs (* 4.0 epsilon.0 new-x)))
                 (c . >= . 100)
                 (not (rational? new-x)))
             (values new-x c)]
            [(and (c . > . 3) (not (= (flsgn new-dx) (flsgn dx))))
             ;; If we detect oscillation, the true value is between new-x and x
             (values (* 0.5 (+ new-x x)) c)]
            [else
             (loop new-dx new-x (fx+ c 1))])))
  #;; For testing
  (when (c . > . max-c)
    (set! max-c c)
    (set! max-c-values (list k log-p 1-p? x new-x)))
  new-x)

(: standard-flgamma-inv-log-cdf (Float Float Any -> Float))
(define (standard-flgamma-inv-log-cdf k log-p 1-p?)
  (let-values ([(log-p log-1-p)  (cond [1-p?  (values (fllog1- log-p) log-p)]
                                       [else  (values log-p (fllog1- log-p))])])
    (cond [(k . < . 0.0)  +nan.0]
          [(k . = . 0.0)  (if (log-p . = . -inf.0) 0.0 +inf.0)]
          [(k . > . 1e32)  (exp (log-z01 k log-p log-1-p 1-p?))]
          [(or (and (not 1-p?) (log-p . > . -inf.0) (log-p . < . 0.0))
               (and 1-p? (log-1-p . > . -inf.0) (log-1-p . < . 0.0)))
           (define x (flgamma-inv-log-cdf-appx k log-p log-1-p 1-p?))
           (flgamma-inv-log-cdf-newton k log-p log-1-p 1-p? x)]
          [(log-p . = . -inf.0)  0.0]
          [(log-p . = . 0.0)  +inf.0]
          [else  +nan.0])))

(: standard-flgamma-inv-cdf (Float Float Any Any -> Float))
(define (standard-flgamma-inv-cdf k p log? 1-p?)
  (cond [log?  (standard-flgamma-inv-log-cdf k p 1-p?)]
        [else  (standard-flgamma-inv-log-cdf k (fllog p) 1-p?)]))
#|  
)

(require 'defs plot
         "../../../flonum.rkt"
         "../../functions/log1p.rkt"
         "../../functions/log-arithmetic.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt"
         "../../functions/incomplete-gamma.rkt"
         "../../functions/lambert.rkt"
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

(define (relative-inverse-error p x f)
  (define new-ps
    (sort (filter rational? (list (f (flstep x -1)) (f x) (f (flstep x 1))))
          <))
  (values
   new-ps
   (cond [(empty? new-ps)  +inf.0]
         [(<= (first new-ps) p (last new-ps))  0.0]
         [else
          (define new-p (/ (apply + new-ps) (length new-ps)))
          (relative-error new-p p)])))

(define (err k p)
  (let ([k  (fl k)] [log-p  (fllog (fl p))])
    ;(printf "k = ~v  p = ~v~n" k p)
    (define x (standard-flgamma-inv-log-cdf k log-p #f))
    (define-values (new-ps err)
      (relative-inverse-error log-p x (λ (x) (fllog-gamma-lower-regularized k x))))
    (when (not (rational? err))
      (printf "k = ~v  p = ~v  x = ~v  new-ps = ~v  err = ~v~n" k (exp log-p) x new-ps err))
    (if (rational? err) err -1.0)))

#;
(plot3d (contour-intervals3d
         err
         0.1 1.0
         0.0001 0.9999)
        #:x-label "k"
        #:y-label "p")

max-c
max-c-values
|#
