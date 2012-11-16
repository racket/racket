#lang typed/racket/base

#|
D. J. Best and D. E. Roberts
Algorithm AS 91: The Percentage Points of the Chi^2 Distribution

Used for starting points in Newton's method
|#

(require racket/fixnum
         "../../../flonum.rkt"
         "../../functions/gamma.rkt"
         "../../functions/log-gamma.rkt"
         "../../functions/incomplete-gamma.rkt"
         "gamma-pdf.rkt"
         "normal-inv-cdf.rkt")

(provide standard-flgamma-inv-cdf)

(: log-z01 (Float Float Float Any -> Float))
;; Normal approximation
(define (log-z01 k log-p log-1-p 1-p?)
  (define norm-x (cond [1-p?  (- (standard-flnormal-inv-log-cdf log-1-p))]
                       [else  (standard-flnormal-inv-log-cdf log-p)]))
  (fl+ (fllog k)
       (fl* 3.0 (fllog1p (fl+ (fl/ norm-x (fl* 3.0 (flsqrt k))) (fl/ #i-1/9 k))))))

(: z02 (Float Float -> Float))
;; Derived from z02 in Best-Roberts, or from asymptotic behavior of lower gamma
(define (z02 k log-p)
  (flexp (fl/ (fl+ (fl+ log-p (fllog k)) (fllog-gamma k)) k)))

(: z03 (Float Float Float -> Float))
;; Derived from z03 in Best-Roberts
(define (z03 k log-1-p log-x)
  (- (fl+ (fl- log-1-p (fl* (fl- k 1.0) (fl+ (fllog 0.5) log-x)))
          (fllog-gamma k))))

(: z04 (Float Float -> Float))
;; Derived from z04 in Best-Roberts
(define (z04 k log-1-p)
  (define a (fl+ (fl+ log-1-p (fllog-gamma k)) (fl* (fl- k 1.0) (fllog 2.0))))
  (let loop ([ch  0.4] [#{n : Fixnum} 99])
    (define p1 (fl/ 1.0 (fl+ 1.0 (fl* ch (fl+ 4.67 ch)))))
    (define p2 (fl* ch (fl+ 6.73 (fl* ch (fl+ 6.66 ch)))))
    (define t (fl- (fl+ -0.5 (fl* (fl+ 4.67 (fl* 2.0 ch)) p1))
                   (fl/ (fl+ 6.73 (fl* ch (fl+ 13.32 (fl* 3.0 ch))))
                        p2)))
    (define new-ch (fl- ch (fl/ (fl- 1.0 (fl* (fl* (flexp (fl+ a (fl* 0.5 ch))) p2) p1)) t)))
    (cond [(n . fx<= . 0)
           (* 0.5 new-ch)]
          [((flabs (fl- ch new-ch)) . fl<= . (flabs (fl* (fl* 4.0 epsilon.0) new-ch)))
           (* 0.5 new-ch)]
          [((flabs (fl- ch new-ch)) . fl<= . (flabs (fl* (fl* 1000.0 epsilon.0) new-ch)))
           (loop new-ch (fxmin 1 (- n 1)))]
          [else
           (loop new-ch (- n 1))])))

;; For testing: tells which approximation `flgamma-inv-log-cdf-appx' chooses
#;;(: flgamma-inv-log-cdf-which-appx (Float Float Float Any -> Float))
(define (flgamma-inv-log-cdf-which-appx k log-p log-1-p 1-p?)
  (cond [(k . < . (* -0.62 log-p))  2.0]
        [(k . > . 0.16)
         (define log-x (log-z01 k log-p log-1-p 1-p?))
         (define x (exp log-x))
         (if (x . > . (+ (* 4.4 k) 6.0)) 3.0 1.0)]
        [else  4.0]))

(: flgamma-inv-log-cdf-appx (Float Float Float Any -> Float))
(define (flgamma-inv-log-cdf-appx k log-p log-1-p 1-p?)
  (cond [(k . fl< . (fl* -0.62 log-p))  (z02 k log-p)]
        [(k . fl> . 0.16)
         (define log-x (log-z01 k log-p log-1-p 1-p?))
         (define x (flexp log-x))
         (if (x . fl> . (fl+ (fl* 4.4 k) 6.0)) (z03 k log-1-p log-x) x)]
        [else  (z04 k log-1-p)]))

(: newton-lower-log-iter (Float Float Float -> Float))
(define (newton-lower-log-iter k log-p x)
  (define real-log-p (fllog-gamma-inc k x #f #t))
  (define pdf-log-p (standard-flgamma-log-pdf k x))
  (define dx (fl/ (fl- log-p real-log-p) (flexp (fl- pdf-log-p real-log-p))))
  (define new-x (fl+ x dx))
  (if (and (new-x . fl>= . 0.0) (new-x . fl< . +inf.0)) new-x x))

(: newton-upper-log-iter (Float Float Float -> Float))
(define (newton-upper-log-iter k log-1-p x)
  (define real-log-1-p (fllog-gamma-inc k x #t #t))
  (define pdf-log-p (standard-flgamma-log-pdf k x))
  (define dx (fl/ (fl- real-log-1-p log-1-p) (flexp (fl- pdf-log-p real-log-1-p))))
  (define new-x (fl+ x dx))
  (if (and (new-x . fl>= . 0.0) (new-x . fl< . +inf.0)) new-x x))

(: flgamma-inv-log-cdf-newton (Float Float Float Any Float -> Float))
(define (flgamma-inv-log-cdf-newton k log-p log-1-p 1-p? x)
  (define-values (new-x c)
    (let: loop : (Values Float Fixnum) ([dx : Float  0.0]
                                        [x : Float  x]
                                        [c : Fixnum  1])
      (define new-x (cond [1-p?  (newton-upper-log-iter k log-1-p x)]
                          [else  (newton-lower-log-iter k log-p x)]))
      (define new-dx (fl- new-x x))
      (cond [(or ((flabs (fl- x new-x)) . fl<= . (flabs (fl* (fl* 4.0 epsilon.0) new-x)))
                 (c . fx>= . 100)
                 (not (rational? new-x)))
             (values new-x c)]
            [(and (c . fx> . 3) (not (fl= (flsgn new-dx) (flsgn dx))))
             ;; If we detect oscillation, the true value is between new-x and x
             (values (fl* 0.5 (fl+ new-x x)) c)]
            [else
             (loop new-dx new-x (fx+ c 1))])))
  new-x)

(: standard-flgamma-inv-log-cdf (Float Float Any -> Float))
(define (standard-flgamma-inv-log-cdf k log-p 1-p?)
  (let-values ([(log-p log-1-p)  (cond [1-p?  (values (lg1- log-p) log-p)]
                                       [else  (values log-p (lg1- log-p))])])
    (cond [(k . fl< . 0.0)  +nan.0]
          [(k . fl= . 0.0)  (if (fl= log-p -inf.0) 0.0 +inf.0)]
          [(k . fl> . 1e32)  (exp (log-z01 k log-p log-1-p 1-p?))]
          [(or (and (not 1-p?) (log-p . fl> . -inf.0) (log-p . fl< . 0.0))
               (and 1-p? (log-1-p . fl> . -inf.0) (log-1-p . fl< . 0.0)))
           (define x (flgamma-inv-log-cdf-appx k log-p log-1-p 1-p?))
           (flgamma-inv-log-cdf-newton k log-p log-1-p 1-p? x)]
          [(fl= log-p -inf.0)  0.0]
          [(fl= log-p 0.0)  +inf.0]
          [else  +nan.0])))

(: standard-flgamma-inv-cdf (Float Float Any Any -> Float))
(define (standard-flgamma-inv-cdf k p log? 1-p?)
  (cond [log?  (standard-flgamma-inv-log-cdf k p 1-p?)]
        [else  (standard-flgamma-inv-log-cdf k (fllog p) 1-p?)]))
