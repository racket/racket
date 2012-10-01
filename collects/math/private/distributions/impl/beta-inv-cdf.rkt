#lang typed/racket/base

;(module defs typed/racket/base

(require racket/fixnum
         "../../../flonum.rkt"
         "../../functions/beta.rkt"
         "../../functions/incomplete-beta.rkt"
         "beta-pdf.rkt"
         "beta-utils.rkt"
         "normal-inv-cdf.rkt")

(provide flbeta-inv-cdf)
;(provide (all-defined-out))

;; =================================================================================================
;; Initial approximation

(define 1-eps (fl- 1.0 (fl* 0.5 epsilon.0)))

(: bound-estimate (Flonum -> Flonum))
(define (bound-estimate x)
  (flmax +min.0 (flmin 1-eps x)))

(: log-const-numer (Flonum Flonum Flonum -> Flonum))
(define (log-const-numer a b x)
  (fl+ (fl* a (fllog x)) (fl* b (fllog1p (- x)))))

(: recip-log-const-numer-diff (Flonum Flonum Flonum -> Flonum))
(define (recip-log-const-numer-diff a b x)
  (fl/ (fl- (fl* x x) x)
       (fl- (fl* (fl+ a b) x) a)))

(: flbeta-inv-log-cdf-appx-asym-0 (Flonum Flonum Flonum -> Flonum))
;; Approximates by inverting the multiplicative term in front of the hypergeometric series (see
;; flbeta-regularized-hypergeom in math/private/functions/incomplete-beta)
;; This is a *really good* approximation in the tails; for the middle, we'll interpolate between
;; two reasonable points near the tails, or use a normal approximation (whichever is better)
(define (flbeta-inv-log-cdf-appx-asym-0 a b log-p)
  (define y (fl+ (fl+ log-p (fllog a)) (fllog-beta a b)))
  ;; ^^^ the easy part; now we need to invert log(p) = a*log(x) + b*log(1-x)
  (define x0 (flexp (fl/ log-p a)))  ; initial guess
  (define x (flmax +min.0 (flmin (fl* 0.9 (fl/ a (fl+ a b))) x0)))  ; bound the guess
  ;(printf "x = ~v~n" x)
  (define fx (log-const-numer a b x))
  (let loop ([x x] [fx fx] [fac 1.0] [i 0])
    ;; Newton says this is the change:
    (define dx (fl* (fl- fx y) (recip-log-const-numer-diff a b x)))
    ;; Halve it until we get something that doesn't obviously overshoot, and adjust `fac'
    (define-values (new-x new-dx new-fac)
      (let: dx-loop : (Values Flonum Flonum Flonum)
        ([dx : Flonum  (fl* fac (flmax -0.1 (flmin 0.1 dx)))]
         [fac : Flonum  fac]
         [j : Nonnegative-Fixnum  0])
        (define new-x (- x dx))
        ;(printf "dx: new-x = ~v  dx = ~v~n" new-x dx)
        (cond [(and (new-x . fl> . 0.0) (new-x . fl<= . 1.0))  (values new-x dx fac)]
              [(not (rational? new-x))  (values x dx fac)]
              [(j . fx< . 1000)  (dx-loop (fl* 0.5 dx) (fl* 0.5 fac) (fx+ j 1))]
              [else  (values x dx fac)])))
    ;(printf "x = ~v  dx = ~v  fac = ~v~n" new-x (- new-x x) new-fac)
    (define new-fx (log-const-numer a b new-x))
    (cond [(or ((flabs (fl- fx new-fx)) . fl<= . (flabs (fl* (fl* 1000.0 epsilon.0) new-fx)))
               (fl<= (flabs new-dx) (fl* (fl* 0.5 epsilon.0) new-x))
               (not (rational? new-x)))
           new-x]
          [(i . fx< . 1000)
           (loop new-x new-fx (flmin 1.0 (fl* new-fac 2.0)) (fx+ i 1))]
          [else
           new-x])))

(: flbeta-inv-log-cdf-appx-asym (Flonum Flonum Flonum Flonum -> Flonum))
(define (flbeta-inv-log-cdf-appx-asym a b log-p log-1-p)
  (define σ (flbeta-stddev a b))
  (define log-p0 (- (log-const-numer a b (bound-estimate (fl- (fl/ a (fl+ a b)) (fl* 0.85 σ))))
                    (fllog a)
                    (fllog-beta a b)))
  (define log-1-p1 (- (log-const-numer b a (bound-estimate (fl- (fl/ b (fl+ a b)) (fl* 0.85 σ))))
                      (fllog b)
                      (fllog-beta a b)))
  (cond [(log-p . fl< . log-p0)
         (flbeta-inv-log-cdf-appx-asym-0 a b log-p)]
        [(log-1-p . fl< . log-1-p1)
         (fl- 1.0 (flbeta-inv-log-cdf-appx-asym-0 b a log-1-p))]
        [else
         (define x0 (flbeta-inv-log-cdf-appx-asym-0 a b log-p0))
         (define x1 (fl- 1.0 (flbeta-inv-log-cdf-appx-asym-0 b a log-1-p1)))
         (define s (fl/ (fl- x1 x0) (fl- (lg1- log-1-p1) log-p0)))
         (define c (fl- x0 (fl* s log-p0)))
         (fl+ (fl* s log-p) c)]))

(: flbeta-inv-log-cdf-appx-normal (Flonum Flonum Flonum Flonum -> Flonum))
(define (flbeta-inv-log-cdf-appx-normal a b log-p log-1-p)
  (define m (flbeta-appx-median a b))
  (define σ (flbeta-stddev a b))
  (fl+ m (fl* σ (standard-flnormal-inv-log-cdf log-p))))

(: flbeta-inv-log-cdf-appx (Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (flbeta-inv-log-cdf-appx a b log-p log-1-p)
  (cond [(or (a . fl< . 10.0) (b . fl< . 10.0))
         (define x (bound-estimate (flbeta-inv-log-cdf-appx-asym a b log-p log-1-p)))
         (values x (fllog-beta-lower-regularized a b x))]
        [else
         (define x0 (bound-estimate (flbeta-inv-log-cdf-appx-asym a b log-p log-1-p)))
         (define x1 (bound-estimate (flbeta-inv-log-cdf-appx-normal a b log-p log-1-p)))
         (define real-log-p0 (fllog-beta-lower-regularized a b x0))
         (define real-log-p1 (fllog-beta-lower-regularized a b x1))
         (if ((flabs (fl- log-p real-log-p0)) . fl< . (flabs (fl- log-p real-log-p1)))
             (values x0 real-log-p0)
             (values x1 real-log-p1))]))

;; =================================================================================================
;; Newton's method

(: newton-lower-log-iter (Flonum Flonum Flonum Flonum Flonum Flonum -> (Values Flonum Flonum)))
(define (newton-lower-log-iter a b log-p x real-log-p fac)
  (define pdf-log-p (flbeta-log-pdf a b x))
  (cond [(fl<= (flabs (fl- log-p real-log-p))
               (flabs (fl* (fl* 100.0 epsilon.0) log-p)))
         (values 0.0 fac)]
        [else
         (define new-dx (fl* (flexp (fl- real-log-p pdf-log-p)) (fl- log-p real-log-p)))
         ;; Limit dx, then halve until the x+dx is in bounds
         (let loop ([new-dx  (fl* fac (flmin 0.1 (flmax -0.1 new-dx)))]
                    [fac fac]
                    [#{j : Nonnegative-Fixnum} 0])
           (define new-x (fl+ x new-dx))
           (cond [(and (new-x . fl>= . 0.0) (new-x . fl<= . 1.0))
                  (values new-dx fac)]
                 [(not (rational? new-x))
                  (values 0.0 fac)]
                 [(j . fx< . 1000)
                  (loop (fl* 0.5 new-dx) (fl* 0.5 fac) (fx+ j 1))]
                 [else
                  (values 0.0 fac)]))]))

(: flbeta-inv-log-cdf-newton (Flonum Flonum Flonum Flonum Flonum -> Flonum))
(define (flbeta-inv-log-cdf-newton a b log-p x real-log-p)
  (let loop ([dx 0.0] [x x] [real-log-p real-log-p] [fac 1.0] [#{c : Nonnegative-Fixnum} 1])
    (define-values (new-dx new-fac) (newton-lower-log-iter a b log-p x real-log-p fac))
    (define new-x (fl+ x new-dx))
    (define new-real-log-p (fllog-beta-lower-regularized a b new-x))
    ;(printf "~v ~v ~v~n" new-x new-dx new-fac)
    (cond [(or (fl<= (flabs (fl- real-log-p new-real-log-p))
                     (flabs (fl* (fl* 1000.0 epsilon.0) new-real-log-p)))
               (fl<= (flabs new-dx) (fl* (fl* 0.5 epsilon.0) new-x))
               (not (rational? new-x)))
           new-x]
          [(c . fx< . 1000)
           (let ([new-fac  (if (fl= (flsgn dx) (flsgn new-dx))
                               (flmin 1.0 (fl* new-fac 2.0))
                               (fl* (flmin new-fac 1.0) 0.5))])
             (loop new-dx new-x (fllog-beta-lower-regularized a b new-x) new-fac (fx+ c 1)))]
          [else
           new-x])))

;; =================================================================================================

(: in-bounds? (Flonum Flonum Flonum -> Boolean))
(define (in-bounds? a b log-p)
  (and (log-p . fl> . -inf.0) (log-p . fl< . 0.0)
       (a . fl> . 0.0) (a . fl< . +inf.0)
       (b . fl> . 0.0) (b . fl< . +inf.0)))

(: flbeta-inv-log-cdf-limits (Flonum Flonum Flonum -> Flonum))
(define (flbeta-inv-log-cdf-limits a b log-p)
  (cond [(not (and (log-p . fl<= . 0.0) (a . fl>= . 0.0) (b . fl>= . 0.0)))  +nan.0]
        [(fl= log-p -inf.0)  0.0]
        [(and (fl= a 0.0) (fl= b 0.0))  (if (log-p . fl< . (fllog 0.5)) 0.0 1.0)]
        [(and (fl= a +inf.0) (fl= b +inf.0))  0.5]
        [(fl= a +inf.0)  1.0]
        [(fl= b +inf.0)  0.0]
        [(fl= a 0.0)  0.0]
        [(fl= b 0.0)  1.0]
        [(fl= log-p 0.0)  1.0]
        [else  +nan.0]))

(: flbeta-inv-log-cdf* (Flonum Flonum Flonum Flonum -> Flonum))
(define (flbeta-inv-log-cdf* a b log-p log-1-p)
  (cond [(not (in-bounds? a b log-p))
         (flbeta-inv-log-cdf-limits a b log-p)]
        [else
         (define mid-log-p
           (if (a . fl> . b)
               (lg1- (fllog-beta-lower-regularized b a (fl/ b (fl+ a b))))
               (fllog-beta-lower-regularized a b (fl/ a (fl+ a b)))))
         (let-values ([(a b log-p log-1-p 1-?)  (if (log-p . fl< . mid-log-p)
                                                    (values a b log-p log-1-p #f)
                                                    (values b a log-1-p log-p #t))])
           (define-values (x0 real-log-p) (flbeta-inv-log-cdf-appx a b log-p log-1-p))
           (define x (flbeta-inv-log-cdf-newton a b log-p x0 real-log-p))
           (if 1-? (fl- 1.0 x) x))]))

(: flbeta-inv-cdf (Flonum Flonum Flonum Any Any -> Flonum))
(define (flbeta-inv-cdf a b p log? 1-p?)
  (cond [log?
         (cond [(p . fl<= . 0.0)
                (let-values ([(log-p log-1-p)  (cond [1-p?  (values (lg1- p) p)]
                                                     [else  (values p (lg1- p))])])
                  (flbeta-inv-log-cdf* a b log-p log-1-p))]
               [else
                +nan.0])]
        [else
         (cond [(and (p . fl>= . 0.0) (p . fl<= . 1.0))
                (let-values ([(log-p log-1-p)  (cond [1-p?  (values (fllog1p (- p)) (fllog p))]
                                                     [else  (values (fllog p) (fllog1p (- p)))])])
                  (flbeta-inv-log-cdf* a b log-p log-1-p))]
               [else
                +nan.0])]))


#|
  )

(require 'defs plot
         "../../../flonum.rkt"
         "../../../bigfloat.rkt"
         "../../functions/beta.rkt"
         "../../functions/incomplete-beta.rkt"
         "../dist-struct.rkt"
         "beta-pdf.rkt"
         "beta-utils.rkt")

(define a 1.4280969904206666)
(define b 1.0756718200854635e-16)
(define log-p (fllog 1.0381437356760724e-15))

(plot (list (inverse (λ (x) (fllog-beta-lower-regularized a b (fl x))))
            (function (λ (p) (flbeta-inv-log-cdf a b (fl p)))
                      #:color 2 #:style 'long-dash #:width 2)
            (function (λ (p) (flbeta-inv-log-cdf-appx a b (fl p) (lg1- (fl p))))
                      #:color 3 #:style 'short-dash)
            )
      ;#:y-min 0.0 ;#:y-max 1e-300
      #:x-min -40.0 ;(fllog-beta-lower-regularized a b +min.0)
      #:x-max (fllog-beta-lower-regularized a b (flprev 1.0))
      )

(plot (function
       (λ (p)
         (flbeta-inv-log-cdf a b (fl p))
         (define cs (get-cs))
         (cond [(empty? cs)  0]
               [else  (first cs)])))
      #:x-min -40.0 ;(fllog-beta-lower-regularized a b +min.0)
      #:x-max (fllog-beta-lower-regularized a b (flprev 1.0))
      #:y-max 20
      )

(require "../dist-struct.rkt"
         "../exponential-dist.rkt"
         "../uniform-dist.rkt")

(define dist1 (exp-dist 1e-16))
(define dist2 (exp-dist 1.0))
(define dist3 (exp-dist 1e16))
(define threshold 1e-7)

(define (random-param)
  (define r (random))
  (cond [(r . < . #i1/3)  (random-real dist1)]
        [(r . < . #i2/3)  (random-real dist2)]
        [else  (random-real dist3)]))
#;
(for ([_  (in-range 1000)])
  (define a (random-param))
  (define b (random-param))
  (define min-p (flbeta-lower-regularized a b +min.0))
  (define max-p (flbeta-lower-regularized a b (flprev 1.0)))
  (define min-log-p (fllog-beta-lower-regularized a b +min.0))
  (define max-log-p (fllog-beta-lower-regularized a b (flprev 1.0)))
  (define p (random-real (uniform-dist min-p max-p)))
  (define log-p (random-real (uniform-dist min-log-p max-log-p)))
  
  (define printed? #f)
  (define (print-header)
    (unless printed?
      (set! printed? #t)
      (printf "a = ~v  b = ~v~n" a b)))
  
  (define x0 (flbeta-inv-cdf a b p))
  (define p0 (flbeta-lower-regularized a b x0))
  (define err0 (flrelative-error p0 p))
  (when (err0 . > . threshold)
    (define p-prev (flbeta-lower-regularized a b (flprev x0)))
    (define p-next (flbeta-lower-regularized a b (flnext x0)))
    (when (not (and (p . >= . p-prev) (p . <= . p-next)))
      (print-header)
      (printf "input: ~v~n" p)
      (printf "err0 = ~v~n" err0)))
  
  (define x1 (flbeta-inv-log-cdf a b log-p))
  (define log-p1 (fllog-beta-lower-regularized a b x1))
  (define err1 (flrelative-error log-p1 log-p))
  (when (err1 . > . threshold)
    (define log-p-prev (fllog-beta-lower-regularized a b (flprev x1)))
    (define log-p-next (fllog-beta-lower-regularized a b (flnext x1)))
    (when (not (and (log-p1 . >= . log-p-prev) (log-p1 . <= . log-p-next)))
      (print-header)
      (printf "input (log): ~v~n" log-p)
      (printf "err1 = ~v~n" err1)))
  
  (when printed? (newline))
  )
|#