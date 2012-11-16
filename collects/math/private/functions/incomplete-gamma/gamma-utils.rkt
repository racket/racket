#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "../stirling-error.rkt")

(provide flsharkfin fllog-sharkfin
         flgamma-series-const fllog-gamma-series-const
         flgamma-upper-const fllog-gamma-upper-const)

(: flsharkfin-high-precision (Flonum Flonum -> Flonum))
(define (flsharkfin-high-precision k x)
  ;; q2+q1 = k * (log1p((x-k)/k) - (x-k)/k)
  (let*-values ([(y2 y1)  (fast-fl-/error x k)]
                [(y2 y1)  (fl2/ y2 y1 k)]
                [(q2 q1)  (fl2log1p y2 y1)]
                [(q2 q1)  (fl2- q2 q1 y2 y1)]
                [(q2 q1)  (fl2* q2 q1 k)])
    (* (flexp q2) (flexp q1))))

(: flsharkfin (Flonum Flonum -> Flonum))
(define (flsharkfin k x)
  (define logy (fl+ (fl* k (fl- (fllog x) (fllog k)))
                    (fl- k x)))
  (cond
    [(or (k . fl< . 0.0) (x . fl< . 0.0))  +nan.0]
    [(fl= k 0.0)  (flexp (- x))]
    [(fl= x 0.0)  0.0]
    [(k . fl>= . 1e35)  (if (fl= x k) 1.0 0.0)]
    ;; Cut out a bunch of zeros immediately
    [(logy . < . -750.0)  0.0]
    ;; Argument reduction to make calculations cleaner if x is subnormal
    [(x . fl<= . +max-subnormal.0)
     (define c (flexpt 2.0 52.0))
     (fl/ (flsharkfin k (fl* x c))
          (fl* (flexpt c k) (flexp (* c x))))]
    [else
     (define-values (x/k x/k-lo) (fast-fl//error x k))
     (cond
       [(and (x/k . fl> . +max-subnormal.0) (x/k . fl<= . +max.0))
        (define-values (k-x k-x-lo) (fast-fl-/error k x))
        (define log-x/k^k (fl* k (fllog x/k)))
        (cond
          ;; First try a straightfoward-ish calculation with just a few flonum tricks
          [(and (k-x . fl> . (fllog +max-subnormal.0))
                (k-x . fl<= . (fllog +max.0))
                (log-x/k^k . fl> . (fllog +max-subnormal.0)))
           (* (flexpt+ x/k x/k-lo k)
              (flexp k-x)
              (flexp k-x-lo))]
          ;; Didn't work... try a similar calculation with argument reductions
          [else
           (define d0 (flmax (fl/ log-x/k^k (fllog (flsqrt +max-subnormal.0)))
                             (fl/ log-x/k^k (fllog (flsqrt +max.0)))))
           (define d1 (flmax (fl/ k-x (fllog (flsqrt +max-subnormal.0)))
                             (fl/ k-x (fllog (flsqrt +max.0)))))
           ;; Reduce exponents by `d' to keep intermediate computations from
           ;; under-/overflowing
           (define log2d (flmax 0.0 (flfloor (fl/ (fllog (flmax d0 d1)) (fllog 2.0)))))
           (define d (flexpt 2.0 log2d))
           (cond
             ;; Do it only if it wouldn't create too much error
             [(d . fl<= . 8.0)
              (define-values (y2 y1) (fast-fl*/error (flexpt+ x/k x/k-lo (fl/ k d))
                                                     (flexp (fl/ k-x d))))
              (fl* (flexpt+ y2 y1 d)
                   (flexp k-x-lo))]
             ;; Otherwise bring out the big guns
             [else
              (flsharkfin-high-precision k x)])])]
       [else
        ;; Plan z: just do it in log space, to heck with accuracy
        (flexp logy)])]))

(: fllog-sharkfin (Flonum Flonum -> Flonum))
(define (fllog-sharkfin k x)
  (define y (flsharkfin k x))
  (cond [(or (and (y . > . +max-subnormal.0) (y . < . 0.15))
             (y . > . 0.55))
         (fllog y)]
        [else
         (define-values (x/k x/k-lo) (fast-fl//error x k))
         (cond
           [(and (x/k . fl> . +max-subnormal.0) (x/k . fl<= . +max.0))
            (define-values (k-x k-x-lo) (fast-fl-/error k x))
            (define log-x/k^k (fl* k (fllog x/k)))
            (define d0 (flmax (fl/ log-x/k^k (fllog (flsqrt +max-subnormal.0)))
                              (fl/ log-x/k^k (fllog (flsqrt +max.0)))))
            (define d1 (flmax (fl/ k-x (fllog (flsqrt +max-subnormal.0)))
                              (fl/ k-x (fllog (flsqrt +max.0)))))
            ;; Reduce exponents by `d' to keep intermediate computations from
            ;; under-/overflowing
            (define log2d (flmax 0.0 (flfloor (fl/ (fllog (flmax d0 d1)) (fllog 2.0)))))
            (define d (flexpt 2.0 log2d))
            (define-values (y2 y1) (fast-fl*/error (flexpt+ x/k x/k-lo (fl/ k d))
                                                   (flexp (fl/ k-x d))))
            (+ (* d (fllog+ y2 y1)) k-x-lo)]
           [else
            (fl+ (fl* k (fl- (fllog x) (fllog k)))
                 (fl- k x))])]))

(: flgamma-series-const (Flonum Flonum -> Flonum))
;; Computes the series' leading constant
(define (flgamma-series-const k x)
  (cond [(or (k . fl< . 0.0) (x . fl< . 0.0))  +nan.0]
        [(fl= k 0.0)  (flexp (- x))]
        [(fl= x 0.0)  0.0]
        [else
         (fl/ (flsharkfin k x)
              (* (flexp-stirling k)
                 (flsqrt k)
                 (flsqrt (fl* 2.0 pi))))]))

(: fllog-gamma-series-const (Flonum Flonum -> Flonum))
;; Computes the series' leading constant
(define (fllog-gamma-series-const k x)
  (cond [(or (k . fl< . 0.0) (x . fl< . 0.0))  +nan.0]
        [(fl= k 0.0)  (- x)]
        [(fl= x 0.0)  -inf.0]
        [else
         (- (fllog-sharkfin k x)
            (+ (flstirling k)
               (* 0.5 (fllog k))
               (fllog (flsqrt (fl* 2.0 pi)))))]))

(: flgamma-upper-const (Flonum Flonum -> Flonum))
;; Computes the continued fraction's leading constant
(define (flgamma-upper-const k x)
  (cond [(or (k . fl< . 0.0) (x . fl< . 0.0))  +nan.0]
        [(or (fl= k 0.0) (fl= x 0.0))  0.0]
        [else
         (/ (* (flsharkfin k x) (flsqrt k))
            (* (flexp-stirling k)
               (flsqrt (* 2.0 pi))))]))

(: fllog-gamma-upper-const (Flonum Flonum -> Flonum))
;; Computes the continued fraction's leading constant
(define (fllog-gamma-upper-const k x)
  (cond [(or (k . fl< . 0.0) (x . fl< . 0.0))  +nan.0]
        [(or (fl= k 0.0) (fl= x 0.0))  -inf.0]
        [else
         (- (+ (fllog-sharkfin k x) (fllog (flsqrt k)))
            (+ (flstirling k)
               (fllog (flsqrt (* 2.0 pi)))))]))
