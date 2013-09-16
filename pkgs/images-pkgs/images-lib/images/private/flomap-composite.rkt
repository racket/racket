#lang typed/racket/base

(require racket/match racket/unsafe/ops
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-pin flomap-pin*
         flomap-lt-superimpose flomap-lc-superimpose flomap-lb-superimpose
         flomap-ct-superimpose flomap-cc-superimpose flomap-cb-superimpose
         flomap-rt-superimpose flomap-rc-superimpose flomap-rb-superimpose
         flomap-vl-append flomap-vc-append flomap-vr-append
         flomap-ht-append flomap-hc-append flomap-hb-append)

(: flomap-pin (case-> (flomap Integer Integer flomap -> flomap)
                      (flomap Integer Integer flomap Integer Integer -> flomap)))
(define flomap-pin
  (case-lambda
    [(fm1 x1 y1 fm2)
     (match-define (flomap argb1-vs c w1 h1) fm1)
     (match-define (flomap argb2-vs c2 w2 h2) fm2)
     
     (unless (c . > . 0)
       (raise-type-error 'flomap-pin "flomap with at least one component" fm1))
     
     (unless (= c c2)
       (error 'flomap-pin 
              (string-append "expected two flomaps with the same number of components; "
                             "given one with ~e and one with ~e")
              c c2))
     
     ;; fm1 and fm2 offsets, in final image coordinates
     (define dx1 (fxmax 0 (fx- 0 x1)))
     (define dy1 (fxmax 0 (fx- 0 y1)))
     (define dx2 (fxmax 0 x1))
     (define dy2 (fxmax 0 y1))
     
     ;; final image size
     (define w (fxmax (unsafe-fx+ dx1 w1) (unsafe-fx+ dx2 w2)))
     (define h (fxmax (unsafe-fx+ dy1 h1) (unsafe-fx+ dy2 h2)))
     
     (define argb-vs (make-flvector (* c w h)))
     (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
       (when (y . fx< . h)
         (define y1 (unsafe-fx- y dy1))
         (define y2 (unsafe-fx- y dy2))
         (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
           (cond
             [(x . fx< . w)
              (define x1 (unsafe-fx- x dx1))
              (define x2 (unsafe-fx- x dx2))
              
              (define i (coords->index c w 0 x y))
              (define-values (i1 a1)
                (cond [(and (x1 . fx>= . 0) (x1 . fx< . w1) (y1 . fx>= . 0) (y1 . fx< . h1))
                       (define i1 (coords->index c w1 0 x1 y1))
                       (values i1 (flvector-ref argb1-vs i1))]
                      [else  (values 0 0.0)]))
              (define-values (i2 a2)
                (cond [(and (x2 . fx>= . 0) (x2 . fx< . w2) (y2 . fx>= . 0) (y2 . fx< . h2))
                       (define i2 (coords->index c w2 0 x2 y2))
                       (values i2 (flvector-ref argb2-vs i2))]
                      [else  (values 0 0.0)]))
              
              (cond
                [(and (a1 . > . 0.0) (a2 . > . 0.0))
                 (let: k-loop : Void ([k : Nonnegative-Fixnum  0])
                   (cond [(k . fx< . c)  (define v1 (flvector-ref argb1-vs (unsafe-fx+ i1 k)))
                                         (define v2 (flvector-ref argb2-vs (unsafe-fx+ i2 k)))
                                         (define v (fl-alpha-blend v1 v2 a2))
                                         (flvector-set! argb-vs (unsafe-fx+ i k) v)
                                         (k-loop (unsafe-fx+ k 1))]
                         [else  (x-loop (unsafe-fx+ x 1))]))]
                [(a1 . > . 0.0)
                 (let: k-loop : Void ([k : Nonnegative-Fixnum  0])
                   (cond [(k . fx< . c)  (define v1 (flvector-ref argb1-vs (unsafe-fx+ i1 k)))
                                         (flvector-set! argb-vs (unsafe-fx+ i k) v1)
                                         (k-loop (unsafe-fx+ k 1))]
                         [else  (x-loop (unsafe-fx+ x 1))]))]
                [(a2 . > . 0.0)
                 (let: k-loop : Void ([k : Nonnegative-Fixnum  0])
                   (cond [(k . fx< . c)  (define v2 (flvector-ref argb2-vs (unsafe-fx+ i2 k)))
                                         (flvector-set! argb-vs (unsafe-fx+ i k) v2)
                                         (k-loop (unsafe-fx+ k 1))]
                         [else  (x-loop (unsafe-fx+ x 1))]))]
                [else  (x-loop (unsafe-fx+ x 1))])]
             [else  (y-loop (unsafe-fx+ y 1))]))))
     (flomap argb-vs c w h)]
    [(fm1 x1 y1 fm2 x2 y2)  (flomap-pin fm1 (- x1 x2) (- y1 y2) fm2)]))

(: flomap-pin* (Real Real Real Real flomap flomap * -> flomap))
(define (flomap-pin* x1-frac y1-frac x2-frac y2-frac fm0 . fms)
  (define-values (fm _x _y)
    (for/fold: ([fm : flomap  fm0]
                [x : Exact-Rational  0]
                [y : Exact-Rational  0]
                ) ([fm1 : flomap  (in-list (cons fm0 fms))]
                   [fm2 : flomap  (in-list fms)])
      (define-values (w1 h1) (flomap-size fm1))
      (define-values (w2 h2) (flomap-size fm2))
      (define x1 (+ x (- (inexact->exact (* x1-frac w1))
                         (inexact->exact (* x2-frac w2)))))
      (define y1 (+ y (- (inexact->exact (* y1-frac h1))
                         (inexact->exact (* y2-frac h2)))))
      (values (flomap-pin fm (round x1) (round y1) fm2)
              (max 0 x1) (max 0 y1))))
  fm)

(: flomap-lt-superimpose (flomap flomap * -> flomap))
(: flomap-lc-superimpose (flomap flomap * -> flomap))
(: flomap-lb-superimpose (flomap flomap * -> flomap))
(: flomap-ct-superimpose (flomap flomap * -> flomap))
(: flomap-cc-superimpose (flomap flomap * -> flomap))
(: flomap-cb-superimpose (flomap flomap * -> flomap))
(: flomap-rt-superimpose (flomap flomap * -> flomap))
(: flomap-rc-superimpose (flomap flomap * -> flomap))
(: flomap-rb-superimpose (flomap flomap * -> flomap))

(define (flomap-lt-superimpose fm . fms) (apply flomap-pin* 0 0 0 0 fm fms))
(define (flomap-lc-superimpose fm . fms) (apply flomap-pin* 0 1/2 0 1/2 fm fms))
(define (flomap-lb-superimpose fm . fms) (apply flomap-pin* 0 1 0 1 fm fms))
(define (flomap-ct-superimpose fm . fms) (apply flomap-pin* 1/2 0 1/2 0 fm fms))
(define (flomap-cc-superimpose fm . fms) (apply flomap-pin* 1/2 1/2 1/2 1/2 fm fms))
(define (flomap-cb-superimpose fm . fms) (apply flomap-pin* 1/2 1 1/2 1 fm fms))
(define (flomap-rt-superimpose fm . fms) (apply flomap-pin* 1 0 1 0 fm fms))
(define (flomap-rc-superimpose fm . fms) (apply flomap-pin* 1 1/2 1 1/2 fm fms))
(define (flomap-rb-superimpose fm . fms) (apply flomap-pin* 1 1 1 1 fm fms))

(: flomap-vl-append (flomap flomap * -> flomap))
(: flomap-vc-append (flomap flomap * -> flomap))
(: flomap-vr-append (flomap flomap * -> flomap))
(: flomap-ht-append (flomap flomap * -> flomap))
(: flomap-hc-append (flomap flomap * -> flomap))
(: flomap-hb-append (flomap flomap * -> flomap))

(define (flomap-vl-append fm . fms) (apply flomap-pin* 0 1 0 0 fm fms))
(define (flomap-vc-append fm . fms) (apply flomap-pin* 1/2 1 1/2 0 fm fms))
(define (flomap-vr-append fm . fms) (apply flomap-pin* 1 1 1 0 fm fms))
(define (flomap-ht-append fm . fms) (apply flomap-pin* 1 0 0 0 fm fms))
(define (flomap-hc-append fm . fms) (apply flomap-pin* 1 1/2 0 1/2 fm fms))
(define (flomap-hb-append fm . fms) (apply flomap-pin* 1 1 0 1 fm fms))
