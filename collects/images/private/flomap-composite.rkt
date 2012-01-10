#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-pin flomap-pin*
         flomap-lt-superimpose flomap-lc-superimpose flomap-lb-superimpose
         flomap-ct-superimpose flomap-cc-superimpose flomap-cb-superimpose
         flomap-rt-superimpose flomap-rc-superimpose flomap-rb-superimpose
         flomap-vl-append flomap-vc-append flomap-vr-append
         flomap-ht-append flomap-hc-append flomap-hb-append)

(: flomap-pin (flomap Real Real flomap Real Real -> flomap))
(define (flomap-pin fm1 x1 y1 fm2 x2 y2)
  (cond
    [(not (and (zero? x2) (zero? y2)))
     (flomap-pin fm1 (- x1 x2) (- y1 y2) fm2 0 0)]
    [else
     (let ([x1  (exact->inexact x1)] [y1  (exact->inexact y1)])
       (match-define (flomap argb1-vs 4 w1 h1) fm1)
       (match-define (flomap argb2-vs 4 w2 h2) fm2)
       
       ;; fm1 and fm2 offsets, in final image coordinates
       (define dx1 (fl->fx (round (max 0.0 (- x1)))))
       (define dy1 (fl->fx (round (max 0.0 (- y1)))))
       (define dx2 (fl->fx (round (max 0.0 x1))))
       (define dy2 (fl->fx (round (max 0.0 y1))))
       
       ;; final image size
       (define w (fxmax (fx+ dx1 w1) (fx+ dx2 w2)))
       (define h (fxmax (fx+ dy1 h1) (fx+ dy2 h2)))
       
       (define-syntax-rule (get-argb-pixel argb-vs dx dy w h x y)
         (let ([x  (fx- x dx)] [y  (fx- y dy)])
           (cond [(and (x . fx>= . 0) (x . fx< . w) (y . fx>= . 0) (y . fx< . h))
                  (define i (coords->index 4 w 0 x y))
                  (values (unsafe-flvector-ref argb-vs i)
                          (unsafe-flvector-ref argb-vs (fx+ i 1))
                          (unsafe-flvector-ref argb-vs (fx+ i 2))
                          (unsafe-flvector-ref argb-vs (fx+ i 3)))]
                 [else  (values 0.0 0.0 0.0 0.0)])))
       
       (define argb-vs (make-flvector (* 4 w h)))
       (let: y-loop : Void ([y : Nonnegative-Fixnum  0])
         (when (y . fx< . h)
           (let: x-loop : Void ([x : Nonnegative-Fixnum  0])
             (cond
               [(x . fx< . w)
                (define-values (a1 r1 g1 b1) (get-argb-pixel argb1-vs dx1 dy1 w1 h1 x y))
                (define-values (a2 r2 g2 b2) (get-argb-pixel argb2-vs dx2 dy2 w2 h2 x y))
                (define i (coords->index 4 w 0 x y))
                (unsafe-flvector-set! argb-vs i (fl-alpha-blend a1 a2 a2))
                (unsafe-flvector-set! argb-vs (fx+ i 1) (fl-alpha-blend r1 r2 a2))
                (unsafe-flvector-set! argb-vs (fx+ i 2) (fl-alpha-blend g1 g2 a2))
                (unsafe-flvector-set! argb-vs (fx+ i 3) (fl-alpha-blend b1 b2 a2))
                (x-loop (fx+ x 1))]
               [else  (y-loop (fx+ y 1))]))))
       (flomap argb-vs 4 w h))]))

(: flomap-pin* (Real Real Real Real flomap flomap * -> flomap))
(define (flomap-pin* x1-frac y1-frac x2-frac y2-frac fm . fms)
  (for/fold ([fm1 fm]) ([fm2  (in-list fms)])
    (define-values (w1 h1) (flomap-size fm1))
    (define-values (w2 h2) (flomap-size fm2))
    (flomap-pin fm1 (* x1-frac w1) (* y1-frac h1)
                fm2 (* x2-frac w2) (* y2-frac h2))))

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
