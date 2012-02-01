#lang typed/racket/base

(require racket/flonum
         (except-in racket/fixnum fl->fx fx->fl)
         racket/match
         "flonum.rkt"
         "flomap-struct.rkt")

(provide flomap-flip-horizontal flomap-flip-vertical flomap-transpose
         flomap-cw-rotate flomap-ccw-rotate
         invertible-2d-function Flomap-Transform
         flomap-transform rotate-transform
         )

(: flomap-flip-horizontal (flomap -> flomap))
(define (flomap-flip-horizontal fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (fx- w 1))
  (inline-build-flomap c w h (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k (fx- w-1 x) y)))))

(define (flomap-flip-vertical fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (fx- h 1))
  (inline-build-flomap c w h (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k x (fx- h-1 y))))))

(define (flomap-transpose fm)
  (match-define (flomap vs c w h) fm)
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k y x)))))

(define (flomap-cw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define h-1 (fx- h 1))
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k (fx- h-1 y) x)))))

(define (flomap-ccw-rotate fm)
  (match-define (flomap vs c w h) fm)
  (define w-1 (fx- w 1))
  (inline-build-flomap c h w (λ (k x y _i)
                               (unsafe-flvector-ref vs (coords->index c w k y (fx- w-1 x))))))

(struct: invertible-2d-function ([f : (Flonum Flonum -> (values Flonum Flonum))]
                                 [g : (Flonum Flonum -> (values Flonum Flonum))]))

(define-type Flomap-Transform (Integer Integer -> invertible-2d-function))

(: flomap-transform (case-> (flomap Flomap-Transform -> flomap)
                            (flomap Flomap-Transform Real Real Real Real -> flomap)))
(define flomap-transform
  (case-lambda
    [(fm t)  
     (match-define (flomap vs c w h) fm)
     (match-define (invertible-2d-function f g) (t w h))
     (define x-min +inf.0)
     (define x-max -inf.0)
     (define y-min +inf.0)
     (define y-max -inf.0)
     (let: y-loop : Void ([y : Integer  0])
       (when (y . fx< . h)
         (let: x-loop : Void ([x : Integer  0])
           (cond [(x . fx< . w)
                  (define-values (new-x new-y) (f (+ 0.5 (fx->fl x)) (+ 0.5 (fx->fl y))))
                  (when (new-x . < . x-min) (set! x-min new-x))
                  (when (new-x . > . x-max) (set! x-max new-x))
                  (when (new-y . < . y-min) (set! y-min new-y))
                  (when (new-y . > . y-max) (set! y-max new-y))
                  (x-loop (fx+ x 1))]
                 [else
                  (y-loop (fx+ y 1))]))))
     (flomap-transform fm t (- x-min 0.5) (+ x-max 0.5) (- y-min 0.5) (+ y-max 0.5))]
    [(fm t x-min x-max y-min y-max)
     (let ([x-min  (exact->inexact x-min)]
           [x-max  (exact->inexact x-max)]
           [y-min  (exact->inexact y-min)]
           [y-max  (exact->inexact y-max)])
       (match-define (flomap vs c w h) fm)
       (match-define (invertible-2d-function f g) (t w h))
       (define int-x-min (fl->fx (floor x-min)))
       (define int-x-max (fl->fx (ceiling x-max)))
       (define int-y-min (fl->fx (floor y-min)))
       (define int-y-max (fl->fx (ceiling y-max)))
       (define new-w (- int-x-max int-x-min))
       (define new-h (- int-y-max int-y-min))
       (define x-offset (+ 0.5 (fx->fl int-x-min)))
       (define y-offset (+ 0.5 (fx->fl int-y-min)))
       (inline-build-flomap
        c new-w new-h
        (λ (k x y _i)
          (define-values (old-x old-y) (g (+ (fx->fl x) x-offset)
                                          (+ (fx->fl y) y-offset)))
          (flomap-bilinear-ref fm k old-x old-y))))]))

(: rotate-transform (Real -> Flomap-Transform))
(define ((rotate-transform θ) w h)
  (let ([θ  (- (exact->inexact θ))])
    (define cos-θ (cos θ))
    (define sin-θ (sin θ))
    (define x-mid (* 0.5 (fx->fl w)))
    (define y-mid (* 0.5 (fx->fl h)))
    (invertible-2d-function
     (λ: ([x : Flonum] [y : Flonum])
       (let ([x  (- x x-mid)]
             [y  (- y y-mid)])
         (values (+ x-mid (- (* x cos-θ) (* y sin-θ)))
                 (+ y-mid (+ (* x sin-θ) (* y cos-θ))))))
     (λ: ([x : Flonum] [y : Flonum])
       (let ([x  (- x x-mid)]
             [y  (- y y-mid)])
         (values (+ x-mid (+ (* x cos-θ) (* y sin-θ)))
                 (+ y-mid (- (* y cos-θ) (* x sin-θ)))))))))
