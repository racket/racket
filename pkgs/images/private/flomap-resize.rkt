#lang typed/racket/base

(require racket/match racket/math
         (only-in racket/unsafe/ops unsafe-fx+)
         "flonum.rkt"
         "flomap-struct.rkt"
         "flomap-stats.rkt"
         "flomap-blur.rkt")

(provide flomap-copy subflomap flomap-trim flomap-inset flomap-crop
         flomap-lt-crop flomap-lc-crop flomap-lb-crop
         flomap-ct-crop flomap-cc-crop flomap-cb-crop
         flomap-rt-crop flomap-rc-crop flomap-rb-crop
         flomap-scale flomap-resize)

(: flomap-copy (flomap Integer Integer Integer Integer -> flomap))
(define (flomap-copy fm x-start y-start x-end y-end)
  (match-define (flomap src-vs c src-w src-h) fm)
  (define dst-w (max 0 (- x-end x-start)))
  (define dst-h (max 0 (- y-end y-start)))
  (define new-fm (make-flomap c dst-w dst-h))
  (define dst-vs (flomap-values new-fm))
  (when (and (dst-w . > . 0) (dst-h . > . 0))
    (let: y-loop : Void ([dst-y : Nonnegative-Fixnum  0])
      (when (dst-y . fx< . dst-h)
        (define src-y (fx+ dst-y y-start))
        (when (and (src-y . fx>= . 0) (src-y . fx< . src-h))
          (let: x-loop : Void ([dst-x : Nonnegative-Fixnum  0])
            (when (dst-x . fx< . dst-w)
              (define src-x (fx+ dst-x x-start))
              (when (and (src-x . fx>= . 0) (src-x . fx< . src-w))
                (let: k-loop : Void ([k : Nonnegative-Fixnum  0])
                  (when (k . fx< . c)
                    (define src-i (coords->index c src-w k src-x src-y))
                    (define dst-i (coords->index c dst-w k dst-x dst-y))
                    (flvector-set! dst-vs dst-i (flvector-ref src-vs src-i))
                    (k-loop (unsafe-fx+ k 1)))))
              (x-loop (unsafe-fx+ dst-x 1)))))
        (y-loop (unsafe-fx+ dst-y 1)))))
  new-fm)

(: subflomap (flomap Integer Integer Integer Integer -> flomap))
(define (subflomap fm x-start y-start x-end y-end)
  (match-define (flomap _ _ src-w src-h) fm)
  (cond [(and (= x-start 0) (= y-start 0) (= x-end src-w) (= y-end src-h))  fm]
        [else  (flomap-copy fm x-start y-start x-end y-end)]))

(: flomap-trim (case-> (flomap -> flomap)
                       (flomap Boolean -> flomap)))
(define flomap-trim
  (case-lambda
    [(fm)  (flomap-trim fm #t)]
    [(fm alpha?)
     (cond [(= (flomap-components fm) 0)  (make-flomap 0 0 0)]
           [else
            (define-values (x-start y-start x-end y-end)
              (flomap-nonzero-rect (if alpha? (flomap-ref-component fm 0) fm)))
            (subflomap fm x-start y-start x-end y-end)])]))

(: flomap-inset (case-> (flomap Integer -> flomap)
                        (flomap Integer Integer -> flomap)
                        (flomap Integer Integer Integer Integer -> flomap)))
(define flomap-inset
  (case-lambda
    [(fm amt)          (flomap-inset fm amt amt amt amt)]
    [(fm h-amt v-amt)  (flomap-inset fm h-amt v-amt h-amt v-amt)]
    [(fm l-amt t-amt r-amt b-amt)
     (match-define (flomap _ _ w h) fm)
     (subflomap fm (- l-amt) (- t-amt) (+ w r-amt) (+ h b-amt))]))

(: flomap-crop (flomap Integer Integer Real Real -> flomap))
(define (flomap-crop fm width height x-frac y-frac)
  (unless (width . >= . 0)
    (raise-type-error 'flomap-crop "nonnegative integer" 1 fm width height x-frac y-frac))
  (unless (height . >= . 0)
    (raise-type-error 'flomap-crop "nonnegative integer" 2 fm width height x-frac y-frac))
  (let ([x-frac  (real->double-flonum x-frac)]
        [y-frac  (real->double-flonum y-frac)])
    (match-define (flomap _ c w h) fm)
    (define l-amt (fl->fx (round (* x-frac (fx->fl (fx- width w))))))
    (define r-amt (fx- (fx- width w) l-amt))
    (define t-amt (fl->fx (round (* y-frac (fx->fl (fx- height h))))))
    (define b-amt (fx- (fx- height h) t-amt))
    (flomap-inset fm l-amt t-amt r-amt b-amt)))

(: flomap-lt-crop (flomap Integer Integer -> flomap))
(: flomap-lc-crop (flomap Integer Integer -> flomap))
(: flomap-lb-crop (flomap Integer Integer -> flomap))
(: flomap-ct-crop (flomap Integer Integer -> flomap))
(: flomap-cc-crop (flomap Integer Integer -> flomap))
(: flomap-cb-crop (flomap Integer Integer -> flomap))
(: flomap-rt-crop (flomap Integer Integer -> flomap))
(: flomap-rc-crop (flomap Integer Integer -> flomap))
(: flomap-rb-crop (flomap Integer Integer -> flomap))

(define (flomap-lt-crop fm w h) (flomap-crop fm w h 0 0))
(define (flomap-lc-crop fm w h) (flomap-crop fm w h 0 1/2))
(define (flomap-lb-crop fm w h) (flomap-crop fm w h 0 1))
(define (flomap-ct-crop fm w h) (flomap-crop fm w h 1/2 0))
(define (flomap-cc-crop fm w h) (flomap-crop fm w h 1/2 1/2))
(define (flomap-cb-crop fm w h) (flomap-crop fm w h 1/2 1))
(define (flomap-rt-crop fm w h) (flomap-crop fm w h 1 0))
(define (flomap-rc-crop fm w h) (flomap-crop fm w h 1 1/2))
(define (flomap-rb-crop fm w h) (flomap-crop fm w h 1 1))

(: flomap-scale (case-> (flomap Real -> flomap)
                        (flomap Real Real -> flomap)))
(define flomap-scale
  (case-lambda
    [(fm scale)
     (cond [(< scale 0)  (raise-type-error 'flomap-scale "nonnegative real" 1 fm scale)]
           [else  (flomap-scale fm scale scale)])]
    [(fm x-scale y-scale)
     (cond [(< x-scale 0)  (raise-type-error 'flomap-scale "nonnegative real" 1 fm x-scale y-scale)]
           [(< y-scale 0)  (raise-type-error 'flomap-scale "nonnegative real" 2 fm x-scale y-scale)]
           [else  (flomap-scale-x (flomap-scale-y fm (real->double-flonum y-scale))
                                  (real->double-flonum x-scale))])]))

(: flomap-resize (flomap (Option Integer) (Option Integer) -> flomap))
(define (flomap-resize fm width height)
  (when (and width (width . < . 0))
    (raise-type-error 'flomap-resize "nonnegative integer" 1 fm width height))
  (when (and height (height . < . 0))
    (raise-type-error 'flomap-resize "nonnegative integer" 2 fm width height))
  (match-define (flomap _ c w h) fm)
  (cond [(and width height)  (flomap-resize-x (flomap-resize-y fm height) width)]
        [width   (cond [(= w 0)  (error 'flomap-resize
                                        "cannot proportionally scale ~e×~e flomap's height"
                                        w h)]
                       [else  (define s (real->double-flonum (/ width w)))
                              (flomap-resize-x (flomap-scale-y fm s) width)])]
        [height  (cond [(= h 0)  (error 'flomap-resize
                                        "cannot proportionally scale ~e×~e flomap's width"
                                        w h)]
                       [else  (define s (real->double-flonum (/ height h)))
                              (flomap-scale-x (flomap-resize-y fm height) s)])]
        [else  (error 'flomap-resize "can't happen")]))

(: flomap-scale-x (flomap Float -> flomap))
(define (flomap-scale-x fm scale)
  (match-define (flomap _ c w h) fm)
  (cond [(= 0.0 scale)  (make-flomap c 0 h)]
        [else
         (let ([scale  (abs scale)])
           (flomap-scale*-x fm scale (abs (fl->fx (ceiling (* (real->double-flonum w) scale))))))]))

(: flomap-scale-y (flomap Float -> flomap))
(define (flomap-scale-y fm scale)
  (match-define (flomap _ c w h) fm)
  (cond [(= 0.0 scale)  (make-flomap c w 0)]
        [else
         (let ([scale  (abs scale)])
           (flomap-scale*-y fm scale (abs (fl->fx (ceiling (* (real->double-flonum h) scale))))))]))

(: flomap-resize-x (flomap Integer -> flomap))
(define (flomap-resize-x fm width)
  (match-define (flomap _ c w h) fm)
  (cond [(= 0 width)  (make-flomap c 0 h)]
        [else  (let ([width  (abs width)])
                 (flomap-scale*-x fm (abs (real->double-flonum (/ width w))) width))]))

(: flomap-resize-y (flomap Integer -> flomap))
(define (flomap-resize-y fm height)
  (match-define (flomap _ c w h) fm)
  (cond [(= 0 height)  (make-flomap c w 0)]
        [else  (let ([height  (abs height)])
                 (flomap-scale*-y fm (abs (real->double-flonum (/ height h))) height))]))

;; variance of an unscaled box filter (i.e. f([-1/2,1/2]) = {1}, zero elsewhere)
(define box-filter-variance (/ 1.0 12.0))
;; variance of an unscaled triangle filter (simulates effect of linear interpolation)
(define triangle-filter-variance (/ 1.0 24.0))

;; calculates the standard deviation of downscaling blur, assuming linear interpolation will be
;; carried out on the blurred image
(: stddev-for-scale (Float -> Float))
(define (stddev-for-scale scale)
  (define var (- (/ box-filter-variance (sqr scale))
                 triangle-filter-variance))
  (flsqrt (max 0.0 var)))

(: flomap-scale*-x (flomap Float Exact-Nonnegative-Integer -> flomap))
(define (flomap-scale*-x fm scale width)
  (cond [(scale . = . 1.0)  fm]
        [(scale . > . 1.0)  (flomap-scale*-x/linear fm scale width)]
        [else  (define low-res-fm
                 (flomap-gaussian-blur-x fm (stddev-for-scale scale)))
               (flomap-scale*-x/linear low-res-fm scale width)]))

(: flomap-scale*-y (flomap Float Exact-Nonnegative-Integer -> flomap))
(define (flomap-scale*-y fm scale height)
  (cond [(scale . = . 1.0)  fm]
        [(scale . > . 1.0)  (flomap-scale*-y/linear fm scale height)]
        [else  (define low-res-fm
                 (flomap-gaussian-blur-y fm (stddev-for-scale scale)))
               (flomap-scale*-y/linear low-res-fm scale height)]))

(: flomap-scale*-x/linear (flomap Float Exact-Nonnegative-Integer -> flomap))
(define (flomap-scale*-x/linear fm s new-w)
  (match-define (flomap vs c w h) fm)
  (define w-1 (unsafe-fx+ w -1))
  (inline-build-flomap
   c new-w h
   (λ (k new-x y _i)
     (define scaled-x (- (/ (+ (fx->fl new-x) 0.5) s) 0.5))
     (define floor-scaled-x (floor scaled-x))
     (define x0 (fl->fx floor-scaled-x))
     (cond [(or (x0 . fx< . -1) (x0 . fx>= . w))  0.0]
           [else
            (define i0 (coords->index c w k x0 y))
            (define v0 (cond [(x0 . fx= . -1)  0.0]
                             [else  (flvector-ref vs i0)]))
            (define v1 (cond [(x0 . fx= . w-1)  0.0]
                             [else  (flvector-ref vs (unsafe-fx+ i0 c))]))
            (fl-convex-combination v0 v1 (- scaled-x floor-scaled-x))]))))

(: flomap-scale*-y/linear (flomap Float Exact-Nonnegative-Integer -> flomap))
(define (flomap-scale*-y/linear fm s new-h)
  (match-define (flomap vs c w h) fm)
  (define h-1 (unsafe-fx+ h -1))
  (define cw (* c w))
  (inline-build-flomap
   c w new-h
   (λ (k x new-y _i)
     (define scaled-y (- (/ (+ (fx->fl new-y) 0.5) s) 0.5))
     (define floor-scaled-y (floor scaled-y))
     (define y0 (fl->fx floor-scaled-y))
     (cond [(or (y0 . fx< . -1) (y0 . fx>= . h))  0.0]
           [else
            (define i0 (coords->index c w k x y0))
            (define v0 (cond [(y0 . fx= . -1)  0.0]
                             [else  (flvector-ref vs i0)]))
            (define v1 (cond [(y0 . fx= . h-1)  0.0]
                             [else  (flvector-ref vs (unsafe-fx+ i0 cw))]))
            (fl-convex-combination v0 v1 (- scaled-y floor-scaled-y))]))))
