#lang racket/base

;; The histogram renderer.

(require racket/match racket/contract racket/class racket/list
         unstable/latent-contract/defthing
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Rectangles

(define ((rectangles-render-proc rects color style line-color line-width line-style alpha label)
         area)
  (send area put-pen line-color line-width line-style)
  (send area put-brush color style)
  (send area put-alpha alpha)
  (for ([rect  (in-list rects)])
    (send area put-rect rect))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else  empty]))

(defproc (rectangles
          [rects  (listof (vector/c ivl? ivl?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (match-define (list (vector (ivl x1s x2s) (ivl y1s y2s)) ...) rects)
  (define rxs (filter rational? (append x1s x2s)))
  (define rys (filter rational? (append y1s y2s)))
  (cond
    [(or (empty? rxs) (empty? rys))  (renderer2d #f #f #f #f)]
    [else
     (let ([x-min  (if x-min x-min (apply min* rxs))]
           [x-max  (if x-max x-max (apply max* rxs))]
           [y-min  (if y-min y-min (apply min* rys))]
           [y-max  (if y-max y-max (apply max* rys))])
       (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                   (rectangles-render-proc rects color style line-color line-width line-style alpha
                                           label)))]))

;; ===================================================================================================
;; Real histograms (or histograms on the real line)

(defproc (area-histogram
          [f (real? . -> . real?)]
          [bin-bounds (listof real?)]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (let* ([bin-bounds  (filter rational? bin-bounds)]
         [bin-bounds  (sort bin-bounds <)])
    (cond
      [((length bin-bounds) . < . 2)  (renderer2d #f #f #f #f)]
      [else
       (define xs (linear-seq (apply min* bin-bounds) (apply max* bin-bounds) samples
                              #:start? #f #:end? #f))
       (define xss (bin-samples bin-bounds xs))
       (define heights (for/list ([xs  (in-list xss)]
                                  [x1  (in-list bin-bounds)]
                                  [x2  (in-list (rest bin-bounds))])
                         (define x-size (- x2 x1))
                         (define ys (map f xs))
                         (/ (apply + ys) (length xs))))
       (rectangles (map (λ (x-ivl h) (vector x-ivl (ivl 0 h)))
                        (bounds->intervals bin-bounds) heights)
                   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
                   #:color color #:style style #:line-color line-color #:line-width line-width
                   #:line-style line-style #:alpha alpha #:label label)])))

;; ===================================================================================================
;; Discrete histograms

(define ((discrete-histogram-ticks-fun cats tick-xs far-ticks? maybe-invert) r)
  (match-define (vector _ (ivl y-min y-max)) (apply maybe-invert (vector->list r)))
  (define-values (x-ticks x-far-ticks)
    (let ([ticks  (for/list ([cat  (in-list cats)] [x  (in-list tick-xs)])
                    (tick x #t (->plot-label cat)))])
      (if far-ticks? (values empty ticks) (values ticks empty))))
  (match-let*
      ([(vector plot-x-ticks plot-y-ticks)          (maybe-invert (plot-x-ticks)
                                                                  (plot-y-ticks))]
       [(vector plot-x-far-ticks plot-y-far-ticks)  (maybe-invert (plot-x-far-ticks)
                                                                  (plot-y-far-ticks))]
       [(vector x-ticks y-ticks)          (maybe-invert x-ticks (plot-y-ticks y-min y-max))]
       [(vector x-far-ticks y-far-ticks)  (maybe-invert x-far-ticks (plot-y-far-ticks y-min y-max))])
    (values x-ticks x-far-ticks y-ticks y-far-ticks)))

(defproc (discrete-histogram
          [cat-vals (listof (vector/c any/c (or/c real? ivl? #f)))]
          [#:x-min x-min (or/c rational? #f) 0] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:skip skip (>=/c 0) (discrete-histogram-skip)]
          [#:invert? invert? boolean? (discrete-histogram-invert?)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          [#:far-ticks? far-ticks? boolean? #f]
          ) renderer2d?
  (match-define (list (vector cats ys) ...) cat-vals)
  (define rys (filter rational? (append* (for/list ([y  (in-list ys)])
                                           (match y
                                             [(ivl y1 y2)  (list y1 y2)]
                                             [_            (list y)])))))
  (cond
    [(empty? rys)  (renderer2d #f #f #f #f)]
    [else
     (define n (length cats))
     (let* ([x-min  (if x-min x-min 0)]
            [x-max  (if x-max x-max (+ x-min (* n skip)))]
            [y-min  (if y-min y-min (apply min* rys))]
            [y-max  (if y-max y-max (apply max* rys))])
       (define xs (linear-seq x-min x-max (add1 n)))
       (define x-ivls (for/list ([x1  (in-list xs)] [x2  (in-list (rest xs))])
                        (define 1/2-gap-size (+ (* 1/2 (- skip 1)) (* 1/2 gap (- x2 x1))))
                        (ivl (+ x1 1/2-gap-size) (- x2 1/2-gap-size))))
       (define tick-xs (linear-seq x-min x-max n #:start? #f #:end? #f))
       (define y-ivls (map (λ (y) (if (ivl? y) y (ivl 0 y))) ys))
       (define maybe-invert (if invert? (λ (x y) (vector y x)) vector))
       (renderer2d
        (maybe-invert (ivl x-min x-max) (ivl y-min y-max)) #f
        (discrete-histogram-ticks-fun cats tick-xs far-ticks? maybe-invert)
        (rectangles-render-proc (map maybe-invert x-ivls y-ivls)
                                color style line-color line-width line-style alpha label)))]))

(defproc (stacked-histogram
          [cat-vals (listof (vector/c any/c (listof real?)))]
          [#:x-min x-min (or/c rational? #f) 0] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) 0] [#:y-max y-max (or/c rational? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:skip skip (>=/c 0) (discrete-histogram-skip)]
          [#:invert? invert? boolean? (discrete-histogram-invert?)]
          [#:colors colors (plot-colors/c nat/c) (stacked-histogram-colors)]
          [#:styles styles (plot-brush-styles/c nat/c) (stacked-histogram-styles)]
          [#:line-colors line-colors (plot-colors/c nat/c) (stacked-histogram-line-colors)]
          [#:line-widths line-widths (pen-widths/c nat/c) (stacked-histogram-line-widths)]
          [#:line-styles line-styles (plot-pen-styles/c nat/c) (stacked-histogram-line-styles)]
          [#:alphas alphas (alphas/c nat/c) (stacked-histogram-alphas)]
          [#:labels labels (labels/c nat/c) '(#f)]
          [#:far-ticks? far-ticks? boolean? #f]
          ) (listof renderer2d?)
  (match-define (list (vector cats ys) ...) cat-vals)
  (define yss (map cumulative-sum ys))
  (define y-ivlss (for/list ([ys  (in-list yss)])
                    (for/list ([y1  (in-list ys)] [y2  (in-list (rest ys))])
                      (ivl y1 y2))))
  (define max-num (apply max (map length yss)))
  (for/list ([y-ivls  (in-list (transpose y-ivlss))]
             [color   (in-cycle (maybe-apply colors max-num))]
             [style   (in-cycle (maybe-apply styles max-num))]
             [line-color  (in-cycle (maybe-apply line-colors max-num))]
             [line-width  (in-cycle (maybe-apply line-widths max-num))]
             [line-style  (in-cycle (maybe-apply line-styles max-num))]
             [alpha   (in-cycle (maybe-apply alphas max-num))]
             [label   (in-cycle (maybe-apply labels max-num))])
    (discrete-histogram
     (map vector cats y-ivls)
     #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
     #:gap gap #:skip skip #:invert? invert?
     #:color color #:style style #:line-color line-color #:line-width line-width
     #:line-style line-style #:alpha alpha #:label label #:far-ticks? far-ticks?)))
