#lang racket/base

;; Renderers for points and other point-like things.

(require racket/contract racket/class racket/match racket/math racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/draw.rkt"
         "../common/parameters.rkt"
         "renderer.rkt"
         "bounds.rkt"
         "clip.rkt")

(provide points vector-field error-bars)

;; ===================================================================================================
;; Points (scatter plots)

(define ((points-render-fun vs sym color size line-width alpha label) area)
  (send area set-alpha alpha)
  (send area set-pen color line-width 'solid)
  (send area put-glyphs vs sym size)
  
  (if label (point-legend-entry label sym color size line-width) empty))

(defproc (points [vs  (listof (vectorof real?))]
                 [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
                 [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
                 [#:sym sym point-sym/c (point-sym)]
                 [#:color color plot-color/c (point-color)]
                 [#:size size (real>=/c 0) (point-size)]
                 [#:line-width line-width (real>=/c 0) (point-line-width)]
                 [#:alpha alpha (real-in 0 1) (point-alpha)]
                 [#:label label (or/c string? #f) #f]
                 ) renderer2d?
  (let ([vs  (filter vregular? (map vector-take-2 vs))])
    (cond
      [(empty? vs)  null-renderer2d]
      [else  (match-define (list (vector xs ys) ...) vs)
             (let ([x-min  (if x-min x-min (apply min* xs))]
                   [x-max  (if x-max x-max (apply max* xs))]
                   [y-min  (if y-min y-min (apply min* ys))]
                   [y-max  (if y-max y-max (apply max* ys))])
               (renderer2d (points-render-fun vs sym color size line-width alpha label)
                           default-2d-ticks-fun
                           null-2d-bounds-fun
                           x-min x-max y-min y-max))])))

;; ===================================================================================================
;; Vector fields

(define ((vector-field-render-fun f samples scale color line-width line-style alpha label) area)
  (define-values (x-min x-max y-min y-max) (send area get-bounds))
  
  (define xs0 (linear-seq x-min x-max samples #:start? #t #:end? #t))
  (define ys0 (linear-seq y-min y-max samples #:start? #t #:end? #t))
  
  (define-values (xs ys dxs dys angles mags)
    (for*/lists (xs ys dxs dys angles mags) ([x   (in-list xs0)]
                                             [y   (in-list ys0)]
                                             [dv  (in-value (f x y))] #:when (vregular? dv))
      (match-define (vector dx dy) dv)
      (values x y dx dy (atan2 dy dx) (sqrt (+ (sqr dx) (sqr dy))))))
  
  (cond [(empty? xs)  empty]
        [else (define box-x-size (/ (- x-max x-min) samples))
              (define box-y-size (/ (- y-max y-min) samples))
              
              (define new-mags
                (match scale
                  [(? real?)  (map (λ (mag) (* scale mag)) mags)]
                  ['normalized  (define box-size (min box-x-size box-y-size))
                                (build-list (length dxs) (λ _ box-size))]
                  ['auto  (define dx-max (apply max (map abs dxs)))
                          (define dy-max (apply max (map abs dys)))
                          (define scale (min (/ box-x-size dx-max)
                                             (/ box-y-size dy-max)))
                          (map (λ (mag) (* scale mag)) mags)]))
              
              (send area set-alpha alpha)
              (send area set-pen color line-width line-style)
              (for ([x      (in-list xs)]
                    [y      (in-list ys)]
                    [angle  (in-list angles)]
                    [mag    (in-list new-mags)])
                (send area put-arrow
                      (vector x y)
                      (vector (+ x (* mag (cos angle))) (+ y (* mag (sin angle))))))
              
              (cond [label  (vector-field-legend-entry label color line-width line-style)]
                    [else   empty])]))

(defproc (vector-field
          [f (or/c (real? real? . -> . (vector/c real? real?))
                   ((vector/c real? real?) . -> . (vector/c real? real?)))]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 1) (vector-field-samples)]
          [#:scale scale (or/c real? (one-of/c 'auto 'normalized)) (vector-field-scale)]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width (real>=/c 0) (vector-field-line-width)]
          [#:line-style line-style plot-pen-style/c (vector-field-line-style)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (let ([f  (cond [(procedure-arity-includes? f 2 #t)  f]
                  [else  (λ (x y) (f (vector x y)))])])
    (renderer2d (vector-field-render-fun
                 f samples scale color line-width line-style alpha label)
                default-2d-ticks-fun
                null-2d-bounds-fun
                x-min x-max y-min y-max)))

;; ===================================================================================================
;; Error bars

(define ((error-bars-render-fun bars color line-width line-style width alpha) area)
  (match-define (list (vector xs ys hs) ...) bars)
  (define-values (x-min x-max y-min y-max) (send area get-clip-bounds))
  
  (define half (* 1/2 width))
  
  (send area set-alpha alpha)
  (send area set-pen color line-width line-style)
  (for ([x  (in-list xs)] [y  (in-list ys)] [h  (in-list hs)])
    (when (point-in-bounds? (vector x y) x-min x-max y-min y-max)
      (define v1 (vector x (- y h)))
      (define v2 (vector x (+ y h)))
      (send area put-line v1 v2)
      
      (match-define (vector dc-x1 dc-y1) (send area plot->dc v1))
      (match-define (vector dc-x2 dc-y2) (send area plot->dc v2))
      (send area draw-line
            (vector (- dc-x1 half) dc-y1)
            (vector (+ dc-x1 half) dc-y1))
      (send area draw-line
            (vector (- dc-x2 half) dc-y2)
            (vector (+ dc-x2 half) dc-y2))))
  
  empty)

(define (error-bars-bounds-fun bars)
  (let ([bars  (filter vregular? bars)])
    (cond [(empty? bars)  null-2d-bounds-fun]
          [else
           (match-define (list (vector xs ys hs) ...) bars)
           (λ (x-min x-max y-min y-max)
             (let ([x-min  (if x-min x-min (apply min* xs))]
                   [x-max  (if x-max x-max (apply max* xs))]
                   [y-min  (if y-min y-min (apply min* (map - ys hs)))]
                   [y-max  (if y-max y-max (apply max* (map + ys hs)))])
               (values x-min x-max y-min y-max)))])))

(defproc (error-bars
          [bars (listof (vector/c real? real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:color color plot-color/c (error-bar-color)]
          [#:line-width line-width (real>=/c 0) (error-bar-line-width)]
          [#:line-style line-style (real>=/c 0) (error-bar-line-style)]
          [#:width width (real>=/c 0) (error-bar-width)]
          [#:alpha alpha (real-in 0 1) (error-bar-alpha)]
          ) renderer2d?
  (let ([bars  (filter vregular? bars)])
    (cond [(empty? bars)  null-renderer2d]
          [else
           (renderer2d  (error-bars-render-fun bars color line-width line-style width alpha)
                        default-2d-ticks-fun
                        (error-bars-bounds-fun bars)
                        x-min x-max y-min y-max)])))
