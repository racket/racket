#lang racket/base

;; Renderers for points and other point-like things.

(require racket/contract racket/class racket/match racket/math racket/list racket/sequence
         unstable/latent-contract/defthing
         unstable/contract
         plot/utils
         "../common/utils.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Points (scatter plots)

(define ((points-render-fun vs sym color fill-color size line-width alpha label) area)
  (send area put-alpha alpha)
  (send area put-pen color line-width 'solid)
  (send area put-brush fill-color 'solid)
  (send area put-glyphs vs sym size)
  
  (if label (point-legend-entry label sym color fill-color size line-width) empty))

(defproc (points [vs  (sequence/c (sequence/c real?))]
                 [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
                 [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
                 [#:sym sym point-sym/c (point-sym)]
                 [#:color color plot-color/c (point-color)]
                 [#:fill-color fill-color (or/c plot-color/c 'auto) 'auto]
                 [#:size size (>=/c 0) (point-size)]
                 [#:line-width line-width (>=/c 0) (point-line-width)]
                 [#:alpha alpha (real-in 0 1) (point-alpha)]
                 [#:label label (or/c string? #f) #f]
                 ) renderer2d?
  (let* ([vs  (sequence->listof-vector 'points vs 2)]
         [vs  (filter vrational? vs)])
    (cond
      [(empty? vs)  (renderer2d #f #f #f #f)]
      [else  (match-define (list (vector xs ys) ...) vs)
             (let ([x-min  (if x-min x-min (apply min* xs))]
                   [x-max  (if x-max x-max (apply max* xs))]
                   [y-min  (if y-min y-min (apply min* ys))]
                   [y-max  (if y-max y-max (apply max* ys))])
               (renderer2d
                (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                (points-render-fun vs sym color (cond [(eq? fill-color 'auto)  (->pen-color color)]
                                                      [else  fill-color])
                                   size line-width alpha label)))])))

;; ===================================================================================================
;; Vector fields

(define ((vector-field-render-fun f samples scale color line-width line-style alpha label) area)
  (match-define (vector (ivl x-min x-max) (ivl y-min y-max)) (send area get-bounds-rect))
  
  (define xs0 (linear-seq x-min x-max samples #:start? #t #:end? #t))
  (define ys0 (linear-seq y-min y-max samples #:start? #t #:end? #t))
  
  (define-values (xs ys dxs dys angles mags)
    (for*/lists (xs ys dxs dys angles mags) ([x   (in-list xs0)]
                                             [y   (in-list ys0)]
                                             [dv  (in-value (f x y))] #:when (vrational? dv))
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
              
              (send area put-alpha alpha)
              (send area put-pen color line-width line-style)
              (for ([x      (in-list xs)]
                    [y      (in-list ys)]
                    [angle  (in-list angles)]
                    [mag    (in-list new-mags)])
                (send area put-arrow
                      (vector x y)
                      (vector (+ x (* mag (cos angle))) (+ y (* mag (sin angle))))))
              
              (cond [label  (arrow-legend-entry label color line-width line-style)]
                    [else   empty])]))

(defproc (vector-field
          [f (or/c (real? real? . -> . (sequence/c real?))
                   ((vector/c real? real?) . -> . (sequence/c real?)))]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:samples samples exact-positive-integer? (vector-field-samples)]
          [#:scale scale (or/c real? (one-of/c 'auto 'normalized)) (vector-field-scale)]
          [#:color color plot-color/c (vector-field-color)]
          [#:line-width line-width (>=/c 0) (vector-field-line-width)]
          [#:line-style line-style plot-pen-style/c (vector-field-line-style)]
          [#:alpha alpha (real-in 0 1) (vector-field-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (let ([f  (cond [(procedure-arity-includes? f 2 #t)
                   (λ (x y) (sequence-head-vector 'vector-field (f x y) 2))]
                  [else
                   (λ (x y) (sequence-head-vector 'vector-field (f (vector x y)) 2))])])
    (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                (vector-field-render-fun f samples scale color line-width line-style alpha label))))

;; ===================================================================================================
;; Error bars

(define ((error-bars-render-fun xs ys hs color line-width line-style width alpha) area)
  (define clip-rect (send area get-clip-rect))
  (define radius (* 1/2 width))
  
  (send area put-alpha alpha)
  (send area put-pen color line-width line-style)
  (for ([x  (in-list xs)] [y  (in-list ys)] [h  (in-list hs)])
    (when (rect-contains? clip-rect (vector x y))
      (define v1 (vector x (- y h)))
      (define v2 (vector x (+ y h)))
      (send area put-line v1 v2)
      (send area put-tick v1 radius 0)
      (send area put-tick v2 radius 0)))
  
  empty)

(defproc (error-bars
          [bars (sequence/c (sequence/c real?))]
          [#:x-min x-min (or/c rational? #f) #f] [#:x-max x-max (or/c rational? #f) #f]
          [#:y-min y-min (or/c rational? #f) #f] [#:y-max y-max (or/c rational? #f) #f]
          [#:color color plot-color/c (error-bar-color)]
          [#:line-width line-width (>=/c 0) (error-bar-line-width)]
          [#:line-style line-style plot-pen-style/c (error-bar-line-style)]
          [#:width width (>=/c 0) (error-bar-width)]
          [#:alpha alpha (real-in 0 1) (error-bar-alpha)]
          ) renderer2d?
  (let* ([bars  (sequence->listof-vector 'error-bars bars 3)]
         [bars  (filter vrational? bars)])
    (cond [(empty? bars)  (renderer2d #f #f #f #f)]
          [else
           (match-define (list (vector xs ys hs) ...) bars)
           (let ([x-min  (if x-min x-min (apply min* xs))]
                 [x-max  (if x-max x-max (apply max* xs))]
                 [y-min  (if y-min y-min (apply min* (map - ys hs)))]
                 [y-max  (if y-max y-max (apply max* (map + ys hs)))])
             (renderer2d (vector (ivl x-min x-max) (ivl y-min y-max)) #f default-ticks-fun
                         (error-bars-render-fun xs ys hs
                                                color line-width line-style width alpha)))])))
