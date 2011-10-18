#lang racket/base

;; The histogram renderer.

(require racket/match racket/contract racket/class racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/format.rkt"
         "../common/ticks.rkt"
         "../common/contract.rkt"
         "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/parameters.rkt"
         "../common/renderer.rkt")

(provide rectangles area-histogram discrete-histogram)

;; ===================================================================================================
;; Rectangles

(define ((rectangles-render-proc rects color style line-color line-width line-style alpha label)
         area)
  (send area set-pen line-color line-width line-style)
  (send area set-brush color style)
  (send area set-alpha alpha)
  (for ([rect  (in-list rects)])
    (match-define (vector (ivl x1 x2) (ivl y1 y2)) rect)
    (send area put-rectangle (vector x1 y1) (vector x2 y2)))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else  empty]))

(defproc (rectangles
          [rects  (listof (vector/c ivl? ivl?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (match-define (list (vector (ivl x1s x2s) (ivl y1s y2s)) ...) rects)
  (define rxs (filter regular? (append x1s x2s)))
  (define rys (filter regular? (append y1s y2s)))
  (cond
    [(or (empty? rxs) (empty? rys))  null-renderer2d]
    [else
     (let ([x-min  (if x-min x-min (apply min* rxs))]
           [x-max  (if x-max x-max (apply max* rxs))]
           [y-min  (if y-min y-min (apply min* rys))]
           [y-max  (if y-max y-max (apply max* rys))])
       (renderer2d
        (vector (ivl x-min x-max) (ivl y-min y-max))
        null-bounds-fun
        default-ticks-fun
        (rectangles-render-proc rects color style line-color line-width line-style alpha label)))]))

;; ===================================================================================================
;; Real histograms (or histograms on the real line)

(defproc (area-histogram
          [f (real? . -> . real?)]
          [bin-bounds (listof real?)]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) 0] [#:y-max y-max (or/c real? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (line-samples)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (let* ([bin-bounds  (filter regular? bin-bounds)]
         [bin-bounds  (sort bin-bounds <)])
    (cond
      [((length bin-bounds) . < . 2)  null-renderer2d]
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

(define ((discrete-histogram-ticks-fun cats tick-xs) r)
  (match-define (vector _ (ivl y-min y-max)) r)
  (define x-ticks
    (for/list ([cat  (in-list cats)] [x  (in-list tick-xs)])
      (tick x #t (->plot-label cat))))
  (values x-ticks (default-y-ticks y-min y-max)))

(defproc (discrete-histogram
          [cat-vals (listof (vector/c any/c real?))]
          [#:x-min x-min (or/c real? #f) 0] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) 0] [#:y-max y-max (or/c real? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (>=/c 0) (rectangle-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (match-define (list (vector cats ys) ...) cat-vals)
  (define rys (filter regular? ys))
  (cond
    [(empty? rys)  null-renderer2d]
    [else
     (define n (length cats))
     (let* ([x-min  (if x-min x-min 0)]
            [x-max  (if x-max x-max (+ x-min n))]
            [y-min  (if y-min y-min (apply min* rys))]
            [y-max  (if y-max y-max (apply max* rys))])
       (define xs (linear-seq x-min x-max (add1 n)))
       (define x-ivls (for/list ([x1  (in-list xs)] [x2  (in-list (rest xs))])
                        (define 1/2-gap-size (* 1/2 gap (- x2 x1)))
                        (ivl (+ x1 1/2-gap-size) (- x2 1/2-gap-size))))
       (define tick-xs (linear-seq x-min x-max n #:start? #f #:end? #f))
       (renderer2d
        (vector (ivl x-min x-max) (ivl y-min y-max))
        null-bounds-fun
        (discrete-histogram-ticks-fun cats tick-xs)
        (rectangles-render-proc (map (λ (x-ivl y) (vector x-ivl (ivl 0 y))) x-ivls ys)
                                color style line-color line-width line-style alpha label)))]))
