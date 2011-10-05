#lang racket/base

;; Functions to create renderers for 3D histograms

(require racket/match racket/list racket/contract racket/class
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/parameters.rkt"
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/legend.rkt"
         "../common/ticks.rkt"
         "../common/format.rkt"
         "renderer.rkt")

(provide rectangles3d discrete-histogram3d)

;; ===================================================================================================
;; Rectangles

(define ((rectangles3d-render-proc rects color style line-color line-width line-style alpha label)
         area)
  (send area put-pen line-color line-width line-style)
  (send area put-brush color style)
  (send area put-alpha alpha)
  (for ([rect  (in-list rects)])
    (match-define (vector (ivl x1 x2) (ivl y1 y2) (ivl z1 z2)) rect)
    (send area put-box (vector x1 y1 z1) (vector x2 y2 z2)))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else  empty]))

(defproc (rectangles3d
          [rects  (listof (vector/c ivl? ivl? ivl?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (real>=/c 0) (rectangle3d-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (match-define (list (vector (ivl x1s x2s) (ivl y1s y2s) (ivl z1s z2s)) ...) rects)
  (define rxs (filter regular? (append x1s x2s)))
  (define rys (filter regular? (append y1s y2s)))
  (define rzs (filter regular? (append z1s z2s)))
  (cond
    [(or (empty? rxs) (empty? rys) (empty? rzs))  null-renderer3d]
    [else
     (let ([x-min  (if x-min x-min (apply min* rxs))]
           [x-max  (if x-max x-max (apply max* rxs))]
           [y-min  (if y-min y-min (apply min* rys))]
           [y-max  (if y-max y-max (apply max* rys))]
           [z-min  (if z-min z-min (apply min* rzs))]
           [z-max  (if z-max z-max (apply max* rzs))])
       (renderer3d (rectangles3d-render-proc rects color style line-color line-width line-style
                                             alpha label)
                   default-3d-ticks-fun
                   null-3d-bounds-fun
                   x-min x-max y-min y-max z-min z-max))]))

;; ===================================================================================================
;; Discrete histograms

(define ((discrete-histogram3d-ticks-fun c1s c2s tick-xs tick-ys)
         _x-min _x-max _y-min _y-max z-min z-max)
  (define x-ticks
    (for/list ([cat  (in-list c1s)] [x  (in-list tick-xs)])
      (tick x (->plot-label cat) #t)))
  (define y-ticks
    (for/list ([cat  (in-list c2s)] [y  (in-list tick-ys)])
      (tick y (->plot-label cat) #t)))
  (values x-ticks y-ticks (default-ticks-fun z-min z-max)))

(define (adjust/gap i gap)
  (match-define (ivl x1 x2) i)
  (define 1/2-gap-size (* 1/2 gap (- x2 x1)))
  (ivl (+ x1 1/2-gap-size) (- x2 1/2-gap-size)))

(defproc (discrete-histogram3d
          [cat-vals (listof (vector/c any/c any/c real?))]
          [#:x-min x-min (or/c real? #f) 0] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) 0] [#:y-max y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) 0] [#:z-max z-max (or/c real? #f) #f]
          [#:gap gap (real-in 0 1) (discrete-histogram-gap)]
          [#:color color plot-color/c (rectangle-color)]
          [#:style style plot-brush-style/c (rectangle-style)]
          [#:line-color line-color plot-color/c (rectangle-line-color)]
          [#:line-width line-width (real>=/c 0) (rectangle3d-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (match-define (list (vector cat1s cat2s zs) ...) cat-vals)
  (define rzs (filter regular? zs))
  (cond
    [(empty? rzs)  null-renderer3d]
    [else
     (define c1s (remove-duplicates cat1s))
     (define c2s (remove-duplicates cat2s))
     (define x-num (length c1s))
     (define y-num (length c2s))
     (let* ([x-min  (if x-min x-min 0)]
            [x-max  (if x-max x-max (+ x-min x-num))]
            [y-min  (if y-min y-min 0)]
            [y-max  (if y-max y-max (+ y-min y-num))]
            [z-min  (if z-min z-min (apply min* rzs))]
            [z-max  (if z-max z-max (apply max* rzs))])
       (define xs (linear-seq x-min x-max (add1 x-num)))
       (define ys (linear-seq y-min y-max (add1 y-num)))
       (define h (make-hash (for/list ([c1 (in-list cat1s)] [c2 (in-list cat2s)] [z (in-list zs)])
                              (cons (cons c1 c2) z))))
       (match-define (list (vector x1s x2s y1s y2s all-zs) ...)
         (for/list ([y1  (in-list ys)]
                    [y2  (in-list (rest ys))]
                    [c2  (in-list c2s)]
                    #:when #t
                    [x1  (in-list xs)]
                    [x2  (in-list (rest xs))]
                    [c1  (in-list c1s)])
           (vector x1 x2 y1 y2 (hash-ref h (cons c1 c2) +nan.0))))
       (define tick-xs (linear-seq x-min x-max x-num #:start? #f #:end? #f))
       (define tick-ys (linear-seq y-min y-max y-num #:start? #f #:end? #f))
       (define rects (map (λ (x1 x2 y1 y2 z)
                            (vector (adjust/gap (ivl x1 x2) gap)
                                    (adjust/gap (ivl y1 y2) gap)
                                    (ivl 0 z)))
                          x1s x2s y1s y2s all-zs))
       (renderer3d (rectangles3d-render-proc rects color style line-color line-width line-style
                                             alpha label)
                   (discrete-histogram3d-ticks-fun c1s c2s tick-xs tick-ys)
                   null-3d-bounds-fun
                   x-min x-max y-min y-max z-min z-max))]))
