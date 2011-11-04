#lang racket/base

;; Functions to create renderers for 3D histograms

(require racket/match racket/list racket/contract racket/class
         plot/utils
         "../common/contract-doc.rkt")

(provide (all-defined-out))

;; ===================================================================================================
;; Rectangles

(define ((rectangles3d-render-proc rects color style line-color line-width line-style alpha label)
         area)
  (send area put-pen line-color line-width line-style)
  (send area put-brush color style)
  (send area put-alpha alpha)
  (for ([rect  (in-list rects)])
    (send area put-rect rect))
  
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
          [#:line-width line-width (>=/c 0) (rectangle3d-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (match-define (list (vector (ivl x1s x2s) (ivl y1s y2s) (ivl z1s z2s)) ...) rects)
  (define rxs (filter regular? (append x1s x2s)))
  (define rys (filter regular? (append y1s y2s)))
  (define rzs (filter regular? (append z1s z2s)))
  (cond
    [(or (empty? rxs) (empty? rys) (empty? rzs))  (renderer3d #f #f #f #f)]
    [else
     (let ([x-min  (if x-min x-min (apply min* rxs))]
           [x-max  (if x-max x-max (apply max* rxs))]
           [y-min  (if y-min y-min (apply min* rys))]
           [y-max  (if y-max y-max (apply max* rys))]
           [z-min  (if z-min z-min (apply min* rzs))]
           [z-max  (if z-max z-max (apply max* rzs))])
       (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                   default-ticks-fun
                   (rectangles3d-render-proc rects color style line-color line-width line-style
                                             alpha label)))]))

;; ===================================================================================================
;; Discrete histograms

(define ((discrete-histogram3d-ticks-fun c1s c2s tick-xs tick-ys x-far-ticks? y-far-ticks?) r)
  (match-define (vector _xi _yi (ivl z-min z-max)) r)
  (define-values (x-ticks x-far-ticks)
    (let ([ts  (for/list ([cat  (in-list c1s)] [x  (in-list tick-xs)])
                 (tick x #t (->plot-label cat)))])
      (if x-far-ticks? (values empty ts) (values ts empty))))
  (define-values (y-ticks y-far-ticks)
    (let ([ts  (for/list ([cat  (in-list c2s)] [y  (in-list tick-ys)])
                 (tick y #t (->plot-label cat)))])
      (if y-far-ticks? (values empty ts) (values ts empty))))
  (values x-ticks x-far-ticks
          y-ticks y-far-ticks
          (default-z-ticks z-min z-max) (default-z-far-ticks z-min z-max)))

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
          [#:line-width line-width (>=/c 0) (rectangle3d-line-width)]
          [#:line-style line-style plot-pen-style/c (rectangle-line-style)]
          [#:alpha alpha (real-in 0 1) (rectangle-alpha)]
          [#:label label (or/c string? #f) #f]
          [#:x-far-ticks? x-far-ticks? boolean? #f]
          [#:y-far-ticks? y-far-ticks? boolean? #f]
          ) renderer3d?
  (match-define (list (vector cat1s cat2s zs) ...) cat-vals)
  (define rzs (filter regular? zs))
  (cond
    [(empty? rzs)  (renderer3d #f #f #f #f)]
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
       (renderer3d (vector (ivl x-min x-max) (ivl y-min y-max) (ivl z-min z-max)) #f
                   (discrete-histogram3d-ticks-fun c1s c2s tick-xs tick-ys x-far-ticks? y-far-ticks?)
                   (rectangles3d-render-proc rects color style line-color line-width line-style
                                             alpha label)))]))
