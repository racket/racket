#lang racket/base

;; Renderers for intervals between functions.

(require racket/contract racket/class racket/match racket/math racket/list
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/legend.rkt"
         "../common/draw.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "renderer.rkt"
         "bounds.rkt"
         "sample.rkt")

(provide lines-interval parametric-interval polar-interval function-interval inverse-interval)

;; ===================================================================================================
;; Lines, parametric, polar

(define ((lines-interval-render-proc v1s v2s color style
                                     line1-color line1-width line1-style
                                     line2-color line2-width line2-style
                                     alpha label)
         area)
  (send area set-alpha alpha)
  (send area set-pen 0 0 'transparent)
  (send area set-brush color style)
  (send area put-polygon (append v1s (reverse v2s)))
  
  (send area set-pen line1-color line1-width line1-style)
  (send area put-lines v1s)
  
  (send area set-pen line2-color line2-width line2-style)
  (send area put-lines v2s)
  
  (cond [label  (interval-legend-entry label color style 0 0 'transparent
                                       line1-color line1-width line1-style
                                       line2-color line2-width line2-style)]
        [else  empty]))

(defproc (lines-interval
          [v1s (listof (vector/c real? real?))]
          [v2s (listof (vector/c real? real?))]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (real>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (real>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define rvs (filter vregular? (append v1s v2s)))
  (cond
    [(empty? rvs)  null-renderer2d]
    [else
     (match-define (list (vector rxs rys) ...) rvs)
     (let ([x-min  (if x-min x-min (apply min* rxs))]
           [x-max  (if x-max x-max (apply max* rxs))]
           [y-min  (if y-min y-min (apply min* rys))]
           [y-max  (if y-max y-max (apply max* rys))])
       (renderer2d (lines-interval-render-proc v1s v2s color style
                                               line1-color line1-width line1-style
                                               line2-color line2-width line2-style
                                               alpha label)
                   default-2d-ticks-fun
                   null-2d-bounds-fun
                   x-min x-max y-min y-max))]))

(defproc (parametric-interval
          [f1 (real? . -> . (vector/c real? real?))]
          [f2 (real? . -> . (vector/c real? real?))]
          [t-min real?] [t-max real?]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (real>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (real>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (lines-interval
   (sample-parametric f1 t-min t-max samples)
   (sample-parametric f2 t-min t-max samples)
   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
   #:color color #:style style
   #:line1-color line1-color #:line1-width line1-width #:line1-style line1-style
   #:line2-color line2-color #:line2-width line2-width #:line2-style line2-style
   #:alpha alpha #:label label))

(defproc (polar-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [θ-min real? 0] [θ-max real? (* 2 pi)]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (real>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (real>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (lines-interval
   (sample-polar f1 θ-min θ-max samples)
   (sample-polar f2 θ-min θ-max samples)
   #:x-min x-min #:x-max x-max #:y-min y-min #:y-max y-max
   #:color color #:style style
   #:line1-color line1-color #:line1-width line1-width #:line1-style line1-style
   #:line2-color line2-color #:line2-width line2-width #:line2-style line2-style
   #:alpha alpha #:label label))

;; ===================================================================================================
;; Function

(define ((function-interval-render-proc f1 f2 samples color style
                                        line1-color line1-width line1-style
                                        line2-color line2-width line2-style
                                        alpha label)
         area)
  (define x-min (send area get-x-min))
  (define x-max (send area get-x-max))
  (match-define (list x1s y1s) (f1 x-min x-max samples))
  (match-define (list x2s y2s) (f2 x-min x-max samples))
  (define v1s (map vector x1s y1s))
  (define v2s (map vector x2s y2s))
  
  ((lines-interval-render-proc v1s v2s color style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style
                               alpha label)
   area))

(defproc (function-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [#:y-min y-min (or/c real? #f) #f] [#:y-max y-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (real>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (real>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g1 (function->sampler f1))
  (define g2 (function->sampler f2))
  (renderer2d (function-interval-render-proc g1 g2 samples color style
                                             line1-color line1-width line1-style
                                             line2-color line2-width line2-style
                                             alpha label)
              default-2d-ticks-fun
              (function-interval-bounds-fun g1 g2 samples)
              x-min x-max y-min y-max))

;; ===================================================================================================
;; Inverse function

(define ((inverse-interval-render-proc f1 f2 samples color style
                                       line1-color line1-width line1-style
                                       line2-color line2-width line2-style
                                       alpha label)
         area)
  (define y-min (send area get-y-min))
  (define y-max (send area get-y-max))
  (match-define (list y1s x1s) (f1 y-min y-max samples))
  (match-define (list y2s x2s) (f2 y-min y-max samples))
  (define v1s (map vector x1s y1s))
  (define v2s (map vector x2s y2s))
  
  ((lines-interval-render-proc v1s v2s color style
                               line1-color line1-width line1-style
                               line2-color line2-width line2-style
                               alpha label)
   area))

(defproc (inverse-interval
          [f1 (real? . -> . real?)] [f2 (real? . -> . real?)]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:x-min x-min (or/c real? #f) #f] [#:x-max x-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (line-samples)]
          [#:color color plot-color/c (interval-color)]
          [#:style style plot-brush-style/c (interval-style)]
          [#:line1-color line1-color plot-color/c (interval-line1-color)]
          [#:line1-width line1-width (real>=/c 0) (interval-line1-width)]
          [#:line1-style line1-style plot-pen-style/c (interval-line1-style)]
          [#:line2-color line2-color plot-color/c (interval-line2-color)]
          [#:line2-width line2-width (real>=/c 0) (interval-line2-width)]
          [#:line2-style line2-style plot-pen-style/c (interval-line2-style)]
          [#:alpha alpha (real-in 0 1) (interval-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer2d?
  (define g1 (inverse->sampler f1))
  (define g2 (inverse->sampler f2))
  (renderer2d (inverse-interval-render-proc g1 g2 samples color style
                                            line1-color line1-width line1-style
                                            line2-color line2-width line2-style
                                            alpha label)
              default-2d-ticks-fun
              (inverse-interval-bounds-fun g1 g2 samples)
              x-min x-max y-min y-max))
