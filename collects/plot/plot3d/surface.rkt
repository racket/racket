#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         "../common/contract.rkt" "../common/contract-doc.rkt"
         "../common/math.rkt"
         "../common/vector.rkt"
         "../common/marching-squares.rkt"
         "../common/ticks.rkt"
         "../common/draw.rkt"
         "../common/legend.rkt"
         "../common/sample.rkt"
         "../common/parameters.rkt"
         "area.rkt"
         "renderer.rkt"
         "sample.rkt"
         "bounds.rkt")

(provide surface3d)

;; ===================================================================================================
;; Surface plots of R R -> R functions

(define ((surface3d-render-proc f samples color style line-color line-width line-style alpha label)
         area)
  (define-values (x-min x-max y-min y-max z-min z-max) (send area get-bounds))
  (match-define (list xs ys zss) (f x-min x-max (samples/animating? samples)
                                    y-min y-max (samples/animating? samples)))
  
  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (for ([ya  (in-list ys)]
        [yb  (in-list (rest ys))]
        [zs0  (in-vector zss)]
        [zs1  (in-vector zss 1)]
        #:when #t
        [xa  (in-list xs)]
        [xb  (in-list (rest xs))]
        [z1  (in-vector zs0)]
        [z2  (in-vector zs0 1)]
        [z3  (in-vector zs1 1)]
        [z4  (in-vector zs1)])
    (send area put-polygon
          (list (vector xa ya z1) (vector xb ya z2) (vector xb yb z3) (vector xa yb z4))))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else   empty]))

(defproc (surface3d
          [f (real? real? . -> . real?)]
          [x-min (or/c real? #f) #f] [x-max (or/c real? #f) #f]
          [y-min (or/c real? #f) #f] [y-max (or/c real? #f) #f]
          [#:z-min z-min (or/c real? #f) #f] [#:z-max z-max (or/c real? #f) #f]
          [#:samples samples (integer>=/c 2) (plot3d-samples)]
          [#:color color plot-color/c (surface-color)]
          [#:style style plot-brush-style/c (surface-style)]
          [#:line-color line-color plot-color/c (surface-line-color)]
          [#:line-width line-width (real>=/c 0) (surface-line-width)]
          [#:line-style line-style plot-pen-style/c (surface-line-style)]
          [#:alpha alpha (real-in 0 1) (surface-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define g (2d-function->sampler f))
  (renderer3d (surface3d-render-proc g samples color style
                                     line-color line-width line-style alpha label)
              default-3d-ticks-fun
              (surface3d-bounds-fun g samples)
              x-min x-max y-min y-max z-min z-max))
