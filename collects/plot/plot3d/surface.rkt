#lang racket/base

(require racket/class racket/match racket/list racket/flonum racket/contract
         unstable/latent-contract/defthing
         plot/utils)

(provide (all-defined-out))

;; ===================================================================================================
;; Surface plots of R R -> R functions

(define ((surface3d-render-proc f samples color style line-color line-width line-style alpha label)
         area)
  (match-define (vector x-ivl y-ivl z-ivl) (send area get-bounds-rect))
  (define num (animated-samples samples))
  (define sample (f (vector x-ivl y-ivl) (vector num num)))
  
  (match-define (ivl x-min x-max) x-ivl)
  (match-define (ivl y-min y-max) y-ivl)
  (match-define (ivl z-min z-max) z-ivl)

  (send area put-alpha alpha)
  (send area put-brush color style)
  (send area put-pen line-color line-width line-style)
  (let* ([flonum-ok?  (flonum-ok-for-3d? x-min x-max y-min y-max z-min z-max)]
         [sample      (if flonum-ok? (2d-sample-exact->inexact sample) sample)])
    (for-2d-sample
     (xa xb ya yb z1 z2 z3 z4) sample
     (send area put-polygon
           (list (vector xa ya z1) (vector xb ya z2) (vector xb yb z3) (vector xa yb z4)))))
  
  (cond [label  (rectangle-legend-entry label color style line-color line-width line-style)]
        [else   empty]))

(defproc (surface3d
          [f (real? real? . -> . real?)]
          [x-min (or/c rational? #f) #f] [x-max (or/c rational? #f) #f]
          [y-min (or/c rational? #f) #f] [y-max (or/c rational? #f) #f]
          [#:z-min z-min (or/c rational? #f) #f] [#:z-max z-max (or/c rational? #f) #f]
          [#:samples samples (and/c exact-integer? (>=/c 2)) (plot3d-samples)]
          [#:color color plot-color/c (surface-color)]
          [#:style style plot-brush-style/c (surface-style)]
          [#:line-color line-color plot-color/c (surface-line-color)]
          [#:line-width line-width (>=/c 0) (surface-line-width)]
          [#:line-style line-style plot-pen-style/c (surface-line-style)]
          [#:alpha alpha (real-in 0 1) (surface-alpha)]
          [#:label label (or/c string? #f) #f]
          ) renderer3d?
  (define x-ivl (ivl x-min x-max))
  (define y-ivl (ivl y-min y-max))
  (define z-ivl (ivl z-min z-max))
  (define g (2d-function->sampler f (vector x-ivl y-ivl)))
  (renderer3d (vector x-ivl y-ivl z-ivl)
              (surface3d-bounds-fun g samples)
              default-ticks-fun
              (surface3d-render-proc g samples color style
                                     line-color line-width line-style alpha label)))
