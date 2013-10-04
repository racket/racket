#lang racket/base

(require racket/contract unstable/latent-contract unstable/contract racket/class)

(require "../common/contract.rkt"
         "../common/plot-element.rkt"
         "../common/math.rkt"
         "../common/legend.rkt"
         "../plot2d/plot-area.rkt"
         "../plot3d/plot-area.rkt")

(provide
 (contract-out
  (struct plot-element
    ([bounds-rect  (or/c (vectorof ivl?) #f)]
     [bounds-fun   (or/c bounds-fun/c #f)]
     [ticks-fun    (or/c ticks-fun/c #f)]))
  (struct (nonrenderer plot-element)
    ([bounds-rect  (or/c (vectorof ivl?) #f)]
     [bounds-fun   (or/c bounds-fun/c #f)]
     [ticks-fun    (or/c ticks-fun/c #f)]))
  (struct (renderer2d plot-element)
    ([bounds-rect  (or/c (vectorof ivl?) #f)]
     [bounds-fun   (or/c bounds-fun/c #f)]
     [ticks-fun    (or/c ticks-fun/c #f)]
     [render-proc  (or/c ((is-a?/c 2d-plot-area%) . -> . (treeof legend-entry?)) #f)]))
  (struct (renderer3d plot-element)
    ([bounds-rect  (or/c (vectorof ivl?) #f)]
     [bounds-fun   (or/c bounds-fun/c #f)]
     [ticks-fun    (or/c ticks-fun/c #f)]
     [render-proc  (or/c ((is-a?/c 3d-plot-area%) . -> . (treeof legend-entry?)) #f)])))
 bounds-fun/c ticks-fun/c
 (activate-contract-out default-ticks-fun
                        function-bounds-fun function-interval-bounds-fun
                        inverse-bounds-fun inverse-interval-bounds-fun
                        surface3d-bounds-fun)
 plot3d-back-layer
 plot3d-area-layer
 plot3d-front-layer
 )
