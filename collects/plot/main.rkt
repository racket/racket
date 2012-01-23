#lang racket/base

(require racket/contract unstable/latent-contract)

;; ===================================================================================================
;; General exports

(require "contracted/parameters.rkt")
(provide (all-from-out "contracted/parameters.rkt"))

(require "contracted/math.rkt")
(provide (struct-out ivl))

(require "contracted/axis-transform.rkt")
(provide (all-from-out "contracted/axis-transform.rkt"))

(require "contracted/ticks.rkt")
(provide (all-from-out "contracted/ticks.rkt"))

(require "contracted/date-time.rkt")
(provide (struct-out plot-time)
         plot-time->seconds seconds->plot-time
         datetime->real)

(require "common/nonrenderer.rkt")
(provide (activate-contract-out x-ticks y-ticks z-ticks invisible-rect invisible-rect3d))

;; ===================================================================================================
;; 2D exports

(require "plot2d/plot.rkt")
(provide (activate-contract-out plot/dc plot plot-bitmap plot-pict plot-snip plot-frame plot-file))

(require "plot2d/point.rkt")
(provide (activate-contract-out points vector-field error-bars))

(require "plot2d/line.rkt")
(provide (activate-contract-out lines parametric polar function inverse density))

(require "plot2d/interval.rkt")
(provide (activate-contract-out
          lines-interval parametric-interval polar-interval function-interval inverse-interval))

(require "plot2d/contour.rkt")
(provide (activate-contract-out isoline contours contour-intervals))

(require "plot2d/rectangle.rkt")
(provide (activate-contract-out rectangles area-histogram discrete-histogram stacked-histogram))

(require "plot2d/decoration.rkt")
(provide (activate-contract-out
          x-axis y-axis axes polar-axes
          x-tick-lines y-tick-lines tick-grid
          point-label parametric-label polar-label function-label inverse-label))

;; ===================================================================================================
;; 3D exports

(require "plot3d/plot.rkt")
(provide (activate-contract-out
          plot3d/dc plot3d plot3d-bitmap plot3d-pict plot3d-snip plot3d-frame plot3d-file))

(require "plot3d/surface.rkt")
(provide (activate-contract-out surface3d))

(require "plot3d/contour.rkt")
(provide (activate-contract-out isoline3d contours3d contour-intervals3d))

(require "plot3d/line.rkt")
(provide (activate-contract-out lines3d parametric3d))

(require "plot3d/point.rkt")
(provide (activate-contract-out points3d vector-field3d))

(require "plot3d/isosurface.rkt")
(provide (activate-contract-out isosurface3d isosurfaces3d polar3d))

(require "plot3d/rectangle.rkt")
(provide (activate-contract-out rectangles3d discrete-histogram3d stacked-histogram3d))

;; ===================================================================================================
;; Deprecated functions

(require "deprecated/deprecated.rkt")
(provide mix (activate-contract-out line contour shade surface))
