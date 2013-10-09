#lang racket/base

(require racket/contract unstable/latent-contract)

;; ===================================================================================================
;; General exports

(require "private/contracted/parameters.rkt")
(provide (all-from-out "private/contracted/parameters.rkt"))

(require "private/contracted/math.rkt")
(provide (struct-out ivl))

(require "private/contracted/axis-transform.rkt")
(provide (all-from-out "private/contracted/axis-transform.rkt"))

(require "private/contracted/ticks.rkt")
(provide (all-from-out "private/contracted/ticks.rkt"))

(require "private/contracted/date-time.rkt")
(provide (struct-out plot-time)
         plot-time->seconds seconds->plot-time
         datetime->real)

(require "private/common/nonrenderer.rkt")
(provide (activate-contract-out x-ticks y-ticks z-ticks invisible-rect invisible-rect3d))

;; ===================================================================================================
;; 2D exports

(require "private/no-gui/plot2d.rkt")
(provide (activate-contract-out plot/dc plot-bitmap plot-pict plot-file))

(require "private/plot2d/point.rkt")
(provide (activate-contract-out points vector-field error-bars))

(require "private/plot2d/line.rkt")
(provide (activate-contract-out lines parametric polar function inverse density))

(require "private/plot2d/interval.rkt")
(provide (activate-contract-out
          lines-interval parametric-interval polar-interval function-interval inverse-interval))

(require "private/plot2d/contour.rkt")
(provide (activate-contract-out isoline contours contour-intervals))

(require "private/plot2d/rectangle.rkt")
(provide (activate-contract-out rectangles area-histogram discrete-histogram stacked-histogram))

(require "private/plot2d/decoration.rkt")
(provide (activate-contract-out
          x-axis y-axis axes polar-axes
          x-tick-lines y-tick-lines tick-grid
          point-label parametric-label polar-label function-label inverse-label))

;; ===================================================================================================
;; 3D exports

(require "private/no-gui/plot3d.rkt")
(provide (activate-contract-out plot3d/dc plot3d-bitmap plot3d-pict plot3d-file))

(require "private/plot3d/surface.rkt")
(provide (activate-contract-out surface3d))

(require "private/plot3d/contour.rkt")
(provide (activate-contract-out isoline3d contours3d contour-intervals3d))

(require "private/plot3d/line.rkt")
(provide (activate-contract-out lines3d parametric3d))

(require "private/plot3d/point.rkt")
(provide (activate-contract-out points3d vector-field3d))

(require "private/plot3d/isosurface.rkt")
(provide (activate-contract-out isosurface3d isosurfaces3d polar3d))

(require "private/plot3d/rectangle.rkt")
(provide (activate-contract-out rectangles3d discrete-histogram3d stacked-histogram3d))

(require "private/plot3d/decoration.rkt")
(provide (activate-contract-out point-label3d))

;; ===================================================================================================
;; Deprecated functions

(require "private/deprecated/deprecated.rkt")
(provide mix (activate-contract-out line contour shade surface))
