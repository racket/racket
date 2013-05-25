#lang racket/base

(require unstable/latent-contract/defthing)

;; ===================================================================================================
;; Common exports

(require "common/parameters.rkt"
         "common/contract.rkt"
         "common/axis-transform.rkt"
         "common/ticks.rkt"
         "common/math.rkt"
         "common/plot-element.rkt"
         "common/nonrenderer.rkt"
         "common/format.rkt"
         "common/sample.rkt"
         "common/draw.rkt"
         "common/date-time.rkt"
         "common/marching-squares.rkt"
         "common/marching-cubes.rkt"
         "common/legend.rkt"
         "common/kde.rkt")

(provide (only-doc-out
          (combine-out (all-from-out "common/parameters.rkt")
                       (all-from-out "common/contract.rkt")
                       (all-from-out "common/axis-transform.rkt")
                       (all-from-out "common/ticks.rkt")
                       (all-from-out "common/math.rkt")
                       (all-from-out "common/plot-element.rkt")
                       (all-from-out "common/nonrenderer.rkt")
                       (all-from-out "common/format.rkt")
                       (all-from-out "common/sample.rkt")
                       (all-from-out "common/draw.rkt")
                       (all-from-out "common/date-time.rkt")
                       (all-from-out "common/marching-squares.rkt")
                       (all-from-out "common/marching-cubes.rkt")
                       (all-from-out "common/legend.rkt")
                       (all-from-out "common/kde.rkt"))))

;; ===================================================================================================
;; 2D exports

(require "plot2d/plot.rkt"
         "plot2d/point.rkt"
         "plot2d/line.rkt"
         "plot2d/interval.rkt"
         "plot2d/contour.rkt"
         "plot2d/rectangle.rkt"
         "plot2d/decoration.rkt")

(provide (only-doc-out
          (combine-out (all-from-out "plot2d/plot.rkt")
                       (all-from-out "plot2d/point.rkt")
                       (all-from-out "plot2d/line.rkt")
                       (all-from-out "plot2d/interval.rkt")
                       (all-from-out "plot2d/contour.rkt")
                       (all-from-out "plot2d/rectangle.rkt")
                       (all-from-out "plot2d/decoration.rkt"))))

;; ===================================================================================================
;; 3D exports

(require "plot3d/plot.rkt"
         "plot3d/surface.rkt"
         "plot3d/contour.rkt"
         "plot3d/line.rkt"
         "plot3d/point.rkt"
         "plot3d/isosurface.rkt"
         "plot3d/rectangle.rkt")

(provide (only-doc-out
          (combine-out (all-from-out "plot3d/plot.rkt")
                       (all-from-out "plot3d/surface.rkt")
                       (all-from-out "plot3d/contour.rkt")
                       (all-from-out "plot3d/line.rkt")
                       (all-from-out "plot3d/point.rkt")
                       (all-from-out "plot3d/isosurface.rkt")
                       (all-from-out "plot3d/rectangle.rkt"))))

;; ===================================================================================================
;; Deprecated functions

(require "deprecated/deprecated.rkt")
(provide (only-doc-out (all-from-out "deprecated/deprecated.rkt")))
