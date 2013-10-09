#lang racket/base

(require unstable/latent-contract/defthing)

;; ===================================================================================================
;; Common exports

(require plot/private/common/parameters
         plot/private/common/contract
         plot/private/common/axis-transform
         plot/private/common/ticks
         plot/private/common/math
         plot/private/common/plot-element
         plot/private/common/nonrenderer
         plot/private/common/format
         plot/private/common/sample
         plot/private/common/draw
         plot/private/common/date-time
         plot/private/common/marching-squares
         plot/private/common/marching-cubes
         plot/private/common/legend
         plot/private/common/kde)

(provide (only-doc-out
          (combine-out
           (all-from-out
            plot/private/common/parameters
            plot/private/common/contract
            plot/private/common/axis-transform
            plot/private/common/ticks
            plot/private/common/math
            plot/private/common/plot-element
            plot/private/common/nonrenderer
            plot/private/common/format
            plot/private/common/sample
            plot/private/common/draw
            plot/private/common/date-time
            plot/private/common/marching-squares
            plot/private/common/marching-cubes
            plot/private/common/legend
            plot/private/common/kde))))

;; ===================================================================================================
;; 2D exports

(require plot/private/no-gui/plot2d
         plot/private/gui/plot2d
         plot/private/plot2d/point
         plot/private/plot2d/line
         plot/private/plot2d/interval
         plot/private/plot2d/contour
         plot/private/plot2d/rectangle
         plot/private/plot2d/decoration)

(provide (only-doc-out
          (combine-out
           (all-from-out
            plot/private/no-gui/plot2d
            plot/private/gui/plot2d
            plot/private/plot2d/point
            plot/private/plot2d/line
            plot/private/plot2d/interval
            plot/private/plot2d/contour
            plot/private/plot2d/rectangle
            plot/private/plot2d/decoration))))

;; ===================================================================================================
;; 3D exports

(require plot/private/no-gui/plot3d
         plot/private/gui/plot3d
         plot/private/plot3d/surface
         plot/private/plot3d/contour
         plot/private/plot3d/line
         plot/private/plot3d/point
         plot/private/plot3d/isosurface
         plot/private/plot3d/rectangle
         plot/private/plot3d/decoration)

(provide (only-doc-out
          (combine-out
           (all-from-out
            plot/private/no-gui/plot3d
            plot/private/gui/plot3d
            plot/private/plot3d/surface
            plot/private/plot3d/contour
            plot/private/plot3d/line
            plot/private/plot3d/point
            plot/private/plot3d/isosurface
            plot/private/plot3d/rectangle
            plot/private/plot3d/decoration))))

;; ===================================================================================================
;; Deprecated functions

(require plot/private/deprecated/deprecated)

(provide (only-doc-out
          (all-from-out
           plot/private/deprecated/deprecated)))
