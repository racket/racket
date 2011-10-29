#lang racket/base

(require racket/contract)

;; ===================================================================================================
;; Common exports

(require "common/parameters.rkt"
         "common/contract.rkt")

(provide (all-from-out "common/parameters.rkt")
         (all-from-out "common/contract.rkt"))

(require "common/axis-transform.rkt")
(provide (all-from-out "common/axis-transform.rkt"))

(require "common/ticks.rkt")
(provide (all-from-out "common/ticks.rkt"))

(require "common/math.rkt")
(provide (struct-out ivl))

(require "common/plot-element.rkt")
(provide plot-element? non-renderer? renderer2d? renderer3d?)

(require "common/non-renderer.rkt")
(provide (all-from-out "common/non-renderer.rkt"))

;; ===================================================================================================
;; 2D exports

(require "plot2d/plot.rkt"
         "plot2d/point.rkt"
         "plot2d/line.rkt"
         "plot2d/interval.rkt"
         "plot2d/contour.rkt"
         "plot2d/rectangle.rkt"
         "plot2d/decoration.rkt"
         "plot2d/kde.rkt")

(provide (all-from-out "plot2d/plot.rkt")
         (all-from-out "plot2d/point.rkt")
         (all-from-out "plot2d/line.rkt")
         (all-from-out "plot2d/interval.rkt")
         (all-from-out "plot2d/contour.rkt")
         (all-from-out "plot2d/rectangle.rkt")
         (all-from-out "plot2d/decoration.rkt")
         density)

;; ===================================================================================================
;; 3D exports

(require "plot3d/plot.rkt"
         "plot3d/surface.rkt"
         "plot3d/contour.rkt"
         "plot3d/line.rkt"
         "plot3d/point.rkt"
         "plot3d/isosurface.rkt"
         "plot3d/rectangle.rkt")

(provide (all-from-out "plot3d/plot.rkt")
         (all-from-out "plot3d/surface.rkt")
         (all-from-out "plot3d/contour.rkt")
         (all-from-out "plot3d/line.rkt")
         (all-from-out "plot3d/point.rkt")
         (all-from-out "plot3d/isosurface.rkt")
         (all-from-out "plot3d/rectangle.rkt"))

;; ===================================================================================================
;; Deprecated functions

(require "deprecated.rkt")
(provide (all-from-out "deprecated.rkt"))
