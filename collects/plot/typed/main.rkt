#lang typed/racket/base

;; Every require/provide pair in this file corresponds with a require/provide pair in plot/main

;; ===================================================================================================
;; General exports

(require "common/types.rkt")
(provide (all-from-out "common/types.rkt"))

(require "contracted/parameters.rkt")
(provide (all-from-out "contracted/parameters.rkt"))

;; Not necessary because re-exporting "types.rkt" exports the `ivl' struct
;(require "contracted/math.rkt")
;(provide (struct-out ivl))

(require "contracted/axis-transform.rkt")
(provide (all-from-out "contracted/axis-transform.rkt"))

(require "contracted/ticks.rkt")
(provide (all-from-out "contracted/ticks.rkt"))

(require "contracted/date-time.rkt")
(provide plot-time->seconds seconds->plot-time datetime->real)

(require "common/nonrenderers.rkt")
(provide (all-from-out "common/nonrenderers.rkt"))

;; ===================================================================================================
;; 2D exports

(require "plot2d/plot.rkt")
(provide (all-from-out "plot2d/plot.rkt"))

(require "plot2d/point.rkt")
(provide (all-from-out "plot2d/point.rkt"))

(require "plot2d/line.rkt")
(provide (all-from-out "plot2d/line.rkt"))

(require "plot2d/interval.rkt")
(provide (all-from-out "plot2d/interval.rkt"))

(require "plot2d/contour.rkt")
(provide (all-from-out "plot2d/contour.rkt"))

(require "plot2d/rectangle.rkt")
(provide (all-from-out "plot2d/rectangle.rkt"))

(require "plot2d/decoration.rkt")
(provide (all-from-out "plot2d/decoration.rkt"))

;; ===================================================================================================
;; 3D exports

(require "plot3d/plot.rkt")
(provide (all-from-out "plot3d/plot.rkt"))

(require "plot3d/surface.rkt")
(provide (all-from-out "plot3d/surface.rkt"))

(require "plot3d/contour.rkt")
(provide (all-from-out "plot3d/contour.rkt"))

(require "plot3d/line.rkt")
(provide (all-from-out "plot3d/line.rkt"))

(require "plot3d/point.rkt")
(provide (all-from-out "plot3d/point.rkt"))

(require "plot3d/isosurface.rkt")
(provide (all-from-out "plot3d/isosurface.rkt"))

(require "plot3d/rectangle.rkt")
(provide (all-from-out "plot3d/rectangle.rkt"))
