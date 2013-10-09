#lang typed/racket/base

;; Every require/provide pair in this file corresponds with a require/provide pair in plot/no-gui

;; ===================================================================================================
;; General exports

(require (except-in "private/common/types.rkt" Pict))
(provide (all-from-out "private/common/types.rkt"))

(require "private/contracted/parameters.rkt")
(provide (all-from-out "private/contracted/parameters.rkt"))

;; Not necessary because re-exporting "types.rkt" exports the `ivl' struct
;(require "private/contracted/math.rkt")
;(provide (struct-out ivl))

(require "private/contracted/axis-transform.rkt")
(provide (all-from-out "private/contracted/axis-transform.rkt"))

(require "private/contracted/ticks.rkt")
(provide (all-from-out "private/contracted/ticks.rkt"))

(require "private/contracted/date-time.rkt")
(provide plot-time->seconds seconds->plot-time datetime->real)

(require "private/common/nonrenderers.rkt")
(provide (all-from-out "private/common/nonrenderers.rkt"))

;; ===================================================================================================
;; 2D exports

(require "private/no-gui/plot2d.rkt")
(provide (all-from-out "private/no-gui/plot2d.rkt"))

(require "private/plot2d/point.rkt")
(provide (all-from-out "private/plot2d/point.rkt"))

(require "private/plot2d/line.rkt")
(provide (all-from-out "private/plot2d/line.rkt"))

(require "private/plot2d/interval.rkt")
(provide (all-from-out "private/plot2d/interval.rkt"))

(require "private/plot2d/contour.rkt")
(provide (all-from-out "private/plot2d/contour.rkt"))

(require "private/plot2d/rectangle.rkt")
(provide (all-from-out "private/plot2d/rectangle.rkt"))

(require "private/plot2d/decoration.rkt")
(provide (all-from-out "private/plot2d/decoration.rkt"))

;; ===================================================================================================
;; 3D exports

(require "private/no-gui/plot3d.rkt")
(provide (all-from-out "private/no-gui/plot3d.rkt"))

(require "private/plot3d/surface.rkt")
(provide (all-from-out "private/plot3d/surface.rkt"))

(require "private/plot3d/contour.rkt")
(provide (all-from-out "private/plot3d/contour.rkt"))

(require "private/plot3d/line.rkt")
(provide (all-from-out "private/plot3d/line.rkt"))

(require "private/plot3d/point.rkt")
(provide (all-from-out "private/plot3d/point.rkt"))

(require "private/plot3d/isosurface.rkt")
(provide (all-from-out "private/plot3d/isosurface.rkt"))

(require "private/plot3d/rectangle.rkt")
(provide (all-from-out "private/plot3d/rectangle.rkt"))

(require "private/plot3d/decoration.rkt")
(provide (all-from-out "private/plot3d/decoration.rkt"))
