#lang racket/base

;; Functions that create renderers for backward-compatible functions 'line', 'contour', etc.

(require racket/match plot/utils
         "../plot2d/line.rkt"
         "../plot2d/contour.rkt"
         "../plot3d/surface.rkt")

(provide line-renderer
         contour-renderer
         shade-renderer
         surface-renderer)

(define (line-renderer f samples width color mode mapping t-min t-max)
  (case mode
    [(standard)
     (case mapping
       [(cartesian)  (function f #:samples samples #:width width #:color color)]
       [(polar)      (polar f t-min t-max
                            #:samples samples #:width width #:color color)])]
    [(parametric)
     (case mapping
       [(cartesian)  (parametric f t-min t-max
                                 #:samples samples #:width width #:color color)]
       [(polar)      (parametric (λ (t)
                                   (match-define (vector θ r) (f t))
                                   (vector (* r (cos θ)) (* r (sin θ))))
                                 t-min t-max
                                 #:samples samples #:width width #:color color)])]))

(define (contour-renderer f samples width color levels)
  (contours f #:samples samples #:levels (if (exact-integer? levels) (sub1 levels) levels)
            #:colors (list color) #:widths (list width) #:styles '(solid)))

(define (shade-fill-colors zs)
  (color-seq* '((0 0 255) (255 255 255) (255 0 0)) (sub1 (length zs))))

(define (shade-renderer f samples levels)
  (contour-intervals f #:samples samples #:levels (if (exact-integer? levels) (sub1 levels) levels)
                     #:colors shade-fill-colors #:contour-styles '(transparent)))

(define (surface-renderer f samples width color)
  (surface3d f #:samples samples #:line-color color #:line-width width))
