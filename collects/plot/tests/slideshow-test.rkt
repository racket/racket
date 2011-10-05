#lang slideshow

(require "../main.rkt")

(plot-font-size (current-font-size))
(plot-width (current-para-width))
(plot-height 600)

(slide
 #:title "A 2D Parabola"
 (bitmap (plot (function sqr -1 1 #:label "y = x^2"))))

(slide
 #:title "A 3D Parabola"
 (bitmap (plot3d (list (surface3d (λ (x y) (+ (sqr x) (sqr y))) -2 2 -2 2
                                  #:label "z = x^2 + y^2" #:color 3)
                       (contours3d (λ (x y) (+ (sqr x) (sqr y))) -2 2 -2 2))
                 #:legend-anchor 'top-left)))
