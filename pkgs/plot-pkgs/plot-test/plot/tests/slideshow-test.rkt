#lang slideshow

(require plot plot/utils)

(module test racket/base)

(plot-font-size (current-font-size))
(plot-title "Untitled")
(plot-width 600)
(plot-height 600)
(plot-background '(192 255 192))
(plot-foreground '(255 64 255))

(slide
 #:title "Two Small 2D Parabolas"
 (para "A small, aliased parabola:"
       (scale (bitmap (plot (function sqr -1 1 #:label "y = x^2"))) 1/3))
 (para "A small, smooth parabola:"
       (rotate (scale (plot-pict (function sqr -1 1 #:label "y = x^2")) 1/3)
               (degrees->radians 15))))

(slide
 #:title "A 2D Parabola"
 (parameterize ([plot-background  1]
                [plot-background-alpha 1/2]
                [plot-foreground  1])
   ;; This parabola should be pink:
   (plot-pict (function sqr -1 1 #:label "y = x^2")
              #:legend-anchor 'center)))

(define (parabola2d x y) (+ (sqr x) (sqr y)))

(slide
 #:title "Two Small 3D Parabolas"
 (para "A small, aliased parabola:"
       (scale (bitmap (plot3d (surface3d parabola2d -1 1 -1 1 #:label "z = x^2 + y^2"))) 1/3))
 (para "A small, smooth parabola:"
       (rotate (scale (plot3d-pict (surface3d parabola2d -1 1 -1 1 #:label "z = x^2 + y^2")) 1/3)
               (degrees->radians 15))))

(slide
 #:title "A 3D Parabola"
 (parameterize ([plot-background  1]
                [plot-background-alpha 1/2]
                [plot-foreground  1])
   (plot3d-pict (list (surface3d parabola2d -1 1 -1 1
                                 #:label "z = x^2 + y^2" #:color 3)
                      (contours3d parabola2d -1 1 -1 1))
                #:legend-anchor 'center)))
