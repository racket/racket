#lang racket

(require plot plot/utils)

(time
 (plot3d (isosurface3d (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z)))) 1
                       #:color 2 #:line-color (->brush-color 2) #:line-width 1
                       #:label "Sphere")
         #:x-min -0.8 #:x-max 0.8
         #:y-min -0.8 #:y-max 0.8
         #:z-min -0.8 #:z-max 0.8
         #:altitude 30
         #:legend-anchor 'center))

(time
 (define saddle (λ (x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z))))))
 (plot3d (isosurfaces3d saddle #:d-min -1 #:d-max 1 #:label "d")
         #:x-min -2 #:x-max 2
         #:y-min -2 #:y-max 2
         #:z-min -2 #:z-max 2
         #:legend-anchor 'top-left))

(time
 (define saddle (λ (x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z))))))
 (plot3d (isosurfaces3d saddle #:d-min -1 #:d-max 1
                        #:colors '(1 2 3)
                        #:line-colors '(1 2 3)
                        #:line-styles '(solid)
                        #:alphas '(3/4)
                        #:label "d")
         #:x-min -2 #:x-max 2
         #:y-min -2 #:y-max 2
         #:z-min -2 #:z-max 2
         #:legend-anchor 'top-left))

(time
 (define saddle (λ (x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z))))))
 (plot3d (isosurface3d saddle -1/4 #:color 0 #:line-color 0 #:alpha 7/8
                       #:label "d = -1/4")
         #:x-min -2 #:x-max 2
         #:y-min -2 #:y-max 2
         #:z-min -2 #:z-max 2))

(time
 (define (f1 θ ρ) (+ 1 (/ θ 2 pi) (* 1/8 (sin (* 8 ρ)))))
 (define (f2 θ ρ) (+ 0 (/ θ 2 pi) (* 1/8 (sin (* 8 ρ)))))
 
 (plot3d (list (polar3d f1 #:samples 41 #:color "navajowhite" #:line-style 'transparent #:alpha 2/3)
               (polar3d f2 #:samples 41 #:color "navajowhite" #:line-style 'transparent #:alpha 2/3))
         #:title "A Seashell" #:x-label #f #:y-label #f))
