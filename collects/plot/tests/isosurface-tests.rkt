#lang racket

(require plot plot/utils unstable/flonum)

(time
 (plot3d (isosurface3d (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z)))) 1
                       #:color 2 #:line-style 'transparent
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
 (plot3d (isosurface3d saddle -1/4 #:samples 21
                       #:color "black" #:style 3
                       #:alpha 1
                       #:label "d = -1/4")
         #:x-min -2 #:x-max 2
         #:y-min -2 #:y-max 2
         #:z-min -2 #:z-max 2))

(define 2pi (* 2 pi))

(time
 (define (f1 θ ρ) (+ 1 (/ θ 2pi) (* 1/8 (sin (* 8 ρ)))))
 (define (f2 θ ρ) (+ (/ θ 2pi) (* 1/8 (sin (* 8 ρ)))))
 (plot3d (list (polar3d f1 #:samples 41 #:color "navajowhite" #:line-style 'transparent #:alpha 2/3)
               (polar3d f2 #:samples 41 #:color "navajowhite" #:line-style 'transparent #:alpha 2/3)
               (parametric3d (λ (ρ) (3d-polar->3d-cartesian 0 ρ (f1 2pi ρ)))
                             (* -1/2 pi) (* 1/2 pi)
                             #:color "navajowhite" #:width 2)
               (lines3d '(#(0 0 2) #(0 0 -2)) #:color "navajowhite" #:width 2))
         #:title "A Seashell" #:x-label #f #:y-label #f #:angle 210 #:altitude 30))

(define flepsilon (flnext 0.0))

(time
 (plot3d (isosurface3d (λ (x y z) (+ (sqr x) (sqr y) (sqr z))) (sqr (inexact->exact flepsilon))
                       (- flepsilon) flepsilon (- flepsilon) flepsilon (- flepsilon) flepsilon)))
