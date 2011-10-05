#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "renderer3d"]{3D Renderers}

@defproc[(renderer3d? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a 3D @tech{renderer}; that is, if @racket[plot3d] can plot @racket[value].
The following functions create such renderers.
}

@section{3D Point Renderers}

@doc-apply[points3d]{
Returns a renderer that draws points in 3D space.

A scatter plot of points sampled uniformly from the surface of a sphere:
@interaction[#:eval plot-eval
                    (let ()
                      (define (runif) (- (* 2 (random)) 1))
                      (define (rnormish) (+ (runif) (runif) (runif) (runif)))
                      
                      (define xs0 (build-list 1000 (λ _ (rnormish))))
                      (define ys0 (build-list 1000 (λ _ (rnormish))))
                      (define zs0 (build-list 1000 (λ _ (rnormish))))
                      (define mags (map (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z))))
                                        xs0 ys0 zs0))
                      (define xs (map / xs0 mags))
                      (define ys (map / ys0 mags))
                      (define zs (map / zs0 mags))
                      
                      (plot3d (points3d (map vector xs ys zs) #:sym 'dot)
                              #:altitude 25))]
}

@section{3D Line Renderers}

@doc-apply[lines3d]{

}

@doc-apply[parametric3d]{
@interaction[#:eval plot-eval
                    (plot3d (parametric3d (λ (t)
                                            (vector (* (cos (* 80 t)) (cos t))
                                                    (* (sin (* 80 t)) (cos t))
                                                    (sin t)))
                                          (- pi) pi
                                          #:samples 3000 #:alpha 0.5)
                            #:altitude 25)]
}

@section{3D Surface Renderers}

@doc-apply[surface3d]{
@interaction[#:eval plot-eval (plot3d (list (surface3d (λ (x y) (+ (sqr x) (sqr y))) -1 1 -1 1
                                                       #:label "z = x^2 + y^2")
                                            (surface3d (λ (x y) (- (+ (sqr x) (sqr y)))) -1 1 -1 1
                                                       #:color 4 #:line-color 4
                                                       #:label "z = -x^2 - y^2")))]
}

@doc-apply[polar3d]{

@interaction[#:eval plot-eval (plot3d (polar3d (λ (θ ρ) 1)) #:altitude 25)]

@interaction[#:eval plot-eval
                    (let ()
                      (define (f1 θ ρ) (+ 1 (/ θ 2 pi) (* 1/8 (sin (* 8 ρ)))))
                      (define (f2 θ ρ) (+ 0 (/ θ 2 pi) (* 1/8 (sin (* 8 ρ)))))
                      
                      (plot3d (list (polar3d f1 #:color "navajowhite"
                                             #:line-style 'transparent #:alpha 2/3)
                                    (polar3d f2 #:color "navajowhite"
                                             #:line-style 'transparent #:alpha 2/3))
                              #:title "A Seashell" #:x-label #f #:y-label #f))]
}

@section{3D Contour Renderers}

@doc-apply[contours3d]{
@interaction[#:eval plot-eval (plot3d (contours3d (λ (x y) (+ (sqr x) (sqr y))) -1.1 1.1 -1.1 1.1
                                                  #:label "z = x^2 + y^2")
                                      #:legend-anchor 'top-left)]
}

@doc-apply[contour-intervals3d]{
@interaction[#:eval plot-eval (plot3d (contour-intervals3d (λ (x y) (+ (sqr x) (sqr y)))
                                                           -1.1 1.1 -1.1 1.1
                                                           #:label "z = x^2 + y^2")
                                      #:legend-anchor 'top-left)]
}

@section{3D Isosurface Renderers}

@doc-apply[isosurface3d]{
@interaction[#:eval plot-eval (plot3d (isosurface3d
                                       (λ (x y z) (sqrt (+ (sqr x) (sqr y) (sqr z)))) 1
                                       -1 1 -1 1 -1 1)
                                      #:altitude 25)]
}

@doc-apply[isosurfaces3d]{
@interaction[#:eval plot-eval (let ()
                                (define (saddle x y z) (- (sqr x) (* 1/2 (+ (sqr y) (sqr z)))))
                                (plot3d (isosurfaces3d saddle #:d-min -1 #:d-max 1 #:label "")
                                        #:x-min -2 #:x-max 2
                                        #:y-min -2 #:y-max 2
                                        #:z-min -2 #:z-max 2
                                        #:legend-anchor 'top-left))]
}

@section{3D Rectangle Renderers}

@doc-apply[rectangles3d]{
@interaction[#:eval plot-eval
                    (let ()
                      (define x-ivls (bounds->intervals (linear-seq 2 8 10)))
                      (define y-ivls (bounds->intervals (linear-seq -5 5 10)))
                      (define x-mids (linear-seq 2 8 9 #:start? #f #:end? #f))
                      (define y-mids (linear-seq -5 5 9 #:start? #f #:end? #f))
                      (plot3d (rectangles3d (append*
                                             (for/list ([y-ivl  (in-list y-ivls)]
                                                        [y  (in-list y-mids)])
                                               (for/list ([x-ivl  (in-list x-ivls)]
                                                          [x  (in-list x-mids)])
                                                 (define z (exp (* -1/2 (+ (sqr (- x 5)) (sqr y)))))
                                                 (vector x-ivl y-ivl (ivl 0 z)))))
                                            #:alpha 3/4
                                            #:label "Approximate 2D Normal")))]
}

@doc-apply[discrete-histogram3d]{
@interaction[#:eval plot-eval
                    (plot3d (discrete-histogram3d '(#(a a 1) #(a b 2) #(b b 3))
                                                  #:label "Missing (b,a)"
                                                  #:color 4 #:line-color 4))]
}
