#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "renderer2d"]{2D Renderers}

@defproc[(renderer2d? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a 2D @tech{renderer}; that is, if @racket[plot] can plot @racket[value].
The following functions create such renderers.
}

@section{2D Point Renderers}

@doc-apply[points]{
Returns a @tech{renderer} that draws points. Use it, for example, to draw 2D scatter plots.

The renderer sets its bounds to the smallest rectangle that contains the points.
Still, it is often necessary to override these bounds, especially with randomized data. For example,

@interaction[#:eval plot-eval
                    (parameterize ([plot-width    150]
                                   [plot-height   150]
                                   [plot-x-label  #f]
                                   [plot-y-label  #f])
                      (define xs (build-list 20 (λ _ (random))))
                      (define ys (build-list 20 (λ _ (random))))
                      (list (plot (points (map vector xs ys)))
                            (plot (points (map vector xs ys)
                                          #:x-min 0 #:x-max 1
                                          #:y-min 0 #:y-max 1))))]

Readers of the first plot could only guess that the random points were generated in [0,1] × [0,1].

The @(racket #:label) argument may be any Unicode string or a symbol in @(racket known-point-symbols).
}

@defthing[known-point-symbols (listof symbol?)]{
A list containing the symbols that are valid @(racket points) labels.

@interaction[#:eval plot-eval known-point-symbols]
}

@doc-apply[vector-field]{
Returns a renderer that draws a vector field.

@interaction[#:eval plot-eval
                    (plot (vector-field (λ (x y) (vector (+ x y) (- x y)))
                                        -2 2 -2 2))]
}

@doc-apply[error-bars]{
@interaction[#:eval plot-eval
                    (let* ([xs  (linear-seq 1 7 20 #:start? #f #:end? #f)]
                           [ys  (map sqr xs)]
                           [errors  (map (λ (x) (+ x (* 2 (random)))) xs)])
                      (plot (list (function sqr 1 7)
                                  (error-bars (map vector xs ys errors))
                                  (points (map vector xs ys)))))]
}

@section{2D Line Renderers}

@doc-apply[function]{
Returns a renderer that plots a function of @italic{x}.

@interaction[#:eval plot-eval (plot (function sqr -2 2))]
}

@doc-apply[inverse]{
Like @(racket function), but regards @(racket f) as a function of @italic{y}.

@interaction[#:eval plot-eval
                    (plot (list (axes)
                                (function sqr -2 2)
                                (function (λ (x) x) #:color 0 #:style 'dot)
                                (inverse sqr -2 2 #:color 3)))]
}

@doc-apply[lines]{
@interaction[#:eval plot-eval
                    (plot (lines
                           (reverse
                            (for/fold ([lst (list (vector 0 0))]) ([i  (in-range 1 200)])
                              (match-define (vector x y) (first lst))
                              (cons (vector i (+ y (* 1/100 (- (random) 1/2)))) lst)))
                           #:color 6 #:label "Random walk"))]
}

@doc-apply[parametric]{
@interaction[#:eval plot-eval
                    (plot (parametric (λ (t) (vector (cos t) (sin t)))
                                      0 (* 2 pi)))]
}

@doc-apply[polar]{
@interaction[#:eval plot-eval
                    (plot (polar (λ (θ) 1)))
                    (plot (polar (λ (θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))))]
}

@doc-apply[density]{
Returns a renderer that plots an estimated density for the given points.

For example, to plot an estimated density of a triangle distribution:
@interaction[#:eval plot-eval
                    (plot (list (function (λ (x) (cond [(or (x . < . -1) (x . > . 1))  0]
                                                       [(x . < . 0)   (+ 1 x)]
                                                       [(x . >= . 0)  (- 1 x)]))
                                          -1.5 1.5 #:label "Density")
                                (density (build-list
                                          2000 (λ (n) (- (+ (random) (random)) 1)))
                                         #:color 0 #:width 2 #:style 'dot
                                         #:label "Est. density")))]
}
 
@section{2D Interval Renderers}

These renderers each correspond with a line renderer, and graph the area between two lines.

@doc-apply[function-interval]{
Corresponds with @(racket function).

@interaction[#:eval plot-eval (plot (function-interval (λ (x) 0) (λ (x) (exp (* -1/2 (sqr x))))
                                                       -4 4 #:line1-style 'transparent))]
}

@doc-apply[inverse-interval]{
Corresponds with @(racket inverse).

@interaction[#:eval plot-eval (plot (inverse-interval sin (λ (x) 0) (- pi) pi
                                                      #:line2-style 'long-dash))]
}

@doc-apply[lines-interval]{
Corresponds with @(racket lines).

@interaction[#:eval plot-eval
                    (plot (list
                           (tick-grid)
                           (lines-interval (list #(0 0) #(1 1/2)) (list #(0 1) #(1 3/2))
                                           #:color 4 #:line1-color 4 #:line2-color 4
                                           #:label "Parallelogram")))]
}

@doc-apply[parametric-interval]{
Corresponds with @(racket parametric).

@interaction[#:eval plot-eval
                    (let ()
                      (define (f1 t) (vector (* 2 (cos (* 4/5 t)))
                                             (* 2 (sin (* 4/5 t)))))
                      (define (f2 t) (vector (* 1/2 (cos t))
                                             (* 1/2 (sin t))))
                      (plot (parametric-interval f1 f2 (- pi) pi)))]
}

@doc-apply[polar-interval]{
Corresponds with @(racket polar).

@interaction[#:eval plot-eval
                    (let ()
                      (define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
                      (define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
                      (plot (list (polar-axes #:number 10)
                                  (polar-interval f1 f2 #:label "[f1,f2]"))))]
}

@section{2D Contour Renderers}

@doc-apply[contours]{
@interaction[#:eval plot-eval
                    (plot (contours (λ (x y) (- (sqr x) (sqr y)))
                                    -2 2 -2 2 #:label "z"))]
}

@doc-apply[contour-intervals]{
@interaction[#:eval plot-eval
                    (plot (list (contour-intervals (λ (x y) (- (sqr x) (sqr y)))
                                                   -2 2 -2 2 #:label "z")
                                (vector-field (λ (x y) (vector (* 2 x) (* -2 y)))
                                              #:color "black" #:label "Gradient")))]
}

@section{2D Rectangle Renderers}

@defstruct[ivl ([min real?] [max real?])]{
Represents a closed interval. Used to give bounds to rectangles in @(racket rectangles), @(racket rectangles3d), and functions derived from them.
}

@doc-apply[rectangles]{
@interaction[#:eval plot-eval (plot (rectangles (list (vector (ivl -1 1) (ivl -1 1))
                                                      (vector (ivl 1 2) (ivl 1 2)))))]
}

@doc-apply[area-histogram]{
@interaction[#:eval plot-eval
                    (let ()
                      (define (f x) (exp (* -1/2 (sqr x))))
                      (plot (list (area-histogram f (linear-seq -4 4 10))
                                  (function f -4 4))))]
}

@doc-apply[discrete-histogram]{
@interaction[#:eval plot-eval
                    (plot (list (discrete-histogram (list #(a 1) #(b 2) #(c 3) #(d 2)
                                                          #(e 4) #(f 2.5) #(g 1))
                                                    #:label "Numbers per letter")
                                (discrete-histogram (list #(1 1) #(4 2) #(3 1.5))
                                                    #:x-min 8
                                                    #:color 2 #:line-color 2
                                                    #:label "Numbers per number")))]
}

@section{2D Plot Decoration Renderers}

@doc-apply[x-axis]{
Returns a renderer that draws an @italic{x} axis.
}

@doc-apply[y-axis]{
Returns a renderer that draws a @italic{y} axis.
}

@doc-apply[axes]{
Returns a list containing an @(racket x-axis) renderer and a @(racket y-axis) renderer. See @(racket inverse) for an example.
}

@doc-apply[polar-axes]{
Returns a renderer that draws polar axes. See @(racket polar-interval) for an example.
}

@doc-apply[x-tick-lines]{
Returns a renderer that draws vertical lines from the lower @italic{x}-axis ticks to the upper.
}

@doc-apply[y-tick-lines]{
Returns a renderer that draws horizontal lines from the left @italic{y}-axis ticks to the right.
}

@doc-apply[tick-grid]{
Returns a list containing an @(racket x-tick-lines) renderer and a @(racket y-tick-lines) renderer.
See @(racket lines-interval) for an example.
}

@doc-apply[point-label]{
Returns a renderer that draws a labeled point.
If @(racket label) is @(racket #f), the point is labeled with its position.

@interaction[#:eval plot-eval
                    (plot (list (function sqr 0 2)
                                (point-label (vector 1 1))))]

The remaining labeled-point functions are defined in terms of this one.
}

@doc-apply[function-label]{
Returns a renderer that draws a labeled point on a function's graph.

@interaction[#:eval plot-eval
                    (plot (list (function sin (- pi) pi)
                                (function-label sin (* 1/6 pi) "(1/6 π, 1/2)"
                                                #:anchor 'right)))]
}

@doc-apply[inverse-label]{
Returns a renderer that draws a labeled point on a function's inverted graph.
}

@doc-apply[parametric-label]{
Returns a renderer that draws a labeled point on a parametric function's graph.
}

@doc-apply[polar-label]{
Returns a renderer that draws a labeled point on a polar function's graph.
}
