#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "renderer2d"]{2D Renderers}

@section[#:tag "renderer2d-function-arguments"]{2D Renderer Function Arguments}

Functions that return 2D renderers always have these kinds of arguments:
@itemlist[
          @item{Required (and possibly optional) arguments representing the graph to plot.}
          @item{Optional keyword arguments for overriding calculated bounds, with the default value @(racket #f).}
          @item{Optional keyword arguments that determine the appearance of the plot.}
          @item{The optional keyword argument @(racket #:label), which specifies the name of the renderer in the legend.}]

We will take @(racket function), perhaps the most commonly used renderer-producing function, as an example.

@bold{Graph arguments.} The first argument to @(racket function) is the required @(racket f), the function to plot.
It is followed by two optional arguments @(racket x-min) and @(racket x-max), which specify the renderer's @italic{x} bounds.
(If not given, the @italic{x} bounds will be the plot area @italic{x} bounds, as requested by another renderer or specified to @(racket plot) using @(racket #:x-min) and @(racket #:x-max).)

These three arguments define the @deftech{graph} of the function @(racket f), a possibly infinite set of pairs of points @(racket x),@(racket (f x)).
An infinite graph cannot be plotted directly, so the renderer must approximately plot the points in it.
The renderer returned by @(racket function) does this by drawing lines connected end-to-end.

@bold{Overriding bounds arguments.} Next in @(racket function)'s argument list are the keyword arguments @(racket #:y-min) and @(racket #:y-max), which override the renderer's calculated @italic{y} bounds if given.

@bold{Appearance arguments.} The next keyword argument is @(racket #:samples), which determines the quality of the renderer's approximate plot (higher is better).
Following @(racket #:samples) are @(racket #:color), @(racket #:width), @(racket #:style) and @(racket #:alpha), which determine the color, width, style and opacity of the lines comprising the plot.

In general, the keyword arguments that determine the appearance of plots follow consistent naming conventions.
The most common keywords are @(racket #:color) (for fill and line colors), @(racket #:width) (for line widths), @(racket #:style) (for fill and line styles) and @(racket #:alpha).
When a function needs both a fill color and a line color, the fill color is given using @(racket #:color), and the line color is given using @(racket #:line-color) (or some variation, such as @(racket #:line1-color)). Styles follow the same rule.

Every appearance keyword argument defaults to the value of a parameter.
This allows whole families of plots to be altered with little work.
For example, setting @(racket (line-color 3)) causes every subsequent renderer that draws connected lines to draw its lines in blue.

@bold{Label argument.} Lastly, there is @(racket #:label). If given, the @(racket function) renderer will generate a label entry that @(racket plot) puts in the legend.

Not every renderer-producing function has a @(racket #:label) argument; for example, @(racket error-bars).


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

The @(racket #:sym) argument may be any integer, a Unicode character or string, or a symbol in @(racket known-point-symbols).
Use an integer when you need different points but don't care exactly what they are.
}

@doc-apply[vector-field]{
Returns a renderer that draws a vector field.

If @(racket scale) is a real number, arrow lengths are multiplied by @(racket scale).
If @(racket 'auto), the scale is calculated in a way that keeps arrows from overlapping.
If @(racket 'normalized), each arrow is made the same length: the maximum length that would have been allowed by @(racket 'auto).

An example of automatic scaling:
@interaction[#:eval plot-eval
                    (plot (vector-field (λ (x y) (vector (+ x y) (- x y)))
                                        -2 2 -2 2))]
}

@doc-apply[error-bars]{
Returns a renderer that draws error bars.
The first and second element in each vector in @(racket bars) comprise the coordinate; the third is the height.
@interaction[#:eval plot-eval
                    (plot (list (function sqr 1 7)
                                (error-bars (list (vector 2 4 12)
                                                  (vector 4 16 20)
                                                  (vector 6 36 10)))))]
}

@section{2D Line Renderers}

@doc-apply[function]{
Returns a renderer that plots a function of @italic{x}. For example, a parabola:
@interaction[#:eval plot-eval (plot (function sqr -2 2))]
}

@doc-apply[inverse]{
Like @(racket function), but regards @(racket f) as a function of @italic{y}.
For example, a parabola, an inverse parabola, and the reflection line:
@interaction[#:eval plot-eval
                    (plot (list (axes)
                                (function sqr -2 2 #:label "y = x^2")
                                (function (λ (x) x) #:color 0 #:style 'dot #:label "y = x")
                                (inverse sqr -2 2 #:color 3 #:label "x = y^2")))]
}

@doc-apply[lines]{
Returns a renderer that draws lines.
This is directly useful for plotting a time series, such as a random walk:
@interaction[#:eval plot-eval
                    (plot (lines
                           (reverse
                            (for/fold ([lst (list (vector 0 0))]) ([i  (in-range 1 200)])
                              (match-define (vector x y) (first lst))
                              (cons (vector i (+ y (* 1/100 (- (random) 1/2)))) lst)))
                           #:color 6 #:label "Random walk"))]

The @(racket parametric) and @(racket polar) functions are defined using @(racket lines).
}

@doc-apply[parametric]{
Returns a renderer that plots vector-valued functions of time.
For example, the circle as a function of time can be plotted using
@interaction[#:eval plot-eval
                    (plot (parametric (λ (t) (vector (cos t) (sin t))) 0 (* 2 pi)))]
}

@doc-apply[polar]{
Returns a renderer that plots functions from angle to radius.
Note that the angle parameters @(racket θ-min) and @(racket θ-max) default to @(racket 0) and @(racket (* 2 pi)).

For example, drawing a full circle:
@interaction[#:eval plot-eval (plot (polar (λ (θ) 1)))]
}

@doc-apply[density]{
Returns a renderer that plots an estimated density for the given points.
The bandwidth for the kernel is calculated as @(racket (* bw-adjust 1.06 sd (expt n -0.2))), where @(racket sd) is the standard deviation of the data and @(racket n) is the number of points.
Currently, the only supported kernel is the Gaussian.

For example, to plot an estimated density of the triangle distribution:
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
                    (define (f1 t) (vector (* 2 (cos (* 4/5 t)))
                                           (* 2 (sin (* 4/5 t)))))
                    (define (f2 t) (vector (* 1/2 (cos t))
                                           (* 1/2 (sin t))))
                    (plot (parametric-interval f1 f2 (- pi) pi))]
}

@doc-apply[polar-interval]{
Corresponds with @(racket polar).

@interaction[#:eval plot-eval
                    (define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
                    (define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
                    (plot (list (polar-axes #:number 10)
                                (polar-interval f1 f2 #:label "[f1,f2]")))]
}

@section{2D Contour (Isoline) Renderers}

@doc-apply[isoline]{
Returns a renderer that plots a contour line, or a line of constant value (height).
A circle of radius @(racket r), for example, is the line of constant value @(racket r) for the distance function:
@interaction[#:eval plot-eval (plot (isoline (λ (x y) (sqrt (+ (sqr x) (sqr y)))) 1.5
                                             -2 2 -2 2 #:label "z"))]
}
In this case, @(racket r) = @(racket 1.5).

This function would have been named @racket[contour], except the name was already used by a deprecated function.
It may be renamed in the future, with @racket[isoline] as an alias.

@doc-apply[contours]{
Returns a renderer that plots contour lines, or lines of constant value (height).

When @(racket levels) is @(racket 'auto), the number of contour lines and their values are chosen the same way as axis tick positions; i.e. they are chosen to be simple.
When @(racket levels) is a number, @(racket contours) chooses that number of values, evenly spaced, within the output range of @(racket f).
When @(racket levels) is a list, @(racket contours) plots contours at the values in @(racket levels).

For example, a saddle:
@interaction[#:eval plot-eval (plot (contours (λ (x y) (- (sqr x) (sqr y)))
                                              -2 2 -2 2 #:label "z"))]

The appearance keyword arguments assign a color, width, style and opacity @italic{to each contour line}.
Each can be given as a list or as a function from a list of output values of @(racket f) to a list of appearance values.
In both cases, when there are more contour lines than list elements, the colors, widths, styles and alphas in the list repeat.

For example,
@interaction[#:eval plot-eval (plot (contours (λ (x y) (- (sqr x) (sqr y)))
                                              -2 2 -2 2 #:levels 4
                                              #:colors '("blue" "red")
                                              #:widths '(4 1)
                                              #:styles '(solid dot)))]
}

@doc-apply[contour-intervals]{
Returns a renderer that fills the area between contour lines, and additionally draws contour lines.

For example, the canonical saddle, with its gradient field superimposed:
@interaction[#:eval plot-eval
                    (plot (list (contour-intervals (λ (x y) (- (sqr x) (sqr y)))
                                                   -2 2 -2 2 #:label "z")
                                (vector-field (λ (x y) (vector (* 2 x) (* -2 y)))
                                              #:color "black" #:label "Gradient")))]
}

@section{2D Rectangle Renderers}

@doc-apply[rectangles]{
Returns a renderer that draws rectangles.
The rectanges are given as a list of vectors of intervals---each vector defines the bounds of a rectangle. For example,
@interaction[#:eval plot-eval (plot (rectangles (list (vector (ivl -1 1) (ivl -1 1))
                                                      (vector (ivl 1 2) (ivl 1 2)))))]
}

@doc-apply[area-histogram]{
Returns a renderer that draws a histogram approximating the area under a curve.
The @(racket #:samples) argument determines the accuracy of the calculated areas.
@interaction[#:eval plot-eval
                    (require (only-in plot/utils linear-seq))
                    (define (f x) (exp (* -1/2 (sqr x))))
                    (plot (list (area-histogram f (linear-seq -4 4 10))
                                (function f -4 4)))]
}

@doc-apply[discrete-histogram]{
Returns a renderer that draws a discrete histogram.

@examples[#:eval plot-eval
                 (plot (discrete-histogram (list #(A 1) #(B 2) #(B 3)
                                                 (vector 'C (ivl 0.5 1.5)))))]

Use @racket[#:invert? #t] to draw horizontal bars. See @racket[stacked-histogram] for an example.

Each bar takes up exactly one plot unit, and is drawn with @racket[(* 1/2 gap)] empty space on each side.
Bar number @racket[i] is drawn at @racket[(+ x-min (* i skip))].
Thus, the first bar (@racket[i] = @racket[0]) is drawn in the interval between @racket[x-min] (default @racket[0]) and @racket[(+ x-min 1)].

To plot two histograms side-by-side, pass the appropriate @racket[x-min] value to the second renderer. For example,
@interaction[#:eval plot-eval
                    (plot (list (discrete-histogram (list #(a 1) #(b 2) #(c 3) #(d 2)
                                                          #(e 4) #(f 2.5) #(g 1))
                                                    #:label "Numbers per letter")
                                (discrete-histogram (list #(1 1) #(4 2) #(3 1.5))
                                                    #:x-min 8
                                                    #:label "Numbers per number"
                                                    #:color 2 #:line-color 2)))]
Here, the first histogram has @racket[7] bars, so the second is drawn starting at @racket[x-min] = @racket[8].

To interleave histograms, such as when plotting benchmark results, use a @racket[skip] value larger than or equal to the number of histograms, and give each histogram a different @racket[x-min].
For example,
@interaction[#:eval plot-eval
                    (plot (list (discrete-histogram
                                 '(#(Eggs 1.5) #(Bacon 2.5) #(Pancakes 3.5))
                                 #:skip 2.5 #:x-min 0
                                 #:label "AMD")
                                (discrete-histogram
                                 '(#(Eggs 1.4) #(Bacon 2.3) #(Pancakes 3.1))
                                 #:skip 2.5 #:x-min 1
                                 #:label "Intel" #:color 2 #:line-color 2))
                          #:x-label "Breakfast Food" #:y-label "Cooking Time (minutes)"
                          #:title "Cooking Times For Breakfast Food, Per Processor")]
When interleaving many histograms, consider setting the @racket[discrete-histogram-skip] parameter to change @racket[skip]'s default value.
}

@doc-apply[stacked-histogram]{
Returns a list of renderers that draw parts of a stacked histogram.
The heights of each bar section are given as a list.
@examples[#:eval plot-eval
                 (plot (stacked-histogram (list #(a (1 1 1)) #(b (1.5 3))
                                                #(c ()) #(d (1/2)))
                                          #:invert? #t
                                          #:labels '("Red" #f "Blue"))
                       #:legend-anchor 'top-right)]
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
