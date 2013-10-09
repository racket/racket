#lang scribble/manual

@(require "common.rkt")

@title[#:tag "intro"]{Introduction}

@defmodule*/no-declare[(plot plot/typed) #:link-target? #f]

@section{Plotting 2D Graphs}

To plot a one-input, real-valued function, do something like

@racketinput[(require plot)]
@interaction[#:eval plot-eval (plot (function sin (- pi) pi #:label "y = sin(x)"))]

The first argument to @(racket function) is the function to be plotted, and the @(racket #:label) argument becomes the name of the function in the legend.

If you're not using DrRacket, start with
@racketblock[(require plot)
             (plot-new-window? #t)]
to open each plot in a new window.

@section{Terminology}

In the above example, @(racket (- pi)) and @(racket pi) define the @italic{x}-axis @deftech{bounds}, or the closed interval in which to plot the @(racket sin) function.
The @(racket function) function automatically determines that the @italic{y}-axis bounds should be [-1,1].

The @(racket function) function constructs a @deftech{renderer}, which does the actual drawing. A renderer also produces legend entries, requests bounds to draw in, and requests axis ticks and tick labels.

The @(racket plot) function collects legend entries, bounds and ticks. It then sets up a @deftech{plot area} with large enough bounds to contain the renderers, draws the axes and ticks, invokes the renderers' drawing procedures, and then draws the legend.

@section{Plotting 3D Graphs}

To plot a two-input, real-valued function as a surface, try something like

@margin-note{The documentation can't show it, but in DrRacket you can rotate 3D plots by clicking on them and dragging the mouse. Try it!}

@interaction[#:eval plot-eval
                    (plot3d (surface3d (λ (x y) (* (cos x) (sin y)))
                                       (- pi) pi (- pi) pi)
                            #:title "An R × R → R function"
                            #:x-label "x" #:y-label "y" #:z-label "cos(x) sin(y)")]

This example also demonstrates using keyword arguments that change the plot, such as @(racket #:title).
In @(plot-name), every keyword argument is optional and almost all have parameterized default values.
In the case of @(racket plot3d)'s @(racket #:title), the corresponding parameter is @(racket plot-title).
That is, keyword arguments are usually shortcuts for parameterizing plots or renderers:

@margin-note{When parameterizing more than one plot, it is often easier to set parameters globally, as in @(racket (plot-title "Untitled")) and @(racket (plot3d-angle 45)).

There are many parameters that do not correspond to keyword arguments, such as @(racket plot-font-size). See @seclink["params"] for the full listing.}

@interaction[#:eval plot-eval
                    (parameterize ([plot-title  "An R × R → R function"]
                                   [plot-x-label "x"]
                                   [plot-y-label "y"]
                                   [plot-z-label "cos(x) sin(y)"])
                      (plot3d (contour-intervals3d (λ (x y) (* (cos x) (sin y)))
                                                   (- pi) pi (- pi) pi)))]

This example also demonstrates @(racket contour-intervals3d), which colors the surface between contour lines, or lines of constant height.
By default, @(racket contour-intervals3d) places the contour lines at the same heights as the ticks on the @italic{z} axis.

@section{Plotting Multiple 2D Renderers}

Renderers may be plotted together by passing them in a list:

@interaction[#:eval plot-eval
                    (plot (list (axes)
                                (function sqr -2 2)
                                (function (λ (x) x) #:color 0 #:style 'dot)
                                (inverse sqr -2 2 #:color 3)))]

Here, @(racket inverse) plots the inverse of a function. (Both @(racket function) and @(racket inverse) plot the reflection line @(racket (λ (x) x)) identically.)

Notice the numbered colors.
@(plot-name) additionally recognizes, as colors, lists of RGB values such as @(racket '(128 128 0)), @(racket color%) instances, and strings like @(racket "red") and @(racket "navajowhite").
(The last are turned into RGB triples using a @(racket color-database<%>).)
Use numbered colors when you just need different colors with good contrast, but don't particularly care what they are.

The @(racket axes) function returns a list of two renderers, one for each axis.
This list is passed in a list to @(racket plot), meaning that @(racket plot) accepts @italic{lists of lists} of renderers.
In general, both @(racket plot) and @(racket plot3d) accept a @(racket treeof) renderers.

Renderers generate legend entries when passed a @(racket #:label) argument. For example,

@interaction[#:eval plot-eval
                    (plot (list (axes)
                                (function sqr -2 2 #:label "y = x^2")
                                (function (λ (x) x) #:label "y = x" #:color 0 #:style 'dot)
                                (inverse sqr -2 2 #:label "x = y^2" #:color 3)))]

Lists of renderers are @(racket flatten)ed, and then plotted @italic{in order}. The order is more obvious with interval plots:

@interaction[#:eval plot-eval
                    (plot (list (function-interval (λ (x) (- (sin x) 3))
                                                   (λ (x) (+ (sin x) 3)))
                                (function-interval (λ (x) (- (sqr x))) sqr #:color 4
                                                   #:line1-color 4 #:line2-color 4))
                          #:x-min (- pi) #:x-max pi)]

Clearly, the blue-colored interval between sine waves is drawn first.

@section{Renderer and Plot Bounds}

In the preceeding example, the @italic{x}-axis @tech{bounds} are passed to @(racket plot) using the keyword arguments @(racket #:x-min) and @(racket x-max).
The bounds could easily have been passed in either call to @(racket function-interval) instead.
In both cases, @(racket plot) and @(racket function-interval) work together to determine @italic{y}-axis @tech{bounds} large enough for both renderers.

It is not always possible for renderers and @(racket plot) or @(racket plot3d) to determine the bounds:

@interaction[#:eval plot-eval
                    (eval:alts
                     (plot (function sqr))
                     (eval:result "" "" "plot: could not determine sensible plot bounds; got x ∈ [#f,#f], y ∈ [#f,#f]"))
                    (eval:alts
                     (plot (function sqr #f #f))
                     (eval:result "" "" "plot: could not determine sensible plot bounds; got x ∈ [#f,#f], y ∈ [#f,#f]"))
                    (eval:alts
                     (plot (function sqr (- pi)))
                     (eval:result "" "" "plot: could not determine sensible plot bounds; got x ∈ [-3.141592653589793,#f], y ∈ [#f,#f]"))
                    (eval:alts
                     (plot (list (function sqr #f 0)
                                 (function sqr 0 #f)))
                     (eval:result "" "" "plot: could not determine sensible plot bounds; got x ∈ [0,0], y ∈ [0,0]"))]

There is a difference between passing bounds to renderers and passing bounds to @(racket plot) or @(racket plot3d): bounds passed to @(racket plot) or @(racket plot3d) cannot be changed by a renderer that requests different bounds.
We might say that bounds passed to renderers are @italic{suggestions}, and bounds passed to @(racket plot) and @(racket plot3d) are @italic{commandments}.

Here is an example of commanding @(racket plot3d) to override a renderer's bounds. First, consider the plot of a sphere with radius @(racket 1):

@interaction[#:eval plot-eval (plot3d (polar3d (λ (θ ρ) 1) #:color 2 #:line-style 'transparent)
                                      #:altitude 25)]

Passing bounds to @(racket plot3d) that are smaller than [-1..1] × [-1..1] × [-1..1] cuts off the six axial poles:

@interaction[#:eval plot-eval
                    (plot3d (polar3d (λ (θ ρ) 1) #:color 2 #:line-style 'transparent)
                            #:x-min -0.8 #:x-max 0.8
                            #:y-min -0.8 #:y-max 0.8
                            #:z-min -0.8 #:z-max 0.8
                            #:altitude 25)]

@section{Plotting Multiple 3D Renderers}

Unlike with rendering 2D plots, rendering 3D plots is order-independent.
Their constituent shapes (such as polygons) are sorted by view distance and drawn back-to-front.

@interaction[#:eval plot-eval (plot3d (list (surface3d (λ (x y) 1) #:color "LavenderBlush")
                                            (surface3d (λ (x y) -1) #:color "LightSteelBlue"))
                                      #:x-min -1 #:x-max 1 #:y-min -1 #:y-max 1)]

Here, the top surface is first in the list, but the bottom surface is drawn first.

@section{Plotting to Files}

Any plot can be rendered to PNG, PDF, PS and SVG files using @(racket plot-file) and @(racket plot3d-file), to include in papers and other published media.

@section{Colors and Styles}

In papers, stick to dark, fully saturated colors for lines, and light, desaturated colors for areas and surfaces.
Papers are often printed in black and white, and sticking to this guideline will help black-and-white versions of color plots turn out nicely.

To make this easy, @(plot-name) provides numbered colors that follow these guidelines, that are designed for high contrast in color as well.
When used as line colors, numbers are interpreted as dark, fully saturated colors.
When used as area or surface colors, numbers are interpreted as light, desaturated colors.

@interaction[#:eval plot-eval
                    (parameterize ([interval-line1-width  3]
                                   [interval-line2-width  3])
                      (plot (for/list ([i  (in-range -7 13)])
                              (function-interval
                               (λ (x) (* i 1.3)) (λ (x) (+ 1 (* i 1.3)))
                               #:color i #:line1-color i #:line2-color i))
                            #:x-min -8 #:x-max 8))]

Color @(racket 0) is black for lines and white for areas.
Colors @(racket 1)..@(racket 120) are generated by rotating hues and adjusting to make neighbors more visually dissimilar.
Colors @(racket 121)..@(racket 127) are grayscale.

Colors @(racket -7)..@(racket -1) are also grayscale because before @(racket 0), colors repeat.
That is, colors @(racket -128)..@(racket -1) are identical to colors @(racket 0)..@(racket 127).
Colors also repeat after @(racket 127).

If the paper will be published in black and white, use styles as well as, or instead of, colors.
There are @(racket 5) numbered pen styles and @(racket 7) numbered brush styles, which also repeat.

@interaction[#:eval plot-eval
                    (parameterize ([line-color  "black"]
                                   [interval-color  "black"]
                                   [interval-line1-color  "black"]
                                   [interval-line2-color  "black"]
                                   [interval-line1-width  3]
                                   [interval-line2-width  3])
                      (plot (for/list ([i  (in-range 7)])
                              (function-interval
                               (λ (x) (* i 1.5)) (λ (x) (+ 1 (* i 1.5)))
                               #:style i #:line1-style i #:line2-style i))
                            #:x-min -8 #:x-max 8))]
