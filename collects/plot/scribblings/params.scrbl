#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "params"]{Plot and Renderer Parameters}

@section{Shared 2D/3D Parameters}

@subsection{Compatibility}

@doc-apply[plot-deprecation-warnings?]{
When @(racket #t), prints a deprecation warning to @(racket current-error-port) on the first use of @(racket mix), @(racket line), @(racket contour), @(racket shade), @(racket surface), or a keyword argument of @(racket plot) or @(racket plot3d) that exists solely for backward compatibility.
}

@subsection{Output}

@doc-apply[plot-new-window?]{When @(racket #t), @(racket plot) and @(racket plot3d) open a new window for each plot instead of returning an @(racket image-snip%).

Users of command-line Racket, which cannot display image snips, should enter

@racketblock[(plot-new-window? #t)]

before using @(racket plot) or @(racket plot3d).}

@doc-apply[plot-width]
@doc-apply[plot-height]{The width and height of a plot, in logical drawing units (e.g. pixels for bitmap plots).}

@doc-apply[plot-jpeg-quality]
@doc-apply[plot-ps-interactive?]
@doc-apply[plot-pdf-interactive?]

@subsection{Axis Transforms}

@doc-apply[plot-x-transform]
@doc-apply[plot-y-transform]
@doc-apply[plot-z-transform]

@doc-apply[id-transform]{
The default transform for all axes.
}

@doc-apply[log-transform]{
A log transform. Use this to generate plots with log-scale axes. Any log-scaled axis must be on a positive interval.

@interaction[#:eval plot-eval
                    (parameterize ([plot-y-transform  log-transform])
                      (plot (function (λ (x) x) 1 2)))
                    (parameterize ([plot-x-transform  log-transform])
                      (plot (function (λ (x) x) -1 1)))]
}

@doc-apply[cbrt-transform]{
A "cube-root" transform. Unlike the log transform, it is defined on the entire real line, making it better for testing the appearance of plots with nonlinearly transformed axes.
}

@doc-apply[hand-drawn-transform]{
An @italic{extremely important} test case, which makes sure that @(plot-name) can use any monotone, invertible function as an axis transform.

The @(racket freq) parameter controls the ``shakiness'' of the transform. At high values, it makes plots look like Peanuts cartoons. For example,

@interaction[#:eval plot-eval (parameterize ([plot-x-transform  (hand-drawn-transform 200)]
                                             [plot-y-transform  (hand-drawn-transform 200)])
                                (plot (function sqr -1 1)))]
}

@subsection{General Appearance}

@doc-apply[plot-foreground]
@doc-apply[plot-background]{The plot foreground and background color. That both are @(racket 0) by default is not a mistake: for foreground colors, @(racket 0) is interpreted as black; for background colors, @(racket 0) is interpreted as white. See @(racket plot-color/c) for details on integer-indexed colors.}
@doc-apply[plot-font-size]{The font size of the title, axis labels, tick labels, and other labels, in drawing units.}
@doc-apply[plot-font-family]{The font family used for the title and labels.}
@doc-apply[plot-line-width]{The width of axis lines and major tick lines. (Minor tick lines are half this width.)}

@doc-apply[plot-legend-anchor]{Controls the placement of the legend.}
@doc-apply[plot-legend-box-alpha]{The opacity of the filled rectangle behind the legend entries.}

@doc-apply[plot-tick-size]{The length of tick lines, in drawing units.}
@doc-apply[plot-tick-skip]{Controls the spacing between major ticks for renderers that use the default tick function, such as the renderers returned by @(racket function) and @(racket surface3d). With the default value @(racket 2), every other tick is major. A tick at @(racket 0) is always major. Major ticks are thicker and labeled; minor ticks are thinner and unlabeled.}

@doc-apply[plot-title]
@doc-apply[plot-x-label]
@doc-apply[plot-y-label]
@doc-apply[plot-z-label]{The title and axis labels. A @(racket #f) value means the label is not drawn and takes no space. A @(racket "") value effectively means the label is not drawn, but it takes space.
}

@subsection{Lines}

@doc-apply[line-samples]
@doc-apply[line-color]
@doc-apply[line-width]
@doc-apply[line-style]
@doc-apply[line-alpha]

@subsection{Intervals}

@doc-apply[interval-color]
@doc-apply[interval-style]
@doc-apply[interval-line1-color]
@doc-apply[interval-line1-width]
@doc-apply[interval-line1-style]
@doc-apply[interval-line2-color]
@doc-apply[interval-line2-width]
@doc-apply[interval-line2-style]
@doc-apply[interval-alpha]

@subsection{Points}

@doc-apply[point-sym]
@doc-apply[point-color]
@doc-apply[point-size]
@doc-apply[point-line-width]
@doc-apply[point-alpha]

@subsection{Vector Fields}

@doc-apply[vector-field-samples]
@doc-apply[vector-field-color]
@doc-apply[vector-field-line-width]
@doc-apply[vector-field-line-style]
@doc-apply[vector-field-scale]
@doc-apply[vector-field-alpha]

@subsection{Error Bars}

@doc-apply[error-bar-width]
@doc-apply[error-bar-color]
@doc-apply[error-bar-line-width]
@doc-apply[error-bar-line-style]
@doc-apply[error-bar-alpha]

@subsection{Contours and Contour Intervals}

@doc-apply[default-contour-colors]
@doc-apply[default-contour-fill-colors]

@doc-apply[contour-samples]
@doc-apply[contour-levels]
@doc-apply[contour-colors]
@doc-apply[contour-widths]
@doc-apply[contour-styles]
@doc-apply[contour-alphas]

@doc-apply[contour-interval-colors]
@doc-apply[contour-interval-styles]
@doc-apply[contour-interval-alphas]

@subsection{Rectangles}

@doc-apply[rectangle-color]
@doc-apply[rectangle-style]
@doc-apply[rectangle-line-color]
@doc-apply[rectangle-line-width]
@doc-apply[rectangle-line-style]
@doc-apply[rectangle-alpha]

@doc-apply[rectangle3d-line-width]

@doc-apply[discrete-histogram-gap]

@subsection{Decorations}

These parameters do not control the @italic{typical} appearance of plots. Instead, they control the look of renderers that add specific decorations, such as labeled points.

@doc-apply[x-axis-ticks?]
@doc-apply[y-axis-ticks?]
@doc-apply[z-axis-ticks?]

@doc-apply[polar-axes-number]
@doc-apply[polar-axes-ticks?]

@doc-apply[label-anchor]
@doc-apply[label-angle]
@doc-apply[label-alpha]
@doc-apply[label-point-size]

@section{3D-Specific Parameters}

@subsection{3D General Appearance}

@doc-apply[plot3d-samples]
@doc-apply[plot3d-animating?]
@doc-apply[plot3d-angle]
@doc-apply[plot3d-altitude]
@doc-apply[plot3d-ambient-light-value]
@doc-apply[plot3d-diffuse-light?]
@doc-apply[plot3d-specular-light?]

@subsection{Surfaces}

@doc-apply[surface-color]
@doc-apply[surface-style]
@doc-apply[surface-line-color]
@doc-apply[surface-line-width]
@doc-apply[surface-line-style]
@doc-apply[surface-alpha]

@subsection{Contour Surfaces}

Contour surface renderers use shared contour parameters except for the following three.

@doc-apply[contour-interval-line-colors]
@doc-apply[contour-interval-line-widths]
@doc-apply[contour-interval-line-styles]

@subsection{Isosurfaces}

Single isosurfaces (@(racket isosurface3d)) use surface parameters. Nested isosurfaces (@(racket isosurfaces3d)) use the following.

@doc-apply[default-isosurface-colors]
@doc-apply[default-isosurface-line-colors]

@doc-apply[isosurface-levels]
@doc-apply[isosurface-colors]
@doc-apply[isosurface-line-colors]
@doc-apply[isosurface-line-widths]
@doc-apply[isosurface-line-styles]
@doc-apply[isosurface-alphas]
