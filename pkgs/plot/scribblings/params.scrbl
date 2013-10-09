#lang scribble/manual

@(require "common.rkt")

@title[#:tag "params"]{Plot and Renderer Parameters}

@declare-exporting[plot]
@defmodule*/no-declare[(plot plot/typed) #:link-target? #f]

@section{Compatibility}

@doc-apply[plot-deprecation-warnings?]{
When @(racket #t), prints a deprecation warning to @(racket current-error-port) on the first use of @(racket mix), @(racket line), @(racket contour), @(racket shade), @(racket surface), or a keyword argument of @(racket plot) or @(racket plot3d) that exists solely for backward compatibility.
}

@section{Output}

@doc-apply[plot-new-window?]{When @(racket #t), @(racket plot) and @(racket plot3d) open a new window for each plot instead of returning an @(racket image-snip%).

Users of command-line Racket, which cannot display image snips, should enter
@racketblock[(plot-new-window? #t)]
before using @(racket plot) or @(racket plot3d).}

@doc-apply[plot-width]
@doc-apply[plot-height]{The width and height of a plot, in logical drawing units (e.g. pixels for bitmap plots).}

@doc-apply[plot-jpeg-quality]{
The quality of JPEG images written by @(racket plot-file) and @(racket plot3d-file). See @(method bitmap% save-file).
}

@doc-apply[plot-ps/pdf-interactive?]{
If @(racket #t), @(racket plot-file) and @(racket plot3d-file) open a dialog when writing PostScript or PDF files. See @(racket post-script-dc%) and @(racket pdf-dc%).
}

@section{General Appearance}

@doc-apply[plot-foreground]
@doc-apply[plot-background]{
The plot foreground and background color.
That both are @(racket 0) by default is not a mistake: for foreground colors, @(racket 0) is interpreted as black; for background colors, @(racket 0) is interpreted as white.
See @(racket ->pen-color) and @(racket ->brush-color) for details on how @(plot-name) interprets integer colors.}
@doc-apply[plot-foreground-alpha]
@doc-apply[plot-background-alpha]{The opacity of the background and foreground colors.}
@doc-apply[plot-font-size]{The font size of the title, axis labels, tick labels, and other labels, in drawing units.}
@doc-apply[plot-font-family]{The font family used for the title and labels.}
@doc-apply[plot-line-width]{The width of axis lines and major tick lines. (Minor tick lines are half this width.)}

@doc-apply[plot-legend-anchor]{Controls the placement of the legend.}
@doc-apply[plot-legend-box-alpha]{The opacity of the filled rectangle behind the legend entries.}

@doc-apply[plot-tick-size]{The length of tick lines, in drawing units.}

@doc-apply[plot-title]
@doc-apply[plot-x-label]
@doc-apply[plot-y-label]
@doc-apply[plot-z-label]{The title and axis labels. A @(racket #f) value means the label is not drawn and takes no space. A @(racket "") value effectively means the label is not drawn, but it takes space.
}
@doc-apply[plot-x-far-label]
@doc-apply[plot-y-far-label]
@doc-apply[plot-z-far-label]{
The axis labels for ``far'' axes. See @racket[plot-x-ticks] for a discussion of near and far axes.
}

@doc-apply[plot-x-tick-label-anchor]
@doc-apply[plot-x-tick-label-angle]
@doc-apply[plot-y-tick-label-anchor]
@doc-apply[plot-y-tick-label-angle]
@doc-apply[plot-x-far-tick-label-anchor]
@doc-apply[plot-x-far-tick-label-angle]
@doc-apply[plot-y-far-tick-label-anchor]
@doc-apply[plot-y-far-tick-label-angle]{
Anchor and angles for axis tick labels (2D only).
Angles are in degrees.
The anchor refers to the part of the label attached to the end of the tick line.

Set these when labels would otherwise overlap; for example, in histograms with long category names.
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-tick-label-anchor  'top-right]
                                   [plot-x-tick-label-angle   30])
                      (plot (discrete-histogram '(#(really-long-category-name-1 2)
                                                  #(long-category-name-2 1.75)
                                                  #(long-category-name-3 2.5)))))]
}

@doc-apply[plot-x-axis?]
@doc-apply[plot-x-far-axis?]
@doc-apply[plot-y-axis?]
@doc-apply[plot-y-far-axis?]
@doc-apply[plot-z-axis?]
@doc-apply[plot-z-far-axis?]{
When any of these is @racket[#f], the corresponding axis is not drawn.

Use these along with @racket[x-axis] and @racket[y-axis] if you want axes that intersect the origin or some other point.
}

@doc-apply[plot-animating?]{
When @(racket #t), certain renderers draw simplified plots to speed up drawing. @(plot-name) sets it to @(racket #t), for example, when a user is clicking and dragging a 3D plot to rotate it.
}

@doc-apply[animated-samples]{
Given a number of samples, returns the number of samples to use.
This returns @racket[samples] when @racket[plot-animating?] is @racket[#f].
}

@doc-apply[plot-decorations?]{
When @(racket #f), axes, axis labels, ticks, tick labels, and the title are not drawn.
}

@section{Lines}

@doc-apply[line-samples]
@doc-apply[line-color]
@doc-apply[line-width]
@doc-apply[line-style]
@doc-apply[line-alpha]

@section{Intervals}

@doc-apply[interval-color]
@doc-apply[interval-style]
@doc-apply[interval-line1-color]
@doc-apply[interval-line1-width]
@doc-apply[interval-line1-style]
@doc-apply[interval-line2-color]
@doc-apply[interval-line2-width]
@doc-apply[interval-line2-style]
@doc-apply[interval-alpha]

@section{Points}

@doc-apply[point-sym]
@doc-apply[point-color]
@doc-apply[point-size]
@doc-apply[point-line-width]
@doc-apply[point-alpha]

@section{Vector Fields}

@doc-apply[vector-field-samples]
@doc-apply[vector-field-color]
@doc-apply[vector-field-line-width]
@doc-apply[vector-field-line-style]
@doc-apply[vector-field-scale]
@doc-apply[vector-field-alpha]

@doc-apply[vector-field3d-samples]

@section{Error Bars}

@doc-apply[error-bar-width]
@doc-apply[error-bar-color]
@doc-apply[error-bar-line-width]
@doc-apply[error-bar-line-style]
@doc-apply[error-bar-alpha]

@section{Contours and Contour Intervals}

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

@section{Rectangles}

@doc-apply[rectangle-color]
@doc-apply[rectangle-style]
@doc-apply[rectangle-line-color]
@doc-apply[rectangle-line-width]
@doc-apply[rectangle-line-style]
@doc-apply[rectangle-alpha]

@doc-apply[rectangle3d-line-width]

@doc-apply[discrete-histogram-gap]
@doc-apply[discrete-histogram-skip]
@doc-apply[discrete-histogram-invert?]

@doc-apply[stacked-histogram-alphas]
@doc-apply[stacked-histogram-colors]
@doc-apply[stacked-histogram-line-colors]
@doc-apply[stacked-histogram-line-styles]
@doc-apply[stacked-histogram-line-widths]
@doc-apply[stacked-histogram-styles]

@section{Decorations}

These parameters do not control the @italic{typical} appearance of plots.
Instead, they control the look of renderers that add specific decorations, such as labeled points.

@doc-apply[x-axis-alpha]
@doc-apply[y-axis-alpha]
@doc-apply[z-axis-alpha]

@doc-apply[x-axis-far?]
@doc-apply[y-axis-far?]
@doc-apply[z-axis-far?]

@doc-apply[x-axis-ticks?]
@doc-apply[y-axis-ticks?]
@doc-apply[z-axis-ticks?]

@doc-apply[x-axis-labels?]
@doc-apply[y-axis-labels?]
@doc-apply[z-axis-labels?]

@doc-apply[polar-axes-number]
@doc-apply[polar-axes-alpha]
@doc-apply[polar-axes-ticks?]
@doc-apply[polar-axes-labels?]

@doc-apply[label-anchor]
@doc-apply[label-angle]
@doc-apply[label-alpha]
@doc-apply[label-point-size]

@section{3D General Appearance}

@doc-apply[plot3d-samples]
@doc-apply[plot3d-angle]
@doc-apply[plot3d-altitude]
@doc-apply[plot3d-ambient-light]
@doc-apply[plot3d-diffuse-light?]
@doc-apply[plot3d-specular-light?]

@section{Surfaces}

@doc-apply[surface-color]
@doc-apply[surface-style]
@doc-apply[surface-line-color]
@doc-apply[surface-line-width]
@doc-apply[surface-line-style]
@doc-apply[surface-alpha]

@section{Contour Surfaces}

Contour surface renderers use shared contour parameters except for the following three.

@doc-apply[contour-interval-line-colors]
@doc-apply[contour-interval-line-widths]
@doc-apply[contour-interval-line-styles]

@section{Isosurfaces}

Single isosurfaces (@(racket isosurface3d)) use surface parameters.
Nested isosurfaces (@(racket isosurfaces3d)) use the following.

@doc-apply[default-isosurface-colors]
@doc-apply[default-isosurface-line-colors]

@doc-apply[isosurface-levels]
@doc-apply[isosurface-colors]
@doc-apply[isosurface-styles]
@doc-apply[isosurface-line-colors]
@doc-apply[isosurface-line-widths]
@doc-apply[isosurface-line-styles]
@doc-apply[isosurface-alphas]
