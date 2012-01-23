#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "nonrenderer"]{Nonrenderers}

The following functions create @deftech{nonrenderers}, or plot elements that draw nothing in the plot.

@doc-apply[x-ticks]
@doc-apply[y-ticks]
@doc-apply[z-ticks]{
The @racket[x-ticks], @racket[y-ticks] and @racket[z-ticks] return a nonrenderer that adds custom ticks to a 2D or 3D plot.

Although @racket[ticks-add] allows placing arbitrary major and minor ticks on an axis, it does not allow them to be formatted differently from the other ticks on the same axis.
Use one of these functions to get maximum control.

@examples[#:eval plot-eval
                 (parameterize ([plot-x-ticks  no-ticks])
                   (plot (list (function sin (- pi) pi)
                               (x-ticks (list (tick (- pi) #t "-π")
                                              (tick (* -3/4 pi) #f "")
                                              (tick (* -1/2 pi) #t "-π/2")
                                              (tick (* -1/4 pi) #f "")
                                              (tick 0 #t "0")
                                              (tick (* 1/4 pi) #f "")
                                              (tick (* 1/2 pi) #t "π/2")
                                              (tick (* 3/4 pi) #f "")
                                              (tick pi #t "π")))
                               (axes))))]
When considering using one of these functions, remember that minor tick labels are never drawn,
and that including a @racket[z-ticks] nonrenderer will not add extra contour lines to contour plots.
}

@doc-apply[invisible-rect]{
Returns a nonrenderer that simply takes up space in the plot. Use this to cause the plot area to include a minimal rectangle.
@examples[#:eval plot-eval
                 (plot (list (function sin (- pi) pi)
                             (invisible-rect #f #f -2 2)))]
}

@doc-apply[invisible-rect3d]{
Returns a nonrenderer that simply takes up space in the plot. Use this to cause the plot area to include a minimal rectangle.
See @racket[invisible-rect] for a 2D example.
}
