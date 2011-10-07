#lang scribble/manual

@(require "common.rkt")

@title[#:tag "utils"]{Plot Utilities}

@defmodule[plot/utils]

@doc-apply[degrees->radians]{
Converts degrees to radians.
}

@doc-apply[radians->degrees]{
Converts radians to degrees.
}

@doc-apply[digits-for-range]{
Given a range, returns the number of decimal places necessary to distinguish numbers in the range. This may return negative numbers for large ranges.

@examples[#:eval plot-eval
                 (digits-for-range 0.01 0.02)
                 (digits-for-range 0 100000)]
}

@doc-apply[real->plot-label]{
Converts a real number to a plot label. Used to format axis tick labels, @(racket point-label)s, and numbers in legend entries.

@examples[#:eval plot-eval
                 (let ([d  (digits-for-range 0.01 0.03)])
                   (real->plot-label 0.02555555 d))
                 (real->plot-label 2352343 -2)
                 (real->plot-label 1000000000. 4)
                 (real->plot-label 1000000000.1234 4)]
}

@doc-apply[->plot-label]{
Converts a Racket value to a label. Used by @(racket discrete-histogram) and @(racket discrete-histogram3d).
}

@doc-apply[real->string/trunc]{
Like @(racket real->decimal-string), but removes trailing zeros and a trailing decimal point.
}

@doc-apply[linear-seq]{
Returns a list of evenly spaced real numbers between @(racket start) and @(racket end).
If @(racket start?) is @(racket #t), the list includes @(racket start).
If @(racket end?) is @(racket #t), the list includes @(racket end).

This function is used internally to generate sample points.

@examples[#:eval plot-eval
                 (linear-seq 0 1 5)
                 (linear-seq 0 1 5 #:start? #f)
                 (linear-seq 0 1 5 #:end? #f)
                 (linear-seq 0 1 5 #:start? #f #:end? #f)]
}

@doc-apply[linear-seq*]{
Like @(racket linear-seq), but accepts a list of reals instead of a start and end.
The @(racket #:start?) and @(racket #:end?) keyword arguments work as in @(racket linear-seq).
This function does not guarantee that each inner value will be in the returned list.

@examples[#:eval plot-eval
                 (linear-seq* '(0 1 2) 5)
                 (linear-seq* '(0 1 2) 6)
                 (linear-seq* '(0 1 0) 5)]
}

@doc-apply[bounds->intervals]{
Given a list of points, returns intervals between each pair.

Use this to construct inputs for @(racket rectangles) and @(racket rectangles3d).
@examples[#:eval plot-eval (bounds->intervals (linear-seq 0 1 5))]
}

@doc-apply[color-seq]{
Interpolates between colors---red, green and blue components separately---using @(racket linear-seq).
The @(racket #:start?) and @(racket #:end?) keyword arguments work as in @(racket linear-seq).

@examples[#:eval plot-eval (plot (contour-intervals (λ (x y) (+ x y)) -2 2 -2 2
                                                    #:levels 4 #:contour-styles '(transparent)
                                                    #:colors (color-seq "red" "blue" 5)))]
}

@doc-apply[color-seq*]{
Interpolates between colors---red, green and blue components separately---using @(racket linear-seq*).
The @(racket #:start?) and @(racket #:end?) keyword arguments work as in @(racket linear-seq).

@examples[#:eval plot-eval (plot (contour-intervals (λ (x y) (+ x y)) -2 2 -2 2
                                                    #:levels 4 #:contour-styles '(transparent)
                                                    #:colors (color-seq* '(red white blue) 5)))]
}

@doc-apply[->color]{
Converts a non-integer plot color to an RGB triplet.

Symbols are converted to strings, and strings are looked up in a @(racket color-database<%>).
Lists are unchanged, and @(racket color%) objects are converted straightforwardly.

@examples[#:eval plot-eval
                 (->color 'navy)
                 (->color "navy")
                 (->color '(36 36 140))
                 (->color (make-object color% 36 36 140))]

This function does not convert integers to RGB triplets, because there is no way for it to know whether the color will be used for a pen or for a brush.
Use @(racket ->pen-color) and @(racket ->brush-color) to convert integers.
}

@doc-apply[->pen-color]{
Converts a @italic{line} color to an RGB triplet. This function interprets integer colors as darker and more saturated than @(racket ->brush-color) does.

Non-integer colors are converted using @(racket ->color).
Integer colors are chosen for good pairwise contrast, especially between neighbors.
Integer colors repeat starting with @(racket 8).

@examples[#:eval plot-eval
                 (equal? (->pen-color 0) (->pen-color 8))
                 (plot (contour-intervals
                        (λ (x y) (+ x y)) -2 2 -2 2
                        #:levels 7 #:contour-styles '(transparent)
                        #:colors (map ->pen-color (build-list 8 values))))]
}

@doc-apply[->brush-color]{
Converts a @italic{fill} color to an RGB triplet. This function interprets integer colors as lighter and less saturated than @(racket ->pen-color) does.

Non-integer colors are converted using @(racket ->color).
Integer colors are chosen for good pairwise contrast, especially between neighbors.
Integer colors repeat starting with @(racket 8).

@examples[#:eval plot-eval
                 (equal? (->brush-color 0) (->brush-color 8))
                 (plot (contour-intervals
                        (λ (x y) (+ x y)) -2 2 -2 2
                        #:levels 7 #:contour-styles '(transparent)
                        #:colors (map ->brush-color (build-list 8 values))))]

In the above example, @(racket map)ping @(racket ->brush-color) over the list is actually unnecessary, because @(racket contour-intervals) uses @(racket ->brush-color) internally to convert fill colors.

The @(racket function-interval) function generally plots areas using a fill color and lines using a line color.
Both kinds of color have the default value @(racket 3).
The following example reverses the default behavior; i.e it draws areas using @italic{line} color @(racket 3) and lines using @italic{fill} color @(racket 3):
@interaction[#:eval plot-eval (plot (function-interval sin (λ (x) 0) -4 4
                                                       #:color (->pen-color 3)
                                                       #:line1-color (->brush-color 3)
                                                       #:line2-color (->brush-color 3)
                                                       #:line1-width 4 #:line2-width 4))]
}

@doc-apply[->pen-style]{
Converts a symbolic pen style or a number to a symbolic pen style.
Symbols are unchanged.
Integer pen styles repeat starting at @(racket 5).

@examples[#:eval plot-eval
                 (eq? (->pen-style 0) (->pen-style 5))
                 (map ->pen-style '(0 1 2 3 4))]
}

@doc-apply[->brush-style]{
Converts a symbolic brush style or a number to a symbolic brush style.
Symbols are unchanged.
Integer brush styles repeat starting at @(racket 7).

@examples[#:eval plot-eval
                 (eq? (->brush-style 0) (->brush-style 7))
                 (map ->brush-style '(0 1 2 3))
                 (map ->brush-style '(4 5 6))]
}

@defstruct[invertible-function ([f (real? . -> . real?)] [finv (real? . -> . real?)])]{
Represents an invertible function.

The function itself is @(racket f), and its inverse is @(racket finv).
Because @(racket real?)s can be inexact, this invariant must be approximate and therefore cannot be enforced.
(For example, @(racket (exp (log 10))) = @(racket 10.000000000000002).)
The obligation to maintain it rests on whomever constructs one.

An axis transform such as @(racket plot-x-transform) is a function from bounds @(racket start end) to an @(racket invertible-function) for which @(racket (f start)) = @(racket start) and @(racket (f end)) = @(racket end) (approximately), and the same is true of @(racket finv).
The function @(racket f) is used to transform points before drawing; its inverse @(racket finv) is used to generate samples that will be evenly spaced after being transformed by @(racket f).

(Technically, because of the way PLoT uses @(racket invertible-function), @(racket f) must only be a left inverse of @(racket finv); there is no requirement that @(racket f) also be a right inverse of @(racket finv).)
}

@doc-apply[nonlinear-seq]{
Generates a list of reals that, if transformed using @(racket transform), would be evenly spaced.
This is used to generate samples for transformed axes.
@examples[#:eval plot-eval
                 (linear-seq 1 10 4)
                 (nonlinear-seq 1 10 4 log-transform)
                 (parameterize ([plot-x-transform  log-transform])
                   (plot (area-histogram sqr (nonlinear-seq 1 10 4 log-transform))))]
}

@defstruct[mapped-function ([f (any/c . -> . any/c)] [fmap ((listof any/c) . -> . (listof any/c))])]{
Represents a function that maps over lists differently than @(racket (map f xs)).

With some functions, mapping over a list can be done much more quickly if done specially.
(An example is a piecewise function with many pieces that first must decide which interval its input belongs to. Deciding that for many inputs can be done more efficiently by sorting all the inputs first.)
Renderer-producing functions that accept a @(racket (real? . -> . real?)) also accept a @(racket mapped-function), and use its @(racket fmap) to sample more efficiently.
}

@doc-apply[kde]{
Given samples and a kernel bandwidth, returns a @(racket mapped-function) representing a kernel density estimate, and bounds, outside of which the density estimate is zero. Used by @(racket density).
}
