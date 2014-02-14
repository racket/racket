#lang scribble/manual

@(require "common.rkt")

@title[#:tag "utils"]{Plot Utilities}

@declare-exporting[plot/utils]
@defmodule*/no-declare[(plot/utils plot/typed/utils)]

@;====================================================================================================
@section{Formatting}

@doc-apply[digits-for-range]{
Given a range, returns the number of decimal places necessary to distinguish numbers in the range.
This may return negative numbers for large ranges.

@examples[#:eval plot-eval
                 (digits-for-range 0.01 0.02)
                 (digits-for-range 0 100000)]
}

@doc-apply[real->plot-label]{
Converts a real number to a plot label.
Used to format axis tick labels, @(racket point-label)s, and numbers in legend entries.

@examples[#:eval plot-eval
                 (let ([d  (digits-for-range 0.01 0.03)])
                   (real->plot-label 0.02555555 d))
                 (real->plot-label 2352343 -2)
                 (real->plot-label 1000000000. 4)
                 (real->plot-label 1000000000.1234 4)]
}

@doc-apply[ivl->plot-label]{
Converts an interval to a plot label.

If @racket[i] = @racket[(ivl x-min x-max)], the number of digits used is @racket[(digits-for-range x-min x-max 10 extra-digits)] when both endpoints are @racket[rational?].
Otherwise, it is unspecified---but will probably remain @racket[15].
@examples[#:eval plot-eval
                 (ivl->plot-label (ivl -10.52312 10.99232))
                 (ivl->plot-label (ivl -inf.0 pi))]
}

@doc-apply[->plot-label]{
Converts a Racket value to a label. Used by @(racket discrete-histogram) and @(racket discrete-histogram3d).
}

@doc-apply[real->string/trunc]{
Like @(racket real->decimal-string), but removes any trailing zeros and any trailing decimal point.
}

@doc-apply[real->decimal-string*]{
Like @racket[real->decimal-string], but accepts both a maximum and minimum number of digits.
@examples[#:eval plot-eval
                 (real->decimal-string* 1 5 10)
                 (real->decimal-string* 1.123456 5 10)
                 (real->decimal-string* 1.123456789123456 5 10)]
Applying @racket[(real->decimal-string* x min-digits)] yields the same value as @racket[(real->decimal-string x min-digits)].
}

@doc-apply[integer->superscript]{
Converts an integer into a string of superscript Unicode characters.
@examples[#:eval plot-eval
                 (integer->superscript -1234567890)]
Systems running some out-of-date versions of Windows XP have difficulty with Unicode superscripts for 4 and up.
Because @racket[integer->superscript] is used by every number formatting function to format exponents, if you have such a system, @(plot-name) will apparently not format all numbers with exponents correctly (until you update it).
}

@;{
@doc-apply[format-tick-labels]{
}
}

@;====================================================================================================
@section{Sampling}

@doc-apply[linear-seq]{
Returns a list of uniformly spaced real numbers between @(racket start) and @(racket end).
If @(racket start?) is @(racket #t), the list includes @(racket start).
If @(racket end?) is @(racket #t), the list includes @(racket end).

This function is used internally to generate sample points.

@examples[#:eval plot-eval
                 (linear-seq 0 1 5)
                 (linear-seq 0 1 5 #:start? #f)
                 (linear-seq 0 1 5 #:end? #f)
                 (linear-seq 0 1 5 #:start? #f #:end? #f)
                 (define xs (linear-seq -1 1 11))
                 (plot (lines (map vector xs (map sqr xs))))]
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

@doc-apply[nonlinear-seq]{
Generates a list of reals that, if transformed using @(racket transform), would be uniformly spaced.
This is used to generate samples for transformed axes.
@examples[#:eval plot-eval
                 (linear-seq 1 10 4)
                 (nonlinear-seq 1 10 4 log-transform)
                 (parameterize ([plot-x-transform  log-transform])
                   (plot (area-histogram sqr (nonlinear-seq 1 10 4 log-transform))))]
}

@;{
@doc-apply[build-linear-seq]
@doc-apply[make-function->sampler]
@doc-apply[make-2d-function->sampler]
@doc-apply[make-3d-function->sampler]
@doc-apply[sample-exact->inexact]
@doc-apply[2d-sample-exact->inexact]
@doc-apply[3d-sample-exact->inexact]
}

@defstruct[mapped-function ([f (any/c . -> . any/c)] [fmap ((listof any/c) . -> . (listof any/c))])]{
Represents a function that maps over lists differently than @(racket (map f xs)).

With some functions, mapping over a list can be done much more quickly if done specially.
(An example is a piecewise function with many pieces that first must decide which interval its input belongs to. Deciding that for many inputs can be done more efficiently by sorting all the inputs first.)
Renderer-producing functions that accept a @(racket (real? . -> . real?)) also accept a @(racket mapped-function), and use its @(racket fmap) to sample more efficiently.
}

@doc-apply[kde]{
Given optionally weighted samples and a kernel bandwidth, returns a @(racket mapped-function) representing a kernel density estimate, and bounds, outside of which the density estimate is zero.
Used by @(racket density).
}

@;====================================================================================================
@section{Plot Colors and Styles}

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
Integer colors repeat starting with @(racket 128).

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
Integer colors repeat starting with @(racket 128).

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

@;====================================================================================================
@section{Plot-Specific Math}

@;----------------------------------------------------------------------------------------------------
@subsection{Real Functions}

@doc-apply[polar->cartesian]{
Converts 2D polar coordinates to 3D cartesian coordinates.
}

@doc-apply[3d-polar->3d-cartesian]{
Converts 3D polar coordinates to 3D cartesian coordinates.
See @racket[parametric3d] for an example of use.
}

@doc-apply[ceiling-log/base]{
Like @racket[(ceiling (/ (log x) (log b)))], but @racket[ceiling-log/base] is not susceptible to floating-point error.
@examples[#:eval plot-eval
                 (ceiling (/ (log 100) (log 10)))
                 (ceiling-log/base 10 100)
                 (ceiling (/ (log 1/1000) (log 10)))
                 (ceiling-log/base 10 1/1000)]
Various number-formatting functions use this.
}

@doc-apply[floor-log/base]{
Like @racket[(floor (/ (log x) (log b)))], but @racket[floor-log/base] is not susceptible to floating-point error.
@examples[#:eval plot-eval
                 (floor (/ (log 100) (log 10)))
                 (floor-log/base 10 100)
                 (floor (/ (log 1000) (log 10)))
                 (floor-log/base 10 1000)]
This is a generalization of @racket[order-of-magnitude].
}

@doc-apply[maybe-inexact->exact]{
Returns @racket[#f] if @racket[x] is @racket[#f]; otherwise @racket[(inexact->exact x)].
Use this to convert interval endpoints, which may be @racket[#f], to exact numbers.
}

@;----------------------------------------------------------------------------------------------------
@subsection[#:tag "math.vectors"]{Vector Functions}

@doc-apply[v+]
@doc-apply[v-]
@doc-apply[vneg]
@doc-apply[v*]
@doc-apply[v/]{
Vector arithmetic. Equivalent to @racket[vector-map]p-ing arithmetic operators over vectors, but specialized so that 2- and 3-vector operations are much faster.
@examples[#:eval plot-eval
                 (v+ #(1 2) #(3 4))
                 (v- #(1 2) #(3 4))
                 (vneg #(1 2))
                 (v* #(1 2 3) 2)
                 (v/ #(1 2 3) 2)]
}

@doc-apply[v=]{
Like @racket[equal?] specialized to numeric vectors, but compares elements using @racket[=].
@examples[#:eval plot-eval
                 (equal? #(1 2) #(1 2))
                 (equal? #(1 2) #(1.0 2.0))
                 (v= #(1 2) #(1.0 2.0))]
}

@doc-apply[vcross]{
Returns the right-hand vector cross product of @racket[v1] and @racket[v2].
@examples[#:eval plot-eval
                 (vcross #(1 0 0) #(0 1 0))
                 (vcross #(0 1 0) #(1 0 0))
                 (vcross #(0 0 1) #(0 0 1))]
}

@doc-apply[vcross2]{
Returns the signed area of the 2D parallelogram with sides @racket[v1] and @racket[v2].
Equivalent to @racket[(vector-ref (vcross (vector-append v1 #(0)) (vector-append v2 #(0))) 2)] but faster.
@examples[#:eval plot-eval
                 (vcross2 #(1 0) #(0 1))
                 (vcross2 #(0 1) #(1 0))]
}

@doc-apply[vdot]{
Returns the dot product of @racket[v1] and @racket[v2].
}

@doc-apply[vmag^2]{
Returns the squared magnitude of @racket[v]. Equivalent to @racket[(vdot v v)].
}

@doc-apply[vmag]{
Returns the magnitude of @racket[v]. Equivalent to @racket[(sqrt (vmag^2 v))].
}

@doc-apply[vnormalize]{
Returns a normal vector in the same direction as @racket[v]. If @racket[v] is a zero vector, returns @racket[v].
@examples[#:eval plot-eval
                 (vnormalize #(1 1 0))
                 (vnormalize #(1 1 1))
                 (vnormalize #(0 0 0.0))]
}

@doc-apply[vcenter]{
Returns the center of the smallest bounding box that contains @racket[vs].
@examples[#:eval plot-eval
                 (vcenter '(#(1 1) #(2 2)))]
}

@doc-apply[vrational?]{
Returns @racket[#t] if every element of @racket[v] is @racket[rational?].
@examples[#:eval plot-eval
                 (vrational? #(1 2))
                 (vrational? #(+inf.0 2))
                 (vrational? #(#f 1))]
}

@;----------------------------------------------------------------------------------------------------
@subsection[#:tag "math.intervals"]{Intervals and Interval Functions}

@defstruct[ivl ([min real?] [max real?])]{
Represents a closed interval.

An interval with two real-valued endpoints always contains the endpoints in order:
@interaction[#:eval plot-eval (ivl 0 1) (ivl 1 0)]

@;{
If either endpoint is @racket[+nan.0], both are, and the interval represents the empty interval:
@interaction[#:eval plot-eval (ivl +nan.0 0) (ivl 0 +nan.0)]
}

An interval can have infinite endpoints:
@interaction[#:eval plot-eval (ivl -inf.0 0) (ivl 0 +inf.0) (ivl -inf.0 +inf.0)]

Functions that return rectangle renderers, such as @racket[rectangles] and @racket[discrete-histogram3d], accept vectors of @racket[ivl]s as arguments.
The @racket[ivl] struct type is also provided by @racketmodname[plot] so users of such renderers do not have to require @racketmodname[plot/utils].
}

@doc-apply[rational-ivl?]{
Returns @racket[#t] if @racket[i] is an interval and each of its endpoints is @racket[rational?].
@examples[#:eval plot-eval
                 (map rational-ivl? (list (ivl -1 1) (ivl -inf.0 2) 'bob))]
}

@;{
@doc-apply[empty-ivl]{
The empty interval.
}

@defproc[(ivl-meet [i ivl?] ...) ivl?]{
Returns the intersection of the given intervals.
@examples[#:eval plot-eval
                 (ivl-meet)
                 (ivl-meet (ivl 0 1) (ivl 2 3))
                 (ivl-meet (ivl 0 2) (ivl 1 3))
                 (ivl-meet empty-ivl (ivl 0 1))]
}

@defproc[(ivl-join [i ivl?] ...) ivl?]{
Returns the smallest interval that contains all the points in the given intervals.
@examples[#:eval plot-eval
                 (ivl-join)
                 (ivl-join (ivl 0 1) (ivl 2 3))
                 (ivl-join (ivl 0 2) (ivl 1 3))
                 (ivl-join empty-ivl (ivl 0 1))]
Think of it as returning an interval union, but with any gaps filled.
}

@doc-apply[ivl-center]{
@examples[#:eval plot-eval
                 (ivl-center (ivl -1 1))
                 (ivl-center empty-ivl)
                 (ivl-center (ivl -inf.0 +inf.0))]
}

@doc-apply[ivl-contains?]{
@examples[#:eval plot-eval
                 (ivl-contains? (ivl -1 1) 0)
                 (ivl-contains? (ivl -1 1) 2)
                 (ivl-contains? (ivl -inf.0 +inf.0) 0)
                 (ivl-contains? empty-ivl 0)]
}

@doc-apply[ivl-empty?]{
@examples[#:eval plot-eval
                 (ivl-empty? empty-ivl)
                 (ivl-empty? (ivl 0 0))]
}

@doc-apply[ivl-length]{
@examples[#:eval plot-eval
                 (ivl-length empty-ivl)
                 (ivl-length (ivl 0 0))
                 (ivl-length (ivl -1 1))
                 (ivl-length (ivl -inf.0 +inf.0))]
}

@doc-apply[ivl-inexact->exact]
@doc-apply[ivl-rational?]
@doc-apply[ivl-singular?]
@doc-apply[ivl-translate]
@doc-apply[ivl-zero-length?]
}

@doc-apply[bounds->intervals]{
Given a list of points, returns intervals between each pair.

Use this to construct inputs for @(racket rectangles) and @(racket rectangles3d).
@examples[#:eval plot-eval (bounds->intervals (linear-seq 0 1 5))]
}

@doc-apply[clamp-real]{
}

@;----------------------------------------------------------------------------------------------------
@;{
@subsection[#:tag "math.rectangles"]{Rectangles and Rectangle Functions}

@margin-note*{The @racket[rect-meet] and @racket[rect-join] functions define a @link["http://en.wikipedia.org/wiki/Lattice_%28order%29"]{pointed lattice} over rectangles.
                  This fact may seem esoteric, but it allows @(plot-name) to combine multiple renderers with different rectangular bounds in a way that is intuitive and mathematically sound.}
@defproc[(rect-meet [i (vectorof ivl?)] ...) (vectorof ivl?)]{
}

@defproc[(rect-join [i (vectorof ivl?)] ...) (vectorof ivl?)]{
}

@doc-apply[empty-rect]
@doc-apply[rect-area]
@doc-apply[rect-center]
@doc-apply[rect-contains?]
@doc-apply[rect-empty?]
@doc-apply[rect-inexact->exact]
@doc-apply[rect-known?]
@doc-apply[rect-rational?]
@doc-apply[rect-singular?]
@doc-apply[rect-translate]
@doc-apply[rect-zero-area?]
@doc-apply[rational-rect?]
@doc-apply[bounding-rect]
}

@;====================================================================================================
@;{
@section{Marching Squares and Cubes}

@doc-apply[heights->cube-polys]
@doc-apply[heights->lines]
@doc-apply[heights->polys]{
winding warning
}
}

@;====================================================================================================
@section{Dates and Times}

@doc-apply[datetime->real]{
Converts various date/time representations into UTC seconds, respecting time zone offsets.

For dates, the value returned is the number of seconds since @italic{a system-dependent UTC epoch}.
See @racket[date-ticks] for more information.

To plot a time series using dates pulled from an SQL database, simply set the relevant axis ticks (probably @racket[plot-x-ticks]) to @racket[date-ticks], and convert the dates to seconds using @racket[datetime->real] before passing them to @racket[lines].
To keep time zone offsets from influencing the plot, set them to @racket[0] first.
}

@defstruct[plot-time ([second (and/c (>=/c 0) (</c 60))]
                      [minute (integer-in 0 59)]
                      [hour (integer-in 0 23)]
                      [day exact-integer?])]{
A time representation that accounts for days, negative times (using negative days), and fractional seconds.

@(plot-name) (specifically @racket[time-ticks]) uses @racket[plot-time] internally to format times, but because renderer-producing functions require only real values,
user code should not need it. It is provided just in case.
}

@doc-apply[plot-time->seconds]
@doc-apply[seconds->plot-time]{
Convert @racket[plot-time]s to real seconds, and vice-versa.
@examples[#:eval plot-eval
                 (define (plot-time+ t1 t2)
                   (seconds->plot-time (+ (plot-time->seconds t1)
                                          (plot-time->seconds t2))))
                 (plot-time+ (plot-time 32 0 12 1)
                             (plot-time 32 0 14 1))]
}
