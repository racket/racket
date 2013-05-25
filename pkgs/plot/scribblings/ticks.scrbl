#lang scribble/manual

@(require "common.rkt" (for-label racket/date db))

@declare-exporting[plot]

@title[#:tag "ticks and transforms"]{Axis Transforms and Ticks}

@section[#:tag "transforms"]{Axis Transforms}

The @italic{x}, @italic{y} and @italic{z} axes for any plot can be independently transformed by parameterizing the plot on different @racket[plot-x-transform], @racket[plot-y-transform] and @racket[plot-z-transform] values.
For example, to plot the @italic{x} axis with a log transform:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform])
                      (plot (function sin 1 100)))]
Most @racket[log-transform]ed plots use different ticks than the default, uniformly spaced ticks, however.
To put log ticks on the @italic{x} axis, set the @racket[plot-x-ticks] parameter:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform]
                                   [plot-x-ticks      (log-ticks)])
                      (plot (function sin 1 100)))]
See @secref["ticks"] for more details on parameterizing a plot's axis ticks.

@margin-note*{
To sample nonlinearly, the @italic{inverse} of a transform is applied to linearly sampled points. See @racket[make-axis-transform] and @racket[nonlinear-seq].}
Renderers cooperate with the current transforms by sampling nonlinearly. For example,
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform])
                      (plot3d (surface3d + 0.01 1 0.01 1)))]
Notice that the surface is sampled uniformly in appearance even though the @italic{x}-axis ticks are not spaced uniformly.

Transforms are applied to the primitive shapes that comprise a plot:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  log-transform])
                      (plot3d (surface3d + 0.01 1 0.01 1 #:samples 3)))]
Here, the renderer returned by @racket[surface3d] does not have to bend the polygons it draws; @racket[plot3d] does this automatically (by recursive subdivision).

@doc-apply[plot-x-transform]
@doc-apply[plot-y-transform]
@doc-apply[plot-z-transform]{
Independent, per-axis, monotone, nonlinear transforms. PLoT comes with some typical (and some atypical) axis transforms, documented immediately below.
}

@doc-apply[id-transform]{
The identity axis transform, the default transform for all axes.
}

@doc-apply[log-transform]{
A log transform. Use this to generate plots with log-scale axes. Any such axis must have positive bounds.

The beginning of the @secref["ticks and transforms"] section has a working example. An example of exceeding the bounds is
@interaction[#:eval plot-eval
                    (eval:alts
                     (parameterize ([plot-x-transform  log-transform])
                       (plot (function (λ (x) x) -1 1)))
                     (eval:result "" "" "log-transform: expects type <positive real> as 1st argument, given: -1; other arguments were: 1"))]
See @racket[axis-transform-bound] and @racket[axis-transform-append] for ways to get around an axis transform's bounds limitations.
}

@doc-apply[stretch-transform]{
Returns an axis transform that stretches a finite interval.

The following example uses a @racket[stretch-transform] to draw attention to the interval [-1,1] in an illustration of the limit of @italic{sin(x)/x} as @italic{x} approaches zero (a critical part of proving the derivative of @italic{sin(x)}):
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (stretch-transform -1 1 20)]
                                   [plot-x-ticks  (ticks-add (plot-x-ticks) '(-1 1))])
                      (plot (list (y-axis -1 #:ticks? #f) (y-axis 1 #:ticks? #f)
                                  (function (λ (x) (/ (sin x) x)) -14 14
                                            #:width 2 #:color 4 #:label "y = sin(x)/x")
                                  (point-label (vector 0 1) "y → 1 as x → 0"
                                               #:anchor 'bottom-right))
                            #:y-max 1.2))]
}

@doc-apply[collapse-transform]{
Returns an axis transform that collapses a finite interval to its midpoint.
For example, to remove part of the long, boring asymptotic approach of @italic{atan(x)} toward π/2:
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (collapse-transform 50 150)])
                      (plot (function atan 10 200 #:label "y = atan(x)")
                            #:legend-anchor 'center))]
In this case, there were already ticks at the collapsed interval's endpoints.
If there had not been, it would have been necessary to use @racket[ticks-add] to let viewers know precisely the interval that was collapsed.
(See @racket[stretch-transform] for an example.)
}

@doc-apply[cbrt-transform]{
A ``cube-root'' transform, mostly used for testing.
Unlike the log transform, it is defined on the entire real line, making it better for testing the appearance of plots with nonlinearly transformed axes.
}

@doc-apply[hand-drawn-transform]{
An @italic{extremely important} test case, which makes sure that @(plot-name) can use any monotone, invertible function as an axis transform.
The @(racket freq) parameter controls the ``shakiness'' of the transform. At high values, it makes plots look like Peanuts cartoons.
@examples[#:eval plot-eval
                 (parameterize ([plot-x-transform  (hand-drawn-transform 200)]
                                [plot-y-transform  (hand-drawn-transform 200)])
                   (plot (function sqr -1 1)))
                 (parameterize ([plot-x-transform  (hand-drawn-transform 50)]
                                [plot-y-transform  (hand-drawn-transform 50)]
                                [plot-z-transform  (hand-drawn-transform 50)])
                   (plot3d (contour-intervals3d (λ (x y) (- (sqr x) (sqr y)))
                                                -1 1 -1 1 #:samples 9)))]
}

@doc-apply[axis-transform/c]{
The contract for axis transforms.

The easiest ways to construct novel axis transforms are to use the axis transform combinators @racket[axis-transform-append], @racket[axis-transform-bound] and @racket[axis-transform-compose], or to apply @racket[make-axis-transform] to an @racket[invertible-function].
}

@doc-apply[axis-transform-append]{
Returns an axis transform that transforms values less than @racket[mid] like @racket[t1], and transforms values greater than @racket[mid] like @racket[t2].
(Whether it transforms @racket[mid] like @racket[t1] or @racket[t2] is immaterial, as a transformed @racket[mid] is equal to @racket[mid] either way.)
@examples[#:eval plot-eval
                 (parameterize ([plot-x-transform  (axis-transform-append
                                                    (stretch-transform -2 -1 10)
                                                    (stretch-transform 1 2 10)
                                                    0)])
                   (plot (function (λ (x) x) -3 3)))]
}

@doc-apply[axis-transform-bound]{
Returns an axis transform that transforms values like @racket[t] does in the interval [@racket[a],@racket[b]], but like the identity transform outside of it.
For example, to bound @racket[log-transform] to an interval in which it is well-defined,
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (axis-transform-bound
                                                       log-transform 0.01 +inf.0)])
                      (plot (function (λ (x) x) -4 8 #:label "y = x")))]
}

@doc-apply[axis-transform-compose]{
Composes two axis transforms.
For example, to collapse part of a @racket[log-transform]ed axis, try something like
@interaction[#:eval plot-eval
                    (parameterize ([plot-x-transform  (axis-transform-compose
                                                       log-transform
                                                       (collapse-transform 2 4))])
                      (plot (function (λ (x) x) 1 5)))]
Argument order matters, but predicting the effects of exchanging arguments can be difficult.
Fortunately, the effects are usually slight.
}

@doc-apply[make-axis-transform]{
Given a monotone @racket[invertible-function], returns an axis transform.
Monotonicity is necessary, but cannot be enforced.
The inverse is used to take samples uniformly along transformed axes (see @racket[nonlinear-seq]).

@examples[#:eval plot-eval
                 (parameterize ([plot-y-transform  (make-axis-transform
                                                    (invertible-function sqrt sqr))])
                   (plot (function (λ (x) x) 0 5)))]

An axis transform created by @racket[make-axis-transform] (or by any of the above combinators) does not transform the endpoints of an axis's bounds, to within floating-point error.
For example,
@interaction[#:eval plot-eval
                    (match-let ([(invertible-function f g)
                                 (apply-axis-transform log-transform 1 3)])
                      (define xs '(1 2 3))
                      (define new-xs (map f xs))
                      (define old-xs (map g new-xs))
                      (values new-xs old-xs))]

Technically, @racket[fun] does not need to be truly invertible.
Given @racket[fun] = @racket[(invertible-function f g)], it is enough for @racket[f] to be a @hyperlink["http://en.wikipedia.org/wiki/Inverse_function#Left_and_right_inverses"]{left inverse} of @racket[g];
that is, always @racket[(f (g x)) = x] but not necessarily @racket[(g (f x)) = x].
If @racket[f] and @racket[g] had to be strict inverses of each other, there could be no @racket[collapse-transform].
}

@doc-apply[apply-axis-transform]{
Returns an invertible function that transforms axis points within the given axis bounds.
This convenience function is used internally to transform points before rendering, but is provided for completeness.
}

@section[#:tag "ticks"]{Axis Ticks}

Each plot axis has two indepedent sets of ticks: the @italic{near} ticks and the @italic{far} ticks.

@doc-apply[plot-x-ticks]
@doc-apply[plot-x-far-ticks]
@doc-apply[plot-y-ticks]
@doc-apply[plot-y-far-ticks]
@doc-apply[plot-z-ticks]
@doc-apply[plot-z-far-ticks]{
@examples[#:eval plot-eval
                 (parameterize ([plot-x-label      "Near x axis"]
                                [plot-y-label      "Near y axis"]
                                [plot-z-label      "Near z axis"]
                                [plot-x-ticks      (date-ticks)]
                                [plot-y-ticks      (time-ticks)]
                                [plot-z-ticks      (fraction-ticks)]
                                [plot-x-far-label  "Far x axis"]
                                [plot-y-far-label  "Far y axis"]
                                [plot-z-far-label  "Far z axis"]
                                [plot-x-far-ticks  (linear-ticks)]
                                [plot-y-far-ticks  (currency-ticks)]
                                [plot-z-far-ticks  (log-ticks #:base 2)])
                   (plot3d (lines3d '(#(1 1 1) #(40000000 4 4)) #:style 'transparent)
                           #:angle 45 #:altitude 50
                           #:title "Axis Names and Tick Locations"))]
At any @racket[#:angle], the far @italic{x} and @italic{y} ticks are behind the plot, and the far @italic{z} ticks are on the right.
Far ticks are drawn, but not labeled, if they are identical to their corresponding near ticks.
They are always identical by default.

@deftech{Major ticks} are longer than @deftech{minor ticks}. Major tick labels are always drawn unless collapsed with a nearby tick.
Minor tick labels are never drawn.

Renderers produced  by @racket[contours] and @racket[contour-intervals] use the value of @racket[plot-z-ticks] to place and label contour lines.
For example, compare plots of the same function renderered using both @racket[contour-intervals] and @racket[contour-intervals3d]:
@interaction[#:eval plot-eval
                    (parameterize ([plot-z-ticks  (currency-ticks)])
                      (define (saddle x y) (- (sqr x) (sqr y)))
                      (values
                       (plot (contour-intervals saddle -1 1 -1 1 #:label "z")
                             #:legend-anchor 'center)
                       (plot3d (contour-intervals3d saddle -1 1 -1 1 #:label "z")
                               #:legend-anchor 'center)))]
}

@doc-apply[contour-ticks]{
Returns the ticks used for contour values.
This is used internally by renderers returned from @racket[contours], @racket[contour-intervals], @racket[contours3d], @racket[contour-intervals3d], and @racket[isosurfaces3d], but is provided for completeness.

When @racket[levels] is @racket['auto], the returned values do not correspond @italic{exactly} with the values of ticks returned by @racket[z-ticks]: they might be missing the endpoint values. For example,
@interaction[#:eval plot-eval
                 (map pre-tick-value 
                      (filter pre-tick-major? ((plot-z-ticks) 0 1)))
                 (map pre-tick-value
                      (contour-ticks (plot-z-ticks) 0 1 'auto #f))]
}

@doc-apply[plot-d-ticks]{
The ticks used for default isosurface values in @racket[isosurfaces3d].
}

@doc-apply[plot-r-ticks]{
The ticks used for radius lines in @racket[polar-axes].
}

@defstruct[ticks ([layout ticks-layout/c] [format ticks-format/c])]{
A @racket[ticks] for a near or far axis consists of a @racket[layout] function, which determines the number of ticks and where they will be placed, and a @racket[format] function, which determines the ticks' labels.
}

@doc-apply[ticks-default-number]{
Most tick layout functions (and thus their corresponding @racket[ticks]-constructing functions) have a @racket[#:number] keyword argument with default @racket[(ticks-default-number)].
What the number means depends on the tick layout function.
Most use it for an average number of major ticks.

It is unlikely to mean the exact number of major ticks.
Without adjusting the number of ticks, layout functions usually cannot find uniformly spaced ticks that will have simple labels after formatting.
For example, the following plot shows the actual number of major ticks for the interval [0,@italic{x}] when the requested number of ticks is 8, as generated by @racket[linear-ticks-layout]:
@interaction[#:eval plot-eval
                    (plot (function (λ (x)
                                      (count pre-tick-major?
                                             ((linear-ticks-layout #:number 8) 0 x)))
                                    0.1 10)
                          #:x-label "Interval [0,x]" #:y-label "Number of ticks")]
}

@subsection{Linear Ticks}

@doc-apply[linear-ticks-layout]
@doc-apply[linear-ticks-format]
@doc-apply[linear-ticks]{
The layout function, format function, and combined @racket[ticks] for uniformly spaced ticks.

To lay out ticks, @racket[linear-ticks-layout] finds the power of @racket[base] closest to the axis interval size, chooses a simple first tick, and then chooses a skip length using @racket[divisors] that maximizes the number of ticks without exceeding @racket[number].
@margin-note*{For strategic use of non-default arguments, see @racket[bit/byte-ticks], @racket[currency-ticks], and @racket[fraction-ticks].}
The default arguments correspond to the standard 1-2-5-in-base-10 rule used almost everywhere in plot tick layout.

To format ticks, @racket[linear-ticks-format] uses @racket[real->plot-label], and uses @racket[digits-for-range] to determine the maximum number of fractional digits in the decimal expansion.
}

@subsection{Log Ticks}

@doc-apply[log-ticks-layout]
@doc-apply[log-ticks-format]
@doc-apply[log-ticks]{
The layout function, format function, and combined @racket[ticks] for exponentially spaced major ticks.
(The minor ticks between are uniformly spaced.)
Use these ticks for @racket[log-transform]ed axes, because when exponentially spaced tick positions are @racket[log-transform]ed, they become uniformly spaced.

The @racket[#:base] keyword argument is the logarithm base.
See @racket[plot-z-far-ticks] for an example of use.
}

@subsection{Date Ticks}

@doc-apply[date-ticks-layout]
@doc-apply[date-ticks-format]
@doc-apply[date-ticks]{
The layout function, format function, and combined @racket[ticks] for uniformly spaced ticks with date labels.

These axis ticks regard values as being in seconds since @italic{a system-dependent Universal Coordinated Time (UTC) epoch}.
(For example, the Unix and Mac OS X epoch is January 1, 1970 UTC, and the Windows epoch is January 1, 1601 UTC.)
Use @racket[date->seconds] to convert local dates to seconds, or @racket[datetime->real] to convert dates to UTC seconds in a way that accounts for time zone offsets.

Actually, @racket[date-ticks-layout] does not always space ticks @italic{quite} uniformly.
For example, it rounds ticks that are spaced about one month apart or more to the nearest month.
Generally, @racket[date-ticks-layout] tries to place ticks at minute, hour, day, week, month and year boundaries, as well as common multiples such as 90 days or 6 months.

To try to avoid displaying overlapping labels, @racket[date-ticks-format] chooses date formats from @racket[formats] for which labels will contain no redundant information.

All the format specifiers given in @racketmodname[srfi/19] (which are derived from Unix's @tt{date} command), except those that represent time zones, are allowed in date format strings.
}

@doc-apply[date-ticks-formats]{
The default date formats.
}

@doc-apply[24h-descending-date-ticks-formats]
@doc-apply[12h-descending-date-ticks-formats]

@subsection{Time Ticks}

@doc-apply[time-ticks-layout]
@doc-apply[time-ticks-format]
@doc-apply[time-ticks]{
The layout function, format function, and combined @racket[ticks] for uniformly spaced ticks with time labels.

These axis ticks regard values as being in seconds.
Use @racket[datetime->real] to convert @racket[sql-time] or @racket[plot-time] values to seconds.

Generally, @racket[time-ticks-layout] tries to place ticks at minute, hour and day boundaries, as well as common multiples such as 12 hours or 30 days.

To try to avoid displaying overlapping labels, @racket[time-ticks-format] chooses a date format from @racket[formats] for which labels will contain no redundant information.

All the time-related format specifiers given in @racketmodname[srfi/19] (which are derived from Unix's @tt{date} command) are allowed in time format strings.
}

@doc-apply[time-ticks-formats]{
The default time formats.
}

@doc-apply[24h-descending-time-ticks-formats]
@doc-apply[12h-descending-time-ticks-formats]

@subsection{Currency Ticks}

@doc-apply[currency-ticks-format]
@doc-apply[currency-ticks]{
The format function and combined @racket[ticks] for uniformly spaced ticks with currency labels.

The @racket[#:kind] keyword argument is either a string containing the currency symbol, or a currency code such as @racket['USD], @racket['GBP] or @racket['EUR].
The @racket[currency-ticks-format] function can map most ISO 4217 currency codes to their corresponding currency symbol.

The @racket[#:scales] keyword argument is a list of suffixes for each 10@superscript{3} scale, such as @racket["K"] (US thousand, or kilo), @racket["bn"] (UK short-scale billion) or @racket["Md"] (EU long-scale milliard). Off-scale amounts are given power-of-ten suffixes such as ``×10@superscript{21}.''

The @racket[#:formats] keyword argument is a list of three format strings, representing the formats of positive, negative, and zero amounts, respectively. The format specifiers are:
@itemlist[@item{@racket["~$"]: replaced by the currency symbol}
          @item{@racket["~w"]: replaced by the whole part of the amount}
          @item{@racket["~f"]: replaced by the fractional part, with 2 or more decimal digits}
          @item{@racket["~s"]: replaced by the scale suffix}
          @item{@racket["~~"]: replaced by ``~''}]
}

@doc-apply[currency-ticks-scales]
@doc-apply[currency-ticks-formats]{
The default currency scales and formats.

For example, a PLoT user in France would probably begin programs with
@racketblock[(require plot)
             (currency-ticks-scales eu-currency-scales)
             (currency-ticks-formats eu-currency-formats)]
and use @racket[(currency-ticks #:kind 'EUR)] for local currency or @racket[(currency-ticks #:kind 'JPY)] for Japanese Yen.

Cultural sensitivity notwithstanding, when writing for a local audience, it is generally considered proper to use local currency scales and formats for foreign currencies, but use the foreign currency symbol.
}

@doc-apply[us-currency-scales]{
Short-scale suffix abbreviations as commonly used in the United States, Canada, and some other English-speaking countries. These stand for ``kilo,'' ``million,'' ``billion,'' and ``trillion.''
}

@doc-apply[uk-currency-scales]{
Short-scale suffix abbreviations as commonly used in the United Kingdom since switching to the short scale in 1974, and as currently recommended by the Daily Telegraph and Times style guides.
}

@doc-apply[eu-currency-scales]{
European Union long-scale suffix abbreviations, which stand for ``kilo,'' ``million,'' ``milliard,'' and ``billion.''

The abbreviations actually used vary with geography, even within countries, but these seem to be common.
Further long-scale suffix abbreviations such as for ``billiard'' are ommitted due to lack of even weak consensus.
}

@doc-apply[us-currency-formats]{
Common currency formats used in the United States.
}

@doc-apply[uk-currency-formats]{
Common currency formats used in the United Kingdom.
Note that it sensibly uses a negative sign to denote negative amounts.
}

@doc-apply[eu-currency-formats]{
A guess at common currency formats for the European Union.
Like scale suffixes, actual formats vary with geography, but currency formats can even vary with audience or tone.
}

@subsection{Other Ticks}

@doc-apply[no-ticks-layout]
@doc-apply[no-ticks-format]
@doc-apply[no-ticks]{
The layout function, format function, and combined @racket[ticks] for no ticks whatsoever.
@examples[#:eval plot-eval
                 (parameterize ([plot-x-ticks  no-ticks]
                                [plot-y-ticks  no-ticks]
                                [plot-x-label  #f]
                                [plot-y-label  #f])
                   (plot (list (polar-axes) (polar (λ (θ) 1/3)))))]
}

@doc-apply[bit/byte-ticks-format]
@doc-apply[bit/byte-ticks]{
The format function and and combined @racket[ticks] for bit or byte values.

The @racket[#:kind] keyword argument indicates either International System of Units (@racket['SI]) suffixes, as used to communicate hard drive capacities, or Computer Science (@racket['CS]) suffixes, as used to communicate memory capacities.
}

@doc-apply[fraction-ticks-format]
@doc-apply[fraction-ticks]{
The format function and and combined @racket[ticks] for fraction-formatted values.
}

@subsection{Tick Combinators}

@doc-apply[ticks-mimic]{
Returns a @racket[ticks] that mimics the given @racket[ticks] returned by @racket[thunk].
Used in default values for @racket[plot-x-far-ticks], @racket[plot-y-far-ticks] and @racket[plot-z-far-ticks] to ensure that, unless one of these parameters is changed, the far tick labels are not drawn.
}

@doc-apply[ticks-add]{
Returns a new @racket[ticks] that acts like @racket[t], except that it puts additional ticks at positions @racket[xs]. If @racket[major?] is true, the ticks at positions @racket[xs] are all @tech{major ticks}; otherwise, they are minor ticks.
}

@doc-apply[ticks-scale]{
Returns a new @racket[ticks] that acts like @racket[t], but for an axis transformed by @racket[fun].
Unlike with typical @secref["transforms"], @racket[fun] is allowed to transform axis endpoints.
(See @racket[make-axis-transform] for an explanation about transforming endpoints.)

Use @racket[ticks-scale] to plot values at multiple scales simultaneously, with one scale on the near axis and one scale on the far axis.
The following example plots degrees Celsius on the left and degrees Farenheit on the right:
@interaction[#:eval plot-eval
                    (parameterize
                        ([plot-x-ticks      (time-ticks)]
                         [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                         (linear-scale 9/5 32))]
                         [plot-y-label      "Temperature (\u00b0C)"]
                         [plot-y-far-label  "Temperature (\u00b0F)"])
                      (define data
                        (list #(0 0) #(15 0.6) #(30 9.5) #(45 10.0) #(60 16.6)
                              #(75 41.6) #(90 42.7) #(105 65.5) #(120 78.9)
                              #(135 78.9) #(150 131.1) #(165 151.1) #(180 176.2)))
                      (plot (list
                             (function (λ (x) (/ (sqr x) 180)) 0 180
                                       #:style 'long-dash #:color 3 #:label "Trend")
                             (lines data #:color 2 #:width 2)
                             (points data #:color 1 #:line-width 2 #:label "Measured"))
                            #:y-min -25 #:x-label "Time"))]
}

@subsection{Tick Data Types and Contracts}

@defstruct[pre-tick ([value real?] [major? boolean?])]{
Represents a tick that has not yet been labeled.
}
@defstruct[(tick pre-tick) ([label string?])]{
Represents a tick with a label.
}

@doc-apply[ticks-layout/c]{
The contract for tick layout functions. Note that a layout function returns @racket[pre-tick]s, or unlabeled ticks.
}

@doc-apply[ticks-format/c]{
The contract for tick format functions. A format function receives axis bounds so it can determine how many decimal digits to display (usually by applying @racket[digits-for-range] to the bounds).
}

@section[#:tag "invertible"]{Invertible Functions}

@defstruct[invertible-function ([f (real? . -> . real?)] [g (real? . -> . real?)])]{
Represents an invertible function. Used for @secref["transforms"] and by @racket[ticks-scale].

The function itself is @racket[f], and its inverse is @racket[g].
Because @racket[real?]s can be inexact, this invariant must be approximate and therefore cannot be enforced.
(For example, @racket[(exp (log 10))] = @racket[10.000000000000002].)
The obligation to maintain it rests on whomever constructs one.
}

@doc-apply[id-function]{
The identity function as an @racket[invertible-function].
}

@doc-apply[invertible-compose]{
Returns the composition of two invertible functions.
}

@doc-apply[invertible-inverse]{
Returns the inverse of an invertible function.
}

@doc-apply[linear-scale]{
Returns a one-dimensional linear scaling function, as an @racket[invertible-function].
This function constructs the most common arguments to @racket[ticks-scale].
}
