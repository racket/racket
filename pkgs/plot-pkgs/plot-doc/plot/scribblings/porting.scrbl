#lang scribble/manual

@(require "common.rkt")

@title[#:tag "porting"]{Porting From @(plot-name) <= 5.1.3}

If it seems porting will take too long, you can get your old code running more quickly using the @secref["compat"].

The update from @(plot-name) version 5.1.3 to 5.2 introduces a few incompatibilities:
@itemlist[
          @item{@(plot-name) now allows plot elements to request plot area bounds, and finds bounds large enough to fit all plot elements.
                The old default plot area bounds of [-5,5] × [-5,5] cannot be made consistent with the improved behavior; the default bounds are now "no bounds".
                This causes code such as @(racket (plot (line sin))), which does not state bounds, to fail.}
          @item{The @(racket #:width) and @(racket #:style) keyword arguments to @(racket vector-field) have been replaced by @(racket #:line-width) and @(racket #:scale) to be consistent with other functions.}
          @item{The @(racket plot) function no longer takes a @(racket ((is-a?/c 2d-view%) . -> . void?)) as an argument, but a @(racket (treeof renderer2d?)).
                The argument change in @(racket plot3d) is similar.
                This should not affect most code because @(plot-name) encourages regarding these data types as black boxes.}
          @item{The @(racket plot-extend) module no longer exists.}
          @item{The @racket[fit] function and @racket[fit-result] struct type have been removed.}

          ]

This section of the @(plot-name) manual will help you port code written for @(plot-name) 5.1.3 and earlier to the most recent @(plot-name).
There are four main tasks:
@itemlist[
          @item{Replace deprecated functions.}
          @item{Ensure that plots have bounds.}
          @item{Change @(racket vector-field), @(racket plot) and @(racket plot3d) keyword arguments.}
          @item{Fix broken calls to @(racket points).}
          ]

You should also set @(racket (plot-deprecation-warnings? #t)) to be alerted to uses of deprecated features.


@section{Replacing Deprecated Functions}

Replace @(racket mix) with @(racket list), and replace @(racket surface) with @(racket surface3d). These functions are drop-in replacements, but @(racket surface3d) has many more features (and a name more consistent with similar functions).

Replace @(racket line) with @(racket function), @(racket parametric) or @(racket polar), depending on the keyword arguments to @(racket line). These are not at all drop-in replacements, but finding the right arguments should be straightforward.

Replace @(racket contour) with @(racket contours), and replace @(racket shade) with @(racket contour-intervals).
These are @italic{mostly} drop-in replacements: they should always work, but may not place contours at the same values (unless the levels are given as a list of values).
For example, the default @(racket #:levels) argument is now @(racket 'auto), which chooses contour values in the same way that @italic{z} axis tick locations are usually chosen in 3D plots.
The number of contour levels is therefore some number between @(racket 4) and @(racket 10), depending on the plot.


@section{Ensuring That Plots Have Bounds}

The safest way to ensure that @(racket plot) can determine bounds for the plot area is to add @(racket #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5) to every call to @(racket plot). Similarly, add @(racket #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5 #:z-min -5 #:z-max 5) to every call to @(racket plot3d).

Because @(plot-name) is now smarter about choosing bounds, there are better ways. For example, suppose you have

@interaction[#:eval plot-eval
                    (eval:alts
                     (plot (line sin))
                     (eval:result "" "" "plot: could not determine sensible plot bounds; got x ∈ [#f,#f], y ∈ [#f,#f]"))]

You could either change it to

@interaction[#:eval plot-eval (plot (function sin) #:x-min -5 #:x-max 5 #:y-min -5 #:y-max 5)]

or change it to

@interaction[#:eval plot-eval (plot (function sin -5 5))]

When @(racket function) is given @italic{x} bounds, it determines tight @italic{y} bounds.


@section{Changing Keyword Arguments}

Replace every @(racket #:width) in a call to @(racket vector-field) with @(racket #:line-width).

Replace every @(racket #:style 'scaled) with @(racket #:scale 'auto) (or because it is the default in both the old and new, take it out).

Replace every @(racket #:style 'real) with @(racket #:scale 1.0).

Replace every @(racket #:style 'normalized) with @(racket #:scale 'normalized).

The @(racket plot) and @(racket plot3d) functions still accept @(racket #:bgcolor), @(racket #:fgcolor) and @(racket #:lncolor), but these are deprecated.
Parameterize on @(racket plot-background) and @(racket plot-foreground) instead.

For example, if you have @(racket (plot (function sin -5 5) #:fgcolor '(0 0 128) #:bgcolor '(224 224 224))), change it to
@interaction[#:eval plot-eval (parameterize ([plot-foreground  '(0 0 128)]
                                             [plot-background  '(224 224 224)])
                                (plot (function sin -5 5)))]

The @(racket #:lncolor) keyword argument now does nothing; change the renderer instead.
For example, if you have @(racket (plot (function sin -5 5) #:lncolor '(0 0 128))), change it to
@interaction[#:eval plot-eval (plot (function sin -5 5 #:color '(0 0 128)))]

Change @(racket #:az) in calls to @(racket plot3d) to @(racket #:angle), and @(racket #:alt) to @(racket #:altitude).
Alternatively, parameterize multiple plots by setting the @(racket plot3d-angle) and @(racket plot3d-altitude) parameters.


@section{Fixing Broken Calls to @(racket points)}

The @(racket points) function used to be documented as accepting a @(racket (listof (vector/c real? real?))), but actually accepted a @(racket (listof (vectorof real?))) and silently ignored any extra vector elements.

If you have code that takes advantage of this, strip down the vectors first.
For example, if @(racket vs) is the list of vectors, send @(racket (map (λ (v) (vector-take v 2)) vs)) to @(racket points).


@section{Replacing Uses of @(racket plot-extend)}

Chances are, if you used @(racket plot-extend), you no longer need it.
The canonical @(racket plot-extend) example used to be a version of @(racket line) that drew dashed lines.
Every line-drawing function in @(plot-name) now has a @(racket #:style) or @(racket #:line-style) keyword argument.

The rewritten @(plot-name) will eventually have a similar extension mechanism.


@section{Deprecated Functions}

@declare-exporting[plot]
@defmodule*/no-declare[(plot) #:link-target? #f]

The following functions exist for backward compatibility, but may be removed in the future.
Set @(racket (plot-deprecation-warnings? #t)) to be alerted the first time each is used.

@defproc[(mix [plot-data (any/c . -> . void?)] ...)
         (any/c . -> . void?)]{
See @(secref "compat") for the original documentation. Replace this with @(racket list).
}

@doc-apply[line]{
See @(secref "compat") for the original documentation. Replace this with @(racket function), @(racket parametric) or @(racket polar), depending on keyword arguments.
}

@doc-apply[contour]{
See @(secref "compat") for the original documentation. Replace this with @(racket contours).
}

@doc-apply[shade]{
See @(secref "compat") for the original documentation. Replace this with @(racket contour-intervals).
}

@doc-apply[surface]{
See @(secref "compat") for the original documentation. Replace this with @(racket surface3d).
}
