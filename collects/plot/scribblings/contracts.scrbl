#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot/utils]

@title[#:tag "contracts"]{Plot Contracts}

@section{Plot Element Contracts}

@defproc[(renderer2d? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a 2D @tech{renderer}; that is, if @racket[plot] can plot @racket[value].
See @secref["renderer2d"] for functions that construct them.
}

@defproc[(renderer3d? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a 3D @tech{renderer}; that is, if @racket[plot3d] can plot @racket[value].
See @secref["renderer3d"] for functions that construct them.
}

@defproc[(nonrenderer? [value any/c]) boolean?]{
Returns @racket[#t] if @racket[value] is a @tech{nonrenderer}. See @secref["nonrenderer"] for functions that construct them.
}

@section{Appearance Argument Contracts}

@doc-apply[anchor/c]{
The contract for @(racket anchor) arguments and parameters, such as @(racket plot-legend-anchor).
}

@doc-apply[color/c]{
A contract for very flexible color arguments.
Functions that accept a @racket[color/c] almost always convert it to an RGB triplet using @racket[->color].
}

@doc-apply[plot-color/c]{
The contract for @(racket #:color) arguments, and parameters such as @(racket line-color) and @(racket surface-color).
For the meaning of integer colors, see @(racket ->pen-color) and @(racket ->brush-color).
}

@doc-apply[plot-pen-style/c]{
The contract for @(racket #:style) arguments (when they refer to lines), and paramters such as @(racket line-style).
For the meaning of integer pen styles, see @(racket ->pen-style).
}

@doc-apply[plot-brush-style/c]{
The contract for @(racket #:style) arguments (when they refer to fills), and parameters such as @(racket interval-style).
For the meaning of integer brush styles, see @(racket ->brush-style).
}

@doc-apply[font-family/c]{
Identifies legal font family values. See @(racket plot-font-family).
}

@doc-apply[point-sym/c]{
The contract for the @(racket #:sym) arguments in @(racket points) and @(racket points3d), and the parameter @(racket point-sym).
}

@doc-apply[known-point-symbols]{
A list containing the symbols that are valid @(racket points) symbols.
}

@section{Appearance Argument List Contracts}

@doc-apply[maybe-function/c]{
Returns a contract that accepts either a function from @racket[in-contract] to @racket[out-contract], or a plain @racket[out-contract] value. 

@interaction[#:eval plot-eval
                    (require racket/contract)
                    (define/contract (maybe-function-of-real-consumer x)
                        ((maybe-function/c real? real?) . -> . real?)
                      (maybe-apply x 10))
                    (maybe-function-of-real-consumer 4)
                    (maybe-function-of-real-consumer (λ (x) x))]

Many @racketmodname[plot] functions, such as @racket[contours] and @racket[isosurfaces3d], optionally take lists of appearance values (such as @racket[(listof plot-color/c)]) as arguments.
A very flexible argument contract would accept @italic{functions} that produce lists of appearance values.
For example, @racket[contours] would accept any @racket[f] with contract @racket[((listof real?) . -> . (listof plot-color/c))] for its @racket[#:colors] argument.
When rendering a contour plot, @racket[contours] would apply @racket[f] to a list of the contour @italic{z} values to get the contour colors.

However, most uses do not need this flexibility.
Therefore, @racketmodname[plot]'s functions accept @italic{either} a list of appearance values @italic{or} a function from a list of appropriate values to a list of appearance values.
The @racket[maybe-function/c] function constructs contracts for such arguments.

In @racketmodname[plot] functions, if @racket[in-contract] is a @racket[listof] contract, the output list's length need not be the same as the input list's length.
If it is shorter, the appearance values will cycle; if longer, the tail will not be used.
}

@doc-apply[maybe-apply]{
If @racket[f] is a function, applies @racket[f] to @racket[args]; otherwise returns @racket[f].

This is used inside many renderer-producing @racket[plot] functions to convert @racket[maybe-function/c] values to lists of appearance values.
}

@doc-apply[plot-colors/c]{
Returns a contract for @(racket #:colors) arguments, as in @(racket contours) and @racket[contour-intervals].
See @racket[maybe-function/c] for a discussion of the returned contract.

The following example sends a @italic{list}-valued @racket[(plot-colors/c ivl?)] to @racket[contour-intervals], which then cycles through the colors:
@interaction[#:eval plot-eval
                    (plot (contour-intervals (λ (x y) (+ x y)) 0 1 0 1
                                             #:colors '(1 2)))]
This is equivalent to sending @racket[(λ _ '(1 2))].

The next example is more sophisticated: it sends a @italic{function}-valued @racket[(plot-colors/c ivl?)] to @racket[contour-intervals].
The function constructs colors from the values of the contour intervals.
@interaction[#:eval plot-eval
                    (define (brown-interval-colors ivls)
                      (define z-size (- (ivl-max (last ivls))
                                        (ivl-min (first ivls))))
                      (for/list ([i  (in-list ivls)])
                        (match-define (ivl z-min z-max) i)
                        (define z-mid (/ (* 1/2 (+ z-min z-max)) z-size))
                        (list (* 255 z-mid) (* 128 z-mid) (* 64 z-mid))))
                    
                    (plot (contour-intervals (λ (x y) (+ x y)) 0 1 0 1
                                             #:colors brown-interval-colors))]
}

@doc-apply[plot-pen-styles/c]{
Like @(racket plot-colors/c), but for line styles.
}

@doc-apply[pen-widths/c]{
Like @(racket plot-colors/c), but for line widths.
}

@doc-apply[plot-brush-styles/c]{
Like @(racket plot-colors/c), but for fill styles.
}

@doc-apply[alphas/c]{
Like @(racket plot-colors/c), but for opacities.
}

@doc-apply[labels/c]{
Like @racket[plot-colors/c], but for strings.
This is used, for example, to label @racket[stacked-histogram]s.
}
