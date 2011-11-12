#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot/utils]

@title[#:tag "contracts"]{Plot Contracts}

@section{Convenience Contracts}

@doc-apply[contract/c]{
Identifies @racket[contract?]s and predicates that can be used as contracts.
}

@doc-apply[treeof]{
Identifies trees of values that meet the contract @(racket ct).
Used by @(racket plot) and @(racket plot3d) to construct the contract for a tree of @(racket renderer2d?) or @(racket renderer3d?).
}

@section{Appearance Argument Contracts}

@doc-apply[anchor/c]{
The contract for @(racket anchor) arguments and parameters, such as @(racket plot-legend-anchor).
}

@doc-apply[color/c]
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

@section{Appearance Argument Sequence Contracts}

@doc-apply[maybe-function/c]{
}

@doc-apply[plot-colors/c]{
The contract for @(racket #:colors) arguments, as in @(racket contours).
If the contracted value is a function, it is intended to take a list of values, such as contour values, as input, and return a list of colors.
The number of colors need not be the same as the number of values.
If shorter, they will cycle; if longer, some will not be used.

See @(racket color-seq) and @(racket color-seq*) for a demonstration of constructing arguments with this contract.
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
