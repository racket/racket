#lang scribble/manual

@(require "common.rkt")

@title[#:tag "utils"]{Plot Utilities}

@defmodule[plot/utils]

@doc-apply[bounds->intervals]{
TODO
}

@doc-apply[degrees->radians]{
Converts degrees to radians.
}

@doc-apply[radians->degrees]{
Converts radians to degrees.
}

@doc-apply[real->string/trunc]{
Like @(racket real->decimal-string), but removes trailing zeros and a trailing decimal point.
Used to format numbers for plots.
}

@doc-apply[digits-for-range]{
Given a range, returns the number of decimal places necessary to distinguish numbers in the range.
}

@doc-apply[->plot-label]{
Converts a Racket value into a label. Used by @(racket discrete-histogram) and @(racket discrete-histogram3d).
}

@doc-apply[linear-seq]{
TODO
}

@doc-apply[linear-seq*]{
TODO
}

@doc-apply[color-seq]{
TODO
}

@doc-apply[color-seq*]{
TODO
}

@doc-apply[->color]{
Converts a color into an RGB triplet.
}

@doc-apply[->pen-color]{
Converts a @italic{pen} color into an RGB triplet. This interprets numbers as darker and more saturated than @(racket ->brush-color) does.
}

@doc-apply[->brush-color]{
Converts a @italic{brush} color into an RGB triplet. This interprets numbers as lighter and less saturated than @(racket ->brush-color) does.
}

@doc-apply[->pen-style]{
Converts a symbolic pen style or a number into a symbolic pen style.
}

@doc-apply[->brush-style]{
Converts a symbolic brush style or a number into a symbolic brush style.
}

@defstruct[invertible-function ([f (real? . -> . real?)] [finv (real? . -> . real?)])]{
Axis transforms return these, given axis bounds. See @(racket plot-x-transform).
}

@defstruct[mapped-function ([f (real? . -> . real?)] [fmap ((listof real?) . -> . (listof real?))])]{
}

@defstruct[mapped-function/bounds ([f (real? . -> . real?)] [fmap ((listof real?) . -> . (listof real?))])]{
}

@doc-apply[make-kde]{
}
