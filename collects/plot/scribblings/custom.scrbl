#lang scribble/manual

@(require "common.rkt")

@title[#:tag "custom"]{Custom Plot Elements}

@declare-exporting[plot/utils]

@section{Plot Elements}

@defstruct[plot-element ([bounds-rect  (or/c (vectorof ivl?) #f)]
                         [bounds-fun   (or/c bounds-fun/c #f)]
                         [ticks-fun    (or/c ticks-fun/c #f)])]{
}

@defstruct[(non-renderer plot-element) ()]{
Examples: @racket[x-ticks], @racket[y-ticks], @racket[z-ticks], @racket[invisible-rect] and @racket[invisible-rect3d]
}

@subsection{Plot Element Bounds}
                                                               
@doc-apply[bounds-fun/c]{
}

@doc-apply[function-bounds-fun]
@doc-apply[inverse-bounds-fun]
@doc-apply[function-interval-bounds-fun]
@doc-apply[inverse-interval-bounds-fun]
@doc-apply[surface3d-bounds-fun]

@subsection{Plot Element Ticks}

@doc-apply[ticks-fun/c]{
}

@doc-apply[default-ticks-fun]{
}

@section{2D Renderers}

@defstruct[(renderer2d plot-element) ([render-proc  (or/c ((is-a?/c 2d-plot-area%)
                                                           . -> . (treeof legend-entry?))
                                                          #f)])]{
}

@section{3D Renderers}

@defstruct[(renderer3d plot-element) ([render-proc  (or/c ((is-a?/c 3d-plot-area%)
                                                           . -> . (treeof legend-entry?))
                                                          #f)])]{
}

@section{Legend Entries}

@defstruct[legend-entry ([label string?]
                         [draw ((is-a?/c plot-device%) real? real? real? real?
                                                       . -> . void?)])]{
}

@doc-apply[line-legend-entry]
@doc-apply[line-legend-entries]
@doc-apply[rectangle-legend-entry]
@doc-apply[rectangle-legend-entries]
@doc-apply[interval-legend-entry]
@doc-apply[interval-legend-entries]
@doc-apply[point-legend-entry]
@doc-apply[vector-field-legend-entry]

todo: rename vector-field-legend-entry to arrow-legend-entry?
