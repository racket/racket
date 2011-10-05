#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "contracts"]{Plot Contracts}

@section[#:tag "contracts.convenience"]{Conveniences}

@doc-apply[real>=/c]
@doc-apply[integer>=/c]
@doc-apply[treeof]

@section[#:tag "contracts.drawing"]{Drawing Parameters}

@doc-apply[anchor/c]
@doc-apply[rgb/c]
@doc-apply[color/c]
@doc-apply[plot-color/c]
@doc-apply[pen-style/c]
@doc-apply[brush-style/c]
@doc-apply[plot-pen-style/c]
@doc-apply[plot-brush-style/c]
@doc-apply[font-family/c]
@doc-apply[point-sym/c]

@section[#:tag "contracts.sequence"]{Color, Width and Style Sequences}

@doc-apply[plot-colors/c]
@doc-apply[plot-pen-styles/c]
@doc-apply[pen-widths/c]
@doc-apply[plot-brush-styles/c]
@doc-apply[alphas/c]

@section[#:tag "contracts.function"]{Color, Width and Style Functions}

@doc-apply[plot-color-function/c]
@doc-apply[plot-pen-style-function/c]
@doc-apply[pen-width-function/c]
@doc-apply[plot-brush-style-function/c]
@doc-apply[alpha-function/c]
