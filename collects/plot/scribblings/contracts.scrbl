#lang scribble/manual

@(require "common.rkt")

@declare-exporting[plot]

@title[#:tag "contracts"]{Plot Contracts}

@section{Convenience Contracts}

@doc-apply[treeof]

@section{Appearance Argument Contracts}

@doc-apply[anchor/c]
@doc-apply[color/c]
@doc-apply[plot-color/c]
@doc-apply[plot-pen-style/c]
@doc-apply[plot-brush-style/c]
@doc-apply[font-family/c]
@doc-apply[point-sym/c]

@defthing[known-point-symbols (listof symbol?)]{
A list containing the symbols that are valid @(racket points) labels.

@interaction[#:eval plot-eval
                    (require (only-in srfi/13 string-pad-right))
                    (for ([sym  (in-list known-point-symbols)]
                          [n    (in-cycle (in-range 3))])
                      (display (string-pad-right (format "~v" sym) 22))
                      (when (= n 2) (newline)))
                    (length known-point-symbols)]
}

@section{Appearance Argument Sequence Contracts}

@doc-apply[plot-colors/c]
@doc-apply[plot-pen-styles/c]
@doc-apply[pen-widths/c]
@doc-apply[plot-brush-styles/c]
@doc-apply[alphas/c]
