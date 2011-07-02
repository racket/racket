#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/graphing))

@teachpack["graphing"]{Graphing Functions}

@;declare-exporting[teachpack/htdp/graphing]
@defmodule[#:require-form beginner-require htdp/graphing #:use-sources (htdp/draw)]

The teachpack provides two functions for graphing functions in the regular
(upper right) quadrant of the Cartesian plane (between 0 and 10 in both
directions): 

@defproc[(graph-fun [f (-> number?  number?)][color symbol?]) true]{
Draws the graph of @racket[f] with the given @racket[color].}

@defproc[(graph-line [line (-> number? number?)][color symbol?]) true]{
Draws @racket[line], a function representing a straight line, with a given
color.} 

For color symbols, see @secref{draw}. 

In addition, the teachpack re-exports the entire functionality of the
drawing library; see @secref{draw} for documentation. 

