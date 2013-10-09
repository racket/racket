#lang scribble/manual

@(require "common.rkt")

@(define (author-email) "neil.toronto@gmail.com")

@title[#:tag "top"]{@(plot-name): Graph Plotting}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[plot]

Typed Racket users should use

@defmodule*/no-declare[(plot/typed)]

@(plot-name) provides a flexible interface for producing nearly any kind of plot.
It includes many common kinds already, such as scatter plots, line plots, contour plots, histograms, and 3D surfaces and isosurfaces.
Thanks to Racket's excellent multiple-backend drawing library, @(plot-name) can render plots as manipulatable images in DrRacket, as bitmaps in slideshows, as PNG, PDF, PS and SVG files, or on any device context.

For non-GUI uses, see @racketmodname[plot/no-gui].
For REPL-like environments outside of DrRacket (including Scribble manuals) in particular, see @racketmodname[plot/pict] and @racketmodname[plot/bitmap].

@bold{A note on backward compatibility.} @(plot-name) has undergone a major rewrite between versions 5.1.3 and 5.2.
Many programs written using @(plot-name) 5.1.3 and earlier will still compile, run and generate plots.
Some programs will not.
Most programs use deprecated functions such as @(racket mix), @(racket line) and @(racket surface). These functions still exist for backward compatibility, but are deprecated and may be removed in the future.
If you have code written for @(plot-name) 5.1.3 or earlier, please see @secref["porting"] (and possibly @secref["compat"]).

@table-of-contents[]

@include-section["intro.scrbl"]

@include-section["plotting.scrbl"]

@include-section["renderer2d.scrbl"]

@include-section["renderer3d.scrbl"]

@include-section["nonrenderer.scrbl"]

@include-section["ticks.scrbl"]

@include-section["utils.scrbl"]

@include-section["params.scrbl"]

@include-section["contracts.scrbl"]

@;@include-section["custom.scrbl"]

@include-section["porting.scrbl"]

@include-section["compat.scrbl"]

@close-plot-eval[]
