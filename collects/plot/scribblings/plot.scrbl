#lang scribble/manual

@(require "common.rkt")

@(define (author-email) "neil.toronto@gmail.com")

@title[#:tag "top"]{@(plot-name): Graph Plotting}
@author{@(author+email "Neil Toronto" (author-email))}

@defmodule[plot]

@(plot-name) provides an flexible interface for producing nearly any kind of plot.
It includes many common kinds already, such as scatter plots, line plots, contour plots, histograms, and 3D surfaces and isosurfaces.
Thanks to Racket's excellent multiple-backend drawing library, @(plot-name) can render plots as manipulatable images in DrRacket, as bitmaps in slideshows, as PNG, PDF, PS and SVG files, or on any device context.

@bold{A note on backward compatibility.} @(plot-name) has undergone a major rewrite between versions 5.1.3 and 5.2.
Many programs written using @(plot-name) 5.1.3 and earlier will still compile, run and generate plots.
For code that does not, see the @(racketmodname plot/compat) module.

Some functions, like @(racket mix), @(racket line) and @(racket surface), still exist for backward compatibility, but are deprecated and may be removed in the future.
Set @(racket (plot-deprecation-warnings? #t)) to be alerted to uses of deprecated features.

@table-of-contents[]

@include-section["intro.scrbl"]

@include-section["plot2d.scrbl"]

@include-section["renderer2d.scrbl"]

@include-section["plot3d.scrbl"]

@include-section["renderer3d.scrbl"]

@include-section["utils.scrbl"]

@include-section["params.scrbl"]

@include-section["contracts.scrbl"]

@include-section["custom.scrbl"]

@include-section["compat.scrbl"]

@include-section["todo.scrbl"]
