#lang scribble/manual
@(require (for-label scribble/struct
                     scriblib/figure
                     scheme/base
                     scheme/contract))

@title[#:tag "figure"]{Figures}

@defmodule[scriblib/figure]

@defproc[(figure-style-extras) list?]{

Include the content of the result list in the style of a document part
that includes all figures. These style extras pull in HTML and Latex
rendering support.}


@deftogether[(
@defproc[(figure [tag string?] [caption any/c] [pre-content any/c] ...) block?]
@defproc[(figure* [tag string?] [caption any/c] [pre-content any/c] ...) block?]
@defproc[(figure** [tag string?] [caption any/c] [pre-content any/c] ...) block?]
)]{

Creates a figure. The given @scheme[tag] is for use with
@scheme[Figure-ref]. The @scheme[caption] is an element. The
@scheme[pre-content] is decoded as a flow.

For HTML output, the @scheme[figure*] and @scheme[figure*] functions
center the figure content, while @scheme[figure**] allows the content
to be wider than the document body.

For two-column latex output, @scheme[figure*] and @scheme[figure**]
generate a figure that spans columns.}

@defproc[(Figure-target [tag string?]) element?]{

Generates a new figure label---normally not used directly, since it is
used by @scheme[figure].}

@defproc[(Figure-ref [tag string?]) element?]{

Generates a reference to a figure.}

