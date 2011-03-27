#lang scribble/manual
@(require (for-label scribble/core
                     scribble/decode
                     scriblib/figure
                     scheme/base
                     scheme/contract))

@title[#:tag "figure"]{Figures}

@defmodule[scriblib/figure]

@deftogether[(
@defproc[(figure [tag string?] [caption content?] [pre-flow pre-flow?] ...) block?]
@defproc[(figure* [tag string?] [caption content?] [pre-flow pre-flow?] ...) block?]
@defproc[(figure** [tag string?] [caption content?] [pre-flow pre-flow?] ...)
block?]
@defproc[(figure-here [tag string?] [caption content?] [pre-flow pre-flow?] ...) block?]
)]{

Creates a figure. The given @scheme[tag] is for use with
@scheme[figure-ref] or @scheme[Figure-ref]. The @scheme[caption] is an
element. The @scheme[pre-flow] is decoded as a flow.

For HTML output, the @scheme[figure] and @scheme[figure*] functions
center the figure content, while @scheme[figure**] allows the content
to be wider than the document body.
For two-column Latex output, @scheme[figure*] and @scheme[figure**]
generate a figure that spans columns.

For Latex output, @scheme[figure-here] generates a figure to be included at
the position in the output text where the @scheme[figure-here] occurs
in the source text. For HTML output, all @scheme[figure] variants
place the figure where the use appears in the source text.}


@defproc[(figure-ref [tag string?]) element?]{

Generates a reference to a figure, using a lowercase word ``figure''.}


@defproc[(Figure-ref [tag string?]) element?]{

Generates a reference to a figure, capitalizing the word ``Figure''.}


@defproc[(Figure-target [tag string?]) element?]{

Generates a new figure label. This function is normally not used
directly, since it is used by @scheme[figure].}
