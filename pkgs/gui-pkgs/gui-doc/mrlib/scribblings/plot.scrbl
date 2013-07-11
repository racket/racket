#lang scribble/doc
@(require "common.rkt" (for-label mrlib/plot))

@title{Plot}

@defmodule[mrlib/plot]{The @racketmodname[mrlib/plot] library provides
a simple tool for plotting data values to a device context.}

@bold{This is an old library, kept only for compatibility.} You will
undoubtedly want to use the @racketmodname[plot #:indirect] library instead, which offers
many more features and is actively maintained.

@defstruct[data-set ([points (listof (is-a?/c point%))]
                     [connected? any/c]
                     [pen (is-a?/c pen%)]
                     [min-x real?]
                     [max-x real?]
                     [min-y real?]
                     [max-y real?])]{

The @racket[points] field contains the data values to plot, and
@racket[connected?] indicates whether the points are connected by a
line. The @racket[pen] field provides a pen for plotting
points/lines. The remaining fields determine the plotting area within
a drawing context.}

@defstruct[plot-setup ([axis-label-font (is-a?/c font%)]
                       [axis-number-font (is-a?/c font%)]
                       [axis-pen (is-a?/c pen)]
                       [grid? any/c]
                       [grid-pen (is-a?/c pen)]
                       [x-axis-marking (listof real?)]
                       [y-axis-marking (listof real?)]
                       [x-axis-label string?]
                       [y-axis-label string?])]{

Configures a plot. The @racket[grid?] field determines whether to draw
a grid at axis markings, and the @racket[x-axis-marking] and
@racket[y-axis-marking] lists supply locations for marks on each
axis. The other fields are self-explanatory.}

@defproc[(plot [dc (is-a?/c dc<%>)]
               [data (listof data-set?)]
               [setup plot-setup?])
         void?]{

Draws the @racket[data-set]s in @racket[data] into the given
@racket[dc]. Uses drawing-context coordinates in @racket[data-set]s
that will accommodate all of the data sets.}
