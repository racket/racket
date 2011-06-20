#lang scribble/doc
@(require "common.rkt" (for-label mrlib/plot))

@title{Plot}

@defmodule[mrlib/plot]{The @schememodname[mrlib/plot] library provides
a simple tool for plotting data values to a device context.}

@defstruct[data-set ([points (listof (is-a?/c point%))]
                     [connected? any/c]
                     [pen (is-a?/c pen%)]
                     [min-x real?]
                     [max-x real?]
                     [min-y real?]
                     [max-y real?])]{

The @scheme[points] field contains the data values to plot, and
@scheme[connected?] indicates whether the points are connected by a
line. The @scheme[pen] field provides a pen for plotting
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

Configures a plot. The @scheme[grid?] field determines whether to draw
a grid at axis markings, and the @scheme[x-axis-marking] and
@scheme[y-axis-marking] lists supply locations for marks on each
axis. The other fields are self-explanatory.}

@defproc[(plot [dc (is-a?/c dc<%>)]
               [data (listof data-set?)]
               [setup plot-setup?])
         void?]{

Draws the @scheme[data-set]s in @scheme[data] into the given
@scheme[dc]. Uses drawing-context coordinates in @scheme[data-set]s
that will accommodate all of the data sets.}
