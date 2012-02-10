#lang scribble/doc
@(require "common.rkt" (for-label "../value-turtles.rkt"))

@title{Value Turtles}

@defmodule[graphics/value-turtles]

The value turtles are a variation on traditional turtles.  Rather than
having just a single window where each operation changes the state of
that window, in the @racket[graphics/value-turtles] library, the
entire turtles window is treated as a value. This means that each of
the primitive operations accepts, in addition to the usual arguments,
a turtles-window value; instead of returning nothing, each returns a
turtles-window value.

@defproc[(turtles [width real?] 
                  [height real?]
                  [init-x real? (/ width 2)]
                  [init-y real? (/ height 2)]
                  [init-angle real? 0])
          turtles?]{

Creates a new turtles window with the given @racket[width] and
@racket[height]. The remaining arguments specify position of the
initial turtle and the direction in radians (where @racket[0] is to
the right).}

@defproc[(turtles? [v any/c]) boolean?]{
  Determines if @racket[v] is a turtles drawing.
}

@defproc[(move [n real?] [turtles turtles?]) turtles?]{

Moves the turtle @racket[n] pixels, returning a new turtles window.}

@defproc[(draw [n real?] [turtles turtles?]) turtles?]{

Moves the turtle @racket[n] pixels and draws a line along the path,
returning a new turtles window.}

@defproc[(erase [n real?] [turtles turtles?]) turtles?]{

Moves the turtle @racket[n] pixels and erases a line along the path,
returning a new turtles window.}


@deftogether[(
@defproc[(move-offset [h real?] [v real?] [turtles turtles?]) turtles?]
@defproc[(draw-offset [h real?] [v real?] [turtles turtles?]) turtles?]
@defproc[(erase-offset [h real?] [v real?] [turtles turtles?]) turtles?]
)]{

Like @racket[move], @racket[draw], and @racket[erase], but using a
horizontal and vertical offset from the turtle's current position.}


@defproc[(turn [theta real?] [turtles turtles?]) turtles?]{

Turns the turtle @racket[theta] degrees counter-clockwise, returning a
new turtles window.}

@defproc[(turn/radians [theta real?] [turtles turtles?]) turtles?]{

Turns the turtle @racket[theta] radians counter-clockwise, returning a
new turtles window.}

@defproc[(merge [turtles1 turtles?] [turtles2 turtles?]) turtles?]{

The @racket[split] and @racket[tprompt] forms provided by
@racketmodname[graphics/turtles] aren't needed for
@racketmodname[graphics/value-turtles], since the turtles window is a
value.
    
Instead, the @racket[merge] accepts two turtles windows and combines
the state of the two turtles windows into a single window. The new
window contains all of the turtles of the previous two windows, but
only the line drawings of the first turtles argument.}

@defproc[(clean [turtles turtles?]) turtles?]{
  Produces a turtles like @racket[turtles], but with only a single
  turtle, positioned in the center.
}

@; ----------------------------------------

@section[#:tag "value-examples"]{Examples}

@defmodule[graphics/value-turtles-examples]

The @racketmodname[graphics/turtle-examples] library's source is meant
to be read, but it also exports the following examples.

@defproc[(radial-turtles [n exact-nonnegative-integer?] [turtles turtles?]) turtles?]{

Places @math{2^n} turtles spaced evenly pointing radially outward.}

@defproc[(spaced-turtles [n exact-nonnegative-integer?] [turtles turtles?]) turtles?]{

Places @math{2^n} turtles evenly spaced in a line and pointing in the
same direction as the original turtle.}


@defproc[(neato [turtles turtles?]) turtles?]{
 As the name says...
}

@defproc[(regular-poly [sides exact-nonnegative-integer?] 
                       [radius real?]
                       [turtles turtles?])
         turtles?]{

Draws a regular poly centered at the turtle with @racket[sides] sides
and with radius @racket[radius].}

@defproc[(regular-polys [n exact-nonnegative-integer?] 
                        [s any/c]
                        [turtles turtles?])
         turtles?]{

Draws @racket[n] regular polys each with @racket[n] sides centered at
the turtle.}

@defproc[(spokes [turtles turtles?]) turtles?]{
 
Draws some spokes, using @racket[radial-turtles] and
@racket[spaced-turtles].}

@defproc[(spyro-gyra [turtles turtles?]) turtles?]{
 
Draws a spyro-grya reminiscent shape.}
