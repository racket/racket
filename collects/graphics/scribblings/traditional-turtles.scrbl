#lang scribble/doc
@(require "common.rkt" (for-label "../turtles.rkt" "../turtle-examples.rkt"))

@title{Traditional Turtles}

@defmodule[graphics/turtles]

To use any of the turtle drawing functions, you first need to
initialize the turtles by calling @racket[(turtles #t)].

@defproc*[([(turtles [on? any/c]) void?]
           [(turtles) void?])]{

Shows and hides the turtles window based on @racket[on?]. If
@racket[on?] is not supplied, the state is toggled.}

@defproc[(move [n real?]) void?]{

Moves the turtle @racket[n] pixels without drawing.}

@defproc[(draw [n real?]) void?]{

Moves the turtle @racket[n] pixels and draws a line on the path.}

@defproc[(erase [n real?]) void?]{

Moves the turtle @racket[n] pixels and erase along the path.}

@deftogether[(
@defproc[(move-offset [h real?][v real?]) void?]
@defproc[(draw-offset [h real?][v real?]) void?]
@defproc[(erase-offset [h real?][v real?]) void?]
)]{

Like @racket[move], @racket[draw], and @racket[erase], but using a
horizontal and vertical offset from the turtle's current position.}


@defproc[(turn [theta real?]) void?]{
 
Turns the turtle @racket[theta] degrees counter-clockwise.}

@defproc[(turn/radians [theta real?]) void?]{

Turns the turtle @racket[theta] radians counter-clockwise.}

@defproc[(clear) void?]{

Erases the turtles window.}

@defproc[(home) void?]{

Leaves only one turtle, in the start position.}

@defform[(split expr ...)]{

Spawns a new turtle where the turtle is currently located. In order to
distinguish the two turtles, only the new one evaluates
@racket[expr]. For example, if you start with a fresh turtle-window
and evaluate:

@racketblock[
(split (turn/radians (/ pi 2)))
]

you will have two turtles, pointing at right angles to each
other. Continue with 

@racketblock[
(draw 100)
]

You will see two lines. Now, if you evaluate those two expression
again, you will have four turtles, etc.}

@defform[(split* expr ...)]{

Like @racket[(split expr ...)], except that one turtle is created for
each @racket[expr].

For example, to create two turtles, one pointing at @math["\u3C0/2"]
and one at @math["\u3C0/3"], evaluate

@racketblock[
 (split* (turn/radians (/ pi 3)) (turn/radians (/ pi 2)))
]}

@defform[(tprompt expr ...)]{

Limits the splitting of the turtles. Before @racket[expr] is evaluated,
the state of the turtles (how many, their positions and headings) is
``checkpointed.'' Then @racket[expr] is evaluated, and then the state
of the turtles is restored, but all drawing that may have occurred
during execution of @racket[expr] remains.

For example

@racketblock[
(tprompt (draw 100))
]

moves a turtle forward 100 pixel while drawing a line, and then moves
the turtle be immediately back to its original position. Similarly,

@racketblock[
(tprompt (split (turn/radians (/ pi 2))))
]

splits the turtle into two, rotates one 90 degrees, and then
collapses back to a single turtle.

The fern functions below demonstrate more advanced use of
@racket[tprompt].}

@defproc[(save-turtle-bitmap [name (or/c path-string? output-port?)]
                             [kind (or/c 'png 'jpeg 'xbm 'xpm 'bmp)])
         void?]{
  Saves the current state of the turtles window in an image file.
}

@defthing[turtle-window-size exact-positive-integer?]{
  The size of the turtles window.
}

@; ----------------------------------------

@section{Examples}

@defmodule[graphics/turtle-examples]

The @racketmodname[graphics/turtle-examples] library's source is meant
to be read, but it also exports the following examples. To display these
examples, first initialize the turtle window with @racket[(turtles #t)].

@defproc[(regular-poly [sides exact-nonnegative-integer?] [radius real?])
         void?]{

Draws a regular poly centered at the turtle with @racket[sides] sides
and with radius @racket[radius].}

@defproc[(regular-polys [n exact-nonnegative-integer?] [s any/c])
         void?]{

Draws @racket[n] regular polys each with @racket[n] sides centered at
the turtle.}

@defproc[(radial-turtles [n exact-nonnegative-integer?]) void?]{

Places @math{2^n} turtles spaced evenly pointing radially outward.}

@defproc[(spaced-turtles [n exact-nonnegative-integer?]) void?]{

Places @math{2^n} turtles evenly spaced in a line and pointing in the
same direction as the original turtle.}

@defproc[(spokes) void?]{
 
Draws some spokes, using @racket[radial-turtles] and
@racket[spaced-turtles].}

@defproc[(gapped-lines) void?]{
   Draw a bunch of parallel line segments, using 
   @racket[spaced-turtles].
}

@defproc[(spyro-gyra) void?]{
 
Draws a spyro-grya reminiscent shape.}

@defproc[(neato) void?]{

As the name says...}

@defproc[(graphics-bexam) void?]{
  Draws a fractal that came up on an exam given at Rice in 1997 or so.
}

@defthing[sierp-size real?]{

A constant that is a good size for the @racket[sierp] procedures.}

@deftogether[(
@defproc[(sierp [sierp-size real?]) void?]
@defproc[(sierp-nosplit [sierp-size real?]) void?]
)]{

Draws the @as-index{Sierpinski triangle} in two different ways, the
first using @racket[split] heavily. After running the first one, try
executing @racket[(draw 10)].}


@defthing[koch-size real?]{

A constant that is a good size for the @racket[koch] procedures.}

@deftogether[(
@defproc[(koch-split [koch-size real?]) void?]
@defproc[(koch-draw [koch-size real?]) void?]
)]{

Draws the same @as-index{Koch snowflake} in two different ways.}

@defproc[(lorenz [a real?] [b real?] [c real?]) void?]{

Watch the @as-index{Lorenz attractor} (a.k.a. @as-index{butterfly attractor})
initial values @racket[a], @racket[b], and @racket[c].}

@defproc[(lorenz1) void?]{

Calls @racket[lorenz] with good initial values.}

@defproc[(peano [peano-size real?]) void?]{
  Draws the @as-index{Peano space-filling curve}.
}

@defproc[(peano-position-turtle) void?]{
  Moves the turtle to a good place to prepare for
  a call to @racket[peano].
}

@defthing[peano-size exact-nonnegative-integer?]{
  One size to use with @racket[peano].
}

@defthing[fern-size exact-nonnegative-integer?]{

A good size for the @racket[fern1] and @racket[fern2] functions.}

@deftogether[(
@defproc[(fern1 [fern-size exact-nonnegative-integer?]) void?]
@defproc[(fern2 [fern-size exact-nonnegative-integer?]) void?]
)]{

Draws a @as-index{fern fractal}.

For @racket[fern1], you will probably want to point the turtle up
before running this one, with something like:

@racketblock[
(turn/radians (- (/ pi 2)))
]

For @racket[fern2], you may need to backup a little.}

