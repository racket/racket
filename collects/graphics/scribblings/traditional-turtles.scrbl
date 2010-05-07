#lang scribble/doc
@(require "common.ss"
          (for-label "../turtles.ss"
                     "../turtle-examples.ss"))

@title{Traditional Turtles}

@defmodule[graphics/turtles]

@defproc*[([(turtles [on? any/c]) void?]
           [(turtles) void?])]{

Shows and hides the turtles window based on @scheme[on?]. If
@scheme[on?] is not supplied, the state is toggled.}

@defproc[(move [n real?]) void?]{

Moves the turtle @scheme[n] pixels without drawing.}

@defproc[(draw [n real?]) void?]{

Moves the turtle @scheme[n] pixels and draws a line on the path.}

@defproc[(erase [n real?]) void?]{

Moves the turtle @scheme[n] pixels and erase along the path.}

@deftogether[(
@defproc[(move-offset [h real?][v real?]) void?]
@defproc[(draw-offset [h real?][v real?]) void?]
@defproc[(erase-offset [h real?][v real?]) void?]
)]{

Like @scheme[move], @scheme[draw], and @scheme[erase], but using a
horizontal and vertical offset from the turtle's current position.}


@defproc[(turn [theta real?]) void?]{
 
Turns the turtle @scheme[theta] degrees counter-clockwise.}

@defproc[(turn/radians [theta real?]) void?]{

Turns the turtle @scheme[theta] radians counter-clockwise.}

@defproc[(clear) void?]{

Erases the turtles window.}

@defproc[(home) void?]{

Leaves only one turtle, in the start position.}

@defform[(split expr ...)]{

Spawns a new turtle where the turtle is currently located. In order to
distinguish the two turtles, only the new one evaluates
@scheme[expr]. For example, if you start with a fresh turtle-window
and type:

@schemeblock[
(split (turn/radians (/ pi 2)))
]

you will have two turtles, pointing at right angles to each
other. Continue with 

@schemeblock[
(draw 100)
]

You will see two lines. Now, if you evaluate those two expression
again, you will have four turtles, etc.}

@defform[(split* expr ...)]{

Like @scheme[(split expr ...)], except that one turtle is created for
each @scheme[expr].

For example, to create two turtles, one pointing at @math["\u3C0/2"]
and one at @math["\u3C0/3"], evaluate

@schemeblock[
 (split* (turn/radians (/ pi 3)) (turn/radians (/ pi 2)))
]}

@defform[(tprompt expr ...)]{

Limits the splitting of the turtles. Before@scheme[expr] is evaluated,
the state of the turtles (how many, their positions and headings) is
``checkpointed.'' Then @scheme[expr] is evaluated, and then the state
of the turtles is restored, but all drawing that may have occurred
during execution of @scheme[expr] remains.

For example

@schemeblock[
(tprompt (draw 100))
]

moves a turtle forward 100 pixel while drawing a line, and then moves
the turtle be immediately back to it's original position. Similarly,

@schemeblock[
(tprompt (split (turn/radians (/ pi 2))))
]

splits the turtle into two, rotates one 90 degrees, and then
collapses back to a single turtle.

The fern functions below demonstrate more advanced use of
@scheme[tprompt].}

@; ----------------------------------------

@section{Examples}

@defmodule[graphics/turtle-examples]

The @schememodname[graphics/turtle-examples] library's source is meant
to be read, but it also exports the following examples.

@defproc[(regular-poly [sides exact-nonnegative-integer?] [radius real?])
         void?]{

Draws a regular poly centered at the turtle with @scheme[sides] sides
and with radius @scheme[radius].}

@defproc[(regular-polys [n exact-nonnegative-integer?] [s any/c])
         void?]{

Draws @scheme[n] regular polys each with @scheme[n] sides centered at
the turtle.}

@defproc[(radial-turtles [n exact-nonnegative-integer?]) void?]{

Places @math{2^n} turtles spaced evenly pointing radially outward.}

@defproc[(spaced-turtles [n exact-nonnegative-integer?]) void?]{

Places @math{2^n} turtles evenly spaced in a line and pointing in the
same direction as the original turtle.}

@defproc[(spokes) void?]{
 
Draws some spokes, using @scheme[radial-turtles] and
@scheme[spaced-turtles].}

@defproc[(spyro-gyra) void?]{
 
Draws a spyro-grya reminiscent shape.}

@defproc[(neato) void?]{

As the name says...}

@defproc[(graphics-bexam) void?]{
  
Draws a fractal that came up on an exam that the author took.}

@defthing[serp-size real?]{
 
A constant that is a good size for the @scheme[serp] procedures.}

@deftogether[(
@defproc[(serp [serp-size real?]) void?]
@defproc[(serp-nosplit [serp-size real?]) void?]
)]{

Draws the @as-index{Serpinski triangle} in two different ways, the
first using @scheme[split] heavily. After running the first one, try
executing @scheme[(draw 10)].}


@defthing[koch-size real?]{

A constant that is a good size for the @scheme[koch] procedures.}

@deftogether[(
@defproc[(koch [koch-size real?]) void?]
@defproc[(koch-nosplit [koch-size real?]) void?]
)]{

Draws the same @as-index{Koch snowflake} in two different ways.}

@defproc[(lorenz [a real?] [b real?] [c real?]) void?]{

Watch the @as-index{Lorenz attractor} (a.k.a. @as-index{butterfly attractor})
initial values @scheme[a], @scheme[b], and @scheme[c].}

@defproc[(lorenz1) void?]{

Calls @scheme[lorenze] with good initial values.}

@deftogether[(
@defproc[(peano1 [peano-size real?]) void?]
@defproc[(peano2 [peano-size real?]) void?]
)]{

Draws the @as-index{Peano space-filling curve}, where @scheme[peano1] uses
@scheme[split].}

@defthing[fern-size exact-nonnegative-integer?]{

A good size for the @scheme[fern1] and @scheme[fern2] functions.}

@deftogether[(
@defproc[(fern1 [fern-size exact-nonnegative-integer?]) void?]
@defproc[(fern2 [fern-size exact-nonnegative-integer?]) void?]
)]{

Draws a @as-index{fern fractal}.

For @scheme[fern1], you will probably want to point the turtle up
before running this one, with something like:

@schemeblock[
(turn/radians (- (/ pi 2)))
]

For @scheme[fern2], you may need to backup a little.}

