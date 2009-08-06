#lang scribble/doc
@(require "common.ss"
          (for-label "../value-turtles.ss"))
                     

@title{Value Turtles}

@defmodule[graphics/value-turtles]

The value turtles are a variation on traditional turtles.  Rather than
having just a single window where each operation changes the state of
that window, in the @scheme[graphics/value-turtles] library, the
entire turtles window is treated as a value. This means that each of
the primitive operations accepts, in addition to the usual arguments,
a turtles-window value; instead of returning nothing, each returns a
turtles-window value.

@defproc[(turtles [width real?] 
                  [height real?]
                  [init-x real? (/ width 2)]
                  [init-y real? (/ height 2)]
                  [init-angle real? 0])
          turtles-window?]{

Creates a new turtles window with the given @scheme[width] and
@scheme[height]. The remaining arguments specify position of the
initial turtle and the direction in radians (where @scheme[0] is to
the right).}

@defproc[(move [n real?] [turtles turtles-window?]) turtles-window?]{

Moves the turtle @scheme[n] pixels, returning a new turtles window.}

@defproc[(draw [n real?] [turtles turtles-window?]) turtles-window?]{

Moves the turtle @scheme[n] pixels and draws a line along the path,
returning a new turtles window.}

@defproc[(erase [n real?] [turtles turtles-window?]) turtles-window?]{

Moves the turtle @scheme[n] pixels and erases a line along the path,
returning a new turtles window.}


@deftogether[(
@defproc[(move-offset [h real?] [v real?] [turtles turtles-window?]) turtles-window?]
@defproc[(draw-offset [h real?] [v real?] [turtles turtles-window?]) turtles-window?]
@defproc[(erase-offset [h real?] [v real?] [turtles turtles-window?]) turtles-window?]
)]{

Like @scheme[move], @scheme[draw], and @scheme[erase], but using a
horizontal and vertical offset from the turtle's current position.}


@defproc[(turn [theta real?] [turtles turtles-window?]) turtles-window?]{

Turns the turtle @scheme[theta] degrees counter-clockwise, returning a
new turtles window.}

@defproc[(turn/radians [theta real?] [turtles turtles-window?]) turtles-window?]{

Turns the turtle @scheme[theta] radians counter-clockwise, returning a
new turtles window.}

@defproc[(merge [turtles1 turtles-window?] [turtles2 turtles-window?]) turtles-window?]{

The @scheme[split] and @scheme[tprompt] forms provided by
@schememodname[graphics/turtles] isn't needed for
@schememodname[graphics/value-turtles], since the turtles window is a
value.
    
Instead, the @scheme[merge] accepts two turtles windows and combines
the state of the two turtles windows into a single window. The new
window contains all of the turtles of the previous two windows, but
only the line drawings of the first turtles argument.}

@; ----------------------------------------

@section[#:tag "value-examples"]{Examples}

@defmodule[graphics/value-turtles-examples]

The @schememodname[graphics/value-turtles-examples] library is similar
to @schememodname[graphics/turtle-examples], but using
@schememodname[graphics/value-turtles] instead of
@schememodname[graphics/turtles].
