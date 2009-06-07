#lang scribble/doc

@(require scribble/manual "shared.ss"
          (for-label scheme 
                     teachpack/htdp/draw))

@teachpack["draw"]{Simple Drawing}

@declare-exporting[teachpack/htdp/draw]

The teachpack provides two sets of functions: one for drawing into a canvas
and one for reacting to canvas events. 

@bold{Warning:} @emph{This teachpack is deprecated. Unless you're solving exercises
taken from @emph{How To Design Programs}, we strongly encourage
you to use the world teachpack instead; see @secref{world}.}

@section[#:tag "drawing"]{Drawing on a Canvas}

@deftech{DrawColor}: @scheme[(and/c symbol? (one-of/c 'white 'yellow 'red 'blue 'green 'black))]
These six colors are definitely provided. If you want other colors,
guess! For example, @scheme['orange] works, but @scheme['mauve]
doesn't. If you apply the function to a symbol that it doesn't recognize as
a color, it raises an error.

@defproc[(start [width number?][height number?]) true]{Opens a
@scheme[width] x @scheme[height] canvas.} 

@defproc[(start/cartesian-plane [width number?][height number?])
         true]{
Opens a @scheme[width] x @scheme[height] canvas and draws a Cartesian
plane.}

@defproc[(stop) true]{Closes the canvas.}

@defproc[(draw-circle [p posn?] [r number?] [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @scheme[c] circle at @scheme[p] with radius @scheme[r].}

@defproc[(draw-solid-disk [p posn?] [r number?] [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @scheme[c] disk at @scheme[p] with radius @scheme[r].}

@defproc[(draw-solid-rect [ul posn?] [width number?] [height number?]
                          [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @scheme[width] x @scheme[height], @scheme[c] rectangle with the
upper-left corner at @scheme[ul].}

@defproc[(draw-solid-line [strt posn?] [end posn?]
                          [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @scheme[c] line from @scheme[strt] to @scheme[end].}

@defproc[(draw-solid-string [p posn?] [s string?]) true]{
Draws @scheme[s] at @scheme[p].}

@defproc[(sleep-for-a-while [s number?]) true]{
Suspends evaluation for @scheme[s] seconds.}

The teachpack also provides @scheme[clear-] operations for each
@scheme[draw-] operation. The arguments are the same. Note: use
@scheme[clear-rectangle] instead of @scheme[clear-string] for now.
The color argument for all @scheme[clear-] functions are optional.

@;-----------------------------------------------------------------------------
@section[#:tag "interaction"]{Interactions with Canvas}

@defproc[(wait-for-mouse-click) posn?]{
Waits for the user to click on the mouse, within the canvas.}

@deftech{DrawKeyEvent}: @scheme[(or/c char? symbol?)] A
@tech{DrawKeyEvent} represents keyboard events: 
@itemize[
 @item{@scheme[char?], if the user pressed an alphanumeric key;}
 @item{@scheme[symbol?], if the user pressed, for example, an arror key:
 @scheme['up] @scheme['down]  @scheme['left] @scheme['right]}
]

@defproc[(get-key-event) (or/c false (unsyntax @tech{DrawKeyEvent}))]{Checks whether the
user has pressed a key within the window; @scheme[false] if not.}

@deftech{DrawWorld}: For proper interactions, using the teachpack
 requires that you provide a data definition for @tech{DrawWorld} . In
 principle, there are no constraints on this data definition. You can even
 keep it implicit, even if this violates the Design Recipe.

The following functions allow programs to react to events from the canvas.

@defproc[(big-bang [n number?] [w (unsyntax @tech{DrawWorld})]) true]{Starts the clock, one tick every
@scheme[n] (fractal) seconds; @scheme[w] becomes the first ``current'' world.}

@defproc[(on-key-event [change (-> (unsyntax @tech{DrawKeyEvent}) (unsyntax @tech{DrawWorld}) (unsyntax @tech{DrawWorld}))])
true]{Adds @scheme[change] to the world. The function reacts to keyboard
events and creates a new @scheme[@#,tech{DrawWorld}].}

@defproc[(on-tick-event [tock (-> (unsyntax @tech{DrawWorld}) (unsyntax @tech{DrawWorld}))]) true]{Adds @scheme[tock]
to the world. The function reacts to clock tick events, creating a new
current world.}

@defproc[(end-of-time) (unsyntax @tech{DrawWorld})]{Stops the world; returns the current world.} 
