#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/draw))

@teachpack["draw"]{Simple Drawing}

@;declare-exporting[teachpack/htdp/draw]
@defmodule[#:require-form beginner-require htdp/draw #:use-sources (htdp/big-draw htdp/draw)]

The teachpack provides two sets of functions: one for drawing into a canvas
and one for reacting to canvas events. 

@deprecated[(list @racketmodname[2htdp/image] " (probably in conjunction with " @racketmodname[2htdp/universe] ")")]{
  You may continue to use the library for solving exercises 
  from @emph{How To Design Programs, First Edition} but do consider
  switching to @link["http://www.ccs.neu.edu/home/matthias/HtDP2e/"]{How To Design Programs, Second Edition} instead.}

@section[#:tag "drawing"]{Drawing on a Canvas}

@deftech{DrawColor}: @racket[(and/c symbol? (one-of/c 'white 'yellow 'red 'blue 'green 'black))]
These six colors are definitely provided. If you want other colors,
guess! For example, @racket['orange] works, but @racket['mauve]
doesn't. If you apply the function to a symbol that it doesn't recognize as
a color, it raises an error.

@defproc[(start [width number?][height number?]) true]{Opens a
@racket[width] x @racket[height] canvas.} 

@defproc[(start/cartesian-plane [width number?][height number?])
         true]{
Opens a @racket[width] x @racket[height] canvas and draws a Cartesian
plane.}

@defproc[(stop) true]{Closes the canvas.}

@defproc[(draw-circle [p posn?] [r number?] [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @racket[c] circle at @racket[p] with radius @racket[r].}

@defproc[(draw-solid-disk [p posn?] [r number?] [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @racket[c] disk at @racket[p] with radius @racket[r].}

@defproc[(draw-solid-rect [ul posn?] [width number?] [height number?]
                          [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @racket[width] x @racket[height], @racket[c] rectangle with the
upper-left corner at @racket[ul].}

@defproc[(draw-solid-line [strt posn?] [end posn?]
                          [c (unsyntax @tech{DrawColor})])
         true]{
Draws a @racket[c] line from @racket[strt] to @racket[end].}

@defproc[(draw-solid-string [p posn?] [s string?]) true]{
Draws @racket[s] at @racket[p].}

@defproc[(sleep-for-a-while [s number?]) true]{
Suspends evaluation for @racket[s] seconds.}

The teachpack also provides @racket[clear-] functions for each
@racket[draw-] function:

@defproc[(clear-circle [p posn?] [r number?] [c (unsyntax @tech{DrawColor})])
         true]{
clears a @racket[c] circle at @racket[p] with radius @racket[r].}

@defproc[(clear-solid-disk [p posn?] [r number?] [c (unsyntax @tech{DrawColor})])
         true]{
clears a @racket[c] disk at @racket[p] with radius @racket[r].}

@defproc[(clear-solid-rect [ul posn?] [width number?] [height number?]
                          [c (unsyntax @tech{DrawColor})])
         true]{
clears a @racket[width] x @racket[height], @racket[c] rectangle with the
upper-left corner at @racket[ul].}

@defproc[(clear-solid-line [strt posn?] [end posn?]
                          [c (unsyntax @tech{DrawColor})])
         true]{
clears a @racket[c] line from @racket[strt] to @racket[end].}

@defproc[(clear-solid-string [p posn?] [s string?]) true]{
 clears @racket[s] at @racket[p].}

@defproc[(clear-all) true]{
 clears the entire screen.}

@;-----------------------------------------------------------------------------
@section[#:tag "interaction"]{Interactions with Canvas}

@defproc[(wait-for-mouse-click) posn?]{
Waits for the user to click on the mouse, within the canvas.}

@deftech{DrawKeyEvent}: @racket[(or/c char? symbol?)] A
@tech{DrawKeyEvent} represents keyboard events: 
@itemize[
 @item{@racket[char?], if the user pressed an alphanumeric key;}
 @item{@racket[symbol?], if the user pressed, for example, an arror key:
 @racket['up] @racket['down]  @racket['left] @racket['right]}
]

@defproc[(get-key-event) (or/c false (unsyntax @tech{DrawKeyEvent}))]{Checks whether the
user has pressed a key within the window; @racket[false] if not.}

@deftech{DrawWorld}: For proper interactions, using the teachpack
 requires that you provide a data definition for @tech{DrawWorld} . In
 principle, there are no constraints on this data definition. You can even
 keep it implicit, even if this violates the Design Recipe.

The following functions allow programs to react to events from the canvas.

@defproc[(big-bang [n number?] [w (unsyntax @tech{DrawWorld})]) true]{Starts the clock, one tick every
@racket[n] (fractal) seconds; @racket[w] becomes the first ``current'' world.}

@defproc[(on-key-event [change (-> (unsyntax @tech{DrawKeyEvent}) (unsyntax @tech{DrawWorld}) (unsyntax @tech{DrawWorld}))])
true]{Adds @racket[change] to the world. The function reacts to keyboard
events and creates a new @racket[@#,tech{DrawWorld}].}

@defproc[(on-tick-event [tock (-> (unsyntax @tech{DrawWorld}) (unsyntax @tech{DrawWorld}))]) true]{Adds @racket[tock]
to the world. The function reacts to clock tick events, creating a new
current world.}

@defproc[(end-of-time) (unsyntax @tech{DrawWorld})]{Stops the world; returns the current world.} 
