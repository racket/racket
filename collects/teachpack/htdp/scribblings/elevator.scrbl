#lang scribble/doc

@(require scribble/manual "shared.rkt"
          (for-label racket teachpack/htdp/elevator))

@teachpack["elevator"]{Controlling an Elevator}

@;declare-exporting[teachpack/htdp/elevator]
@defmodule[#:require-form beginner-require htdp/elevator]

The teachpack implements an elevator simulator.

It displays an eight-floor elevator and accepts mouse clicks from the user,
which are translated into service demands for the elevator. 

@defproc[(run [NextFloor number?]) any/c]{Creates an elevator simulator
that is controlled by @racket[NextFloor]. This function consumes the
current floor, the direction in which the elevator is moving, and the
current demands. From that, it computes where  to send the elevator next.} 

Example: Define a function that consumes the current state of
the elevator (three arguments) and returns a number between 1 and 8. Here
is a non-sensical definition:

@racketblock[(define (controller x y z) 7)]

It moves the elevator once, to the 7th floor.

Second, set the teachpack to @filepath{elevator.rkt}, click Run, and
evaluate
@racketblock[(run controller)]
