#lang scribble/doc

@(require scribble/manual "shared.rkt" scribble/eval
          (for-label scheme
                     (only-in lang/htdp-beginner check-expect)
                     teachpack/2htdp/universe
                     2htdp/image))
@(require scribble/struct)

@(define note-scene @margin-note*{See @secref{scene} for @racket[scene?].})

@(define (table* . stuff)
  ;; (list paragraph paragraph) *-> Table
   (define (flow* x) (make-flow (list x)))
   (make-blockquote #f
     (list
       (make-table (make-with-attributes 'boxed
                     '((cellspacing . "6")))
         ;list
         (map (lambda (x) (map flow* x)) stuff)
         #;(map flow* (map car stuff))
         #;(map flow* (map cadr stuff))))))

@(define WorldState @tech[#:tag-prefixes '("world")]{WorldState})
@(define S-expression @tech[#:tag-prefixes '("universe")]{S-expression})

@; -----------------------------------------------------------------------------

@teachpack["universe"]{Worlds and the Universe}

@author{Matthias Felleisen}

@defmodule[#:require-form beginner-require 2htdp/universe #:use-sources (teachpack/2htdp/image)]

@;{FIXME: the following paragraph uses `defterm' instead of `deftech',
   because the words "world" and "universe" are used as datatypes, and
   datatypes are currently linked as technical terms --- which is a hack.
   Fix the paragraph when we have a better way to link datatype names.}

The @tt{universe.rkt} teachpack implements and provides the functionality
 for creating interactive, graphical programs that consist of plain
 mathematical functions. We refer to such programs as @deftech{world}
 programs. In addition, world programs can also become a part of a
 @deftech{universe}, a collection of worlds that can exchange messages.

The purpose of this documentation is to give experienced Racketeers and HtDP
 teachers a concise overview for using the library. The first part of the
 documentation focuses on @tech{world} programs. Section
 @secref["world-example"] presents an illustration of how to design such
 programs for a simple domain; it is suited for a novice who knows how to
 design conditional functions for enumerations, intervals, and unions. The
 second half of the documentation focuses on "universe" programs: how it is
 managed via a server, how @tech{world} programs register with the server,
 etc. The last two sections show how to design a simple universe of two
 communicating worlds.

@emph{Note}: For a quick and educational introduction to just worlds, see
 @link["http://www.ccs.neu.edu/home/matthias/HtDP2e/prologue.html"]{How
 to Design Programs, Second Edition: Prologue}. As of August 2008, we also
 have a series of projects available as a small booklet on
 @link["http://world.cs.brown.edu/"]{How to Design Worlds}.

@; -----------------------------------------------------------------------------
@section[#:tag "scene"]{Background}

The universe teachpack assumes working knowledge of the basic image
manipulation operations, either @racketmodname[htdp/image] or
@racketmodname[2htdp/image]. As far as this extended reference is
concerned, the major difference between the two image teachpacks is
the assumption that
@nested[#:style 'inset]{
  @racketmodname[htdp/image] programs render their state as @emph{scenes},
  i.e., images that satisfy the @racket[scene?] predicate.
}
Recall that @racketmodname[htdp/image] defines a scene to be an image whose
pinhole is at @math{(0,0)}. If your program uses the operations of
@racketmodname[2htdp/image], all images are also scenes.

While the operations of this teachpack work with both image teachpacks, we
hope to eliminate @racketmodname[htdp/image] in the not-too-distant future.
All example programs are already written using @racketmodname[2htdp/image]
operations. We urge programmers to use @racketmodname[2htdp/image] when
they design new ``world'' and ``universe'' programs and to rewrite their
existing @racketmodname[htdp/image] programs to use
@racketmodname[2htdp/image].

@; -----------------------------------------------------------------------------
@section[#:tag "simulations"]{Simple Simulations}

The simplest kind of animated @tech{world} program is a time-based
 simulation, which is a series of images.  The programmer's task is to
 supply a function that creates an image for each natural number. Handing
 this function to the teachpack displays the simulation.

@defproc[(animate [create-image (-> natural-number/c scene?)])
         natural-number/c]{
@note-scene
 opens a canvas and starts a clock that ticks 28 times per second.  Every
 time the clock ticks, DrRacket applies @racket[create-image] to the
 number of ticks passed since this function call. The results of these
 function calls are displayed in the canvas. The simulation runs until you
 click the @tt{Stop} button in DrRacket or close the window. At that
 point, @racket[animate] returns the number of ticks that have
 passed.
}

Example:
@racketblock[
(define (create-UFO-scene height)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 height UFO))

(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))

(animate create-UFO-scene)
]

@defproc[(run-simulation [create-image (-> natural-number/c scene?)])
         true]{
@note-scene
 @racket[animate] was originally called @racket[run-simulation], and this
 binding is retained for backwards compatibility}

@defproc[(run-movie [r (and/c real? positive?)] [m [Listof image?]])
         true]{

 @racket[run-movie] displays the list of images @racket[m], spending 
 @racket[r] seconds per image.}


@;-----------------------------------------------------------------------------
@section[#:tag "interactive" #:tag-prefix "world"]{Interactions}

The step from simulations to interactive programs is relatively
 small. Roughly speaking, a simulation designates one function,
 @racket[_create-image], as a handler for one kind of event: clock ticks.  In
 addition to clock ticks, @tech{world} programs can also deal with two
 other kinds of events: keyboard events and mouse events. A keyboard event
 is triggered when a computer user presses a key on the
 keyboard. Similarly, a mouse event is the  movement of the mouse, a click
 on a mouse button, the crossing of a boundary by a mouse movement, etc.

Your program may deal with such events via the @emph{designation} of
 @emph{handler} functions.  Specifically, the teachpack provides for the
 installation of four event handlers: @racket[on-tick], @racket[on-key],
 @racket[on-mouse], and @racket[on-pad]. In addition, a @tech{world}
 program must specify a
 @racket[draw] function, which is called every time your program should
 visualize the current world, and a @racket[done] predicate, which is used
 to determine when the @tech{world} program should shut down.

Each handler function consumes the current state of the @tech{world} and
 optionally a data representation of the event. It produces a new state of
 the @tech{world}.

The following picture provides an intuitive overview of the workings of a
 @tech{world} program in the form of a state transition diagram.

@image["nuworld.png"]

 The @racket[big-bang] form installs @racket[World_0] as the initial @tech{WorldState}.
 The handlers @racket[tock], @racket[react], and @racket[click] transform
 one world into another one; each time an event is handled, @racket[done] is
 used to check whether the world is final, in which case the program is
 shut down; and finally, @racket[draw] renders each world as an image, which
 is then displayed on an external canvas.

@deftech{WorldState} : @racket[any/c]

The design of a world program demands that you come up with a data
 definition of all possible states. We use @tech{WorldState} to refer to
 this collection of data, using a capital W to distinguish it from the
 program.  In principle, there are no constraints on this data
 definition though it mustn't be an instance of the @tech[#:tag-prefixes '("universe")]{Package}
 structure (see below). You can even keep it implicit, even if this
 violates the Design Recipe.

@defform/subs[#:id big-bang
              #:literals
              (on-tick to-draw on-draw on-key on-pad on-release on-mouse on-receive stop-when
              check-with register record? state name)
              (big-bang state-expr clause ...)
              ([clause
                 (on-tick tick-expr)
                 (on-tick tick-expr rate-expr)
                 (on-tick tick-expr rate-expr limit-expr)
                 (on-key key-expr)
                 (on-pad pad-expr)
                 (on-release release-expr)
                 (on-mouse mouse-expr)
                 (to-draw draw-expr)
                 (to-draw draw-expr width-expr height-expr)
                 (stop-when stop-expr) (stop-when stop-expr last-scene-expr)
                 (check-with world?-expr)
                 (record? r-expr)
                 (state expr)
                 (on-receive rec-expr)
                 (register IP-expr)
                 (port Port-expr)
                 (name name-expr)
                 ])]{

 starts a @tech{world} program in the initial state specified with
 @racket[state-expr], which must of course evaluate to an element of
 @tech{WorldState}.  Its behavior is specified via the handler functions
 designated in the optional @racket[spec] clauses, especially how the
 @tech{world} program deals with clock ticks, with key events, with mouse
 events, and eventually with messages from the universe; how it renders
 itself as an image; when the program must shut down; where to register the
 world with a universe; and whether to record the stream of events. A world
 specification may not contain more than one @racket[on-tick],
 @racket[to-draw], or @racket[register] clause. A @racket[big-bang]
 expression returns the last world when the stop condition is satisfied
 (see below) or when the programmer clicks on the @tt{Stop} button or
 closes the canvas.
}

The only mandatory clause of a @racket[big-bang] description is
@racket[to-draw] (or @racket[on-draw] for backwards compatibility):
@itemize[

@item{

@defform[(to-draw render-expr)
         #:contracts
         ([render-expr (-> (unsyntax @tech{WorldState}) scene?)])]{
@note-scene
 tells DrRacket to call the function @racket[render-expr] whenever the
 canvas must be drawn. The external canvas is usually re-drawn after DrRacket has
 dealt with an event. Its size is determined by the size of the first
 generated image.}

@defform/none[#:literals (to-draw)
              (to-draw render-expr width-expr height-expr)
              #:contracts
              ([render-expr (-> (unsyntax @tech{WorldState}) scene?)]
               [width-expr natural-number/c]
               [height-expr natural-number/c])]{
@note-scene
 tells DrRacket to use a @racket[width-expr] by @racket[height-expr]
 canvas instead of one determine by the first generated image.
}

For compatibility reasons, the teachpack also supports the keyword
@defidform/inline[on-draw] in lieu of @racket[to-draw] but the latter is preferred
now.
}

]
All remaining clauses are optional. To introduce them, we need one more
data definition: 

@deftech{HandlerResult} : is a synonym for @tech{WorldState} until @secref[#:tag-prefixes '("universe")]{world2}

@itemize[

@item{
@defform[(on-tick tick-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{

tells DrRacket to call the @racket[tick-expr] function on the current
world every time the clock ticks. The result of the call becomes the
current world. The clock ticks at the rate of 28 times per second.}}

@item{
@defform/none[#:literals(on-tick)
              (on-tick tick-expr rate-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))]
               [rate-expr (and/c real? positive?)])]{
tells DrRacket to call the @racket[tick-expr] function on the current
world every time the clock ticks. The result of the call becomes the
current world. The clock ticks every @racket[rate-expr] seconds.}}

@item{
@defform/none[#:literals(on-tick)
              (on-tick tick-expr rate-expr limit-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))]
               [rate-expr (and/c real? positive?)]
               [limit-expr (and/c integer? positive?)])]{
tells DrRacket to call the @racket[tick-expr] function on the current
world every time the clock ticks. The result of the call becomes the
current world. The clock ticks every @racket[rate-expr] seconds.
The world ends when the clock has ticked more than @scheme[limit-expr] times.}}

@item{A @tech{KeyEvent} represents key board events.

@deftech{KeyEvent} : @racket[string?]

For simplicity, we represent key events with strings, but not all strings
 are key events. The representation of key events comes in distinct
 classes. First, a single-character string is used to signal that the user
 has hit a "regular" key, such as 
@itemize[

@item{@racket["q"] stands for the q key;}
@item{@racket["w"] stands for the w key;}
@item{@racket["e"] stands for the e key;}
@item{@racket["r"] stands for the r key; and so on.}
]
 Some of these one-character strings look somewhat unusual:
@itemize[

@item{@racket[" "] stands for the space bar (@racket[#\space]);}
@item{@racket["\r"] stands for the return and enter key (@racket[#\return]);}
@item{@racket["\t"] stands for the tab key (@racket[#\tab]); and}
@item{@racket["\b"] stands for the backspace key (@racket[#\backspace]).}
]
 Here is "proof" that these strings really have length 1: 
@interaction[
(string-length "\t")
]
 On rare occasions your programs may also encounter @racket["\u007F"],
 which is the string representing the delete key (aka rubout).

Second, some keys have multiple-character string representations. Strings
 with more than one character denote arrow keys or other special events,
 starting with the four most important ones:
@itemize[
@item{@racket["left"] is the left arrow;}
@item{@racket["right"] is the right arrow;}
@item{@racket["up"] is the up arrow;}
@item{@racket["down"] is the down arrow;}
]
Here are some others that you may encounter: 
@itemize[
@item{@racket["start"]}
@item{@racket["cancel"]}
@item{@racket["clear"]}
@item{@racket["shift"]}
@item{@racket["rshift"]}
@item{@racket["control"]}
@item{@racket["rcontrol"]}
@item{@racket["menu"]}
@item{@racket["pause"]}
@item{@racket["capital"]}
@item{@racket["prior"]}
@item{@racket["next"]}
@item{@racket["end"]}
@item{@racket["home"]}
@item{@racket["escape"]}
@item{@racket["select"]}
@item{@racket["print"]}
@item{@racket["execute"]}
@item{@racket["snapshot"]}
@item{@racket["insert"]}
@item{@racket["help"]}
 @;item{@racket["numpad0"],
 @;racket["numpad1"],
 @;racket["numpad2"],
 @;racket["numpad3"],
 @;racket["numpad4"],
 @;racket["numpad5"],
 @;racket["numpad6"],
 @;racket["numpad7"],
 @;racket["numpad8"],
 @;racket["numpad9"],
 @;racket["numpad-enter"],
 @;racket["multiply"],
 @;racket["add"],
 @;racket["separator"],
 @;racket["subtract"],
 @;racket["decimal"],
 @;racket["divide"]}
@item{function keys: 
 @racket["f1"],
 @racket["f2"],
 @racket["f3"],
 @racket["f4"],
 @racket["f5"],
 @racket["f6"],
 @racket["f7"],
 @racket["f8"],
 @racket["f9"],
 @racket["f10"],
 @racket["f11"],
 @racket["f12"],
 @racket["f13"],
 @racket["f14"],
 @racket["f15"],
 @racket["f16"],
 @racket["f17"],
 @racket["f18"],
 @racket["f19"],
 @racket["f20"],
 @racket["f21"],
 @racket["f22"],
 @racket["f23"],
 @racket["f24"]}
@item{@racket["numlock"]}
@item{@racket["scroll"]}]

The following four count as keyevents even though they are triggered by
physical events on some form of mouse: 
@itemize[
@item{@racket["wheel-up"]}
@item{@racket["wheel-down"]}
@item{@racket["wheel-left"]}
@item{@racket["wheel-right"]}
]
 The preceding enumeration is neither complete in covering all the events
 that this library deals with nor does it specify which events the library
 ignores. If you wish to design a program that relies on specific keys on
 your keyboard, you should first write a small test program to find out
 whether the chosen keystrokes are caught by the library and, if so, which
 string representations are used for these events. 

@defproc[(key-event? [x any]) boolean?]{
 determines whether @racket[x] is a @tech{KeyEvent}}

@defproc[(key=? [x key-event?][y key-event?]) boolean?]{
 compares two @tech{KeyEvent} for equality}

@defform[(on-key key-expr)
         #:contracts
          ([key-expr (-> (unsyntax @tech{WorldState}) key-event? (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 tells DrRacket to call the @racket[key-expr] function on the current world and a
 @tech{KeyEvent} for every keystroke the user of the computer makes. The result
 of the call becomes the current world.

 Here is a typical key-event handler:
@racketblock[
(define (change w a-key)
  (cond
    [(key=? a-key "left")  (world-go w -DELTA)]
    [(key=? a-key "right") (world-go w +DELTA)]
    [(= (string-length a-key) 1) w] (code:comment "order-free checking")
    [(key=? a-key "up")    (world-go w -DELTA)]
    [(key=? a-key "down")  (world-go w +DELTA)]
    [else w]))
]
 }
 The omitted, auxiliary function @emph{world-go} is supposed to consume a
 world and a number and produces a world.

@defform[(on-release release-expr)
         #:contracts
          ([release-expr (-> (unsyntax @tech{WorldState}) key-event? (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 tells DrRacket to call the @racket[release-expr] function on the current world and a
 @tech{KeyEvent} for every release event on the keyboard. A release event
 occurs when a user presses the key and then releases it. The second argument
 indicates which key has been released. The result of the function call
 becomes the current world.
}
}

@item{A @tech{PadEvent} is a @tech{KeyEvent} for a game-pad simulation via
@racket[big-bang]. The presence of an @racket[on-pad] clause superimposes
the game-pad image onto the current image, suitably scaled to its size:

@image["gamepad.png"]

@deftech{PadEvent} : @racket[key-event?]

It is one of the following:
@itemize[
@item{@racket["left"] is the left arrow;}
@item{@racket["right"] is the right arrow;}
@item{@racket["up"] is the up arrow;}
@item{@racket["down"] is the down arrow;}
@item{@racket["w"] to be interpreted as up arrow;}
@item{@racket["s"] to be interpreted as down arrow;}
@item{@racket["a"] to be interpreted as left arrow;}
@item{@racket["d"] to be interpreted as right arrow;}
@item{@racket[" "] is the space bar;}
@item{@racket["shift"] is the left shift key;}
@item{@racket["rshift"] is the right shift key;}
]

@defproc[(pad-event? [x any]) boolean?]{
 determines whether @racket[x] is a @tech{PadEvent}}

@defproc[(pad=? [x pad-event?][y pad-event?]) boolean?]{
 compares two @tech{PadEvent} for equality}

@defform[(on-pad pad-expr)
         #:contracts
          ([pad-expr (-> (unsyntax @tech{WorldState}) pad-event? (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 tells DrRacket to call the @racket[pad-expr] function on the current world and the
 @tech{KeyEvent} for every keystroke that is also a @tech{PadEvent}. The result
 of the call becomes the current world.

 Here is a typical @tech{PadEvent} handler:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; ComplexNumber PadEvent -> ComplexNumber
(define (handle-pad-events x k)
  (case (string->symbol k)
    [(up    w)      (- x 0+10i)]
    [(down  s)      (+ x 0+10i)]
    [(left  a)      (- x 10)]
    [(right d)      (+ x 10)]
    [(| |)          x0]
    [(shift)        (conjugate x)]
    [(rshift)       (stop-with (conjugate x))]))

))
@;%

 }

When a @racket[big-bang] expression specifies an @racket[on-pad] clause,
all @tech{PadEvent}s are sent to the @racket[on-pad] handler. All other
key events are discarded, unless an @racket[on-key] and/or an
@racket[on-release] clause are specified, in which case all remaining
@tech{KeyEvent}s are sent there.

To facilitate the definition of @racket[on-pad] handlers, the library
provides the @racket[pad-handler] form.

@defform/subs[#:id pad-handler
              #:literals
              (up down left right space shift)
              (pad-handler clause ...)
              ([clause
                 (up up-expr)
                 (down down-expr)
                 (left left-expr)
                 (right right-expr)
                 (space space-expr)
                 (shift shift-expr)])]{
 Creates a function that deals with @tech{PadEvent}s. Each (optional) clause
 contributes one function that consumes a @tech{World} and produces a
 world. The name of the clause determines for which kind of @tech{PadEvent}
 the function is called.

 Using the form is entirely optional and not required to use
 @racket[on-pad]. Indeed, @racket[pad-handler] could be used to define a
 plain @tech{KeyEvent} handler---if we could guarantee that players never
 hit keys other than @tech{PadEvent} keys.
}

All clauses in a @racket[pad-handler] form are optional:
@itemize[

@item{
@defform[(up up-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 Creates a handler for @racket["up"] and @racket["w"] events.}
}

@item{
@defform[(down down-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 Creates a handler for @racket["down"] and @racket["s"] events.}
}

@item{
@defform[(left left-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 Creates a handler for @racket["left"] and @racket["a"] events.}
}

@item{
@defform[(right right-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 Creates a handler for @racket["right"] and @racket["d"] events.}
}

@item{
@defform[(space space-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 Creates a handler for space-bar events (@racket[" "]).}
}

@item{
@defform[(shift shift-expr)
         #:contracts
         ([tick-expr (-> (unsyntax @tech{WorldState}) (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 Creates a handler for @racket["shift"] and @racket["rshift"] events.}
}

]
 If a clause is omitted, @racket[pad-handler] installs a default function
 that maps the existing world to itself.

 Here is a @tech{PadEvent} handler defined with @racket[pad-handler]:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; ComplexNumber -> ComplexNumber
(define (i-sub1 x) (- x 0+1i))

;; ComplexNumber -> ComplexNumber
(define (i-add1 x) (+ x 0+1i))

;; ComplexNumber -> ComplexNumber
;; deal with all @tech{PadEvent}s
(define handler
  (pad-handler (left sub1) (right add1)
               (up i-sub1) (down i-add1)
               (shift (lambda (w) 0))
               (space stop-with)))

;; some tests:
(check-expect (handler 9 "left") 8)
(check-expect (handler 8 "up")   8-i)
))
@;%
}

@item{ A @tech{MouseEvent} represents mouse events, e.g., mouse movements
 or mouse clicks, by the computer's user.

@deftech{MouseEvent} : @racket[(one-of/c "button-down" "button-up" "drag" "move" "enter" "leave")]

All @tech{MouseEvent}s are represented via strings:
@itemize[

@item{@racket["button-down"]
 signals that the computer user has pushed a mouse button down;}
@item{@racket["button-up"]
 signals that the computer user has let go of a mouse button;}
@item{@racket["drag"]
 signals that the computer user is dragging the mouse. A dragging event
 occurs when the mouse moves while a mouse button is pressed.}
@item{@racket["move"]
 signals that the computer user has moved the mouse;}
@item{@racket["enter"]
 signals that the computer user has moved the mouse into the canvas area; and}
@item{@racket["leave"]
 signals that the computer user has moved the mouse out of the canvas area.}
]

@defproc[(mouse-event? [x any]) boolean?]{
 determines whether @racket[x] is a @tech{MouseEvent}}

@defproc[(mouse=? [x mouse-event?][y mouse-event?]) boolean?]{
 compares two @tech{MouseEvent}s for equality}

@defform[(on-mouse mouse-expr)
         #:contracts
         ([mouse-expr
           (-> (unsyntax @tech{WorldState})
               integer? integer? (unsyntax @tech{MouseEvent})
               (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult}))])]{
 tells DrRacket to call @racket[mouse-expr] on the current world, the current
 @racket[x] and @racket[y] coordinates of the mouse, and a
 @tech{MouseEvent} for every (noticeable) action of the mouse by the
 computer user. The result of the call becomes the current world.

 For @racket["leave"] and @racket["enter"] events, the coordinates of the
 mouse click may be outside of the (implicit) rectangle. That is, the
 coordinates may be negative or larger than the (implicitly) specified
 width and height.

 @bold{Note 1}: the operating system doesn't really notice every single movement
 of the mouse (across the mouse pad). Instead it samples the movements and
 signals most of them.}

 @bold{Note 2}: while mouse events are usually reported in the expected
 manner, the operating system doesn't necessarily report them in the
 expected order. For example, the Windows operating system insists on
 signaling a @racket["move"] event immediately after a @racket["button-up"]
 event is discovered.  Programmers must design the @racket[on-mouse]
 handler to handle any possible mouse event at any moment.  }

@item{

@defform[(stop-when last-world?)
         #:contracts
         ([last-world? (-> (unsyntax @tech{WorldState}) boolean?)])]{
 tells DrRacket to call the @racket[last-world?] function whenever the canvas is
 drawn. If this call produces @racket[true], the world program is shut
 down. Specifically, the  clock is stopped; no more
 tick events, @tech{KeyEvent}s, or @tech{MouseEvent}s are forwarded to
 the respective handlers. The @racket[big-bang] expression returns this
 last world.
}

@defform/none[#:literals (stop-when)
         (stop-when last-world? last-picture)
         #:contracts
         ([last-world? (-> (unsyntax @tech{WorldState}) boolean?)]
          [last-picture (-> (unsyntax @tech{WorldState}) scene?)])]{
@note-scene
 tells DrRacket to call the @racket[last-world?] function whenever the canvas is
 drawn. If this call produces @racket[true], the world program is shut
 down after displaying the world one last time, this time using the image
 rendered with @racket[last-picture]. Specifically, the  clock is stopped; no more
 tick events, @tech{KeyEvent}s, or @tech{MouseEvent}s are forwarded to
 the respective handlers. The @racket[big-bang] expression returns this
 last world.
}
}

@item{

@defstruct*[stop-with ([w (unsyntax @tech[#:tag-prefixes '("world")]{HandlerResult})])]{signals to
DrRacket that the world program should shut down. That is, any
handler may return @racket[(stop-with w)] provided @racket[w] is a
@tech[#:tag-prefixes '("world")]{HandlerResult}. If it does, the state of the world becomes @racket[w]
and @racket[big-bang] will close down all event handling.}

}

@item{

@defform[#:literals (check-with)
         (check-with world-expr?)
         #:contracts
         ([world-expr? (-> Any boolean?)])]{
 tells DrRacket to call the @racket[world-expr?] function on the result of
 every world handler call. If this call produces @racket[true], the result
 is considered a world; otherwise the world program signals an error.
}}

@item{

@defform[#:literals (record?)
         (record? r-expr)
         #:contracts
         ([r-expr any/c])]{
 tells DrRacket to enable a visual replay of the interaction,
 unless @racket[#f].
 The replay action generates one png image per image and
 an animated gif for the entire sequence in the directory of the user's
 choice.  If @racket[r-expr] evaluates to the name of an existing
 directory/folder (in the local directory/folder), the directory is used to
 deposit the images.
}}

@item{

@defform[#:literals (state) (state expr)]{
 if not @racket[#f], DrRacket opens a separate window in which the current
 state is rendered each time it is updated. This is useful for beginners
 who wish to see how their world evolves---without having to design a
 rendering function---plus for the debugging of world programs.
}}

@item{
@defform[(name name-expr)
         #:contracts
         ([name-expr (or/c symbol? string?)])]{
 provide a name (@racket[namer-expr]) to this world, which is used as the
 title of the canvas.}
}

]

The following example shows that @racket[(run-simulation create-UFO-scene)] is
a short-hand for three lines of code:

@(begin
#reader scribble/comment-reader
@racketblock[
(define (create-UFO-scene height)
  (underlay/xy (rectangle 100 100 "solid" "white") 50 height UFO))

(define UFO
  (underlay/align "center"
                  "center"
                  (circle 10 "solid" "green")
                  (rectangle 40 4 "solid" "green")))

;; (run-simulation create-UFO-scene) is short for:
(big-bang 0
          (on-tick add1)
          (to-draw create-UFO-scene))
])

Exercise: Add a condition for stopping the flight of the UFO when it
reaches the bottom.


@; -----------------------------------------------------------------------------
@section[#:tag "world-example"]{A First Sample World}

This section uses a simple example to explain the design of worlds. The
 first subsection introduces the sample domain, a door that closes
 automatically. The second subsection is about the design of @tech{world}
 programs in general, the remaining subsections implement a simulation of
 the door.

@subsection{Understanding a Door}

Say we wish to design a @tech{world} program that simulates the working of
 a door with an automatic door closer. If this kind of door is locked, you
 can unlock it with a key. While this doesn't open the door per se, it is
 now possible to do so. That is, an unlocked door is closed and pushing at
 the door opens it. Once you have passed through the door and you let go,
 the automatic door closer takes over and closes the door again. When a
 door is closed, you can lock it again.

Here is a diagram that translates our words into a graphical
 representation:

@image["door-real.png"]

Like the picture of the general workings of a @tech{world} program, this
 diagram displays a so-called ``state machine.'' The three circled words are
 the states that our informal description of the door identified: locked,
 closed (and unlocked), and open. The arrows specify how the door can go
 from one state into another. For example, when the door is open, the
 automatic door closer shuts the door as time passes. This transition is
 indicated by the arrow labeled ``time.'' The other arrows represent
 transitions in a similar manner:

@itemize[

@item{``push'' means a person pushes the door open (and let's go);}

@item{``lock'' refers to the act of inserting a key into the lock and turning
it to the locked position; and}

@item{``unlock'' is the opposite of ``lock.''}

]

@; -----------------------------------------------------------------------------
@subsection{Hints on Designing Worlds}

Simulating any dynamic behavior via a @tech{world} program demands two
 different activities. First, we must tease out those portions of our
 domain that change over time or in reaction to actions, and we must
 develop a data representation for this information.  This is what we call
 @|WorldState|. Keep in
 mind that a good data definition makes it easy for readers to map data to
 information in the real world and vice versa. For all others aspects of
 the world, we use global constants, including graphical or visual
 constants that are used in conjunction with the rendering operations.

Second, we must translate the actions in our domain---the arrows in the
 above diagram---into interactions with the computer that the universe
 teachpack can deal with. Once we have decided to use the passing of time
 for one aspect, key presses for another, and mouse movements for a third,
 we must develop functions that map the current state of the
 world---represented as data from @|WorldState|---into the next state of the
 world. Put differently, we have just created a wish list with three
 handler functions that have the following general contract and purpose
 statements:

@(begin
#reader scribble/comment-reader
(racketblock
;; tick : WorldState -> HandlerResult
;; deal with the passing of time
(define (tick w) ...)

;; click : WorldState @emph{Number} @emph{Number} @tech{MouseEvent} -> @tech[#:tag-prefixes '("world")]{HandlerResult}
;; deal with a mouse click at @emph{(x,y)} of kind @emph{me}
;; in the current world @emph{w}
(define (click w x y me) ...)

;; control : WorldState @tech{KeyEvent} -> @tech[#:tag-prefixes '("world")]{HandlerResult}
;; deal with a key event @emph{ke}
;; in the current world @emph{w}
(define (control w ke) ...)
))

That is, the contracts of the various handler designations dictate what the
 contracts of our functions are, once we have defined how to represent the
 domain with data in our chosen language.

A typical program does not use all three of these functions. Furthermore,
 the design of these functions provides only the top-level, initial design
 goal. It often demands the design of many auxiliary functions. The
 collection of all these functions is your @tech{world} program.

@centerline{An extended example is available in
 @link["http://www.ccs.neu.edu/home/matthias/HtDP2e/"]{How to Design Programs/2e}.}

@; -----------------------------------------------------------------------------
@section[#:tag "world2" #:tag-prefix "universe"]{The World is not Enough}

The library facilities covered so far are about designing individual
 programs with interactive graphical user interfaces (simulations,
 animations, games, etc.). In this section, we introduce capabilities for
 designing a distributed program, which is really a number of programs that
 coordinate their actions in some fashion. Each of the individual programs
 may run on any computer in the world (as in our planet and the spacecrafts
 that we sent out), as long as it is on the internet and as long as the
 computer allows the program to send and receive messages (via TCP). We
 call this arrangement a @tech{universe} and the program that coordinates
 it all a @emph{universe server} or just @tech{server}.

This section explains what messages are, how to send them from a
 @tech{world} program, how to receive them, and how to connect a
 @tech{world} program to a @tech{universe}.

@; -----------------------------------------------------------------------------

@subsection{Messages}

After a world program has become a part of a universe, it may send messages
 and receive them. In terms of data, a message is just an
 @tech{S-expression}.

@deftech{S-expression} An S-expression is roughly a nested list of basic
data; to be precise, an S-expression is one of:

@itemize[
 @item{a string,}
 @item{a symbol,}
 @item{a number,}
 @item{a boolean,}
 @item{a char, or}
 @item{a list of S-expressions, or}
 @item{a prefab struct of S-expressions.}
]
Note the @racket[list] clause includes @racket[empty] of course.

@defproc[(sexp? [x any/c]) boolean?]{
 determines whether @racket[x] is an @tech{S-expression}.}

@subsection{Sending Messages}

Each world-producing callback in a world program---those for handling clock
 tick events, keyboard events, and mouse events---may produce a
 @tech{Package} in addition to just a @|WorldState|:

@deftech{HandlerResult} is one of the following:
@itemize[
@item{@|WorldState|}
@item{@tech{Package}}
]
 where @deftech{Package} represents a pair consisting of a @|WorldState|
 and a message from a @tech{world} program to the @tech{server}.  Because
 programs send messages via @tech{Package}, the teachpack does not
 provide the selectors for the structure, only the constructor and a
 predicate.

@defproc[(package? [x any/c]) boolean?]{
 determine whether @racket[x] is a @tech{Package}.}

@defproc[(make-package [w any/c][m sexp?]) package?]{
 create a @tech{Package} from a @|WorldState| and an @tech{S-expression}.}

Recall that event handlers return a @tech[#:tag-prefixes'("universe")]{HandlerResult},
 and we have just refined this data definition. Hence, each handler may return either a
 @|WorldState| or a @tech{Package}.  If an event handler produces a @tech{Package},
 the content of the world field becomes the next world and the message field specifies
 what the world sends to the universe. This distinction also explains why the data
 definition for @|WorldState| may not include a @tech{Package}.

@subsection{Connecting with the Universe}

Messages are sent to the universe program, which runs on some computer in
 the world. The next section is about constructs for creating such a universe
 server. For now, we just need to know that it exists and that it is the recipient
 of messages.

@deftech{IP} @racket[string?]

Before a world program can send messages, it must register with the
 server. Registration must specify the internet address of the computer on which
 the server runs, also known as an @tech{IP} address or a host.  Here a
 @tech{IP} address is a string of the right shape, e.g., @racket["192.168.1.1"]
 or @racket["www.google.com"].

@defthing[LOCALHOST string?]{the @tech{IP} of your computer. Use it while you
 are developing a distributed program, especially while you are
 investigating whether the participating world programs collaborate in an
 appropriate manner. This is called @emph{integration testing} and differs
 from unit testing quite a bit.}

A @racket[big-bang] description of a world program that wishes to communicate
with other programs must contain a @racket[register] clause of one of the
following shapes:

@itemize[

@item{
@defform[(register ip-expr) #:contracts ([ip-expr string?])]{
 connect this world to a universe server at the specified @racket[ip-expr]
 address and set up capabilities for sending and receiving messages.
 If the world description includes a name specification of the form
 @racket[(name SomeString)] or @racket[(name SomeSymbol)], the name of the
 world is sent along to the server.
}}

@item{
@defform[(port port-expr) #:contracts ([port-expr natural-number/c])]{
 specifies port on which a world wishes to receive and send messages. A
 port number is an integer between @racket[0] and @racket[65536].
}}
]

When a world program registers with a universe program and the universe program
stops working, the world program stops working, too.

@subsection{Receiving Messages}

Finally, the receipt of a message from the server is an event, just like
 tick events, keyboard events, and mouse events. Dealing with the receipt of a
 message works exactly like dealing with any other event. DrRacket
 applies the event handler that the world program specifies; if there is no
 clause, the message is discarded.

The @racket[on-receive] clause of a @racket[big-bang] specifies the event handler
 for message receipts.

@defform[(on-receive receive-expr)
         #:contracts
         ([receive-expr (-> (unsyntax @|WorldState|) sexp? (unsyntax @tech[#:tag-prefixes '("universe")]{HandlerResult}))])]{
 tells DrRacket to call @racket[receive-expr] for every message receipt, on the current
 @|WorldState| and the received message. The result of the call becomes the current
 @|WorldState|.

 Because @racket[receive-expr] is (or evaluates to) a world-transforming
 function, it too can produce a @tech{Package} instead of just a
 @|WorldState|. If the result is a @tech{Package}, its message content is
 sent to the @tech{server}.}

The diagram below summarizes the extensions of this section in graphical form.

@image["universe.png"]

A registered world program may send a message to the universe server
 at any time by returning a @tech{Package} from an event handler. The
 message is transmitted to the server, which may forward it to some
 other world program as given or in some massaged form. The arrival of a
 message is just another event that a world program must deal with. Like
 all other event handlers @emph{receive} accepts a @|WorldState| and some
 auxiliary arguments (a message in this case) and produces a
 @|WorldState| or a @tech{Package}.

When messages are sent from any of the worlds to the universe or vice versa,
 there is no need for the sender and receiver to synchronize. Indeed, a sender
 may dispatch as many messages as needed without regard to whether the
 receiver has processed them yet. The messages simply wait in queue until
 the receiving @tech{server} or @tech{world} program takes care of them.

@; -----------------------------------------------------------------------------
@section[#:tag "universe-server"]{The Universe Server}

A @deftech{server} is the central control program of a @tech{universe} and
 deals with receiving and sending of messages between the world
 programs that participate in the @tech{universe}. Like a @tech{world}
 program, a server is a program that reacts to events, though to different
 events than @tech{world}s. The two primary kinds of events are the
 appearance of a new @tech{world} program in the @tech{universe}
 and the receipt of a message from a @tech{world} program.

The teachpack provides a mechanism for designating event handlers for
 servers that is quite similar to the mechanism for describing @tech{world}
 programs. Depending on the designated event handlers, the server takes on
 distinct roles:

@itemize[

@item{A server may be a ``pass through'' channel between two worlds, in which case
 it has no other function than to communicate whatever message it receives
 from one world to the other, without any interference.}

@item{A server may enforce a ``back and forth'' protocol, i.e., it may force two
 (or more) worlds to engage in a civilized tit-for-tat exchange. Each
 world is given a chance to send a message and must then wait
 to get a reply before it sends anything again.}

@item{A server may play the role of a special-purpose arbiter, e.g., the referee
 or administrator of a game. It may check that each world ``plays'' by the rules,
 and it administrates the resources of the game.}

]

As a matter of fact, a pass-through @tech{server} can become basically
invisible, making it appear as if all communication goes from peer
@tech{world} to peer in a @tech{universe}.

This section first introduces some basic forms of data that the
 @tech{server} uses to represent @tech{world}s and other matters. Second,
 it explains how to describe a server program.

@; -----------------------------------------------------------------------------
@subsection{Worlds and Messages}

Understanding the server's event handling functions demands several data
 representations: that of (a connection to) a @tech{world} program and that
 of a response of a handler to an event.

@itemize[

@item{The @tech{server} and its event handlers must agree on a
 data representation of the @tech{world}s that participate in the
 universe.

@defproc[(iworld? [x any/c]) boolean?]{
 determines whether @racket[x] is an @emph{iworld}. Because the universe server
 represents worlds via structures that collect essential information about
 the connections, the teachpack does not export any constructor or selector
 functions on worlds.}

@defproc[(iworld=? [u iworld?][v iworld?]) boolean?]{
 compares two @emph{iworld}s for equality.}

@defproc[(iworld-name [w iworld?]) string?]{
 extracts the name from a @emph{iworld} structure.}

@defthing[iworld1 iworld?]{an @emph{iworld} for testing your programs}
@defthing[iworld2 iworld?]{another iworld for testing your programs}
@defthing[iworld3 iworld?]{and a third one}

The three sample iworlds are provided so that you can test your functions
for universe programs. For example:

@racketblock[
(check-expect (iworld=? iworld1 iworld2) false)
(check-expect (iworld=? iworld2 iworld2) true)
]
}

@item{Each event handler produces a state of the universe or a @emph{bundle}, which is a structure
 that contains the @tech{server}'s state, a list of mails to other worlds,
 and the list of @emph{iworld}s that are to be disconnected.

@defproc[(bundle? [x any/c]) boolean?]{
 determines whether @racket[x] is a @emph{bundle}.}

@defproc[(make-bundle [state any/c] [mails (listof mail?)] [low (listof iworld?)]) bundle?]{
 creates a @emph{bundle} from a piece of data that
 represents a server state, a list of mails, and a list of iworlds.}

If disconnecting from these worlds results in an empty list of
participants, the universe server is restarted in the initial state.

A @emph{mail} represents a message from an event handler to a world. The
teachpack provides only a predicate and a constructor for these structures:

@defproc[(mail? [x any/c]) boolean?]{
 determines whether @racket[x] is a @emph{mail}.}

@defproc[(make-mail [to iworld?] [content sexp?]) mail?]{
 creates a @emph{mail} from a @emph{iworld} and an @|S-expression|.}
}
]

@; -----------------------------------------------------------------------------
@subsection{Universe Descriptions}

A @tech{server} keeps track of information about the @tech{universe} that
 it manages. One kind of tracked information is obviously the collection of
 participating world programs, but in general the kind of information that
 a server tracks and how the information is represented depends on the
 situation and the programmer, just as with @tech{world} programs.

@deftech{UniverseState} : @racket[any/c] 

The design of a universe server demands that you come up with a data
definition for all possible server states. For running
@tech{universe}s, the teachpack demands that you come up with a data
definition for (your state of the) @tech{server}.  Any piece of data can
represent the state. We just assume that you introduce a data definition
for the possible states and that your event handlers are designed
according to the design recipe for this data definition.

The @tech{server} itself is created with a description that includes the
 first state and a number of clauses that specify functions for dealing
 with @tech{universe} events.

@defform/subs[#:id universe
              #:literals
              (on-new on-msg on-tick on-disconnect to-string check-with port state)
              (universe state-expr clause ...)
              ([clause
                 (on-new new-expr)
                 (on-msg msg-expr)
                 (on-tick tick-expr)
                 (on-tick tick-expr rate-expr)
                 (on-tick tick-expr rate-expr limit-expr)
                 (on-disconnect dis-expr)
                 (state expr)
                 (to-string render-expr)
		 (port port-expr)
                 (check-with universe?-expr)
                 ])]{

creates a server with a given state, @racket[state-expr]. The
behavior is specified via handler functions through mandatory and optional
@emph{clause}s. These functions govern how the server deals with the
registration of new worlds, how it disconnects worlds, how it sends
messages from one world to the rest of the registered worlds, and how it
renders its current state as a string.}

Evaluating a @racket[universe] expression starts a server. Visually it opens
 a console window on which you can see that worlds join, which messages are
 received from which world, and which messages are sent to which world. For
 convenience, the console also has two buttons: one for shutting down a
 universe and another one for re-starting it. The latter functionality is
 especially useful during the integration of the various pieces of a
 distributed program.

The mandatory clauses of a @racket[universe] server description are
@racket[on-new] and @racket[on-msg]:

@itemize[

@item{
 @defform[(on-new new-expr)
          #:contracts
          ([new-expr (-> (unsyntax @tech{UniverseState}) iworld? (or/c (unsyntax @tech{UniverseState}) bundle?))])]{
 tells DrRacket to call the function @racket[new-expr] every time another world joins the
 universe. The event handler is called with the current state and the
 joining iworld, which isn't on the list yet. In particular, the handler may
 reject a @tech{world} program from participating in a @tech{universe},
 by simply returning the given state or by immediately including the new world in the third field of the resulting @racket[bundle] structure.

@history[
 #:changed 
 "1.1" 
 "allow universe handlers to return a plain universe state"]

}}

@item{
 @defform[(on-msg msg-expr)
          #:contracts
          ([msg-expr (-> (unsyntax @tech{UniverseState}) iworld? sexp? (or/c (unsyntax @tech{UniverseState}) bundle?))])]{
 tells DrRacket to apply @racket[msg-expr] to the current state of the
 universe, the world
 @racket[w] that sent the message, and the message itself.

@history[
 #:changed 
 "1.1" 
 "allow universe handlers to return a plain universe state"]

}

}]
 All proper event handlers produce a state of the universe or a
 @emph{bundle}.  The state of the universe is safe-guarded by the server until the next event, and the mails
 are broadcast as specified.  The list of iworlds in the third field of the
 bundle is removed from the list of participants from which to expect
 messages.

The following picture provides a graphical overview of the server's workings.

@; -----------------------------------------------------------------------------
@;; THE PICTURE IS WRONG
@; -----------------------------------------------------------------------------

@image["server.png"]

In addition to the mandatory handlers, a program may wish to add some
optional handlers:

@itemize[

@item{
@defform/none[#:literals (on-tick)
              (on-tick tick-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{UniverseState}) (or/c (unsyntax @tech{UniverseState}) bundle?))])]{
 tells DrRacket to apply @racket[tick-expr] to the current state of the
 universe.

@history[
 #:changed 
 "1.1" 
 "allow universe handlers to return a plain universe state"]
}

@defform/none[#:literals (on-tick)
              (on-tick tick-expr rate-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{UniverseState}) (or/c (unsyntax @tech{UniverseState}) bundle?))]
               [rate-expr (and/c real? positive?)])]{
 tells DrRacket to apply @racket[tick-expr] as above; the clock ticks
 every  @racket[rate-expr] seconds.

@history[
 #:changed 
 "1.1" 
 "allow universe handlers to return a plain universe state"]

}

@defform/none[#:literals (on-tick)
              (on-tick tick-expr rate-expr)
              #:contracts
              ([tick-expr (-> (unsyntax @tech{UniverseState}) (or/c (unsyntax @tech{UniverseState}) bundle?))]
               [rate-expr (and/c real? positive?)]
               [limit-expr (and/c integer? positive?)])]{
 tells DrRacket to apply @racket[tick-expr] as above; the clock ticks
 every  @racket[rate-expr] seconds. The universe stops when the clock has
 ticked more than @scheme[limit-expr] times.

@history[
 #:changed 
 "1.1" 
 "allow universe handlers to return a plain universe state"]
}
}

@item{
 @defform[#:literals (on-disconnect)
	  (on-disconnect dis-expr)
          #:contracts
          ([dis-expr (-> (unsyntax @tech{UniverseState}) iworld? (or/c (unsyntax @tech{UniverseState}) bundle?))])]{
 tells DrRacket to invoke @racket[dis-expr] every time a participating
 @tech{world} drops its connection to the server. The first argument
 is the current state of the universe server, while the second argument is
 the (representation of the) world that got disconnected. The resulting
 bundle usually includes this second argument in the third field, telling
 DrRacket not to wait for messages from this world anymore.

@history[
 #:changed 
 "1.1" 
 "allow universe handlers to return a plain universe state"]
}
}

@item{
@defform/none[#:literals (port)
              (port port-expr) 
	      #:contracts 
	      ([port-expr natural-number/c])]{
 specifies port on which a universe wishes to receive and send messages. A
 port number is an integer between @racket[0] and @racket[65536].
}}

@item{
 @defform[(to-string render-expr)
          #:contracts
          ([render-expr (-> (unsyntax @tech{UniverseState}) string?)])]{
 tells DrRacket to render the state of the universe after each event and to
 display this string in the universe console.
 }
}

@item{
 @defform/none[#:literals (check-with)
               (check-with universe?-expr)
          #:contracts
          ([universe?-expr (-> Any boolean?)])]{
 ensure that what the event handlers produce is really an element of
 @tech{UniverseState}.}
}

@item{
@defform/none[#:literals (state) (state expr)]{
 tells DrRacket to display a separate window in which the current
 state is rendered each time it is updated. This is mostly useful for
 debugging server programs.
}}

]

@subsection{Exploring a Universe}

In order to explore the workings of a universe, it is necessary to launch a
 server and several world programs on one and the same computer. We
 recommend launching one server out of one DrRacket tab and as many worlds
 as necessary out of a second tab. For the latter, the teachpack provides a
 special form.

@defform[(launch-many-worlds expression ...)]{
 evaluates all sub-expressions in parallel. Typically each sub-expression
 is an application of a function that evaluates a @racket[big-bang]
 expression. When all worlds have stopped, the expression returns all final
 worlds in order.}

Once you have designed a world program, add a function definition
 concerning @racket[big-bang] to the end of the tab:
@(begin
#reader scribble/comment-reader
(racketblock
;; String -> World
(define (main n)
  (big-bang ... (name n) ...))
))
 Then in DrRacket's Interactions area, use @racket[launch-many-worlds]
 to create several distinctively named worlds:
@(begin
#reader scribble/comment-reader
(racketblock
> (launch-many-worlds (main "matthew")
                      (main "kathi")
                      (main "h3"))
10
25
33
))
 The three worlds can then interact via a server. When all of them have
 stopped, they produce the final states, here @racket[10], @racket[25], and
 @racket[33].

For advanced programmers, the library also provides a programmatic
interface for launching many worlds in parallel.

@defproc[(launch-many-worlds/proc [thunk-that-runs-a-world (-> any/c)] ...)
          (values any @#,racketfont{...})]{
 invokes all given @racket[thunk-that-runs-a-world] in parallel. Typically
 each argument is a function of no argument that evaluates a @racket[big-bang]
 expression. When all worlds have stopped, the function expression returns
 all final worlds in order.}

It is thus possible to decide at run time how many and which worlds to run
in parallel:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
> (apply launch-many-worlds/proc
         (build-list (random 10)
                     (lambda (i)
                       (lambda ()
                         (main (number->string i))))))
0
9
1
2
3
6
5
4
8
7
))
@;%



@; -----------------------------------------------------------------------------
@section[#:tag "universe-sample"]{A First Sample Universe}

This section uses a simple example to explain the design of a universe,
 especially its server and some participating worlds. The first subsection
 explains the example, the second introduces the general design plan for
 such universes. The remaining sections present the full-fledged solution.

@subsection{Two Ball Tossing Worlds}

Say we want to represent a universe that consists of a number of worlds and
 that gives each world a ``turn'' in a round-robin fashion. If a world is
 given its turn, it displays a ball that ascends from the bottom of a
 canvas to the top. It relinquishes its turn at that point and the server
 gives the next world a turn.

Here is an image that illustrates how this universe would work if two
 worlds participated:

@image["balls" #:suffixes '(".gif" ".png")]

 The two @tech{world} programs could be located on two distinct computers
 or on just one. A @tech{server} mediates between the two worlds, including
 the initial start-up.

@; -----------------------------------------------------------------------------
@subsection{Hints on Designing Universes}

The first step in designing a @tech{universe} is to understand the
 coordination of the @tech{world}s from a global perspective. To some
 extent, it is all about knowledge and the distribution of knowledge
 throughout a system. We know that the @tech{universe} doesn't exist until
 the server starts and the @tech{world}s are joining. Because of the nature
 of computers and networks, however, we may assume little else. Our network
 connections ensure that if some @tech{world} or the @tech{server} sends
 two messages to the @emph{same} place in some order, they arrive in the
 same order (if they arrive at all). In contrast, if two distinct
 @tech{world} programs send one message each, the network does not
 guarantee the order of arrival at the server; similarly, if the
 @tech{server} is asked to send some messages to several distinct
 @tech{world} programs, they may arrive at those worlds in the order sent
 or in the some other order. In the same vein, it is impossible to ensure
 that one world joins before another. Worst, when someone removes the
 connection (cable, wireless) between a computer that runs a @tech{world}
 program and the rest of the network or if some network cable is cut,
 messages don't go anywhere. Due to this vagaries, it is therefore the
 designer's task to establish a protocol that enforces a certain order onto
 a universe and this activity is called @emph{protocol design}.

From the perspective of the @tech{universe}, the design of a protocol is
 about the design of data representations for tracking universe information
 in the server and the participating worlds and the design of a data
 representation for messages. As for the latter, we know that they must be
 @|S-expression|s, but usually @tech{world} programs don't send all
 kinds of @|S-expression|s. The data definitions for messages must
 therefore select a subset of suitable @|S-expression|s. As for the
 state of the server and the worlds, they must reflect how they currently
 relate to the universe. Later, when we design their ``local'' behavior, we
 may add more components to their state space.

In summary, the first step of a protocol design is to introduce:

@itemize[

@item{a data definition for the information about the universe that the
server tracks, call it @tech{UniverseState};}

@item{a data definition for the world(s) about their current relationship
to the universe;}

@item{data definitions for the messages that are sent from the server to
the worlds and vice versa. Let's call them @deftech{S2W} for messages
from the server to the worlds and @deftech{W2S} for the other direction;
in the most general case you may need one pair per world.}
]

If all the worlds exhibit the same behavior over time, a single data
definition suffices for step 2. If they play different roles, we may need
one data definition per world.

Of course, as you define these collections of data always keep in mind what
the pieces of data mean, what they represent from the universe's
perspective.

The second step of a protocol design is to figure out which major
 events---the addition of a world to the universe, the arrival of a message
 at the server or at a world---to deal with and what they imply for the
 exchange of messages. Conversely, when a server sends a message to a
 world, this may have implications for both the state of the server and the
 state of the world. A good tool for writing down these agreements is an
 interaction diagram.


@verbatim{

     Server              World1                  World2
       |                   |                       |
       |   'go             |                       |
       |<------------------|                       |
       |    'go            |                       |
       |------------------------------------------>|
       |                   |                       |
       |                   |                       |
}

 Each vertical line is the life line of a @tech{world} program or the
 @tech{server}. Each horizontal arrow denotes a message sent from one
 @tech{universe} participant to another.

The design of the protocol, especially the data definitions, have direct
implications for the design of event handling functions. For example, in
the server we may wish to deal with two kinds of events: the joining of a
new world and the receipt of a message from one of the worlds. This
translates into the design of two functions with the following headers,

@(begin
#reader scribble/comment-reader
(racketblock
;; Bundle is
;;   (make-bundle UniverseState [Listof mail?] [Listof iworld?])

;; UniverseState iworld? -> Bundle
;; next list of worlds when world @racket[iw] is joining
;; the universe in state @racket[s]
(define (add-world s iw) ...)

;; UniverseState iworld? W2U -> Bundle
;; next list of worlds when world @racket[iw] is sending message @racket[m] to
;; the universe in state @racket[s]
(define (process s iw m) ...)
))

Finally, we must also decide how the messages affect the states of the
 worlds; which of their callback may send messages and when; and what to do
 with the messages a world receives. Because this step is difficult to
 explain in the abstract, we move on to the protocol design for the
 universe of ball worlds.

@; -----------------------------------------------------------------------------
@subsection{Designing the Ball Universe}

Running the ball @tech{universe} has a simple overall goal: to ensure that at any
 point in time, one @tech{world} is active and all others are passive. The active
 @tech{world} displays a moving ball, and the passive @tech{world}s should display
 something, anything that indicates that it is some other @tech{world}'s turn.

As for the server's state, it must obviously keep track of all @tech{world}s that
 joined the @tech{universe}, and it must know which one is active and which ones
 are passive. Of course, initially the @tech{universe} is empty, i.e., there are
 no @tech{world}s and, at that point, the server has nothing to track.

While there are many different useful ways of representing such a
 @tech{universe}, we just use the list of @emph{iworlds} that is handed to
 each handler and that handlers return via their bundles. The
 @tech{UniverseState} itself is useless for this trivial example. We
 interpret non-empty lists as those where the first @emph{iworld} is active
 and the remainder are the passive @emph{iworld}s. As for the two possible
 events,

@itemize[

@item{it is natural to add new @emph{iworld}s to the end of the list; and}

@item{it is natural to move an active @emph{iworld} that relinquishes its turn to
the end of the list, too.}
]

The server should send messages to the first @emph{iworld} of its list as
 long as it wishes this @emph{iworld} to remain active. In turn, it should
 expect to receive messages only from this one active @emph{iworld} and no
 other @emph{iworld}. The content of these two messages is nearly irrelevant
 because a message from the server to an @emph{iworld} means that it is the
 @emph{iworld}'s turn and a message from the @emph{iworld} to the server
 means that the turn is over. Just so that we don't confuse ourselves, we
 use two distinct symbols for these two messages:
@itemize[
@item{A @defterm{GoMessage} is @racket['it-is-your-turn].}
@item{A @defterm{StopMessage} is @racket['done].}
]

From the @tech{universe}'s perspective, each @tech{world} is in one of two states:
@itemize[
@item{A passive @tech{world} is @emph{resting}. We use @racket['resting] for this state.}
@item{An active @tech{world} is not resting. We delay choosing a representation
for this part of a @tech{world}'s state until we design its ``local'' behavior.}
]
 It is also clear that an active @tech{world} may receive additional messages,
 which it may ignore. When it is done with its turn, it will send a
 message.

@verbatim{
     Server
       |                 World1
       |<==================|
       |  'it-is-your-turn |
       |------------------>|
       |                   |                    World2
       |<==========================================|
       |  'done            |                       |
       |<------------------|                       |
       |  'it-is-your-turn |                       |
       |------------------------------------------>|
       |                   |                       |
       |                   |                       |
       |  'done            |                       |
       |<------------------------------------------|
       |  'it-is-your-turn |                       |
       |------------------>|                       |
       |                   |                       |
       |                   |                       |
}

Here the double-lines (horizontal) denote the registration step, the others
 are message exchanges. The diagram thus shows how the @tech{server}
 decides to make the first registered world the active one and to enlist
 all others as they join.


@; -----------------------------------------------------------------------------
@subsection{Designing the Ball Server}

The preceding subsection dictates that our server program starts like this:

@(begin
#reader scribble/comment-reader
[racketblock
;; teachpack: universe.rkt

;; UniverseState is '*
;; StopMessage is 'done.
;; GoMessage is 'it-is-your-turn.
])

 The design of a protocol has immediate implications for the design of the
 event handling functions of the server. Here we wish to deal with two
 events: the appearance of a new world and the receipt of a message. Based
 on our data definitions and based on the general contracts of the event
 handling functions spelled out in this documentation, we get two functions
 for our wish list:

@(begin
#reader scribble/comment-reader
[racketblock
;; Result is
;;   (make-bundle [Listof iworld?]
;;                (list (make-mail iworld? GoMessage))
;;                '())

;; [Listof iworld?] iworld? -> Result
;; add world @racket[iw] to the universe, when server is in state @racket[u]
(define (add-world u iw) ...)

;; [Listof iworld?] iworld? StopMessage -> Result
;; world @racket[iw] sent message @racket[m] when server is in state @racket[u]
(define (switch u iw m) ...)
])

Although we could have re-used the generic contracts from this
documentation, we also know from our protocol that our server sends a
message to exactly one world. Note how these contracts are just refinements
of the generic ones. (A type-oriented programmer would say that the
contracts here are subtypes of the generic ones.)

The second step of the design recipe calls for functional examples:

@(begin
#reader scribble/comment-reader
[racketblock
;; an obvious example for adding a world:
(check-expect
  (add-world '() world1)
  (make-bundle (list world1)
               (list (make-mail world1 'it-is-your-turn))
               '()))

;; an example for receiving a message from the active world:
(check-expect
 (switch (list world1 world2) world1 'done)
 (make-bundle (list world2 world1)
              (list (make-mail world2 'it-is-your-turn))
              '()))
])

 Note that our protocol analysis dictates this behavior for the two
 functions. Also note how we use @racket[world1], @racket[world2], and
 @racket[world3] because the teachpack applies these event handlers to real
 worlds.

Exercise: Create additional examples for the two functions based on our
protocol.

The protocol tells us that @emph{add-world} just adds the given
 @emph{world} structure---recall that this a data representation of the
 actual @tech{world} program---to the given list of worlds. It then sends a
 message to the first world on this list to get things going:

@(begin
#reader scribble/comment-reader
[racketblock
(define (add-world univ wrld)
  (local ((define univ* (append univ (list wrld))))
    (make-bundle univ*
                 (list (make-mail (first univ*) 'it-is-your-turn))
                 '())))
])

Because @emph{univ*} contains at least @emph{wrld}, it is acceptable to
create a mail to @racket[(first univ*)]. Of course, this same reasoning
also implies that if @emph{univ} isn't empty, its first element is an
active world and is about to receive a second @racket['it-is-your-turn] message.

Similarly, the protocol says that when @emph{switch} is invoked because a
 @tech{world} program sends a message, the data representation of the
 corresponding world is moved to the end of the list and the next world on
 the (resulting) list is sent a message:

@(begin
#reader scribble/comment-reader
[racketblock
(define (switch univ wrld m)
  (local ((define univ* (append (rest univ) (list (first univ)))))
    (make-bundle univ*
                 (list (make-mail (first univ*) 'it-is-your-turn))
                 '())))
])

 As before, appending the first world to the end of the list guarantees
 that there is at least this one world on this list. It is therefore
 acceptable to create a mail for this world.

Start the server now.

 @racketblock[(universe '() (on-new add-world) (on-msg switch))]

Exercise: The function definition simply assumes that @emph{wrld} is
 @racket[world=?] to @racket[(first univ)] and that the received message
 @emph{m} is @racket['done]. Modify the function definition so that it
 checks these assumptions and raises an error signal if either of them is
 wrong. Start with functional examples. If stuck, re-read the section on
 checked functions from HtDP. (Note: in a @tech{universe} it is quite
 possible that a program registers with a @tech{server} but fails to stick
 to the agreed-upon protocol. How to deal with such situations properly
 depends on the context. For now, stop the @tech{universe} at this point by
 returning an empty list of worlds. Consider alternative solutions, too.)

Exercise: An alternative state representation would equate
 @tech{UniverseState} with @emph{world} structures, keeping track of the
 active world. The list of world in the server would track the passive
 worlds only. Design appropriate @racket[add-world] and @racket[switch]
 functions.

@; -----------------------------------------------------------------------------
@subsection{Designing the Ball World}

The final step is to design the ball @tech{world}. Recall that each world
 is in one of two possible states: active or passive. The second kind of
 @tech{world} moves a ball upwards, decreasing the ball's @emph{y}
 coordinate; the first kind of @tech{world} displays something that says
 it's someone else's turn.  Assuming the ball always moves along a vertical
 line and that the vertical line is fixed, the state of the world is an
 enumeration of two cases:

@(begin #reader scribble/comment-reader
(racketblock
;; teachpack: universe.rkt

;; WorldState is one of:
;; -- Number             %% representing the @emph{y} coordinate
;; -- @racket['resting]

(define WORLD0 'resting)

;; A WorldResult is one of:
;; -- WorldState
;; -- (make-package WorldState StopMessage)
))
 The definition says that initially a @tech{world} is passive.

The communication protocol and the refined data definition of @|WorldState|
 imply a number of contract and purpose statements:

@(begin
#reader scribble/comment-reader
(racketblock

;; WorldState GoMessage -> WorldResult
;; make sure the ball is moving
(define (receive w n) ...)

;; WorldState -> WorldResult
;; move this ball upwards for each clock tick
;; or stay @racket['resting]
(define (move w) ...)

;; WorldState -> Image
;; render the world as an image
(define (render w) ...)
))

Let's design one function at a time, starting with @emph{receive}.  Since
 the protocol doesn't spell out what @emph{receive} is to compute, let's
 create a good set of functional examples, exploiting the structure of the
 data organization of @|WorldState|:

@(begin
#reader scribble/comment-reader
(racketblock
(check-expect (receive 'resting 'it-is-your-turn) HEIGHT)
(check-expect (receive (- HEIGHT 1) 'it-is-your-turn) ...)
))

Since there are two kinds of states, we make up at least two kinds of
 examples: one for a @racket['resting] state and another one for a numeric
 state. The dots in the result part of the second unit test reveal the
 first ambiguity; specifically it isn't clear what the result should be
 when an active @tech{world} receives another message to activate itself. The
 second ambiguity shows up when we study additional examples, which are
 suggested by our approach to designing functions on numeric intervals
 (HtDP, section 3). That is we should consider the following three inputs
 to @emph{receive}:

@itemize[
@item{@racket[HEIGHT] when the ball is at the bottom of the image;}
@item{@racket[(- HEIGHT 1)] when the ball is properly inside the image; and}
@item{@racket[0] when the ball has hit the top of the image.}
]

 In the third case the function could produce three distinct results:
 @racket[0], @racket['resting], or @racket[(make-package 'resting
 'done)]. The first leaves things alone; the second turns the active @tech{world}
 into a resting one; the third does so, too, and tells the universe about
 this switch.

We choose to design @emph{receive} so that it ignores the message and
 returns the current state of an active @tech{world}.  This ensures that the ball
 moves in a continuous fashion and that the @tech{world} remains active.

Exercise: One alternative design is to move the ball back to the bottom of
the image every time @racket['it-is-your-turn] is received. Design this function, too.

@(begin
#reader scribble/comment-reader
(racketblock

(define (receive w m)
  (cond
    [(symbol? w) HEIGHT] ;; meaning: @racket[(symbol=? w 'resting)]
    [else w]))
))

 Our second function to design is @emph{move}, the function that computes
 the ball movement. We have the contract and the second step in the design
 recipe calls for examples:

@(begin
#reader scribble/comment-reader
(racketblock
; WorldState -> WorldState or @racket[(make-package 'resting 'done)]
; move the ball if it is flying

(check-expect (move 'resting) 'resting)
(check-expect (move HEIGHT) (- HEIGHT 1))
(check-expect (move (- HEIGHT 1)) (- HEIGHT 2))
(check-expect (move 0) (make-package 'resting 'done))

(define (move x) ...)
))

 Following HtDP again, the examples cover four typical situations:
 @racket['resting], two end points of the specified numeric interval, and
 one interior point. They tell us that @emph{move} leaves a passive @tech{world}
 alone and that it otherwise moves the ball until the @emph{y} coordinate
 becomes @racket[0]. In the latter case, the result is a package that
 renders the @tech{world} passive and tells the server about it.

 Turning these thoughts into a complete definition is straightforward now:

@(begin
#reader scribble/comment-reader
(racketblock
(define (move x)
  (cond
    [(symbol? x) x]
    [(number? x) (if (<= x 0)
                     (make-package 'resting 'done)
                     (sub1 x))]))
))

Exercise: what could happen if we had designed @emph{receive} so that it
 produces @racket['resting] when the state of the world is @racket[0]?  Use
 your answer to explain why you think it is better to leave this kind of
 state change to the tick event handler instead of the message receipt
 handler?

Finally, here is the third function, which renders the state as an image:

@(begin
#reader scribble/comment-reader
(racketblock
; WorldState -> Image
; render the state of the world as an image

(check-expect (render HEIGHT) (underlay/xy MT 50 HEIGHT BALL))
(check-expect (render 'resting)
              (underlay/xy MT 10 10 (text "resting" 11 "red")))

(define (render w)
  (underlay/xy
    (cond
      [(symbol? w) (underlay/xy MT 10 10 (text "resting" 11 "red"))]
      [(number? w) (underlay/xy MT 50 w BALL)])
    5 85
    (text name 11 "black")))

))

 Here is an improvement that adds a name to the image and abstracts over
 the name at the same time:

@(begin
#reader scribble/comment-reader
(racketblock
; String -> (WorldState -> Image)
; render the state of the world as an image

(check-expect
 ((draw "Carl") 100)
 (underlay/xy (underlay/xy MT 50 100 BALL)
              5 85
              (text "Carl" 11 "black")))

(define (draw name)
  (lambda (w)
    (overlay/xy
     (cond
       [(symbol? w) (underlay/xy MT 10 10 (text "resting" 11 "red"))]
       [(number? w) (underlay/xy MT 50 w BALL)])
     5 85
     (text name 11 'black))))

))

 By doing so, we can use the same program to create many different
 @tech{world}s that register with a @tech{server} on your computer:
@(begin
#reader scribble/comment-reader
(racketblock

; String -> WorldState
; create and hook up a world with the @racket[LOCALHOST] server
(define (create-world n)
  (big-bang WORLD0
           (on-receive receive)
           (to-draw (draw n))
           (on-tick move)
           (name n)
           (register LOCALHOST)))
))

 Now you can use @racket[(create-world 'carl)] and @racket[(create-world 'sam)],
 respectively, to run two different worlds, after launching a @tech{server}
 first.

Exercise: Design a function that takes care of a world to which the
 universe has lost its connection. Is @emph{Result} the proper contract for
 the result of this function?
