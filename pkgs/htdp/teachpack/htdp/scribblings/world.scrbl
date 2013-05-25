#lang scribble/doc

@(require scribble/manual
          (only-in scribble/core make-nested-flow make-style)
          "shared.rkt"
	  scribble/struct
          (for-label racket
                     teachpack/htdp/image
                     teachpack/htdp/world))

@teachpack["world"]{Simulations and Animations}

@defmodule[#:require-form beginner-require htdp/world #:use-sources (htdp/image)]

@deprecated[@racketmodname[2htdp/universe]]{
  For guidance on how to convert your @racketmodname[htdp/world] programs
  to use @racketmodname[2htdp/universe], see @secref[#:tag-prefixes '("2htdp")]{htdp-port}}

@emph{Note}: For a quick and educational introduction to the teachpack, see
@link["http://www.ccs.neu.edu/home/matthias/HtDP/Prologue/book.html"]{How
to Design Programs, Second Edition: Prologue}. As of August 2008, we also
have a series of projects available as a small booklet on
@link["http://world.cs.brown.edu/"]{How to Design Worlds}. 

The purpose of this documentation is to give experienced Racketers a concise
overview for using the library and for incorporating it elsewhere. The last
section presents @secref["example"] for an extremely simple domain and is
suited for a novice who knows how to design conditional functions for
symbols. 

The teachpack provides two sets of tools. The first allows students to
create and display a series of animated scenes, i.e., a simulation. The
second one generalizes the first by adding interactive GUI features. 

@; -----------------------------------------------------------------------------
@section[#:tag "simulations"]{Simple Simulations}

@defproc[(run-movie [r (and/c real? positive?)] [m [Listof image?]]) 
         true]{

 @racket[run-movie] displays the list of images @racket[m] at the rate of
 @racket[r] images per second.}

@defproc[(run-simulation
           [w natural-number/c]
           [h natural-number/c]
           [r number?]
           [create-image (-> natural-number/c scene)])
         true]{
   creates and shows a canvas of width @racket[w] and height @racket[h] , 
   starts a clock, making it tick every @racket[r] (usually fractional)
   seconds. Every time the clock ticks, @racket[run-simulation] applies @racket[create-image] to
   the number of ticks passed since this function call. The results of
   these applications are displayed in the canvas.
}

Example:
@racketblock[
(define (create-UFO-scene height)
  (place-image UFO 50 height (empty-scene 100 100)))

(define UFO
  (overlay (circle 10 'solid 'green)
           (rectangle 40 4 'solid 'green)))

(run-simulation 100 100 (/ 1 28) create-UFO-scene)
]

@;-----------------------------------------------------------------------------
@section[#:tag "interactive"]{Interactions}

An animation starts from a given ``world'' and generates new ones in
 response to events on the computer. This teachpack keeps track of the
 ``current world'' and recognizes three kinds of events: clock ticks;
 keyboard presses and releases; and mouse movements, mouse clicks,
 etc.

Your program may deal with such events via the @emph{installation} of
 @emph{handlers}.  The teachpack provides for the installation of three
 event handlers: @racket[on-tick-event], @racket[on-key-event], and
 @racket[on-mouse-event]. In addition, it provides for the installation of
 a @racket[draw] handler, which is called every time your program should
 visualize the current world.

The following picture provides an intuitive overview of the workings of
 "world".

@image["world.png"]

 The @racket[big-bang] function installs @emph{World_0} as the initial
 world; the callbacks @emph{tock}, @emph{react}, and @emph{click} transform
 one world into another one; @emph{done} checks each time whether the world
 is final; and @emph{draw} renders each world as a scene. 

@deftech{World} @racket[any/c]

 For animated worlds and games, using the teachpack requires that you
 provide a data definition for @tech{World}. In principle, there are no
 constraints on this data definition. You can even keep it implicit, even
 if this violates the Design Recipe.

@defproc*[(
[(big-bang [width natural-number/c] [height natural-number/c] [r number?] [world0 (unsyntax @tech{World})]) true]
[(big-bang [width natural-number/c] [height natural-number/c] [r number?] [world0 (unsyntax @tech{World})][animated-gif? boolean?]) true]
)]{
   Creates and displays a @racket[width] x @racket[height] canvas,
   starts the clock, 
   makes it tick every @racket[r] seconds, 
   and makes @racket[world0] the current world. 
   If it is called with five instead of four arguments and the last one
   (@racket[animated-gif?]) is @racket[true], the teachpack allows the
   generation of images from the animation, including an animated GIF image. }

@defproc[(on-tick-event [tock (-> (unsyntax @tech{World}) (unsyntax @tech{World}))]) true]{
   Tells @racket[big-bang] to call @racket[tock] on the current world every time the
   clock ticks. The result of the call becomes the current world.} 

@deftech{KeyEvent} @racket[(or/c char? symbol?)]

A @tech{KeyEvent} represents key board events, e.g., keys pressed or
   released, by the computer's user. A @racket[char?] @tech{KeyEvent} is
   used to signal that the user has hit an alphanumeric key. Symbols such
   as @racket['left], @racket['right], @racket['up], @racket['down],
   @racket['release] denote arrow keys or special events, such as releasing
   the key on the keypad.

@defproc[(key-event? [x any]) boolean?]{
   is @racket[x] a @tech{KeyEvent}}

@defproc[(key=? [x key-event?][y key-event?]) boolean?]{
   compares two @tech{KeyEvent} for equality}

@defproc[(on-key-event [change (-> (unsyntax @tech{World}) key-event? (unsyntax @tech{World}))]) true]{
   Tells @racket[big-bang] to call @racket[change] on the current world and a 
   @tech{KeyEvent} for every keystroke the user of the computer makes. The result
   of the call becomes the current world.

   Here is a typical key-event handler: 
@(begin
#reader scribble/comment-reader
(racketblock
(define (change w a-key-event)
  (cond
    [(key=? a-key-event 'left)  (world-go w -DELTA)]
    [(key=? a-key-event 'right) (world-go w +DELTA)]
    [(char? a-key-event) w] ;; to demonstrate order-free checking 
    [(key=? a-key-event 'up)    (world-go w -DELTA)]
    [(key=? a-key-event 'down)  (world-go w +DELTA)]
    [else w]))
))

}

@deftech{MouseEvent} @racket[(one-of/c 'button-down 'button-up 'drag 'move 'enter 'leave)]
 
 A @tech{MouseEvent} represents mouse events, e.g., mouse movements or mouse clicks, by the
   computer's user. 

@defproc[(on-mouse-event [clack (-> (unsyntax @tech{World}) natural-number/c natural-number/c (unsyntax @tech{MouseEvent}) (unsyntax @tech{World}))]) true]{
   Tells @racket[big-bang] to call @racket[clack] on the current world, the current
   @racket[x] and @racket[y] coordinates of the mouse, and a
   @tech{MouseEvent} for every action of the mouse by the user of the
   computer. The result of the call becomes the current world.}

@defproc[(on-redraw [to-scene (-> (unsyntax @tech{World}) (unsyntax @tech{Scene}))]) true]{ Tells @racket[big-bang] to call @racket[to-scene]
   whenever the canvas must be redrawn. The canvas is usually re-drawn after a tick event, a keyboard
   event, or a mouse event has occurred.  The generated scene is  displayed in the world's canvas.}

@defproc[(stop-when [last-world? (-> (unsyntax @tech{World}) boolean?)]) true]{
   Tells @racket[big-bang] to call @racket[last-world?] whenever the canvas is
   drawn. If this call produces @racket[true], the clock is stopped; no more
   tick events, @tech{KeyEvent}s, or @tech{MouseEvent}s are forwarded to
   the respective handlers. As a result, the canvas isn't updated either.} 

Example: The following examples shows that @racket[(run-simulation 100 100
(/ 1 28) create-UFO-scene)] is a short-hand for three lines of code:
@racketblock[
(define (create-UFO-scene height)
  (place-image UFO 50 height (empty-scene 100 100)))

(define UFO
  (overlay (circle 10 'solid 'green)
           (rectangle 40 4 'solid 'green)))

(big-bang 100 100 (/1 28) 0)
(on-tick-event add1)
(on-redraw create-UFO-scene)
]
Exercise: Add a condition for stopping the flight of the UFO when it
reaches the bottom. 

@; -----------------------------------------------------------------------------

@(define (table* . stuff)
   ;; (list paragraph paragraph) *-> Table
   (define (flow* x) (make-flow (list x)))
   (make-blockquote #f
    (list
     (make-table (make-with-attributes 'boxed '((cellspacing . "6")))
                 ;; list
                 (map (lambda (x) (map flow* x)) stuff)
                 #;(map flow* (map car stuff))
                 #;(map flow* (map cadr stuff))))))

@; -----------------------------------------------------------------------------
@section[#:tag "example"]{A First Example} 


@subsection{Understanding a Door}

Say we want to represent a door with an automatic door closer. If this kind
 of door is locked, you can unlock it. While this doesn't open the door per
 se, it is now possible to do so. That is, an unlocked door is closed and
 pushing at the door opens it. Once you have passed through the door and
 you let go, the automatic door closer takes over and closes the door
 again. Of course, at this point you could lock it again. 

Here is a picture that translates our words into a graphical
 representation: 

@image["door-real.png"]

The picture displays a so-called "state machine". The three circled words
 are the states that our informal description of the door identified:
 locked, closed (and unlocked), and open. The arrows specify how the door
 can go from one state into another. For example, when the door is open,
 the automatic door closer shuts the door as time passes. This transition
 is indicated by the arrow labeled "time passes." The other arrows
 represent transitions in a similar manner: 

@itemize[

@item{"push" means a person pushes the door open (and let's go);}

@item{"lock" refers to the act of inserting a key into the lock and turning
it to the locked position; and}

@item{"unlock" is the opposite of "lock".}

]

@; -----------------------------------------------------------------------------
@subsection{Simulations of the World}

Simulating any dynamic behavior via a program demands two different
 activities. First, we must tease out those portions of our "world" that
 change over time or in reaction to actions, and we must develop a data
 representation @deftech{D} for this information.  Keep in mind that a good data
 definition makes it easy for readers to map data to information in the
 real world and vice versa. For all others aspects of the world, we use
 global constants, including graphical or visual constants that are used in
 conjunction with the rendering functions.

Second, we must translate the "world" actions---the arrows in the above
 diagram---into interactions with the computer that the world teachpack can
 deal with. Once we have decided to use the passing of time for one aspect
 and mouse movements for another, we must develop functions that map the
 current state of the world---represented as data---into the next state of
 the world. Since the data definition @tech{D} describes the class of data
 that represents the world, these functions have the following general
 contract and purpose statements: 

@(begin
#reader scribble/comment-reader
(racketblock
;; tick : @tech{D} -> @tech{D}
;; deal with the passing of time 
(define (tick w) ...)

;; click : @tech{D} @racket[Number] @racket[Number] @tech{MouseEvent} -> @tech{D}
;; deal with a mouse click at (x,y) of kind @racket[me]
;; in the current world @racket[w]
(define (click w x y me) ...)

;; control : @tech{D} @tech{KeyEvent} -> @tech{D}
;; deal with a key event (symbol, char) @racket[ke] 
;; in the current world @racket[w]
(define (control w ke) ...)
))

That is, the contracts of the various hooks dictate what the contracts of
these functions are once we have defined how to represent the world in
data. 

A typical program does not use all three of these actions and functions but
 often just one or two. Furthermore, the design of these functions provides
 only the top-level, initial design goal. It often demands the design of
 many auxiliary functions.

@; -----------------------------------------------------------------------------
@subsection{Simulating a Door: Data}

Our first and immediate goal is to represent the world as data. In this
 specific example, the world consists of our door and what changes about
 the door is whether it is locked, unlocked but closed, or open. We use
 three symbols to represent the three states:

@deftech{SD}

@(begin
#reader scribble/comment-reader
(racketblock
;; DATA DEF.
;; The state of the door (SD) is one of: 
;; -- @racket['locked]
;; -- @racket['closed]
;; -- @racket['open]
))

Symbols are particularly well-suited here because they directly express
 the state of the door. 

Now that we have a data definition, we must also decide which computer
 actions and interactions should model the various actions on the door.
 Our pictorial representation of the door's states and transitions,
 specifically the arrow from "open" to "closed" suggests the use of a
 function that simulates time. For the other three arrows, we could use
 either keyboard events or mouse clicks or both. Our solution uses three
 keystrokes: 
@racket[#\u] for unlocking the door, 
@racket[#\l] for locking it, and 
@racket[#\space] for pushing it open. 
 We can express these choices graphically by translating the above "state
 machine" from the world of information into the world of data: 

@image["door-sim.png"]

@; -----------------------------------------------------------------------------
@subsection{Simulating a Door: Functions}

Our analysis and data definition leaves us with three functions to design: 

@itemize[

@item{@racket[automatic-closer], which closes the time during one tick;}

@item{@racket[door-actions], which manipulates the time in response to
pressing a key; and}

@item{@racket[render], which translates the current state of the door into
a visible scene.}

]

Let's start with @racket[automatic-closer]. We know its contract and it is
easy to refine the purpose statement, too: 

@(begin
#reader scribble/comment-reader
(racketblock
;; automatic-closer : SD -> SD
;; closes an open door over the period of one tick 
(define (automatic-closer state-of-door) ...)
))

 Making up examples is trivial when the world can only be in one of three
 states: 

@table*[
	@list[@t{ given state } @t{ desired state }]
        @list[@t{ 'locked } @t{ 'locked }]
        @list[@t{ 'closed } @t{ 'closed }]
        @list[@t{ 'open } @t{ 'closed }]
]

@(begin
#reader scribble/comment-reader
(racketblock
;; automatic-closer : SD -> SD
;; closes an open door over the period of one tick 

(check-expect (automatic-closer 'locked) 'locked)
(check-expect (automatic-closer 'closed) 'closed)
(check-expect (automatic-closer 'open) 'closed)

(define (automatic-closer state-of-door) ...)
))

 The template step demands a conditional with three clauses: 

@(begin
#reader scribble/comment-reader
(racketblock
(define (automatic-closer state-of-door)
  (cond
    [(symbol=? 'locked state-of-door) ...]
    [(symbol=? 'closed state-of-door) ...]
    [(symbol=? 'open state-of-door) ...]))
))

 The examples basically dictate what the outcomes of the three cases must
 be:

@(begin
#reader scribble/comment-reader
(racketblock
(define (automatic-closer state-of-door)
  (cond
    [(symbol=? 'locked state-of-door) 'locked]
    [(symbol=? 'closed state-of-door) 'closed]
    [(symbol=? 'open state-of-door) 'closed]))
))

 Don't forget to run the example-tests. 

For the remaining three arrows of the diagram, we design a function that
 reacts to the three chosen keyboard events. As mentioned, functions that
 deal with keyboard events consume both a world and a keyevent:

@(begin
#reader scribble/comment-reader
(racketblock
;; door-actions : SD Keyevent -> SD
;; key events simulate actions on the door 
(define (door-actions s k) ...)
))

@table*[
	@list[@t{ given state } @t{ given keyevent } @t{ desired state }]
 
@list[ @t{  'locked } @t{ #\u    } @t{ 'closed}]
@list[ @t{  'closed } @t{ #\l    } @t{ 'locked} ]
@list[ @t{  'closed } @t{ #\space} @t{ 'open } ]
@list[ @t{  'open   } @t{ --- }    @t{ 'open } ]]

 The examples combine what the above picture shows and the choices we made
 about mapping actions to keyboard events. 

From here, it is straightforward to turn this into a complete design:
 
@racketblock[
(define (door-actions s k)
  (cond
    [(and (symbol=? 'locked s) (key=? #\u k)) 'closed]
    [(and (symbol=? 'closed s) (key=? #\l k)) 'locked]
    [(and (symbol=? 'closed s) (key=? #\space k)) 'open]
    [else s]))

(check-expect (door-actions 'locked #\u) 'closed)
(check-expect (door-actions 'closed #\l) 'locked)
(check-expect (door-actions 'closed #\space) 'open)
(check-expect (door-actions 'open 'any) 'open)
(check-expect (door-actions 'closed 'any) 'closed)
]

Last but not least we need a function that renders the current state of the
world as a scene. For simplicity, let's just use a large enough text for
this purpose: 

@(begin
#reader scribble/comment-reader
(racketblock
;; render : @tech{SD} -> @racket[Scene]
;; translate the current state of the door into a large text 
(define (render s)
  (text (symbol->string s) 40 'red))

(check-expecy (render 'closed) (text "closed" 40 'red))
))
 The function @racket[symbol->string] translates a symbol into a string,
 which is needed because @racket[text] can deal only with the latter, not
 the former. A look into the language documentation revealed that this
 conversion function exists, and so we use it. 

Once everything is properly designed, it is time to @emph{run} the
program. In the case of the world teachpack, this means we must specify
which function takes care of tick events, key events, and redraws: 

@(begin
#reader scribble/comment-reader
(racketblock
(big-bang 100 100 1 'locked)
(on-tick-event automatic-closer)
(on-key-event door-actions)
(on-redraw render)
))
 
Now it's time for you to collect the pieces and run them in @racket[big-bang] to see
whether it all works. 
