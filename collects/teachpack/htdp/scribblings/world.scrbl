#lang scribble/doc

@(require scribble/manual
          (for-label scheme
		     teachpack/htdp/image
		     teachpack/htdp/world
		     lang/private/imageeq))

@title[#:tag "world"]{Simulations and Animations: world.ss}


The teachpack provides two sets of tools. The first allows students to
create and display a series of animated scenes, i.e., a simulation. The
second one generalizes the first by adding interactive GUI features. 

@declare-exporting[teachpack/htdp/world #:use-sources (teachpack/htdp/image)]

@section[#:tag "basics"]{Basics}

The teachpack assumes working knowledge of the basic image manipulation
primitives and introduces a special kind of image: a scene. 

@deftech{Scene}@schemeblock[(and/c image?
			           (lambda (i)
				     (and (= (pinhole-x i) 0) (= (pinhole-y i) 0))))]

The teachpack can display only @tech{Scene}s, which are images whose
pinholes are at position @scheme[(0,0)].

@defproc[(empty-scene [width natural-number/c]
	   [height natural-number/c])
	 (unsyntax @tech{Scene})]
{Creates a @scheme[width] x @scheme[height] @tech{Scene}.} 

@defproc[(place-image [img image?] [x number?][y number?]
	   [s (unsyntax @tech{Scene})]) (unsyntax @tech{Scene})]
{Creates a scene by placing @scheme[img] at @scheme[(x,y)] into @scheme[s];
 @scheme[(x,y)] are comp. graph. coordinates, i.e., they count left and
 down from the upper-left corner.}

@section[#:tag "simulations"]{Simple Simulations}

@defproc[(run-simulation
	   [w natural-number/c]
	   [h natural-number/c]
	   [r number?]
	   [create-image (-> natural-number/c scene)]
	   [gifs? boolean? #f])
	 true]{
   creates and shows a canvas of width @scheme[w] and height @scheme[h] , 
   starts a clock, making it tick every @scheme[r] (usually fractional)
   seconds. Every time the clock ticks, drscheme applies @scheme[create-image] to
   the number of ticks passed since this function call. The results of
   these applications are displayed in the canvas.

   The fifth (and last) argument is optional. Providing @scheme[true] as
   the fifth argument causes drscheme to collect the scenes that the
   animation generates and to create an animated GIF from the results. Both
   the intermediate images as well as the final animated GIF are saved in a
   user-specified directory. This is useful for writing documentation and
   for describing students work.  
}

In addition, 
@schemeblock[
(define (create-UFO-scene height)
  (place-image UFO 50 height (empty-scene 100 100)))

(define UFO
  (overlay (circle 10 'solid 'green)
           (rectangle 40 4 'solid 'green)))

(run-simulation 100 100 (/ 1 28) create-UFO-scene)
]

@;-----------------------------------------------------------------------------
@section[#:tag "interactive"]{Interactions}

An animation starts from a given ``world'' and generates new ones in response to events on the
computer. This teachpack keeps track of the ``current world'' and recognizes three kinds of events:
clock ticks; keyboard presses and releases; and mouse movements, mouse clicks, etc. Your program may
deal with such events via the @emph{installation} of @emph{handlers}. The teachpack provides for the
installation of three event handlers: @scheme[on-tick-event], @scheme[on-key-event], and
@scheme[on-mouse-event]. In addition, it provides for the installation of a @scheme[draw] handler,
which is called every time your program should visualize the current world. 

@deftech{World} @scheme[any/c]

 For animated worlds and games, using the teachpack requires that you
 provide a data definition for @tech{World}. In principle, there are no
 constraints on this data definition. You can even keep it implicit, even
 if this violates the Design Recipe.

@defproc*[(
[(big-bang [width natural-number/c] [height natural-number/c] [r number?] [world0 (unsyntax @tech{World})]) true]
[(big-bang [width natural-number/c] [height natural-number/c] [r number?] [world0 (unsyntax @tech{World})][animated-gif? boolean?]) true]
)]{
   Creates and displays a @scheme[width] x @scheme[height] canvas,
   starts the clock, 
   makes it tick every n seconds, 
   and makes @scheme[w] the current world. 
   If it is called with five instead of four arguments and the last one
   (@scheme[animated-gif?]) is @scheme[true], the teachpack allows the
   generation of images from the animation, including an animated GIF image. }

@defproc[(on-tick-event [tock (-> (unsyntax @tech{World}) (unsyntax @tech{World}))]) true]{
   Tell DrScheme to call @scheme[tock] on the current world every time the
   clock ticks. The result of the call becomes the current world.} 

@deftech{KeyEvent} @scheme[(or/c char? symbol?)]

A @tech{KeyEvent} represents key board events, e.g., keys pressed or
   released, by the computer's user. A @scheme[char?] @tech{KeyEvent} is
   used to signal that the user has hit an alphanumeric key. Symbols such
   as @scheme['left], @scheme['right], @scheme['up], @scheme['down],
   @scheme['release] denote arrow keys or special events, such as releasing
   the key on the keypad.

@defproc[(on-key-event [change (-> (unsyntax @tech{World}) (unsyntax @tech{KeyEvent}) (unsyntax @tech{World}))]) true]{
   Tell DrScheme to call @scheme[change] on the current world and a 
   @tech{KeyEvent} for every keystroke the user of the computer makes. The result
   of the call becomes the current world.

   Here is a typical key-event handler: 
@(begin
#reader scribble/comment-reader
(schemeblock
(define (change w a-key-event)
  (cond
    [(char? a-key-event) w]
    ;; else (symbol? a-key-event) holds
    [(symbol=? a-key-event 'left)  (world-go w -DELTA)]
    [(symbol=? a-key-event 'right) (world-go w +DELTA)]
    [(symbol=? a-key-event 'up)    (world-go w -DELTA)]
    [(symbol=? a-key-event 'down)  (world-go w +DELTA)]
    [else w]))
))

}

@deftech{MouseEvent} @scheme[(one-of/c 'button-down 'button-up 'drag 'move 'enter 'leave)]
 
 A @tech{MouseEvent} represents mouse events, e.g., mouse movements or mouse clicks, by the
   computer's user. 

@defproc[(on-mouse-event [clack (-> (unsyntax @tech{World}) natural-number/c natural-number/c (unsyntax @tech{MouseEvent}) (unsyntax @tech{World}))]) true]{
   Tell DrScheme to call @scheme[clack] on the current world, the current
   @scheme[x] and @scheme[y] coordinates of the mouse, and and a
   @tech{MouseEvent} for every action of the mouse by the user of the
   computer. The result of the call becomes the current world.}

@defproc[(on-redraw [to-scene (-> (unsyntax @tech{World}) (unsyntax @tech{Scene}))]) true]{ Tell DrScheme to call @scheme[to-scene]
   whenever the canvas must be redrawn. The canvas is usually re-drawn after a tick event, a keyboard
   event, or a mouse event has occurred.  The generated scene is  displayed in the world's canvas.}

@defproc[(stop-when [last-world? (-> (unsyntax @tech{World}) boolean?)]) true]{
   Tell DrScheme to call @scheme[last-world?] whenever the canvas is
   drawn. If this call produces @scheme[true], the clock is stopped; no more
   tick events, @tech{KeyEvent}s, or @tech{MouseEvent}s are forwarded to
   the respective handlers. As a result, the canvas isn't updated either.} 

@section{Scenes and Images}

For the creation of scenes from the world, use the functions from @secref["image"].  The following two
functions have turned out to be useful for the creation of scenes, too. 


@defproc[(nw:rectangle [width natural-number/c] [height natural-number/c] [solid-or-filled Mode] [c Color]) image?]{
   Creates a @scheme[width] x @scheme[height] rectangle, solid or outlined as specified by
   @scheme[solid-or-filled] and colored according to @scheme[c], with a pinhole at the upper left
   corner.}
   
@defproc[(scene+line [s (unsyntax @tech{Scene})][x0 number?][y0 number?][x1 number?][y1 number?][c Color]) (unsyntax @tech{Scene})]{
   Creates a scene by placing a line of color @scheme[c] from @scheme[(x0,y0)] to
   @scheme[(x1,y1)] into @scheme[scene];  
   @scheme[(x,y)] are comp. graph. coordinates; 
   in contrast to the @scheme[add-line] function, this
   one cuts off those portions of the line that go beyond the boundaries of
   the given @scheme[s].}
