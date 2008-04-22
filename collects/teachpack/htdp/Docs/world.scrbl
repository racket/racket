#lang scribble/doc

@(require scribble/manual
          (for-label scheme
		     teachpack/htdp/image
		     teachpack/htdp/world
		     lang/private/imageeq))

@title[#:tag "world"]{Simulations and Animations: world.ss}

@;[May I suggest that in the docs, you give a simple world, and then translate it into a classical class.ss/GUI code, with timers, etc. ?]

@declare-exporting[teachpack/htdp/world]

The teachpack provides two sets of tools. The first allows students to
create and display a series of animated scenes, i.e., a simulation. The
second one generalizes the first by adding interactive GUI features. 

@section[#:tag "basics"]{Basics}

The teachpack assumes working knowledge of the basic image manipulation
primitives and introduces a special kind of image: a scene. 

@deftogether[(
@defthing[Image image?]
@defthing[Scene (and/c image? (lambda (i) 
		                 (and (= (pinhole-x i) 0) (= (pinhole-y i) 0))))])]{
For image creation and manipulation, see @secref["image"]. 

A @scheme[Scene] is an image whose  pinhole is at position @scheme[(0,0)]}

@defproc[(empty-scene [width natural-number/c][height natural-number/c]) Scene]{Creates a @scheme[width] x @scheme[height] Scene.}

@defproc[(place-image [img image?] [x number?][y number?][s Scene]) Scene]{
 Creates a scene by placing @scheme[img] at @scheme[(x,y)] into @scheme[s];
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

@defthing[World any/c]{For animated worlds and games, using the teachpack
 requires that you provide a data definition for @scheme[World]. In
 principle, there are no constraints on this data definition.}

Given a data definition for worlds, the following functions create worlds,
 visualize it, make the clock tick, and provide feedback about the mouse
 and keyboard actions that the program's users perform.
