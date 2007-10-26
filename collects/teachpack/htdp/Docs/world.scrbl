#lang scribble/doc

@begin[(require
	 (lib "manual.ss" "scribble"))
       (require-for-label
	 (lib "lang.ss" "big")
	 (only "../world.ss" run-simulation)
	 "../image.ss"
	 )]

@title[#:tag "world"]{Simulations and Animations: world.ss}

The teachpack provides two kinds of functions. The first five allow
students to simulate a small world of animated drawings and games: 

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
