#lang scribble/doc

@(require "shared.ss" 
	  "port.ss"
	  scribble/manual 
          (for-label scheme
                     (only-in 2htdp/universe on-tick on-draw)
                     (prefix-in htdp: teachpack/htdp/world)
                     (prefix-in htdp: htdp/image)
		     (prefix-in 2htdp: teachpack/2htdp/universe)
                     (prefix-in 2htdp: 2htdp/image)
                     (only-in lang/htdp-beginner check-expect)))

@; -----------------------------------------------------------------------------

@title[#:tag "htdp-port"]{Porting World Programs to Universe}

@author{Matthias Felleisen, Robby Findler}

@; -----------------------------------------------------------------------------
@section{The World is Not Enough}

With the June 2009 release, we started deprecating the world teachpack; instead
 we recommended the use of the universe teachpack. With the January 2010 release,
 we are also introducing a new image teachpack and, in support of this second
 teachpack, we have separated out the image functionality from the
 functionality for world programs. 

In this document, we explain how to port programs that assume the old world
 teachpack into this new setting, one step at a time. Most importantly,
 programs must now import @emph{two} teachpacks insteead of one: 
@port[
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname htdp/world))
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname 2htdp/universe))
(require #,(schememodname htdp/image))
))
]
 The table shows the old style on the left and the new style on the
 right. If your programs imported teachpacks via the drscheme teachpack
 menu, we recommend that you use the @scheme[require] form from now on; 
 alternatively, you use the drscheme menu @emph{twice} to import the 
 functions from two teachpacks. 

In the next section, we first explain how to port world programs so that
 they use the universe teachpack and the @emph{old} image teachpack. In the
 section after that, we list suggestions for changing programs so that they
 no longer rely on the old image functionality but the new one. 

In order to distinguish between the various pieces of functionality, we
 uniformly prefix old functionality with "htdp:" and new functionality with
 "2htdp:". There is no need to use these prefixes in your programs of
 course. 

@; -----------------------------------------------------------------------------
@section{Porting World Programs}

Here is the first program from the documentation for the world teachpack:
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname htdp/world))

;; Number -> Scene 
(define (create-UFO-scene height)
  (htdp:place-image UFO 
                    50 height
		    (htdp:empty-scene 100 100)))

;; Scene 
(define UFO
  (htdp:overlay
    (htdp:circle 10 'solid 'red)
    (htdp:rectangle 40 4 'solid 'red)))

;; --- run program run 
(htdp:big-bang 100 100 (/1 28) 0)
(htdp:on-tick-event add1)
(htdp:on-redraw create-UFO-scene)
))
 This program defines a function for placing a @scheme[UFO] into a 100 by
 100 scene, where @scheme[UFO] is a defined image. The world program itself
 consists of three lines: 
@itemize[
@item{the first one creates the 100 by 100 scene, specifies a rate of 28
 images per second, and @scheme[0] as the initial world description;}
@item{the second one says that for each clock tick, the world (a number) is
 increased by @scheme[1]; and}
@item{the last line tells drscheme to use @scheme[create-UFO-scene] as the
 function that renders the current world as a scene.}
]

Let us now convert this program into the universe setting, step by
 step, staring with the @scheme[require] specification, which is converted
 as above: 
@port[
@schemeblock[(require #,(schememodname htdp/world))]
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname 2htdp/universe))
(require #,(schememodname htdp/image))
))
]

The function that renders the world as a scene remains the same: 
@port[
@(begin
#reader scribble/comment-reader
(schemeblock
;; Number -> Scene 
(define (create-UFO-scene height)
  (htdp:place-image
    UFO 
    50 height
    (htdp:empty-scene 100 100)))
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
;; Number -> Scene 
(define (create-UFO-scene height)
  (htdp:place-image
    UFO 
    50 height
    (htdp:empty-scene 100 100)))
))
]

For the image constant we switch from symbols to strings: 
@port[
@(begin
#reader scribble/comment-reader
(schemeblock
;; Scene 
(define UFO
  (htdp:overlay
    (htdp:circle 
     10 'solid 'red)
    (htdp:rectangle
     40 4 'solid 'red)))
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
;; Scene 
(define UFO
  (htdp:overlay
    (htdp:circle
     10 "solid" "red")
    (htdp:rectangle
     40 4 "solid" "red")))
))
]
 Strictly speaking, this isn't necessary, but we intend to replace symbols
 with strings whenever possible because strings are more common than
 symbols. 

The most important change concerns the lines that launch the world program: 
@port[
@schemeblock[
(htdp:big-bang 100 100 (/1 28) 0)
(htdp:on-tick-event add1)
(htdp:on-redraw create-UFO-scene)
]
@; ---------------------------------
@schemeblock[
(2htdp:big-bang
  0
  (on-tick add1)
  (on-draw create-UFO-scene))
]
]
 They are turned into a single expression that comes with as many clauses
 as there are lines in the old program. As you can see, the
 @scheme[big-bang] expression from the universe teachpack no longer
 requires the specification of the size of the scene or the rate at which
 the clock ticks (though it is possible to supply the clock rate if the default
 is not satisfactory). 
 Furthermore, the names of the clauses are similar to
 the old names but shorter. 


The other big change concerns key event handling and mouse event
 handling. The respective handlers no longer accept symbols and chars but
 strings only. Here is the first key event handler from the documentation
 of the world teachpack: 

@port[
@schemeblock[
 (define (change w a-key-event)
    (cond
      [(key=? a-key-event 'left)  
       (world-go w -DELTA)]
      [(key=? a-key-event 'right)
       (world-go w +DELTA)]
      [(char? a-key-event)
       w] 
      [(key=? a-key-event 'up)
       (world-go w -DELTA)]
      [(key=? a-key-event 'down)
       (world-go w +DELTA)]
      [else
       w]))]
@; ---------------------------------
@schemeblock[
 (define (change w a-key-event)
    (cond
      [(key=? a-key-event "left")  
       (world-go w -DELTA)]
      [(key=? a-key-event "right")
       (world-go w +DELTA)]
      [(= (string-length a-key-event) 1)
       w] 
      [(key=? a-key-event "up")
       (world-go w -DELTA)]
      [(key=? a-key-event "down")
       (world-go w +DELTA)]
      [else
       w]))
]]
 Note how the @scheme[char?] clause changed. Since all chars are now
 represented as strings containing one ``letter'', the program on the right
 just checks the length of the string. Otherwise, we simply change all
 symbols into strings. 

If you ever recorded your programs' work via an animated gif, you can still
 do so. Instead of adding a fifth argument to @scheme[big-bang], however,
 you will need to add a clause of the shape @scheme[(record? x)]. 

Finally, the universe teachpack implements a richer functionality than the
 world teachpack. 

@; -----------------------------------------------------------------------------
@section{Porting Image Programs}

The universe library also comes with a new image library, @schememodname[2htdp/image].
Using the old image
library still works fine with @schememodname[2htdp/universe], but the
new image library provides a number of improvements, including faster 
image comparison (especially useful in @scheme[check-expect] expressions),
rotating images, scaling images, curves, a number of new polygon shapes,
and more control over line drawing.

To use the new image library in isloation:

@port[
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname htdp/image))
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname 2htdp/image))
))
]

and to use the new image library with the universe teachpack:

@port[
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname htdp/world))
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require #,(schememodname 2htdp/universe))
(require #,(schememodname 2htdp/image))
))]
  
@bold{Overlay vs Underlay}

The @scheme[htdp:overlay] function places its first argument
under its second (and subsequent) arguments and so in 
@schememodname[2htdp/image], we decided to call that
function @scheme[2htdp:underlay].

@port[(schemeblock
       (htdp:overlay
        (htdp:rectangle 
         10 20 "solid" "red")
        (htdp:rectangle
         20 10 "solid" "blue")))
      (schemeblock
       (2htdp:underlay
        (2htdp:rectangle
         10 20 "solid" "red")
        (2htdp:rectangle
         20 10 "solid" "blue")))]

@bold{No more pinholes}

The concept of pinholes from @schememodname[htdp/image]
has no correspondance in @schememodname[2htdp/image] 
(we do expect to bring back pinholes in @schememodname[2htdp/image]
eventually, but they will not be as pervasive as they are
in @scheme[htdp/image]).

Instead of 
a special position in the image that overlay operations
are sensitive to, 
@schememodname[2htdp/image] has a family of overlay operations,
that overlay images based on their centers or their edges.

Since the default position of the pinhole is in the center
for most images and the default for overlaying and underlaying
images in @scheme[2htdp/image] is based on the center, 
simple examples (like the one above) behave the same
in both libraries.

But, consider this expression that overlays two images on
their upper-left corners, written using both libraries.

@port[@schemeblock[(htdp:overlay
                    (htdp:put-pinhole
                     (htdp:rectangle 10 20 "solid" "red")
                     0 0)
                    (htdp:put-pinhole
                     (htdp:rectangle 20 10 "solid" "blue")
                     0 0))]
       @schemeblock[(2htdp:underlay/align
                     "left"
                     "top"
                     (2htdp:rectangle 
                      10 20 "solid" "red")
                     (2htdp:rectangle
                      20 10 "solid" "blue"))]]

In the @schememodname[2htdp/image] version, the programmer
uses @scheme[2htdp:underlay/align] to specify where
the images should be lined up, instead of using the pinhole.

@bold{Outlines in different places}

The outline style shapes are now shifted by one pixel for @schememodname[2htdp/image] 
images as compared to @schememodname[htdp/image].
This means that these two rectangles draw the same sets of pixels.

@port[@schemeblock[(htdp:rectangle 
                    11 11 "outline" "black")]
       @schemeblock[(2htdp:rectangle
                     10 10 "outline" "black")]]

See also @secref["nitty-gritty"].

@bold{Star changed}

The @scheme[2htdp:star] function is a completely different
function from @scheme[htdp:star]. Both produce stars based, 
on polygons, but @scheme[2htdp:star] always produces a five-pointed
star. See also @scheme[2htdp:star-polygon] for more general star
shapes.
