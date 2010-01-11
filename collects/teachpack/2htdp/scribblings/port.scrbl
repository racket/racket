#lang scribble/doc

@(require "shared.ss" 
	  "port.ss"
	  scribble/manual 
          (for-label scheme
                     (prefix-in htdp: teachpack/htdp/world)
		     (prefix-in 2htdp: teachpack/2htdp/universe)
                     (only-in lang/htdp-beginner check-expect)
		     2htdp/image))

@; -----------------------------------------------------------------------------

@title[#:tag "htdp-port"]{Porting World Programs to Universe}

@author{Matthias Felleisen, Robby Findler}

@; -----------------------------------------------------------------------------
@section{The World is Not Enough}

With the June 2009 release, we started deprecating the world teachpack; instead
 we recommended the use of the world teachpack. With the January 2010 release,
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
(require htdp/world)
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require 2htdp/universe)
(require htdp/image)
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
(require htdp/world)

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
@schemeblock[(require htdp/world)]
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require 2htdp/universe)
(require htdp/image)
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
    (htdp:circle 10 'solid 'red)
    (htdp:rectangle 40 4 'solid 'red)))
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
;; Scene 
(define UFO
  (htdp:overlay
    (htdp:circle 10 "solid" "red")
    (htdp:rectangle 40 4 "solid" "red")))
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
 the clock ticks (though it is possible to supply these dimensions if the
 defaults don't work). Furthermore, the names of the clauses are similar to
 the old names but shorter. 

key events and mouse events are string: use key=? and mouse=? 

stuff from Todd: 

I'm not actually worried about me--I try to stay up to date, and I
teach three intro classes, so enough of my time is devoted to the HtDP
languages that I can stay on top of things. I'm worried about people
like Renee Ciezki in Arizona who pushes other teachers in her county
to use HtDP only to discover that the lessons she's given them to use
don't quite work anymore. At least she emails and asks what's going
on--I'm afraid other people just throw up their hands in frustration.

And it's not just that one change to go from world to universe--key
events became strings instead of symbols, for example, and there are a
number of other small changes. Having a checklist that explains how to
get from X to Y would make the transition less complicated.

Also, being unable to show previous student projects as examples is a
major downer. It's incredibly motivational to current students to see
what previous students have done. Porting five or six old projects to
the new version can eat up a couple of hours if you have to run them
and just correct errors. That's not horrible unless you don't have a
couple of hours to do it.


@; -----------------------------------------------------------------------------
@section{Porting Image Programs}

robby's section 

using the new image library in isolation: 

@port[
@(begin
#reader scribble/comment-reader
(schemeblock
(require htdp/image)
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require 2htdp/image)
))
]

using the new image library with the universe teachpack 

@port[
@(begin
#reader scribble/comment-reader
(schemeblock
(require htdp/world)
))
@; ---------------------------------
@(begin
#reader scribble/comment-reader
(schemeblock
(require 2htdp/universe)
(require 2htdp/image)
))
]



