#lang scribble/doc

@(require scribble/manual)

@title[#:tag "draw"]{Draw: draw.*}

Add 
@verbatim[#:indent 3]{
  import draw.*
}
at the top of your Definitions Window to import this library. 

This package provides classes and methods for a visual
world. Here is its class diagram of public fields and methods: 
@verbatim[#:indent 3]{
import colors.*;
import geometry.*;

 +-----------------------------------+
 | abstract World                    |
 +-----------------------------------+       +---------------------------------------+
 | Canvas theCanvas                  |------>| Canvas                                |
 +-----------------------------------+       +---------------------------------------+
 | boolean bigBang(int,int,double)   |       +---------------------------------------+
 | boolean endOfTime(String)         |       | boolean show()                        |
 | World endOfWorld(String)          |       | boolean close()                       |
 |                                   |       | boolean drawCircle(Posn,int,IColor)   |
 |                                   |       | boolean drawDisk(Posn,int,IColor)     |
 | abstract World onTick()           |       | boolean drawRect(Posn,int,int,IColor) |
 | abstract World onKeyEvent(String) |       | boolean drawLine(Posn,Posn,IColor)    |
 | abstract boolean draw()           |       | boolean drawString(Posn,String)       |
 +-----------------------------------+       +---------------------------------------+
}

Methods in these classes may fail due to the unavailability of the physical
devices, inappropriate uses, etc. In those cases, they fail with an
exception.

@section[#:tag "world"]{World}

The abstract @scheme[World] class exports the following methods. 

@; -----------------------------------------------------------------------------
bigBang(int width,int height,double speed)

Initializes the world, associates it with a @scheme[width] x
@scheme[height] <a href="#canvas">@scheme[Canvas]</a>, displays
this canvas, enables keyevents, and finally starts the clock at a rate of
one tick per @scheme[speed] seconds. If it succeeds with all of its
actions, the method produces @scheme[true].

@bold{Note}: @scheme[width], @scheme[height] and 
@scheme[speed] must be a positive. 

@; -----------------------------------------------------------------------------
The canvas in @scheme[World] is called

  @scheme[theCanvas]. 

References to a "canvas" in conjunction with the @scheme[World] class
 denote this default canvas. 

@; -----------------------------------------------------------------------------
endOfTime()

Displays the given message, stops the clock and, if it succeeds, produces
@scheme[true]. After the end of time, events no longer trigger calls
to @scheme[onTick] or @scheme[onKeyEvent]. The canvas remains visible. 

@; -----------------------------------------------------------------------------
endOfWorld(String msg)

Displays the given message, stops the clock and, if it succeeds, produces the 
last @scheme[World]. After the end of the world,  events no longer trigger calls
to @scheme[onTick] or @scheme[onKeyEvent] (see below). The canvas
remains visible. 

@; -----------------------------------------------------------------------------
A derived concrete class must supply definitions for the following methods: 

@; -----------------------------------------------------------------------------
onTick()

Invoked for every tick of the clock. Its purpose is to create a
@scheme[World] whose differences with @scheme[this] one represent
what happened during the amount of time it takes the clock to tick.  

@; -----------------------------------------------------------------------------
onKeyEvent(String key)

Invoked for every keyboard event associated with the canvas. Its purposes
  is to create a @scheme[World] whose differences with
  @scheme[this] one represent what happens due to the user's use of the
  keyboard. The latter is represented with the string-valued argument
  @scheme[key]. 

@; -----------------------------------------------------------------------------
draw()

Invoked <em>after</em> one of the two event handlers has been called. Its
purpose is to present @scheme[this World ] graphically on its
canvas. If it succeeds, its result is @scheme[true.] 

A program may, in principle, start several instances of (subclasses of)
@scheme[World]. If it does, the event handlers are called in a unpredictable
order. 

@section[#:tag "canvas"]{Canvas}

To create an instance of the @scheme[Canvas] class, a program must supply
two @scheme[int] values: one for the width of the canvas and one for its
height. The canvas is a rectangle, whose borders are parallel to the computer
screen's borders. A program can use the following methods on instances of
@scheme[Canvas:]

show()

Initializes the canvas to a white area, enables the drawing methods, and
  finally displays the canvas. If it succeeds, it produces
  @scheme[true]. Invoking the method a second time without calling
  @scheme[close] before has no effect.

close()

Hides the canvas and erases the current content.  If it succeeds, it
produces @scheme[true].

Closing the Canvas using the display controls does not fully hide the
canvas; it is still necessary to invoke @scheme[close] before
@scheme[show] is re-enabled.

drawCircle(Posn p,int r,IColor c)>

Draws a circle on @scheme[this Canvas] at @scheme[p] with radius
@scheme[r] and color @scheme[c].  If it succeeds, it produces
@scheme[true].

drawDisk(Posn p,int r,IColor c)

Draws a disk on @scheme[this Canvas] at @scheme[p] with radius
@scheme[r] and color @scheme[c].  If it succeeds, it produces
@scheme[true].

drawRect(Posn p,int w,int h,IColor c)

Draws a solid rectangle on @scheme[this Canvas] at @scheme[p] with
width @scheme[w], height @scheme[h], and color @scheme[c].  The
rectangle's lines are parallel to the canvas's borders. If it succeeds, it
produces @scheme[true].

drawLine(Posn p0,Posn p1,IColor c)

Draws a line on @scheme[this Canvas] from @scheme[p0] to
@scheme[p1] using color @scheme[c].  If it succeeds, it produces
@scheme[true].

drawString(Posn p,String s)

Draws the string @scheme[s] at @scheme[p] on @scheme[this
Canvas].  If it succeeds, it produces @scheme[true].
