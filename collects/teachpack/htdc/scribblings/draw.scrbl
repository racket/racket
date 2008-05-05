#lang scribble/doc

@(require scribble/manual (for-label teachpack/htdc/draw))

@title[#:tag "Jdraw"]{Draw: draw.*}

@declare-exporting[teachpack/htdc/draw]

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
 +-----------------------------------+       
 | Canvas theCanvas                  |----+
 +-----------------------------------+    |
 | boolean bigBang(int,int,double)   |    |  
 | boolean endOfTime(String)         |    |  
 | World endOfWorld(String)          |    |  
 |                                   |    |  
 |                                   |    |  
 | abstract World onTick()           |    |  
 | abstract World onKeyEvent(String) |    |  
 | abstract boolean draw()           |    |  
 +-----------------------------------+    |  
 					  |
					  v
    +---------------------------------------+
    | Canvas                                |
    +---------------------------------------+
    +---------------------------------------+
    | boolean show()                        |
    | boolean close()                       |
    | boolean drawCircle(Posn,int,IColor)   |
    | boolean drawDisk(Posn,int,IColor)     |
    | boolean drawRect(Posn,int,int,IColor) |
    | boolean drawLine(Posn,Posn,IColor)    |
    | boolean drawString(Posn,String)       |
    +---------------------------------------+}

Methods in these classes may fail due to the unavailability of the physical
devices, inappropriate uses, etc. In those cases, they fail with an
exception.

@section[#:tag "Jworld"]{World}

The abstract @tt{World} class exports the following methods. 

@; -----------------------------------------------------------------------------
@defthing[bigBang (int width,int height,double speed)]

Initializes the world, associates it with a @tt{width} x
@tt{height} @seclink["canvas"]{@tt{Canvas}}, displays
this canvas, enables keyevents, and finally starts the clock at a rate of
one tick per @tt{speed} seconds. If it succeeds with all of its
actions, the method produces @tt{true}.

@bold{Note}: @tt{width}, @tt{height} and 
@tt{speed} must be a positive. 

@; -----------------------------------------------------------------------------
The canvas in @tt{World} is called

  @tt{theCanvas}. 

 References to a "canvas" in conjunction with the @tt{World} class
 denote this default canvas. 

@; -----------------------------------------------------------------------------
@defthing[endOfTime ()]

Displays the given message, stops the clock and, if it succeeds, produces
@tt{true}. After the end of time, events no longer trigger calls
to @tt{onTick} or @tt{onKeyEvent}. The canvas remains visible. 

@; -----------------------------------------------------------------------------
@defthing[endOfWorld (String msg)]

Displays the given message, stops the clock and, if it succeeds, produces the 
last @tt{World}. After the end of the world,  events no longer trigger calls
to @tt{onTick} or @tt{onKeyEvent} (see below). The canvas
remains visible. 

@; -----------------------------------------------------------------------------
A derived concrete class must supply definitions for the following methods: 

@; -----------------------------------------------------------------------------
@defthing[onTick ()]

Invoked for every tick of the clock. Its purpose is to create a
@tt{World} whose differences with @tt{this} one represent
what happened during the amount of time it takes the clock to tick.  

@; -----------------------------------------------------------------------------
@defthing[onKeyEvent (String key)]

Invoked for every keyboard event associated with the canvas. Its purposes
  is to create a @tt{World} whose differences with
  @tt{this} one represent what happens due to the user's use of the
  keyboard. The latter is represented with the string-valued argument
  @tt{key}. 

@; -----------------------------------------------------------------------------
@defthing[draw ()]

Invoked @emph{after} one of the two event handlers has been called. Its
purpose is to present @tt{this World} graphically on its
canvas. If it succeeds, its result is @tt{true}.

A program may, in principle, start several instances of (subclasses of)
@tt{World}. If it does, the event handlers are called in a unpredictable
order. 

@; -----------------------------------------------------------------------------
@section[#:tag "canvas"]{Canvas}

To create an instance of the @tt{Canvas} class, a program must supply
two @tt{int} values: one for the width of the canvas and one for its
height. The canvas is a rectangle, whose borders are parallel to the computer
screen's borders. A program can use the following methods on instances of
@tt{Canvas}]

@defthing[show ()] 

Initializes the canvas to a white area, enables the drawing methods, and
  finally displays the canvas. If it succeeds, it produces
  @tt{true}. Invoking the method a second time without calling
  @tt{close} before has no effect.

@defthing[close ()]

Hides the canvas and erases the current content.  If it succeeds, it
produces @tt{true}.

Closing the Canvas using the display controls does not fully hide the
canvas; it is still necessary to invoke @tt{close} before
@tt{show} is re-enabled.

@defthing[drawCircle (Posn p,int r,IColor c)]

Draws a circle on @tt{this}Canvas] at @tt{p} with radius
@tt{r} and color @tt{c}.  If it succeeds, it produces
@tt{true}.

@defthing[drawDisk (Posn p,int r,IColor c)]

Draws a disk on @tt{this}Canvas] at @tt{p} with radius
@tt{r} and color @tt{c}.  If it succeeds, it produces
@tt{true}.

@defthing[drawRect (Posn p,int w,int h,IColor c)]

Draws a solid rectangle on @tt{this}Canvas] at @tt{p} with
width @tt{w}, height @tt{h}, and color @tt{c}.  The
rectangle's lines are parallel to the canvas's borders. If it succeeds, it
produces @tt{true}.

@defthing[drawLine (Posn p0,Posn p1,IColor c)]

Draws a line on @tt{this}Canvas] from @tt{p0} to
@tt{p1} using color @tt{c}.  If it succeeds, it produces
@tt{true}.

@defthing[drawString (Posn p,String s)]

Draws the string @tt{s} at @tt{p} on @tt{this}Canvas].  If it succeeds, it produces @tt{true}.
