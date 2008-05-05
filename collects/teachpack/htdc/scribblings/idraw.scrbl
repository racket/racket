#lang scribble/doc

@(require scribble/manual (for-label teachpack/htdc/draw))

@title[#:tag "iJdraw"]{Draw: idraw.*}

Add 
@verbatim[#:indent 3]{
  import idraw.*
}
at the top of your Definitions Window to import this library. 


This package provides stateful classes and imperative methods for a visual
world. Here is its class diagram of public fields and methods:
@verbatim[#:indent 3]{
import colors.*; 
import geometry.*; 

  +---------------------------------+     
  | abstract World                  |
  +---------------------------------+       
  | Canvas theCanvas                |---+
  +---------------------------------+   |   
  | void bigBang(int,int,double)    |   |   
  | World endOfTime(String)         |   |   
  | World endOfWorld(String)        |   |   
  | abstract void onTick()          |   |   
  | abstract void onKeyEvent(String)|   |   
  | abstract void draw()            |   |   
  +---------------------------------+   |   
                                        |   
					v
          +------------------------------------+
	  | Canvas                             |
	  +------------------------------------+
	  +------------------------------------+
	  | void show()                        |
	  | void close()                       |
	  | void drawCircle(Posn,int,IColor)   |
	  | void drawDisk(Posn,int,IColor)     |
	  | void drawRect(Posn,int,int,IColor) |
	  | void drawLine(Posn,Posn,IColor)    |
	  | void drawString(Posn,String)       |
	  +------------------------------------+
}

The abstract @tt{World} class in @tt{idraw} provides the same methods as
the @tt{World} class in @secref["Jworld"] (@tt{draw} package). Their return
values are usually @tt{void}, however, except for @tt{endOfTime} and
@tt{endOfWorld}, which continue to return the last world.

In an analogous manner, the methods in the @tt{Canvas} class export
the same methods as the @tt{Canvas} class in @secref["canvas"]
(@tt{draw} package). Again their return values are @tt{void}. 

