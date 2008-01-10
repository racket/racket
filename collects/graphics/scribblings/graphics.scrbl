#lang scribble/doc
@(require scribble/manual
          (for-label scheme/base
                     scheme/contract
                     "../graphics.ss"))

@title{@bold{Graphics}: Legacy Library}

@table-of-contents[]

@section[#:style 'toc]{Viewport Graphics}

The viewport graphics library is a relatively simple toolbox of
graphics commands. The library is not very powerful; it is intended as
a simplified alternative to @schememodname[scheme/gui]'s full
graphical toolbox.

The graphics library originated as SIXlib, a library of X Windows
commands available within Chez Scheme at Rice University. The
functionality of that library has been reproduced (with backward
compatibility) in this version.

@defmodule[graphics/graphics]

@local-table-of-contents[]

@; ----------------------------------------------------------------------

@subsection{Basic Commands}

@defproc[(open-graphics) void?]{

Initializes the library's graphics routines. It must be called before
 any other graphics operations.}

@defproc[(close-graphics) void?]{

Closes all of the windows. Until @scheme[open-graphics] is called
again, no graphics routines will work.}

@defproc*[([(open-viewport [name string?]
                           [horiz exact-nonnegative-integer?]
                           [vert exact-nonnegative-integer?])
            viewport?]
           [(open-viewport [name string?]
                           [dimensions posn?])
            viewport?])]{

Creates a new window called @scheme[name].  The window is
@scheme[horiz] pixels wide and @scheme[vert] pixels high.  For
backward compatibility, a single @scheme[posn] value can be submitted
in the place of @scheme[horiz] and @scheme[vert].  The result is a
viewport descriptor.}


@defproc*[([(open-pixmap [name string?]
                         [horiz exact-nonnegative-integer?]
                         [vert exact-nonnegative-integer?])
            viewport?]
           [(open-pixmap [name string?]
                         [dimensions posn?])
            viewport?])]{

Like @scheme[open-viewport], but the resulting viewport is not
displayed on the screen. Offscreen pixmaps are useful for executing a
sequence of drawing commands and displaying them all at once with
@scheme[copy-viewport].
  
Offscreen pixmaps are also useful in conjunction with viewport->snip
(see below). This allows functions to compute with graphical objects
and view the graphics when results are returned to the interactions
window.}


@defproc[(close-viewport [viewport viewport?]) void?]{

Removes the viewport from the screen and makes subsequent operations
dealing with the viewport illegal.}


@defproc[(viewport? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a viewport (i.e., a destination
for drawing), @scheme[#f] otherwise.}

@; ----------------------------------------------------------------------

@subsection{Position Operations}

A position is a pixel location within a viewport.  The upper-left
corner is pixel @math{(0, 0)}, and positions increase to the left and
down.


@defstruct[posn ([x real?][y real?])]{

Represents a positions.}


@defproc[((get-pixel [viewport viewport?]) [p posn?]) (one-of/c 0 1)]{

Returns the color of the pixel at position @scheme[p] in
@scheme[viewport]; @scheme[0] denotes white and @scheme[1] denotes not
white.}


@defproc[((get-color-pixel [viewport viewport?]) [p posn?]) rgb?]{

Returns an @scheme[rgb] value for color of the pixel at position
@scheme[p] in @scheme[viewport].}


@defproc[((test-pixel [viewport viewport?]) [color (or/c (integer-in 0 299)
                                                         string?
                                                         rgb?)])
         rgb?]{

Returns the color that will actually be used if @scheme[color] is used
to draw.}

@; ----------------------------------------------------------------------

@subsection{Color Operations}

A color can be represented in three ways: as a color index (an integer
in 0 to 299, inclusive), as a color name string, or as a @scheme[rgb]
value. All drawing functions which take a color argument accept colors
in any form. An @scheme[rgb] value is assigned to an index with
@scheme[change-color].


@defstruct[rgb ([red (real-in 0 1)][green (real-in 0 1)][blue (real-in 0 1)])]{

Takes three values in the range 0 (dark) to 1 (bright) and returns an
@scheme[rgb] (a color).}


@defproc[(change-color [index (integer-in 0 299)] [rgb rgb?]) 
         void?]{

Changes the color at @scheme[index] in the color table to the
color specified in @scheme[rgb]. Only the first twenty-one indices
are initialized; a color index should not be used until it has
been initialized.}


@defproc[(default-display-is-color?) boolean?]{

Returns @scheme[#t] if the default display screen for viewports is in
color or @scheme[#f] otherwise.}

@; ----------------------------------------------------------------------

@subsection{Draw, Clear, and Flip Operations}

The following are the basic graphics operations for drawing to a
viewport.  Each function takes a viewport as its argument and returns
a function operating within that viewport.  Further arguments, if any,
are curried.  For example, @scheme[(draw-line _viewport)] returns a
function, that can then be applied to the proper arguments to draw a
line in the viewport corresponding to viewport descriptor
@scheme[_viewport].

In general, @schemeidfont{draw-} functions make pixels black or
colored, @schemeidfont{clear-} functions make them white, and
@schemeidfont{flip-} commands @deftech{invert} pixels (which makes
black white, white black, and is otherwise ill-defined).

@subsubsection{Viewports}

@defproc[((draw-viewport [viewport viewport?])
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{
       
Colors the entire contents of @scheme[viewport] with @scheme[color].}

@defproc[((clear-viewport [viewport viewport?]))
         void?]{
       
Whitens the entire contents of @scheme[viewport].}

@defproc[((flip-viewport [viewport viewport?]))
         void?]{
       
@tech{Inverts} the entire contents of @scheme[viewport].}


@defproc[(copy-viewport [source viewport?] [dest viewport?])
         void?]{

Copies the content of @scheme[source] into @scheme[dest].}

@; ----------------------------------------

@subsubsection{Pixels}

@defproc[((draw-pixel [viewport viewport?]) 
          [p posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Colors the pixel in @scheme[viewport] at @scheme[p].}

@defproc[((clear-pixel [viewport viewport?]) 
          [p posn?])
         void?]{

Whitens the pixel in @scheme[viewport] at @scheme[p].}

@defproc[((flip-pixel [viewport viewport?]) 
          [p posn?])
         void?]{

@tech{Inverts} the pixel in @scheme[viewport] at @scheme[p].}

@; ----------------------------------------

@subsubsection{Lines}

@defproc[((draw-line [viewport viewport?]) 
          [p1 posn?]
          [p2 posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a line in @scheme[viewport] connecting positions @scheme[p1] and
@scheme[p2].}


@defproc[((clear-line [viewport viewport?]) 
          [p1 posn?]
          [p2 posn?])
         void?]{

Whitens a line in @scheme[viewport] connecting positions @scheme[p1]
and @scheme[p2].}


@defproc[((flip-line [viewport viewport?]) 
          [p1 posn?]
          [p2 posn?])
         void?]{

@tech{Inverts} a line in @scheme[viewport] connecting positions
@scheme[p1] and @scheme[p2].}

@; ----------------------------------------

@subsubsection{Rectangles}

@defproc[((draw-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a rectangle border in the @scheme[viewport] with the top-left of
the rectangle at the position @scheme[p] and with sides @scheme[width]
across and @scheme[height] tall.}


@defproc[((clear-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a rectangle border in the @scheme[viewport], analogous to
@scheme[draw-rectangle].}


@defproc[((flip-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a rectangle border in the @scheme[viewport], analogous
to @scheme[draw-rectangle].}


@defproc[((draw-solid-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a solid rectangle in the @scheme[viewport] with the top-left of
the rectangle at the position @scheme[p] and with sides @scheme[width]
across and @scheme[height] tall.}


@defproc[((clear-solid-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a rectangle border in the @scheme[viewport], analogous to
@scheme[draw-solid-rectangle].}


@defproc[((flip-solid-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a rectangle border in the @scheme[viewport], analogous
to @scheme[draw-solid-rectangle].}

@; ----------------------------------------

@subsubsection{Ellipses}

@defproc[((draw-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a ellipse border in the @scheme[viewport]. The ellipse is
inscribed with a rectangle whose top-left is at position @scheme[p]
and with sides @scheme[width] across and @scheme[height] tall.}


@defproc[((clear-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a ellipse border in the @scheme[viewport], analogous to
@scheme[draw-ellipse].}


@defproc[((flip-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a ellipse border in the @scheme[viewport], analogous
to @scheme[draw-ellipse].}


@defproc[((draw-solid-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a solid ellipse in the @scheme[viewport]. The ellipse is
inscribed with a rectangle whose top-left is at position @scheme[p]
and with sides @scheme[width] across and @scheme[height] tall.}


@defproc[((clear-solid-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a ellipse border in the @scheme[viewport], analogous to
@scheme[draw-solid-ellipse].}


@defproc[((flip-solid-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a ellipse border in the @scheme[viewport], analogous
to @scheme[draw-solid-ellipse].}

@; ----------------------------------------

@subsubsection{Polygons}

@defproc[((draw-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a polygon border in @scheme[viewport] using @scheme[points] for
the polygon vertices and @scheme[offset] as an offset added to all
points.}


@defproc[((clear-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

Whitens a polygon border in @scheme[viewport], analogous to
@scheme[draw-polygon].}


@defproc[((flip-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

@tech{Inverts} a polygon border in @scheme[viewport], analogous to
@scheme[draw-polygon].}


@defproc[((draw-solid-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a solid polygon in @scheme[viewport] using @scheme[points] for
the polygon vertices and @scheme[offset] as an offset added to all
points.}


@defproc[((clear-solid-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

Whitens a polygon border in @scheme[viewport], analogous to
@scheme[draw-solid-polygon].}


@defproc[((flip-solid-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

@tech{Inverts} a polygon border in @scheme[viewport], analogous to
@scheme[draw-solid-polygon].}

@; ----------------------------------------

@subsubsection{Strings}

@defproc[((draw-string [viewport viewport?]) 
          [p posn?]
          [str string?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a string at a specified location in the @scheme[viewport].
The lower left of the string begins at @scheme[p].}


@defproc[((clear-string [viewport viewport?]) 
          [p posn?]
          [str string?])
         void?]{

Whitens a string at a specified location in the @scheme[viewport].
The lower left of the string begins at @scheme[p].}

@defproc[((flip-string [viewport viewport?]) 
          [p posn?]
          [str string?])
         void?]{

@tech{Inverts} a string at a specified location in the
@scheme[viewport].  The lower left of the string begins at
@scheme[p].}

@; ----------------------------------------

@subsubsection{Pixmaps}

@defproc[(((draw-pixmap-posn [file path-string?] 
                             [type (one-of/c 'unknown 'unknown/mask 
                                             'gif 'gif/mask 'jpeg 'png 'png/mask 
                                             'xbm 'xpm 'bmp 'pict)
                                   'unknown/mask])
           [viewport viewport?])
          [p posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a pixmap into @scheme[viewport] with its upper left corner at
position @scheme[p]. If @scheme[type] is @scheme['unknown] or
@scheme['unknown/mask], then the content of the file is examined to
determine the type.  All formats are supported on all platforms,
except @scheme['pict] which is only supported under Mac OS X.  The
@scheme['gif/mask], @scheme['png/mask], and @scheme['unknown/mask]
types draw the bitmap with a transparent background if
@scheme[filename] refers to a GIF/PNG file with a transparent
background.

The argument @scheme[color] is only used when the loaded pixmap is
monochrome. In that case, the color is used instead of black in the
drawn image.}

@defproc[((draw-pixmap [viewport viewport?])
          [file path-string?]
          [p posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Equivalent to @scheme[(((draw-pixmap-posn file) viewport) p color)].}

@defproc[((save-pixmap [viewport viewport?])
          [file path-string?]
          [type (one-of/c 'gif 'jpeg 'png 'xbm 'xpm 'bmp) 'xpm])
         void?]{

Saves the current content of @scheme[viewport] to @scheme[file].
The @scheme[type] argument determines the kind of file that is written.}

@; ----------------------------------------

@;{

@subsection{World Operations}

Every canvas comes with an associated world. A client program can set the world,
start the world's clock, stop the world's clock, and deal with tick events (the
clock ticks) and keyboard inputs (keyevents). 

@itemize{
@item{\Function{init-world}
  @scheme[((init-world @scheme[viewport]) X)] 
Takes a viewport descriptor.  It returns a function whose input becomes the
initial value of the world associated with this canvas.}

@item{\Function{set-on-tick-event}
  @scheme[((set-on-tick-event @scheme[viewport]) number @scheme[unary procedure])] 
Takes a viewport descriptor.  It returns a function whose first input is a
  number and the second one is a function from worlds to worlds. The number
  determines how frequently the clock ticks. The given function is called for
  every clock tick on the current world; the result becomes the next world.} 

@item{\Function{stop-tick}
  @scheme[((stop-tick @scheme[viewport]))] 
Takes a viewport descriptor.  It returns a function of no arguments that can
  stop the clock for this canvas's world.} 

@item{\Function{set-on-key-event} 
  @scheme[((set-on-key-event @scheme[viewport]) unary-procedure)]
Takes a viewport descriptor.  It returns a function whose input becomes the
  keyevent callback function. This callback consumes the latest keyevent and the
  current world and a keyevent; it produces the next world.}
}

@subsection{Miscellaneous Operations}

@itemize{
@item{\Function{get-string-size}
  @scheme[((get-string-size @scheme[viewport]) string)] 
 Takes a viewport descriptor.  It returns a
function that returns the size of a string as a list of two numbers:
the width and height.}

@item{\Function{viewport->snip}
  @scheme[(viewport->snip @scheme[viewport])] 
 Takes a viewport descriptor.  It returns an
object that can be inserted into an editor buffer to display the
current image in the viewport. (Subsequent drawing to the viewport
does not affect the snip's image.)

When snips are the results of computations in the interactions window, DrScheme will print show the contents of the viewport, right in the interactions window.}

@item{\Function{viewport-dc}
  @scheme[(viewport-dc @scheme[viewport])] 
 Takes a viewport descriptor.  It returns an
object that can be used with the primitive MrEd toolbox
functions to draw into the viewport's on-screen representation
(if any). Mirror all such drawing to the result of 
@scheme[(viewport-offscreen-dc @scheme[viewport])], too.}

@item{\Function{viewport-offscreen-dc}
  @scheme[(viewport-offscreen-dc @scheme[viewport])] 
 Takes a viewport descriptor.  It returns an
 object that can be used with the primitive MrEd toolbox
 functions to draw into the viewport's off-screen representation.
 Mirror all such drawing to the result of 
 @scheme[(viewport-dc @scheme[viewport])], too.}
}

@subsection{An Example}

@schemeblock[
(open-graphics)
;; nothing appears to happen, but the library is initialized...

(define w (open-viewport "practice" 300 300))
;; viewport appears

((draw-line w) (make-posn 30 30) (make-posn 100 100))     
;; line appears

(close-viewport w)
;; viewport disappears

(close-graphics)
;; again, nothing appears to happen, but
;; unclosed viewports (if any) would disappear
]

@subsection{A More Complicated Example}

The use of multiple viewports, viewport descriptors, drawing
operations for multiple viewports is as easy as the use of a single
viewport: 

@schemeblock[
(open-graphics)
(let* (;; @scheme[w1] and @scheme[w2] are viewport descriptors for different windows
       [w1  (open-viewport "viewport 1" 300 300)]
       [w2  (open-viewport "viewport 2" 200 500)]
       ;; d1 and d2 are functions that draw lines in different viewports
       [d1  (draw-line w1)]
       [d2  (draw-line w2)])
  ;; draws a line in viewport labeled "viewport 1"
  (d1 (make-posn 100 5) (make-posn 5 100))
  ;; draws a line in viewport labeled "viewport 2"
  (d2 (make-posn 100 100) (make-posn 101 400)))
 
;; we no longer have access to viewports 1 and 2, 
;; since their descriptors did not escape the @scheme[let]
(close-graphics) 
;; removes the viewports
]

@subsection{Protecting Graphics Operations}

To guarantee the proper closing of viewports in cases of errors,
especially when a program manages several viewports simultaneously, a
programmer should use @scheme[dynamic-wind:]

@schemeblock[
(let ([w (open-viewport "hello" 100 100)])
  (dynamic-wind
    ;; what we want to happen first: nothing
    void
    ;; the main program (errors constrained to this piece)
    (lambda () (draw-pixel 13))  ; an error
    ;; what we would like to happen, whether the main program finishes 
    ;; normally or not
    (lambda () (close-viewport w))))
]

@subsection{Mouse Operations}

The graphics library contains functions that determine where the
mouse is, if there are any clicks, etc.
The functions @scheme[get-mouse-click] and @scheme[ready-mouse-click] first
return a ``mouse-click descriptor,'' and then other functions take
the descriptor and return the mouse's position, which button was
pushed, etc.
Mouse clicks are buffered and returned in the same order in which
they occurred.
Thus, the descriptors returned by @scheme[get-mouse-click] and 
@scheme[ready-mouse-click] may be from clicks that occurred long
before these functions were called.

@itemize{
@item{\Function{get-mouse-click}
  @scheme[(get-mouse-click @scheme[viewport])] 
 Takes a viewport descriptor and returns
a mouse click descriptor. 
It returns the next mouse click in the @scheme[viewport], waiting for a click 
if necessary.}

@item{\Function{ready-mouse-click}
  @scheme[(ready-mouse-click @scheme[viewport])] 
 Takes a viewport descriptor and returns
either a mouse click descriptor, or else @scheme[#f] if none is available.
Unlike the previous function, @scheme[ready-mouse-click] returns immediately.}

@item{\Function{ready-mouse-release}
  @scheme[(ready-mouse-release @scheme[viewport])] 
 Takes a viewport descriptor and returns
either a click descriptor from a mouse-release (button-up) event,
or else @scheme[#f] if none is available.}

@item{\Function{query-mouse-posn}
  @scheme[(query-mouse-posn @scheme[viewport])] 
 Takes a viewport descriptor and returns
either the position of the mouse cursor within the @scheme[viewport], 
or else @scheme[#f] if the cursor is currently outside the @scheme[viewport].}

@item{\Function{mouse-click-posn}
  @scheme[(mouse-click-posn @scheme[mouse-click])] 
 Takes a mouse click descriptor and
returns the position of the pixel where the click occurred.}

@item{\Function{left-mouse-click?}
  @scheme[(left-mouse-click?\ @scheme[mouse-click])]  
 Takes a mouse click descriptor and returns
@scheme[#t] if the click occurred with the left mouse button,
or else @scheme[#f].}

@item{\Function{middle-mouse-click?}
  @scheme[(middle-mouse-click?\ @scheme[mouse-click])] 
Similar to @scheme[left-mouse-click?].}

@item{\Function{right-mouse-click?}
  @scheme[(right-mouse-click?\ @scheme[mouse-click])] 
Similar to @scheme[left-mouse-click?].}
}

@subsection{Keyboard Operations}

The graphics library contains functions that report key presses from
 the keyboard.  The functions @scheme[get-key-press] and
 @scheme[ready-key-press] return a ``key-press descriptor,'' and then
 @scheme[key-value] takes the descriptor and returns a character or
 symbol (usually a character) representing the key that was pressed.
 Key presses are buffered and returned in the same order in which they
 occurred.  Thus, the descriptors returned by @scheme[get-key-press] and
 @scheme[ready-key-press] may be from presses that occurred long before
 these functions were called.

@itemize{
@item{\Function{get-key-press}
  @scheme[(get-key-press @scheme[viewport])] 
 Takes a viewport descriptor and returns
a key press descriptor. 
It returns the next key press in the @scheme[viewport], waiting for a click 
if necessary.}

@item{\Function{ready-key-press}
  @scheme[(ready-key-press @scheme[viewport])] 
 Takes a viewport descriptor and returns
either a key press descriptor, or else @scheme[#f] if none is available.
Unlike the previous function, @scheme[ready-key-press] returns immediately.}

@item{\Function{key-value}
  @scheme[(key-value @scheme[key-press])]  

 Takes a key press descriptor and returns a character or special
 symbol for the key that was pressed. For example, the Enter key
 generates \scmch{return}, and the up-arrow key generates @scheme['up].
 For a complete list of possible return values, see {\MrEdManual}.} 
}

@subsection{Flushing}

@itemize{
@item{\Function{viewport-flush-input}
  @scheme[(viewport-flush-input @scheme[viewport])] 
As noted above, key presses and mouse clicks are buffered.
@scheme[viewport-flush-input] takes a viewport descriptor 
and empties the input buffer of mouse and keyboard events.}
}

@subsection{Unitized Graphics}

To use a unitized version of the graphics library (see {\MzLibManual}
 for more information on units), get the signatures
 \scmsigfirst{graphics}, \scmsigfirst{graphics:posn-less}, and
 \scmsigfirst{graphics:posn} with:

@schemeblock[
(require (libKW "graphics-sig.ss" "graphics"))
]

The \scmsig{graphics} signature includes all of the names defined in
 this chapter. The \scmsig{graphics:posn-less} signature contains
 everything except the \scm{posn} structure information, and
 \scmsig{graphics:posn} contains only the \scm{posn} structure.

To obtain \scmunitfirst{graphics}, which imports \scmsig{mred} (all of
 the MrEd classes, functions, and constants) and exports
 \scmsig{graphics}:

@schemeblock[
(require (libKW "graphics-unit.ss" "graphics"))
]

The @filepath{graphics-posn-less-unit.ss} library provides
 \scmunit{graphics-posn-less}, which imports \scmsig{graphics:posn} in
 addition to MrEd.

@; ======================================================================

@section[#:tag "misc:turtles"]{Turtles}

@subsection{Traditional Turtles}

There are two ways to use the turtles in DrScheme. You can
use it as a TeachPack (see the DrScheme manual for details
of TeachPacks) or as a library. Use the
@filepath{turtles.ss} TeachPack.

In the MrEd language or in a module, load turtles with

@schemeblock[
(require (libKW "turtles.ss" "graphics"))
]

The following are the turtle functions:

@itemize{
@item{{@scheme[(turtles b)]}\Function{turtles}
  shows and hides the turtles window based on the boolean @scheme[b].
  The parameter @scheme[b] is optional; if it is left out, it toggles the
  state of the turtles.}

@item{{{@scheme[(move n)]}}\Function{move}
  moves the turtle n pixels.}

@item{{{@scheme[(draw n)]}}\Function{draw}
  moves the turtle n pixels and draws a line on that path.}

@item{{{@scheme[(erase n)]}}\Function{erase}
  moves the turtle n pixels and erases along that path.

\Function{move-offset}
\Function{draw-offset}
\Function{erase-offset}}
@item{{{ @scheme[(move-offset h v)], @scheme[(draw-offset h v)], @scheme[(erase-offset h v)]}}
  are just like move, draw and erase, except they take a horizontal and
  vertical offset from the turtle's current position.}

@item{{ @scheme[(turn theta)]}\Function{turn}
  turns the turtle theta degrees counter-clockwise.}

@item{{ @scheme[(turn/radians theta)]}\Function{turn/radians}
  turns the turtle theta radians counter-clockwise.}

@item{{{@scheme[(clear)]}}\Function{clear}
  erases the turtles window.}
}

Turtles also defines these syntactic forms:

@itemize{
@item{{{@scheme[(split E)]}}\Function{split}
  spawns a new turtle where
  the turtle is currently located. In order to distinguish the two turtles,
  only the new one evaluates the expression E. For example, if you start
  with a fresh turtle-window and type:

\begin{center}
\begin{schemebox}
(split (turn/radians (/ pi 2)))
\end{schemebox}
\end{center}

  you will have two turtles, pointing at right angles to each other.
  To see that, try this:

\begin{center}
\begin{schemebox}
(draw 100)
\end{schemebox}
\end{center}

  You will see two lines. Now, if you evaluate those two expression
  again, you will have four turtles, etc}

@item{{{@scheme[(split* E ...)]}}\Function{split*}
  is similar to @scheme[(split E ...)], except it creates as many turtles as
  there are expressions and each turtles does one of the expression. For
  example, to create two turtles, one pointing at $\pi/2$ and one at
  $\pi/3$, evaluate this:

\begin{center}
\begin{schemebox}
(split* (turn/radians (/ pi 3)) (turn/radians (/ pi 2)))
\end{schemebox}
\end{center}}

@item{{{@scheme[(tprompt E...)]}}\Function{tprompt}
  provides a way to limit the splitting of the turtles. Before
  the expression E is run, the state of the turtles (how many, their
  positions and headings) is "checkpointed," then E is evaluated and
  the state of the turtles is restored, but all drawing that may have
  occurred during execution of E remains. 

  For example, if you do this:

\begin{center}
\begin{schemebox}
(tprompt (draw 100))
\end{schemebox}
\end{center}

  the turtle will move forward 100 pixels, draw a line there and then
  be immediately put back in it's original position. Also, if you do this:

\begin{center}
\begin{schemebox}
(tprompt (split (turn/radians (/ pi 2))))
\end{schemebox}
\end{center}

  the turtle will split into two turtles, one will turn 90 degrees and then
  the turtles will be put back into their original state -- as if the split
  never took place.

  The fern functions below demonstrate more advanced use of @scheme[tprompt].}
}

In the file @filepath{turtle-examples.ss} in the @filepath{graphics} library of your PLT
 distribution, you will find these functions and values defined, as
 example turtle programs. (The file is located in the @filepath{graphics}
 subdirectory of the @filepath{collects} subdirectory of the PLT
 distribution).

@itemize{
@item{{{@scheme[(regular-poly sides radius)]}}
  draws a regular poly centered at the turtle with 
  sides @scheme[sides] and with radius @scheme[radius].}

@item{{{@scheme[(regular-polys sides s)]}}
  draws s regular polys spaced evenly outwards with sides @scheme[sides].}

@item{{{@scheme[(radial-turtles  n)]}}
  places $2^n$ turtles spaced evenly pointing radially outward} 

@item{{{@scheme[(spaced-turtles n)]}}
  places $2^n$ turtles pointing in the same direction as the original turtle
  evenly spaced in a line.}

@item{{{@scheme[(spokes)]}}
  draws some spokes, using radial-turtles and spaced-turtles}

@item{{{@scheme[(spyro-gyra)]}}
  draws a spyro-grya reminiscent shape}

@item{{{@scheme[(neato)]}}
  as the name says\ldots}

@item{{{@scheme[(graphics-bexam)]}}
  draws a fractal that came up on an exam I took.}

@item{{{@scheme[serp-size]}}
  a constant which is a good size for the serp procedures

\index{Serpinski Triangle}}
@item{{@scheme[(serp serp-size)], @scheme[(serp-nosplit serp-size)]}
  draws the Serpinski triangle in two different ways, the first using split
  heavily. After running the first one, try executing
 @scheme[(draw 10)].}

@item{{{@scheme[koch-size]}}
  a constant which is a good size for the koch procedures

\index{Koch Snowflake}}
@item{{{@scheme[(koch-split koch-size)],@scheme[(koch-draw koch-size)]}}
  draws the same koch snowflake in two different ways.

\index{Lorenz Attractor}
\index{Butterfly Attractor}}
@item{{{@scheme[(lorenz a b c)]}}
  watch the lorenz "butterfly" attractor with initial values a b and c.}

@item{{{@scheme[(lorenz1)]}}
  a good setting for the lorenz attractor

\index{Peano space-filling curve}}
@item{{@scheme[(peano1 peano-size)]}

This will draw the Peano space-filling curve, using split.}

@item{{@scheme[(peano2 peano-size)]}

This will draw the Peano space-filling curve, without using split.

\index{Fern Fractal}}
@item{{{@scheme[fern-size]}}
  a good size for the fern functions}

@item{{{@scheme[(fern1 fern-size)]}}
  You will probably want to point the turtle up before running
  this one, with something like:

\begin{center}
@scheme[(turn/radians (- (/ pi 2)))]
\end{center}}

@item{{{@scheme[(fern2 fern-size)]}}
  a fern -- you may need to backup a little for this one.}

}

@subsection{Value Turtles}

There are two ways to use the turtles in DrScheme. You can
use it as a TeachPack (see the DrScheme manual for details
of TeachPacks) or as a library. Use the
@filepathFirst{value-turtles.ss} TeachPack.

In the MrEd language or in a module, load turtles with

@schemeblock[
(require (libKW "value-turtles.ss" "graphics"))
]


The value turtles are a variation on the turtles library.
Rather than having just a single window where each operation
changes the state of that window, in this library, the
entire turtles window is treated as a value. This  means
that each of the primitive operations accepts, in addition
to the usual arguments, a turtles window value and instead
of returning nothing, returns a turtles window value.

The following are the value turtle functions:

@itemize{
@item{{@scheme[(turtles number number [number number number])]}\FunctionK{turtles}{turtlesVal}
  creates a new turtles window. The first two arguments are
  the width and height of the turtles window. The remaining
  arguments specify the x,y position of the initial turtle
  and the angle. The default to a turtle in the middle of
  the window, pointing to the right.}

@item{{{@scheme[(move n turtles)]}}\FunctionK{move}{moveVal}
  moves the turtle n pixels, returning a new turtles window.}

@item{{{@scheme[(draw n turtles)]}}\FunctionK{draw}{drawVal}
  moves the turtle n pixels and draws a line on that path, 
  returning a new turtles window.}

@item{{{@scheme[(erase n turtles)]}}\FunctionK{erase}{eraseVal}
  moves the turtle n pixels and erases along that path,
  returning a new turtles window.

\FunctionK{move-offset}{move-offsetVal}
\FunctionK{draw-offset}{draw-offsetVal}
\FunctionK{erase-offset}{erase-offsetVal}}
@item{{{@scheme[(move-offset h v turtles)],
       @scheme[(draw-offset h v turtles)],
       @scheme[(erase-offset h v turtles)]}}
  are just like move, draw and erase, except they take a horizontal and
  vertical offset from the turtle's current position.}

@item{{ @scheme[(turn theta turtles)]}\FunctionK{turn}{turnVal}
  turns the turtle theta degrees counter-clockwise,
  returning a new turtles window.}

@item{{ @scheme[(turn/radians theta)]}\FunctionK{turn/radians}{turn/radiansVal}
  turns the turtle theta radians counter-clockwise,
  returning a new turtles window.}

@item{{@scheme[(merge turtles turtles)]}\Function{merge}
    
    The @scheme[split] and @scheme[tprompt] functionality
    provided by the imperative turtles implementation aren't
    needed for this, since the turtles window is itself a
    value. 
    
    Instead, the @scheme[merge] accepts two turtles windows
    and combines the state of the two turtles windows into a
    single window. The new window contains all of the
    turtles of the previous two windows, but only the line
    drawings of the first turtles argument.}

}

In the file @filepath{value-turtles-examples.ss} in the
@filepath{graphics} library of your PLT distribution, you will
find these functions and values defined, as example turtle
programs. (The file is located in the @filepath{graphics}
subdirectory of the @filepath{collects} subdirectory of the PLT
distribution).

It contains a sampling of the examples from the normal
turtles implementation, but translated to use @scheme[merge]
and the values turtles.

}
