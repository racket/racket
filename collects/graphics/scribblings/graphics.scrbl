#lang scribble/doc
@(require "common.ss"
          (for-label "../graphics.ss"
                     "../graphics-sig.ss"
                     "../graphics-unit.ss"
                     "../graphics-posn-less-unit.ss"))

@title{@bold{Graphics}: Legacy Library}

@defmodule[graphics/graphics]

The viewport graphics library is a relatively simple toolbox of
graphics commands. The library is not very powerful; it is intended as
a simplified alternative to @schememodname[scheme/gui]'s full
graphical toolbox.

The graphics library originated as SIXlib, a library of X Windows
commands available within Chez Scheme at Rice University. The
functionality of that library has been reproduced (with backward
compatibility) in this version.

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Basic Commands}

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

@section[#:tag "posn"]{Position Operations}

A position is a pixel location within a viewport.  The upper-left
corner is pixel @math{(0, 0)}, and positions increase to the right and
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

@section{Color Operations}

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

@section{Draw, Clear, and Flip Operations}

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

@subsection{Viewports}

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

@subsection{Pixels}

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

@subsection{Lines}

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

@subsection{Rectangles}

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

@subsection{Ellipses}

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

@subsection{Polygons}

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

@subsection{Strings}

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

@subsection{Pixmaps}

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

@section{World Operations}

Every canvas comes with an associated world. A client program can set
the world, start the world's clock, stop the world's clock, and deal
with tick events (the clock ticks) and keyboard inputs (keyevents).

@defproc[((init-world [viewport viewport?]) [v any/c]) void?]{

Sets the initial value of @scheme[viewport]'s world to @scheme[v].}

@defproc[((set-on-tick-event [viewport viewport?])
          [secs real?] [update-callback (any/c . -> . any/c)]) void?]{

For @scheme[viewport], sets @scheme[update-callback] to be invoked to
transform the world value every @scheme[secs] seconds. Only one
callback is installed at a time.}

@defproc[((stop-tick [viewport viewport?])) void?]{

Stops updating @scheme[viewport]'s world via a callback installed with
@scheme[set-on-tick-event].}

@defproc[((set-on-key-event [viewport viewport?])
          [key-callback (any/c any/c . -> . any/c)])
         void?]{

Sets @scheme[key-callback] as the function to call whenever a key
event is received for @scheme[viewport]. The @scheme[key-callback] is
given a key event and the current world, and it produces an updated
world.}

@; ----------------------------------------

@section{Miscellaneous Operations}

@defproc[((get-string-size [viewport viewport?]) [str string?]) (list/c real? real?)]{

Returns the size of @scheme[str] as drawn into @scheme[viewport] as a
list of two numbers: width and height.}

@defproc[(viewport->snip [viewport viewport?]) (is-a?/c snip%)] 

Returns an object that can be inserted into an editor buffer to
display the current image in the viewport. (Subsequent drawing to the
viewport does not affect the snip's image.)

When snips are the results of computations in the interactions window,
DrRacket shows the snip in the interactions window.}

@defproc[(viewport-dc [viewport viewport?]) (is-a?/c dc<%>)]{

Returns an object for direct drawing into @scheme[viewport]'s
on-screen representation (if any). Mirror all such drawing to the
result of @scheme[(viewport-buffer-dc viewport)], too.}

@defproc[(viewport-buffer-dc [viewport viewport?]) (is-a?/c dc<%>)]{

Returns an object for direct drawing into @scheme[viewport]'s
off-screen representation. Mirror all such drawing to the
result of @scheme[(viewport-dc viewport)], too.}

@; ----------------------------------------

@section{An Example}

@schemeblock[
(open-graphics)
(code:comment @#,t{nothing appears to happen, but the library is initialized...})

(define w (open-viewport "practice" 300 300))
(code:comment @#,t{viewport window appears})

((draw-line w) (make-posn 30 30) (make-posn 100 100))     
(code:comment @#,t{line appears})

(close-viewport w)
(code:comment @#,t{viewport disappears})

(close-graphics)
(code:comment @#,t{again, nothing appears to happen, but})
(code:comment @#,t{unclosed viewports (if any) would disappear})
]

@section{A More Complicated Example}

The use of multiple viewports, viewport descriptors, drawing
operations for multiple viewports is as easy as the use of a single
viewport: 

@schemeblock[
(open-graphics)
(let* ((code:comment @#,t{@scheme[w1] and @scheme[w2] are viewports for different windows})
       [w1  (open-viewport "viewport 1" 300 300)]
       [w2  (open-viewport "viewport 2" 200 500)]
       (code:comment @#,t{@scheme[d1] and @scheme[d2] draw lines in different viewports})
       [d1  (draw-line w1)]
       [d2  (draw-line w2)])
  (code:comment @#,t{draws a line in viewport labeled ``viewport 1''})
  (d1 (make-posn 100 5) (make-posn 5 100))
  (code:comment @#,t{draws a line in viewport labeled ``viewport 2''})
  (d2 (make-posn 100 100) (make-posn 101 400)))
 
(code:comment @#,t{we no longer have access to viewports 1 and 2,})
(code:comment @#,t{since their descriptors did not escape the @scheme[let]})
(close-graphics) 
(code:comment @#,t{removes the viewports})
]

@section{Protecting Graphics Operations}

To guarantee the proper closing of viewports in cases of errors,
especially when a program manages several viewports simultaneously, a
programmer should use @scheme[dynamic-wind:]

@schemeblock[
(let ([w (open-viewport "hello" 100 100)])
  (dynamic-wind
    (code:comment @#,t{what we want to happen first: nothing})
    void
    (code:comment @#,t{the main program (errors constrained to this piece)})
    (lambda () (draw-pixel 13))  (code:comment @#,t{an error})
    (code:comment @#,t{what we would like to happen, whether the main program})
    (code:comment @#,t{finishes normally or not})
    (lambda () (close-viewport w))))
]

@; ----------------------------------------

@section{Mouse Operations}

The graphics library contains functions that determine where the mouse
is, if there are any clicks, etc.  The functions
@scheme[get-mouse-click] and @scheme[ready-mouse-click] first return a
``mouse-click descriptor,'' and then other functions take the
descriptor and return the mouse's position, which button was pushed,
etc.  Mouse clicks are buffered and returned in the same order in
which they occurred.  Thus, the descriptors returned by
@scheme[get-mouse-click] and @scheme[ready-mouse-click] may be from
clicks that occurred long before these functions were called.

@defproc[(get-mouse-click [viewport viewport?]) mouse-click?]{

Returns the next mouse click in @scheme[viewport], waiting for a click
if necessary.}


@defproc[(ready-mouse-click [viewport viewport?])
         (or/c mouse-click? false/c)]{

Returns either a mouse click descriptor or @scheme[#f] if none is
available.  Unlike @scheme[get-mouse-click],
@scheme[ready-mouse-click] always returns immediately.}


@defproc[(ready-mouse-release [viewport viewport?]) 
         (or/c mouse-click? false/c)]{

Returns either a click descriptor from a mouse-release (button-up)
event or @scheme[#f] if none is available.}


@defproc[(query-mouse-posn [viewport viewport?]) (or/c posn? false/c)]{

Returns either the position of the mouse cursor within
@scheme[viewport] or else @scheme[#f] if the cursor is currently
outside @scheme[viewport].}


@defproc[(mouse-click-posn [mouse-click mouse-click?]) posn?]{

Returns the position of the pixel within a viewport where a given
mouse click occurred.}


@defproc[(left-mouse-click? [mouse-click mouse-click?]) boolean?]{

Returns @scheme[#t] if the mouse click occurred with the left mouse
button, @scheme[#f] otherwise.}

@defproc[(middle-mouse-click? [mouse-click mouse-click?]) boolean?]{

Returns @scheme[#t] if the mouse click occurred with the middle mouse
button, @scheme[#f] otherwise.}

@defproc[(right-mouse-click? [mouse-click mouse-click?]) boolean?]{

Returns @scheme[#t] if the mouse click occurred with the right mouse
button, @scheme[#f] otherwise.}

@; ----------------------------------------

@section{Keyboard Operations}

The graphics library contains functions that report key presses from
the keyboard.  The functions @scheme[get-key-press] and
@scheme[ready-key-press] return a ``key-press descriptor,'' and then
@scheme[key-value] takes the descriptor and returns a character or
symbol (usually a character) representing the key that was pressed.
Key presses are buffered and returned in the same order in which they
occurred.  Thus, the descriptors returned by @scheme[get-key-press]
and @scheme[ready-key-press] may be from presses that occurred long
before these functions were called.

@defproc[(get-key-press [viewport viewport?]) key-press?]{

Returns the next key press in the @scheme[viewport], waiting for a
key press if necessary.}

@defproc[(ready-key-press [viewport viewport?]) key-press?]{

Returns the next key press in the @scheme[viewport] or returns
@scheme[#f] if none is available. Unlike @scheme[get-key-press],
@scheme[ready-key-press] always returns immediately.}

@defproc[(key-value [key-press key-press?]) (or/c character? symbol?)]{

Returns a character or special symbol for the key that was
pressed. For example, the Enter key generates @scheme[#\return], and the
up-arrow key generates @scheme['up].  For a complete list of possible
return values, see @method[key-event% get-key-code].}

@; ----------------------------------------

@section{Flushing}

@defproc[(viewport-flush-input [viewport viewport?]) void?]{

Empties all mouse and keyboard events in the input buffer of
@scheme[viewport].}

@; ----------------------------------------

@section{Graphics Library as a Unit}

@subsection{Signatures}

@defmodule[graphics/graphics-sig]

@defsignature[graphics^ ()]

Includes all of the bindings defined earlier in this chapter, except
the @scheme[posn] bindings of @secref["posn"].


@defsignature[graphics:posn^ ()]

Includes the @scheme[posn] bindings of @secref["posn"].


@subsection{Unit with @scheme[posn]}

@defmodule[graphics/graphics-unit]

@defthing[graphics@ unit?]{

Imports @scheme[mred^] and exports both @scheme[graphics^] and
@scheme[graphics:posn^].}

@subsection{Unit without @scheme[posn]}

@defmodule[graphics/graphics-posn-less-unit]

@defthing[graphics-posn-less@ unit?]{

Imports @scheme[mred^] and @scheme[graphics:posn^] and exports
@scheme[graphics^].}
