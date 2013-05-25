#lang scribble/doc
@(require "common.rkt"
          (for-label "../graphics.rkt"
                     "../graphics-sig.rkt"
                     "../graphics-unit.rkt"
                     "../graphics-posn-less-unit.rkt"))

@title{Graphics: Legacy Library}

@defmodule[graphics/graphics]

The viewport graphics library is a relatively simple toolbox of
graphics commands. The library is not very powerful; it is intended as
a simplified alternative to @racketmodname[racket/gui]'s full
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

Closes all of the windows. Until @racket[open-graphics] is called
again, no graphics routines will work.}

@defproc[(graphics-open?) boolean?]{
   Determines if the graphics have been opened (or not).
}

@defproc*[([(open-viewport [name string?]
                           [horiz exact-nonnegative-integer?]
                           [vert exact-nonnegative-integer?])
            viewport?]
           [(open-viewport [name string?]
                           [dimensions posn?])
            viewport?])]{

Creates a new window called @racket[name].  The window is
@racket[horiz] pixels wide and @racket[vert] pixels high.  For
backward compatibility, a single @racket[posn] value can be submitted
in the place of @racket[horiz] and @racket[vert].  The result is a
viewport descriptor.}


@defproc*[([(open-pixmap [name string?]
                         [horiz exact-nonnegative-integer?]
                         [vert exact-nonnegative-integer?])
            viewport?]
           [(open-pixmap [name string?]
                         [dimensions posn?])
            viewport?])]{

Like @racket[open-viewport], but the resulting viewport is not
displayed on the screen. Offscreen pixmaps are useful for executing a
sequence of drawing commands and displaying them all at once with
@racket[copy-viewport].
  
Offscreen pixmaps are also useful in conjunction with viewport->snip
(see below). This allows functions to compute with graphical objects
and view the graphics when results are returned to the interactions
window.}


@defproc[(close-viewport [viewport viewport?]) void?]{

Removes the viewport from the screen and makes subsequent operations
dealing with the viewport illegal.}


@defproc[(viewport? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a viewport (i.e., a destination
for drawing), @racket[#f] otherwise.}

@; ----------------------------------------------------------------------

@section[#:tag "posn"]{Position Operations}

A position is a pixel location within a viewport.  The upper-left
corner is pixel @math{(0, 0)}, and positions increase to the right and
down.


@defstruct[posn ([x real?][y real?])]{

Represents a positions.}


@defproc[((get-pixel [viewport viewport?]) [p posn?]) (one-of/c 0 1)]{

Returns the color of the pixel at position @racket[p] in
@racket[viewport]; @racket[0] denotes white and @racket[1] denotes not
white.}


@defproc[((get-color-pixel [viewport viewport?]) [p posn?]) rgb?]{

Returns an @racket[rgb] value for color of the pixel at position
@racket[p] in @racket[viewport].}


@defproc[((test-pixel [viewport viewport?]) [color (or/c (integer-in 0 299)
                                                         string?
                                                         rgb?)])
         rgb?]{

Returns the color that will actually be used if @racket[color] is used
to draw.}

@; ----------------------------------------------------------------------

@section{Color Operations}

A color can be represented in three ways: as a color index (an integer
in 0 to 299, inclusive), as a color name string, or as a @racket[rgb]
value. All drawing functions which take a color argument accept colors
in any form. An @racket[rgb] value is assigned to an index with
@racket[change-color].


@defstruct[rgb ([red (real-in 0 1)][green (real-in 0 1)][blue (real-in 0 1)])]{

Takes three values in the range 0 (dark) to 1 (bright) and returns an
@racket[rgb] (a color).}


@defproc[(change-color [index (integer-in 0 299)] [rgb rgb?]) 
         void?]{

Changes the color at @racket[index] in the color table to the
color specified in @racket[rgb]. Only the first twenty-one indices
are initialized; a color index should not be used until it has
been initialized.}


@defproc[(default-display-is-color?) boolean?]{

Returns @racket[#t] if the default display screen for viewports is in
color or @racket[#f] otherwise.}

@; ----------------------------------------------------------------------

@section{Draw, Clear, and Flip Operations}

The following are the basic graphics operations for drawing to a
viewport.  Each function takes a viewport as its argument and returns
a function operating within that viewport.  Further arguments, if any,
are curried.  For example, @racket[(draw-line _viewport)] returns a
function, that can then be applied to the proper arguments to draw a
line in the viewport corresponding to viewport descriptor
@racket[_viewport].

In general, @racketidfont{draw-} functions make pixels black or
colored, @racketidfont{clear-} functions make them white, and
@racketidfont{flip-} commands @deftech{invert} pixels (which makes
black white, white black, and is otherwise ill-defined).

@subsection{Viewports}

@defproc[((draw-viewport [viewport viewport?])
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Colors the entire contents of @racket[viewport] with @racket[color].}

@defproc[((clear-viewport [viewport viewport?]))
         void?]{

Whitens the entire contents of @racket[viewport].}

@defproc[((flip-viewport [viewport viewport?]))
         void?]{

@tech{Inverts} the entire contents of @racket[viewport].}


@defproc[(copy-viewport [source viewport?] [dest viewport?])
         void?]{

Copies the content of @racket[source] into @racket[dest].}

@; ----------------------------------------

@subsection{Pixels}

@defproc[((draw-pixel [viewport viewport?])
          [p posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Colors the pixel in @racket[viewport] at @racket[p].}

@defproc[((clear-pixel [viewport viewport?])
          [p posn?])
         void?]{

Whitens the pixel in @racket[viewport] at @racket[p].}

@defproc[((flip-pixel [viewport viewport?])
          [p posn?])
         void?]{

@tech{Inverts} the pixel in @racket[viewport] at @racket[p].}

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

Draws a line in @racket[viewport] connecting positions @racket[p1] and
@racket[p2].}


@defproc[((clear-line [viewport viewport?]) 
          [p1 posn?]
          [p2 posn?])
         void?]{

Whitens a line in @racket[viewport] connecting positions @racket[p1]
and @racket[p2].}


@defproc[((flip-line [viewport viewport?]) 
          [p1 posn?]
          [p2 posn?])
         void?]{

@tech{Inverts} a line in @racket[viewport] connecting positions
@racket[p1] and @racket[p2].}

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

Draws a rectangle border in the @racket[viewport] with the top-left of
the rectangle at the position @racket[p] and with sides @racket[width]
across and @racket[height] tall.}


@defproc[((clear-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a rectangle border in the @racket[viewport], analogous to
@racket[draw-rectangle].}


@defproc[((flip-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a rectangle border in the @racket[viewport], analogous
to @racket[draw-rectangle].}


@defproc[((draw-solid-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a solid rectangle in the @racket[viewport] with the top-left of
the rectangle at the position @racket[p] and with sides @racket[width]
across and @racket[height] tall.}


@defproc[((clear-solid-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a rectangle border in the @racket[viewport], analogous to
@racket[draw-solid-rectangle].}


@defproc[((flip-solid-rectangle [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a rectangle border in the @racket[viewport], analogous
to @racket[draw-solid-rectangle].}

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

Draws a ellipse border in the @racket[viewport]. The ellipse is
inscribed with a rectangle whose top-left is at position @racket[p]
and with sides @racket[width] across and @racket[height] tall.}


@defproc[((clear-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a ellipse border in the @racket[viewport], analogous to
@racket[draw-ellipse].}


@defproc[((flip-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a ellipse border in the @racket[viewport], analogous
to @racket[draw-ellipse].}


@defproc[((draw-solid-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a solid ellipse in the @racket[viewport]. The ellipse is
inscribed with a rectangle whose top-left is at position @racket[p]
and with sides @racket[width] across and @racket[height] tall.}


@defproc[((clear-solid-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

Whitens a ellipse border in the @racket[viewport], analogous to
@racket[draw-solid-ellipse].}


@defproc[((flip-solid-ellipse [viewport viewport?]) 
          [p posn?]
          [width (and/c real? (not/c negative?))]
          [height (and/c real? (not/c negative?))])
         void?]{

@tech{Inverts} a ellipse border in the @racket[viewport], analogous
to @racket[draw-solid-ellipse].}

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

Draws a polygon border in @racket[viewport] using @racket[points] for
the polygon vertices and @racket[offset] as an offset added to all
points.}


@defproc[((clear-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

Whitens a polygon border in @racket[viewport], analogous to
@racket[draw-polygon].}


@defproc[((flip-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

@tech{Inverts} a polygon border in @racket[viewport], analogous to
@racket[draw-polygon].}


@defproc[((draw-solid-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a solid polygon in @racket[viewport] using @racket[points] for
the polygon vertices and @racket[offset] as an offset added to all
points.}


@defproc[((clear-solid-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

Whitens a polygon border in @racket[viewport], analogous to
@racket[draw-solid-polygon].}


@defproc[((flip-solid-polygon [viewport viewport?]) 
          [points (listof posn?)]
          [offset posn?])
         void?]{

@tech{Inverts} a polygon border in @racket[viewport], analogous to
@racket[draw-solid-polygon].}

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

Draws a string at a specified location in the @racket[viewport].
The lower left of the string begins at @racket[p].}


@defproc[((clear-string [viewport viewport?]) 
          [p posn?]
          [str string?])
         void?]{

Whitens a string at a specified location in the @racket[viewport].
The lower left of the string begins at @racket[p].}

@defproc[((flip-string [viewport viewport?]) 
          [p posn?]
          [str string?])
         void?]{

@tech{Inverts} a string at a specified location in the
@racket[viewport].  The lower left of the string begins at
@racket[p].}

@; ----------------------------------------

@subsection{Pixmaps}

@defproc[(((draw-pixmap-posn [file path-string?] 
                             [type (one-of/c 'unknown 'unknown/mask 
                                             'gif 'gif/mask 'jpeg 'png 'png/mask 
                                             'xbm 'xpm 'bmp)
                                   'unknown/mask])
           [viewport viewport?])
          [p posn?]
          [color (or/c (integer-in 0 299)
                       string?
                       rgb?)
                 "black"])
         void?]{

Draws a pixmap into @racket[viewport] with its upper left corner at
position @racket[p]. If @racket[type] is @racket['unknown] or
@racket['unknown/mask], then the content of the file is examined to
determine the type.  The
@racket['gif/mask], @racket['png/mask], and @racket['unknown/mask]
types draw the bitmap with a transparent background if
@racket[filename] refers to a GIF/PNG file with a transparent
background.

The argument @racket[color] is only used when the loaded pixmap is
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

Equivalent to @racket[(((draw-pixmap-posn file) viewport) p color)].}

@defproc[((save-pixmap [viewport viewport?])
          [file path-string?]
          [type (one-of/c 'gif 'jpeg 'png 'xbm 'xpm 'bmp) 'xpm])
         void?]{

Saves the current content of @racket[viewport] to @racket[file].
The @racket[type] argument determines the kind of file that is written.}

@; ----------------------------------------

@section{World Operations}

Every canvas comes with an associated world. A client program can set
the world, start the world's clock, stop the world's clock, and deal
with tick events (the clock ticks) and keyboard inputs (keyevents).

@defproc[((init-world [viewport viewport?]) [v any/c]) void?]{

Sets the initial value of @racket[viewport]'s world to @racket[v].}

@defproc[((set-on-tick-event [viewport viewport?])
          [secs real?] [update-callback (any/c . -> . any/c)]) void?]{

For @racket[viewport], sets @racket[update-callback] to be invoked to
transform the world value every @racket[secs] seconds. Only one
callback is installed at a time.}

@defproc[((stop-tick [viewport viewport?])) void?]{

Stops updating @racket[viewport]'s world via a callback installed with
@racket[set-on-tick-event].}

@defproc[((set-on-key-event [viewport viewport?])
          [key-callback (any/c any/c . -> . any/c)])
         void?]{

Sets @racket[key-callback] as the function to call whenever a key
event is received for @racket[viewport]. The @racket[key-callback] is
given a key event and the current world, and it produces an updated
world.}

@; ----------------------------------------

@section{Miscellaneous Operations}

@defproc[((get-string-size [viewport viewport?]) [str string?]) (list/c real? real?)]{

Returns the size of @racket[str] as drawn into @racket[viewport] as a
list of two numbers: width and height.}

@defproc[(viewport->snip [viewport viewport?]) (is-a?/c snip%)] 

Returns an object that can be inserted into an editor buffer to
display the current image in the viewport. (Subsequent drawing to the
viewport does not affect the snip's image.)

When snips are the results of computations in the interactions window,
DrRacket shows the snip in the interactions window.}

@defproc[(viewport-dc [viewport viewport?]) (is-a?/c dc<%>)]{

Returns an object for direct drawing into @racket[viewport]'s
on-screen representation (if any). Mirror all such drawing to the
result of @racket[(viewport-buffer-dc viewport)], too.}

@defproc[(viewport-buffer-dc [viewport viewport?]) (is-a?/c dc<%>)]{

Returns an object for direct drawing into @racket[viewport]'s
off-screen representation. Mirror all such drawing to the
result of @racket[(viewport-dc viewport)], too.}

@; ----------------------------------------

@section{An Example}

@racketblock[
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

@racketblock[
(open-graphics)
(let* ((code:comment @#,t{@racket[w1] and @racket[w2] are viewports for different windows})
       [w1  (open-viewport "viewport 1" 300 300)]
       [w2  (open-viewport "viewport 2" 200 500)]
       (code:comment @#,t{@racket[d1] and @racket[d2] draw lines in different viewports})
       [d1  (draw-line w1)]
       [d2  (draw-line w2)])
  (code:comment @#,t{draws a line in viewport labeled ``viewport 1''})
  (d1 (make-posn 100 5) (make-posn 5 100))
  (code:comment @#,t{draws a line in viewport labeled ``viewport 2''})
  (d2 (make-posn 100 100) (make-posn 101 400)))
 
(code:comment @#,t{we no longer have access to viewports 1 and 2,})
(code:comment @#,t{since their descriptors did not escape the @racket[let]})
(close-graphics) 
(code:comment @#,t{removes the viewports})
]

@section{Protecting Graphics Operations}

To guarantee the proper closing of viewports in cases of errors,
especially when a program manages several viewports simultaneously, a
programmer should use @racket[dynamic-wind:]

@racketblock[
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
@racket[get-mouse-click] and @racket[ready-mouse-click] first return a
``mouse-click descriptor,'' and then other functions take the
descriptor and return the mouse's position, which button was pushed,
etc.  Mouse clicks are buffered and returned in the same order in
which they occurred.  Thus, the descriptors returned by
@racket[get-mouse-click] and @racket[ready-mouse-click] may be from
clicks that occurred long before these functions were called.

@defproc[(get-mouse-click [viewport viewport?]) mouse-click?]{

Returns the next mouse click in @racket[viewport], waiting for a click
if necessary.}


@defproc[(ready-mouse-click [viewport viewport?])
         (or/c mouse-click? false/c)]{

Returns either a mouse click descriptor or @racket[#f] if none is
available.  Unlike @racket[get-mouse-click],
@racket[ready-mouse-click] always returns immediately.}


@defproc[(ready-mouse-release [viewport viewport?]) 
         (or/c mouse-click? false/c)]{

Returns either a click descriptor from a mouse-release (button-up)
event or @racket[#f] if none is available.}


@defproc[(query-mouse-posn [viewport viewport?]) (or/c posn? false/c)]{

Returns either the position of the mouse cursor within
@racket[viewport] or else @racket[#f] if the cursor is currently
outside @racket[viewport].}


@defproc[(mouse-click-posn [mouse-click mouse-click?]) posn?]{

Returns the position of the pixel within a viewport where a given
mouse click occurred.}


@defproc[(left-mouse-click? [mouse-click mouse-click?]) boolean?]{

Returns @racket[#t] if the mouse click occurred with the left mouse
button, @racket[#f] otherwise.}

@defproc[(middle-mouse-click? [mouse-click mouse-click?]) boolean?]{

Returns @racket[#t] if the mouse click occurred with the middle mouse
button, @racket[#f] otherwise.}

@defproc[(right-mouse-click? [mouse-click mouse-click?]) boolean?]{

Returns @racket[#t] if the mouse click occurred with the right mouse
button, @racket[#f] otherwise.}

@; ----------------------------------------

@section{Keyboard Operations}

The graphics library contains functions that report key presses from
the keyboard.  The functions @racket[get-key-press] and
@racket[ready-key-press] return a ``key-press descriptor,'' and then
@racket[key-value] takes the descriptor and returns a character or
symbol (usually a character) representing the key that was pressed.
Key presses are buffered and returned in the same order in which they
occurred.  Thus, the descriptors returned by @racket[get-key-press]
and @racket[ready-key-press] may be from presses that occurred long
before these functions were called.

@defproc[(get-key-press [viewport viewport?]) key-press?]{

Returns the next key press in the @racket[viewport], waiting for a
key press if necessary.}

@defproc[(ready-key-press [viewport viewport?]) key-press?]{

Returns the next key press in the @racket[viewport] or returns
@racket[#f] if none is available. Unlike @racket[get-key-press],
@racket[ready-key-press] always returns immediately.}

@defproc[(key-value [key-press key-press?]) (or/c character? symbol?)]{

Returns a character or special symbol for the key that was
pressed. For example, the Enter key generates @racket[#\return], and the
up-arrow key generates @racket['up].  For a complete list of possible
return values, see @method[key-event% get-key-code].}

@; ----------------------------------------

@section{Flushing}

@defproc[(viewport-flush-input [viewport viewport?]) void?]{

Empties all mouse and keyboard events in the input buffer of
@racket[viewport].}

@; ----------------------------------------

@section{Graphics Library as a Unit}

@subsection{Signatures}

@defmodule[graphics/graphics-sig]

@defsignature[graphics^ ()]

Includes all of the bindings defined earlier in this chapter, except
the @racket[posn] bindings of @secref["posn"].


@defsignature[graphics:posn^ ()]

Includes the @racket[posn] bindings of @secref["posn"].


@subsection{Unit with @racket[posn]}

@defmodule[graphics/graphics-unit]

@defthing[graphics@ unit?]{

Imports @racket[mred^] and exports both @racket[graphics^] and
@racket[graphics:posn^].}

@subsection{Unit without @racket[posn]}

@defmodule[graphics/graphics-posn-less-unit]

@defthing[graphics-posn-less@ unit?]{

Imports @racket[mred^] and @racket[graphics:posn^] and exports
@racket[graphics^].}
