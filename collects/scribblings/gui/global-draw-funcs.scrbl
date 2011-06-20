#lang scribble/doc
@(require "common.rkt")

@title{Global Graphics}

@defproc[(flush-display)
         void?]{

Flushes canvas offscreen drawing and other updates onto the
 screen.

Normally, drawing is automatically flushed to the screen. Use
@racket[flush-display] sparingly to force updates to the screen when
other actions depend on updating the display.}


@defproc[(get-display-depth)
         exact-nonnegative-integer?]{

Returns the depth of the main display (a value of 1 denotes a monochrome display).

}

@defproc[(get-display-left-top-inset [avoid-bars? any/c #f])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

When the optional argument is @scheme[#f] (the default), this function
 returns the offset of the main screen's origin from the
 top-left of the physical screen. On Unix and Windows, the result is
 always @scheme[0] and @scheme[0]; on Mac OS X, the result is
 @scheme[0] and the height of the menu bar.

When the optional argument is true, this function returns the amount
 space at the left and top of the main screen that is occupied by the
 task bar (Windows) or menu bar and dock (Mac OS X). On Unix, the
 result is always @scheme[0] and @scheme[0].

}

@defproc[(get-display-size [full-screen? any/c #f])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

@index["screen resolution"]{Gets} the physical size of the display in
 pixels.  On Windows, this size does not include the task bar by
 default.  On Mac OS X, this size does not include the menu bar or
 dock area by default.

On Windows and Mac OS X, if the optional argument is true, then
 the task bar, menu bar, and dock area are included in the result.

Returns the screen's width and height.

}

@defproc[(is-color-display?)
         boolean?]{

Returns @scheme[#t] if the main display has color, @scheme[#f]
otherwise.

}
