#lang scribble/doc
@(require "common.ss")

@title{Global Graphics}

@defproc[(flush-display)
         void?]{

Under X and Mac OS X, flushes pending display messages such that the
 user's display reflects the actual state of the windows. Under
 Windows, the procedure has no effect.

}

@defproc[(get-display-depth)
         exact-nonnegative-integer?]{

Returns the depth of the main display (a value of 1 denotes a monochrome display).

}

@defproc[(get-display-left-top-inset [avoid-bars? bool #f])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

When the optional argument is @scheme[#f] (the default), this function
 returns the offset of the main screen's origin from the
 top-left of the physical screen. Under X and Windows, the result is
 always @scheme[0] and @scheme[0]; under Mac OS X, the result is
 @scheme[0] and the height of the menu bar.

When the optional argument is true, this function returns the amount
 space at the left and top of the main screen that is occupied by the
 task bar (Windows) or menu bar and dock (Mac OS X). Under X, the
 result is always @scheme[0] and @scheme[0].

}

@defproc[(get-display-size [full-screen? bool #f])
         (values exact-nonnegative-integer? exact-nonnegative-integer?)]{

@index["screen resolution"]{Gets} the physical size of the display in
 pixels.  Under Windows, this size does not include the task bar by
 default.  Under Mac OS X, this size does not include the menu bar or
 dock area by default.

Under Windows and Mac OS X, if the optional argument is true, then
 the task bar, menu bar, and dock area are included in the result.

Returns the screen's width and height.

}

@defproc[(is-color-display?)
         boolean?]{

Returns @scheme[#t] if the main display has color, @scheme[#f]
otherwise.

}
