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


@defproc[(get-display-count) exact-positive-integer?]{
Returns the number of monitors currently active. 

On Windows and Mac OS X, the result can change at any time.
See also @xmethod[top-level-window<%> display-changed].

}


@defproc[(get-display-depth)
         exact-nonnegative-integer?]{

Returns the depth of the main display (a value of 1 denotes a monochrome display).

}

@defproc[(get-display-left-top-inset [avoid-bars? any/c #f]
                                     [#:monitor monitor exact-nonnegative-integer? 0])
         (values (or/c exact-nonnegative-integer? #f)
                 (or/c exact-nonnegative-integer? #f))]{

When the optional argument is @racket[#f] (the default), this function
 returns the offset of @racket[monitor]'s origin from the
 top-left of the physical monitor. For @racket[monitor] @racket[0], on Unix and Windows, the result is
 always @racket[0] and @racket[0]; on Mac OS X, the result is
 @racket[0] and the height of the menu bar. To position a frame
 at a given @racket[monitor]'s top-left corner, use the negated results from
 @racket[get-display-left-top-inset] as the frame's position.

When the optional @racket[avoid-bars?] argument is true, for @racket[monitor]
 @racket[0], @racket[get-display-left-top-inset] function returns the
 amount space at the left and top of the monitor that is occupied by
 the task bar (Windows) or menu bar and dock (Mac OS X). On Unix, for
 monitor @racket[0], the result is always @racket[0] and @racket[0].
 For monitors other than @racket[0], @racket[avoid-bars?] has no effect.

If @racket[monitor] is not less than the current number of available
 monitors (which can change at any time), the results are @racket[#f]
 and @racket[#f]. See also @xmethod[top-level-window<%> display-changed].
}


@defproc[(get-display-size [full-screen? any/c #f]
                           [#:monitor monitor exact-nonnegative-integer? 0])
         (values (or/c exact-nonnegative-integer? #f)
                 (or/c exact-nonnegative-integer? #f))]{

@index["screen resolution"]{Gets} the physical size of the specified @racket[monitor] in
 pixels.  On Windows, this size does not include the task bar by
 default.  On Mac OS X, this size does not include the menu bar or
 dock area by default.

On Windows and Mac OS X, if the optional argument is true and @racket[monitor] is @racket[0], then
 the task bar, menu bar, and dock area are included in the result.

If @racket[monitor] is not less than the current number of available
 monitors (which can change at any time), the results are @racket[#f]
 and @racket[#f]. See also @xmethod[top-level-window<%> display-changed].
}


@defproc[(is-color-display?)
         boolean?]{

Returns @racket[#t] if the main display has color, @racket[#f]
otherwise.

}
