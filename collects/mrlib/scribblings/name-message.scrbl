#lang scribble/doc
@(require "common.ss"
          (for-label mrlib/name-message))

@title{Name Message}

@defmodule[mrlib/name-message]

@defclass[name-message% canvas% ()]{

A @scheme[name-message%] control displays a filename that the user can
click to show the filename's path and select one of the enclosing
directories. Override the @method[name-message% on-choose-directory]
method to handle the user's selection.


@defconstructor/auto-super[()]{

Passes all arguments to @scheme[super-init].}


@defmethod[(on-choose-directory [dir path-string?])
           void?]{

Called when one of the popup menu items is chosen. The argument is a
represents the selected directory.}


@defmethod[#:mode override 
           (on-event [event (is-a?/c mouse-event%)])
           void?]{

Handles the click by popping up a menu or message.}


@defmethod[#:mode override 
           (on-paint)
           void?]{

Draws the control's current message.}


@defmethod[(set-hidden? [hidden? any/c])
           void?]{
Calling this method with @scheme[#f] causes the name message
to become invisible and to stop responding to mouse movements.

Calling it with a true value restores its visibility and
makes it respond to mouse movements again.}


@defmethod[(set-message [file-name? any/c]
                        [msg path-string?])
           void?]{

Sets the label for the control.

If @scheme[file-name?] is @scheme[#t], @scheme[msg] is treated like a
 pathname, and a click on the name-message control creates a popup
 menu to open a get-file dialog.

If @scheme[file-name?] is @scheme[#f], @scheme[msg] is treated as a
label string. Clicking on the name-message control pops up a dialog
saying that there is no file name until the file is saved.}


@defmethod[(get-background-color) (or/c false/c (is-a/c color%) string?)]{

The result of this method is used for the background color
when redrawing the name message. If it is @scheme[#f], the
OS's default panel background is used.

}

@defmethod[(set-allow-shrinking [width (or/c false/c number?)]) void?]{

When this method receives a number, the name-message will
then shrink (the number indicates the minimum width the name
message will have).

If it receives false, the name message will not shrink and
its minimum width will be the size required to display its
current label.

Defaultly, the name-message does not allow shrinking.
}

}

@; ----------------------------------------------------------------------


@defproc[(calc-button-min-sizes [dc (is-a?/c dc<%>)]
                                [str string?]) 
         (values real? real?)]{

Calculates the minimum width and height of a button label (when drawn
with @scheme[draw-button-label]). Returns two values: the width
and height. The @scheme[dc] argument is used for sizing.}


@defproc[(draw-button-label [dc (is-a?/c dc<%>)]
                            [str string?]
                            [dx real?]
                            [dy real?]
                            [width real?]
                            [height real?]
                            [mouse-over? boolean?]
                            [grabbed? boolean?]
                            [font (is-a?/c font%)]
                            [background (or/c (is-a?/c color%) string? false/c)])
         void?]{

Draws a button label like the one for the @onscreen{(define ...)} and
filename buttons in the top-left corner of the DrScheme frame. Use
this function to draw similar buttons.

The basic idea is to create a canvas object whose on-paint method is
overridden to call this function.  The @scheme[dc] argument should be
canvas's drawing context, and @scheme[str] should be the string to
display on the button.  The @scheme[width] and @scheme[height]
arguments should be the width and height of the button, and the
@scheme[dx] and @scheme[dy] arguments specify an offset into
@scheme[dc] for the button. The @scheme[mouse-over?] argument should
be true when the mouse is over the button, and the @scheme[grabbed?]
argument should be true when the button has been pressed. The
@scheme[font] and @scheme[background] arguments supply the font to use
in drawing (possibly @scheme[normal-control-font]) and the background
color to paint (if any).

See @scheme[calc-button-min-sizes] for help calculating the min sizes
of the button.}

