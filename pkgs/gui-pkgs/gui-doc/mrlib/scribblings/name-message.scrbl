#lang scribble/doc
@(require "common.rkt"
          (for-label mrlib/name-message))

@title{Name Message}

@defmodule[mrlib/name-message]

@defclass[name-message% canvas% ()]{

A @racket[name-message%] control displays a filename that the user can
click to show the filename's path and select one of the enclosing
directories. Override the @method[name-message% on-choose-directory]
method to handle the user's selection.


@defconstructor/auto-super[()]{

Passes all arguments to @racket[super-init].}


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
Calling this method with @racket[#f] causes the name message
to become invisible and to stop responding to mouse movements.

Calling it with a true value restores its visibility and
makes it respond to mouse movements again.}


@defmethod[(set-message [file-name? any/c]
                        [msg (if filename? path-string? string?)])
           void?]{

Sets the label for the control.

If @racket[file-name?] is @racket[#t], @racket[msg] is treated like a
 pathname, and a click on the name-message control creates a popup
 menu to open a get-file dialog.

If @racket[file-name?] is @racket[#f], @racket[msg] is treated as a
label string. Clicking on the name-message control pops up a dialog
saying that there is no file name until the file is saved.}

@defmethod[(set-short-title [short-title? boolean?]) void?]{
  Sets the @racket[short-title?] flag. The flag defaults to @racket[#f].
  
  If the flag is @racket[#t], then
  the label for the control is simply the string @racket["/"]. Otherwise,
  the label is determined by
  the @method[name-message% set-message].
}

@defmethod[(fill-popup [menu (is-a?/c popup-menu%)]
                       [reset (-> void?)])
           any]{
  This method is called when the user clicks in the name message.
  Override it to fill in the menu items for the popup menu @racket[menu].
}

@defmethod[(get-background-color) (or/c #f (is-a/c color%) string?)]{

The result of this method is used for the background color
when redrawing the name message. If it is @racket[#f], the
OS's default panel background is used.

}

@defmethod[(set-allow-shrinking [width (or/c #f number?)]) void?]{

When this method receives a number, the name-message will
then shrink (the number indicates the minimum width the name
message will have).

If it receives false, the name message will not shrink and
its minimum width will be the size required to display its
current label.

By default, the name-message does not allow shrinking.
}

}

@; ----------------------------------------------------------------------


@defproc[(calc-button-min-sizes [dc (is-a?/c dc<%>)]
                                [str string?]
                                [font (or/c #f (is-a?/c font%)) #f])
         (values real? real?)]{

Calculates the minimum width and height of a button label (when drawn
with @racket[draw-button-label]). Returns two values: the width
and height. The @racket[dc] argument is used for sizing.}


@defproc[(draw-button-label [dc (is-a?/c dc<%>)]
                            [str string?]
                            [dx real?]
                            [dy real?]
                            [width real?]
                            [height real?]
                            [mouse-over? boolean?]
                            [grabbed? boolean?]
                            [font (is-a?/c font%)]
                            [background (or/c (is-a?/c color%) string? #f)])
         void?]{

Draws a button label like the one for the @onscreen{(define ...)} and
filename buttons in the top-left corner of the DrRacket frame. Use
this function to draw similar buttons.

The basic idea is to create a canvas object whose on-paint method is
overridden to call this function.  The @racket[dc] argument should be
canvas's drawing context, and @racket[str] should be the string to
display on the button.  The @racket[width] and @racket[height]
arguments should be the width and height of the button, and the
@racket[dx] and @racket[dy] arguments specify an offset into
@racket[dc] for the button. The @racket[mouse-over?] argument should
be true when the mouse is over the button, and the @racket[grabbed?]
argument should be true when the button has been pressed. The
@racket[font] and @racket[background] arguments supply the font to use
in drawing (possibly @racket[normal-control-font]) and the background
color to paint (if any).

See @racket[calc-button-min-sizes] for help calculating the min sizes
of the button.}

@defproc[(pad-xywh [tx number?]
                   [ty number?]
                   [tw (>=/c 0)]
                   [th (>=/c 0)])
         (values number? number? (>=/c 0) (>=/c 0))]{
  Returns spacing information describing how
  @racket[draw-button-label] draws. The inputs are 
  the x and y coordinates where the text should appear
  and the width and height of the text, and the results
  are the x and y coordinates where the shape should be
  drawn and the width and height of the overall shape.
}
