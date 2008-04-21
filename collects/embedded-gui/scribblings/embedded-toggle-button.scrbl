#lang scribble/doc
@(require "common.ss")

@defclass/title[embedded-toggle-button% snip-wrapper% (alignment<%>)]{

A @scheme[check-box%]-like control that a user can toggle between
checked and unchecked states.

@defconstructor[([images-off (cons/c path-string? path-string?)]
                 [images-on (cons/c path-string? path-string?)]
                 [turn-on ((is-a?/c toggle-button-snip%) (is-a?/c event%) . -> . void?)]
                 [turn-off ((is-a?/c toggle-button-snip%) (is-a?/c event%) . -> . void?)]
                 [state (symbols 'on 'off) 'on])]{

The @scheme[images-off] argument is a pair filenames to be load as the
button-label image, where the first is the image for when the button
is at rest, and the second is the image for the button while its
pressed---in both cases when the button is not checked by the
user.  The @scheme[images-on] argument similarly determines the images
for then the button is checked.

The @scheme[turn-on] and @scheme[turn-off] callbacks are invoked when
the button changes to checked or unchecked, respectively.

The @scheme[state] argument determines whether the button is initially
checked.}

}
