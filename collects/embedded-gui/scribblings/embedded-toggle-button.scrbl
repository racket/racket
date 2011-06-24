#lang scribble/doc
@(require "common.rkt")

@defclass/title[embedded-toggle-button% snip-wrapper% (alignment<%>)]{

A @racket[check-box%]-like control that a user can toggle between
checked and unchecked states.

@defconstructor[([images-off (cons/c path-string? path-string?)]
                 [images-on (cons/c path-string? path-string?)]
                 [turn-on ((is-a?/c toggle-button-snip%) (is-a?/c event%) . -> . void?)]
                 [turn-off ((is-a?/c toggle-button-snip%) (is-a?/c event%) . -> . void?)]
                 [state (symbols 'on 'off) 'on])]{

The @racket[images-off] argument is a pair filenames to be load as the
button-label image, where the first is the image for when the button
is at rest, and the second is the image for the button while its
pressed---in both cases when the button is not checked by the
user.  The @racket[images-on] argument similarly determines the images
for then the button is checked.

The @racket[turn-on] and @racket[turn-off] callbacks are invoked when
the button changes to checked or unchecked, respectively.

The @racket[state] argument determines whether the button is initially
checked.}

}
