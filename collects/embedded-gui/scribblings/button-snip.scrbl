#lang scribble/doc
@(require "common.rkt")

@defclass/title[button-snip% snip% ()]{

A clickable button with a bitmap label.

@defconstructor[([images (cons/c path-string? path-string?)]
                 [callback ((is-a?/c button-snip%) (is-a?/c event%) . -> . void?)])]{

The @racket[images] argument is a pair filenames to be load as the
button-label image, where the first is the image for when the button
is at rest, and the second is the image for the button while its
pressed.

The @racket[callback] is called when the button is clicked.}

}
