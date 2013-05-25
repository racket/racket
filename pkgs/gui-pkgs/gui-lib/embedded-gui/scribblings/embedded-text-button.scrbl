#lang scribble/doc
@(require "common.rkt")

@defclass/title[embedded-text-button% snip-wrapper% (alignment<%>)]{

A button with a text label.

@defconstructor[([label string?]
                 [callback ((is-a?/c text-button-snip%) (is-a?/c event%) . -> . void?)])]{

The @racket[callback] is called when the button is clicked.}

}
