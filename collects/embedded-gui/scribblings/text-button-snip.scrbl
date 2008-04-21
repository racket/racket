#lang scribble/doc
@(require "common.ss")

@defclass/title[text-button-snip% snip% ()]{

A button with a text label.

@defconstructor[([label string?]
                 [callback ((is-a?/c text-button-snip%) (is-a?/c event%) . -> . void)])]{

The @scheme[callback] is called when the button is clicked.}

}
