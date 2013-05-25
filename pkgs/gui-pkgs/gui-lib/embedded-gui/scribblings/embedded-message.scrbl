#lang scribble/doc
@(require "common.rkt")

@defclass/title[embedded-message% snip-wrapper% ()]{

A static text label.

@defconstructor[([parent (is-a?/c alignment-parent<%>)]
                 [label string?])]{

Creates a static control that displays @racket[label].}}
