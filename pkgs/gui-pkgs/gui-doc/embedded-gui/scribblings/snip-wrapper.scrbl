#lang scribble/doc
@(require "common.rkt")

@defclass/title[snip-wrapper% dllist<%> (alignment<%>)]{

Adapts an arbitrary @racket[snip<%>] to work in an alignment
container.

@defconstructor[([parent (is-a?/c alignment-parent<%>)]
                 [snip (is-a?/c snip%)])]{

Adds @racket[snip] to @racket[parent].}}
