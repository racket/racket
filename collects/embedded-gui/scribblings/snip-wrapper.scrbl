#lang scribble/doc
@(require "common.rkt")

@defclass/title[snip-wrapper% dllist<%> (alignment<%>)]{

Adapts an arbitrary @scheme[snip<%>] to work in an alignment
container.

@defconstructor[([parent (is-a?/c alignment-parent<%>)]
                 [snip (is-a?/c snip%)])]{

Adds @scheme[snip] to @scheme[parent].}}
