#lang scribble/doc
@(require "common.rkt")

@defclass/title[horizontal-alignment% dllist<%> (alignment<%> alignment-parent<%>)]{

@defconstructor[([parent (is-a?/c alignment-parent<%>)]
                 [show? boolean? #t]
                 [after (or/c (is-a?/c alignment<%>) false/c) #f])]{

Inserts a new horizontal-alignment container into
@racket[parent]---optionally after a given container also in
@racket[parent]. The new container can be initially shown or hidden.}}
