#lang scribble/doc
@(require "common.ss")

@defclass/title[vertical-alignment% dllist<%> (alignment<%> alignment-parent<%>)]{

@defconstructor[([parent (is-a?/c alignment-parent<%>)]
                 [show? boolean? #t]
                 [after (or/c (is-a?/c alignment<%>) false/c) #f])]{

Inserts a new vertical-alignment container into
@scheme[parent]---optionally after a given container also in
@scheme[parent]. The new container can be initially shown or hidden.}}
