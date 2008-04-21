#lang scribble/doc
@(require "common.ss")

@defclass/title[embedded-message% snip-wrapper% ()]{

A static text label.

@defconstructor[([parent (is-a?/c alignment-parent<%>)]
                 [label string?])]{

Creates a static control that displays @scheme[label].}}
