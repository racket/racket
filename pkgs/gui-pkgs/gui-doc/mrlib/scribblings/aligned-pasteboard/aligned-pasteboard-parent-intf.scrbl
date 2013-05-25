#lang scribble/doc
@(require "common.rkt")

@definterface/title[aligned-pasteboard-parent<%> ()]{

This interface must be implemented by any class who's editor
is an @racket[aligned-pasteboard<%>].

@defmethod[(set-aligned-min-sizes)
           void?]{}

}
