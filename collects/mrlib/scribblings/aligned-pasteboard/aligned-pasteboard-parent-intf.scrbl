#lang scribble/doc
@(require "common.ss")

@definterface/title[aligned-pasteboard-parent<%> ()]{

This interface must be implemented by any class who's editor
is an @scheme[aligned-pasteboard<%>].

@defmethod[(set-aligned-min-sizes)
           void?]{}

}

