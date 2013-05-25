#lang scribble/doc
@(require "common.rkt")

@defclass/title[editor-stream-in-bytes-base% editor-stream-in-base% ()]{

An @racket[editor-stream-in-bytes-base%] object can be used to
read editor data from a byte string.


@defconstructor/make[([s bytes?])]{

Creates a stream base that reads from @racket[s].

}}
