#lang scribble/doc
@(require "common.rkt")

@definterface/title[editor-snip-editor-admin<%> ()]{

An instance of this administrator interface is created with each
 @racket[editor-snip%] object; new instances cannot be
 created directly.


@defmethod[(get-snip)
           (is-a?/c editor-snip%)]{

Returns the snip that owns this administrator (and displays the
editor controlled by the administrator, if any).

}}

