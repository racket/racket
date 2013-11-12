#lang scribble/doc
@(require "common.rkt")

@defclass/title[grow-box-spacer-pane% pane% ()]{

A @racket[grow-box-spacer-pane%] object is intended for use as a
 lightweight spacer in the bottom-right corner of a frame, rather than
 as a container. On older version of Mac OS X, a
 @racket[grow-box-spacer-pane%] has the same width and height as the
 grow box that is inset into the bottom-right corner of a frame. On
 Windows, Unix, and recent Mac OS X, a @racket[grow-box-spacer-pane%] has zero width and
 height. Unlike all other container types, a
 @racket[grow-box-spacer-pane%] is unstretchable by default.


@defconstructor/auto-super[()]{

See @racket[pane%] for information on initialization arguments.

}}
