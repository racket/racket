#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Number Snip}

@defclass[number-snip:snip-class% snip-class% ()]{

  @defmethod*[#:mode override (((read (f stream-in)) snip))]{

    Constructs a number snip from its input.
  }
}

@(include-previously-extracted "main-extracts.ss" #rx"^number-snip:")
