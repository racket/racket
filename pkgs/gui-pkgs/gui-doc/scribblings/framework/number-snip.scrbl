#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Number Snip}

@defclass[number-snip:snip-class% snip-class% ()]{

  @defmethod*[#:mode override (((read (f (is-a?/c editor-stream-in%))) (or/c (is-a?/c snip%) #f)))]{

    Constructs a number snip from its input.
  }
}

@(include-previously-extracted "main-extracts.rkt" #rx"^number-snip:")
