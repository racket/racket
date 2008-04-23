#lang scribble/doc
@(require scribble/manual)
@(require (for-label framework/framework))
@(require (for-label scheme/gui))
@title{Number Snip}

@defclass[number-snip:snip-class% snip-class% ()]{

  @defmethod*[#:mode override (((read (f stream-in)) snip))]{

    Constructs a number snip from its input.
  }
}
@(require framework/framework-docs)
@(def-fw-procs number-snip)
