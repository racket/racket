#lang scribble/doc
@(require "common.rkt")

@definterface/title[subwindow<%> (subarea<%> window<%>)]{

A @racket[subwindow<%>] is a containee window.

@defmethod[(reparent [new-parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                                       (is-a?/c panel%) (is-a?/c pane%))])
           void?]{

Removes the window from its current parent and makes it a child of
@racket[new-parent]. The current and new parents must have the same
eventspace, and @racket[new-parent] cannot be a descendant of
@this-obj[].

If @this-obj[] is deleted within its current parent, it remains
deleted in @racket[new-parent]. Similarly, if @this-obj[] is shown in
its current parent, it is shown in @racket[new-parent].}

}
