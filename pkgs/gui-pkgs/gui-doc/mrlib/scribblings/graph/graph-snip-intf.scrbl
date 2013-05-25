#lang scribble/doc
@(require "common.rkt")

@definterface/title[graph-snip<%> ()]{



@defmethod[(add-child [child (is-a?/c graph-snip<%>)])
           void?]{

  Adds a child of this snip. Instead of calling this method,
  consider using the @racket[add-links] function.

}

@defmethod*[([(add-parent [parent (is-a?/c graph-snip<%>)])
              void?]
             [(add-parent [parent (is-a?/c graph-snip<%>)]
                          [mouse-over-pen (or/c false/c (is-a?/c pen%))]
                          [mouse-off-pen (or/c false/c (is-a?/c pen%))]
                          [mouse-over-brush (or/c false/c (is-a?/c brush%))]
                          [mouse-off-brush (or/c false/c (is-a?/c brush%))])
              void?])]{

  Adds a parent of this snip. Instead of calling this
  method, consider using the @racket[add-links] function.

}

@defmethod[(get-children)
           (listof snip%)]{

returns a list of snips that implement
@racket[graph-snip<%>]. Each of these snips will have a line
drawn from it, pointing at this snip.

}


@defmethod[(get-parents)
           (listof graph-snip<%>)]{

Returns a list of snips that implement @racket[graph-snip<%>]. Each
of these snips will have a line drawn to it, starting from
this snip.
}


@defmethod[(remove-child [child (is-a?/c graph-snip<%>)])
           void?]{

  Removes a child snip from this snip. Be sure to remove
  this snip as a parent from the argument, too.
  Instead of calling this method, consider using the
  @racket[remove-links] function.
}


@defmethod[(remove-parent [parent (is-a?/c graph-snip<%>)])
           void?]{

  Removes a parent snip from this snip. Be sure to remove this
  snip as a child from the argument, too.
  Instead of calling this method, consider using the
  @racket[remove-links] function.
}


@defmethod[(set-parent-link-label [parent (is-a?/c graph-snip<%>)]
                                  [label (or/c false/c string/)])
           void?]{

  Changes the label on the edge going to the @racket[parent] to be
  @racket[label].  Ignored if no such egde exists.
}

}
