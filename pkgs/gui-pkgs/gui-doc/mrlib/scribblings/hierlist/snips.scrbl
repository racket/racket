#lang scribble/doc
@(require "../common.rkt" (for-label mrlib/hierlist))

@title{Snips in a @racket[hierarchical-list%] Instance}

The @xmethod[text% find-snip] method of the editor in a
@racket[hierarchical-list%] return instances of
@racket[hierarchical-item-snip%] and @racket[hierarchical-list-snip%].

@defclass[hierarchical-item-snip% editor-snip% ()]{

 @defmethod[(get-item) (is-a?/c hierarchical-list-item<%>)]{

 Returns the @racket[hierarchical-list-item<%>] corresponding to the
 snip.}

}


@defclass[hierarchical-list-snip% editor-snip% ()]{

 @defmethod[(get-item) (is-a?/c hierarchical-list-compound-item<%>)]{

 Returns the @racket[hierarchical-list-compound-item<%>] corresponding to the
 snip.}

 @defmethod[(get-content-buffer) (is-a?/c text%)]{

 Returns the text% that contains the sub-item snips.}

}
